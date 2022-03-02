/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.cache.mmu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import freechips.rocketchip.util.SRAMAnnotation
import xiangshan._
import utils._
import xiangshan.backend.fu.{PMPChecker, PMPReqBundle}
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.fu.util.HasCSRConst
import firrtl.FirrtlProtos.Firrtl.Module.ExternalModule.Parameter


trait TLBBlockHandlerDefault{this: TLBBase =>
  def handle_block(Width: Int, sameCycle: Boolean): Unit ={
    io.ptw <> DontCare
    io.requestor <> DonCare
  }
}

@chiselName
class TLBBase(Width: Int, q: TLBParameters)(implicit p: Parameters) extends TlbModule
  with TLBBlockHandlerDefault
  with HasCSRConst
  with HasPerfEvents
{
  val io = IO(new TlbIO(Width, q))

  require(q.superAssociative == "fa")
  if (q.sameCycle || q.missSameCycle) {
    require(q.normalAssociative == "fa")
  }

  val req = io.requestor.map(_.req)
  val resp = io.requestor.map(_.resp)
  val ptw = io.ptw
  val pmp = io.pmp

  val sfence = io.sfence
  val flush = sfence.valid
  val csr = io.csr
  val satp = csr.satp
  val priv = csr.priv
  val ifecth = if (q.fetchi) true.B else false.B
  val mode = if (q.useDmode) priv.dmode else priv.imode
  // val vmEnable = satp.mode === 8.U // && (mode < ModeM) // FIXME: fix me when boot xv6/linux...
  val vmEnable = if (EnbaleTlbDebug) (satp.mode === 8.U)
  else (satp.mode === 8.U && (mode < ModeM))

  val req_in = req
  val req_out = { if (q.sameCycle) req_in.map(_.bits) else req.map(a => RegEnable(a.bits, a.fire())) }
  val req_out_v = { if (q.sameCycle) req_in.map(_.valid) else {
    (0 until Width).map(i => ValidHold(req_in(i).fire, resp(i).fire, flush)) }}

  // Normal page && Super page
  val normalPage = TlbStorage(
    name = "normal",
    associative = q.normalAssociative,
    sameCycle = q.sameCycle,
    ports = Width,
    nSets = q.normalNSets,
    nWays = q.normalNWays,
    sramSinglePort = sramSinglePort,
    saveLevel = q.saveLevel,
    normalPage = true,
    superPage = false
  )
  val superPage = TlbStorage(
    name = "super",
    associative = q.superAssociative,
    sameCycle = q.sameCycle,
    ports = Width,
    nSets = q.superNSets,
    nWays = q.superNWays,
    sramSinglePort = sramSinglePort,
    saveLevel = q.saveLevel,
    normalPage = q.normalAsVictim,
    superPage = true,
  )


  for (i <- 0 until Width) {
    normalPage.r_req_apply(
      valid = io.requestor(i).req.valid,
      vpn = get_pn(req_in(i).bits.vaddr),
      asid = csr.satp.asid,
      i = i
    )
    superPage.r_req_apply(
      valid = io.requestor(i).req.valid,
      vpn = get_pn(req_in(i).bits.vaddr),
      asid = csr.satp.asid,
      i = i
    )
  }

  normalPage.victim.in <> superPage.victim.out
  normalPage.victim.out <> superPage.victim.in
  normalPage.sfence <> io.sfence
  superPage.sfence <> io.sfence
  normalPage.csr <> io.csr
  superPage.csr <> io.csr

  def TLBRead(i: Int) = {
    val (n_hit_sameCycle, normal_hit, normal_ppn, normal_perm) = normalPage.r_resp_apply(i)
    val (s_hit_sameCycle, super_hit, super_ppn, super_perm) = superPage.r_resp_apply(i)
    assert(!(normal_hit && super_hit && vmEnable && RegNext(req(i).valid, init = false.B)))

    val hit = normal_hit || super_hit
    val hit_sameCycle = n_hit_sameCycle || s_hit_sameCycle
    val ppn = Mux(super_hit, super_ppn, normal_ppn)
    val perm = Mux(super_hit, super_perm, normal_perm)

    /** *************** next cycle when two cycle is false******************* */
    val miss = !hit && vmEnable
    val fast_miss = !super_hit && vmEnable
    val miss_sameCycle = !hit_sameCycle && vmEnable
    hit.suggestName(s"hit_${i}")
    miss.suggestName(s"miss_${i}")

    XSDebug(req_out_v(i), p"(${i.U}) hit:${hit} miss:${miss} ppn:${Hexadecimal(ppn)} perm:${perm}\n")

    val paddr = Cat(ppn, get_off(req_out(i).vaddr))
    val vaddr = SignExt(req_out(i).vaddr, PAddrBits)

    resp(i).bits.paddr := Mux(vmEnable, paddr, vaddr)
    resp(i).bits.miss := { if (q.missSameCycle) miss_sameCycle else miss }
    resp(i).bits.fast_miss := fast_miss
    resp(i).bits.ptwBack := io.ptw.resp.fire()

    val pmp_paddr = Mux(vmEnable, Cat(super_ppn, get_off(req_out(i).vaddr)), vaddr)
    // pmp_paddr seems same to paddr functionally. It abandons normal_ppn for timing optimization.
    val static_pm = normal_perm.pm
    val static_pm_valid = !super_hit && vmEnable && q.partialStaticPMP.B

    (hit, miss, pmp_paddr, static_pm, static_pm_valid, perm)
  }

  def pmp_check(addr: UInt, size: UInt, cmd: UInt, idx: Int): Unit = {
    pmp(idx).valid := resp(idx).valid
    pmp(idx).bits.addr := addr
    pmp(idx).bits.size := size
    pmp(idx).bits.cmd := cmd
  }

  def perm_check(perm: TlbPermBundle, cmd: UInt, spm: TlbPMBundle, spm_v: Bool, idx: Int) = {
    // for timing optimization, pmp check is divided into dynamic and static
    // dynamic: superpage (or full-connected reg entries) -> check pmp when translation done
    // static: 4K pages (or sram entries) -> check pmp with pre-checked results
    val af = perm.af
    val pf = perm.pf
    val ldUpdate = !perm.a && TlbCmd.isRead(cmd) && !TlbCmd.isAmo(cmd) // update A/D through exception
    val stUpdate = (!perm.a || !perm.d) && (TlbCmd.isWrite(cmd) || TlbCmd.isAmo(cmd)) // update A/D through exception
    val instrUpdate = !perm.a && TlbCmd.isExec(cmd) // update A/D through exception
    val modeCheck = !(mode === ModeU && !perm.u || mode === ModeS && perm.u && (!priv.sum || ifecth))
    val ldPermFail = !(modeCheck && (perm.r || priv.mxr && perm.x))
    val stPermFail = !(modeCheck && perm.w)
    val instrPermFail = !(modeCheck && perm.x)
    val ldPf = (ldPermFail || pf) && (TlbCmd.isRead(cmd) && !TlbCmd.isAmo(cmd))
    val stPf = (stPermFail || pf) && (TlbCmd.isWrite(cmd) || TlbCmd.isAmo(cmd))
    val instrPf = (instrPermFail || pf) && TlbCmd.isExec(cmd)
    val fault_valid = vmEnable
    resp(idx).bits.excp.pf.ld := (ldPf || ldUpdate) && fault_valid && !af
    resp(idx).bits.excp.pf.st := (stPf || stUpdate) && fault_valid && !af
    resp(idx).bits.excp.pf.instr := (instrPf || instrUpdate) && fault_valid && !af
    // NOTE: pf need && with !af, page fault has higher priority than access fault
    // but ptw may also have access fault, then af happens, the translation is wrong.
    // In this case, pf has lower priority than af

    // for tlb without sram, tlb will miss, pm should be ignored outsize
    resp(idx).bits.excp.af.ld    := (af || (spm_v && !spm.r)) && TlbCmd.isRead(cmd) && fault_valid
    resp(idx).bits.excp.af.st    := (af || (spm_v && !spm.w)) && TlbCmd.isWrite(cmd) && fault_valid
    resp(idx).bits.excp.af.instr := (af || (spm_v && !spm.x)) && TlbCmd.isExec(cmd) && fault_valid
    resp(idx).bits.static_pm.valid := spm_v && fault_valid // ls/st unit should use this mmio, not the result from pmp
    resp(idx).bits.static_pm.bits := !spm.c
  }

  val readResult = (0 until Width).map(TLBRead(_))
  val hitVec = readResult.map(_._1)
  val missVec = readResult.map(_._2)
  val pmp_addr = readResult.map(_._3)
  val static_pm = readResult.map(_._4)
  val static_pm_v = readResult.map(_._5)
  val perm = readResult.map(_._6)

  (0 until Width).foreach{i =>
    pmp_check(pmp_addr(i), req_out(i).size, req_out(i).cmd, i)
    perm_check(perm(i), req_out(i).cmd, static_pm(i), static_pm_v(i), i)
  }

  // replacement
  def get_access(one_hot: UInt, valid: Bool): Valid[UInt] = {
    val res = Wire(Valid(UInt(log2Up(one_hot.getWidth).W)))
    res.valid := Cat(one_hot).orR && valid
    res.bits := OHToUInt(one_hot)
    res
  }

  val normal_refill_idx = if (q.outReplace) {
    io.replace.normalPage.access <> normalPage.access
    io.replace.normalPage.chosen_set := get_set_idx(io.ptw.resp.bits.entry.tag, q.normalNSets)
    io.replace.normalPage.refillIdx
  } else if (q.normalAssociative == "fa") {
    val re = ReplacementPolicy.fromString(q.normalReplacer, q.normalNWays)
    re.access(normalPage.access.map(_.touch_ways)) // normalhitVecVec.zipWithIndex.map{ case (hv, i) => get_access(hv, validRegVec(i))})
    re.way
  } else { // set-acco && plru
    val re = ReplacementPolicy.fromString(q.normalReplacer, q.normalNSets, q.normalNWays)
    re.access(normalPage.access.map(_.sets), normalPage.access.map(_.touch_ways))
    re.way(get_set_idx(io.ptw.resp.bits.entry.tag, q.normalNSets))
  }

  val super_refill_idx = if (q.outReplace) {
    io.replace.superPage.access <> superPage.access
    io.replace.superPage.chosen_set := DontCare
    io.replace.superPage.refillIdx
  } else {
    val re = ReplacementPolicy.fromString(q.superReplacer, q.superNWays)
    re.access(superPage.access.map(_.touch_ways))
    re.way
  }

  val refill = ptw.resp.fire() && !sfence.valid && !satp.changed
  normalPage.w_apply(
    valid = { if (q.normalAsVictim) false.B
    else refill && ptw.resp.bits.entry.level.get === 2.U },
    wayIdx = normal_refill_idx,
    data = ptw.resp.bits,
    data_replenish = io.ptw_replenish
  )
  superPage.w_apply(
    valid = { if (q.normalAsVictim) refill
    else refill && ptw.resp.bits.entry.level.get =/= 2.U },
    wayIdx = super_refill_idx,
    data = ptw.resp.bits,
    data_replenish = io.ptw_replenish
  )

  // if sameCycle, just req.valid
  // if !sameCycle, add one more RegNext based on !sameCycle's RegNext 
  // because sram is too slow and dtlb is too distant from dtlbRepeater
  handle_block(Width, q.sameCycle)
  def need_RegNext[T <: Data](need: Boolean, data: T): T = {
    if (need) RegNext(data)
    else data
  }
  def need_RegNextInit[T <: Data](need: Boolean, data: T, init_value: T): T = {
    if (need) RegNext(data, init = init_value)
    else data
  }

  def param_choose[T <: Data](need: Boolean, truedata: T, falsedata: T): T = {
    if (need) truedata
    else falsedata
  }

  val result_ok = { if (q.sameCycle) req_in.map(_.fire()) else req_in.map(a => RegNext(a.fire())) }
  if (!q.shouldBlock) {
    for (i <- 0 until Width) {
      XSPerfAccumulate("first_access" + Integer.toString(i, 10), result_ok(i) && vmEnable && RegNext(req(i).bits.debug.isFirstIssue))
      XSPerfAccumulate("access" + Integer.toString(i, 10), result_ok(i) && vmEnable)
    }
    for (i <- 0 until Width) {
      XSPerfAccumulate("first_miss" + Integer.toString(i, 10), result_ok(i) && vmEnable && missVec(i) && RegNext(req(i).bits.debug.isFirstIssue))
      XSPerfAccumulate("miss" + Integer.toString(i, 10), result_ok(i) && vmEnable && missVec(i))
    }
  } else {
    // NOTE: ITLB is blocked, so every resp will be valid only when hit
    // every req will be ready only when hit
    for (i <- 0 until Width) {
      XSPerfAccumulate(s"access${i}",result_ok(i)  && vmEnable)
      XSPerfAccumulate(s"miss${i}", result_ok(i) && missVec(i))
    }
  }
  //val reqCycleCnt = Reg(UInt(16.W))
  //reqCycleCnt := reqCycleCnt + BoolStopWatch(ptw.req(0).fire(), ptw.resp.fire || sfence.valid)
  //XSPerfAccumulate("ptw_req_count", ptw.req.fire())
  //XSPerfAccumulate("ptw_req_cycle", Mux(ptw.resp.fire(), reqCycleCnt, 0.U))
  XSPerfAccumulate("ptw_resp_count", ptw.resp.fire())
  XSPerfAccumulate("ptw_resp_pf_count", ptw.resp.fire() && ptw.resp.bits.pf)

  // Log
  for(i <- 0 until Width) {
    XSDebug(req(i).valid, p"req(${i.U}): (${req(i).valid} ${req(i).ready}) ${req(i).bits}\n")
    XSDebug(resp(i).valid, p"resp(${i.U}): (${resp(i).valid} ${resp(i).ready}) ${resp(i).bits}\n")
  }

  XSDebug(sfence.valid, p"Sfence: ${sfence}\n")
  XSDebug(ParallelOR(req_out_v) || ptw.resp.valid, p"vmEnable:${vmEnable} hit:${Binary(VecInit(hitVec).asUInt)} miss:${Binary(VecInit(missVec).asUInt)}\n")
  for (i <- ptw.req.indices) {
    XSDebug(ptw.req(i).fire(), p"PTW req:${ptw.req(i).bits}\n")
  }
  XSDebug(ptw.resp.valid, p"PTW resp:${ptw.resp.bits} (v:${ptw.resp.valid}r:${ptw.resp.ready}) \n")

  println(s"${q.name}: normal page: ${q.normalNWays} ${q.normalAssociative} ${q.normalReplacer.get} super page: ${q.superNWays} ${q.superAssociative} ${q.superReplacer.get}")

//   // NOTE: just for simple tlb debug, comment it after tlb's debug
  // assert(!io.ptw.resp.valid || io.ptw.resp.bits.entry.tag === io.ptw.resp.bits.entry.ppn, "Simple tlb debug requires vpn === ppn")

  val perfEvents = if(!q.shouldBlock) {
    Seq(
      ("access", PopCount((0 until Width).map(i => vmEnable && result_ok(i)))              ),
      ("miss  ", PopCount((0 until Width).map(i => vmEnable && result_ok(i) && missVec(i)))),
    )
  } else {
    Seq(
      ("access", PopCount((0 until Width).map(i => io.requestor(i).req.fire()))),
      ("miss  ", PopCount((0 until Width).map(i => ptw.req(i).fire()))         ),
    )
  }
  generatePerfEvent()
}

trait TLBBlockHanlerNonBlock{this: TLBBase =>
  override def handle_block(Width: Int, sameCycle: Boolean): Unit = {
    req_out_v.zip(io.requestor).foreach { case (v,r) => 
      r.resp.valid := v
      r.req.ready := r.resp.ready // should always be true
    }
    for (i <- 0 until Width) {
      io.ptw.req(i).valid :=  need_RegNextInit(!sameCycle, req_out_v(i) && missVec(i), false.B) &&
        !RegNext(refill, init = false.B) &&
        param_choose(!sameCycle, !RegNext(RegNext(refill, init = false.B), init = false.B), true.B)
      io.ptw.req(i).bits.vpn := need_RegNext(!sameCycle, need_RegNext(!sameCycle, get_pn(req_out(i).vaddr)))
    }
    io.ptw.resp.ready := true.B
  }
}

trait TLBBlockHandlerBlock{this: TLBBase =>
  override def handle_block(Width: Int, sameCycle: Boolean): Unit = {
    val miss = Wire(Vec(Width, Bool())) // TODO
    val miss_req = Reg(Vec(Width, Valid(new PtwReq))) // this valid for if req (not) sent to ptw
    val miss_v = Reg(Vec(Width, Bool())) // this valid for if miss, try to unset resp.valid
    req_out_v.zip(io.requestor).foreach { 
      case (v,r) => r.req.ready := !v 
    } // req_out_v for if there is a request

    for (i <- 0 until Width) {
      // miss request entries
      resp(i).valid := req_out_v(i) && !miss(i) && !miss_v(i)
      when (miss(i)) { // TODO: bypass ptw's resp
        miss_v(i) := true.B
        miss_req(i).valid := true.B
        miss_req(i).bits.vpn := get_pn(req_out(i).vaddr)
      }
      // when ptw resp, check if hit, reset miss_v, resp to lsu/ifu
      when (io.ptw.resp.fire()) {
        val hit = io.ptw.resp.bits.entry.hit(miss_req(i).bits.vpn, satp.asid, allType = true)
        when (hit && req_out_v(i)) {
          resp(i).valid := true.B
          resp(i).bits.miss := false.B // for blocked tlb, this is useless
        }
        // NOTE: the unfiltered req would be handled by Repeater
      }
    }
    // send reqs to ptw
    if (io.ptw.req.length == 1) {
      val ptw_arb = Module(new RRArbiter(new PtwReq(), Width))
      ptw_arb.io.in <> miss_req // DecoupledIO <> ValidIO
      when (ptw_arb.io.out.fire()) {
        miss_req(ptw_arb.io.chosen).valid := false.B
      }
      io.ptw.req(0).valid := ptw_arb.io.out.valid
      io.ptw.req(0).bits.vpn := ptw_arb.io.out.bits.vpn
    } else {
      require(io.ptw.req.length == Width) // NOTE: or we need a M2N Arbiter
      io.ptw.req.zip(miss_req).foreach { case(p,m) =>
        p.valid := m.valid
        p.bits.vpn := m.bits.vpn
        when (p.fire()) { m.valid := false.B }
      }
    }

    // TODO: how to handle sfence.valid? when flush happen, request will be dropped 
    when (flush) {
      // req_out_v.map(_ := false.B) // done upside
      miss_req.map(_.valid := false.B)
      miss_v.map(_ := false.B)
    }
  }
}

class TLBBlock(Width: Int, q: TLBParameters)(implicit p: Parameters) extends TLBBase(Width, q) with TLBBlockHandlerBlock
class TLBNonBlock(Width: Int, q: TLBParameters)(implicit p: Parameters) extends TLBBase(Width, q) with TLBBlockHanlerNonBlock

class TlbReplace(Width: Int, q: TLBParameters)(implicit p: Parameters) extends TlbModule {
  val io = IO(new TlbReplaceIO(Width, q))

  if (q.normalAssociative == "fa") {
    val re = ReplacementPolicy.fromString(q.normalReplacer, q.normalNWays)
    re.access(io.normalPage.access.map(_.touch_ways))
    io.normalPage.refillIdx := re.way
  } else { // set-acco && plru
    val re = ReplacementPolicy.fromString(q.normalReplacer, q.normalNSets, q.normalNWays)
    re.access(io.normalPage.access.map(_.sets), io.normalPage.access.map(_.touch_ways))
    io.normalPage.refillIdx := { if (q.normalNWays == 1) 0.U else re.way(io.normalPage.chosen_set) }
  }

  if (q.superAssociative == "fa") {
    val re = ReplacementPolicy.fromString(q.superReplacer, q.superNWays)
    re.access(io.superPage.access.map(_.touch_ways))
    io.superPage.refillIdx := re.way
  } else { // set-acco && plru
    val re = ReplacementPolicy.fromString(q.superReplacer, q.superNSets, q.superNWays)
    re.access(io.superPage.access.map(_.sets), io.superPage.access.map(_.touch_ways))
    io.superPage.refillIdx := { if (q.superNWays == 1) 0.U else re.way(io.superPage.chosen_set) }
  }
}

// object TLB {
//   def apply
//   (
//     in: Seq[BlockTlbRequestIO],
//     sfence: SfenceBundle,
//     csr: TlbCsrBundle,
//     width: Int,
//     shouldBlock: Boolean,
//     q: TLBParameters
//   )(implicit p: Parameters) = {
//     require(in.length == width)

//     val tlb = Module(new TLB(width, q))

//     tlb.io.sfence <> sfence
//     tlb.io.csr <> csr
//     tlb.suggestName(s"tlb_${q.name}")

//     if (!shouldBlock) { // dtlb
//       for (i <- 0 until width) {
//         tlb.io.requestor(i) <> in(i)
//         // tlb.io.requestor(i).req.valid := in(i).req.valid
//         // tlb.io.requestor(i).req.bits := in(i).req.bits
//         // in(i).req.ready := tlb.io.requestor(i).req.ready

//         // in(i).resp.valid := tlb.io.requestor(i).resp.valid
//         // in(i).resp.bits := tlb.io.requestor(i).resp.bits
//         // tlb.io.requestor(i).resp.ready := in(i).resp.ready
//       }
//     } else { // itlb
//       //require(width == 1)
//       (0 until width).map{ i =>
//         tlb.io.requestor(i).req.valid := in(i).req.valid
//         tlb.io.requestor(i).req.bits := in(i).req.bits
//         in(i).req.ready := !tlb.io.requestor(i).resp.bits.miss && in(i).resp.ready && tlb.io.requestor(i).req.ready

//         require(q.missSameCycle || q.sameCycle)
//         // NOTE: the resp.valid seems to be useless, it must be true when need
//         //       But don't know what happens when true but not need, so keep it correct value, not just true.B
//         if (q.missSameCycle && !q.sameCycle) {
//           in(i).resp.valid := tlb.io.requestor(i).resp.valid && !RegNext(tlb.io.requestor(i).resp.bits.miss)
//         } else {
//           in(i).resp.valid := tlb.io.requestor(i).resp.valid && !tlb.io.requestor(i).resp.bits.miss
//         }
//         in(i).resp.bits := tlb.io.requestor(i).resp.bits
//         tlb.io.requestor(i).resp.ready := in(i).resp.ready
//       }
//       tlb.io.ptw_replenish <> DontCare // itlb only use reg, so no static pmp/pma
//     }
//     tlb.io.ptw
//   }
// }
