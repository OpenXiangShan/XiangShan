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


@chiselName
class TLB(Width: Int, q: TLBParameters)(implicit p: Parameters) extends TlbModule with HasCSRConst {
  val io = IO(new TlbIO(Width, q))

  require(q.superAssociative == "fa")
  if (q.sameCycle) {
    require(q.normalAssociative == "fa")
  }

  val req = io.requestor.map(_.req)
  val resp = io.requestor.map(_.resp)
  val ptw = io.ptw
  val pmp = io.pmp

  val sfence = io.sfence
  val csr = io.csr
  val satp = csr.satp
  val priv = csr.priv
  val ifecth = if (q.fetchi) true.B else false.B
  val mode = if (q.useDmode) priv.dmode else priv.imode
  // val vmEnable = satp.mode === 8.U // && (mode < ModeM) // FIXME: fix me when boot xv6/linux...
  val vmEnable = if (EnbaleTlbDebug) (satp.mode === 8.U)
  else (satp.mode === 8.U && (mode < ModeM))

  val reqAddr = req.map(_.bits.vaddr.asTypeOf((new VaBundle).cloneType))
  val vpn = reqAddr.map(_.vpn)
  val cmd = req.map(_.bits.cmd)
  val valid = req.map(_.valid)

  def widthMapSeq[T <: Seq[Data]](f: Int => T) = (0 until Width).map(f)

  def widthMap[T <: Data](f: Int => T) = (0 until Width).map(f)

  // Normal page && Super page
  val normalPage = TlbStorage(
    name = "normal",
    associative = q.normalAssociative,
    sameCycle = q.sameCycle,
    ports = Width,
    nSets = q.normalNSets,
    nWays = q.normalNWays,
    sramSinglePort = sramSinglePort,
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
    normalPage = q.normalAsVictim,
    superPage = true,
  )


  for (i <- 0 until Width) {
    normalPage.r_req_apply(
      valid = io.requestor(i).req.valid,
      vpn = vpn(i),
      asid = csr.satp.asid,
      i = i
    )
    superPage.r_req_apply(
      valid = io.requestor(i).req.valid,
      vpn = vpn(i),
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

  def TLBNormalRead(i: Int) = {
    val (normal_hit, normal_ppn, normal_perm) = normalPage.r_resp_apply(i)
    val (super_hit, super_ppn, super_perm) = superPage.r_resp_apply(i)
    assert(!(normal_hit && super_hit && vmEnable && RegNext(req(i).valid, init = false.B)))

    val hit = normal_hit || super_hit
    val ppn = Mux(normal_hit, normal_ppn, super_ppn)
    val perm = Mux(normal_hit, normal_perm, super_perm)

    val pf = perm.pf && hit
    val af = perm.af && hit
    val cmdReg = if (!q.sameCycle) RegNext(cmd(i)) else cmd(i)
    val validReg = if (!q.sameCycle) RegNext(valid(i)) else valid(i)
    val offReg = if (!q.sameCycle) RegNext(reqAddr(i).off) else reqAddr(i).off
    val sizeReg = if (!q.sameCycle) RegNext(req(i).bits.size) else req(i).bits.size

    /** *************** next cycle when two cycle is false******************* */
    val miss = !hit && vmEnable
    hit.suggestName(s"hit_${i}")
    miss.suggestName(s"miss_${i}")

    XSDebug(validReg, p"(${i.U}) hit:${hit} miss:${miss} ppn:${Hexadecimal(ppn)} perm:${perm}\n")

    val paddr = Cat(ppn, offReg)
    val vaddr = SignExt(req(i).bits.vaddr, PAddrBits)

    req(i).ready := resp(i).ready
    resp(i).valid := validReg
    resp(i).bits.paddr := Mux(vmEnable, paddr, if (!q.sameCycle) RegNext(vaddr) else vaddr)
    resp(i).bits.miss := miss
    resp(i).bits.ptwBack := io.ptw.resp.fire()

    pmp(i).valid := resp(i).valid
    pmp(i).bits.addr := resp(i).bits.paddr
    pmp(i).bits.size := sizeReg
    pmp(i).bits.cmd := cmdReg

    val ldUpdate = hit && !perm.a && TlbCmd.isRead(cmdReg) && !TlbCmd.isAmo(cmdReg) // update A/D through exception
    val stUpdate = hit && (!perm.a || !perm.d) && (TlbCmd.isWrite(cmdReg) || TlbCmd.isAmo(cmdReg)) // update A/D through exception
    val instrUpdate = hit && !perm.a && TlbCmd.isExec(cmdReg) // update A/D through exception
    val modeCheck = !(mode === ModeU && !perm.u || mode === ModeS && perm.u && (!priv.sum || ifecth))
    val ldPermFail = !(modeCheck && (perm.r || priv.mxr && perm.x))
    val stPermFail = !(modeCheck && perm.w)
    val instrPermFail = !(modeCheck && perm.x)
    val ldPf = (ldPermFail || pf) && (TlbCmd.isRead(cmdReg) && !TlbCmd.isAmo(cmdReg))
    val stPf = (stPermFail || pf) && (TlbCmd.isWrite(cmdReg) || TlbCmd.isAmo(cmdReg))
    val fault_valid = vmEnable && hit
    val instrPf = (instrPermFail || pf) && TlbCmd.isExec(cmdReg)
    resp(i).bits.excp.pf.ld := (ldPf || ldUpdate) && fault_valid && !af
    resp(i).bits.excp.pf.st := (stPf || stUpdate) && fault_valid && !af
    resp(i).bits.excp.pf.instr := (instrPf || instrUpdate) && fault_valid && !af
    // NOTE: pf need && with !af, page fault has higher priority than access fault
    // but ptw may also have access fault, then af happens, the translation is wrong.
    // In this case, pf has lower priority than af

    resp(i).bits.excp.af.ld := af && TlbCmd.isRead(cmdReg) && fault_valid
    resp(i).bits.excp.af.st := af && TlbCmd.isWrite(cmdReg) && fault_valid
    resp(i).bits.excp.af.instr := af && TlbCmd.isExec(cmdReg) && fault_valid

    (hit, miss, validReg)
  }

  val readResult = (0 until Width).map(TLBNormalRead(_))
  val hitVec = readResult.map(_._1)
  val missVec = readResult.map(_._2)
  val validRegVec = readResult.map(_._3)

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
    data = ptw.resp.bits
  )
  superPage.w_apply(
    valid = { if (q.normalAsVictim) refill
    else refill && ptw.resp.bits.entry.level.get =/= 2.U },
    wayIdx = super_refill_idx,
    data = ptw.resp.bits
  )

  for (i <- 0 until Width) {
    io.ptw.req(i).valid := validRegVec(i) && missVec(i) && !RegNext(refill)
    io.ptw.req(i).bits.vpn := RegNext(reqAddr(i).vpn)
  }
  io.ptw.resp.ready := true.B

  if (!q.shouldBlock) {
    for (i <- 0 until Width) {
      XSPerfAccumulate("first_access" + Integer.toString(i, 10), validRegVec(i) && vmEnable && RegNext(req(i).bits.debug.isFirstIssue))
      XSPerfAccumulate("access" + Integer.toString(i, 10), validRegVec(i) && vmEnable)
    }
    for (i <- 0 until Width) {
      XSPerfAccumulate("first_miss" + Integer.toString(i, 10), validRegVec(i) && vmEnable && missVec(i) && RegNext(req(i).bits.debug.isFirstIssue))
      XSPerfAccumulate("miss" + Integer.toString(i, 10), validRegVec(i) && vmEnable && missVec(i))
    }
  } else {
    // NOTE: ITLB is blocked, so every resp will be valid only when hit
    // every req will be ready only when hit
    for (i <- 0 until Width) {
      XSPerfAccumulate(s"access${i}", io.requestor(i).req.fire() && vmEnable)
      XSPerfAccumulate(s"miss${i}", ptw.req(i).fire())
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
  XSDebug(ParallelOR(valid)|| ptw.resp.valid, p"CSR: ${csr}\n")
  XSDebug(ParallelOR(valid) || ptw.resp.valid, p"vmEnable:${vmEnable} hit:${Binary(VecInit(hitVec).asUInt)} miss:${Binary(VecInit(missVec).asUInt)}\n")
  for (i <- ptw.req.indices) {
    XSDebug(ptw.req(i).fire(), p"PTW req:${ptw.req(i).bits}\n")
  }
  XSDebug(ptw.resp.valid, p"PTW resp:${ptw.resp.bits} (v:${ptw.resp.valid}r:${ptw.resp.ready}) \n")

  println(s"${q.name}: normal page: ${q.normalNWays} ${q.normalAssociative} ${q.normalReplacer.get} super page: ${q.superNWays} ${q.superAssociative} ${q.superReplacer.get}")

//   // NOTE: just for simple tlb debug, comment it after tlb's debug
  // assert(!io.ptw.resp.valid || io.ptw.resp.bits.entry.tag === io.ptw.resp.bits.entry.ppn, "Simple tlb debug requires vpn === ppn")
  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(2))
  })
    if(!q.shouldBlock) {
      val perfEvents = Seq(
        ("access         ", PopCount((0 until Width).map(i => vmEnable && validRegVec(i)))                                         ),
        ("miss           ", PopCount((0 until Width).map(i => vmEnable && validRegVec(i) && missVec(i)))                           ),
        )
      for (((perf_out,(perf_name,perf)),i) <- perfinfo.perfEvents.perf_events.zip(perfEvents).zipWithIndex) {
        perf_out.incr_step := RegNext(perf)
      }
    } else {
      val perfEvents = Seq(
        ("access         ", PopCount((0 until Width).map(i => io.requestor(i).req.fire()))                           ),
        ("miss           ", PopCount((0 until Width).map(i => ptw.req(i).fire()))                                    ),
      )
      for (((perf_out,(perf_name,perf)),i) <- perfinfo.perfEvents.perf_events.zip(perfEvents).zipWithIndex) {
        perf_out.incr_step := RegNext(perf)
      }
    }
}

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

object TLB {
  def apply
  (
    in: Seq[BlockTlbRequestIO],
    sfence: SfenceBundle,
    csr: TlbCsrBundle,
    width: Int,
    shouldBlock: Boolean,
    q: TLBParameters
  )(implicit p: Parameters) = {
    require(in.length == width)

    val tlb = Module(new TLB(width, q))

    tlb.io.sfence <> sfence
    tlb.io.csr <> csr
    tlb.suggestName(s"tlb_${q.name}")

    if (!shouldBlock) { // dtlb
      for (i <- 0 until width) {
        tlb.io.requestor(i) <> in(i)
        // tlb.io.requestor(i).req.valid := in(i).req.valid
        // tlb.io.requestor(i).req.bits := in(i).req.bits
        // in(i).req.ready := tlb.io.requestor(i).req.ready

        // in(i).resp.valid := tlb.io.requestor(i).resp.valid
        // in(i).resp.bits := tlb.io.requestor(i).resp.bits
        // tlb.io.requestor(i).resp.ready := in(i).resp.ready
      }
    } else { // itlb
      //require(width == 1)
      (0 until width).map{ i =>
        tlb.io.requestor(i).req.valid := in(i).req.valid
        tlb.io.requestor(i).req.bits := in(i).req.bits
        in(i).req.ready := !tlb.io.requestor(i).resp.bits.miss && in(i).resp.ready && tlb.io.requestor(i).req.ready

        in(i).resp.valid := tlb.io.requestor(i).resp.valid && !tlb.io.requestor(i).resp.bits.miss
        in(i).resp.bits := tlb.io.requestor(i).resp.bits
        tlb.io.requestor(i).resp.ready := in(i).resp.ready
      }
    }
    tlb.io.ptw
  }
}
