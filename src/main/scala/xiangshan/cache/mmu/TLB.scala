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
import chisel3.util._
import freechips.rocketchip.util.SRAMAnnotation
import xiangshan._
import utils._
import xiangshan.backend.roq.RoqPtr
import xiangshan.backend.fu.util.HasCSRConst



class TLB(Width: Int, q: TLBParameters)(implicit p: Parameters) extends TlbModule with HasCSRConst{
  val io = IO(new TlbIO(Width))

  require(q.superAssociative == "fa")
  if (q.sameCycle) {
    require(q.normalAssociative == "fa")
  }

  val req    = io.requestor.map(_.req)
  val resp   = io.requestor.map(_.resp)
  val ptw    = io.ptw

  val sfence = io.sfence
  val csr    = io.csr
  val satp   = csr.satp
  val priv   = csr.priv
  val ifecth = if (q.fetchi) true.B else false.B
  val mode   = if (q.useDmode) priv.dmode else priv.imode
  // val vmEnable = satp.mode === 8.U // && (mode < ModeM) // FIXME: fix me when boot xv6/linux...
  val vmEnable = if(EnbaleTlbDebug) (satp.mode === 8.U)
                 else               (satp.mode === 8.U && (mode < ModeM))

  val reqAddr = req.map(_.bits.vaddr.asTypeOf((new VaBundle).cloneType))
  val vpn = reqAddr.map(_.vpn)
  val cmd     = req.map(_.bits.cmd)
  val valid   = req.map(_.valid)

  def widthMapSeq[T <: Seq[Data]](f: Int => T) = (0 until Width).map(f)
  def widthMap[T <: Data](f: Int => T) = (0 until Width).map(f)

  // Normal page && Super page
  val normalPage = TlbStorage(
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
    associative = q.superAssociative,
    sameCycle = q.sameCycle,
    ports = Width,
    nSets = 1,
    nWays = q.superSize,
    sramSinglePort = sramSinglePort,
    normalPage = q.normalAsVictim,
    superPage = true,
  )

  val refill = ptw.resp.fire() && !sfence.valid
  val nReplace = ReplacementPolicy.fromString(q.normalReplacer, q.normalNWays)
  val sReplace = ReplacementPolicy.fromString(q.superReplacer, q.superSize)
  val nRefillIdx = nReplace.way
  val sRefillIdx = sReplace.way

  for (i <- 0 until Width) {
    normalPage.r_req_apply(
      valid = io.requestor(i).req.valid,
      wayIdx = get_idx(vpn(i), q.normalNWays),
      vpn = vpn(i),
      i = i
    )
    superPage.r_req_apply(
      valid = io.requestor(i).req.valid,
      wayIdx = get_idx(vpn(i), q.superSize),
      vpn = vpn(i),
      i = i
    )
  }
  normalPage.w_apply(
    valid = { if (q.normalAsVictim) false.B
              else refill && ptw.resp.bits.entry.level.get === 2.U },
    wayIdx = nRefillIdx,
    data = ptw.resp.bits
  )
  superPage.w_apply(
    valid = { if (q.normalAsVictim) refill
              else refill && ptw.resp.bits.entry.level.get =/= 2.U },
    wayIdx = nRefillIdx,
    data = ptw.resp.bits
  )

  normalPage.victim.in <> superPage.victim.out
  normalPage.victim.out <> superPage.victim.in
  normalPage.sfence <> io.sfence
  superPage.sfence <> io.sfence

  def TLBNormalRead(i: Int) = {
    val (normal_hit, normal_ppn, normal_perm, normal_hitVec) = normalPage.r_resp_apply(i)
    val (super_hit, super_ppn, super_perm, super_hitVec) = superPage.r_resp_apply(i)
    assert(!(normal_hit && super_hit && vmEnable && RegNext(req(i).valid, init = false.B)))

    val hit = normal_hit || super_hit
    val ppn = Mux(normal_hit, normal_ppn, super_ppn)
    val perm = Mux(normal_hit, normal_perm, super_perm)

    val pf = perm.pf && hit
    val cmdReg = if (!q.sameCycle) RegNext(cmd(i)) else cmd(i)
    val validReg = if (!q.sameCycle) RegNext(valid(i)) else valid(i)
    val offReg = if (!q.sameCycle) RegNext(reqAddr(i).off) else reqAddr(i).off

    /***************** next cycle when two cycle is false********************/
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

    val update = hit && (!perm.a || !perm.d && TlbCmd.isWrite(cmdReg)) // update A/D through exception
    val modeCheck = !(mode === ModeU && !perm.u || mode === ModeS && perm.u && (!priv.sum || ifecth))
    val ldPf = !(modeCheck && (perm.r || priv.mxr && perm.x)) && (TlbCmd.isRead(cmdReg) && true.B/* TODO !isAMO*/)
    val stPf = !(modeCheck && perm.w) && (TlbCmd.isWrite(cmdReg) || false.B/*TODO isAMO. */)
    val instrPf = !(modeCheck && perm.x) && TlbCmd.isExec(cmdReg)
    resp(i).bits.excp.pf.ld    := (ldPf || update || pf) && vmEnable && hit
    resp(i).bits.excp.pf.st    := (stPf || update || pf) && vmEnable && hit
    resp(i).bits.excp.pf.instr := (instrPf || update || pf) && vmEnable && hit

    // if vmenable, use pre-calcuated pma check result
    resp(i).bits.mmio := Mux(TlbCmd.isExec(cmdReg), !perm.pi, !perm.pd) && vmEnable && hit
    resp(i).bits.excp.af.ld    := Mux(TlbCmd.isAtom(cmdReg), !perm.pa, !perm.pr) && TlbCmd.isRead(cmdReg) && vmEnable && hit
    resp(i).bits.excp.af.st    := Mux(TlbCmd.isAtom(cmdReg), !perm.pa, !perm.pw) && TlbCmd.isWrite(cmdReg) && vmEnable && hit
    resp(i).bits.excp.af.instr := Mux(TlbCmd.isAtom(cmdReg), false.B, !perm.pe) && vmEnable && hit

    // if !vmenable, check pma
    val (pmaMode, accessWidth) = AddressSpace.memmapAddrMatch(resp(i).bits.paddr)
    when(!vmEnable){
      resp(i).bits.mmio := Mux(TlbCmd.isExec(cmdReg), !PMAMode.icache(pmaMode), !PMAMode.dcache(pmaMode))
      resp(i).bits.excp.af.ld    := Mux(TlbCmd.isAtom(cmdReg), !PMAMode.atomic(pmaMode), !PMAMode.read(pmaMode)) && TlbCmd.isRead(cmdReg)
      resp(i).bits.excp.af.st    := Mux(TlbCmd.isAtom(cmdReg), !PMAMode.atomic(pmaMode), !PMAMode.write(pmaMode)) && TlbCmd.isWrite(cmdReg)
      resp(i).bits.excp.af.instr := Mux(TlbCmd.isAtom(cmdReg), false.B, !PMAMode.execute(pmaMode))
    }

    (hit, miss, normal_hitVec.asBools() ++ super_hitVec.asBools(), validReg)
  }

  val readResult = (0 until Width).map(TLBNormalRead(_))
  val hitVec = readResult.map(res => res._1)
  val missVec = readResult.map(res => res._2)
  val hitVecVec = readResult.map(res => res._3)
  val validRegVec = readResult.map(res => res._4)

  // replacement
  def get_access_index(one_hot: UInt): Valid[UInt] = {
    val res = Wire(Valid(UInt(log2Up(one_hot.getWidth).W)))
    res.valid := Cat(one_hot).orR
    res.bits := OHToUInt(one_hot)
    res
  }
  def get_access(one_hot: Seq[Bool], stop: Int, start: Int): Valid[UInt] = {
    val tmp = VecInit(one_hot).asUInt
    get_access_index(tmp(stop, start))
  }
  val nAccess = hitVecVec.map(a => get_access(a, q.normalNWays - 1, 0))
  val sAccess = hitVecVec.map(a => get_access(a, q.normalNWays + q.superSize - 1, q.normalNWays))
  if (Width == 1) {
    when (nAccess(0).valid) { nReplace.access(nAccess(0).bits) }
    when (sAccess(0).valid) { sReplace.access(sAccess(0).bits) }
  } else {
    nReplace.access(nAccess)
    sReplace.access(sAccess)
  }

  for (i <- 0 until Width) {
    io.ptw.req(i).valid := validRegVec(i) && missVec(i) && !RegNext(refill)
    io.ptw.req(i).bits.vpn := RegNext(reqAddr(i).vpn)
  }
  io.ptw.resp.ready := true.B

  if (!q.sameCycle) {
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
    XSPerfAccumulate("access", io.requestor(0).req.fire() && vmEnable)
    XSPerfAccumulate("miss", ptw.req(0).fire())
  }
  //val reqCycleCnt = Reg(UInt(16.W))
  //reqCycleCnt := reqCycleCnt + BoolStopWatch(ptw.req(0).fire(), ptw.resp.fire || sfence.valid)
  //XSPerfAccumulate("ptw_req_count", ptw.req.fire())
  //XSPerfAccumulate("ptw_req_cycle", Mux(ptw.resp.fire(), reqCycleCnt, 0.U))
  XSPerfAccumulate("ptw_resp_count", ptw.resp.fire())
  XSPerfAccumulate("ptw_resp_pf_count", ptw.resp.fire() && ptw.resp.bits.pf)
  for (i <- 0 until q.normalNWays) {
    val indexHitVec = hitVecVec.zip(validRegVec).map{ case (h, v) => h(i) && v }
    XSPerfAccumulate(s"NormalAccessIndex${i}", Mux(vmEnable, PopCount(indexHitVec), 0.U))
  }
  for (i <- 0 until q.superSize) {
    val indexHitVec = hitVecVec.zip(validRegVec).map{ case (h, v) => h(i + q.normalNWays) && v }
    XSPerfAccumulate(s"SuperAccessIndex${i}", Mux(vmEnable, PopCount(indexHitVec), 0.U))
  }
  for (i <- 0 until q.normalNWays) {
    XSPerfAccumulate(s"NormalRefillIndex${i}", refill && ptw.resp.bits.entry.level.getOrElse(0.U) === 2.U && i.U === nRefillIdx)
  }
  for (i <- 0 until q.superSize) {
    XSPerfAccumulate(s"SuperRefillIndex${i}", refill && ptw.resp.bits.entry.level.getOrElse(0.U) =/= 2.U && i.U === sRefillIdx)
  }

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

  println(s"${q.name}: normal page: ${q.normalNWays} ${q.normalAssociative} ${q.normalReplacer.get} super page: ${q.superSize} ${q.superAssociative} ${q.superReplacer.get}")

//   // NOTE: just for simple tlb debug, comment it after tlb's debug
  // assert(!io.ptw.resp.valid || io.ptw.resp.bits.entry.tag === io.ptw.resp.bits.entry.ppn, "Simple tlb debug requires vpn === ppn")
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
      require(width == 1)
      tlb.io.requestor(0).req.valid := in(0).req.valid
      tlb.io.requestor(0).req.bits := in(0).req.bits
      in(0).req.ready := !tlb.io.requestor(0).resp.bits.miss && in(0).resp.ready && tlb.io.requestor(0).req.ready

      in(0).resp.valid := tlb.io.requestor(0).resp.valid && !tlb.io.requestor(0).resp.bits.miss
      in(0).resp.bits := tlb.io.requestor(0).resp.bits
      tlb.io.requestor(0).resp.ready := in(0).resp.ready
    }

    tlb.io.ptw
  }
}
