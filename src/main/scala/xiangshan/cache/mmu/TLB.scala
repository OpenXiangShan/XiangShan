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

  val reqAddr = req.map(_.bits.vaddr.asTypeOf((new vaBundle).cloneType))
  val vpn = reqAddr.map(_.vpn)
  val cmd     = req.map(_.bits.cmd)
  val valid   = req.map(_.valid)

  def widthMapSeq[T <: Seq[Data]](f: Int => T) = (0 until Width).map(f)
  def widthMap[T <: Data](f: Int => T) = (0 until Width).map(f)

  // Normal page && Super page
  val nv = RegInit(VecInit(Seq.fill(q.normalSize)(false.B)))
  val nentries = Reg(Vec(q.normalSize, new TlbEntry(true, false)))
  val sv = RegInit(VecInit(Seq.fill(q.superSize)(false.B)))
  val sentries = Reg(Vec(q.superSize, new TlbEntry(false, true)))
  val v = nv ++ sv
  val entries = nentries ++ sentries
  val g = VecInit(entries.map(_.perm.g))

  /**
    * PTW refill
    */
  val refill = ptw.resp.fire() && !sfence.valid

  val nReplace = ReplacementPolicy.fromString(q.normalReplacer, q.normalSize)
  val sReplace = ReplacementPolicy.fromString(q.superReplacer, q.superSize)
  val nRefillIdx = replaceWrapper(nv, nReplace.way)
  val sRefillIdx = replaceWrapper(sv, sReplace.way)

  when (refill) {
    val resp = ptw.resp.bits
    when (resp.entry.level.getOrElse(0.U) === 2.U) {
      nv(nRefillIdx) := true.B
      nentries(nRefillIdx).apply(resp)
      nReplace.access(nRefillIdx)
      XSDebug(p"Refill normal: idx:${nRefillIdx} entry:${resp.entry} pf:${resp.pf}\n")
    }.otherwise {
      sv(sRefillIdx) := true.B
      sentries(sRefillIdx).apply(resp)
      sReplace.access(sRefillIdx)
      XSDebug(p"Refill superpage: idx:${sRefillIdx} entry:${resp.entry} pf:${resp.pf}\n")
    }
  }

  val nRefillMask = Mux(refill && ptw.resp.bits.entry.level.get === 2.U, UIntToOH(nRefillIdx)(q.normalSize-1, 0), 0.U).asBools
  val sRefillMask = Mux(refill && ptw.resp.bits.entry.level.get =/= 2.U, UIntToOH(sRefillIdx)(q.superSize-1, 0), 0.U).asBools

  def tlb_read(vpn: UInt, vVec: Vec[Bool], entryVec: Vec[TlbEntry], associative: String, mask: Seq[Bool]) = {
//    if (associative == "fa") {
      val hitVec = VecInit(entryVec.zip(vVec zip mask).map{ case (e, m) => e.hit(vpn) && m._1 && !m._2})
      val hitVecReg = if (q.sameCycle) hitVec else RegNext(hitVec)
      val vpnReg = if (q.sameCycle) vpn else RegNext(vpn)
      /******************* next cycle if same cycle is false *****************/
      val hit = ParallelOR(hitVecReg).asBool
      val ppn = ParallelMux(hitVecReg zip entryVec.map(_.genPPN(vpnReg)))
      val perm = ParallelMux(hitVecReg zip entryVec.map(_.perm))
      (hit, ppn, perm, hitVecReg)
//    }
  }

  def TLBNormalRead(i: Int) = {
    val (normal_hit, normal_ppn, normal_perm, normal_hitVec) = tlb_read(vpn(i), nv, nentries, q.normalAssociative, nRefillMask)
    val (super_hit, super_ppn, super_perm, super_hitVec) = tlb_read(vpn(i), sv, sentries, q.superAssociative, sRefillMask)
    assert(!(normal_hit && super_hit))

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

    (hit, miss, normal_hitVec ++ super_hitVec, validReg)
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
  val nAccess = hitVecVec.map(a => get_access(a, q.normalSize - 1, 0))
  val sAccess = hitVecVec.map(a => get_access(a, q.normalSize + q.superSize - 1, q.normalSize))
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

  // val tooManyPf = PopCount(pf) > 5.U
  // when (tooManyPf) { // when too much pf, just clear
  //   XSDebug(p"Too many pf just flush all the pf v:${Hexadecimal(VecInit(v).asUInt)} pf:${Hexadecimal(pf.asUInt)}\n")
  //   v.zipWithIndex.map{ case (a, i) => a := a & !pf(i) }
  // }

  // sfence (flush)
  val sfence_vpn = sfence.bits.addr.asTypeOf(new vaBundle().cloneType).vpn
  val sfenceHit = entries.map(_.hit(sfence_vpn))
  when (sfence.valid) {
    when (sfence.bits.rs1) { // virtual address *.rs1 <- (rs1===0.U)
      when (sfence.bits.rs2) { // asid, but i do not want to support asid, *.rs2 <- (rs2===0.U)
        // all addr and all asid
        v.map(_ := false.B)
      }.otherwise {
        // all addr but specific asid
        v.zipWithIndex.map{ case (a,i) => a := a & g(i) }
      }
    }.otherwise {
      when (sfence.bits.rs2) {
        // specific addr but all asid
        v.zipWithIndex.map{ case (a,i) => a := a & !sfenceHit(i) }
      }.otherwise {
        // specific addr and specific asid
        v.zipWithIndex.map{ case (a,i) => a := a & !(sfenceHit(i) && !g(i)) }
      }
    }
  }

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
  for (i <- 0 until q.normalSize) {
    val indexHitVec = hitVecVec.zip(validRegVec).map{ case (h, v) => h(i) && v }
    XSPerfAccumulate(s"NormalAccessIndex${i}", Mux(vmEnable, PopCount(indexHitVec), 0.U))
  }
  for (i <- 0 until q.superSize) {
    val indexHitVec = hitVecVec.zip(validRegVec).map{ case (h, v) => h(i + q.normalSize) && v }
    XSPerfAccumulate(s"SuperAccessIndex${i}", Mux(vmEnable, PopCount(indexHitVec), 0.U))
  }
  for (i <- 0 until q.normalSize) {
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
  XSDebug(ParallelOR(valid) || ptw.resp.valid, p"vmEnable:${vmEnable} hit:${Binary(VecInit(hitVec).asUInt)} miss:${Binary(VecInit(missVec).asUInt)} v:${Hexadecimal(VecInit(v).asUInt)}\n")
  for (i <- ptw.req.indices) {
    XSDebug(ptw.req(i).fire(), p"PTW req:${ptw.req(i).bits}\n")
  }
  XSDebug(ptw.resp.valid, p"PTW resp:${ptw.resp.bits} (v:${ptw.resp.valid}r:${ptw.resp.ready}) \n")

  println(s"${q.name}: normal page: ${q.normalSize} ${q.normalAssociative} ${q.normalReplacer.get} super page: ${q.superSize} ${q.superAssociative} ${q.superReplacer.get}")

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
