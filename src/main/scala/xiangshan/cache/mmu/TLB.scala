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

class TLB(Width: Int, isDtlb: Boolean)(implicit p: Parameters) extends TlbModule with HasCSRConst{
  val io = IO(new TlbIO(Width))

  val req    = io.requestor.map(_.req)
  val resp   = io.requestor.map(_.resp)
  val ptw    = io.ptw

  val sfence = io.sfence
  val csr    = io.csr
  val satp   = csr.satp
  val priv   = csr.priv
  val ifecth = if (isDtlb) false.B else true.B
  val mode   = if (isDtlb) priv.dmode else priv.imode
  // val vmEnable = satp.mode === 8.U // && (mode < ModeM) // FIXME: fix me when boot xv6/linux...
  val vmEnable = if(EnbaleTlbDebug) (satp.mode === 8.U)
                 else               (satp.mode === 8.U && (mode < ModeM))

  val reqAddr = req.map(_.bits.vaddr.asTypeOf(vaBundle))
  val cmd     = req.map(_.bits.cmd)
  val valid   = req.map(_.valid)

  def widthMapSeq[T <: Seq[Data]](f: Int => T) = (0 until Width).map(f)
  def widthMap[T <: Data](f: Int => T) = (0 until Width).map(f)

  // Normal page && Super page
  val nv = RegInit(VecInit(Seq.fill(TlbEntrySize)(false.B)))
  val nMeta = Module(new CAMTemplate(UInt(vpnLen.W), TlbEntrySize, Width + 1)).io
  val nData = Reg(Vec(TlbEntrySize, new TlbData(false)))
  val sv = RegInit(VecInit(Seq.fill(TlbSPEntrySize)(false.B)))
  val sMeta = Reg(Vec(TlbSPEntrySize, new TlbSPMeta))
  val sData = Reg(Vec(TlbSPEntrySize, new TlbData(true)))
  val v = nv ++ sv
  val data = nData ++ sData
  val g = VecInit(data.map(_.perm.g))
  val pf = VecInit(data.zip(v).map{ case(e, vi) => e.perm.pf & vi })

  /**
    * PTW refill
    */
  val refill = ptw.resp.fire() && !sfence.valid

  val normalReplacer = if (isDtlb) Some("random") else Some("plru")
  val superReplacer = if (isDtlb) Some("random") else Some("plru")
  val nReplace = ReplacementPolicy.fromString(normalReplacer, TlbEntrySize)
  val sReplace = ReplacementPolicy.fromString(superReplacer, TlbSPEntrySize)
  val nRefillIdx = replaceWrapper(nv, nReplace.way)
  val sRefillIdx = replaceWrapper(sv, sReplace.way)

  nMeta.w := DontCare
  nMeta.w.valid := false.B
  when (refill) {
    val resp = ptw.resp.bits
    when (resp.entry.level.getOrElse(0.U) === 2.U) {
      val refillIdx = nRefillIdx
      refillIdx.suggestName(s"NormalRefillIdx")

      nv(refillIdx) := true.B
      nMeta.w.bits.index := nRefillIdx
      nMeta.w.bits.data  := resp.entry.tag
      nMeta.w.valid := true.B
      nData(refillIdx).apply(
        ppn   = resp.entry.ppn,
        level = resp.entry.level.getOrElse(0.U),
        perm  = VecInit(resp.entry.perm.getOrElse(0.U)).asUInt,
        pf    = resp.pf
      )
      nReplace.access(nRefillIdx)
      XSDebug(p"Refill normal: idx:${refillIdx} entry:${resp.entry} pf:${resp.pf}\n")
    }.otherwise {
      val refillIdx = sRefillIdx
      refillIdx.suggestName(s"SuperRefillIdx")

      val dup = Cat(sv.zip(sMeta).map{ case (v, m) =>
        v && m.hit(resp.entry.tag)
      }).orR // NOTE: may have long latency, RegNext it

      when (!dup) {
        sv(refillIdx) := true.B
        sMeta(refillIdx).apply(
          vpn = resp.entry.tag,
          level = resp.entry.level.getOrElse(0.U)
        )
        sData(refillIdx).apply(
          ppn   = resp.entry.ppn,
          level = resp.entry.level.getOrElse(0.U),
          perm  = VecInit(resp.entry.perm.getOrElse(0.U)).asUInt,
          pf    = resp.pf
        )
        sReplace.access(sRefillIdx)
        XSDebug(p"Refill superpage: idx:${refillIdx} entry:${resp.entry} pf:${resp.pf}\n")
      }
    }
  }

  /**
    * L1 TLB read
    */
  val sfenceVpn = sfence.bits.addr.asTypeOf(vaBundle).vpn
  for (i <- 0 until Width) {
    nMeta.r.req(i) := io.requestor(i).req.bits.vaddr.asTypeOf(vaBundle).vpn
  }
  nMeta.r.req(Width) := sfenceVpn

  val nRefillMask = Mux(refill, UIntToOH(nRefillIdx)(TlbEntrySize-1, 0), 0.U).asBools
  val sRefillMask = Mux(refill, UIntToOH(sRefillIdx)(TlbSPEntrySize-1, 0), 0.U).asBools
  def TLBNormalRead(i: Int) = {
    val entryHitVec = (
      if (isDtlb)
        VecInit(nMeta.r.resp(i).zip(nRefillMask).map{ case (e, m) => ~m && e } ++
                sMeta.zip(sRefillMask).map{ case (e,m) => ~m && e.hit(reqAddr(i).vpn) })
      else
        VecInit(nMeta.r.resp(i) ++ sMeta.map(_.hit(reqAddr(i).vpn/*, satp.asid*/)))
    )

    val reqAddrReg = if (isDtlb) RegNext(reqAddr(i)) else reqAddr(i)
    val cmdReg = if (isDtlb) RegNext(cmd(i)) else cmd(i)
    val validReg = if (isDtlb) RegNext(valid(i)) else valid(i)
    val entryHitVecReg = if (isDtlb) RegNext(entryHitVec) else entryHitVec
    entryHitVecReg.suggestName(s"entryHitVecReg_${i}")

    /***************** next cycle when two cycle is need********************/

    val hitVec  = VecInit((v zip entryHitVecReg).map{ case (a,b) => a&b })
    val pfHitVec   = VecInit((pf zip entryHitVecReg).map{ case (a,b) => a&b })
    val pfArray = ParallelOR(pfHitVec).asBool && validReg && vmEnable
    val hit     = ParallelOR(hitVec).asBool && validReg && vmEnable && ~pfArray
    val miss    = !hit && validReg && vmEnable && ~pfArray
    val hitppn  = ParallelMux(hitVec zip data.map(_.genPPN(reqAddrReg.vpn)))
    val hitPerm = ParallelMux(hitVec zip data.map(_.perm))

    hitVec.suggestName(s"hitVec_${i}")
    pfHitVec.suggestName(s"pfHitVec_${i}")
    hit.suggestName(s"hit_${i}")
    miss.suggestName(s"miss_${i}")
    hitppn.suggestName(s"hitppn_${i}")
    hitPerm.suggestName(s"hitPerm_${i}")

    XSDebug(valid(i), p"(${i.U}) entryHit:${Hexadecimal(entryHitVec.asUInt)}\n")
    XSDebug(validReg, p"(${i.U}) entryHitReg:${Hexadecimal(entryHitVecReg.asUInt)} hitVec:${Hexadecimal(hitVec.asUInt)} pfHitVec:${Hexadecimal(pfHitVec.asUInt)} pfArray:${Hexadecimal(pfArray.asUInt)} hit:${hit} miss:${miss} hitppn:${Hexadecimal(hitppn)} hitPerm:${hitPerm}\n")

    // resp  // TODO: A/D has not being concerned
    val paddr = Cat(hitppn, reqAddrReg.off)
    val vaddr = SignExt(req(i).bits.vaddr, PAddrBits)

    req(i).ready := resp(i).ready
    resp(i).valid := validReg
    resp(i).bits.paddr := Mux(vmEnable, paddr, if (isDtlb) RegNext(vaddr) else vaddr)
    resp(i).bits.miss := miss
    resp(i).bits.ptwBack := io.ptw.resp.fire()

    val perm = hitPerm // NOTE: given the excp, the out module choose one to use?
    val update = false.B && hit && (!hitPerm.a || !hitPerm.d && TlbCmd.isWrite(cmdReg)) // update A/D through exception
    val modeCheck = !(mode === ModeU && !perm.u || mode === ModeS && perm.u && (!priv.sum || ifecth))
    val ldPf = (pfArray && TlbCmd.isRead(cmdReg) && true.B /*!isAMO*/) || hit && !(modeCheck && (perm.r || priv.mxr && perm.x)) && (TlbCmd.isRead(cmdReg) && true.B/*!isAMO*/) // TODO: handle isAMO
    val stPf = (pfArray && TlbCmd.isWrite(cmdReg) || false.B /*isAMO*/ ) || hit && !(modeCheck && perm.w) && (TlbCmd.isWrite(cmdReg) || false.B/*TODO isAMO. */)
    val instrPf = (pfArray && TlbCmd.isExec(cmdReg)) || hit && !(modeCheck && perm.x) && TlbCmd.isExec(cmdReg)
    resp(i).bits.excp.pf.ld    := ldPf || update
    resp(i).bits.excp.pf.st    := stPf || update
    resp(i).bits.excp.pf.instr := instrPf || update

    // if vmenable, use pre-calcuated pma check result
    resp(i).bits.mmio := Mux(TlbCmd.isExec(cmdReg), !perm.pi, !perm.pd)
    resp(i).bits.excp.af.ld    := Mux(TlbCmd.isAtom(cmdReg), !perm.pa, !perm.pr) && TlbCmd.isRead(cmdReg)
    resp(i).bits.excp.af.st    := Mux(TlbCmd.isAtom(cmdReg), !perm.pa, !perm.pw) && TlbCmd.isWrite(cmdReg)
    resp(i).bits.excp.af.instr := Mux(TlbCmd.isAtom(cmdReg), false.B, !perm.pe)

    // if !vmenable, check pma
    val (pmaMode, accessWidth) = AddressSpace.memmapAddrMatch(resp(i).bits.paddr)
    when(!vmEnable){
      resp(i).bits.mmio := Mux(TlbCmd.isExec(cmdReg), !PMAMode.icache(pmaMode), !PMAMode.dcache(pmaMode))
      resp(i).bits.excp.af.ld    := Mux(TlbCmd.isAtom(cmdReg), !PMAMode.atomic(pmaMode), !PMAMode.read(pmaMode)) && TlbCmd.isRead(cmdReg)
      resp(i).bits.excp.af.st    := Mux(TlbCmd.isAtom(cmdReg), !PMAMode.atomic(pmaMode), !PMAMode.write(pmaMode)) && TlbCmd.isWrite(cmdReg)
      resp(i).bits.excp.af.instr := Mux(TlbCmd.isAtom(cmdReg), false.B, !PMAMode.execute(pmaMode))
    }

    // TODO: MMIO check

    (hit, miss, hitVec, validReg)
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
  val nAccess = hitVecVec.map(a => get_access(a, TlbEntrySize - 1, 0))
  val sAccess = hitVecVec.map(a => get_access(a, TlbEntrySize + TlbSPEntrySize - 1, TlbEntrySize))
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
  val sfenceHit = nMeta.r.resp(Width) ++ sMeta.map(_.hit(sfenceVpn))
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
        v.zipWithIndex.map{ case (a,i) => a := a & !sfenceHit(i) && !g(i) }
      }
    }
  }

  if (isDtlb) {
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
  for (i <- 0 until TlbEntrySize) {
    val indexHitVec = hitVecVec.zip(validRegVec).map{ case (h, v) => h(i) && v }
    XSPerfAccumulate(s"NormalAccessIndex${i}", Mux(vmEnable, PopCount(indexHitVec), 0.U))
  }
  for (i <- 0 until TlbSPEntrySize) {
    val indexHitVec = hitVecVec.zip(validRegVec).map{ case (h, v) => h(i + TlbEntrySize) && v }
    XSPerfAccumulate(s"SuperAccessIndex${i}", Mux(vmEnable, PopCount(indexHitVec), 0.U))
  }
  for (i <- 0 until TlbEntrySize) {
    XSPerfAccumulate(s"NormalRefillIndex${i}", refill && ptw.resp.bits.entry.level.getOrElse(0.U) === 2.U && i.U === nRefillIdx)
  }
  for (i <- 0 until TlbSPEntrySize) {
    XSPerfAccumulate(s"SuperRefillIndex${i}", refill && ptw.resp.bits.entry.level.getOrElse(0.U) =/= 2.U && i.U === sRefillIdx)
  }

  // Log
  for(i <- 0 until Width) {
    XSDebug(req(i).valid, p"req(${i.U}): (${req(i).valid} ${req(i).ready}) ${req(i).bits}\n")
    XSDebug(resp(i).valid, p"resp(${i.U}): (${resp(i).valid} ${resp(i).ready}) ${resp(i).bits}\n")
  }

  XSDebug(sfence.valid, p"Sfence: ${sfence}\n")
  XSDebug(ParallelOR(valid)|| ptw.resp.valid, p"CSR: ${csr}\n")
  XSDebug(ParallelOR(valid) || ptw.resp.valid, p"vmEnable:${vmEnable} hit:${Binary(VecInit(hitVec).asUInt)} miss:${Binary(VecInit(missVec).asUInt)} v:${Hexadecimal(VecInit(v).asUInt)} pf:${Hexadecimal(pf.asUInt)}\n")
  for (i <- ptw.req.indices) {
    XSDebug(ptw.req(i).fire(), p"PTW req:${ptw.req(i).bits}\n")
  }
  XSDebug(ptw.resp.valid, p"PTW resp:${ptw.resp.bits} (v:${ptw.resp.valid}r:${ptw.resp.ready}) \n")

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
    isDtlb: Boolean,
    shouldBlock: Boolean
  )(implicit p: Parameters) = {
    require(in.length == width)

    val tlb = Module(new TLB(width, isDtlb))

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
