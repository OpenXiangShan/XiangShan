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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._

class PTWReapterIO(Width: Int)(implicit p: Parameters) extends MMUIOBaseBundle {
  val tlb = Flipped(new TlbPtwIO(Width))
  val ptw = new TlbPtwIO

  def apply(tlb: TlbPtwIO, ptw: TlbPtwIO, sfence: SfenceBundle, csr: TlbCsrBundle): Unit = {
    this.tlb <> tlb
    this.ptw <> ptw
    this.sfence <> sfence
    this.csr <> csr
  }

  def apply(tlb: TlbPtwIO, sfence: SfenceBundle, csr: TlbCsrBundle): Unit = {
    this.tlb <> tlb
    this.sfence <> sfence
    this.csr <> csr
  }

}

class PTWRepeaterNB(Width: Int = 1, passReady: Boolean = false, FenceDelay: Int)(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new PTWReapterIO(Width))

  val req_in = if (Width == 1) {
    io.tlb.req(0)
  } else {
    val arb = Module(new RRArbiterInit(io.tlb.req(0).bits.cloneType, Width))
    arb.io.in <> io.tlb.req
    arb.io.out
  }
  val (tlb, ptw, flush) = (io.tlb, io.ptw, DelayN(io.sfence.valid || io.csr.satp.changed || io.csr.vsatp.changed || io.csr.hgatp.changed || io.csr.priv.virt_changed, FenceDelay))
  /* sent: tlb -> repeater -> ptw
   * recv: ptw -> repeater -> tlb
   */

  // tlb -> repeater -> ptw
  val req = RegEnable(req_in.bits, req_in.fire)
  val sent = BoolStopWatch(req_in.fire, ptw.req(0).fire || flush)
  req_in.ready := !sent || { if (passReady) ptw.req(0).ready else false.B }
  ptw.req(0).valid := sent
  ptw.req(0).bits := req

  // ptw -> repeater -> tlb
  val resp = RegEnable(ptw.resp.bits, ptw.resp.fire)
  val recv = BoolStopWatch(ptw.resp.fire, tlb.resp.fire || flush)
  ptw.resp.ready := !recv || { if (passReady) tlb.resp.ready else false.B }
  tlb.resp.valid := recv
  tlb.resp.bits := resp

  XSPerfAccumulate("req", req_in.fire)
  XSPerfAccumulate("resp", tlb.resp.fire)
  if (!passReady) {
    XSPerfAccumulate("req_blank", req_in.valid && sent && ptw.req(0).ready)
    XSPerfAccumulate("resp_blank", ptw.resp.valid && recv && tlb.resp.ready)
    XSPerfAccumulate("req_blank_ignore_ready", req_in.valid && sent)
    XSPerfAccumulate("resp_blank_ignore_ready", ptw.resp.valid && recv)
  }
  XSDebug(req_in.valid || io.tlb.resp.valid, p"tlb: ${tlb}\n")
  XSDebug(io.ptw.req(0).valid || io.ptw.resp.valid, p"ptw: ${ptw}\n")
}

class PTWFilterIO(Width: Int, hasHint: Boolean = false)(implicit p: Parameters) extends MMUIOBaseBundle {
  val tlb = Flipped(new VectorTlbPtwIO(Width))
  val ptw = new TlbPtwIO()
  val hint = if (hasHint) Some(new TlbHintIO) else None
  val fastHint = Option.when(hasHint)(ValidIO(new TLBHintResp))
  val rob_head_miss_in_tlb = Output(Bool())
  val debugTopDown = new Bundle {
    val robHeadVaddr = Flipped(Valid(UInt(VAddrBits.W)))
  }

  def apply(tlb: VectorTlbPtwIO, ptw: TlbPtwIO, sfence: SfenceBundle, csr: TlbCsrBundle): Unit = {
    this.tlb <> tlb
    this.ptw <> ptw
    this.sfence <> sfence
    this.csr <> csr
  }

  def apply(tlb: VectorTlbPtwIO, sfence: SfenceBundle, csr: TlbCsrBundle): Unit = {
    this.tlb <> tlb
    this.sfence <> sfence
    this.csr <> csr
  }

}

class PTWFilterEntryIO(Width: Int, hasHint: Boolean = false)(implicit p: Parameters) extends PTWFilterIO(Width, hasHint){
  val flush = Input(Bool())
  val refill = Output(Bool())
  val getGpa = Output(Bool())
  val fastResp = if (hasHint) Some(Flipped(ValidIO(new PtwRespS2))) else None
}

class PTWFilterEntry(Width: Int, Size: Int, hasHint: Boolean = false)(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new PTWFilterEntryIO(Width, hasHint))
  require(isPow2(Size), s"Filter Size ($Size) must be a power of 2")

  def firstValidIndex(v: Seq[Bool], valid: Bool): UInt = {
    val index = WireInit(0.U(log2Up(Size).W))
    for (i <- v.indices.reverse) {
      when (v(i) === valid) {
        index := i.U
      }
    }
    index
  }

  val v = RegInit(VecInit(Seq.fill(Size)(false.B)))
  val sent = RegInit(VecInit(Seq.fill(Size)(false.B)))
  val vpn = Reg(Vec(Size, UInt(vpnLen.W)))
  val mptOnly = Option.when(HasMptCheck) (RegInit(Vec(Size, Bool()), 0.U.asTypeOf(Vec(Size, Bool()))))
  val s2xlate = Reg(Vec(Size, UInt(2.W)))
  val getGpa = Reg(Vec(Size, Bool()))

  val enqvalid = WireInit(VecInit(Seq.fill(Width)(false.B)))
  val canenq = WireInit(VecInit(Seq.fill(Width)(false.B)))
  val enqidx = WireInit(VecInit(Seq.fill(Width)(0.U(log2Up(Size).W))))

  val entryIsMatchVec = WireInit(VecInit(Seq.fill(Width)(false.B)))
  val entryMatchIndexVec = WireInit(VecInit(Seq.fill(Width)(0.U(log2Up(Size).W))))
  val ptwResp_EntryMatchVec = vpn.zip(v).zip(s2xlate).map{ case ((pi, vi), s2xlatei) => vi && s2xlatei === io.ptw.resp.bits.s2xlate && io.ptw.resp.bits.hit(pi, io.csr.satp.asid, io.csr.vsatp.asid, io.csr.hgatp.vmid, allType = true)}
  val ptwResp_EntryMatchFirst = firstValidIndex(ptwResp_EntryMatchVec, true.B)
  val ptwResp_ReqMatchVec = io.tlb.req.map(a => io.ptw.resp.valid && a.bits.s2xlate === io.ptw.resp.bits.s2xlate && io.ptw.resp.bits.hit(a.bits.vpn, io.csr.satp.asid, io.csr.vsatp.asid, io.csr.hgatp.vmid, allType = true))

  io.refill := Cat(ptwResp_EntryMatchVec).orR && io.ptw.resp.fire
  io.ptw.resp.ready := true.B
  // DontCare
  io.tlb.req.map(_.ready := true.B)
  io.tlb.resp.valid := false.B
  io.tlb.resp.bits.data := 0.U.asTypeOf(new PtwRespS2WithGpa)
  io.tlb.resp.bits.vector := 0.U.asTypeOf(Vec(Width, Bool()))
  io.tlb.resp.bits.getGpa := 0.U.asTypeOf(Vec(Width, Bool()))
  io.getGpa := false.B

  require(Width > 0 && Width <= 4, s"Filter Width ($Width) must equal or less than 4")
  require(Size >= Width && ((Size & (Size-1)) == 0), s"Filter Size ($Size) must be bigger than Width ($Width) and a power of 2")
  val step = Size / Width
  for (i <- 0 until Width) {
    val start = i * step
    val end = if (i == Width - 1) Size else (i + 1) * step
    val partition = v.slice(start, end)
    canenq(i) := !Cat(partition).andR
    enqidx(i) := firstValidIndex(partition, false.B) + start.U
  }

  for (i <- 0 until Width) {
    enqvalid(i) := io.tlb.req(i).valid && !ptwResp_ReqMatchVec(i) && !entryIsMatchVec(i) && canenq(i)
    when (!enqvalid(i)) {
      enqidx(i) := entryMatchIndexVec(i)
    }

    val entryIsMatch = vpn.zip(v).zip(s2xlate).map{ case ((pi, vi), s2xlatei) => vi && s2xlatei === io.tlb.req(i).bits.s2xlate && pi === io.tlb.req(i).bits.vpn}
    entryIsMatchVec(i) := Cat(entryIsMatch).orR
    entryMatchIndexVec(i) := firstValidIndex(entryIsMatch, true.B)

    if (i > 0) {
      for (j <- 0 until i) {
        val newIsMatch = io.tlb.req(i).bits.vpn === io.tlb.req(j).bits.vpn && io.tlb.req(i).bits.s2xlate === io.tlb.req(j).bits.s2xlate
        when (newIsMatch && io.tlb.req(j).valid) {
          enqidx(i) := enqidx(j)
          canenq(i) := canenq(j)
          enqvalid(i) := false.B
        }
      }
    }

    when (enqvalid(i)) {
      v(enqidx(i)) := true.B
      sent(enqidx(i)) := false.B
      vpn(enqidx(i)) := io.tlb.req(i).bits.vpn
      if (HasMptCheck) {
        mptOnly.get(enqidx(i)) := io.tlb.req(i).bits.mptOnly.get
      }
      s2xlate(enqidx(i)) := io.tlb.req(i).bits.s2xlate
      getGpa(enqidx(i)) := io.tlb.req(i).bits.getGpa
    }
  }

  val issuevec = v.zip(sent).map{ case (v, s) => v && !s}
  val issueindex = firstValidIndex(issuevec, true.B)
  val canissue = Cat(issuevec).orR
  io.ptw.req(0).valid := canissue
  io.ptw.req(0).bits.vpn := vpn(issueindex)
  io.ptw.req(0).bits.s2xlate := s2xlate(issueindex)
  if (HasMptCheck) {
    io.ptw.req(0).bits.mptOnly.get := mptOnly.get(issueindex)
  }
  when (io.ptw.req(0).fire) {
    sent(issueindex) := true.B
  }

  when (io.ptw.resp.fire) {
    v.zip(ptwResp_EntryMatchVec).map{ case (vi, mi) => when (mi) { vi := false.B }}
    io.getGpa := getGpa(ptwResp_EntryMatchFirst)
  }

  when (io.flush) {
    v.map(_ := false.B)
  }

  if (hasHint) {
    val hintIO = io.hint.getOrElse(new TlbHintIO)
    val fastResp = io.fastResp.getOrElse(Flipped(ValidIO(new PtwRespS2)))
    val fastHint = io.fastHint.getOrElse(ValidIO(new TLBHintResp))
    for (i <- hintIO.req.indices) {
      hintIO.req(i).id := enqidx(i)
      hintIO.req(i).full := !canenq(i) || ptwResp_ReqMatchVec(i)
    }
    hintIO.resp.valid := io.refill
    hintIO.resp.bits.id := ptwResp_EntryMatchFirst
    hintIO.resp.bits.replay_all := PopCount(ptwResp_EntryMatchVec) > 1.U

    // Keep the L2 response path short: match only the originating vpn/s2xlate,
    // register the one-hot result, and encode the id from the registered bits.
    val fastRespVpn = Mux(
      fastResp.bits.s2xlate === onlyStage2,
      fastResp.bits.s2.entry.tag(vpnLen - 1, 0),
      Cat(fastResp.bits.s1.entry.tag, fastResp.bits.s1.addr_low)
    )
    val fastMatchVec = VecInit(vpn.zip(v).zip(s2xlate).map { case ((entryVpn, entryValid), entryS2xlate) =>
      entryValid && entryS2xlate === fastResp.bits.s2xlate && entryVpn === fastRespVpn
    }).asUInt
    val fastMatchReg = RegEnable(fastMatchVec, 0.U(Size.W), fastResp.valid)
    val fastRespValidReg = RegNext(fastResp.valid && !io.flush, init = false.B)
    val fastHintId = Cat((0 until log2Up(Size)).reverse.map { bit =>
      val idMask = (0 until Size).foldLeft(BigInt(0)) { case (mask, index) =>
        if (((index >> bit) & 1) == 1) mask.setBit(index) else mask
      }
      (fastMatchReg & idMask.U(Size.W)).orR
    })

    // Multiple matching entries are handled by the original replay_all hint.
    fastHint.valid := fastRespValidReg && fastMatchReg.orR && !io.flush
    fastHint.bits.id := fastHintId
    fastHint.bits.replay_all := false.B
  }

  io.rob_head_miss_in_tlb := VecInit(v.zip(vpn).map{case (vi, vpni) => {
    vi && io.debugTopDown.robHeadVaddr.valid && vpni === get_pn(io.debugTopDown.robHeadVaddr.bits)
  }}).asUInt.orR


  // Perf Counter
  val counter = PopCount(v)
  val fullReqVec = io.tlb.req.indices.map(i =>
    io.tlb.req(i).valid && !ptwResp_ReqMatchVec(i) && !entryIsMatchVec(i) && !canenq(i)
  )
  val sentEntryCount = PopCount(v.zip(sent).map { case (valid, issued) => valid && issued })
  when (counter === 0.U) {
    assert(!io.ptw.req(0).fire, "when counter is 0, should not req")
  }

  XSPerfAccumulate("tlb_req_count", PopCount(Cat(io.tlb.req.map(_.valid))))
  XSPerfAccumulate("tlb_req_count_filtered", PopCount(enqvalid))
  XSPerfAccumulate("tlb_req_full_count", PopCount(fullReqVec))
  XSPerfAccumulate("entry_cycle", counter)
  XSPerfAccumulate("sent_entry_cycle", sentEntryCount)
  XSPerfAccumulate("ptw_req_count", io.ptw.req(0).fire)
  XSPerfAccumulate("ptw_req_blocked", io.ptw.req(0).valid && !io.ptw.req(0).ready)
  XSPerfAccumulate("ptw_resp_match_count", io.refill)

}

class PTWFilter(Width: Int, Size: Int, FenceDelay: Int, isITLB: Boolean = false)(implicit p: Parameters) extends XSModule with HasPtwConst {
  require(Size >= Width)

  private val LdExuCnt = backendParams.LdExuCnt
  private val StaCnt = backendParams.StaCnt
  private val loadWidth = LdExuCnt + PfNumInDtlbLD
  private val groupWidths = if (isITLB) Seq(Width) else Seq(loadWidth, StaCnt, PfNumInDtlbPF)
  private val groupStarts =
    if (isITLB) Seq(0)
    else Seq(0, loadWidth, loadWidth + StaCnt)
  require(Width == groupWidths.sum)

  val io = IO(new PTWFilterIO(Width, hasHint = !isITLB))

  val filters = if (isITLB) {
    Seq(Module(new PTWFilterEntry(Width, Size)).io)
  } else {
    Seq(
      Module(new PTWFilterEntry(loadWidth, loadfiltersize, hasHint = true)).io,
      Module(new PTWFilterEntry(StaCnt, storefiltersize)).io,
      Module(new PTWFilterEntry(PfNumInDtlbPF, prefetchfiltersize)).io
    )
  }
  for (((filter, start), width) <- filters.zip(groupStarts).zip(groupWidths)) {
    for ((request, input) <- filter.tlb.req.zip(io.tlb.req.slice(start, start + width))) {
      request.valid := input.valid
      request.bits := input.bits
    }
  }

  val flush = DelayN(io.sfence.valid || io.csr.satp.changed || io.csr.vsatp.changed || io.csr.hgatp.changed || io.csr.priv.virt_changed, FenceDelay)
  val ptwResp = RegEnable(io.ptw.resp.bits, io.ptw.resp.fire)
  val ptwRespValid = RegNext(io.ptw.resp.fire, init = false.B)
  for (filter <- filters) {
    filter.tlb.resp.ready := true.B
    filter.ptw.resp.valid := ptwRespValid
    filter.ptw.resp.bits := ptwResp
    filter.flush := flush
    filter.sfence := io.sfence
    filter.csr := io.csr
    filter.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  }

  io.tlb.req.map(_.ready := true.B)
  io.tlb.resp.valid := Cat(filters.map(_.refill)).orR
  io.tlb.resp.bits.data.s2xlate := ptwResp.s2xlate
  if (isITLB) {
    io.tlb.resp.bits.data.getGpa := filters.head.getGpa
  } else {
    io.tlb.resp.bits.data.getGpa := DontCare // DTLBs use the per-group getGpa vector
  }
  io.tlb.resp.bits.data.s1 := ptwResp.s1
  io.tlb.resp.bits.data.s2 := ptwResp.s2
  if (HasMptCheck) {
    io.tlb.resp.bits.data.mpt.get := ptwResp.mpt.get
  }
  // Only each DTLB group's first port marks that group's refill/GPA result
  for (port <- 0 until Width) {
    val group = groupStarts.indexOf(port)
    io.tlb.resp.bits.vector(port) := (if (group >= 0) filters(group).refill else false.B)
    io.tlb.resp.bits.getGpa(port) := (if (group >= 0) filters(group).getGpa else false.B)
  }

  if (!isITLB) {
    val loadFilter = filters.head
    val hint = io.hint.get
    val loadHint = loadFilter.hint.get
    loadFilter.fastResp.get.valid := io.ptw.resp.fire
    loadFilter.fastResp.get.bits := io.ptw.resp.bits
    io.fastHint.get := loadFilter.fastHint.get
    for (i <- hint.req.indices) {
      hint.req(i) := RegNext(loadHint.req(i))
    }
    hint.resp.valid := RegNext(loadHint.resp.valid)
    hint.resp.bits := RegEnable(loadHint.resp.bits, loadHint.resp.valid)
  }

  if (isITLB) {
    io.ptw.req(0) <> filters.head.ptw.req(0)
  } else {
    val ptwArb = Module(new RRArbiterInit(new PtwReq, 3))
    for ((input, filter) <- ptwArb.io.in.zip(filters)) {
      input <> filter.ptw.req(0)
    }
    io.ptw.req(0) <> ptwArb.io.out
  }
  io.ptw.resp.ready := true.B

  io.rob_head_miss_in_tlb := Cat(filters.map(_.rob_head_miss_in_tlb)).orR
}

object PTWRepeaterNB {
  def apply(passReady: Boolean, fenceDelay: Int,
    tlb: TlbPtwIO,
    sfence: SfenceBundle,
    csr: TlbCsrBundle
  )(implicit p: Parameters) = {
    val width = tlb.req.size
    val repeater = Module(new PTWRepeaterNB(width, passReady,fenceDelay))
    repeater.io.apply(tlb, sfence, csr)
    repeater
  }

  def apply(passReady: Boolean, fenceDelay: Int,
    tlb: TlbPtwIO,
    ptw: TlbPtwIO,
    sfence: SfenceBundle,
    csr: TlbCsrBundle
  )(implicit p: Parameters) = {
    val width = tlb.req.size
    val repeater = Module(new PTWRepeaterNB(width, passReady, fenceDelay))
    repeater.io.apply(tlb, ptw, sfence, csr)
    repeater
  }
}

object PTWFilter {
  def apply(
    fenceDelay: Int,
    tlb: VectorTlbPtwIO,
    ptw: TlbPtwIO,
    sfence: SfenceBundle,
    csr: TlbCsrBundle,
    size: Int,
    isITLB: Boolean
  )(implicit p: Parameters) = {
    val width = tlb.req.size
    val filter = Module(new PTWFilter(width, size, fenceDelay, isITLB))
    filter.io.apply(tlb, ptw, sfence, csr)
    filter
  }

  def apply(
    fenceDelay: Int,
    tlb: VectorTlbPtwIO,
    sfence: SfenceBundle,
    csr: TlbCsrBundle,
    size: Int,
    isITLB: Boolean
  )(implicit p: Parameters) = {
    val width = tlb.req.size
    val filter = Module(new PTWFilter(width, size, fenceDelay, isITLB))
    filter.io.apply(tlb, sfence, csr)
    filter
  }
}
