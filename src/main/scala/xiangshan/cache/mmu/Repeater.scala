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
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import utility._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._

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

class PTWRepeater(Width: Int = 1, FenceDelay: Int)(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new PTWReapterIO(Width))

  val req_in = if (Width == 1) {
    io.tlb.req(0)
  } else {
    val arb = Module(new RRArbiter(io.tlb.req(0).bits.cloneType, Width))
    arb.io.in <> io.tlb.req
    arb.io.out
  }
  val (tlb, ptw, flush) = (io.tlb, io.ptw, DelayN(io.sfence.valid || io.csr.satp.changed, FenceDelay))
  val req = RegEnable(req_in.bits, req_in.fire)
  val resp = RegEnable(ptw.resp.bits, ptw.resp.fire)
  val haveOne = BoolStopWatch(req_in.fire, tlb.resp.fire || flush)
  val sent = BoolStopWatch(ptw.req(0).fire, req_in.fire || flush)
  val recv = BoolStopWatch(ptw.resp.fire && haveOne, req_in.fire || flush)

  req_in.ready := !haveOne
  ptw.req(0).valid := haveOne && !sent
  ptw.req(0).bits := req

  tlb.resp.bits := resp
  tlb.resp.valid := haveOne && recv
  ptw.resp.ready := !recv

  XSPerfAccumulate("req_count", ptw.req(0).fire)
  XSPerfAccumulate("tlb_req_cycle", BoolStopWatch(req_in.fire, tlb.resp.fire || flush))
  XSPerfAccumulate("ptw_req_cycle", BoolStopWatch(ptw.req(0).fire, ptw.resp.fire || flush))

  XSDebug(haveOne, p"haveOne:${haveOne} sent:${sent} recv:${recv} sfence:${flush} req:${req} resp:${resp}")
  XSDebug(req_in.valid || io.tlb.resp.valid, p"tlb: ${tlb}\n")
  XSDebug(io.ptw.req(0).valid || io.ptw.resp.valid, p"ptw: ${ptw}\n")
  assert(!RegNext(recv && io.ptw.resp.valid, init = false.B), "re-receive ptw.resp")
  XSError(io.ptw.req(0).valid && io.ptw.resp.valid && !flush, "ptw repeater recv resp when sending")
  XSError(io.ptw.resp.valid && (req.vpn =/= io.ptw.resp.bits.entry.tag), "ptw repeater recv resp with wrong tag")
  XSError(io.ptw.resp.valid && !io.ptw.resp.ready, "ptw repeater's ptw resp back, but not ready")
  TimeOutAssert(sent && !recv, timeOutThreshold, "Repeater doesn't recv resp in time")
}

/* dtlb
 *
 */

class PTWRepeaterNB(Width: Int = 1, passReady: Boolean = false, FenceDelay: Int)(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new PTWReapterIO(Width))

  val req_in = if (Width == 1) {
    io.tlb.req(0)
  } else {
    val arb = Module(new RRArbiter(io.tlb.req(0).bits.cloneType, Width))
    arb.io.in <> io.tlb.req
    arb.io.out
  }
  val (tlb, ptw, flush) = (io.tlb, io.ptw, DelayN(io.sfence.valid || io.csr.satp.changed, FenceDelay))
  /* sent: tlb -> repeater -> ptw
   * recv: ptw -> repeater -> tlb
   * different from PTWRepeater
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
  val memidx = Output(new MemBlockidxBundle)
}

class PTWFilterEntry(Width: Int, Size: Int, hasHint: Boolean = false)(implicit p: Parameters) extends XSModule with HasPtwConst {

  val io = IO(new PTWFilterEntryIO(Width, hasHint))
  require(isPow2(Size), s"Filter Size ($Size) must be a power of 2")

  def firstValidIndex(v: Seq[Bool], valid: Bool): UInt = {
    val index = WireInit(0.U(log2Up(Size).W))
    for (i <- 0 until v.size) {
      when (v(i) === valid) {
        index := i.U
      }
    }
    index
  }

  val v = RegInit(VecInit(Seq.fill(Size)(false.B)))
  val sent = RegInit(VecInit(Seq.fill(Size)(false.B)))
  val vpn = Reg(Vec(Size, UInt(vpnLen.W)))
  val memidx = Reg(Vec(Size, new MemBlockidxBundle))

  val enqvalid = WireInit(VecInit(Seq.fill(Width)(false.B)))
  val canenq = WireInit(VecInit(Seq.fill(Width)(false.B)))
  val enqidx = WireInit(VecInit(Seq.fill(Width)(0.U(log2Up(Size).W))))

  //val selectCount = RegInit(0.U(log2Up(Width).W))

  val entryIsMatchVec = WireInit(VecInit(Seq.fill(Width)(false.B)))
  val entryMatchIndexVec = WireInit(VecInit(Seq.fill(Width)(0.U(log2Up(Size).W))))
  val ptwResp_EntryMatchVec = vpn.zip(v).map{ case (pi, vi) => vi && io.ptw.resp.bits.hit(pi, io.csr.satp.asid, true, true)}
  val ptwResp_EntryMatchFirst = firstValidIndex(ptwResp_EntryMatchVec, true.B)
  val ptwResp_ReqMatchVec = io.tlb.req.map(a => io.ptw.resp.valid && io.ptw.resp.bits.hit(a.bits.vpn, 0.U, allType = true, true))

  io.refill := Cat(ptwResp_EntryMatchVec).orR && io.ptw.resp.fire
  io.ptw.resp.ready := true.B
  // DontCare
  io.tlb.req.map(_.ready := true.B)
  io.tlb.resp.valid := false.B
  io.tlb.resp.bits.data := 0.U.asTypeOf(new PtwSectorRespwithMemIdx)
  io.tlb.resp.bits.vector := 0.U.asTypeOf(Vec(Width, Bool()))
  io.memidx := 0.U.asTypeOf(new MemBlockidxBundle)

  // ugly code, should be optimized later
  require(Width <= 3, s"DTLB Filter Width ($Width) must equal or less than 3")
  if (Width == 1) {
    require(Size == 8, s"prefetch filter Size ($Size) should be 8")
    canenq(0) := !(Cat(v).andR)
    enqidx(0) := firstValidIndex(v, false.B)
  } else if (Width == 2) {
    require(Size == 8, s"store filter Size ($Size) should be 8")
    canenq(0) := !(Cat(v.take(Size/2)).andR)
    enqidx(0) := firstValidIndex(v.take(Size/2), false.B)
    canenq(1) := !(Cat(v.drop(Size/2)).andR)
    enqidx(1) := firstValidIndex(v.drop(Size/2), false.B) + (Size/2).U
  } else if (Width == 3) {
    require(Size == 16, s"load filter Size ($Size) should be 16")
    canenq(0) := !(Cat(v.take(4)).andR)
    enqidx(0) := firstValidIndex(v.take(4), false.B)
    canenq(1) := !(Cat(v.drop(4).take(4)).andR)
    enqidx(1) := firstValidIndex(v.drop(4).take(4), false.B) + 4.U
    // eight entries for prefetch
    canenq(2) := !(Cat(v.drop(8)).andR)
    enqidx(2) := firstValidIndex(v.drop(8), false.B) + 8.U
  }

  for (i <- 0 until Width) {
    enqvalid(i) := io.tlb.req(i).valid && !ptwResp_ReqMatchVec(i) && !entryIsMatchVec(i) && canenq(i)
    when (!enqvalid(i)) {
      enqidx(i) := entryMatchIndexVec(i)
    }

    val entryIsMatch = vpn.zip(v).map{ case (pi, vi) => vi && pi === io.tlb.req(i).bits.vpn}
    entryIsMatchVec(i) := Cat(entryIsMatch).orR
    entryMatchIndexVec(i) := firstValidIndex(entryIsMatch, true.B)

    if (i > 0) {
      for (j <- 0 until i) {
        val newIsMatch = io.tlb.req(i).bits.vpn === io.tlb.req(j).bits.vpn
        when (newIsMatch && io.tlb.req(j).valid) {
          enqidx(i) := enqidx(j)
          enqvalid(i) := false.B
        }
      }
    }

    when (enqvalid(i)) {
      v(enqidx(i)) := true.B
      sent(enqidx(i)) := false.B
      vpn(enqidx(i)) := io.tlb.req(i).bits.vpn
      memidx(enqidx(i)) := io.tlb.req(i).bits.memidx
    }
  }

  val issuevec = v.zip(sent).map{ case (v, s) => v && !s}
  val issueindex = firstValidIndex(issuevec, true.B)
  val canissue = Cat(issuevec).orR
  for (i <- 0 until Size) {
    io.ptw.req(0).valid := canissue
    io.ptw.req(0).bits.vpn := vpn(issueindex)
  }
  when (io.ptw.req(0).fire) {
    sent(issueindex) := true.B
  }

  when (io.ptw.resp.fire) {
    v.zip(ptwResp_EntryMatchVec).map{ case (vi, mi) => when (mi) { vi := false.B }}
    io.memidx := memidx(ptwResp_EntryMatchFirst)
  }

  when (io.flush) {
    v.map(_ := false.B)
  }

  if (hasHint) {
    val hintIO = io.hint.getOrElse(new TlbHintIO)
    for (i <- 0 until exuParameters.LduCnt) {
      hintIO.req(i).id := enqidx(i)
      hintIO.req(i).full := ptwResp_ReqMatchVec(i) || (!canenq(i) && !ptwResp_ReqMatchVec(i))
    }
    hintIO.resp.valid := io.refill
    hintIO.resp.bits.id := ptwResp_EntryMatchFirst
    hintIO.resp.bits.replay_all := PopCount(ptwResp_EntryMatchVec) > 1.U
  }

  io.rob_head_miss_in_tlb := VecInit(v.zip(vpn).map{case (vi, vpni) => {
    vi && io.debugTopDown.robHeadVaddr.valid && vpni === get_pn(io.debugTopDown.robHeadVaddr.bits)
  }}).asUInt.orR


  // Perf Counter
  val counter = PopCount(v)
  val inflight_counter = RegInit(0.U(log2Up(Size).W))
  val inflight_full = inflight_counter === Size.U
  when (io.ptw.req(0).fire =/= io.ptw.resp.fire) {
    inflight_counter := Mux(io.ptw.req(0).fire, inflight_counter + 1.U, inflight_counter - 1.U)
  }

  assert(inflight_counter <= Size.U, "inflight should be no more than Size")
  when (counter === 0.U) {
    assert(!io.ptw.req(0).fire, "when counter is 0, should not req")
  }

  when (io.flush) {
    inflight_counter := 0.U
  }

  XSPerfAccumulate("tlb_req_count", PopCount(Cat(io.tlb.req.map(_.valid))))
  XSPerfAccumulate("tlb_req_count_filtered", PopCount(enqvalid))
  XSPerfAccumulate("ptw_req_count", io.ptw.req(0).fire)
  XSPerfAccumulate("ptw_req_cycle", inflight_counter)
  XSPerfAccumulate("tlb_resp_count", io.tlb.resp.fire)
  XSPerfAccumulate("ptw_resp_count", io.ptw.resp.fire)
  XSPerfAccumulate("inflight_cycle", Cat(sent).orR)

  for (i <- 0 until Size + 1) {
    XSPerfAccumulate(s"counter${i}", counter === i.U)
  }

  for (i <- 0 until Size) {
    TimeOutAssert(v(i), timeOutThreshold, s"Filter ${i} doesn't recv resp in time")
  }

}

class PTWNewFilter(Width: Int, Size: Int, FenceDelay: Int)(implicit p: Parameters) extends XSModule with HasPtwConst {
  require(Size >= Width)

  val io = IO(new PTWFilterIO(Width, hasHint = true))

  val load_filter = VecInit(Seq.fill(1) {
    val load_entry = Module(new PTWFilterEntry(Width = exuParameters.LduCnt + 1, Size = loadfiltersize, hasHint = true))
    load_entry.io
  })

  val store_filter = VecInit(Seq.fill(1) {
    val store_entry = Module(new PTWFilterEntry(Width = exuParameters.StuCnt, Size = storefiltersize))
    store_entry.io
  })

  val prefetch_filter = VecInit(Seq.fill(1) {
    val prefetch_entry = Module(new PTWFilterEntry(Width = 1, Size = prefetchfiltersize))
    prefetch_entry.io
  })

  val filter = load_filter ++ store_filter ++ prefetch_filter

  load_filter.map(_.tlb.req := io.tlb.req.take(exuParameters.LduCnt + 1))
  store_filter.map(_.tlb.req := io.tlb.req.drop(exuParameters.LduCnt + 1).take(exuParameters.StuCnt))
  prefetch_filter.map(_.tlb.req := io.tlb.req.drop(exuParameters.LduCnt + 1 + exuParameters.StuCnt))

  val flush = DelayN(io.sfence.valid || io.csr.satp.changed, FenceDelay)
  val ptwResp = RegEnable(io.ptw.resp.bits, io.ptw.resp.fire)
  val ptwResp_valid = Cat(filter.map(_.refill)).orR
  filter.map(_.tlb.resp.ready := true.B)
  filter.map(_.ptw.resp.valid := RegNext(io.ptw.resp.fire, init = false.B))
  filter.map(_.ptw.resp.bits := ptwResp)
  filter.map(_.flush := flush)
  filter.map(_.sfence := io.sfence)
  filter.map(_.csr := io.csr)
  filter.map(_.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr)

  io.tlb.req.map(_.ready := true.B)
  io.tlb.resp.valid := ptwResp_valid
  io.tlb.resp.bits.data.entry := ptwResp.entry
  io.tlb.resp.bits.data.addr_low := ptwResp.addr_low
  io.tlb.resp.bits.data.ppn_low := ptwResp.ppn_low
  io.tlb.resp.bits.data.valididx := ptwResp.valididx
  io.tlb.resp.bits.data.pteidx := ptwResp.pteidx
  io.tlb.resp.bits.data.pf := ptwResp.pf
  io.tlb.resp.bits.data.af := ptwResp.af
  io.tlb.resp.bits.data.memidx := 0.U.asTypeOf(new MemBlockidxBundle)
  // vector used to represent different requestors of DTLB
  // (e.g. the store DTLB has StuCnt requestors)
  // However, it is only necessary to distinguish between different DTLB now
  for (i <- 0 until Width) {
    io.tlb.resp.bits.vector(i) := false.B
  }
  io.tlb.resp.bits.vector(0) := load_filter(0).refill
  io.tlb.resp.bits.vector(exuParameters.LduCnt + 1) := store_filter(0).refill
  io.tlb.resp.bits.vector(exuParameters.LduCnt + 1 + exuParameters.StuCnt) := prefetch_filter(0).refill

  val hintIO = io.hint.getOrElse(new TlbHintIO)
  val load_hintIO = load_filter(0).hint.getOrElse(new TlbHintIO)
  for (i <- 0 until exuParameters.LduCnt) {
    hintIO.req(i) := RegNext(load_hintIO.req(i))
  }
  hintIO.resp := RegNext(load_hintIO.resp)

  when (load_filter(0).refill) {
    io.tlb.resp.bits.vector(0) := true.B
    io.tlb.resp.bits.data.memidx := load_filter(0).memidx
  }
  when (store_filter(0).refill) {
    io.tlb.resp.bits.vector(exuParameters.LduCnt + 1) := true.B
    io.tlb.resp.bits.data.memidx := store_filter(0).memidx
  }
  when (prefetch_filter(0).refill) {
    io.tlb.resp.bits.vector(exuParameters.LduCnt + 1 + exuParameters.StuCnt) := true.B
    io.tlb.resp.bits.data.memidx := 0.U.asTypeOf(new MemBlockidxBundle)
  }

  val ptw_arb = Module(new RRArbiterInit(new PtwReq, 3))
  for (i <- 0 until 3) {
    ptw_arb.io.in(i).valid := filter(i).ptw.req(0).valid
    ptw_arb.io.in(i).bits.vpn := filter(i).ptw.req(0).bits.vpn
    filter(i).ptw.req(0).ready := ptw_arb.io.in(i).ready
  }
  ptw_arb.io.out.ready := io.ptw.req(0).ready
  io.ptw.req(0).valid := ptw_arb.io.out.valid
  io.ptw.req(0).bits.vpn := ptw_arb.io.out.bits.vpn
  io.ptw.resp.ready := true.B

  io.rob_head_miss_in_tlb := Cat(filter.map(_.rob_head_miss_in_tlb)).orR
}

class PTWFilter(Width: Int, Size: Int, FenceDelay: Int)(implicit p: Parameters) extends XSModule with HasPtwConst {
  require(Size >= Width)

  val io = IO(new PTWFilterIO(Width))

  val v = RegInit(VecInit(Seq.fill(Size)(false.B)))
  val ports = Reg(Vec(Size, Vec(Width, Bool()))) // record which port(s) the entry come from, may not able to cover all the ports
  val vpn = Reg(Vec(Size, UInt(vpnLen.W)))
  val memidx = Reg(Vec(Size, new MemBlockidxBundle))
  val enqPtr = RegInit(0.U(log2Up(Size).W)) // Enq
  val issPtr = RegInit(0.U(log2Up(Size).W)) // Iss to Ptw
  val deqPtr = RegInit(0.U(log2Up(Size).W)) // Deq
  val mayFullDeq = RegInit(false.B)
  val mayFullIss = RegInit(false.B)
  val counter = RegInit(0.U(log2Up(Size+1).W))

  val flush = DelayN(io.sfence.valid || io.csr.satp.changed, FenceDelay)
  val tlb_req = WireInit(io.tlb.req) // NOTE: tlb_req is not io.tlb.req, see below codes, just use cloneType
  tlb_req.suggestName("tlb_req")

  val inflight_counter = RegInit(0.U(log2Up(Size + 1).W))
  val inflight_full = inflight_counter === Size.U
  when (io.ptw.req(0).fire =/= io.ptw.resp.fire) {
    inflight_counter := Mux(io.ptw.req(0).fire, inflight_counter + 1.U, inflight_counter - 1.U)
  }

  val canEnqueue = Wire(Bool()) // NOTE: actually enqueue
  val ptwResp = RegEnable(io.ptw.resp.bits, io.ptw.resp.fire)
  val ptwResp_OldMatchVec = vpn.zip(v).map{ case (pi, vi) =>
    vi && io.ptw.resp.bits.hit(pi, io.csr.satp.asid, true, true)}
  val ptwResp_valid = RegNext(io.ptw.resp.fire && Cat(ptwResp_OldMatchVec).orR, init = false.B)
  // May send repeated requests to L2 tlb with same vpn(26, 3) when sector tlb
  val oldMatchVec_early = io.tlb.req.map(a => vpn.zip(v).map{ case (pi, vi) => vi && pi === a.bits.vpn})
  val lastReqMatchVec_early = io.tlb.req.map(a => tlb_req.map{ b => b.valid && b.bits.vpn === a.bits.vpn && canEnqueue})
  val newMatchVec_early = io.tlb.req.map(a => io.tlb.req.map(b => a.bits.vpn === b.bits.vpn))

  (0 until Width) foreach { i =>
    tlb_req(i).valid := RegNext(io.tlb.req(i).valid &&
      !(ptwResp_valid && ptwResp.hit(io.tlb.req(i).bits.vpn, 0.U, true, true)) &&
      !Cat(lastReqMatchVec_early(i)).orR,
      init = false.B)
    tlb_req(i).bits := RegEnable(io.tlb.req(i).bits, io.tlb.req(i).valid)
  }

  val oldMatchVec = oldMatchVec_early.map(a => RegNext(Cat(a).orR))
  val newMatchVec = (0 until Width).map(i => (0 until Width).map(j =>
    RegNext(newMatchVec_early(i)(j)) && tlb_req(j).valid
  ))
  val ptwResp_newMatchVec = tlb_req.map(a =>
    ptwResp_valid && ptwResp.hit(a.bits.vpn, 0.U, allType = true, true))

  val oldMatchVec2 = (0 until Width).map(i => oldMatchVec_early(i).map(RegNext(_)).map(_ & tlb_req(i).valid))
  val update_ports = v.indices.map(i => oldMatchVec2.map(j => j(i)))
  val ports_init = (0 until Width).map(i => (1 << i).U(Width.W))
  val filter_ports = (0 until Width).map(i => ParallelMux(newMatchVec(i).zip(ports_init).drop(i)))
  val resp_vector = RegEnable(ParallelMux(ptwResp_OldMatchVec zip ports), io.ptw.resp.fire)

  def canMerge(index: Int) : Bool = {
    ptwResp_newMatchVec(index) || oldMatchVec(index) ||
    Cat(newMatchVec(index).take(index)).orR
  }

  def filter_req() = {
    val reqs =  tlb_req.indices.map{ i =>
      val req = Wire(ValidIO(new PtwReqwithMemIdx()))
      val merge = canMerge(i)
      req.bits := tlb_req(i).bits
      req.valid := !merge && tlb_req(i).valid
      req
    }
    reqs
  }

  val reqs = filter_req()
  val req_ports = filter_ports
  val isFull = enqPtr === deqPtr && mayFullDeq
  val isEmptyDeq = enqPtr === deqPtr && !mayFullDeq
  val isEmptyIss = enqPtr === issPtr && !mayFullIss
  val accumEnqNum = (0 until Width).map(i => PopCount(reqs.take(i).map(_.valid)))
  val enqPtrVecInit = VecInit((0 until Width).map(i => enqPtr + i.U))
  val enqPtrVec = VecInit((0 until Width).map(i => enqPtrVecInit(accumEnqNum(i))))
  val enqNum = PopCount(reqs.map(_.valid))
  canEnqueue := counter +& enqNum <= Size.U

  // the req may recv false ready, but actually received. Filter and TLB will handle it.
  val enqNum_fake = PopCount(io.tlb.req.map(_.valid))
  val canEnqueue_fake = counter +& enqNum_fake <= Size.U
  io.tlb.req.map(_.ready := canEnqueue_fake) // NOTE: just drop un-fire reqs

  // tlb req flushed by ptw resp: last ptw resp && current ptw resp
  // the flushed tlb req will fakely enq, with a false valid
  val tlb_req_flushed = reqs.map(a => io.ptw.resp.valid && io.ptw.resp.bits.hit(a.bits.vpn, 0.U, true, true))

  io.tlb.resp.valid := ptwResp_valid
  io.tlb.resp.bits.data.entry := ptwResp.entry
  io.tlb.resp.bits.data.addr_low := ptwResp.addr_low
  io.tlb.resp.bits.data.ppn_low := ptwResp.ppn_low
  io.tlb.resp.bits.data.valididx := ptwResp.valididx
  io.tlb.resp.bits.data.pteidx := ptwResp.pteidx
  io.tlb.resp.bits.data.pf := ptwResp.pf
  io.tlb.resp.bits.data.af := ptwResp.af
  io.tlb.resp.bits.data.memidx := memidx(OHToUInt(ptwResp_OldMatchVec))
  io.tlb.resp.bits.vector := resp_vector

  val issue_valid = v(issPtr) && !isEmptyIss && !inflight_full
  val issue_filtered = ptwResp_valid && ptwResp.hit(io.ptw.req(0).bits.vpn, io.csr.satp.asid, allType=true, ignoreAsid=true)
  val issue_fire_fake = issue_valid && (io.ptw.req(0).ready || (issue_filtered && false.B /*timing-opt*/))
  io.ptw.req(0).valid := issue_valid && !issue_filtered
  io.ptw.req(0).bits.vpn := vpn(issPtr)
  io.ptw.resp.ready := true.B

  reqs.zipWithIndex.map{
    case (req, i) =>
      when (req.valid && canEnqueue) {
        v(enqPtrVec(i)) := !tlb_req_flushed(i)
        vpn(enqPtrVec(i)) := req.bits.vpn
        memidx(enqPtrVec(i)) := req.bits.memidx
        ports(enqPtrVec(i)) := req_ports(i).asBools
      }
  }
  for (i <- ports.indices) {
    when (v(i)) {
      ports(i) := ports(i).zip(update_ports(i)).map(a => a._1 || a._2)
    }
  }

  val do_enq = canEnqueue && Cat(reqs.map(_.valid)).orR
  val do_deq = (!v(deqPtr) && !isEmptyDeq)
  val do_iss = issue_fire_fake || (!v(issPtr) && !isEmptyIss)
  when (do_enq) {
    enqPtr := enqPtr + enqNum
  }
  when (do_deq) {
    deqPtr := deqPtr + 1.U
  }
  when (do_iss) {
    issPtr := issPtr + 1.U
  }
  when (issue_fire_fake && issue_filtered) { // issued but is filtered
    v(issPtr) := false.B
  }
  when (do_enq =/= do_deq) {
    mayFullDeq := do_enq
  }
  when (do_enq =/= do_iss) {
    mayFullIss := do_enq
  }

  when (io.ptw.resp.fire) {
    v.zip(ptwResp_OldMatchVec).map{ case (vi, mi) => when (mi) { vi := false.B }}
  }

  counter := counter - do_deq + Mux(do_enq, enqNum, 0.U)
  assert(counter <= Size.U, "counter should be no more than Size")
  assert(inflight_counter <= Size.U, "inflight should be no more than Size")
  when (counter === 0.U) {
    assert(!io.ptw.req(0).fire, "when counter is 0, should not req")
    assert(isEmptyDeq && isEmptyIss, "when counter is 0, should be empty")
  }
  when (counter === Size.U) {
    assert(mayFullDeq, "when counter is Size, should be full")
  }

  when (flush) {
    v.map(_ := false.B)
    deqPtr := 0.U
    enqPtr := 0.U
    issPtr := 0.U
    ptwResp_valid := false.B
    mayFullDeq := false.B
    mayFullIss := false.B
    counter := 0.U
    inflight_counter := 0.U
  }

  val robHeadVaddr = io.debugTopDown.robHeadVaddr
  io.rob_head_miss_in_tlb := VecInit(v.zip(vpn).map{case (vi, vpni) => {
    vi && robHeadVaddr.valid && vpni === get_pn(robHeadVaddr.bits)
  }}).asUInt.orR

  // perf
  XSPerfAccumulate("tlb_req_count", PopCount(Cat(io.tlb.req.map(_.valid))))
  XSPerfAccumulate("tlb_req_count_filtered", Mux(do_enq, accumEnqNum(Width - 1), 0.U))
  XSPerfAccumulate("ptw_req_count", io.ptw.req(0).fire)
  XSPerfAccumulate("ptw_req_cycle", inflight_counter)
  XSPerfAccumulate("tlb_resp_count", io.tlb.resp.fire)
  XSPerfAccumulate("ptw_resp_count", io.ptw.resp.fire)
  XSPerfAccumulate("inflight_cycle", !isEmptyDeq)
  for (i <- 0 until Size + 1) {
    XSPerfAccumulate(s"counter${i}", counter === i.U)
  }

  for (i <- 0 until Size) {
    TimeOutAssert(v(i), timeOutThreshold, s"Filter ${i} doesn't recv resp in time")
  }
}

object PTWRepeater {
  def apply(fenceDelay: Int,
    tlb: TlbPtwIO,
    sfence: SfenceBundle,
    csr: TlbCsrBundle
  )(implicit p: Parameters) = {
    val width = tlb.req.size
    val repeater = Module(new PTWRepeater(width, fenceDelay))
    repeater.io.apply(tlb, sfence, csr)
    repeater
  }

  def apply(fenceDelay: Int,
    tlb: TlbPtwIO,
    ptw: TlbPtwIO,
    sfence: SfenceBundle,
    csr: TlbCsrBundle
  )(implicit p: Parameters) = {
    val width = tlb.req.size
    val repeater = Module(new PTWRepeater(width, fenceDelay))
    repeater.io.apply(tlb, ptw, sfence, csr)
    repeater
  }
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
  def apply(fenceDelay: Int,
    tlb: VectorTlbPtwIO,
    ptw: TlbPtwIO,
    sfence: SfenceBundle,
    csr: TlbCsrBundle,
    size: Int
  )(implicit p: Parameters) = {
    val width = tlb.req.size
    val filter = Module(new PTWFilter(width, size, fenceDelay))
    filter.io.apply(tlb, ptw, sfence, csr)
    filter
  }

  def apply(fenceDelay: Int,
    tlb: VectorTlbPtwIO,
    sfence: SfenceBundle,
    csr: TlbCsrBundle,
    size: Int
  )(implicit p: Parameters) = {
    val width = tlb.req.size
    val filter = Module(new PTWFilter(width, size, fenceDelay))
    filter.io.apply(tlb, sfence, csr)
    filter
  }
}

object PTWNewFilter {
  def apply(fenceDelay: Int,
            tlb: VectorTlbPtwIO,
            ptw: TlbPtwIO,
            sfence: SfenceBundle,
            csr: TlbCsrBundle,
            size: Int
           )(implicit p: Parameters) = {
    val width = tlb.req.size
    val filter = Module(new PTWNewFilter(width, size, fenceDelay))
    filter.io.apply(tlb, ptw, sfence, csr)
    filter
  }

  def apply(fenceDelay: Int,
            tlb: VectorTlbPtwIO,
            sfence: SfenceBundle,
            csr: TlbCsrBundle,
            size: Int
           )(implicit p: Parameters) = {
    val width = tlb.req.size
    val filter = Module(new PTWNewFilter(width, size, fenceDelay))
    filter.io.apply(tlb, sfence, csr)
    filter
  }
}
