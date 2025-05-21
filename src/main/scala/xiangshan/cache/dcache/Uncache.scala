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

package xiangshan.cache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleA, TLBundleD, TLClientNode, TLEdgeOut, TLMasterParameters, TLMasterPortParameters}
import xiangshan._
import xiangshan.mem._
import xiangshan.mem.Bundles._
import coupledL2.{MemBackTypeMM, MemBackTypeMMField, MemPageTypeNC, MemPageTypeNCField}
import difftest._

trait HasUncacheBufferParameters extends HasXSParameter with HasDCacheParameters {

  def doMerge(oldData: UInt, oldMask: UInt, newData:UInt, newMask: UInt):(UInt, UInt) = {
    val resData = VecInit((0 until DataBytes).map(j =>
      Mux(newMask(j), newData(8*(j+1)-1, 8*j), oldData(8*(j+1)-1, 8*j))
    )).asUInt
    val resMask = newMask | oldMask
    (resData, resMask)
  }

  def INDEX_WIDTH = log2Up(UncacheBufferSize)
  def BLOCK_OFFSET = log2Up(XLEN / 8)
  def getBlockAddr(x: UInt) = x >> BLOCK_OFFSET
}

abstract class UncacheBundle(implicit p: Parameters) extends XSBundle with HasUncacheBufferParameters

abstract class UncacheModule(implicit p: Parameters) extends XSModule with HasUncacheBufferParameters


class UncacheFlushBundle extends Bundle {
  val valid = Output(Bool())
  val empty = Input(Bool())
}

class UncacheEntry(implicit p: Parameters) extends UncacheBundle {
  val cmd = UInt(M_SZ.W)
  val addr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val data = UInt(XLEN.W)
  val mask = UInt(DataBytes.W)
  val nc = Bool()
  val memBackTypeMM = Bool()

  val resp_nderr = Bool()

  /* NOTE: if it support the internal forward logic, here can uncomment */
  // val fwd_data = UInt(XLEN.W)
  // val fwd_mask = UInt(DataBytes.W)

  def set(x: UncacheWordReq): Unit = {
    cmd := x.cmd
    addr := x.addr
    vaddr := x.vaddr
    data := x.data
    mask := x.mask
    nc := x.nc
    memBackTypeMM := x.memBackTypeMM
    resp_nderr := false.B
    // fwd_data := 0.U
    // fwd_mask := 0.U
  }

  def update(x: UncacheWordReq): Unit = {
    val (resData, resMask) = doMerge(data, mask, x.data, x.mask)
    // mask -> get the first position as 1 -> for address align
    val (resOffset, resFlag) = PriorityEncoderWithFlag(resMask)
    data := resData
    mask := resMask
    when(resFlag){
      addr := (getBlockAddr(addr) << BLOCK_OFFSET) | resOffset
      vaddr := (getBlockAddr(vaddr) << BLOCK_OFFSET) | resOffset
    }
  }

  def update(x: TLBundleD): Unit = {
    when(cmd === MemoryOpConstants.M_XRD) {
      data := x.data
    }
    resp_nderr := x.denied || x.corrupt
  }

  // def update(forwardData: UInt, forwardMask: UInt): Unit = {
  //   fwd_data := forwardData
  //   fwd_mask := forwardMask
  // }

  def toUncacheWordResp(eid: UInt): UncacheWordResp = {
    // val resp_fwd_data = VecInit((0 until DataBytes).map(j =>
    //   Mux(fwd_mask(j), fwd_data(8*(j+1)-1, 8*j), data(8*(j+1)-1, 8*j))
    // )).asUInt
    val resp_fwd_data = data
    val r = Wire(new UncacheWordResp)
    r := DontCare
    r.data := resp_fwd_data
    r.id := eid
    r.nderr := resp_nderr
    r.nc := nc
    r.is2lq := cmd === MemoryOpConstants.M_XRD
    r.miss := false.B
    r.replay := false.B
    r.tag_error := false.B
    r.error := false.B
    r
  }
}

class UncacheEntryState(implicit p: Parameters) extends DCacheBundle {
  // valid (-> waitSame) -> inflight -> waitReturn
  val valid = Bool()
  val inflight = Bool() // uncache -> L2
  val waitSame = Bool()
  val waitReturn = Bool() // uncache -> LSQ

  def init: Unit = {
    valid := false.B
    inflight := false.B
    waitSame := false.B
    waitReturn := false.B
  }

  def isValid(): Bool = valid
  def isInflight(): Bool = valid && inflight
  def isWaitReturn(): Bool = valid && waitReturn
  def isWaitSame(): Bool = valid && waitSame
  def can2Bus(): Bool = valid && !inflight && !waitSame && !waitReturn
  def can2Lsq(): Bool = valid && waitReturn
  def canMerge(): Bool = valid && !inflight
  def isFwdOld(): Bool = valid && (inflight || waitReturn)
  def isFwdNew(): Bool = valid && !inflight && !waitReturn

  def setValid(x: Bool): Unit = { valid := x}
  def setInflight(x: Bool): Unit = { inflight := x}
  def setWaitReturn(x: Bool): Unit = { waitReturn := x }
  def setWaitSame(x: Bool): Unit = { waitSame := x}

  def updateUncacheResp(): Unit = {
    assert(inflight, "The request was not sent and a response was received")
    inflight := false.B
    waitReturn := true.B
  }
  def updateReturn(): Unit = {
    valid := false.B
    inflight := false.B
    waitSame := false.B
    waitReturn := false.B
  }
}

class UncacheIO(implicit p: Parameters) extends DCacheBundle {
  val hartId = Input(UInt())
  val enableOutstanding = Input(Bool())
  val flush = Flipped(new UncacheFlushBundle)
  val lsq = Flipped(new UncacheWordIO)
  val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
  val wfi = Flipped(new WfiReqBundle)
  val busError = Output(new L1BusErrorUnitInfo())
}

// convert DCacheIO to TileLink
// for Now, we only deal with TL-UL

class Uncache()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false
  def idRange: Int = UncacheBufferSize

  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "uncache",
      sourceId = IdRange(0, idRange)
    )),
    requestFields = Seq(MemBackTypeMMField(), MemPageTypeNCField())
  )
  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new UncacheImp(this)
}

/* Uncache Buffer */
class UncacheImp(outer: Uncache)extends LazyModuleImp(outer)
  with HasTLDump
  with HasXSParameter
  with HasUncacheBufferParameters
  with HasPerfEvents
{
  println(s"Uncahe Buffer Size: $UncacheBufferSize entries")
  val io = IO(new UncacheIO)

  val (bus, edge) = outer.clientNode.out.head

  val req  = io.lsq.req
  val resp = io.lsq.resp
  val mem_acquire = bus.a
  val mem_grant   = bus.d
  val req_ready = WireInit(false.B)

  // assign default values to output signals
  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.d.ready := false.B
  bus.e.valid := false.B
  bus.e.bits  := DontCare
  io.lsq.req.ready := req_ready
  io.lsq.resp.valid := false.B
  io.lsq.resp.bits := DontCare


  /******************************************************************
   * Data Structure
   ******************************************************************/

  val entries = Reg(Vec(UncacheBufferSize, new UncacheEntry))
  val states = RegInit(VecInit(Seq.fill(UncacheBufferSize)(0.U.asTypeOf(new UncacheEntryState))))
  val s_idle :: s_inflight :: s_wait_return :: Nil = Enum(3)
  val uState = RegInit(s_idle)
  val noPending = RegInit(VecInit(Seq.fill(UncacheBufferSize)(true.B)))

  // drain buffer
  val empty = Wire(Bool())
  val f1_needDrain = Wire(Bool())
  val do_uarch_drain = RegInit(false.B)
  when((f1_needDrain || io.flush.valid) && !empty){
    do_uarch_drain := true.B
  }.elsewhen(empty){
    do_uarch_drain := false.B
  }.otherwise{
    do_uarch_drain := false.B
  }

  val q0_entry = Wire(new UncacheEntry)
  val q0_canSentIdx = Wire(UInt(INDEX_WIDTH.W))
  val q0_canSent = Wire(Bool())


  /******************************************************************
   * Functions
   ******************************************************************/
  def sizeMap[T <: Data](f: Int => T) = VecInit((0 until UncacheBufferSize).map(f))
  def sizeForeach[T <: Data](f: Int => Unit) = (0 until UncacheBufferSize).map(f)
  def isStore(e: UncacheEntry): Bool = e.cmd === MemoryOpConstants.M_XWR
  def isStore(x: UInt): Bool = x === MemoryOpConstants.M_XWR
  def addrMatch(x: UncacheEntry, y: UncacheWordReq) : Bool = getBlockAddr(x.addr) === getBlockAddr(y.addr)
  def addrMatch(x: UncacheWordReq, y: UncacheEntry) : Bool = getBlockAddr(x.addr) === getBlockAddr(y.addr)
  def addrMatch(x: UncacheEntry, y: UncacheEntry) : Bool = getBlockAddr(x.addr) === getBlockAddr(y.addr)
  def addrMatch(x: UInt, y: UInt) : Bool = getBlockAddr(x) === getBlockAddr(y)

  def continueAndAlign(mask: UInt): Bool = {
    val res =
      PopCount(mask) === 1.U ||
      mask === 0b00000011.U ||
      mask === 0b00001100.U ||
      mask === 0b00110000.U ||
      mask === 0b11000000.U ||
      mask === 0b00001111.U ||
      mask === 0b11110000.U ||
      mask === 0b11111111.U
    res
  }

  def canMergePrimary(x: UncacheWordReq, e: UncacheEntry, eid: UInt): Bool = {
    // vaddr same, properties same
    getBlockAddr(x.vaddr) === getBlockAddr(e.vaddr) &&
      x.cmd === e.cmd && x.nc && e.nc &&
      x.memBackTypeMM === e.memBackTypeMM &&
      continueAndAlign(x.mask | e.mask) &&
    // not receiving uncache response, not waitReturn -> no wake-up signal in these cases
      !(mem_grant.fire && mem_grant.bits.source === eid || states(eid).isWaitReturn())
  }

  def canMergeSecondary(eid: UInt): Bool = {
    // old entry is not inflight and senting
    states(eid).canMerge() && !(q0_canSent && q0_canSentIdx === eid)
  }

  /******************************************************************
   * uState for non-outstanding
   ******************************************************************/

  switch(uState){
    is(s_idle){
      when(mem_acquire.fire){
        uState := s_inflight
      }
    }
    is(s_inflight){
      when(mem_grant.fire){
        uState := s_wait_return
      }
    }
    is(s_wait_return){
      when(resp.fire){
        uState := s_idle
      }
    }
  }


  /******************************************************************
   * Enter Buffer
   *  Version 0 (better timing)
   *    e0 judge: alloc/merge write vec
   *    e1 alloc
   *
   *  Version 1 (better performance)
   *    e0: solved in one cycle for achieving the original performance.
   *    e1: return idResp to set sid for handshake
   ******************************************************************/

  /* e0: merge/alloc */
  val e0_fire = req.fire
  val e0_req_valid = req.valid
  val e0_req = req.bits

  val e0_rejectVec = Wire(Vec(UncacheBufferSize, Bool()))
  val e0_mergeVec = Wire(Vec(UncacheBufferSize, Bool()))
  val e0_allocWaitSameVec = Wire(Vec(UncacheBufferSize, Bool()))
  sizeForeach(i => {
    val valid = e0_req_valid && states(i).isValid()
    val isAddrMatch = addrMatch(e0_req, entries(i))
    val canMerge1 = canMergePrimary(e0_req, entries(i), i.U)
    val canMerge2 = canMergeSecondary(i.U)
    e0_rejectVec(i) := valid && isAddrMatch && !canMerge1
    e0_mergeVec(i) := valid && isAddrMatch && canMerge1 && canMerge2
    e0_allocWaitSameVec(i) := valid && isAddrMatch && canMerge1 && !canMerge2
  })
  assert(PopCount(e0_mergeVec) <= 1.U, "Uncache buffer should not merge multiple entries")

  val e0_invalidVec = sizeMap(i => !states(i).isValid())
  val (e0_mergeIdx, e0_canMerge) = PriorityEncoderWithFlag(e0_mergeVec)
  val (e0_allocIdx, e0_canAlloc) = PriorityEncoderWithFlag(e0_invalidVec)
  val e0_allocWaitSame = e0_allocWaitSameVec.reduce(_ || _)
  val e0_sid = Mux(e0_canMerge, e0_mergeIdx, e0_allocIdx)
  val e0_reject = do_uarch_drain || (!e0_canMerge && !e0_invalidVec.asUInt.orR) || e0_rejectVec.reduce(_ || _)

  // e0_fire is used to guarantee that it will not be rejected
  when(e0_canMerge && e0_req_valid){
    entries(e0_mergeIdx).update(e0_req)
  }.elsewhen(e0_canAlloc && e0_fire){
    entries(e0_allocIdx).set(e0_req)
    states(e0_allocIdx).setValid(true.B)
    when(e0_allocWaitSame){
      states(e0_allocIdx).setWaitSame(true.B)
    }
  }

  req_ready := !e0_reject

  /* e1: return accept */
  io.lsq.idResp.valid := RegNext(e0_fire)
  io.lsq.idResp.bits.mid := RegEnable(e0_req.id, e0_fire)
  io.lsq.idResp.bits.sid := RegEnable(e0_sid, e0_fire)
  io.lsq.idResp.bits.is2lq := RegEnable(!isStore(e0_req.cmd), e0_fire)
  io.lsq.idResp.bits.nc := RegEnable(e0_req.nc, e0_fire)

  /******************************************************************
   * Uncache Req
   *  Version 0 (better timing)
   *    q0: choose which one is sent
   *    q0: sent
   *
   *  Version 1 (better performance)
   *    solved in one cycle for achieving the original performance.
   *    NOTE: "Enter Buffer" & "Uncache Req" not a continuous pipeline,
   *          because there is no guarantee that mem_aquire will be always ready.
   ******************************************************************/

  val q0_canSentVec = sizeMap(i =>
    (io.enableOutstanding || uState === s_idle) &&
    states(i).can2Bus()
  )
  val q0_res = PriorityEncoderWithFlag(q0_canSentVec)
  q0_canSentIdx := q0_res._1
  q0_canSent := q0_res._2
  q0_entry := entries(q0_canSentIdx)

  val size = PopCount(q0_entry.mask)
  val (lgSize, legal) = PriorityMuxWithFlag(Seq(
    1.U -> 0.U,
    2.U -> 1.U,
    4.U -> 2.U,
    8.U -> 3.U
  ).map(m => (size===m._1) -> m._2))
  assert(!(q0_canSent && !legal))

  val q0_load = edge.Get(
    fromSource      = q0_canSentIdx,
    toAddress       = q0_entry.addr,
    lgSize          = lgSize
  )._2

  val q0_store = edge.Put(
    fromSource      = q0_canSentIdx,
    toAddress       = q0_entry.addr,
    lgSize          = lgSize,
    data            = q0_entry.data,
    mask            = q0_entry.mask
  )._2

  val q0_isStore = q0_entry.cmd === MemoryOpConstants.M_XWR

  mem_acquire.valid := q0_canSent && !io.wfi.wfiReq
  mem_acquire.bits := Mux(q0_isStore, q0_store, q0_load)
  mem_acquire.bits.user.lift(MemBackTypeMM).foreach(_ := q0_entry.memBackTypeMM)
  mem_acquire.bits.user.lift(MemPageTypeNC).foreach(_ := q0_entry.nc)
  when(mem_acquire.fire){
    states(q0_canSentIdx).setInflight(true.B)
    noPending(q0_canSentIdx) := false.B

    // q0 should judge whether wait same block
    (0 until UncacheBufferSize).map(j =>
      when(q0_canSentIdx =/= j.U && states(j).isValid() && !states(j).isWaitReturn() && addrMatch(q0_entry, entries(j))){
        states(j).setWaitSame(true.B)
      }
    )
  }

  // uncache store but memBackTypeMM should update the golden memory
  if (env.EnableDifftest) {
    val difftest = DifftestModule(new DiffUncacheMMStoreEvent, delay = 1)
    difftest.coreid := io.hartId
    difftest.index  := 0.U
    difftest.valid  := mem_acquire.fire && isStore(entries(q0_canSentIdx)) && entries(q0_canSentIdx).memBackTypeMM
    difftest.addr   := entries(q0_canSentIdx).addr
    difftest.data   := entries(q0_canSentIdx).data.asTypeOf(Vec(DataBytes, UInt(8.W)))
    difftest.mask   := entries(q0_canSentIdx).mask
  }

  /******************************************************************
   * Uncache Resp
   ******************************************************************/

  val (_, _, refill_done, _) = edge.addr_inc(mem_grant)

  mem_grant.ready := true.B
  when (mem_grant.fire) {
    val id = mem_grant.bits.source
    entries(id).update(mem_grant.bits)
    states(id).updateUncacheResp()
    noPending(id) := true.B
    assert(refill_done, "Uncache response should be one beat only!")

    // remove state of wait same block
    (0 until UncacheBufferSize).map(j =>
      when(id =/= j.U && states(j).isValid() && states(j).isWaitSame() && addrMatch(entries(id), entries(j))){
        states(j).setWaitSame(false.B)
      }
    )
  }
  io.busError.ecc_error.valid := mem_grant.fire && isStore(entries(mem_grant.bits.source)) &&
    (mem_grant.bits.denied || mem_grant.bits.corrupt)
  io.busError.ecc_error.bits := entries(mem_grant.bits.source).addr >> blockOffBits << blockOffBits

  io.wfi.wfiSafe := GatedValidRegNext(noPending.asUInt.andR && io.wfi.wfiReq)
  /******************************************************************
   * Return to LSQ
   ******************************************************************/

  val r0_canSentVec = sizeMap(i => states(i).can2Lsq())
  val (r0_canSentIdx, r0_canSent) = PriorityEncoderWithFlag(r0_canSentVec)
  resp.valid := r0_canSent
  resp.bits := entries(r0_canSentIdx).toUncacheWordResp(r0_canSentIdx)
  when(resp.fire){
    states(r0_canSentIdx).updateReturn()
  }


  /******************************************************************
   * Buffer Flush
   * 1. when io.flush.valid is true: drain store queue and ubuffer
   ******************************************************************/
  empty := !VecInit(states.map(_.isValid())).asUInt.orR
  io.flush.empty := empty


  /******************************************************************
   * Load Data Forward to loadunit
   *  f0: vaddr match, fast resp
   *  f1: mask & data select, merge; paddr match; resp
   *      NOTE: forward.paddr from dtlb, which is far from uncache f0
   ******************************************************************/

  val f0_validMask = sizeMap(i => isStore(entries(i)) && states(i).isValid())
  val f0_fwdMaskCandidates = VecInit(entries.map(e => e.mask))
  val f0_fwdDataCandidates = VecInit(entries.map(e => e.data))
  val f1_fwdMaskCandidates = sizeMap(i => RegEnable(entries(i).mask, f0_validMask(i)))
  val f1_fwdDataCandidates = sizeMap(i => RegEnable(entries(i).data, f0_validMask(i)))
  val f1_tagMismatchVec = Wire(Vec(LoadPipelineWidth, Bool()))
  f1_needDrain := f1_tagMismatchVec.asUInt.orR && !empty

  for ((forward, i) <- io.forward.zipWithIndex) {
    val f0_fwdValid = forward.valid
    val f1_fwdValid = RegNext(f0_fwdValid)

    /* f0 */
    // vaddr match
    val f0_vtagMatches = sizeMap(w => addrMatch(entries(w).vaddr, forward.vaddr))
    val f0_flyTagMatches = sizeMap(w => f0_vtagMatches(w) && f0_validMask(w) && f0_fwdValid && states(w).isFwdOld())
    val f0_idleTagMatches = sizeMap(w => f0_vtagMatches(w) && f0_validMask(w) && f0_fwdValid && states(w).isFwdNew())
    // ONLY for fast use to get better timing
    val f0_flyMaskFast = shiftMaskToHigh(
      forward.vaddr,
      Mux1H(f0_flyTagMatches, f0_fwdMaskCandidates)
    ).asTypeOf(Vec(VDataBytes, Bool()))
    val f0_idleMaskFast = shiftMaskToHigh(
      forward.vaddr,
      Mux1H(f0_idleTagMatches, f0_fwdMaskCandidates)
    ).asTypeOf(Vec(VDataBytes, Bool()))

    /* f1 */
    val f1_flyTagMatches = RegEnable(f0_flyTagMatches, f0_fwdValid)
    val f1_idleTagMatches = RegEnable(f0_idleTagMatches, f0_fwdValid)
    val f1_fwdPAddr = RegEnable(forward.paddr, f0_fwdValid)
    // select
    val f1_flyMask = Mux1H(f1_flyTagMatches, f1_fwdMaskCandidates)
    val f1_flyData = Mux1H(f1_flyTagMatches, f1_fwdDataCandidates)
    val f1_idleMask = Mux1H(f1_idleTagMatches, f1_fwdMaskCandidates)
    val f1_idleData = Mux1H(f1_idleTagMatches, f1_fwdDataCandidates)
    // merge old(inflight) and new(idle)
    val (f1_fwdDataTmp, f1_fwdMaskTmp) = doMerge(f1_flyData, f1_flyMask, f1_idleData, f1_idleMask)
    val f1_fwdMask = shiftMaskToHigh(f1_fwdPAddr, f1_fwdMaskTmp).asTypeOf(Vec(VDataBytes, Bool()))
    val f1_fwdData = shiftDataToHigh(f1_fwdPAddr, f1_fwdDataTmp).asTypeOf(Vec(VDataBytes, UInt(8.W)))
    // paddr match and mismatch judge
    val f1_ptagMatches = sizeMap(w => addrMatch(RegEnable(entries(w).addr, f0_fwdValid), f1_fwdPAddr))
    f1_tagMismatchVec(i) := sizeMap(w =>
      RegEnable(f0_vtagMatches(w), f0_fwdValid) =/= f1_ptagMatches(w) && RegEnable(f0_validMask(w), f0_fwdValid) && f1_fwdValid
    ).asUInt.orR
    XSDebug(
      f1_tagMismatchVec(i),
      "forward tag mismatch: pmatch %x vmatch %x vaddr %x paddr %x\n",
      f1_ptagMatches.asUInt,
      RegEnable(f0_vtagMatches.asUInt, f0_fwdValid),
      RegEnable(forward.vaddr, f0_fwdValid),
      RegEnable(forward.paddr, f0_fwdValid)
    )
    // response
    forward.addrInvalid := false.B // addr in ubuffer is always ready
    forward.dataInvalid := false.B // data in ubuffer is always ready
    forward.matchInvalid := f1_tagMismatchVec(i) // paddr / vaddr cam result does not match
    for (j <- 0 until VDataBytes) {
      forward.forwardMaskFast(j) := f0_flyMaskFast(j) || f0_idleMaskFast(j)

      forward.forwardData(j) := f1_fwdData(j)
      forward.forwardMask(j) := false.B
      when(f1_fwdMask(j) && f1_fwdValid) {
        forward.forwardMask(j) := true.B
      }
    }

  }


  /******************************************************************
   * Debug / Performance
   ******************************************************************/

  /* Debug Counters */
  // print all input/output requests for debug purpose
  // print req/resp
  XSDebug(req.fire, "req cmd: %x addr: %x data: %x mask: %x\n",
    req.bits.cmd, req.bits.addr, req.bits.data, req.bits.mask)
  XSDebug(resp.fire, "data: %x\n", req.bits.data)
  // print tilelink messages
  XSDebug(mem_acquire.valid, "mem_acquire valid, ready=%d ", mem_acquire.ready)
  mem_acquire.bits.dump(mem_acquire.valid)

  XSDebug(mem_grant.fire, "mem_grant fire ")
  mem_grant.bits.dump(mem_grant.fire)

  /* Performance Counters */
  XSPerfAccumulate("e0_reject", e0_reject && e0_req_valid)
  XSPerfAccumulate("e0_total_enter", e0_fire)
  XSPerfAccumulate("e0_merge", e0_fire && e0_canMerge)
  XSPerfAccumulate("e0_alloc_simple", e0_fire && e0_canAlloc && !e0_allocWaitSame)
  XSPerfAccumulate("e0_alloc_wait_same", e0_fire && e0_canAlloc && e0_allocWaitSame)
  XSPerfAccumulate("q0_acquire", q0_canSent)
  XSPerfAccumulate("q0_acquire_store", q0_canSent && q0_isStore)
  XSPerfAccumulate("q0_acquire_load", q0_canSent && !q0_isStore)
  XSPerfAccumulate("uncache_memBackTypeMM", io.lsq.req.fire && io.lsq.req.bits.memBackTypeMM)
  XSPerfAccumulate("uncache_mmio_store", io.lsq.req.fire && isStore(io.lsq.req.bits.cmd) && !io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_mmio_load", io.lsq.req.fire && !isStore(io.lsq.req.bits.cmd) && !io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_nc_store", io.lsq.req.fire && isStore(io.lsq.req.bits.cmd) && io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_nc_load", io.lsq.req.fire && !isStore(io.lsq.req.bits.cmd) && io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_outstanding", uState =/= s_idle && mem_acquire.fire)
  XSPerfAccumulate("forward_count", PopCount(io.forward.map(_.forwardMask.asUInt.orR)))
  XSPerfAccumulate("forward_vaddr_match_failed", PopCount(f1_tagMismatchVec))

  val perfEvents = Seq(
    ("uncache_mmio_store", io.lsq.req.fire && isStore(io.lsq.req.bits.cmd) && !io.lsq.req.bits.nc),
    ("uncache_mmio_load", io.lsq.req.fire && !isStore(io.lsq.req.bits.cmd) && !io.lsq.req.bits.nc),
    ("uncache_nc_store", io.lsq.req.fire && isStore(io.lsq.req.bits.cmd) && io.lsq.req.bits.nc),
    ("uncache_nc_load", io.lsq.req.fire && !isStore(io.lsq.req.bits.cmd) && io.lsq.req.bits.nc),
    ("uncache_outstanding", uState =/= s_idle && mem_acquire.fire),
    ("forward_count", PopCount(io.forward.map(_.forwardMask.asUInt.orR))),
    ("forward_vaddr_match_failed", PopCount(f1_tagMismatchVec))
  )

  generatePerfEvent()
  //  End
}
