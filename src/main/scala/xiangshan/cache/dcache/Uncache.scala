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
import xscache.coupledL2.{MemBackTypeMM, MemBackTypeMMField, MemPageTypeNC, MemPageTypeNCField}
import difftest._

trait HasUncacheBufferParameters extends HasXSParameter with HasDCacheParameters {

  def doMerge(oldData: UInt, oldMask: UInt, newData:UInt, newMask: UInt):(UInt, UInt) = {
    val resData = VecInit((0 until DataBytes).map(j =>
      Mux(newMask(j), newData(8*(j+1)-1, 8*j), oldData(8*(j+1)-1, 8*j))
    )).asUInt
    val resMask = newMask | oldMask
    (resData, resMask)
  }

  def indexWidth = log2Up(UncacheBufferSize)
  def blockOffset = log2Up(XLEN / 8)
  def getBlockAddr(x: UInt) = x >> blockOffset
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

  val respNderr = Bool()
  val respDerr = Bool()

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
    respNderr := false.B
    respDerr := false.B
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
      addr := (getBlockAddr(addr) << blockOffset) | resOffset
      vaddr := (getBlockAddr(vaddr) << blockOffset) | resOffset
    }
  }

  def update(x: TLBundleD): Unit = {
    when(cmd === MemoryOpConstants.M_XRD) {
      data := x.data
    }
    respNderr := x.denied
    respDerr := x.corrupt && !x.denied
  }

  // def update(forwardData: UInt, forwardMask: UInt): Unit = {
  //   fwd_data := forwardData
  //   fwd_mask := forwardMask
  // }

  def toUncacheWordResp(eid: UInt): UncacheWordResp = {
    // val resp_fwd_data = VecInit((0 until DataBytes).map(j =>
    //   Mux(fwd_mask(j), fwd_data(8*(j+1)-1, 8*j), data(8*(j+1)-1, 8*j))
    // )).asUInt
    val respFwdData = data
    val r = Wire(new UncacheWordResp)
    r := DontCare
    r.data := respFwdData
    r.id := eid
    r.nderr := respNderr
    r.nc := nc
    r.is2lq := cmd === MemoryOpConstants.M_XRD
    r.miss := false.B
    r.replay := false.B
    r.tagError := false.B
    r.error := respDerr
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
  val forward = Vec(LoadPipelineWidth, Flipped(new UncacheForward))
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
  val memAcquire = bus.a
  val memGrant   = bus.d
  val reqReady = WireInit(false.B)

  // assign default values to output signals
  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.d.ready := false.B
  bus.e.valid := false.B
  bus.e.bits  := DontCare
  io.lsq.req.ready := reqReady
  io.lsq.resp.valid := false.B
  io.lsq.resp.bits := DontCare


  /******************************************************************
   * Data Structure
   ******************************************************************/

  val entries = Reg(Vec(UncacheBufferSize, new UncacheEntry))
  val states = RegInit(VecInit(Seq.fill(UncacheBufferSize)(0.U.asTypeOf(new UncacheEntryState))))
  val sIdle :: s_inflight :: s_wait_return :: Nil = Enum(3)
  val uState = RegInit(sIdle)
  val noPending = RegInit(VecInit(Seq.fill(UncacheBufferSize)(true.B)))

  // drain buffer
  val empty = Wire(Bool())
  val f1NeedDrain = Wire(Bool())
  val doUarchDrain = RegInit(false.B)
  when((f1NeedDrain || io.flush.valid) && !empty){
    doUarchDrain := true.B
  }.elsewhen(empty){
    doUarchDrain := false.B
  }.otherwise{
    doUarchDrain := false.B
  }

  val q0Entry = Wire(new UncacheEntry)
  val q0CanSentIdx = Wire(UInt(indexWidth.W))
  val q0CanSent = Wire(Bool())


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
      !(memGrant.fire && memGrant.bits.source === eid || states(eid).isWaitReturn())
  }

  def canMergeSecondary(eid: UInt): Bool = {
    // old entry is not inflight and senting
    states(eid).canMerge() && !(q0CanSent && q0CanSentIdx === eid)
  }

  /******************************************************************
   * uState for non-outstanding
   ******************************************************************/

  switch(uState){
    is(sIdle){
      when(memAcquire.fire){
        uState := s_inflight
      }
    }
    is(s_inflight){
      when(memGrant.fire){
        uState := s_wait_return
      }
    }
    is(s_wait_return){
      when(resp.fire){
        uState := sIdle
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
  val e0Fire = req.fire
  val e0ReqValid = req.valid
  val e0Req = req.bits

  val e0RejectVec = Wire(Vec(UncacheBufferSize, Bool()))
  val e0MergeVec = Wire(Vec(UncacheBufferSize, Bool()))
  val e0AllocWaitSameVec = Wire(Vec(UncacheBufferSize, Bool()))
  sizeForeach(i => {
    val valid = e0ReqValid && states(i).isValid()
    val isAddrMatch = addrMatch(e0Req, entries(i))
    val canMerge1 = canMergePrimary(e0Req, entries(i), i.U)
    val canMerge2 = canMergeSecondary(i.U)
    e0RejectVec(i) := valid && isAddrMatch && !canMerge1
    e0MergeVec(i) := valid && isAddrMatch && canMerge1 && canMerge2
    e0AllocWaitSameVec(i) := valid && isAddrMatch && canMerge1 && !canMerge2
  })
  assert(PopCount(e0MergeVec) <= 1.U, "Uncache buffer should not merge multiple entries")

  val e0InvalidVec = sizeMap(i => !states(i).isValid())
  val (e0_mergeIdx, e0_canMerge) = PriorityEncoderWithFlag(e0MergeVec)
  val (e0_allocIdx, e0_canAlloc) = PriorityEncoderWithFlag(e0InvalidVec)
  val e0AllocWaitSame = e0AllocWaitSameVec.reduce(_ || _)
  val e0Sid = Mux(e0_canMerge, e0_mergeIdx, e0_allocIdx)
  val e0Reject = doUarchDrain || (!e0_canMerge && !e0InvalidVec.asUInt.orR) || e0RejectVec.reduce(_ || _)

  // e0_fire is used to guarantee that it will not be rejected
  when(e0_canMerge && e0ReqValid){
    entries(e0_mergeIdx).update(e0Req)
  }.elsewhen(e0_canAlloc && e0Fire){
    entries(e0_allocIdx).set(e0Req)
    states(e0_allocIdx).setValid(true.B)
    when(e0AllocWaitSame){
      states(e0_allocIdx).setWaitSame(true.B)
    }
  }

  reqReady := !e0Reject

  /* e1: return accept */
  io.lsq.idResp.valid := RegNext(e0Fire)
  io.lsq.idResp.bits.mid := RegEnable(e0Req.id, e0Fire)
  io.lsq.idResp.bits.sid := RegEnable(e0Sid, e0Fire)
  io.lsq.idResp.bits.is2lq := RegEnable(!isStore(e0Req.cmd), e0Fire)
  io.lsq.idResp.bits.nc := RegEnable(e0Req.nc, e0Fire)

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

  val q0CanSentVec = sizeMap(i => states(i).can2Bus())
  val q0Res = PriorityEncoderWithFlag(q0CanSentVec)
  q0CanSentIdx := q0Res._1
  q0CanSent := q0Res._2 && (io.enableOutstanding || uState === sIdle)
  q0Entry := entries(q0CanSentIdx)

  val size = PopCount(q0Entry.mask)
  val (lgSize, legal) = PriorityMuxWithFlag(Seq(
    1.U -> 0.U,
    2.U -> 1.U,
    4.U -> 2.U,
    8.U -> 3.U
  ).map(m => (size===m._1) -> m._2))
  assert(!(q0CanSent && !legal))

  val q0Load = edge.Get(
    fromSource      = q0CanSentIdx,
    toAddress       = q0Entry.addr,
    lgSize          = lgSize
  )._2

  val q0Store = edge.Put(
    fromSource      = q0CanSentIdx,
    toAddress       = q0Entry.addr,
    lgSize          = lgSize,
    data            = q0Entry.data,
    mask            = q0Entry.mask
  )._2

  val q0IsStore = q0Entry.cmd === MemoryOpConstants.M_XWR

  memAcquire.valid := q0CanSent && !io.wfi.wfiReq
  memAcquire.bits := Mux(q0IsStore, q0Store, q0Load)
  memAcquire.bits.user.lift(MemBackTypeMM).foreach(_ := q0Entry.memBackTypeMM)
  memAcquire.bits.user.lift(MemPageTypeNC).foreach(_ := q0Entry.nc)
  when(memAcquire.fire){
    states(q0CanSentIdx).setInflight(true.B)
    noPending(q0CanSentIdx) := false.B

    // q0 should judge whether wait same block
    (0 until UncacheBufferSize).map(j =>
      when(q0CanSentIdx =/= j.U && states(j).isValid() && !states(j).isWaitReturn() && addrMatch(q0Entry, entries(j))){
        states(j).setWaitSame(true.B)
      }
    )
  }

  // uncache store but memBackTypeMM should update the golden memory
  if (env.EnableDifftest) {
    val difftest = DifftestModule(new DiffUncacheMMStoreEvent, delay = 1)
    difftest.coreid := io.hartId
    difftest.index  := 0.U
    difftest.valid  := memAcquire.fire && isStore(entries(q0CanSentIdx)) && entries(q0CanSentIdx).memBackTypeMM
    difftest.addr   := entries(q0CanSentIdx).addr
    difftest.data   := entries(q0CanSentIdx).data.asTypeOf(Vec(DataBytes, UInt(8.W)))
    difftest.mask   := entries(q0CanSentIdx).mask
  }

  /******************************************************************
   * Uncache Resp
   ******************************************************************/

  val (_, _, refillDone, _) = edge.addr_inc(memGrant)

  memGrant.ready := true.B
  when (memGrant.fire) {
    val id = memGrant.bits.source
    entries(id).update(memGrant.bits)
    states(id).updateUncacheResp()
    noPending(id) := true.B
    assert(refillDone, "Uncache response should be one beat only!")

    // remove state of wait same block
    (0 until UncacheBufferSize).map(j =>
      when(id =/= j.U && states(j).isValid() && states(j).isWaitSame() && addrMatch(entries(id), entries(j))){
        states(j).setWaitSame(false.B)
      }
    )
  }
  io.busError.ecc_error.valid := memGrant.fire && isStore(entries(memGrant.bits.source)) &&
    (memGrant.bits.denied || memGrant.bits.corrupt)
  io.busError.ecc_error.bits := entries(memGrant.bits.source).addr >> blockOffBits << blockOffBits

  io.wfi.wfiSafe := GatedValidRegNext(noPending.asUInt.andR && io.wfi.wfiReq)
  /******************************************************************
   * Return to LSQ
   ******************************************************************/

  val r0CanSentVec = sizeMap(i => states(i).can2Lsq())
  val (r0_canSentIdx, r0_canSent) = PriorityEncoderWithFlag(r0CanSentVec)
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

  val f0ValidMask = sizeMap(i => isStore(entries(i)) && states(i).isValid())
  val f0FwdMaskCandidates = VecInit(entries.map(e => e.mask))
  val f0FwdDataCandidates = VecInit(entries.map(e => e.data))
  val f1FwdMaskCandidates = sizeMap(i => RegEnable(entries(i).mask, f0ValidMask(i)))
  val f1FwdDataCandidates = sizeMap(i => RegEnable(entries(i).data, f0ValidMask(i)))
  val f1TagMismatchVec = Wire(Vec(LoadPipelineWidth, Bool()))
  f1NeedDrain := f1TagMismatchVec.asUInt.orR && !empty

  for ((forward, i) <- io.forward.zipWithIndex) {
    val fn1FwdValid = forward.s0Req.valid
    val fn1Req = forward.s0Req.bits
    val f0FwdValid = RegNext(fn1FwdValid)
    val f0Req = RegEnable(fn1Req, fn1FwdValid)
    val f0Paddr = forward.s1Req.paddr
    val f0Kill = forward.s1Kill
    val f1FwdValid = RegNext(f0FwdValid)

    /* f0 */
    // vaddr match
    val f0VtagMatches = sizeMap(w => addrMatch(entries(w).vaddr, f0Req.vaddr))
    val f0FlyTagMatches = sizeMap(w => f0VtagMatches(w) && f0ValidMask(w) && f0FwdValid && states(w).isFwdOld())
    val f0IdleTagMatches = sizeMap(w => f0VtagMatches(w) && f0ValidMask(w) && f0FwdValid && states(w).isFwdNew())
    // ONLY for fast use to get better timing
    val f0FlyMaskFast = shiftMaskToHigh(
      f0Req.vaddr,
      Mux1H(f0FlyTagMatches, f0FwdMaskCandidates)
    ).asTypeOf(Vec(VDataBytes, Bool()))
    val f0IdleMaskFast = shiftMaskToHigh(
      f0Req.vaddr,
      Mux1H(f0IdleTagMatches, f0FwdMaskCandidates)
    ).asTypeOf(Vec(VDataBytes, Bool()))

    /* f1 */
    val f1FlyTagMatches = RegEnable(f0FlyTagMatches, f0FwdValid)
    val f1IdleTagMatches = RegEnable(f0IdleTagMatches, f0FwdValid)
    val f1FwdPAddr = RegEnable(f0Paddr, f0FwdValid)
    val f1Kill = RegEnable(f0Kill, f0FwdValid)
    // select
    val f1FlyMask = Mux1H(f1FlyTagMatches, f1FwdMaskCandidates)
    val f1FlyData = Mux1H(f1FlyTagMatches, f1FwdDataCandidates)
    val f1IdleMask = Mux1H(f1IdleTagMatches, f1FwdMaskCandidates)
    val f1IdleData = Mux1H(f1IdleTagMatches, f1FwdDataCandidates)
    // merge old(inflight) and new(idle)
    val (f1_fwdDataTmp, f1_fwdMaskTmp) = doMerge(f1FlyData, f1FlyMask, f1IdleData, f1IdleMask)
    val f1FwdMask = shiftMaskToHigh(f1FwdPAddr, f1_fwdMaskTmp).asTypeOf(Vec(VDataBytes, Bool()))
    val f1FwdData = shiftDataToHigh(f1FwdPAddr, f1_fwdDataTmp).asTypeOf(Vec(VDataBytes, UInt(8.W)))
    // paddr match and mismatch judge
    val f1PtagMatches = sizeMap(w => addrMatch(RegEnable(entries(w).addr, f0FwdValid), f1FwdPAddr))
    f1TagMismatchVec(i) := sizeMap(w =>
      RegEnable(f0VtagMatches(w), f0FwdValid) =/= f1PtagMatches(w) && RegEnable(f0ValidMask(w), f0FwdValid) && f1FwdValid
    ).asUInt.orR && !f1Kill
    XSDebug(
      f1TagMismatchVec(i),
      "forward tag mismatch: pmatch %x vmatch %x vaddr %x paddr %x\n",
      f1PtagMatches.asUInt,
      RegEnable(f0VtagMatches.asUInt, f0FwdValid),
      RegEnable(f0Req.vaddr, f0FwdValid),
      RegEnable(f0Paddr, f0FwdValid)
    )
    // response
    forward.s2Resp.bits.matchInvalid := f1TagMismatchVec(i) // paddr / vaddr cam result does not match
    for (j <- 0 until VDataBytes) {
      forward.s2Resp.bits.forwardData(j) := f1FwdData(j)
      forward.s2Resp.bits.forwardMask(j) := false.B
      when(f1FwdMask(j) && f1FwdValid) {
        forward.s2Resp.bits.forwardMask(j) := true.B
      }
    }
    forward.s2Resp.valid := f1FwdValid

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
  XSDebug(memAcquire.valid, "mem_acquire valid, ready=%d ", memAcquire.ready)
  memAcquire.bits.dump(memAcquire.valid)

  XSDebug(memGrant.fire, "mem_grant fire ")
  memGrant.bits.dump(memGrant.fire)

  /* Performance Counters */
  XSPerfAccumulate("e0_reject", e0Reject && e0ReqValid)
  XSPerfAccumulate("e0_total_enter", e0Fire)
  XSPerfAccumulate("e0_merge", e0Fire && e0_canMerge)
  XSPerfAccumulate("e0_alloc_simple", e0Fire && e0_canAlloc && !e0AllocWaitSame)
  XSPerfAccumulate("e0_alloc_wait_same", e0Fire && e0_canAlloc && e0AllocWaitSame)
  XSPerfAccumulate("q0_acquire", q0CanSent)
  XSPerfAccumulate("q0_acquire_store", q0CanSent && q0IsStore)
  XSPerfAccumulate("q0_acquire_load", q0CanSent && !q0IsStore)
  XSPerfAccumulate("uncache_memBackTypeMM", io.lsq.req.fire && io.lsq.req.bits.memBackTypeMM)
  XSPerfAccumulate("uncache_mmio_store", io.lsq.req.fire && isStore(io.lsq.req.bits.cmd) && !io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_mmio_load", io.lsq.req.fire && !isStore(io.lsq.req.bits.cmd) && !io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_nc_store", io.lsq.req.fire && isStore(io.lsq.req.bits.cmd) && io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_nc_load", io.lsq.req.fire && !isStore(io.lsq.req.bits.cmd) && io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_outstanding", uState =/= sIdle && memAcquire.fire)
  XSPerfAccumulate("forward_count", PopCount(io.forward.map(_.s2Resp.bits.forwardMask.asUInt.orR)))
  XSPerfAccumulate("forward_vaddr_match_failed", PopCount(f1TagMismatchVec))

  val perfEvents = Seq(
    ("uncache_mmio_store", io.lsq.req.fire && isStore(io.lsq.req.bits.cmd) && !io.lsq.req.bits.nc),
    ("uncache_mmio_load", io.lsq.req.fire && !isStore(io.lsq.req.bits.cmd) && !io.lsq.req.bits.nc),
    ("uncache_nc_store", io.lsq.req.fire && isStore(io.lsq.req.bits.cmd) && io.lsq.req.bits.nc),
    ("uncache_nc_load", io.lsq.req.fire && !isStore(io.lsq.req.bits.cmd) && io.lsq.req.bits.nc),
    ("uncache_outstanding", uState =/= sIdle && memAcquire.fire),
    ("forward_count", PopCount(io.forward.map(_.s2Resp.bits.forwardMask.asUInt.orR))),
    ("forward_vaddr_match_failed", PopCount(f1TagMismatchVec))
  )

  generatePerfEvent()
  //  End
}
