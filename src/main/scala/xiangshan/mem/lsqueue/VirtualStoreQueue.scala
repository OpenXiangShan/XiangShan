/***************************************************************************************
 * Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          https://license.coscl.org.cn/MulanPSL2
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package xiangshan.mem

import chisel3._
import chisel3.util._
import difftest._
import difftest.common.DifftestMem
import org.chipsalliance.cde.config.Parameters
import top.ArgParser
import utility._
import xiangshan.backend.Bundles.{DynInst, UopIdx}
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.vector.Bundles.NumLsElem
import xiangshan.backend.rob.RobPtr
import xiangshan.{XSCoreParamsKey, XSModule, XSTileKey}

class VirtualStoreQueueDataEntry (implicit p: Parameters) extends MemBlockBundle {
  val reqNum   = NumLsElem()
  val robIdx   = new RobPtr
  val uopIdx   = UopIdx()

  // debug info
  val debugUop = Option.when(debugEn)(new DynInst())
}

class VirtualStoreQueueCtrlEntry (implicit p: Parameters) extends MemBlockBundle {
  val allocated = Bool()
  val isVec = Bool()
  val vecMbCommit = Bool()
}

class VirtualStoreQueue[PhysicalQueuePtrType <: MultiFlagCircularQueuePtr[PhysicalQueuePtrType]] (
                         PhysicalQueueSize: Int, Multiple: Int, SnapshotInterval: Int,
                         PhysicalQueuePtr: PhysicalQueuePtrType) (implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper with HasMemBlockParameters {

  require(isPow2(SnapshotInterval))
  //TODO: currently we only support snapshot interval of 1, which means every entry is a snapshot entry, to simplify the design.
  /* we may support larger snapshot interval in the future, but need to solve the issue of physicalQueue pointer calculation:
  *  eg. deqPtr = 2, snapshot entry is 0, which had dequeued. The calculation of physical queue pointer for entry 2 maybe wrong (snapshot 0 maybe overwrite).
  */

  require(SnapshotInterval == 1)
  require(PhysicalQueueSize * Multiple % SnapshotInterval == 0)

  val io = IO(new VirtualStoreQueueIO(PhysicalQueuePtr))

  val Size = PhysicalQueueSize * Multiple
  val EnqLength = io.enq.req.length
  val SnapshotWidth = log2Ceil(SnapshotInterval)
  val reqNumSumWidth = log2Ceil(Size + 1)

  protected object WalkState extends ChiselEnum {
    val idle = Value
    val release = Value // release entries which had be retired by rob
    val walk = Value // redirect physical queue
  }
  class VirtualStoreQueuePtr(implicit p: Parameters) extends CircularQueuePtr[VirtualStoreQueuePtr](p => Size) {
    def getSnapshotPtr: UInt = {
      this.value(this.value.getWidth - 1, SnapshotWidth)
    }

    def getSnapshotBase: UInt = {
      if (SnapshotWidth == 0) this.value(this.value.getWidth - 1, SnapshotWidth)
      else Cat(this.value(this.value.getWidth - 1, SnapshotWidth), Fill(SnapshotWidth, 0.U(1.W)))
    }

    def getSnapshotOffset: UInt = {
      if (SnapshotWidth == 0) 0.U
      else this.value(SnapshotWidth - 1, 0)
    }

    def isRotateBy[T <: VirtualStoreQueuePtr](right: T): Bool = (this.flag ^ right.flag) && this.value === right.value
  }
  class SnapshotEntry(implicit p: Parameters) extends MemBlockBundle {
    val entryEndSqIdx = PhysicalQueuePtr.cloneType
  }

  val dataEntries   = Reg(Vec(Size, new VirtualStoreQueueDataEntry))
  val ctrlEntries   = RegInit(VecInit(Seq.fill(Size)(0.U.asTypeOf(new VirtualStoreQueueCtrlEntry))))
  val snapshotQueue = Reg(Vec(Size / SnapshotInterval, new SnapshotEntry())) // first entry's end sqIdx in each snapshot group
  val enqPtrVec     = RegInit(VecInit((0 until EnqLength).map(_.U.asTypeOf(new VirtualStoreQueuePtr))))
  val deqPtrVec     = RegInit(VecInit((0 until CommitWidth).map(_.U.asTypeOf(new VirtualStoreQueuePtr))))
  val preCommitPtr  = RegInit(0.U.asTypeOf(new VirtualStoreQueuePtr))
  val allowEnqueue  = RegInit(true.B)
  val state         = RegInit(WalkState.idle)
  val stateNext     = WireInit(state)
  val needCancel    = WireInit(VecInit(Seq.fill(Size)(false.B)))
  val retireVec     = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val physicalQueueEnqPtr = RegInit(0.U.asTypeOf(PhysicalQueuePtr.cloneType))
  // TODO: vecMbCommit will be remove in the future.
  val vecCommittmp = Wire(Vec(StoreQueueSize, Vec(VecStorePipelineWidth, Bool())))
  val vecCommit = Wire(Vec(StoreQueueSize, Bool()))

  // Reconstruct per-entry end sqIdx from the snapshot group's first-entry end sqIdx.
  private def getEntryEndSqIdx(entryPtr: VirtualStoreQueuePtr): PhysicalQueuePtrType = {
    val snapshotIdx       = entryPtr.getSnapshotPtr
    val snapshotBaseValue = entryPtr.getSnapshotBase
    val entryOffset       = entryPtr.getSnapshotOffset

    val snapshotBase = snapshotQueue(snapshotIdx).entryEndSqIdx

    val reqNumAfterFirstEntry = (1 until SnapshotInterval).map { offset =>
      Mux(
        entryOffset >= offset.U,
        dataEntries(snapshotBaseValue + offset.U).reqNum.asUInt,
        0.U(reqNumSumWidth.W)
      )
    }.foldLeft(0.U(reqNumSumWidth.W))(_ +& _)

    snapshotBase + reqNumAfterFirstEntry
  }

  private def getEntryStartSqIdx(entryPtr: VirtualStoreQueuePtr): PhysicalQueuePtrType = {
    getEntryEndSqIdx(entryPtr) - dataEntries(entryPtr.value).reqNum
  }

  private def makeEntryPtrFromValue(value: UInt): VirtualStoreQueuePtr = {
    val ptr = Wire(new VirtualStoreQueuePtr)
    ptr.flag := false.B // don't care flag
    ptr.value := value
    ptr
  }

  private def sumBalanced(inputs: Seq[UInt]): UInt = {
    require(inputs.nonEmpty)
    if (inputs.length == 1) {
      inputs.head
    } else {
      val grouped = inputs.grouped(2).map {
        case Seq(a, b) => a +& b
        case Seq(a) => a
      }.toSeq
      sumBalanced(grouped)
    }
  }

  state             := stateNext
  // if no redirect (state === idle), robHeadPtr is regNext, and is latched every cycle.
  // if have redirect (state =/= idle), robHeadPtr is regEnable, and is need to latch rob deqPtr head onece before redirect.
  private val robHeadPtr  = RegEnable(io.fromRob.robHeadPtr, state === WalkState.idle)
  private val redirectReg = Wire(io.redirect.cloneType)
  // [1]. need flush, don't need to release first.
  // [2]. need flush, release finish.
  redirectReg.valid := RegNext(io.redirect.valid) && state === WalkState.idle ||
    state === WalkState.walk
  redirectReg.bits  := RegEnable(io.redirect.bits, io.redirect.valid)

  val deqPtrHead = deqPtrVec.head

  // enq
  val allocatedPtrVec     = VecInit((0 until EnqLength).map(i =>
    enqPtrVec(PopCount(io.enq.req.take(i).map(req => req.valid && req.bits.needAlloc)))))
  val canEnqueue          = io.enq.req.map{ case req => req.valid && req.bits.needAlloc && io.enq.canAccept }
  val enqNumber           = Mux(io.enq.canAccept, PopCount(io.enq.req.map(req => req.valid && req.bits.needAlloc)), 0.U)

  for(i <- 0 until io.enq.req.length) {
    io.enq.resp(i).physicalQueuePtr := io.enq.req(i).bits.reqStartPtr.sqIdx
  }

  for (i <- 0 until Size) {
    val enqOH       = canEnqueue.zip(allocatedPtrVec.map(_.value === i.U)).map(x => x._1 && x._2)
    val enqBits     = Mux1H(enqOH, io.enq.req.map(_.bits))
    val enqSet      = enqOH.reduce(_ || _) && !io.redirect.valid
    val deqCancel   = deqPtrVec.zipWithIndex.map{ case (ptr, j) =>
      ptr.value === i.U && retireVec(j)}.reduce(_ || _)

    when(enqSet) {
      ctrlEntries(i).allocated := true.B
      ctrlEntries(i).isVec := enqBits.uop.isVec
    }.elsewhen(deqCancel || needCancel(i)) {
      ctrlEntries(i).allocated := false.B
      ctrlEntries(i).isVec := false.B
    }

    when(enqSet) {
      dataEntries(i).robIdx := enqBits.uop.robIdx
      dataEntries(i).uopIdx := enqBits.uop.uopIdx
    } // don't need to reset for low power, it will be set every instruction.

    when(enqSet) {
      dataEntries(i).reqNum := enqBits.uop.numLsElem
    }

    // cancel ctrl
    needCancel(i) := dataEntries(i).robIdx.needFlush(redirectReg) && ctrlEntries(i).allocated

    // snapshot queue
    when(enqSet && (i % SnapshotInterval == 0).B){
      snapshotQueue(i / SnapshotInterval).entryEndSqIdx := enqBits.reqEndPtr.sqIdx
    }

    // TODO: vecMbCommit will be remove in the future.
    val fbk = io.fromVMergeBuffer
    for (j <- 0 until VecStorePipelineWidth) {
      vecCommittmp(i)(j) := fbk(j).valid && (fbk(j).bits.isCommit || fbk(j).bits.isFlush) &&
        dataEntries(i).robIdx === fbk(j).bits.robidx && dataEntries(i).uopIdx === fbk(j).bits.uopidx
    }
    // vector feedback may occur with deqCancel/needCancel at the same time
    vecCommit(i) := vecCommittmp(i).reduce(_ || _) && !needCancel(i) && !deqCancel && ctrlEntries(i).allocated

    when (vecCommit(i)) {
      ctrlEntries(i).vecMbCommit := true.B
    }.elsewhen(deqCancel || needCancel(i)) {
      ctrlEntries(i).vecMbCommit := false.B
    }

    // debug info
    if(debugEn){
      when(enqSet) {
        dataEntries(i).debugUop.get := enqBits.debugUop.get
      }
    }
  }

  // query whether mdp hit when load query forward.
  //TODO: maybe we need the distance-base mdp.
  for(i <- 0 until LoadPipelineWidth) {
    // forward stage 0
    val s0Req = io.mdpQuery(i)
    val s0MdpHitVec = WireInit(VecInit((0 until StoreQueueSize).map(j =>
      s0Req.bits.loadWaitBit && dataEntries(j).robIdx === s0Req.bits.waitForRobIdx && ctrlEntries(j).allocated)))

    // forward stage 1
    val s1ReqValid  = RegNext(s0Req.valid && s0Req.bits.loadWaitBit)
    val s1MdpHitVec = RegEnable(s0MdpHitVec, s0Req.valid)
    //TODO: vector store maybe hit multiple entry, but we only care the first one, need to verify it in the future.
    val s1MdpHitIdx = ParallelPriorityEncoder(s1MdpHitVec.asUInt)

    val s1VirtualQueueHit = s1MdpHitVec.reduce(_ || _) && s1ReqValid

    val s2HitEntryPtr = RegEnable(makeEntryPtrFromValue(s1MdpHitIdx), s1ReqValid)

    val s2VirtualQueueSqIdx = WireDefault(0.U.asTypeOf(PhysicalQueuePtr.cloneType))
    s2VirtualQueueSqIdx := getEntryStartSqIdx(s2HitEntryPtr)

    io.toPhysicalQueue.mdpHitPtr(i).valid := RegNext(s1VirtualQueueHit)
    io.toPhysicalQueue.mdpHitPtr(i).bits  := s2VirtualQueueSqIdx
  }

  /**
   * Update physicalQueueEnqPtr
   * */
  when (io.toPhysicalQueue.redirectPtr.valid) {
    physicalQueueEnqPtr := io.toPhysicalQueue.redirectPtr.bits
  }.otherwise {
    val enqOH       = canEnqueue
    val enqBits     = ParallelPriorityMux(enqOH.reverse, io.enq.req.map(_.bits).reverse)
    val enqSet      = enqOH.reduce(_ || _) && !io.redirect.valid
    when(enqSet) {
      physicalQueueEnqPtr := enqBits.reqEndPtr.sqIdx
    }
  }

  // enq ctrl
  val validCnt = distanceBetween(enqPtrVec.head, deqPtrVec.head)
  allowEnqueue := enqPtrVec.head >= deqPtrVec.head && state === WalkState.idle

  // update enter queue pointer
  val cancelCount = PopCount(needCancel)
  val enqPtrVecNext = enqPtrVec.map{ case ptr =>
    val newPtr = WireInit(ptr)
    when(redirectReg.valid) {
      newPtr := ptr - cancelCount
    }.otherwise {
      newPtr := ptr + enqNumber
    }
    newPtr
  }

  val redirectLastValidPtrMoveCnt = cancelCount + 1.U
  val redirectLastValidPtr = RegEnable(enqPtrVec.head - redirectLastValidPtrMoveCnt, redirectReg.valid)

  enqPtrVec := enqPtrVecNext

  val headIsRetired = isBefore(dataEntries(deqPtrHead.value).robIdx, robHeadPtr) && ctrlEntries(deqPtrHead.value).allocated
  //redirect logic
  switch(state) {
    is(WalkState.idle) {
      when(io.redirect.valid && headIsRetired) { // have store need to retired first
        stateNext := WalkState.release
      }
    }
    is(WalkState.release) {
      when(!headIsRetired) {
        stateNext := WalkState.walk
      }
    }
    is(WalkState.walk) {
      stateNext := WalkState.idle
    }
  }

  // retired store, which had retired by rob
  val deqRobIdxVec = VecInit(deqPtrVec.map(ptr => dataEntries(ptr.value).robIdx))
  val deqAllocatedVec = VecInit(deqPtrVec.map(ptr => ctrlEntries(ptr.value).allocated))
  val deqReqNumVec = VecInit(deqPtrVec.map(ptr => dataEntries(ptr.value).reqNum.asUInt))
  val retireBaseVec = VecInit((0 until CommitWidth).map(i => isBefore(deqRobIdxVec(i), robHeadPtr) && deqAllocatedVec(i)))
  val deqMbCommitVec = VecInit(deqPtrVec.map(ptr => ctrlEntries(ptr.value).vecMbCommit))
  val mbCommitVec = VecInit((0 until CommitWidth).map(i => (deqRobIdxVec(i) === robHeadPtr) && deqMbCommitVec(i) && deqAllocatedVec(i)))
  val retireCount = PopCount(retireVec)
  val preCommitRelease = WireInit(VecInit(Seq.fill(EnsbufferWidth)(false.B)))
  val retireCarryVec = Wire(Vec(CommitWidth, Bool()))

  for (i <- 0 until CommitWidth) {
    val releaseHit = if (i < EnsbufferWidth) preCommitRelease(i) else false.B
    if (i == 0) {
      retireCarryVec(i) := (retireBaseVec(i) || mbCommitVec(i)) || releaseHit
    } else {
      retireCarryVec(i) := ((retireBaseVec(i) || mbCommitVec(i)) && retireCarryVec(i - 1)) || releaseHit
    }
    retireVec(i) := retireCarryVec(i)
  }

  val retireValid = retireVec.reduce(_ || _)
  val retiredReqNumVec = VecInit((0 until CommitWidth).map(i =>
    Mux(retireVec(i), deqReqNumVec(i), 0.U(reqNumSumWidth.W))
  ))
  val retireReqNumReg = RegEnable(sumBalanced(retiredReqNumVec), retireValid)
  val retireValidReg = RegNext(retireValid)

  val deqPtrVecNext = deqPtrVec.map(_ + retireCount)
  deqPtrVec := deqPtrVecNext

  // precommit store, it will be write to sbuffer before rob retire.
  val preCommitEntry = ctrlEntries(preCommitPtr.value)
  val preCommitMoveValid = dataEntries(preCommitPtr.value).robIdx === robHeadPtr &&
    preCommitEntry.allocated && (!preCommitEntry.isVec || preCommitEntry.vecMbCommit)

  val preCommitPtrNext = WireInit(preCommitPtr)
  when(redirectReg.valid) { // redirect next cycle update preCommitPtr
    preCommitPtrNext := deqPtrVecNext.head
  }.elsewhen(preCommitMoveValid && preCommitPtr =/= enqPtrVec.head) { // preCommitPtr advance
    preCommitPtrNext := preCommitPtr + 1.U
  }.elsewhen(preCommitPtr <= deqPtrHead) { // precommitPtr advance after deqPtr advance
    preCommitPtrNext := deqPtrVecNext.head
  }.otherwise {
    preCommitPtrNext := preCommitPtr
  }

  preCommitPtr := preCommitPtrNext

  XSError(preCommitPtr < deqPtrVec.head && RegNext(retireCount === 0.U) &&
    !deqPtrVec.head.isRotateBy(preCommitPtr), s"preCommitPtr had exceed deqPtr!\n")
  XSError(enqPtrVec.head < deqPtrVec.head &&
    !deqPtrVec.head.isRotateBy(enqPtrVec.head), s"enqPtr < deqPtr, something wrong!\n")
  XSError(preCommitPtr > enqPtrVec.head &&
    !preCommitPtr.isRotateBy(enqPtrVec.head), s"preCommitPtr had exceed enqPtr!\n")

  // io assign
  io.enq.canAccept := allowEnqueue
  io.empty := deqPtrVec.head === enqPtrVec.head

  io.toPhysicalQueue.physicalQueueEnqPtr := physicalQueueEnqPtr

  // retired logic, state driver
  // rob retire -> [next cycle] -> virtual storeQueue retire -> [next 2 cycle] -> physical storeQueue commit
  val toPhysicalQueueRetirePtr = RegInit(0.U.asTypeOf(PhysicalQueuePtr.cloneType))
  val toPhysicalQueueRetirePtrNext = WireInit(toPhysicalQueueRetirePtr)
  toPhysicalQueueRetirePtrNext := toPhysicalQueueRetirePtr + Mux(retireValidReg, retireReqNumReg, 0.U(retireReqNumReg.getWidth.W))

  toPhysicalQueueRetirePtr := toPhysicalQueueRetirePtrNext

  io.toPhysicalQueue.retiredPtr := toPhysicalQueueRetirePtrNext

  XSError(
    retireValidReg && toPhysicalQueueRetirePtrNext =/= getEntryEndSqIdx(deqPtrVec.head - 1.U),
    "retirePtr update error!\n"
  )

  // pre-commit logic, event driver
  val preCommitValid = RegNext(preCommitPtr > deqPtrHead || preCommitMoveValid) && !redirectReg.valid // timing is ok?
  val physicalQueuePreCommitPtr = RegEnable(getEntryEndSqIdx(preCommitPtr), preCommitMoveValid)
  io.toPhysicalQueue.preCommitPtr.valid := preCommitValid
  io.toPhysicalQueue.preCommitPtr.bits  := physicalQueuePreCommitPtr

  // redirect logic, event driver
  val toPhysicalQueueRedirectValid = DelayN(redirectReg.valid, 2)
  val redirectRecoverPtr = WireInit(toPhysicalQueueRetirePtr)
  when(ctrlEntries(redirectLastValidPtr.value).allocated) {
    redirectRecoverPtr := getEntryEndSqIdx(redirectLastValidPtr)
  }.otherwise {
    redirectRecoverPtr := toPhysicalQueueRetirePtrNext
  }
  val toPhysicalQueueRedirectPtr = RegEnable(redirectRecoverPtr, RegNext(redirectReg.valid))
  io.toPhysicalQueue.redirectPtr.valid := toPhysicalQueueRedirectValid
  io.toPhysicalQueue.redirectPtr.bits := toPhysicalQueueRedirectPtr

  io.toPhysicalQueue.headRobIdx := RegEnable(dataEntries(deqPtrVec.head.value).robIdx, preCommitMoveValid)

  val sqRecoverStall = state =/= WalkState.idle || RegNext(redirectReg.valid) || DelayN(redirectReg.valid, 2)
  io.sqRecoverStall := sqRecoverStall

  // preCommit entries release
   // to ensure preCommitRelease can cover all preCommit entries,
   //otherwise may have case that preCommit entry had been released, but later preCommit entry is not released, which cause correctness issue.
  require(CommitWidth >= EnsbufferWidth)
  preCommitRelease.zipWithIndex.foreach{ case(sink, i) =>
    val ptr = deqPtrVec(i)
    sink := io.fromPhysicalQueue.deqPtr >= getEntryEndSqIdx(ptr) && io.fromPhysicalQueue.deqCount.valid && ctrlEntries(ptr.value).allocated
  }

  if(debugEn) {
    dontTouch(retireVec)
    dontTouch(preCommitPtrNext)
    dontTouch(preCommitMoveValid)
    dontTouch(redirectLastValidPtr)
    dontTouch(redirectReg)
    dontTouch(toPhysicalQueueRedirectValid)
    dontTouch(toPhysicalQueueRedirectPtr)
    dontTouch(enqPtrVec)
    dontTouch(retireReqNumReg)
    dontTouch(preCommitRelease)
    dontTouch(redirectRecoverPtr)
    dontTouch(toPhysicalQueueRetirePtrNext)
  }

  XSError(state =/= WalkState.idle && io.enq.req.map(_.valid).reduce(_ || _), s"Virtual StoreQueue is walking, but new request enter!\n")
  XSError(isFull(enqPtrVec.head, deqPtrVec.head) && io.enq.req.map(_.valid).reduce(_ || _), s"Virtual StoreQueue is full, but have requestor enter!\n")
  XSError(preCommitRelease.zipWithIndex.map{ case(hit, i) => hit && !retireBaseVec(i) }.reduce(_ || _) &&
  !preCommitValid && !redirectReg.valid && !RegNext(redirectReg.valid), "Virtual StoreQueue release error!\n")
}

import top.Generator
object VirtualStoreQueueMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform" :+ "--target" :+ "verilog")

  val defaultConfig = config.alterPartial({
    // Get XSCoreParams and pass it to the "small module"
    case XSCoreParamsKey => config(XSTileKey).head
  })

  Generator.execute(
    firrtlOpts :+ "--full-stacktrace" :+ "--target-dir" :+ "VirtualStoreQueue" :+ "--throw-on-first-error",
    new VirtualStoreQueue(32, 2, 4, new SqPtr()(defaultConfig))(defaultConfig),
    firtoolOpts :+ "-O=release" :+ "--disable-annotation-unknown" :+ "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  //  emitVerilog(new NewStoreQueue()(defaultConfig), Array("--target-dir", "build/storeQueue", "--full-stacktrace"))

  println("done")
}
