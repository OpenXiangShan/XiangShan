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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.Bundles._
import xiangshan.mem._
import xiangshan.backend.fu.FuType
import freechips.rocketchip.diplomacy.BufferParams

class MBufferBundle(implicit p: Parameters) extends VLSUBundle{
  val data             = UInt(VLEN.W)
  val mask             = UInt(VLENB.W)
  val flowNum          = UInt(flowIdxBits.W)
  val exceptionVec     = ExceptionVec()
  val uop              = new DynInst
  // val vdOffset         = UInt(vOffsetBits.W)
  val sourceType       = VSFQFeedbackType()
  val flushState       = Bool()
  val vdIdx            = UInt(3.W)

  def allReady(): Bool = (flowNum === 0.U)
}

abstract class BaseVMergeBuffer(isVStore: Boolean=false)(implicit p: Parameters) extends VLSUModule{
  val io = IO(new VMergeBufferIO(isVStore))

  def EnqConnect(source: MergeBufferReq): MBufferBundle = {
    val sink           = WireInit(0.U.asTypeOf(new MBufferBundle))
    sink.data         := source.data
    sink.mask         := source.mask
    sink.flowNum      := source.flowNum
    sink.exceptionVec := 0.U.asTypeOf(ExceptionVec())
    sink.uop          := source.uop
    sink.sourceType   := 0.U.asTypeOf(VSFQFeedbackType())
    sink.flushState   := false.B
    sink.vdIdx        := source.vdIdx
    sink
    // sink.vdOffset     := source.vdOffset
  }
  def DeqConnect(source: MBufferBundle): MemExuOutput = {
    val sink               = WireInit(0.U.asTypeOf(new MemExuOutput(isVector = true)))
    sink.data             := source.data
    sink.mask.get         := source.mask
    sink.uop.exceptionVec := source.exceptionVec
    sink.uop              := source.uop
    sink.uop.vpu.vmask    := source.mask
    sink.debug            := 0.U.asTypeOf(new DebugBundle)
    sink.vdIdxInField.get := 0.U
    sink.vdIdx.get        := source.vdIdx
    sink
  }
  def ToLsqConnect(source: MBufferBundle): FeedbackToLsqIO = {
    val sink                                 = WireInit(0.U.asTypeOf(new FeedbackToLsqIO))
    sink.robidx                             := source.uop.robIdx
    sink.uopidx                             := source.uop.uopIdx
    sink.feedback(VecFeedbacks.COMMIT)      := true.B // TODO:
    sink.feedback(VecFeedbacks.FLUSH)       := false.B
    sink.feedback(VecFeedbacks.LAST)        := true.B
    sink.vaddr                              := 0.U // TODO: used when exception
    sink
  }
  // freeliset: store valid entries index.
  // +---+---+--------------+-----+-----+
  // | 0 | 1 |      ......  | n-2 | n-1 |
  // +---+---+--------------+-----+-----+
  val freeList: FreeList
  val uopSize: Int
  val enqWidth = io.fromSplit.length
  val deqWidth = io.uopWriteback.length
  val pipeWidth = io.fromPipeline.length

  val entries      = Reg(Vec(uopSize, new MBufferBundle))
  val needCancel   = WireInit(VecInit(Seq.fill(uopSize)(false.B)))
  val allocated    = RegInit(VecInit(Seq.fill(uopSize)(false.B)))
  val freeMaskVec  = WireInit(VecInit(Seq.fill(uopSize)(false.B)))
  val uopFinish    = RegInit(VecInit(Seq.fill(uopSize)(false.B)))
  val needRSReplay = RegInit(VecInit(Seq.fill(uopSize)(false.B)))
  // enq, from splitPipeline
  // val allowEnqueue =
  val cancelEnq    = io.fromSplit.map(_.req.bits.uop.robIdx.needFlush(io.redirect))
  val canEnqueue   = io.fromSplit.map(_.req.valid)
  val needEnqueue  = (0 until enqWidth).map{i =>
    canEnqueue(i) && !cancelEnq(i)
  }

  for ((enq, i) <- io.fromSplit.zipWithIndex){
    freeList.io.doAllocate(i) := false.B

    freeList.io.allocateReq(i) := true.B

    val offset    = PopCount(needEnqueue.take(i))
    val canAccept = freeList.io.canAllocate(offset)
    val enqIndex  = freeList.io.allocateSlot(offset)
    enq.req.ready := canAccept

    when(needEnqueue(i) && enq.req.ready){
      freeList.io.doAllocate(i) := true.B
      // enqueue
      allocated(enqIndex)       := true.B
      uopFinish(enqIndex)       := false.B
      needRSReplay(enqIndex)    := false.B

      entries(enqIndex) := EnqConnect(enq.req.bits)// initial entry
    }

    enq.resp.bits.mBIndex := enqIndex
    enq.resp.bits.fail    := false.B
    enq.resp.valid        := canAccept //resp in 1 cycle
  }

  //redirect
  for (i <- 0 until uopSize){
    needCancel(i) := entries(i).uop.robIdx.needFlush(io.redirect) && allocated(i)
    when (needCancel(i)) {
      allocated(i)   := false.B
      freeMaskVec(i) := true.B
      uopFinish(i)   := false.B
      needRSReplay(i):= false.B
    }
  }
  freeList.io.free := freeMaskVec.asUInt
  //pipelineWriteback
  for((pipewb) <- io.fromPipeline){
    val wbIndex          = pipewb.bits.mBIndex
    val flowNumNext      = Mux(pipewb.bits.usSecondInv,
                               entries(wbIndex).flowNum - 2.U,
                               entries(wbIndex).flowNum - 1.U)
    val sourceTypeNext   = entries(wbIndex).sourceType | pipewb.bits.sourceType
    val hasExp           = pipewb.bits.exceptionVec.asUInt.orR
    val exceptionVecNext = Mux(hasExp, pipewb.bits.exceptionVec, entries(wbIndex).exceptionVec)

    // if is VLoad, need latch 1 cycle to merge data. only flowNum and wbIndex need to latch
    val latchWbValid     = if(isVStore) pipewb.valid else RegNext(pipewb.valid)
    val latchWbIndex     = if(isVStore) wbIndex      else RegEnable(wbIndex, pipewb.valid)
    val latchFlowNum     = if(isVStore) flowNumNext  else RegEnable(flowNumNext, pipewb.valid)
    when(latchWbValid){
      entries(latchWbIndex).flowNum := latchFlowNum
    }

    when(pipewb.valid){
      entries(wbIndex).sourceType   := sourceTypeNext
      entries(wbIndex).exceptionVec := exceptionVecNext
      entries(wbIndex).flushState   := pipewb.bits.flushState
    }
    when(pipewb.valid && !pipewb.bits.hit){
      needRSReplay(wbIndex) := true.B
    }
    pipewb.ready := true.B
    XSError((flowNumNext > entries(wbIndex).flowNum) && pipewb.valid, "FlowWriteback overflow!!\n")
    XSError(!allocated(wbIndex) && pipewb.valid, "Writeback error flow!!\n")
  }
  // for inorder mem asscess
  io.toSplit := DontCare

  //uopwriteback(deq)
  for (i <- 0 until uopSize){
    when(allocated(i) && entries(i).allReady()){
      uopFinish(i) := true.B
    }
  }
   val selPolicy = SelectOne("circ", uopFinish, deqWidth) // select one entry to deq
   for(((port, lsqport), i) <- (io.uopWriteback zip io.toLsq).zipWithIndex){
    val canGo    = port.ready
    val (selValid, selOHVec) = selPolicy.getNthOH(i + 1)
    val entryIdx = OHToUInt(selOHVec)
    val selEntry = entries(entryIdx)
    val selFire  = selValid && canGo
    when(selFire){
      freeMaskVec(entryIdx) := true.B
      allocated(entryIdx)   := false.B
      uopFinish(entryIdx)   := false.B
      needRSReplay(entryIdx):= false.B
    }
    //writeback connect
    port.valid   := selFire && allocated(entryIdx) && !needRSReplay(entryIdx)
    port.bits    := DeqConnect(selEntry)
    //to lsq
    lsqport.bits := ToLsqConnect(selEntry) // when uopwriteback, free MBuffer entry, write to lsq
    lsqport.valid:= selFire && allocated(entryIdx) && !needRSReplay(entryIdx)
    //to RS
    io.feedback(i).valid                 := selFire && allocated(entryIdx)
    io.feedback(i).bits.hit              := !needRSReplay(entryIdx)
    io.feedback(i).bits.robIdx           := selEntry.uop.robIdx
    io.feedback(i).bits.sourceType       := selEntry.sourceType
    io.feedback(i).bits.flushState       := selEntry.flushState
    io.feedback(i).bits.dataInvalidSqIdx := DontCare
    io.feedback(i).bits.uopIdx.get       := selEntry.uop.uopIdx
   }
}

class VLMergeBufferImp(implicit p: Parameters) extends BaseVMergeBuffer(isVStore=false){
  override lazy val uopSize = VlMergeBufferSize
  println(s"VLMergeBuffer Size: ${VlMergeBufferSize}")
  override lazy val freeList = Module(new FreeList(
    size = uopSize,
    allocWidth = VecLoadPipelineWidth,
    freeWidth = deqWidth,
    enablePreAlloc = false,
    moduleName = "VLoad MergeBuffer freelist"
  ))

  //merge data
  val flowWbElemIdx = Wire(Vec(LoadPipelineWidth, UInt(elemIdxBits.W)))
  val flowWbElemIdxInVd = Wire(Vec(LoadPipelineWidth, UInt(elemIdxBits.W)))

  for((pipewb, i) <- io.fromPipeline.zipWithIndex){
    /** step0 **/
    val wbIndex = pipewb.bits.mBIndex
    val alignedType = pipewb.bits.alignedType.get
    val elemIdxInsideVd = pipewb.bits.elemIdxInsideVd
    flowWbElemIdx(i) := pipewb.bits.elemIdx.get
    flowWbElemIdxInVd(i) := elemIdxInsideVd.get
    // handle the situation where multiple ports are going to write the same uop queue entry
    val mergedByPrevPort = (i != 0).B && Cat((0 until i).map(j =>
      io.fromPipeline(j).bits.mBIndex === pipewb.bits.mBIndex &&
      io.fromPipeline(j).valid)).orR
    val mergePortVec = (0 until LoadPipelineWidth).map(j => (j == i).B ||
      (j > i).B &&
      io.fromPipeline(j).bits.mBIndex === pipewb.bits.mBIndex &&
      io.fromPipeline(j).valid)
    val mergeExpPortVec = (0 until LoadPipelineWidth).map(j => mergePortVec(j))
    val mergedData = mergeDataWithElemIdx(
      oldData = entries(wbIndex).data,
      newData = io.fromPipeline.map(_.bits.vecdata.get),
      alignedType = alignedType(1,0),
      elemIdx = flowWbElemIdxInVd,
      valids = mergeExpPortVec
    )
    /* this only for unit-stride load data merge
     * cycle0: broden 128-bits to 256-bits (max 6 to 1)
     * cycle1: select 128-bits data from 256-bits (16 to 1)
     */
    val (brodenMergeData, brodenMergeMask)     = mergeDataByIndex(
      data    = io.fromPipeline.map(_.bits.vecdata.get).drop(i),
      mask    = io.fromPipeline.map(_.bits.mask.get).drop(i),
      index   = io.fromPipeline(i).bits.elemIdx.get,
      valids  = mergeExpPortVec.drop(i)
    )
    /** step1 **/
    val pipewbValidReg      = RegNext(pipewb.valid)
    val wbIndexReg          = RegEnable(wbIndex, pipewb.valid)
    val mergeDataReg        = RegEnable(mergedData, pipewb.valid) // for not Unit-stride
    val brodenMergeDataReg  = RegEnable(brodenMergeData, pipewb.valid) // only for Unit-stride
    val brodenMergeMaskReg  = RegEnable(brodenMergeMask, pipewb.valid)
    val mergedByPrevPortReg = RegEnable(mergedByPrevPort, pipewb.valid)
    val regOffsetReg        = RegEnable(pipewb.bits.reg_offset.get, pipewb.valid) // only for Unit-stride
    val isusMerge           = RegEnable(alignedType(2), pipewb.valid)

    val usSelData           = Mux1H(UIntToOH(regOffsetReg), (0 until VLENB).map{case i => getNoAlignedSlice(brodenMergeDataReg, i, 128)})
    val usSelMask           = Mux1H(UIntToOH(regOffsetReg), (0 until VLENB).map{case i => brodenMergeMaskReg(16 + i - 1, i)})
    val usMergeData         = mergeDataByByte(entries(wbIndexReg).data, usSelData, usSelMask)
    when(pipewbValidReg && !mergedByPrevPortReg){
      entries(wbIndexReg).data := Mux(isusMerge, usMergeData, mergeDataReg) // if aligned(2) == 1, is Unit-Stride inst
    }
  }
}

class VSMergeBufferImp(implicit p: Parameters) extends BaseVMergeBuffer(isVStore=true){
  override lazy val uopSize = VsMergeBufferSize
  println(s"VSMergeBuffer Size: ${VsMergeBufferSize}")
  override lazy val freeList = Module(new FreeList(
    size = uopSize,
    allocWidth = VecStorePipelineWidth,
    freeWidth = deqWidth,
    enablePreAlloc = false,
    moduleName = "VStore MergeBuffer freelist"
  ))
  override def DeqConnect(source: MBufferBundle): MemExuOutput = {
    val sink               = Wire(new MemExuOutput(isVector = true))
    sink.data             := DontCare
    sink.mask.get         := DontCare
    sink.uop.exceptionVec := source.exceptionVec
    sink.uop              := source.uop
    sink.debug            := 0.U.asTypeOf(new DebugBundle)
    sink.vdIdxInField.get := DontCare
    sink.vdIdx.get        := DontCare
    sink
  }
}
