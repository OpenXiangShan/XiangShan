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

class MBufferBundle(implicit p: Parameters) extends VLSUBundle{
  val data             = UInt(VLEN.W)
  val mask             = UInt(VLENB.W)
  val flowNum          = UInt(flowIdxBits.W)
  val exceptionVec     = ExceptionVec()
  val uop              = new DynInst
  val vdOffset         = UInt(vOffsetBits.W)

  def allReady(): Bool = (flowNum === 0.U)
}

abstract class BaseVMergeBuffer(isVStore: Boolean=false)(implicit p: Parameters) extends VLSUModule{
  val io = IO(new VMergeBufferIO(isVStore))

  def EnqConnect(source: MergeBufferReq, sink: MBufferBundle) = {
    sink.data         := source.data
    sink.mask         := source.mask
    sink.flowNum      := source.flowNum
    sink.exceptionVec := 0.U.asTypeOf(ExceptionVec())
    sink.uop          := source.uop
    // sink.vdOffset     := source.vdOffset
  }
  def DeqConnect(source: MBufferBundle, sink: MemExuOutput) = {
    sink.data             := source.data
    sink.mask.get         := source.mask
    sink.uop.exceptionVec := source.exceptionVec
    sink.uop              := source.uop
  }
  def ToLsqConnect(source: MBufferBundle, sink: FeedbackToLsqIO) = {
    sink.robidx                             := source.uop.robIdx
    sink.uopidx                             := source.uop.uopIdx
    sink.feedback(VecFeedbacks.COMMIT)      := true.B // TODO
    sink.feedback(VecFeedbacks.FLUSH)       := false.B
    sink.feedback(VecFeedbacks.LAST)        := true.B
  }
  // freeliset: store valid entries index.
  // +---+---+--------------+-----+-----+
  // | 0 | 1 |      ......  | n-2 | n-1 |
  // +---+---+--------------+-----+-----+
  val freeList: FreeList
  val uopSize: Int
  val enqWidth = io.fromSplit.length
  val deqWidth = io.uopWriteback.length

  val entries      = Reg(Vec(uopSize, new MBufferBundle))
  val needCancel   = WireInit(VecInit(Seq.fill(uopSize)(false.B)))
  val allocated    = RegInit(VecInit(Seq.fill(uopSize)(false.B)))
  val freeMaskVec  = WireInit(VecInit(Seq.fill(uopSize)(false.B)))
  val uopFinish    = RegInit(VecInit(Seq.fill(uopSize)(false.B)))
  // enq, from splitPipeline
  // val allowEnqueue =
  val cancelEnq_test0 = io.fromSplit(0).req.bits.uop.robIdx.needFlush(io.redirect)
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
      EnqConnect(enq.req.bits, entries(enqIndex))// initial entry

      enq.resp.bits.mBIndex := enqIndex
    }
  }

  //redirect
  for (i <- 0 until uopSize){
    needCancel(i) := entries(i).uop.robIdx.needFlush(io.redirect) && allocated(i)
    when (needCancel(i)) {
      allocated(i)   := false.B
      freeMaskVec(i) := true.B
      uopFinish(i)   := false.B
    }
  }
  freeList.io.free := freeMaskVec.asUInt
  //pipelineWriteback
  for((pipewb) <- io.fromPipeline){
    val wbIndex = pipewb.bits.mBIndex
    val flowNumNext = Mux(pipewb.bits.usSecondInv, entries(wbIndex).flowNum - 2.U, entries(wbIndex).flowNum - 1.U)
    when(pipewb.valid && pipewb.bits.hit){
      entries(wbIndex).flowNum := flowNumNext
    }
    XSError(flowNumNext > entries(wbIndex).flowNum, "FlowWriteback overflow!!\n")
  }

  //feedback to rs
  io.feedback := DontCare
  //uopwriteback(deq)
  for (i <- 0 until uopSize){
    when(allocated(i) && entries(i).allReady()){
      uopFinish(i) := true.B
    }
  }
   val selPolicy = SelectOne("circ", uopFinish, deqWidth) // select one entry to deq
   for(((port, lsqport), i) <- (io.uopWriteback zip io.toLsq).zipWithIndex){
    val (selValid, selOHVec) = selPolicy.getNthOH(i + 1)
    when(selValid){
      val entryIdx = OHToUInt(selOHVec)
      val selEntry = entries(entryIdx)
      freeMaskVec(entryIdx) := true.B
      allocated(entryIdx) := false.B
      //writeback connect
      DeqConnect(selEntry, port.bits)
      port.valid := true.B
      //to lsq
      ToLsqConnect(selEntry, lsqport.bits) // when uopwriteback, free MBuffer entry, write to lsq
    }
   }
}

class VLMergeBufferImp(implicit p: Parameters) extends BaseVMergeBuffer(isVStore=false){
  override lazy val uopSize = VlMergeBufferSize
  println(s"VLMergeBuffer Size: ${VlMergeBufferSize}")
  override lazy val freeList = Module(new FreeList(
    size = uopSize,
    allocWidth = LoadPipelineWidth,
    freeWidth = deqWidth,
    enablePreAlloc = false,
    moduleName = "VLoad MergeBuffer freelist"
  ))

  //merge data
  val flowWbElemIdx = Wire(Vec(LoadPipelineWidth, UInt(elemIdxBits.W)))
  val flowWbElemIdxInVd = Wire(Vec(LoadPipelineWidth, UInt(elemIdxBits.W)))

  for((pipewb, i) <- io.fromPipeline.zipWithIndex){
    val wbIndex = pipewb.bits.mBIndex
    val alignedType = pipewb.bits.alignedType.get
    val elemIdxInsideVd = pipewb.bits.elemIdxInsideVd
    flowWbElemIdx(i) := pipewb.bits.elemIdx.get
    flowWbElemIdxInVd(i) := elemIdxInsideVd.get
    // handle the situation where multiple ports are going to write the same uop queue entry
    val mergedByPrevPort = (i != 0).B && Cat((0 until i).map(j =>
      io.fromPipeline(j).bits.mBIndex === pipewb.bits.mBIndex)).orR
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
    val usMergeData = mergeDataByoffset(
      oldData = entries(wbIndex).data,
      newData = io.fromPipeline.map(_.bits.vecdata.get),
      mask    = io.fromPipeline.map(_.bits.mask.get),
      offset  = io.fromPipeline.map(_.bits.reg_offset.get),
      valids  = mergeExpPortVec
    )
    when(pipewb.valid && !mergedByPrevPort && alignedType =/= "b100".U){
      entries(wbIndex).data := Mux(alignedType(2), usMergeData, mergedData) // if aligned(2) == 1, is Unit-Stride inst
    }
  }
}

class VSMergeBufferImp(implicit p: Parameters) extends BaseVMergeBuffer(isVStore=true){
  override lazy val uopSize = VsMergeBufferSize
  println(s"VSMergeBuffer Size: ${VsMergeBufferSize}")
  override lazy val freeList = Module(new FreeList(
    size = uopSize,
    allocWidth = StorePipelineWidth,
    freeWidth = deqWidth,
    enablePreAlloc = false,
    moduleName = "VStore MergeBuffer freelist"
  ))
  override def DeqConnect(source: MBufferBundle, sink: MemExuOutput) = {
    sink.data             := DontCare
    sink.mask.get         := source.mask
    sink.uop.exceptionVec := source.exceptionVec
    sink.uop              := source.uop
  }
}
