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
  // for exception
  val vstart           = UInt(elemIdxBits.W)
  val vl               = UInt(elemIdxBits.W)
  val vaddr            = UInt(VAddrBits.W)
  val fof              = Bool()
  val vlmax            = UInt(elemIdxBits.W)

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
    sink.sourceType   := 0.U.asTypeOf(VSFQFeedbackType())
    sink.flushState   := false.B
    sink.vdIdx        := source.vdIdx
    sink.fof          := source.fof
    sink.vlmax        := source.vlmax
    sink.vl           := source.uop.vpu.vl
    sink.vstart       := 0.U
  }
  def DeqConnect(source: MBufferBundle): MemExuOutput = {
    val sink               = WireInit(0.U.asTypeOf(new MemExuOutput(isVector = true)))
    sink.data             := source.data
    sink.mask.get         := source.mask
    sink.uop              := source.uop
    sink.uop.exceptionVec := source.exceptionVec
    sink.uop.vpu.vmask    := source.mask
    sink.debug            := 0.U.asTypeOf(new DebugBundle)
    sink.vdIdxInField.get := source.vdIdx // Mgu needs to use this.
    sink.vdIdx.get        := source.vdIdx
    sink.uop.vpu.vstart   := source.vstart
    sink.uop.vpu.vl       := source.vl
    sink
  }
  def ToLsqConnect(source: MBufferBundle): FeedbackToLsqIO = {
    val sink                                 = WireInit(0.U.asTypeOf(new FeedbackToLsqIO))
    val hasExp                               = source.exceptionVec.asUInt.orR
    sink.robidx                             := source.uop.robIdx
    sink.uopidx                             := source.uop.uopIdx
    sink.feedback(VecFeedbacks.COMMIT)      := !hasExp
    sink.feedback(VecFeedbacks.FLUSH)       := hasExp
    sink.feedback(VecFeedbacks.LAST)        := true.B
    sink.vstart                             := source.vstart // TODO: if lsq need vl for fof?
    sink.vaddr                              := source.vaddr
    sink.vl                                 := source.vl
    sink.exceptionVec                       := source.exceptionVec
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

  val freeCount    = uopSize.U - freeList.io.validCount

  for ((enq, i) <- io.fromSplit.zipWithIndex){
    freeList.io.doAllocate(i) := false.B

    freeList.io.allocateReq(i) := true.B

    val offset    = PopCount(needEnqueue.take(i))
    val canAccept = freeList.io.canAllocate(offset)
    val enqIndex  = freeList.io.allocateSlot(offset)
    enq.req.ready := freeCount >= (i + 1).U // for better timing

    when(needEnqueue(i) && enq.req.ready){
      freeList.io.doAllocate(i) := true.B
      // enqueue
      allocated(enqIndex)       := true.B
      uopFinish(enqIndex)       := false.B
      needRSReplay(enqIndex)    := false.B

      EnqConnect(enq.req.bits, entries(enqIndex))// initial entry
    }

    enq.resp.bits.mBIndex := enqIndex
    enq.resp.bits.fail    := false.B
    enq.resp.valid        := freeCount >= (i + 1).U // for better timing
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
  // handle the situation where multiple ports are going to write the same uop queue entry
  val mergePortMatrix        = Wire(Vec(pipeWidth, Vec(pipeWidth, Bool())))
  val mergedByPrevPortVec    = Wire(Vec(pipeWidth, Bool()))
  (0 until pipeWidth).map{case i => (0 until pipeWidth).map{case j =>
    mergePortMatrix(i)(j) := (j == i).B ||
      (j > i).B &&
      io.fromPipeline(j).bits.mBIndex === io.fromPipeline(i).bits.mBIndex &&
      io.fromPipeline(j).valid
  }}
  (0 until pipeWidth).map{case i =>
    mergedByPrevPortVec(i) := (i != 0).B && Cat((0 until i).map(j =>
      io.fromPipeline(j).bits.mBIndex === io.fromPipeline(i).bits.mBIndex &&
      io.fromPipeline(j).valid)).orR
  }
  dontTouch(mergePortMatrix)
  dontTouch(mergedByPrevPortVec)

  // for exception, select exception, when multi port writeback exception, we need select oldest one
  def selectOldest[T <: VecPipelineFeedbackIO](valid: Seq[Bool], bits: Seq[T], sel: Seq[UInt]): (Seq[Bool], Seq[T], Seq[UInt]) = {
    assert(valid.length == bits.length)
    assert(valid.length == sel.length)
    if (valid.length == 0 || valid.length == 1) {
      (valid, bits, sel)
    } else if (valid.length == 2) {
      val res = Seq.fill(2)(Wire(ValidIO(chiselTypeOf(bits(0)))))
      for (i <- res.indices) {
        res(i).valid := valid(i)
        res(i).bits := bits(i)
      }
      val oldest = Mux(valid(0) && valid(1),
        Mux(sel(0) < sel(1),
            res(0), res(1)),
        Mux(valid(0) && !valid(1), res(0), res(1)))
      (Seq(oldest.valid), Seq(oldest.bits), Seq(0.U))
    } else {
      val left  = selectOldest(valid.take(valid.length / 2), bits.take(bits.length / 2), sel.take(sel.length / 2))
      val right = selectOldest(valid.takeRight(valid.length - (valid.length / 2)), bits.takeRight(bits.length - (bits.length / 2)), sel.takeRight(sel.length - (sel.length / 2)))
      selectOldest(left._1 ++ right._1, left._2 ++ right._2, left._3 ++ right._3)
    }
  }

  val pipeValid        = io.fromPipeline.map(_.valid)
  val pipeBits         = io.fromPipeline.map(x => x.bits)
  val wbElemIdx        = pipeBits.map(_.elemIdx)
  val wbMbIndex        = pipeBits.map(_.mBIndex)
  val wbElemIdxInField = wbElemIdx.zip(wbMbIndex).map(x => x._1 & (entries(x._2).vlmax - 1.U))

  val portHasExcp       = pipeBits.zip(mergePortMatrix).map{case (port, v) =>
    (0 until pipeWidth).map{case i =>
      (v(i) && io.fromPipeline(i).bits.exceptionVec.asUInt.orR && io.fromPipeline(i).bits.mask.orR) // this port have exception or merged port have exception
    }.reduce(_ || _)
  }

  for((pipewb, i) <- io.fromPipeline.zipWithIndex){
    val entry               = entries(wbMbIndex(i))
    val entryVeew           = entry.uop.vpu.veew
    val entryIsUS           = LSUOpType.isUStride(entry.uop.fuOpType)
    val entryExcp           = entry.exceptionVec.asUInt.orR && entry.mask.orR

    val sel                    = selectOldest(mergePortMatrix(i), pipeBits, wbElemIdxInField)
    val selPort                = sel._2
    val selElemInfield         = selPort(0).elemIdx & (entries(wbMbIndex(i)).vlmax - 1.U)
    val selExceptionVec        = selPort(0).exceptionVec

    val USFirstUopOffset       = (searchVFirstUnMask(selPort(0).mask) << entryVeew).asUInt
    val isUSFirstUop           = !selPort(0).elemIdx.orR
    val vaddr                  = selPort(0).vaddr + Mux(entryIsUS && isUSFirstUop, USFirstUopOffset, 0.U)

    // select oldest port to raise exception
    when((((entries(wbMbIndex(i)).vstart >= selElemInfield) && entryExcp && portHasExcp(i)) || (!entryExcp && portHasExcp(i))) && pipewb.valid && !mergedByPrevPortVec(i)){
      when(!entries(wbMbIndex(i)).fof || selElemInfield === 0.U){
        // For fof loads, if element 0 raises an exception, vl is not modified, and the trap is taken.
        entries(wbMbIndex(i)).vstart       := selElemInfield
        entries(wbMbIndex(i)).exceptionVec := selExceptionVec
        entries(wbMbIndex(i)).vaddr        := vaddr
      }.otherwise{
        entries(wbMbIndex(i)).vl           := selElemInfield
      }
    }
  }

  // for pipeline writeback
  for((pipewb, i) <- io.fromPipeline.zipWithIndex){
    val wbIndex          = pipewb.bits.mBIndex
    val flowNumOffset    = Mux(pipewb.bits.usSecondInv,
                               2.U,
                               PopCount(mergePortMatrix(i)))
    val sourceTypeNext   = entries(wbIndex).sourceType | pipewb.bits.sourceType
    val hasExp           = pipewb.bits.exceptionVec.asUInt.orR

    // if is VLoad, need latch 1 cycle to merge data. only flowNum and wbIndex need to latch
    val latchWbValid     = if(isVStore) pipewb.valid else RegNext(pipewb.valid)
    val latchWbIndex     = if(isVStore) wbIndex      else RegEnable(wbIndex, pipewb.valid)
    val latchFlowNum     = if(isVStore) flowNumOffset else RegEnable(flowNumOffset, pipewb.valid)
    val latchMergeByPre  = if(isVStore) mergedByPrevPortVec(i) else RegEnable(mergedByPrevPortVec(i), pipewb.valid)
    when(latchWbValid && !latchMergeByPre){
      entries(latchWbIndex).flowNum := entries(latchWbIndex).flowNum - latchFlowNum
    }

    when(pipewb.valid){
      entries(wbIndex).sourceType   := sourceTypeNext
      entries(wbIndex).flushState   := pipewb.bits.flushState
    }
    when(pipewb.valid && !pipewb.bits.hit){
      needRSReplay(wbIndex) := true.B
    }
    pipewb.ready := true.B
    XSError((entries(latchWbIndex).flowNum - latchFlowNum > entries(latchWbIndex).flowNum) && latchWbValid && !latchMergeByPre, "FlowWriteback overflow!!\n")
    XSError(!allocated(latchWbIndex) && latchWbValid, "Writeback error flow!!\n")
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
    val selAllocated = allocated(entryIdx)
    val selFire  = selValid && canGo
    when(selFire){
      freeMaskVec(entryIdx) := selAllocated
      allocated(entryIdx)   := false.B
      uopFinish(entryIdx)   := false.B
      needRSReplay(entryIdx):= false.B
    }
    //writeback connect
    port.valid   := selFire && selAllocated && !needRSReplay(entryIdx) && !selEntry.uop.robIdx.needFlush(io.redirect)
    port.bits    := DeqConnect(selEntry)
    //to lsq
    lsqport.bits := ToLsqConnect(selEntry) // when uopwriteback, free MBuffer entry, write to lsq
    lsqport.valid:= selFire && selAllocated && !needRSReplay(entryIdx)
    //to RS
    io.feedback(i).valid                 := selFire && selAllocated
    io.feedback(i).bits.hit              := !needRSReplay(entryIdx)
    io.feedback(i).bits.robIdx           := selEntry.uop.robIdx
    io.feedback(i).bits.sourceType       := selEntry.sourceType
    io.feedback(i).bits.flushState       := selEntry.flushState
    io.feedback(i).bits.dataInvalidSqIdx := DontCare
    io.feedback(i).bits.uopIdx.get       := selEntry.uop.uopIdx
   }

  QueuePerf(uopSize, freeList.io.validCount, freeList.io.validCount === 0.U)
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
  val flowWbElemIdx     = Wire(Vec(pipeWidth, UInt(elemIdxBits.W)))
  val flowWbElemIdxInVd = Wire(Vec(pipeWidth, UInt(elemIdxBits.W)))
  val pipewbValidReg    = Wire(Vec(pipeWidth, Bool()))
  val wbIndexReg        = Wire(Vec(pipeWidth, UInt(vlmBindexBits.W)))
  val mergeDataReg      = Wire(Vec(pipeWidth, UInt(VLEN.W)))

  for((pipewb, i) <- io.fromPipeline.zipWithIndex){
    /** step0 **/
    val wbIndex = pipewb.bits.mBIndex
    val alignedType = pipewb.bits.alignedType
    val elemIdxInsideVd = pipewb.bits.elemIdxInsideVd
    flowWbElemIdx(i) := pipewb.bits.elemIdx
    flowWbElemIdxInVd(i) := elemIdxInsideVd.get

    val oldData = PriorityMux(Seq(
      (pipewbValidReg(0) && (wbIndexReg(0) === wbIndex)) -> mergeDataReg(0),
      (pipewbValidReg(1) && (wbIndexReg(1) === wbIndex)) -> mergeDataReg(1),
      (pipewbValidReg(2) && (wbIndexReg(2) === wbIndex)) -> mergeDataReg(2),
      true.B                                             -> entries(wbIndex).data // default use entries_data
    ))
    val mergedData = mergeDataWithElemIdx(
      oldData = oldData,
      newData = io.fromPipeline.map(_.bits.vecdata.get),
      alignedType = alignedType(1,0),
      elemIdx = flowWbElemIdxInVd,
      valids = mergePortMatrix(i)
    )
    /* this only for unit-stride load data merge
     * cycle0: broden 128-bits to 256-bits (max 6 to 1)
     * cycle1: select 128-bits data from 256-bits (16 to 1)
     */
    val (brodenMergeData, brodenMergeMask)     = mergeDataByIndex(
      data    = io.fromPipeline.map(_.bits.vecdata.get).drop(i),
      mask    = io.fromPipeline.map(_.bits.mask).drop(i),
      index   = io.fromPipeline(i).bits.elemIdxInsideVd.get,
      valids  = mergePortMatrix(i).drop(i)
    )
    /** step1 **/
    pipewbValidReg(i)      := RegNext(pipewb.valid)
    wbIndexReg(i)          := RegEnable(wbIndex, pipewb.valid)
    mergeDataReg(i)        := RegEnable(mergedData, pipewb.valid) // for not Unit-stride
    val brodenMergeDataReg  = RegEnable(brodenMergeData, pipewb.valid) // only for Unit-stride
    val brodenMergeMaskReg  = RegEnable(brodenMergeMask, pipewb.valid)
    val mergedByPrevPortReg = RegEnable(mergedByPrevPortVec(i), pipewb.valid)
    val regOffsetReg        = RegEnable(pipewb.bits.reg_offset.get, pipewb.valid) // only for Unit-stride
    val isusMerge           = RegEnable(alignedType(2), pipewb.valid)

    val usSelData           = Mux1H(UIntToOH(regOffsetReg), (0 until VLENB).map{case i => getNoAlignedSlice(brodenMergeDataReg, i, 128)})
    val usSelMask           = Mux1H(UIntToOH(regOffsetReg), (0 until VLENB).map{case i => brodenMergeMaskReg(16 + i - 1, i)})
    val usMergeData         = mergeDataByByte(entries(wbIndexReg(i)).data, usSelData, usSelMask)
    when(pipewbValidReg(i) && !mergedByPrevPortReg){
      entries(wbIndexReg(i)).data := Mux(isusMerge, usMergeData, mergeDataReg(i)) // if aligned(2) == 1, is Unit-Stride inst
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
    sink.uop              := source.uop
    sink.uop.exceptionVec := source.exceptionVec
    sink.debug            := 0.U.asTypeOf(new DebugBundle)
    sink.vdIdxInField.get := DontCare
    sink.vdIdx.get        := DontCare
    sink.uop.vpu.vstart   := source.vstart
    sink
  }
}
