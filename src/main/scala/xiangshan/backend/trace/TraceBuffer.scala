package xiangshan.backend.trace

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper}
import xiangshan.HasXSParameter

class TraceBuffer(implicit val p: Parameters) extends Module
  with HasXSParameter
  with HasCircularQueuePtrHelper {

  val io = IO(new Bundle {
    val in = new Bundle{
      val fromEncoder = Input(new FromEncoder)
      val fromRob     = Flipped(new TraceBundle(hasIaddr = false, TraceCommitWidth, IretireWidthCompressed))
    }
    val out = new Bundle { // output groups to pcMem
      val blockCommit = Output(Bool())
      val groups = new TraceBundle(hasIaddr = false, TraceGroupNum, IretireWidthCompressed)
    }
  })

  // buffer: compress info from robCommit
  val traceEntries = Reg(Vec(TraceCommitWidth, ValidIO(new TraceBlock(false, IretireWidthCompressed))))
  val blockCommit = RegInit(false.B) // to rob

  /**
   * compress, update blocks
   */
  val inValidVec = VecInit(io.in.fromRob.blocks.map(_.valid))
  val inTypeIsNotNoneVec = VecInit(io.in.fromRob.blocks.map(block => Itype.isNotNone(block.bits.tracePipe.itype)))
  val lastValid = Reverse(PriorityEncoderOH(Reverse(inValidVec.asUInt)))
  val needPcVec = VecInit(inValidVec.indices.map(i =>
    inValidVec(i) && (inTypeIsNotNoneVec(i) || lastValid(i)) && !blockCommit
  ))

  val blocksUpdate = WireInit(io.in.fromRob.blocks)
  for (i <- 1 until TraceCommitWidth) {
    val prev = blocksUpdate(i - 1)
    val curr = blocksUpdate(i)
    when(prev.valid && !needPcVec(i - 1)) {
      // An unsplit ROB entry leaves its second lane invalid. Carry a pending
      // block across the gap without adding a length or taking its address.
      curr.valid := true.B
      curr.bits.tracePipe.iretire := prev.bits.tracePipe.iretire +
        Mux(inValidVec(i), io.in.fromRob.blocks(i).bits.tracePipe.iretire, 0.U)
      curr.bits.ftqOffset.get := prev.bits.ftqOffset.get
      curr.bits.ftqIdx.get := prev.bits.ftqIdx.get
    }
  }

  /**
   * enq to traceEntries
   */
  val countVec = VecInit(needPcVec.indices.map(i => PopCount(needPcVec.asUInt(i, 0))))
  val numNeedPc = countVec.last

  val enqPtr = RegInit(0.U.asTypeOf(new TracePtr(TraceCommitWidth)))
  val deqPtr = RegInit(0.U.asTypeOf(new TracePtr(TraceCommitWidth)))
  val deqPtrPre = RegNext(deqPtr, 0.U.asTypeOf(deqPtr))
  val enqPtrNext = WireInit(enqPtr)
  val deqPtrNext = WireInit(deqPtr)
  enqPtr := enqPtrNext
  deqPtr := deqPtrNext
  val canNotTraceAll = distanceBetween(enqPtrNext, deqPtrNext) > 0.U
  blockCommit := io.in.fromEncoder.enable && (canNotTraceAll || io.in.fromEncoder.stall)

  enqPtrNext := enqPtr + numNeedPc
  deqPtrNext := Mux(deqPtr + TraceGroupNum.U > enqPtrNext, enqPtrNext, deqPtr + TraceGroupNum.U)

  val traceIdxVec = VecInit(countVec.map(count => (enqPtr + count - 1.U).value))
  for(i <- needPcVec.indices){
    when(needPcVec(i)){
      traceEntries(traceIdxVec(i)) := blocksUpdate(i)
    }
  }

  /**
   * deq from traceEntries
   */
  val blockOut = WireInit(0.U.asTypeOf(io.out.groups))
  for(i <- 0 until TraceGroupNum) {
    when(deqPtrPre + i.U < enqPtr) {
      blockOut.blocks(i) := traceEntries((deqPtrPre + i.U).value)
    } .otherwise {
      blockOut.blocks(i).valid := false.B
    }
  }

  io.out.blockCommit := blockCommit
  io.out.groups := blockOut

  if(backendParams.debugEn){
    dontTouch(countVec)
    dontTouch(numNeedPc)
    dontTouch(traceIdxVec)
  }
}

class TracePtr(entries: Int) extends CircularQueuePtr[TracePtr](entries)
