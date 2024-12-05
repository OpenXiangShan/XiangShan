package xiangshan.backend.trace

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper}
import xiangshan.{HasXSParameter, XSCoreParamsKey}

class TraceBuffer(implicit val p: Parameters) extends Module
  with HasXSParameter
  with HasCircularQueuePtrHelper {

  val io = IO(new Bundle {
    val in = new Bundle{
      val fromEncoder = Input(new FromEncoder)
      val fromRob     = Flipped(new TraceBundle(hasIaddr = false, CommitWidth, IretireWidthCompressed))
    }
    val out = new Bundle { // output groups to pcMem
      val blockCommit = Output(Bool())
      val groups = new TraceBundle(hasIaddr = false, TraceGroupNum, IretireWidthCompressed)
    }
  })

  // buffer: compress info from robCommit
  val traceEntries = Reg(Vec(CommitWidth, ValidIO(new TraceBlock(false, IretireWidthCompressed))))
  val blockCommit = RegInit(false.B) // to rob

  /**
   * compress, update blocks
   */
  val inValidVec = VecInit(io.in.fromRob.blocks.map(_.valid))
  val inTypeIsNotNoneVec = VecInit(io.in.fromRob.blocks.map(block => Itype.isNotNone(block.bits.tracePipe.itype)))
  val needPcVec = Wire(Vec(CommitWidth, Bool()))
  for(i <- 0 until CommitWidth) {
    val rightHasValid = if(i == CommitWidth - 1) false.B  else (inValidVec.asUInt(CommitWidth-1, i+1).orR)
    needPcVec(i) := inValidVec(i) & (inTypeIsNotNoneVec(i) || !rightHasValid) & !blockCommit
  }

  val blocksUpdate = WireInit(io.in.fromRob.blocks)
  for(i <- 1 until CommitWidth){
    when(!needPcVec(i-1)){
      blocksUpdate(i).bits.tracePipe.iretire := blocksUpdate(i - 1).bits.tracePipe.iretire + io.in.fromRob.blocks(i).bits.tracePipe.iretire
      blocksUpdate(i).bits.ftqOffset.get := blocksUpdate(i - 1).bits.ftqOffset.get
      blocksUpdate(i).bits.ftqIdx.get := blocksUpdate(i - 1).bits.ftqIdx.get
     }
  }

  /**
   * enq to traceEntries
   */
  val countVec = VecInit((0 until CommitWidth).map(i => PopCount(needPcVec.asUInt(i, 0))))
  val numNeedPc = countVec(CommitWidth-1)

  val enqPtr = RegInit(TracePtr(false.B, 0.U))
  val deqPtr = RegInit(TracePtr(false.B, 0.U))
  val deqPtrPre = RegNext(deqPtr)
  val enqPtrNext = WireInit(enqPtr)
  val deqPtrNext = WireInit(deqPtr)
  enqPtr := enqPtrNext
  deqPtr := deqPtrNext
  val canNotTraceAll = distanceBetween(enqPtrNext, deqPtrNext) > 0.U
  blockCommit := io.in.fromEncoder.enable && (canNotTraceAll || io.in.fromEncoder.stall)

  enqPtrNext := enqPtr + numNeedPc
  deqPtrNext := Mux(deqPtr + TraceGroupNum.U > enqPtrNext, enqPtrNext, deqPtr + TraceGroupNum.U)

  val traceIdxVec = VecInit(countVec.map(count => (enqPtr + count - 1.U).value))
  for(i <- 0 until CommitWidth){
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

class TracePtr(entries: Int) extends CircularQueuePtr[TracePtr](
  entries
) with HasCircularQueuePtrHelper {

  def this()(implicit p: Parameters) = this(p(XSCoreParamsKey).CommitWidth)

}

object TracePtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): TracePtr = {
    val ptr = Wire(new TracePtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}
