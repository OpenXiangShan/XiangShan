package xiangshan.backend.trace

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper}
import xiangshan.{HasXSParameter, XSCoreParamsKey}

class TraceBuffer(implicit val p: Parameters) extends Module
  with HasXSParameter
  with HasCircularQueuePtrHelper {

  private val inputWidth = 2 * CommitWidth
  val io = IO(new Bundle {
    val in = new Bundle{
      val fromEncoder = Input(new FromEncoder)
      val fromRob     = Flipped(new TraceBundle(hasIaddr = false, inputWidth, IretireWidthCompressed))
    }
    val out = new Bundle { // output groups to pcMem
      val blockCommit = Output(Bool())
      val groups = new TraceBundle(hasIaddr = false, TraceGroupNum, IretireWidthCompressed)
    }
  })

  // buffer: compress info from robCommit
  val traceEntries = Reg(Vec(inputWidth, ValidIO(new TraceBlock(false, IretireWidthCompressed))))
  val blockCommit = Wire(Bool())

  /**
   * compress, update blocks
   */
  val inValidVec = VecInit(io.in.fromRob.blocks.map(_.valid))
  val inTypeIsNotNoneVec = VecInit(io.in.fromRob.blocks.map(block => Itype.isNotNone(block.bits.tracePipe.itype)))
  val needPcVec = Wire(Vec(inputWidth, Bool()))
  for(i <- 0 until inputWidth) {
    val rightHasValid = if(i == inputWidth - 1) false.B else inValidVec.asUInt(inputWidth - 1, i + 1).orR
    needPcVec(i) := inValidVec(i) && (inTypeIsNotNoneVec(i) || !rightHasValid)
  }

  val blocksUpdate = WireInit(io.in.fromRob.blocks)
  val segmentOpen = Wire(Vec(inputWidth, Bool()))
  segmentOpen(0) := inValidVec(0) && !needPcVec(0)
  for(i <- 1 until inputWidth) {
    val previousOpen = segmentOpen(i - 1)
    when(!inValidVec(i)) {
      blocksUpdate(i).bits := blocksUpdate(i - 1).bits
    }.elsewhen(previousOpen) {
      blocksUpdate(i).bits.tracePipe.iretire := blocksUpdate(i - 1).bits.tracePipe.iretire +
        io.in.fromRob.blocks(i).bits.tracePipe.iretire
      blocksUpdate(i).bits.ftqOffset.get := blocksUpdate(i - 1).bits.ftqOffset.get
      blocksUpdate(i).bits.ftqIdx.get := blocksUpdate(i - 1).bits.ftqIdx.get
    }
    segmentOpen(i) := (previousOpen || inValidVec(i)) && !needPcVec(i)
  }

  /**
   * enq to traceEntries
   */
  val countVec = VecInit((0 until inputWidth).map(i => PopCount(needPcVec.asUInt(i, 0))))
  val numNeedPc = countVec(inputWidth - 1)

  val enqPtr = RegInit(TracePtr(false.B, 0.U))
  val deqPtr = RegInit(TracePtr(false.B, 0.U))
  val deqPtrPre = RegInit(TracePtr(false.B, 0.U))
  when(!io.in.fromEncoder.stall) {
    deqPtrPre := deqPtr
  }
  val enqPtrNext = WireInit(enqPtr)
  val deqPtrNext = WireInit(deqPtr)
  enqPtr := enqPtrNext
  deqPtr := deqPtrNext
  blockCommit := io.in.fromEncoder.enable &&
    (enqPtr =/= deqPtr || inValidVec.asUInt.orR || io.in.fromEncoder.stall)

  enqPtrNext := enqPtr + numNeedPc
  deqPtrNext := Mux(io.in.fromEncoder.stall, deqPtr,
    Mux(deqPtr + TraceGroupNum.U > enqPtrNext, enqPtrNext, deqPtr + TraceGroupNum.U))

  val traceIdxVec = VecInit(countVec.map(count => (enqPtr + count - 1.U).value))
  for(i <- 0 until inputWidth){
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

  def this()(implicit p: Parameters) = this(2 * p(XSCoreParamsKey).CommitWidth)

}

object TracePtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): TracePtr = {
    val ptr = Wire(new TracePtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}
