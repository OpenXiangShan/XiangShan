package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._

/** Own forward replay pointers and the two-cycle recovery setup protocol. */
final class RobRecovery(
  redirect: ValidIO[Redirect],
  useSnapshot: Bool,
  snapshotPointer: RobPtr,
  nextDequeuePointer: RobPtr,
  rabWalkEnd: Bool,
  vtbWalkEnd: Bool
)(implicit val p: Parameters) extends HasXSParameter with HasCircularQueuePtrHelper {
  val idle :: walk :: Nil = Enum(2)
  val state = RegInit(idle)
  val nextState = Wire(chiselTypeOf(state))
  val pointers = Reg(Vec(CommitWidth,new RobPtr))
  val exactPointer = Reg(new RobPtr)
  val lastPointer = Reg(new RobPtr)
  val prefixSkip = RegInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val scannedPointers = RegNext(pointers)
  val scanDone = exactPointer.isAfterSlot(lastPointer)
  private val dequeueBank = nextDequeuePointer.lineHeadPtr
  val snapshotBank = snapshotPointer.lineHeadPtr
  val dequeuePointers = VecInit((0 until CommitWidth).map(i => dequeueBank.addEntries(i.U).asFormer))
  val snapshotPointers = VecInit((0 until CommitWidth).map(i => snapshotBank.addEntries(i.U).asFormer))
  val nextPointers = Mux(redirect.valid,
    Mux(useSnapshot,snapshotPointers,dequeuePointers),
    Mux(state===walk && !scanDone,
      VecInit(pointers.map(_.addEntries(CommitWidth.U).asFormer)),pointers))
  val nextExactPointer = Mux(redirect.valid,
    Mux(useSnapshot,snapshotPointer,nextDequeuePointer),
    Mux(state===walk && !scanDone,nextPointers.head,exactPointer))
  pointers := nextPointers
  exactPointer := nextExactPointer
  private val startLowBits = Reg(UInt(log2Up(CommitWidth).W))
  when(redirect.valid) {
    startLowBits := Mux(useSnapshot,
      snapshotPointer.value(log2Up(CommitWidth)-1,0),nextDequeuePointer.value(log2Up(CommitWidth)-1,0))
  }
  when(redirect.valid) {
    prefixSkip := Fill(CommitWidth,true.B).asTypeOf(prefixSkip)
  }.elsewhen(RegNext(redirect.valid)) {
    prefixSkip := (0 until CommitWidth).map(i => i.U < startLowBits)
  }.otherwise {
    prefixSkip := 0.U.asTypeOf(prefixSkip)
  }
  when(redirect.valid) {
    lastPointer := Mux(redirect.bits.robIdx.isFormer && redirect.bits.flushItself(),
      redirect.bits.robIdx.subEntries(1.U).asFormer,redirect.bits.robIdx)
  }
  val valid = Wire(Vec(CommitWidth,Bool()))
  when(redirect.valid) {
    valid := 0.U.asTypeOf(valid)
  }.elsewhen(RegNext(redirect.valid)) {
    valid := 0.U.asTypeOf(valid)
  }.elsewhen(state===walk) {
    valid := VecInit(scannedPointers.map(_.isNotAfterSlot(lastPointer)).zip(prefixSkip).map {
      case (withinRange,skip) => withinRange && !skip
    })
  }.otherwise {
    valid := 0.U.asTypeOf(valid)
  }
  nextState := Mux(redirect.valid || RegNext(redirect.valid),walk,
    Mux(state===walk && scanDone && rabWalkEnd && vtbWalkEnd,idle,state))
  state := nextState
}
