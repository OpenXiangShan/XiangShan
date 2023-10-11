package xiangshan.backend.rename

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper}
import utils.XSError
import xiangshan.{XSCoreParamsKey, XSModule}


class SnapshotPtr(implicit p: Parameters) extends CircularQueuePtr[SnapshotPtr](
  p => p(XSCoreParamsKey).RenameSnapshotNum
)

object SnapshotGenerator extends HasCircularQueuePtrHelper {
  def apply[T <: Data](enqData: T, enq: Bool, deq: Bool, flush: Bool)(implicit p: Parameters): Vec[T] = {
    val snapshotGen = Module(new SnapshotGenerator(enqData))
    snapshotGen.io.enq := enq
    snapshotGen.io.enqData.head := enqData
    snapshotGen.io.deq := deq
    snapshotGen.io.flush := flush
    snapshotGen.io.snapshots
  }
}

class SnapshotGenerator[T <: Data](dataType: T)(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper {

  class SnapshotGeneratorIO extends Bundle {
    val enq = Input(Bool())
    val enqData = Input(Vec(1, chiselTypeOf(dataType))) // make chisel happy
    val deq = Input(Bool())
    val flush = Input(Bool())
    val snapshots = Output(Vec(RenameSnapshotNum, chiselTypeOf(dataType)))
    val enqPtr = Output(new SnapshotPtr)
    val deqPtr = Output(new SnapshotPtr)
    val valids = Output(Vec(RenameSnapshotNum, Bool()))
  }

  val io = IO(new SnapshotGeneratorIO)

  val snapshots = Reg(Vec(RenameSnapshotNum, chiselTypeOf(dataType)))
  val snptEnqPtr = RegInit(0.U.asTypeOf(new SnapshotPtr))
  val snptDeqPtr = RegInit(0.U.asTypeOf(new SnapshotPtr))
  val snptValids = RegInit(VecInit.fill(RenameSnapshotNum)(false.B))

  io.snapshots := snapshots
  io.enqPtr := snptEnqPtr
  io.deqPtr := snptDeqPtr
  io.valids := snptValids

  when(!isFull(snptEnqPtr, snptDeqPtr) && io.enq) {
    snapshots(snptEnqPtr.value) := io.enqData.head
    snptValids(snptEnqPtr.value) := true.B
    snptEnqPtr := snptEnqPtr + 1.U
  }
  when(io.deq) {
    snptValids(snptDeqPtr.value) := false.B
    snptDeqPtr := snptDeqPtr + 1.U
    XSError(isEmpty(snptEnqPtr, snptDeqPtr), "snapshots should not be empty when dequeue!\n")
  }
  when(io.flush) {
    snptValids := 0.U.asTypeOf(snptValids)
    snptEnqPtr := 0.U.asTypeOf(new SnapshotPtr)
    snptDeqPtr := 0.U.asTypeOf(new SnapshotPtr)
  }
}
