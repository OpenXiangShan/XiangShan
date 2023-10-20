package xiangshan.backend.rename

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper}
import utils.XSError
import xiangshan.{XSCoreParamsKey, XSModule}


class SnapshotPtr(implicit p: Parameters) extends CircularQueuePtr[SnapshotPtr](
  p => p(XSCoreParamsKey).RenameSnapshotNum
)

object SnapshotGenerator extends HasCircularQueuePtrHelper {
  def apply[T <: Data](enqData: T, enq: Bool, deq: Bool, redirect: Bool, flushVec: Vec[Bool])(implicit p: Parameters): Vec[T] = {
    val snapshotGen = Module(new SnapshotGenerator(enqData))
    snapshotGen.io.enq := enq
    snapshotGen.io.enqData := enqData
    snapshotGen.io.deq := deq
    snapshotGen.io.redirect := redirect
    snapshotGen.io.flushVec := flushVec
    snapshotGen.io.snapshots
  }
}

class SnapshotGenerator[T <: Data](dataType: T)(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper {

  class SnapshotGeneratorIO extends Bundle {
    val enq = Input(Bool())
    val enqData = Input(chiselTypeOf(dataType))
    val deq = Input(Bool())
    val redirect = Input(Bool())
    val flushVec = Input(Vec(RenameSnapshotNum, Bool()))
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

  when(!io.redirect && !isFull(snptEnqPtr, snptDeqPtr) && io.enq) {
    snapshots(snptEnqPtr.value) := io.enqData
    snptValids(snptEnqPtr.value) := true.B
    snptEnqPtr := snptEnqPtr + 1.U
  }
  when(!io.redirect && io.deq) {
    snptValids(snptDeqPtr.value) := false.B
    snptDeqPtr := snptDeqPtr + 1.U
    XSError(isEmpty(snptEnqPtr, snptDeqPtr), "snapshots should not be empty when dequeue!\n")
  }
  snptValids.zip(io.flushVec).foreach { case (valid, flush) =>
    when(flush) { valid := false.B }
  }
  when((Cat(io.flushVec) & Cat(snptValids)).orR) {
    val newEnqPtrCandidate = (0 until RenameSnapshotNum).map(snptDeqPtr + _.U)
    val newEnqPtrQualified = Wire(Vec(RenameSnapshotNum, Bool()))
    newEnqPtrQualified.head := !snptValids(newEnqPtrCandidate.head.value) || io.flushVec(newEnqPtrCandidate.head.value)
    newEnqPtrQualified.tail zip newEnqPtrCandidate.tail.zip(newEnqPtrCandidate.drop(1)).map {
      case (thiz, last) => snptValids(last.value) && (!snptValids(thiz.value) || io.flushVec(thiz.value))
    } foreach (x => x._1 := x._2)
    snptEnqPtr := MuxCase(newEnqPtrCandidate.last, newEnqPtrQualified.zip(newEnqPtrCandidate).dropRight(1))
  }
}
