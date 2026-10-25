package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import utility._
import xiangshan.backend.rename.CompressType
import xiangshan.backend.rob.RobBundles._

/** Existing LSQ notification and pending-slot registers, without a new module boundary. */
final class RobLsqControl(
  commits: RobCommitIO,
  deqPtr: RobPtr,
  nextDeqPtr: RobPtr,
  pendingEntry: RobEntryBundle,
  redirect: ValidIO[Redirect],
  slotIsLoad: Seq[Seq[Bool]],
  entryIsVls: Seq[Bool]
)(implicit val p: Parameters) extends HasXSParameter with HasCircularQueuePtrHelper {
  val ldCommitVec = VecInit((0 until CommitWidth).flatMap { i =>
    (0 until 2).map { j =>
      val slotValid = commits.commitValid(i) && (j == 0).B ||
        commits.commitValid(i) && !CompressType.isNORMAL(commits.info(i).entryPairType)
      slotValid && slotIsLoad(i)(j)
    }
  })
  // TODO: Check if meet the require that only set scommit when commit scala store uop
  val newStCommit = VecInit((0 until CommitWidth).map(i => commits.commitValid(i) && commits.info(i).entryHasStore && !entryIsVls(i))).asUInt.orR
  val lcommit = RegNext(Mux(commits.isCommit, PopCount(ldCommitVec), 0.U))

  val commit = RegNext(commits.isCommit && commits.commitValid(0))

  /**
   * [ROB-39]
   * The LSQ pending boundary can advance from former to latter while the physical
   * entry remains at the head. Redirects that remove the whole head or only its
   * latter slot must restore former immediately, bypassing the delayed pointer.
   */

  val pendingBackPtr = Wire(new RobPtr)
  pendingBackPtr := deqPtr
  pendingBackPtr.isFormer := false.B

  val flushesHeadEntry = redirect.valid &&
    redirect.bits.robIdx.isSameEntry(deqPtr) &&
    redirect.bits.robIdx.isFormer &&
    redirect.bits.flushItself()
  val clearsHeadLatter = redirect.valid &&
    redirect.bits.robIdx.isSameEntry(deqPtr) &&
    CompressType.isNotNORMAL(pendingEntry.entryPairType) && (
      redirect.bits.robIdx.isFormer && !redirect.bits.flushItself() ||
      !redirect.bits.robIdx.isFormer && redirect.bits.flushItself()
    )
  val resetsPendingSlot = flushesHeadEntry || clearsHeadLatter
  val pendingPtrIsFormer = RegInit(true.B)
  when (resetsPendingSlot || commits.isCommit && commits.commitValid.asUInt.orR) {
    pendingPtrIsFormer := true.B
  }.elsewhen(
    !pendingBackPtr.needFlush(redirect) &&
    pendingEntry.valid &&
    CompressType.isNotNORMAL(pendingEntry.entryPairType) &&
    pendingEntry.formerUopNum === 0.U &&
    pendingEntry.latterUopNum =/= 0.U
  ) {
    pendingPtrIsFormer := false.B
  }

  val pendingPtrInput = WireInit(deqPtr)
  pendingPtrInput.isFormer := Mux(resetsPendingSlot, true.B, pendingPtrIsFormer)
  val pendingPtrReg = RegInit(RobPtr(false.B, 0.U))
  pendingPtrReg := pendingPtrInput

  // VirtualStoreQueue samples this boundary in the redirect cycle.  Do not let
  // a stale latter state survive either a whole-entry flush or a redirect that
  // removes only the latter half of the head entry.
  val pendingPtr = Mux(resetsPendingSlot, deqPtr.asFormer, pendingPtrReg)

  val pendingPtrNext = RegNext(nextDeqPtr)
  val scommit = RegNext(
    Mux(commits.isCommit && newStCommit ||
      CompressType.isCC(pendingEntry.entryPairType) &&
      pendingEntry.entryHasStore &&
      pendingEntry.formerUopNum === 0.U &&
      pendingEntry.latterUopNum =/= 0.U, 1.U, 0.U))
}
