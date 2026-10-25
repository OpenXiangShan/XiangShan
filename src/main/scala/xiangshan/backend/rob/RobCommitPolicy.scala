package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.rename.CompressType
import xiangshan.backend.rob.RobBundles._

/** Preserve row-relative candidates and the separate global retirement mode. */
final class RobCommitPolicy(
  row: Vec[RobCommitEntryBundle],
  alreadyRetired: Vec[Bool],
  interruptPending: Bool,
  interruptEnabled: Bool,
  headNeedsFlush: Bool,
  headHasFlushed: Bool,
  trapObservationPending: Bool
)(implicit val p: Parameters) extends HasXSParameter {
  val allowOnlyOne = VecInit(row.map(entry => entry.commit_v && (
    entry.slotNeedFlushMask(0) ||
      entry.slotNeedFlushMask(1) && CompressType.isNotNORMAL(entry.entryPairType)
  ))).asUInt.orR || interruptPending || trapObservationPending
  private val blockedByIncomplete = VecInit((0 until CommitWidth).map(i =>
    !row(i).commit_w && !alreadyRetired(i)))
  val rowCandidates = Wire(Vec(CommitWidth, Bool()))
  for (i <- 0 until CommitWidth) {
    val blocked = interruptEnabled || (headNeedsFlush && !headHasFlushed)
    val blockedByOlder = if (i != 0) {
      blockedByIncomplete.asUInt(i, 0).orR ||
        allowOnlyOne && !alreadyRetired.asUInt(i - 1, 0).andR
    } else false.B
    rowCandidates(i) := row(i).commit_v && row(i).commit_w &&
      !blocked && !blockedByOlder && !alreadyRetired(i)
  }
  val headCandidates = PriorityMux(rowCandidates, (0 until CommitWidth).map(i =>
    (rowCandidates.asUInt >> i).asUInt.asTypeOf(rowCandidates)))
}
