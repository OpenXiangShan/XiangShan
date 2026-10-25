package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.rename.CompressType

/** Decode uop acceptance separately from physical entry allocation. */
final class RobEnqueueEvents(
  requests: Vec[ValidIO[EnqRobUop]],
  canAccept: Bool,
  pointers: Vec[RobPtr]
)(implicit val p: Parameters) extends HasXSParameter {
  val allocatedPointers = VecInit((0 until RenameWidth).map { lane =>
    pointers(PopCount(requests.take(lane).map(req => req.valid && req.bits.firstUop)))
  })
  val acceptedUops = VecInit(requests.map(req => req.valid && canAccept))
  val allocatedEntries = VecInit(requests.map(req => req.valid && req.bits.firstUop && canAccept))
}

final case class RobSlotWritebackEvents(
  formerCount: UInt, latterCount: UInt,
  formerNeedsFlush: Bool, latterNeedsFlush: Bool, anyNeedsFlush: Bool
)

/** Keep generation filtering upstream, matching the original physical-index protocol. */
object RobWritebackEvents {
  def forEntry(
    index: UInt,
    writebacks: Seq[ValidIO[WriteBackRobBundle]],
    exceptionWritebacks: Seq[ValidIO[WriteBackRobBundle]],
    needsFlush: Seq[Bool]
  ): RobSlotWritebackEvents = {
    val formerCount = PopCount(writebacks.map(wb =>
      wb.valid && wb.bits.robIdx.value === index && wb.bits.robIdx.isFormer))
    val latterCount = PopCount(writebacks.map(wb =>
      wb.valid && wb.bits.robIdx.value === index && !wb.bits.robIdx.isFormer))
    val exceptionMatches = exceptionWritebacks.zip(needsFlush).map { case (wb, flush) =>
      wb.valid && flush && wb.bits.robIdx.value === index
    }
    val formerMatches = exceptionMatches.zip(exceptionWritebacks).map { case (hit, wb) =>
      hit && wb.bits.robIdx.isFormer
    }
    val latterMatches = exceptionMatches.zip(exceptionWritebacks).map { case (hit, wb) =>
      hit && !wb.bits.robIdx.isFormer
    }
    RobSlotWritebackEvents(formerCount, latterCount,
      Cat(formerMatches).orR, Cat(latterMatches).orR, Cat(exceptionMatches).orR)
  }
}

object RobRedirectEffect {
  def clearsLatter(pairType: UInt, former: Bool, self: Bool): Bool =
    CompressType.isNotNORMAL(pairType) && (former && !self || !former && self)
}
