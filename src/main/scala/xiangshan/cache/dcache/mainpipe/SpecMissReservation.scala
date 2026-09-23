/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ParallelMux

private[cache] final case class SpecMissReservationChoice(
  valid: Bool,
  id: UInt,
  rank: UInt,
  choiceOH: UInt,
  addressBusy: Bool
)

/** Pure combinational reservation selection shared by MissQueue and its directed tests. */
private[cache] object SpecMissReservationLogic {
  private def any(signals: Seq[Bool]): Bool =
    if (signals.isEmpty) false.B else VecInit(signals).asUInt.orR

  def allocate(
    initialFree: Seq[Bool],
    freeCountBefore: Seq[UInt],
    normalValid: Seq[Bool],
    requests: Seq[(Bool, UInt)],
    occupiedBlocks: Seq[(Bool, UInt)]
  ): Seq[SpecMissReservationChoice] = {
    require(initialFree.nonEmpty)
    require(initialFree.length == freeCountBefore.length)

    val nEntries = initialFree.length
    val idWidth = log2Up(nEntries)
    val rankWidth = log2Ceil(nEntries + normalValid.length + requests.length + 1)
    val normalBound = PopCount(VecInit(normalValid))

    requests.zipWithIndex.map { case ((requestValid, requestBlock), w) =>
      val occupiedBusy = occupiedBlocks.map { case (valid, block) =>
        valid && block === requestBlock
      }
      // Raw request validity, rather than grant validity, keeps all ports independent
      // of one another's fabric ready path while still assigning distinct ranks/blocks.
      val earlierRequestBusy = requests.take(w).map { case (valid, block) =>
        valid && block === requestBlock
      }
      val addressBusy = any(occupiedBusy ++ earlierRequestBusy)
      val earlierRequestCount = if (w == 0) 0.U else PopCount(VecInit(requests.take(w).map(_._1)))
      val rank = Wire(UInt(rankWidth.W))
      rank := normalBound +& earlierRequestCount
      val choice = initialFree.zip(freeCountBefore).map { case (free, countBefore) =>
        free && countBefore === rank
      }
      val choiceOH = VecInit(choice).asUInt
      val choiceValid = choiceOH.orR
      val id = ParallelMux(choice.zipWithIndex.map { case (selected, entry) =>
        selected -> entry.U(idWidth.W)
      })

      SpecMissReservationChoice(
        valid = requestValid && choiceValid && !addressBusy,
        id = id,
        rank = rank,
        choiceOH = choiceOH,
        addressBusy = addressBusy
      )
    }
  }
}

private[cache] class SpecMissGrantCapture(implicit p: Parameters) extends DCacheBundle {
  val id = UInt(log2Up(cfg.nMissEntries).W)
  val line = new MissTrackLine
}

/** One-cycle candidate.fire to grant handoff. All payload flops are overwritten each cycle. */
private[cache] class SpecMissGrantRegister(nPorts: Int)(implicit p: Parameters) extends DCacheModule {
  require(nPorts > 0)

  val io = IO(new Bundle {
    val capture = Input(Vec(nPorts, Valid(new SpecMissGrantCapture)))
    val grant = Output(Vec(nPorts, Valid(UInt(log2Up(cfg.nMissEntries).W))))
    val line = Output(Vec(nPorts, new MissTrackLine))
  })

  private val valid = RegInit(VecInit(Seq.fill(nPorts)(false.B)))
  private val id = Reg(Vec(nPorts, UInt(log2Up(cfg.nMissEntries).W)))
  private val line = Reg(Vec(nPorts, new MissTrackLine))

  for (w <- 0 until nPorts) {
    valid(w) := io.capture(w).valid
    id(w) := io.capture(w).bits.id
    line(w) := io.capture(w).bits.line
    io.grant(w).valid := valid(w)
    io.grant(w).bits := id(w)
    io.line(w) := line(w)
  }
}
