package xiangshan.backend.rename

import chipsalliance.rocketchip.config.Parameters
import chisel3.Bundle
import xiangshan.backend.Bundles.DecodedInst
import xiangshan.XSModule
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.DecodeLogic
import xiangshan._

class CompressUnit(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val in = Vec(RenameWidth, Flipped(Valid(new DecodedInst)))
    val out = new Bundle {
      val needRobFlags = Vec(RenameWidth, Output(Bool()))
      val instrSizes = Vec(RenameWidth, Output(UInt(log2Ceil(RenameWidth + 1).W)))
      val masks = Vec(RenameWidth, Output(UInt(RenameWidth.W)))
    }
  })

  val noExc = io.in.map(!_.bits.exceptionVec.asUInt.orR)
  val uopCanCompress = io.in.map(_.bits.canRobCompress)
  val canCompress = io.in.zip(noExc).zip(uopCanCompress).map { case ((in, noExc), canComp) =>
    in.valid && !CommitType.isFused(in.bits.commitType) && in.bits.lastUop && noExc && canComp
  }

  val compressTable = (0 until 1 << RenameWidth).map { case keyCandidate =>
    // padding 0s at each side for convenience
    val key = 0 +: (0 until RenameWidth).map(idx => (keyCandidate >> idx) & 1) :+ 0
    // count 1s on the left side of key (including itself)
    def cntL(idx: Int): Int = (if (key(idx - 1) == 1) cntL(idx - 1) else 0) + key(idx)
    // count 1s on the right side of key (including itself)
    def cntR(idx: Int): Int = (if (key(idx + 1) == 1) cntR(idx + 1) else 0) + key(idx)
    // the last instruction among consecutive rob-compressed instructions is marked
    val needRobs = (0 until RenameWidth).map(idx => ~(key.tail(idx) & key.tail(idx + 1)) & 1)
    // how many instructions are rob-compressed with this instruction (including itself)
    val uopSizes = (1 to RenameWidth).map(idx => if (key(idx) == 0) 1 else cntL(idx) + cntR(idx) - 1)
    // which instructions are rob-compressed with this instruction
    val masks = uopSizes.zip(1 to RenameWidth).map { case (size, idx) => // compress masks
      if (key(idx) == 0) Seq.fill(RenameWidth)(0).updated(idx - 1, 1)
      else Seq.fill(RenameWidth)(0).patch(idx - cntL(idx), Seq.fill(size)(1), size)
    }

    println("[Rename.Compress]" +
      " i: "        + keyCandidate +
      " key: "      + key.tail.dropRight(1) +
      " needRobs: " + needRobs +
      " uopSizes: " + uopSizes +
      " masks: "    + masks.map(_.map(_.toBinaryString).reduce(_ + _))
    )

    val keyBitPat = BitPat(keyCandidate.U)
    val needRobBitPats = needRobs.map(x => BitPat(x.U))
    val uopSizeBitPats = uopSizes.map(x => BitPat(x.U))
    val maskBitPats = masks.map(m => BitPat(m.foldRight(0)(_ | _ << 1).U))

    (keyBitPat -> (needRobBitPats ++ uopSizeBitPats ++ maskBitPats))
  }

  val default = Seq.fill(3 * RenameWidth)(BitPat.N())
  val decoder = DecodeLogic(VecInit(canCompress).asUInt, default, compressTable)
  (io.out.needRobFlags ++ io.out.instrSizes ++ io.out.masks).zip(decoder).foreach {
    case (sink, source) => sink := source
  }
}
