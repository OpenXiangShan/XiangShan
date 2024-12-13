/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] Fernando Latorre, Grigorios Magklis, Jose González, Pedro Chaparro, and Antonio González. "[Crob: implementing a
* large instruction window through compression.](https://doi.org/10.1007/978-3-642-19448-1_7)" Transactions on
* High-Performance Embedded Architectures and Compilers III: 115-134. Berlin, Heidelberg: Springer Berlin Heidelberg.
* 2011.
***************************************************************************************/

package xiangshan.backend.rename

import org.chipsalliance.cde.config.Parameters
import chisel3.Bundle
import xiangshan.backend.Bundles.DecodedInst
import xiangshan.XSModule
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.DecodeLogic
import xiangshan._
import xiangshan.backend.fu.FuType

class CompressUnit(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val in = Vec(RenameWidth, Flipped(Valid(new DecodedInst)))
    val oddFtqVec = Vec(RenameWidth, Input(Bool()))
    val out = new Bundle {
      val needRobFlags = Vec(RenameWidth, Output(Bool()))
      val instrSizes = Vec(RenameWidth, Output(UInt(log2Ceil(RenameWidth + 1).W)))
      val masks = Vec(RenameWidth, Output(UInt(RenameWidth.W)))
      val canCompressVec = Vec(RenameWidth, Output(Bool()))
    }
  })

  val noExc = io.in.map(in => !in.bits.exceptionVec.asUInt.orR && !TriggerAction.isDmode(in.bits.trigger))
  val uopCanCompress = io.in.map(_.bits.canRobCompress)
  val canCompress = io.in.zip(noExc).zip(uopCanCompress).map { case ((in, noExc), canComp) =>
    in.valid && in.bits.lastUop && noExc && canComp
  }
  val extendedCanCompress = canCompress.zip(io.in).zip(io.oddFtqVec).flatMap { case ((canComp, in), oddFtq) =>
    Seq(FuType.isBlockBackCompress(in.bits.fuType) || canComp ,canComp && !oddFtq)
  }

  val compressTable = (0 until 1 << (2 * RenameWidth)).filter { baseCandidate =>
    // check exist 01 pair
    !(0 until RenameWidth).exists { i =>
      val bitPair = (baseCandidate >> (2 * i)) & 0x3
      bitPair == 0x2
    }
  }.zipWithIndex.map{ case (keyCandidate, index) =>
    // padding 0s at each side for convenience
    val key = 0 +: (0 until RenameWidth * 2).map(idx => (keyCandidate >> idx) & 1) :+ 0
    // count 1s on the left side of key (including itself)
    def cntL(idx: Int): Int = (if (key(idx - 1) == 1) cntL(idx - 1) else 0) + key(idx)
    // count 1s on the right side of key (including itself)
    def cntR(idx: Int): Int = (if (key(idx + 1) == 1) cntR(idx + 1) else 0) + key(idx)
    // the last instruction among consecutive rob-compressed instructions is marked
    val needRobsExpand = (0 until RenameWidth * 2).map( idx => ~(key.tail(idx) & key.tail(idx + 1)) & 1)
    val needRobs = needRobsExpand.grouped(2).map(group => group.reduce(_ | _)).toIndexedSeq
    // how many instructions are rob-compressed with this instruction (including itself)
    val uopSizes = (1 to RenameWidth).map{ idx =>
      val i = idx * 2 - 1
      if (key(i) == 0) 1 else (cntL(i) + cntR(i)) / 2
    }
    // which instructions are rob-compressed with this instruction
    val masks = uopSizes.zip(1 to RenameWidth).map { case (size, idx) => // compress masks
      val i = idx * 2 - 1
      if (key(i) == 0) Seq.fill(RenameWidth)(0).updated(idx - 1, 1)
      else Seq.fill(RenameWidth)(0).patch(idx - (cntL(i) + 1)/2, Seq.fill(size)(1), size)
    }

    println("[Rename.Compress]" +
      " index: "    + index +
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
  val decoder = DecodeLogic(VecInit(extendedCanCompress).asUInt, default, compressTable)
  (io.out.needRobFlags ++ io.out.instrSizes ++ io.out.masks).zip(decoder).foreach {
    case (sink, source) => sink := source
  }
  io.out.canCompressVec := VecInit(canCompress)
}
