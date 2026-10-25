/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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
* [1] James E. Smith, and Andrew R. Pleszkun. "[Implementation of precise interrupts in pipelined processors.]
* (https://dl.acm.org/doi/10.5555/327010.327125)" 12th Annual International Symposium on Computer Architecture (ISCA).
* 1985.
***************************************************************************************/

package xiangshan.backend.rob

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import difftest.common._
import difftest.plugin.topdown._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import utils._
import xiangshan._
import xiangshan.PerfDebugInfo
import xiangshan.backend.{BackendParams, Bundles, GPAMemEntry, RatToVecExcpMod, RegWriteFromRab, VecExcpInfo}
import xiangshan.backend.Bundles._
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.fu.FuType
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import xiangshan.backend.ctrlblock.{DebugLSIO, DebugLsInfo, LsTopdownInfo}
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.rename.{SnapshotGenerator, CompressType, NoCompressReason}
import xiangshan.backend.rob.RobBundles._
import xiangshan.backend.trace._
import chisel3.experimental.BundleLiterals._
import chisel3.util.experimental.decode.TruthTable
import xiangshan.TopDownCounters._
import xiangshan.backend.dispatch._

final class RobTraceCodec(implicit val p: Parameters) extends HasXSParameter {
  val traceInstrCountWidth = log2Ceil(RenameWidth + 1)
  private val traceEncodeTable = (1 to RenameWidth).flatMap { instrNum =>
    (0 to instrNum).map(nonRVCNum => (instrNum, nonRVCNum))
  }
  private val traceDecodeTable = traceEncodeTable.map { case (instrNum, nonRVCNum) =>
    (instrNum, instrNum + nonRVCNum)
  }
  def decodeTraceInstrCount(traceIretire: UInt): UInt = {
    chisel3.util.experimental.decode.decoder(
      traceIretire,
      TruthTable(
        traceDecodeTable.zipWithIndex.map { case ((instrNum, _), encode) =>
          BitPat((encode + 1).U(IretireWidthEncoded.W)) -> BitPat(instrNum.U(traceInstrCountWidth.W))
        },
        BitPat.N(traceInstrCountWidth)
      )
    )
  }
  def decodeTraceHalfWords(traceIretire: UInt): UInt = {
    chisel3.util.experimental.decode.decoder(
      traceIretire,
      TruthTable(
        traceDecodeTable.zipWithIndex.map { case ((_, halfWords), encode) =>
          BitPat((encode + 1).U(IretireWidthEncoded.W)) -> BitPat(halfWords.U(IretireWidthCommited.W))
        },
        BitPat.N(IretireWidthCommited)
      )
    )
  }
  def traceHalfWordsToBytes(halfWords: UInt): UInt = {
    val formerLenWidth = log2Ceil(RenameWidth * 4 + 1)
    Cat(halfWords, 0.U(1.W))(formerLenWidth - 1, 0)
  }

}
