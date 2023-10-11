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
***************************************************************************************/

package xiangshan.cache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.{ClientMetadata, TLClientParameters, TLEdgeOut}
import utility.{Code, ParallelOR, ReplacementPolicy, SRAMTemplate}
import utils.XSDebug
import xiangshan.L1CacheErrorInfo

import scala.math.max


class L1DataReadReq(implicit p: Parameters) extends DCacheBundle {
  // you can choose which bank to read to save power
  val rmask = Bits(blockRows.W)
  val way_en = Bits(nWays.W)
  val addr = Bits(untagBits.W)
}

// Now, we can write a cache-block in a single cycle
class L1DataWriteReq(implicit p: Parameters) extends L1DataReadReq {
  val wmask = Bits(blockRows.W)
  val data = Vec(blockRows, Bits(rowBits.W))
}

abstract class AbstractDataArray(implicit p: Parameters) extends DCacheModule {
  val io = IO(new DCacheBundle {
    val read = Vec(3, Flipped(DecoupledIO(new L1DataReadReq)))
    val write = Flipped(DecoupledIO(new L1DataWriteReq))
    val resp = Output(Vec(3, Vec(blockRows, Bits(encRowBits.W))))
    val nacks = Output(Vec(3, Bool()))
    val errors = Output(Vec(3, new L1CacheErrorInfo))
  })

  def pipeMap[T <: Data](f: Int => T) = VecInit((0 until 3).map(f))

  def dumpRead() = {
    (0 until 3) map { w =>
      when(io.read(w).valid) {
        XSDebug(s"DataArray Read channel: $w valid way_en: %x addr: %x\n",
          io.read(w).bits.way_en, io.read(w).bits.addr)
      }
    }
  }

  def dumpWrite() = {
    when(io.write.valid) {
      XSDebug(s"DataArray Write valid way_en: %x addr: %x\n",
        io.write.bits.way_en, io.write.bits.addr)

      (0 until blockRows) map { r =>
        XSDebug(s"cycle: $r data: %x wmask: %x\n",
          io.write.bits.data(r), io.write.bits.wmask(r))
      }
    }
  }

  def dumpResp() = {
    (0 until 3) map { w =>
      XSDebug(s"DataArray ReadResp channel: $w\n")
      (0 until blockRows) map { r =>
        XSDebug(s"cycle: $r data: %x\n", io.resp(w)(r))
      }
    }
  }

  def dumpNack() = {
    (0 until 3) map { w =>
      when(io.nacks(w)) {
        XSDebug(s"DataArray NACK channel: $w\n")
      }
    }
  }

  def dump() = {
    dumpRead
    dumpWrite
    dumpNack
    dumpResp
  }
}
