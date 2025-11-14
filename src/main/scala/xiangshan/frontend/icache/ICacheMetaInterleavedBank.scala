// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.sram.SplittedSRAMTemplate

class ICacheMetaInterleavedBank(implicit p: Parameters) extends ICacheModule
    with ICacheEccHelper
    with ICacheAddrHelper {
  class ICacheMetaInterleavedBankIO extends Bundle {
    class Read extends Bundle {
      class Req extends Bundle {
        val setIdx: UInt = UInt(InterleavedSetIdxBits.W)
      }
      class Resp extends Bundle {
        val entries: Vec[Valid[ICacheMetaEntry]] = Vec(nWays, Valid(new ICacheMetaEntry))
      }
      val req:  DecoupledIO[Req] = Flipped(DecoupledIO(new Req))
      val resp: Resp             = Output(new Resp)
    }

    class Write extends Bundle {
      class Req extends Bundle {
        val setIdx:  UInt            = UInt(InterleavedSetIdxBits.W)
        val waymask: UInt            = UInt(nWays.W)
        val entry:   ICacheMetaEntry = new ICacheMetaEntry
      }
      val req: DecoupledIO[Req] = Flipped(DecoupledIO(new Req))
    }

    class Flush extends Bundle {
      class Req extends Bundle {
        val setIdx:  UInt = UInt(InterleavedSetIdxBits.W)
        val waymask: UInt = UInt(nWays.W)
      }
      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    val read:     Read  = new Read
    val write:    Write = new Write
    val flush:    Flush = new Flush
    val flushAll: Bool  = Input(Bool())
  }

  val io: ICacheMetaInterleavedBankIO = IO(new ICacheMetaInterleavedBankIO)

  private val tagArray = Module(new SplittedSRAMTemplate(
    new ICacheMetaEntry,
    set = NumInterleavedSet,
    way = nWays,
    waySplit = MetaWaySplit,
    dataSplit = MetaDataSplit,
    shouldReset = true,
    singlePort = true,
    withClockGate = true,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl
  ))

  private val validArray = RegInit(VecInit.fill(NumInterleavedSet)(0.U(nWays.W)))

  /* *** read *** */
  io.read.req.ready := !io.write.req.valid && !io.flush.req.valid && !io.flushAll && tagArray.io.r.req.ready

  tagArray.io.r.req.valid := io.read.req.valid
  tagArray.io.r.req.bits.apply(
    setIdx = io.read.req.bits.setIdx
  )

  private val readReqReg = RegEnable(io.read.req.bits, 0.U.asTypeOf(io.read.req.bits), io.read.req.fire)

  io.read.resp.entries.zipWithIndex.foreach { case (e, i) =>
    e.valid := validArray(readReqReg.setIdx)(i)
    e.bits  := tagArray.io.r.resp.data(i)
  }

  /* *** write *** */
  io.write.req.ready := tagArray.io.w.req.ready

  tagArray.io.w.req.valid := io.write.req.valid
  tagArray.io.w.req.bits.apply(
    data = io.write.req.bits.entry,
    setIdx = io.write.req.bits.setIdx,
    waymask = io.write.req.bits.waymask
  )

  when(io.write.req.valid) {
    validArray(io.write.req.bits.setIdx) := validArray(io.write.req.bits.setIdx) | io.write.req.bits.waymask
  }

  /* *** flush *** */
  // flush standalone set (e.g. flushed by mainPipe before doing re-fetch)
  when(io.flush.req.valid) {
    validArray(io.flush.req.bits.setIdx) := validArray(io.flush.req.bits.setIdx) & (~io.flush.req.bits.waymask).asUInt
  }

  // flush all (e.g. fence.i)
  when(io.flushAll) {
    validArray := 0.U.asTypeOf(validArray)
  }
}
