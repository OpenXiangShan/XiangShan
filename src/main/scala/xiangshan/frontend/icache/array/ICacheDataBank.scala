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

package xiangshan.frontend.icache.array

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.mbist.MbistPipeline
import utility.sram.SRAMTemplate
import xiangshan.frontend.icache.ICacheDataEntry
import xiangshan.frontend.icache.ICacheModule

class ICacheDataBank(bankIdx: Int)(implicit p: Parameters) extends ICacheModule {
  class ICacheDataBankIO extends Bundle {
    class Read extends Bundle {
      class Req extends Bundle {
        val setIdx:  UInt = UInt(idxBits.W)
        val waymask: UInt = UInt(nWays.W)
      }
      class Resp extends Bundle {
        val entry: ICacheDataEntry = new ICacheDataEntry
      }
      val req:  DecoupledIO[Req] = Flipped(Decoupled(new Req))
      val resp: Resp             = Output(new Resp)
    }

    class Write extends Bundle {
      class Req extends Bundle {
        val setIdx:  UInt            = UInt(idxBits.W)
        val waymask: UInt            = UInt(nWays.W)
        val entry:   ICacheDataEntry = new ICacheDataEntry
      }
      val req: DecoupledIO[Req] = Flipped(Decoupled(new Req))
    }

    val read:  Read  = new Read
    val write: Write = new Write
  }

  val io: ICacheDataBankIO = IO(new ICacheDataBankIO)

  // sanity check
  require(DataSramWidth == (new ICacheDataEntry).getWidth)

  // manually handle ways instead of using SRAMTemplate way parameter for better power (r.req.valid control)
  private val ways = Seq.tabulate(nWays) { i =>
    Module(new SRAMTemplate(
      new ICacheDataEntry,
      set = nSets,
      way = 1,
      shouldReset = true,
      singlePort = true,
      withClockGate = false, // enable signal timing is bad, no gating here
      hasMbist = hasMbist,
      hasSramCtl = hasSramCtl,
      suffix = Option("icache_data")
    ))
  }
  private val mbistPl = MbistPipeline.PlaceMbistPipeline(1, s"MbistPipeICacheData_bank$bankIdx", hasMbist)

  /* *** read *** */
  io.read.req.ready := !io.write.req.valid && ways.map(_.io.r.req.ready).reduce(_ && _)

  ways.zipWithIndex.foreach { case (w, i) =>
    w.io.r.req.valid := io.read.req.valid && io.read.req.bits.waymask(i)
    w.io.r.req.bits.apply(
      setIdx = io.read.req.bits.setIdx
    )
  }

  private val readReqReg = RegEnable(io.read.req.bits, 0.U.asTypeOf(io.read.req.bits), io.read.req.fire)

  io.read.resp.entry := Mux1H(readReqReg.waymask, ways.map(_.io.r.resp.data.head))

  /* *** write *** */
  io.write.req.ready := ways.map(_.io.w.req.ready).reduce(_ && _)
  ways.zipWithIndex.foreach { case (w, i) =>
    w.io.w.req.valid := io.write.req.valid && io.write.req.bits.waymask(i)
    w.io.w.req.bits.apply(
      setIdx = io.write.req.bits.setIdx,
      data = io.write.req.bits.entry,
      waymask = 0.U // ignored in SRAMTemplate with way = 1, but required
    )
  }
}
