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

    val read:  Vec[Read] = Vec(FetchPorts, new Read)
    val write: Write     = new Write
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
  // read sram
  ways.zipWithIndex.foreach { case (w, i) =>
    // decide which port needs to read this way
    // we allow more than 1 port reading the same way with same setIdx, so this can be more-than-one-hot
    // in this case, Mux1H (mask0 & setIdx0 | mask1 & setIdx1) should still give the correct setIdx
    // (setIdx0 | setIdx1 === setIdx0 when setIdx0 === setIdx1)
    val reqValidVec = io.read.map(port => port.req.valid && port.req.bits.waymask(i))

    w.io.r.req.valid := reqValidVec.reduce(_ || _)
    w.io.r.req.bits.apply(
      setIdx = Mux1H(reqValidVec, io.read.map(_.req.bits.setIdx))
    )

    // we disallow reading different setIdx, this should be guaranteed by ICacheDataArray or upper level
    // do sanity check here, should not be included in the final hardware, so no worry about bad timing
    val firstValidReq = Mux1H(PriorityEncoderOH(reqValidVec), io.read.map(_.req.bits))
    assert(
      // we require either: 1. reqValidVec is one-hot
      PopCount(reqValidVec) <= 1.U ||
        // or 2. all valid requests on this way have same setIdx
        reqValidVec.zipWithIndex.map { case (v, portIdx) =>
          !v || io.read(portIdx).req.bits.setIdx === firstValidReq.setIdx
        }.reduce(_ && _),
      s"DataArray bank$bankIdx way$i read conflict!"
    )
  }

  io.read.zipWithIndex.foreach { case (port, i) =>
    // ready
    port.req.ready := !io.write.req.valid && ways.map(_.io.r.req.ready).reduce(_ && _)

    // send response
    val reqReg = RegEnable(port.req.bits, 0.U.asTypeOf(port.req.bits), port.req.fire)
    port.resp.entry := Mux1H(reqReg.waymask, ways.map(_.io.r.resp.data.head))
  }

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
