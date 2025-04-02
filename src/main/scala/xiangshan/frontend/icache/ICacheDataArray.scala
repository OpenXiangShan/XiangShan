// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import utility.mbist.MbistPipeline

class ICacheDataArray(implicit p: Parameters) extends ICacheModule with ICacheEccHelper with ICacheDataHelper {
  class ICacheDataArrayIO(implicit p: Parameters) extends ICacheBundle {
    val write: DataWriteBundle = Flipped(new DataWriteBundle)
    val read:  DataReadBundle  = Flipped(new DataReadBundle)
  }

  val io: ICacheDataArrayIO = IO(new ICacheDataArrayIO)

  class ICacheDataEntry(implicit p: Parameters) extends ICacheBundle {
    val data: UInt = UInt(ICacheDataBits.W)
    val code: UInt = UInt(ICacheDataCodeBits.W)
  }

  private object ICacheDataEntry {
    def apply(data: UInt, poison: Bool)(implicit p: Parameters): ICacheDataEntry = {
      val entry = Wire(new ICacheDataEntry)
      entry.data := data
      entry.code := encodeDataEccByBank(data, poison)
      entry
    }
  }

  /**
   ******************************************************************************
   * data array
   ******************************************************************************
   */
  private val writeDatas   = io.write.req.bits.data.asTypeOf(Vec(ICacheDataBanks, UInt(ICacheDataBits.W)))
  private val writeEntries = writeDatas.map(ICacheDataEntry(_, io.write.req.bits.poison).asUInt)

  // io.read() are copies to control fan-out, we can simply use .head here
  private val bankSel  = getBankSel(io.read.req.head.bits.blkOffset, io.read.req.head.valid)
  private val lineSel  = getLineSel(io.read.req.head.bits.blkOffset)
  private val waymasks = io.read.req.head.bits.waymask
  private val masks    = Wire(Vec(nWays, Vec(ICacheDataBanks, Bool())))
  (0 until nWays).foreach { way =>
    (0 until ICacheDataBanks).foreach { bank =>
      masks(way)(bank) := Mux(
        lineSel(bank),
        waymasks(1)(way) && bankSel(1)(bank),
        waymasks(0)(way) && bankSel(0)(bank)
      )
    }
  }

  private val dataArrays = (0 until nWays).map { way =>
    val banks = (0 until ICacheDataBanks).map { bank =>
      val sramBank = Module(new SRAMTemplateWithFixedWidth(
        UInt(ICacheDataEntryBits.W),
        set = nSets,
        width = ICacheDataSRAMWidth,
        shouldReset = true,
        holdRead = true,
        singlePort = true,
        withClockGate = false, // enable signal timing is bad, no gating here
        hasMbist = hasMbist,
        hasSramCtl = hasSramCtl
      ))

      // read
      sramBank.io.r.req.valid := io.read.req(bank % 4).valid && masks(way)(bank)
      sramBank.io.r.req.bits.apply(setIdx =
        Mux(lineSel(bank), io.read.req(bank % 4).bits.vSetIdx(1), io.read.req(bank % 4).bits.vSetIdx(0))
      )
      // write
      sramBank.io.w.req.valid := io.write.req.valid && io.write.req.bits.waymask(way).asBool
      sramBank.io.w.req.bits.apply(
        data = writeEntries(bank),
        setIdx = io.write.req.bits.vSetIdx,
        // waymask is invalid when way of SRAMTemplate <= 1
        waymask = 0.U
      )
      sramBank
    }
    MbistPipeline.PlaceMbistPipeline(1, s"MbistPipeIcacheDataWay${way}", hasMbist)
    banks
  }

  /**
   ******************************************************************************
   * read logic
   ******************************************************************************
   */
  private val masksReg = RegEnable(masks, 0.U.asTypeOf(masks), io.read.req(0).valid)
  private val readDataWithCode = (0 until ICacheDataBanks).map { bank =>
    Mux1H(VecInit(masksReg.map(_(bank))).asTypeOf(UInt(nWays.W)), dataArrays.map(_(bank).io.r.resp.asUInt))
  }
  private val readEntries = readDataWithCode.map(_.asTypeOf(new ICacheDataEntry()))
  private val readDatas   = VecInit(readEntries.map(_.data))
  private val readCodes   = VecInit(readEntries.map(_.code))

  // TEST: force ECC to fail by setting readCodes to 0
  if (ICacheForceDataECCError) {
    readCodes.foreach(_ := 0.U)
  }

  /**
   ******************************************************************************
   * IO
   ******************************************************************************
   */
  io.read.resp.datas := readDatas
  io.read.resp.codes := readCodes
  io.write.req.ready := true.B
  io.read.req.foreach(_.ready := !io.write.req.valid)
}
