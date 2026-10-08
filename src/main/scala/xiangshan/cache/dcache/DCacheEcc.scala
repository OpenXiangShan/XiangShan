/***************************************************************************************
 * Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          https://license.coscl.org.cn/MulanPSL2
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 ***************************************************************************************/

package xiangshan.cache

import chisel3._
import org.chipsalliance.cde.config.Parameters
import utility.Code

class DCacheEccDetect(dataBits: Int, code: Code) extends Module {
  val io = IO(new Bundle {
    val encoded = Input(UInt(code.width(dataBits).W))
    val valid = Input(Bool())
    val correctable = Output(Bool())
    val uncorrectable = Output(Bool())
    val corrected = Output(UInt(dataBits.W))
  })

  private val decoded = code.decode(io.encoded)
  io.correctable := io.valid && decoded.correctable && !decoded.uncorrectable
  io.uncorrectable := io.valid && decoded.uncorrectable
  io.corrected := decoded.corrected
}

class DCacheEccCorrect(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val tag = Input(Vec(nWays, UInt(encTagBits.W)))
    val tagValid = Input(UInt(nWays.W))
    val correctedTag = Output(Vec(nWays, UInt(tagBits.W)))
    val tagCorrectable = Output(UInt(nWays.W))
    val tagUncorrectable = Output(UInt(nWays.W))

    val data = Input(Vec(DCacheBanks, new L1BankedDataReadResult))
    val dataValid = Input(UInt(DCacheBanks.W))
    val correctedData = Output(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
    val dataCorrectable = Output(UInt(DCacheBanks.W))
    val dataUncorrectable = Output(UInt(DCacheBanks.W))
  })

  private val tagDecoders = Seq.tabulate(nWays) { i =>
    val decoder = Module(new DCacheEccDetect(tagBits, cacheParams.tagCode))
    decoder.io.encoded := io.tag(i)
    decoder.io.valid := io.tagValid(i)
    io.correctedTag(i) := decoder.io.corrected
    decoder
  }
  io.tagCorrectable := VecInit(tagDecoders.map(_.io.correctable)).asUInt
  io.tagUncorrectable := VecInit(tagDecoders.map(_.io.uncorrectable)).asUInt

  private val dataDecoders = Seq.tabulate(DCacheBanks) { i =>
    val decoder = Module(new DCacheEccDetect(DCacheSRAMRowBits, cacheParams.dataCode))
    decoder.io.encoded := io.data(i).asECCData()
    decoder.io.valid := io.dataValid(i)
    io.correctedData(i) := decoder.io.corrected
    decoder
  }
  io.dataCorrectable := VecInit(dataDecoders.map(_.io.correctable)).asUInt
  io.dataUncorrectable := VecInit(dataDecoders.map(_.io.uncorrectable)).asUInt
}
