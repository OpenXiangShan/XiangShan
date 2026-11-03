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
import chisel3.util.RegEnable
import org.chipsalliance.cde.config.Parameters
import utility.Code

class DCacheEccResult(dataBits: Int) extends Bundle {
  val errorMask = UInt(dataBits.W)
  val correctable = Bool()
  val uncorrectable = Bool()
}

class DCacheEccDetect(dataBits: Int, code: Code) extends Module {
  val io = IO(new Bundle {
    val encoded = Input(UInt(code.width(dataBits).W))
    val valid = Input(Bool())
    val errorMask = Output(UInt(dataBits.W))
    val correctable = Output(Bool())
    val uncorrectable = Output(Bool())
  })

  private val decoded = code.decode(io.encoded)
  io.correctable := io.valid && decoded.correctable && !decoded.uncorrectable
  io.uncorrectable := io.valid && decoded.uncorrectable
  private val errorMask = decoded.uncorrected ^ decoded.corrected
  io.errorMask := Mux(io.correctable, errorMask, 0.U(dataBits.W))
}

class DCacheEccCorrect(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val tag = Input(Vec(nWays, UInt(tagBits.W)))
    val tagErrorMask = Input(Vec(nWays, UInt(tagBits.W)))
    val tagEnable = Input(Bool())
    val correctedTag = Output(Vec(nWays, UInt(tagBits.W)))

    val data = Input(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
    val dataErrorMask = Input(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
    val correctedData = Output(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
  })

  private val tag = RegEnable(io.tag, 0.U.asTypeOf(io.tag), io.tagEnable)
  private val tagErrorMask = RegEnable(io.tagErrorMask, 0.U.asTypeOf(io.tagErrorMask), io.tagEnable)

  // Detection has already completed in the preceding pipeline stage.
  for (i <- 0 until nWays) {
    io.correctedTag(i) := tag(i) ^ tagErrorMask(i)
  }

  for (i <- 0 until DCacheBanks) {
    // The SRAM response is already registered into s3 alongside this mask.
    io.correctedData(i) := io.data(i) ^ io.dataErrorMask(i)
  }
}
