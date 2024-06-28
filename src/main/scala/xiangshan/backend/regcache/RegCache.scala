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

package xiangshan.backend.regcache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.BackendParams

class RegCache()(implicit p: Parameters, params: BackendParams) extends XSModule {

  val io = IO(new RegCacheIO())

  println(s"[RegCache] readPorts: ${params.getIntExuRCReadSize} + ${params.getMemExuRCReadSize}, " +
    s"writePorts: ${params.getIntExuRCWriteSize} + ${params.getMemExuRCWriteSize}")

  println(s"[RegCache] dataWidth: ${params.intSchdParams.get.rfDataWidth}, addrWidth: ${RegCacheIdxWidth}, tagWidth: ${params.intSchdParams.get.pregIdxWidth}")

  require(RegCacheIdxWidth == (log2Up(IntRegCacheSize) + 1), "IntRegCache should be half of the whole RegCache")
  require(RegCacheIdxWidth == (log2Up(MemRegCacheSize) + 1), "MemRegCache should be half of the whole RegCache")

  private val IntRegCacheReadSize = params.getIntExuRCReadSize + params.getMemExuRCReadSize
  private val IntRegCacheWriteSize = params.getIntExuRCWriteSize
  private val MemRegCacheReadSize = params.getIntExuRCReadSize + params.getMemExuRCReadSize
  private val MemRegCacheWriteSize = params.getMemExuRCWriteSize

  val IntRegCache = Module(new RegCacheDataModule("IntRegCache", IntRegCacheSize, IntRegCacheReadSize, IntRegCacheWriteSize, 
                                                  params.intSchdParams.get.rfDataWidth, RegCacheIdxWidth - 1, params.intSchdParams.get.pregIdxWidth))

  val MemRegCache = Module(new RegCacheDataModule("MemRegCache", MemRegCacheSize, MemRegCacheReadSize, MemRegCacheWriteSize, 
                                                  params.intSchdParams.get.rfDataWidth, RegCacheIdxWidth - 1, params.intSchdParams.get.pregIdxWidth))

  val IntRegCacheAgeTimer = Module(new RegCacheAgeTimer(IntRegCacheSize, IntRegCacheReadSize, IntRegCacheWriteSize, RegCacheIdxWidth - 1))

  val MemRegCacheAgeTimer = Module(new RegCacheAgeTimer(MemRegCacheSize, MemRegCacheReadSize, MemRegCacheWriteSize, RegCacheIdxWidth - 1))

  val IntRegCacheRepRCIdx = RegCacheAgeDetector(IntRegCacheSize, IntRegCacheWriteSize, IntRegCacheAgeTimer.io.ageInfo)
  val MemRegCacheRepRCIdx = RegCacheAgeDetector(MemRegCacheSize, MemRegCacheWriteSize, MemRegCacheAgeTimer.io.ageInfo)

  IntRegCacheAgeTimer.io.validInfo := IntRegCache.io.validInfo
  MemRegCacheAgeTimer.io.validInfo := MemRegCache.io.validInfo

  io.readPorts
  .lazyZip(IntRegCache.io.readPorts.lazyZip(MemRegCache.io.readPorts))
  .lazyZip(IntRegCacheAgeTimer.io.readPorts.lazyZip(MemRegCacheAgeTimer.io.readPorts))
  .foreach{ case (r_in, (r_int, r_mem), (r_int_at, r_mem_at)) => 
    val in_addr = RegEnable(r_in.addr, r_in.ren)
    val int_ren = GatedValidRegNext(r_in.ren & ~r_in.addr(RegCacheIdxWidth - 1))
    val mem_ren = GatedValidRegNext(r_in.ren & r_in.addr(RegCacheIdxWidth - 1))
    r_int.ren  := int_ren
    r_mem.ren  := mem_ren
    r_int.addr := in_addr(RegCacheIdxWidth - 2, 0)
    r_mem.addr := in_addr(RegCacheIdxWidth - 2, 0)
    r_in.data  := Mux(in_addr(RegCacheIdxWidth - 1), r_mem.data, r_int.data)
    r_int_at.ren  := int_ren
    r_mem_at.ren  := mem_ren
    r_int_at.addr := in_addr(RegCacheIdxWidth - 2, 0)
    r_mem_at.addr := in_addr(RegCacheIdxWidth - 2, 0)
  }

  IntRegCache.io.writePorts.zip(io.writePorts.take(IntRegCacheWriteSize)).foreach{ case (w_int, w_in) => 
    w_int.wen  := w_in.wen
    w_int.addr := w_in.addr(RegCacheIdxWidth - 2, 0)
    w_int.data := w_in.data
    w_int.tag.foreach(_ := w_in.tag.get)
  }

  MemRegCache.io.writePorts.zip(io.writePorts.takeRight(MemRegCacheWriteSize)).foreach{ case (w_mem, w_in) => 
    w_mem.wen  := w_in.wen
    w_mem.addr := w_in.addr(RegCacheIdxWidth - 2, 0)
    w_mem.data := w_in.data
    w_mem.tag.foreach(_ := w_in.tag.get)
  }

  IntRegCacheAgeTimer.io.writePorts.zip(io.writePorts.take(IntRegCacheWriteSize)).foreach{ case (w_int, w_in) => 
    w_int.wen  := w_in.wen
    w_int.addr := w_in.addr(RegCacheIdxWidth - 2, 0)
  }

  MemRegCacheAgeTimer.io.writePorts.zip(io.writePorts.takeRight(MemRegCacheWriteSize)).foreach{ case (w_mem, w_in) => 
    w_mem.wen  := w_in.wen
    w_mem.addr := w_in.addr(RegCacheIdxWidth - 2, 0)
  }

  io.toWakeupQueueRCIdx.zipWithIndex.foreach{ case (rcIdx, i) => 
    if (i < IntRegCacheWriteSize) {
      rcIdx := Cat("b0".U, IntRegCacheRepRCIdx(i))
    }
    else {
      rcIdx := Cat("b1".U, MemRegCacheRepRCIdx(i - IntRegCacheWriteSize))
    }
  }
}

class RegCacheIO()(implicit p: Parameters, params: BackendParams) extends XSBundle {

  val readPorts = Vec(params.getIntExuRCReadSize + params.getMemExuRCReadSize, 
    new RCReadPort(params.intSchdParams.get.rfDataWidth, RegCacheIdxWidth))

  val writePorts = Vec(params.getIntExuRCWriteSize + params.getMemExuRCWriteSize, 
    new RCWritePort(params.intSchdParams.get.rfDataWidth, RegCacheIdxWidth, params.intSchdParams.get.pregIdxWidth, params.debugEn))

  val toWakeupQueueRCIdx = Vec(params.getIntExuRCWriteSize + params.getMemExuRCWriteSize, 
     Output(UInt(RegCacheIdxWidth.W)))
}
