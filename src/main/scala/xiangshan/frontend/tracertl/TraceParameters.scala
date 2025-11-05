/** *************************************************************************************
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
 * ************************************************************************************* */
package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.collection.mutable.ListBuffer

case object TraceRTLParamKey extends Field[TraceRTLParameters]

case class TraceRTLParameters
(
  TraceRTLOnPLDM: Boolean = false, // for pldm
  TraceRTLOnFPGA: Boolean = false, // for fpga

  TraceFetchWidth: Int = 16,
  TraceEnableDuplicateFlush: Boolean = true,
  TraceEnableWrongPathEmu: Boolean = true,
  TraceWrongPathEmuWhenConvergence: Boolean = true,
  TraceSoftL1TLB: Boolean = false,
  TraceSoftL1TLBCheck: Boolean = false,
  TraceDummyFixCycleDivSqrt: Boolean = false,
  TraceDummyFixCycleIntDiv: Boolean = false,
  TraceEliminateArthi: Boolean = true,
  TraceEliminateMemory: Boolean = true,
  TraceOverrideTarget: Boolean = true,
  Trace2StageMMU: Boolean = false,

  TraceFpgaHugeBufferSize: Int = 1 * 1024, // 1K
  // TraceFpgaUnpackInstNum: Int = 128, // 16 * 8
  TraceFpgaUnpackInstNum: Int = 1 * 16, // 16 * 8
  TraceFpgaAxisWidth: Int = 512,
  TraceFpgaCollectWidth: Int = 128,
) {
  def TraceBufferSize = TraceFetchWidth * 4
  def TraceFpgaRecvWidth = TraceFetchWidth // set it same to TraceFetchWidth now
}

object TraceRTLParameters {
  def generateCppHeader(implicit p: Parameters): Unit = {
    val trtl = p(TraceRTLParamKey)
    val tOutBundle = new TraceInstrOuterBundle
    val tCollectBundle = new TraceFPGACollectBundle

    val result = ListBuffer.empty[String]
    result += "#ifndef __TRACERTL_DUT_INFO_H__"
    result += "#define __TRACERTL_DUT_INFO_H__"

    // FPGA
    result +=
      s"""
         |#define TRACERTL_FPGA_AXIS_WIDTH ${trtl.TraceFpgaAxisWidth}
         |#define TRACERTL_FPGA_PACKET_INST_NUM ${trtl.TraceFpgaUnpackInstNum}
         |#define TRACERTL_FPGA_COLLECT_INST_NUM ${trtl.TraceFpgaCollectWidth}
         |#define TRACERTL_FPGA_COLLECT_INST_WIDTH ${tCollectBundle.getWidth}
         |""".stripMargin

    // TraceFormat InstructionBundle
    result += s"#define TRACERTL_INST_BIT_WIDTH ${tOutBundle.getWidth}"
    tOutBundle.elements.foreach { case (name, elt) =>
      result += s"#define TRACERTL_INST_${name}_BIT_WIDTH"
    }

    result += "#endif // __TRACERTL_DUT_INFO_H__"

    streamToFile(result, "tracertl_dut_info.h", append=false)
  }

  // copy from DifftestModule
  def streamToFile(fileStream: ListBuffer[String], fileName: String, append: Boolean = false): Unit = {
    val outputDir = sys.env("NOOP_HOME") + "/build/generated-src/"
    Files.createDirectories(Paths.get(outputDir))
    val outputFile = outputDir + fileName
    val options = if (append) {
      Seq(StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    } else {
      Seq(StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    }
    Files.write(
      Paths.get(outputFile),
      (fileStream.mkString("\n") + "\n").getBytes(StandardCharsets.UTF_8),
      options: _*
    )
  }
}