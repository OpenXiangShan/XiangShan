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

  TraceFpgaHugeBufferSize: Int = 1 * 128, // 128 --> 1024
  TraceFpgaHugeBufferWriteWidth: Int = 16, // 1K
  TraceFpgaHugeBufferReadWidth: Int = 16, // 1K
  // TraceFpgaUnpackInstNum: Int = 128, // 16 * 8
  TraceFpgaUnpackInstNum: Int = 1 * 16, // 16 * 8
  TraceFpgaAxisWidth: Int = 512,
  TraceFpgaCollectWidth: Int = 128,

  TraceFpgaSmallBufferSize: Int = 600, // TODO

  // TraceInstrFpgaBundleSize
  TraceVAddrWidth: Int = 50,
  TracePAddrWidth: Int = 48,
  TraceInstCodeWidth: Int = 32,
  TraceInstIDWidth: Int = 64,
  TraceFastSimWidth: Int = 1,
  TraceException: Int = 8,
  TraceBranchTaken: Int = 1,
) {
  require(TraceFpgaHugeBufferWriteWidth == TraceFetchWidth, "They are different in the future")
  require(TraceFpgaHugeBufferReadWidth == TraceFetchWidth, "They are different in the future")

  def TraceBufferSize = TraceFetchWidth * 4
  def TraceFpgaRecvWidth = TraceFetchWidth // set it same to TraceFetchWidth now
}

object TraceRTLParameters {
  def bitLenToType(bitLen: Int): String = {
    require(bitLen > 0 && bitLen <= 64, "bitLen should be in 1-64")
    if (bitLen <= 8) "uint8_t"
    else if (bitLen <= 16) "uint16_t"
    else if (bitLen <= 32) "uint32_t"
    else "uint64_t"
  }

  def bitLenToPrintType(bitLen: Int): String = {
    require(bitLen > 0 && bitLen <= 64, "bitLen should be in 1-64")
    if (bitLen <= 8) "%02x"
    else if (bitLen <= 16) "%04x"
    else if (bitLen <= 32) "%08x"
    else "%016lx"
  }

  def generateCppHeader(implicit p: Parameters): Unit = {
    val trtl = p(TraceRTLParamKey)
    val tOutBundle = new TraceInstrFpgaBundle
    val tCollectBundle = new TraceFPGACollectBundle

    val TRACERTL_FPGA_PACKET_CYCLE_NUM = (tOutBundle.getWidth * trtl.TraceFpgaUnpackInstNum +
      trtl.TraceFpgaAxisWidth - 1) / trtl.TraceFpgaAxisWidth
    val TRACERTL_FPGA_COLLECT_CYCLE_NUM = (tCollectBundle.getWidth * trtl.TraceFpgaCollectWidth +
      trtl.TraceFpgaAxisWidth - 1) / trtl.TraceFpgaAxisWidth

    val result = ListBuffer.empty[String]
    result += "#ifndef __TRACERTL_DUT_INFO_H__"
    result += "#define __TRACERTL_DUT_INFO_H__"

    // FPGA
    result +=
      s"""
         |#define TRACERTL_FPGA_AXIS_WIDTH (${trtl.TraceFpgaAxisWidth})
         |#define TRACERTL_FPGA_PACKET_INST_NUM (${trtl.TraceFpgaUnpackInstNum})
         |#define TRACERTL_FPGA_PACKET_CYCLE_NUM (${TRACERTL_FPGA_PACKET_CYCLE_NUM})
         |
         |#define TRACERTL_FPGA_COLLECT_INST_WIDTH (${tCollectBundle.getWidth})
         |#define TRACERTL_FPGA_COLLECT_INST_NUM (${trtl.TraceFpgaCollectWidth})
         |#define TRACERTL_FPGA_COLLECT_CYCLE_NUM (${TRACERTL_FPGA_COLLECT_CYCLE_NUM})
         |
         |#define TRACERTL_INST_BIT_WIDTH (${tOutBundle.getWidth})
         |#define TRACERTL_FPGA_INST_BIT_ALIGN_WIDTH (${TraceInstrFpgaBundle.alignWidth()})
         |#define TRACERTL_FPGA_INST_BYTE_WIDTH (${TraceInstrFpgaBundle.alignWidth() / 8})
         |""".stripMargin

    // TraceFormat InstructionBundle
    tOutBundle.elements.foreach { case (name, elt) =>
      result += s"#define TRACERTL_INST_${name}_BIT_WIDTH (${elt.getWidth})"
    }

    // TraceFormat Struct
    result += s"struct __attribute__((packed)) TraceFpgaInstruction {"
    tOutBundle.elements.foreach { case (name, elt) =>
      result += s"  ${bitLenToType(elt.getWidth)} ${name}:${elt.getWidth};"
    }
    result +=
      s"""
         |  void genFrom(Instruction &instruction) {
         |    pcVA = instruction.instr_pc_va;
         |    pcPAWoOff = instruction.instr_pc_pa >> 12;
         |    inst = instruction.instr;
         |
         |    if (instruction.branch_type != 0) traceType = ${TraceInstType.Branch.litValue};
         |    else if (instruction.memory_type != 0) {
         |      if (instruction.memory_type == 1 || instruction.memory_type == 4)
         |        traceType = ${TraceInstType.Load.litValue};
         |      else
         |        traceType = ${TraceInstType.Store.litValue};
         |    } else if (instruction.isTrap()) traceType = ${TraceInstType.Exception.litValue};
         |    else traceType = ${TraceInstType.Compute.litValue};
         |
         |    if (traceType == ${TraceInstType.Branch.litValue}) {
         |      op1 = instruction.target;
         |      op2 = instruction.branch_taken & 1;
         |    } else if (traceType == ${TraceInstType.Load.litValue} || traceType == ${TraceInstType.Store.litValue}) {
         |      op1 = instruction.exu_data.memory_address.va;
         |      op2 = instruction.exu_data.memory_address.pa >> 12;
         |    } else if (traceType == ${TraceInstType.Exception.litValue}) {
         |      op1 = instruction.target;
         |      op2 = instruction.exception;
         |    }
         |  }
         """.stripMargin

    val dump_string_pre = tOutBundle.elements.toSeq.reverse.map { case (name, elt) => s"${name} ${bitLenToPrintType(elt.getWidth)}" }.mkString(" ")
    val dump_string_sur = tOutBundle.elements.toSeq.reverse.map { case (name, elt) => s"${name}" }.mkString(", ")
    result +=
      s"""
         |  void dump(uint64_t instID) {
         |    printf("[%lx] ${dump_string_pre}\\n", instID, ${dump_string_sur});
         |  }
         """.stripMargin

    result += s"};"

    // Trace Collect Module
    result +=
      s"""
         |#define TraceFpgaCollect_BIT_WIDTH (${tCollectBundle.getWidth})
         |#define TraceFpgaCollect_BIT_ALIGN_WIDTH (${TraceFPGACollectBundle.alignWidth()})
         |#define TraceFpgaCollect_BYTE_WIDTH (${TraceFPGACollectBundle.alignWidth() / 8})
         |#define TraceFpgaCollect_PCVA_WIDTH ${tCollectBundle.pcVA.getWidth}
         |#define TraceFpgaCollect_INSTNUM_WIDTH ${tCollectBundle.instNum.getWidth}
         |
         |struct __attribute__((packed)) TraceFpgaCollectStruct {
         |  uint64_t pcVA:${tCollectBundle.pcVA.getWidth};
         |  uint8_t instNum:${tCollectBundle.instNum.getWidth};
         |
         |  void dump() {
         |    printf("pcVA: %lx, instNum: %d\\n", pcVA, instNum);
         |  }
         |};
         |
         """.stripMargin


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