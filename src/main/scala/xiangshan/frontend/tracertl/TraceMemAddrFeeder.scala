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
import chisel3.util.experimental.BoringUtils
import chisel3.experimental.ExtModule
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.mem.{L1PrefetchReq, HasL1PrefetchSourceParameter}
import xiangshan.mem.prefetch.{HasL1PrefetchHelper, StreamPrefetchReqBundle, MLPReqFilterBundle}
import utility.CircularQueuePtr
import difftest.trace.Trace
import xiangshan.backend.datapath.DataConfig.VAddrBits
import xiangshan.backend.fu.NewCSR.CSRConfig.PAddrWidth


// TODO: change BoringUtils.addSink to BoringUtils.tapAndRead
class TraceMAFeaderPtr(Size: Int)(implicit p: Parameters) extends CircularQueuePtr[TraceMAFeaderPtr](Size)

class TraceMemAddrBundle(implicit p: Parameters) extends TraceBundle {
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
}

class TraceMemAddrIO(implicit p: Parameters) extends TraceBundle {
  val req = DecoupledIO(new L1PrefetchReq)
}

class TraceMemAddrFeeder(implicit p: Parameters) extends TraceModule with HasL1PrefetchHelper {
  val ReqQueueSize = 12
  val ReqWriteWidth = 4

  val io = IO(new TraceMemAddrIO)

  // val block = WireInit(false.B)
  // BoringUtils.addSink(block, "MSHRFullBlockTracePrf")
  val block = TraceBoringUtils.addSink("MSHRFullBlockTracePrf")

  val reqQue = Reg(Vec(ReqQueueSize, new TraceMemAddrBundle))
  val validVec = RegInit(VecInit(Seq.fill(ReqQueueSize)(false.B)))
  val readPtr = RegInit(0.U.asTypeOf(new TraceMAFeaderPtr(ReqQueueSize)))
  val writePtr = RegInit(0.U.asTypeOf(new TraceMAFeaderPtr(ReqQueueSize)))

  // read from reqQue to io.req
  io.req.valid := validVec(readPtr.value) && !block
  io.req.bits := fromAddrToL1PrefetchReq(reqQue(readPtr.value))
  when (io.req.fire) {
    readPtr := readPtr + 1.U
    validVec(readPtr.value) := false.B
  }

  // read from dpi-c to reqQue
  val notFull = PopCount(validVec) <= 8.U
  // set finished init to false.
  // when fastSim fisnished or not enabled, dpic will return 0, then finished will be set to true
  val finished = RegInit(false.B)
  // TraceBoringUtils.addSource(finished, "TraceMemAddrFeederFinished")
  BoringUtils.addSource(finished, "TraceMemAddrFeederFinished")

  val helper = Module(new TraceMemAddrFeederHelper(ReqWriteWidth))
  helper.clock := clock
  helper.reset := reset
  helper.enable := notFull && !finished
  when (helper.enable) {
    finished := !(0 until ReqWriteWidth).map(i => helper.valid(i)(0)).reduce(_ && _)
    writePtr := writePtr + ReqWriteWidth.U
    for (i <- 0 until ReqWriteWidth) {
      when (helper.valid(i)) {
        reqQue(writePtr.value + i.U).paddr := helper.paddr(i)(PAddrBits - 1, 0)
        reqQue(writePtr.value + i.U).vaddr := helper.vaddr(i)(VAddrBits - 1, 0)
        validVec(writePtr.value + i.U) := helper.valid(i)(0)
      }
    }
  }

  def fromAddrToL1PrefetchReq(m: TraceMemAddrBundle): L1PrefetchReq = {
    val req = Wire(new L1PrefetchReq)
    req.paddr := m.paddr // paddr's low 12 bits are 0, handled by DPI-C
    req.alias := get_region_tag(m.vaddr)(PAGE_OFFSET - REGION_TAG_OFFSET + 1, PAGE_OFFSET - REGION_TAG_OFFSET)
    req.confidence := 1.U // high?
    req.is_store := false.B
    req.pf_source.value := L1_HW_PREFETCH_TRACE

    req
  }
}

class TraceMemAddrFeederHelper(width: Int)(implicit p: Parameters)
  extends ExtModule
  with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))

  val enable = IO(Input(Bool()))
  val valid = IO(Output(Vec(width, Bool())))
  val vaddr = IO(Output(Vec(width, UInt(64.W))))
  val paddr = IO(Output(Vec(width, UInt(64.W))))

  def genModulePort: String = {
    def genElementPort(size: Int, baseName: String): String = {
      val sizeStr = if (size == 1) "" else s"[${size - 1}:0]"
      (0 until width)
        .map(i => s"output ${sizeStr} ${baseName}_${i},")
        .mkString("  ", "\n  ", "\n")
    }
    s"""
       |  input clock,
       |  input reset,
       |
       |
       |${genElementPort(64, "vaddr")}
       |${genElementPort(64, "paddr")}
       |${genElementPort(1,  "valid")}
       |  input enable
       |""".stripMargin
  }

  def genLogicDeclare: String = {
    (0 until width).map { case idx =>
        s"  logic [63:0] logic_vaddr_${idx};\n" +
        s"  logic [63:0] logic_paddr_${idx};\n" +
        s"  logic logic_valid_${idx};\n"
    }.mkString
  }

  def genFromLogicToIO: String = {
    (0 until width).map { case idx =>
      s"  assign vaddr_${idx} = logic_vaddr_${idx};\n" +
      s"  assign paddr_${idx} = logic_paddr_${idx};\n" +
      s"  assign valid_${idx} = logic_valid_${idx};\n"
    }.mkString
  }

  def genFuncDeclare: String = {
    s"""
       |import "DPI-C" function void trace_fastsim_mem_addr_reader(
       |  input byte idx,
       |  output byte valid,
       |  output longint vaddr,
       |  output longint paddr
       |);
       """.stripMargin
  }

  def genCallFunc: String = {
    def assignTrace: String = {
      (0 until width).map { case idx =>
        s"trace_fastsim_mem_addr_reader(${idx}, logic_valid_${idx}, logic_vaddr_${idx}, logic_paddr_${idx});"
      }.mkString("", "\n", "\n")
    }
    def assignInvalid: String = {
      (0 until width).map { case idx =>
        s"logic_valid_${idx} = 0;\n" +
        s"logic_vaddr_${idx} = 0;\n" +
        s"logic_paddr_${idx} = 0;\n"
      }.mkString("", "\n", "\n")
    }
    s"""
       |always @(negedge clock) begin
       |  if (!reset && enable) begin
       |    ${assignTrace}
       |  end
       |  else begin
       |    ${assignInvalid}
       |  end
       |end
       """.stripMargin
  }

  // TODO: read from dpi-c
  def getVerilog: String = {
    s"""
       |$genFuncDeclare
       |
       |module TraceMemAddrFeederHelper(
       |$genModulePort
       |);
       |
       |$genLogicDeclare
       |$genCallFunc
       |$genFromLogicToIO
       |
       |endmodule
       |
       |""".stripMargin
  }

  setInline(s"$desiredName.sv", getVerilog)
}