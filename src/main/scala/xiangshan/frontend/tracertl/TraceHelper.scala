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
import chisel3.experimental.ExtModule
import chisel3.util._
import difftest.DifftestModule.createCppExtModule
import org.chipsalliance.cde.config.Parameters

class TraceReaderHelper(width: Int)(implicit p: Parameters)
    extends ExtModule
    with HasExtModuleInline {
  val clock  = IO(Input(Clock()))
  val reset  = IO(Input(Reset()))
  val enable = IO(Input(Bool()))

  private val fields             = new TraceInstrInnerBundle().elements.toSeq.reverse
  private val fieldNames         = fields.map(_._1)
  private val logicalFieldWidths = fields.map(_._2.getWidth)
  private def cppStorageWidth(width: Int): Int = width match {
    case 1            => 8
    case w if w <= 8  => 8
    case w if w <= 16 => 16
    case w if w <= 32 => 32
    case w if w <= 64 => 64
  }
  // GSIM's C++ ExtModule path does not automatically clear bits above a
  // narrow output port. Receive the complete C++ scalar and narrow it in RTL.
  private val portWidths = logicalFieldWidths.map(cppStorageWidth)
  private def field(name: String): Data = fields.find(_._1 == name).get._2
  private def outputPorts(name: String, widthBits: Int): Seq[UInt] =
    Seq.tabulate(width)(i => IO(Output(UInt(widthBits.W))).suggestName(s"insts_${i}_${name}"))

  private def rawOutputPorts(name: String): Seq[UInt] = {
    val logicalWidth = field(name).getWidth
    outputPorts(name, cppStorageWidth(logicalWidth))
  }

  val insts_pcVA           = rawOutputPorts("pcVA")
  val insts_pcPA           = rawOutputPorts("pcPA")
  val insts_memoryAddrVA   = rawOutputPorts("memoryAddrVA")
  val insts_memoryAddrPA   = rawOutputPorts("memoryAddrPA")
  val insts_target         = rawOutputPorts("target")
  val insts_inst           = rawOutputPorts("inst")
  val insts_memoryType     = rawOutputPorts("memoryType")
  val insts_memorySize     = rawOutputPorts("memorySize")
  val insts_branchType     = rawOutputPorts("branchType")
  val insts_branchTaken    = rawOutputPorts("branchTaken")
  val insts_exception      = rawOutputPorts("exception")
  val insts_fastSimulation = rawOutputPorts("fastSimulation")
  val insts_InstID         = rawOutputPorts("InstID")

  private def cppType(width: Int, isOutput: Boolean): String = {
    val base = width match {
      case 1            => "uint8_t"
      case w if w <= 8  => "uint8_t"
      case w if w <= 16 => "uint16_t"
      case w if w <= 32 => "uint32_t"
      case w if w <= 64 => "uint64_t"
    }
    s"$base${if (isOutput) "&" else ""}"
  }

  private def cppFieldParams: Seq[String] =
    fieldNames.zip(portWidths).flatMap { case (name, fieldWidth) =>
      (0 until width).map(i => f"${cppType(fieldWidth, isOutput = true)}%-10s insts_${i}_${name}")
    }

  private val cppExtModule =
    s"""
       |void TraceReaderHelper(
       |  uint8_t   reset,
       |  uint8_t   enable,
       |  ${cppFieldParams.mkString(",\n  ")}
       |) {
       |${(0 until width).map { i =>
        val args = Seq(
          s"&insts_${i}_pcVA",
          s"&insts_${i}_pcPA",
          s"&insts_${i}_memoryAddrVA",
          s"&insts_${i}_memoryAddrPA",
          s"&insts_${i}_target",
          s"&insts_${i}_inst",
          s"&insts_${i}_memoryType",
          s"&insts_${i}_memorySize",
          s"&insts_${i}_branchType",
          s"&insts_${i}_branchTaken",
          s"&insts_${i}_exception",
          s"&insts_${i}_fastSimulation",
          s"&insts_${i}_InstID",
          i.toString
        ).mkString(", ")
        val zero = fieldNames.map(name => s"    insts_${i}_${name} = 0;").mkString("\n")
        s"""  if (!reset && enable) {
            |    trace_read_one_instr($args);
            |  } else {
            |$zero
            |  }""".stripMargin
      }.mkString("\n")}
       |}
       |""".stripMargin
  createCppExtModule("TraceReaderHelper", cppExtModule, Some("\"tracertl.h\""))

  private def getVerilog: String = {
    val nameList = fieldNames
    val sizeList = portWidths

    def genElementPort(size: Int, baseName: String): String =
      (0 until width)
        .map(i => s"output [${size - 1}:0] insts_${i}_${baseName},")
        .mkString("  ", "\n  ", "\n")

    def genModulePort: String =
      s"""
         |  input  clock,
         |  input  reset,
         |${nameList.zip(sizeList).map { case (name, size) => genElementPort(size, name) }.mkString}
         |  input  enable
         |""".stripMargin

    def genLogicDeclare: String =
      (0 until width).map { idx =>
        nameList.zip(sizeList).map { case (name, size) =>
          s"logic [${size - 1}:0] logicInsts_${idx}_${name};"
        }.mkString("  ", "\n  ", "\n")
      }.mkString

    def fromLogicToIO: String =
      (0 until width).map { idx =>
        nameList.map(name => s"assign insts_${idx}_${name} = logicInsts_${idx}_${name};").mkString(
          "  ",
          "\n  ",
          "\n"
        )
      }.mkString

    def funcDeclare: String =
      s"""
         |import "DPI-C" function void trace_read_one_instr(
         |  output longint pc_va,
         |  output longint pc_pa,
         |  output longint memory_addr_va,
         |  output longint memory_addr_pa,
         |  output longint target,
         |  output int instr,
         |  output byte memory_type,
         |  output byte memory_size,
         |  output byte branch_type,
         |  output byte branch_taken,
         |  output byte exception,
         |  output byte fast_simulation,
         |  output longint InstID,
         |  input  byte idx
         |);
         |""".stripMargin

    def callDPIC(destName: String): String =
      (0 until width).map { i =>
        val assignTrace =
          s"""
             |    trace_read_one_instr(
             |      ${destName}_${i}_pcVA, ${destName}_${i}_pcPA,
             |      ${destName}_${i}_memoryAddrVA, ${destName}_${i}_memoryAddrPA,
             |      ${destName}_${i}_target, ${destName}_${i}_inst,
             |      ${destName}_${i}_memoryType, ${destName}_${i}_memorySize,
             |      ${destName}_${i}_branchType, ${destName}_${i}_branchTaken,
             |      ${destName}_${i}_exception, ${destName}_${i}_fastSimulation,
             |      ${destName}_${i}_InstID,
             |      $i);
             |""".stripMargin

        val assignDummy = nameList.map(name => s"${destName}_${i}_${name} = 0;").mkString("    ", "\n    ", "\n")

        s"""
           |always @(negedge clock) begin
           |  if (!reset && enable) begin
           |$assignTrace
           |  end else begin
           |$assignDummy
           |  end
           |end
           |""".stripMargin
      }.mkString("\n")

    s"""
       |$funcDeclare
       |
       |module TraceReaderHelper(
       |$genModulePort
       |);
       |
       |$genLogicDeclare
       |${callDPIC("logicInsts")}
       |$fromLogicToIO
       |
       |endmodule
       |""".stripMargin
  }

  setInline(s"$desiredName.sv", getVerilog)
}

class TraceRedirectHelper extends ExtModule with HasExtModuleInline {
  val clock               = IO(Input(Clock()))
  val reset               = IO(Input(Reset()))
  val enable              = IO(Input(Bool()))
  val InstID              = IO(Input(UInt(64.W)))
  val preserveDriveBefore = IO(Input(Bool()))

  private val cppExtModule =
    """
      |void TraceRedirectHelper(
      |  uint8_t  reset,
      |  uint8_t  enable,
      |  uint64_t InstID,
      |  uint8_t  preserveDriveBefore
      |) {
      |  if (enable && !reset) trace_redirect(InstID, preserveDriveBefore);
      |}
      |""".stripMargin
  createCppExtModule("TraceRedirectHelper", cppExtModule, Some("\"tracertl.h\""))

  private def getVerilog: String =
    s"""
       |import "DPI-C" function void trace_redirect(
       |  input longint InstID,
       |  input byte preserveDriveBefore
       |);
       |
       |module TraceRedirectHelper(
       |  input clock,
       |  input reset,
       |  input enable,
       |  input [63:0] InstID,
       |  input preserveDriveBefore
       |);
       |
       |  always @(negedge clock) begin
       |    if (enable && !reset) begin
       |      trace_redirect(InstID, {7'b0, preserveDriveBefore});
       |    end
       |  end
       |endmodule
       |""".stripMargin

  setInline(s"$desiredName.sv", getVerilog)
}

class TraceCollectorHelper(width: Int) extends ExtModule with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))

  val enable  = IO(Input(Vec(width, Bool())))
  val pc      = IO(Input(Vec(width, UInt(64.W))))
  val inst    = IO(Input(Vec(width, UInt(32.W))))
  val instNum = IO(Input(Vec(width, UInt(8.W))))

  private val cppExtModule = {
    def params(cppType: String, name: String): String =
      (0 until width).map(i => s"  $cppType ${name}_$i").mkString(",\n")

    val calls = (0 until width).map { i =>
      s"  if (!reset && enable_$i) trace_collect_commit(pc_$i, inst_$i, instNum_$i, $i);"
    }.mkString("\n")

    s"""
       |void TraceCollectorHelper(
       |  uint8_t reset,
       |${params("uint8_t", "enable")},
       |${params("uint64_t", "pc")},
       |${params("uint32_t", "inst")},
       |${params("uint8_t", "instNum")}
       |) {
       |$calls
       |}
       |""".stripMargin
  }
  createCppExtModule("TraceCollectorHelper", cppExtModule, Some("\"tracertl.h\""))

  private def getVerilog: String = {
    def genPort(size: Int, baseName: String): String =
      (0 until width)
        .map(i => s"input [${size - 1}:0] ${baseName}_${i},")
        .mkString("  ", "\n  ", "\n")

    def genBoolPort(baseName: String): String =
      (0 until width)
        .map(i => s"input ${baseName}_${i},")
        .mkString("  ", "\n  ", "\n")

    val callDPIC = (0 until width)
      .map(i =>
        s"""
           |    if (enable_${i}) begin
           |      trace_collect_commit(pc_${i}, inst_${i}, instNum_${i}, $i);
           |    end
           |""".stripMargin
      )
      .mkString

    s"""
       |import "DPI-C" function void trace_collect_commit(
       |  input longint pc,
       |  input int inst,
       |  input byte instNum,
       |  input byte idx
       |);
       |
       |module TraceCollectorHelper(
       |  input  clock,
       |${genBoolPort("enable")}
       |${genPort(64, "pc")}
       |${genPort(32, "inst")}
       |${genPort(8, "instNum")}
       |  input  reset
       |);
       |
       |  always @(negedge clock) begin
       |    if (!reset) begin
       |$callDPIC
       |    end
       |  end
       |endmodule
       |""".stripMargin
  }

  setInline(s"$desiredName.sv", getVerilog)
}

class TraceDriveCollectorHelper(width: Int) extends ExtModule with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))

  val enable = IO(Input(Vec(width, Bool())))
  val pc     = IO(Input(Vec(width, UInt(64.W))))
  val inst   = IO(Input(Vec(width, UInt(32.W))))

  private val cppExtModule = {
    def params(cppType: String, name: String): String =
      (0 until width).map(i => s"  $cppType ${name}_$i").mkString(",\n")

    val calls = (0 until width).map { i =>
      s"  if (!reset && enable_$i) trace_collect_drive(pc_$i, inst_$i, $i);"
    }.mkString("\n")

    s"""
       |void TraceDriveCollectorHelper(
       |  uint8_t reset,
       |${params("uint8_t", "enable")},
       |${params("uint64_t", "pc")},
       |${params("uint32_t", "inst")}
       |) {
       |$calls
       |}
       |""".stripMargin
  }
  createCppExtModule("TraceDriveCollectorHelper", cppExtModule, Some("\"tracertl.h\""))

  private def getVerilog: String = {
    def genPort(size: Int, baseName: String): String =
      (0 until width)
        .map(i => s"input [${size - 1}:0] ${baseName}_${i},")
        .mkString("  ", "\n  ", "\n")

    def genBoolPort(baseName: String): String =
      (0 until width)
        .map(i => s"input ${baseName}_${i},")
        .mkString("  ", "\n  ", "\n")

    val callDPIC = (0 until width)
      .map(i =>
        s"""
           |    if (enable_${i}) begin
           |      trace_collect_drive(pc_${i}, inst_${i}, $i);
           |    end
           |""".stripMargin
      )
      .mkString

    s"""
       |import "DPI-C" function void trace_collect_drive(
       |  input longint pc,
       |  input int inst,
       |  input byte idx
       |);
       |
       |module TraceDriveCollectorHelper(
       |  input  clock,
       |${genBoolPort("enable")}
       |${genPort(64, "pc")}
       |${genPort(32, "inst")}
       |  input  reset
       |);
       |
       |  always @(negedge clock) begin
       |    if (!reset) begin
       |$callDPIC
       |    end
       |  end
       |endmodule
       |""".stripMargin
  }

  setInline(s"$desiredName.sv", getVerilog)
}

class TraceATSHelper extends ExtModule with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val valid = IO(Input(Bool()))
  val asid  = IO(Input(UInt(16.W)))
  val vmid  = IO(Input(UInt(16.W)))
  val vaddr = IO(Input(UInt(64.W)))
  val paddr = IO(Output(UInt(64.W)))
  val hit   = IO(Output(Bool()))

  private val cppExtModule =
    """
      |void TraceATSHelper(
      |  uint8_t   reset,
      |  uint8_t   valid,
      |  uint16_t  asid,
      |  uint16_t  vmid,
      |  uint64_t  vaddr,
      |  uint64_t& paddr,
      |  uint8_t&  hit
      |) {
      |  if (!reset && valid) {
      |    hit = trace_tlb_ats_hit(vaddr, asid, vmid);
      |    paddr = trace_tlb_ats(vaddr, asid, vmid);
      |  } else {
      |    hit = 0;
      |    paddr = 0;
      |  }
      |}
      |""".stripMargin
  createCppExtModule("TraceATSHelper", cppExtModule, Some("\"tracertl.h\""))

  private def getVerilog: String =
    s"""
       |import "DPI-C" function longint trace_tlb_ats(
       |  input  longint vaddr,
       |  input  shortint asid,
       |  input  shortint vmid
       |);
       |import "DPI-C" function byte trace_tlb_ats_hit(
       |  input  longint vaddr,
       |  input  shortint asid,
       |  input  shortint vmid
       |);
       |
       |module TraceATSHelper(
       |  input              clock,
       |  input              reset,
       |  input              valid,
       |  input       [15:0] asid,
       |  input       [15:0] vmid,
       |  input       [63:0] vaddr,
       |  output      [63:0] paddr,
       |  output             hit
       |);
       |
       |  logic [63:0] logic_paddr;
       |  logic        logic_hit;
       |
       |  always_comb begin
       |    if (!reset && valid) begin
       |      logic_hit   = trace_tlb_ats_hit(vaddr, asid, vmid);
       |      logic_paddr = trace_tlb_ats(vaddr, asid, vmid);
       |    end else begin
       |      logic_hit   = 0;
       |      logic_paddr = 0;
       |    end
       |  end
       |
       |  assign paddr = logic_paddr;
       |  assign hit   = logic_hit;
       |endmodule
       |""".stripMargin

  setInline(s"$desiredName.sv", getVerilog)
}

class TraceFakeMMU(implicit p: Parameters) extends TraceModule {
  val io = IO(new TraceATSBundle)

  val helper = Module(new TraceATSHelper)
  helper.clock := clock
  helper.reset := reset
  helper.valid := io.valid
  helper.vaddr := io.vaddr
  helper.asid  := 0.U
  helper.vmid  := 0.U

  io.paddr := RegEnable(helper.paddr, io.valid)
}

class TraceSatpPpnHelper extends ExtModule with HasExtModuleInline {
  val clock  = IO(Input(Clock()))
  val reset  = IO(Input(Reset()))
  val enable = IO(Input(Bool()))
  val ppn    = IO(Output(UInt(44.W)))

  private val cppExtModule =
    """
      |void TraceSatpPpnHelper(
      |  uint8_t   reset,
      |  uint8_t   enable,
      |  uint64_t& ppn
      |) {
      |  if (!reset && enable) {
      |    ppn = trace_get_satp_ppn() & ((1ULL << 44) - 1);
      |  } else {
      |    ppn = 0;
      |  }
      |}
      |""".stripMargin
  createCppExtModule("TraceSatpPpnHelper", cppExtModule, Some("\"tracertl.h\""))

  private def getVerilog: String =
    s"""
       |import "DPI-C" function longint trace_get_satp_ppn();
       |module TraceSatpPpnHelper(
       |  input clock,
       |  input reset,
       |  input enable,
       |  output [43:0] ppn
       |);
       |assign ppn = (!reset && enable) ? trace_get_satp_ppn() : 44'b0;
       |endmodule
       |""".stripMargin

  setInline(s"$desiredName.sv", getVerilog)
}

class TraceFakeSatpPpn(implicit p: Parameters) extends TraceModule {
  val io = IO(new Bundle {
    val ppn = Output(UInt(44.W))
  })

  private val helper = Module(new TraceSatpPpnHelper)
  helper.clock := clock
  helper.reset := reset
  val startCount = RegInit(0.U(4.W))
  when(startCount < 10.U) {
    startCount := startCount + 1.U
  }
  val workingState = startCount >= 5.U
  helper.enable := workingState
  io.ppn        := RegEnable(helper.ppn, 0.U, RegNext(workingState))
}

class TraceFakePageTableRespBundle(implicit p: Parameters) extends TraceBundle {
  val data = Vec(2, UInt(256.W))
  val addr = UInt(PAddrBits.W)
}

class TraceDynPageTableHelper extends ExtModule with HasExtModuleInline {
  val clock  = IO(Input(Clock()))
  val reset  = IO(Input(Reset()))
  val enable = IO(Input(Bool()))
  val addr   = IO(Input(UInt(64.W)))
  // GSIM does not support array ports on external modules. Keep these as
  // scalar ports and reconstruct the vector in the enclosing Chisel module.
  val data_0 = IO(Output(UInt(64.W)))
  val data_1 = IO(Output(UInt(64.W)))
  val data_2 = IO(Output(UInt(64.W)))
  val data_3 = IO(Output(UInt(64.W)))
  val data_4 = IO(Output(UInt(64.W)))
  val data_5 = IO(Output(UInt(64.W)))
  val data_6 = IO(Output(UInt(64.W)))
  val data_7 = IO(Output(UInt(64.W)))

  private val cppExtModule = {
    val dataArgs = (0 until 8).map(i => s"  uint64_t& data_$i").mkString(",\n")
    val readData = (0 until 8)
      .map(i => s"    data_$i = trace_dyn_pt_dword_helper(addrAlign + ${i * 8});")
      .mkString("\n")
    val clearData = (0 until 8).map(i => s"    data_$i = 0;").mkString("\n")

    s"""
       |void TraceDynPageTableHelper(
       |  uint8_t  reset,
       |  uint8_t  enable,
       |  uint64_t addr,
       |$dataArgs
       |) {
       |  if (!reset && enable) {
       |    const uint64_t addrAlign = addr & ~0x1fULL;
       |$readData
       |  } else {
       |$clearData
       |  }
       |}
       |""".stripMargin
  }
  createCppExtModule("TraceDynPageTableHelper", cppExtModule, Some("\"tracertl.h\""))

  private def getVerilog: String = {
    val dataPorts = (0 until 8).map(i => s"output [63:0] data_$i,").mkString("  ", "\n  ", "\n")
    val readData = (0 until 8)
      .map(i => s"assign data_$i = (!reset && enable) ? trace_dyn_pt_dword_helper(addr_align + ${i * 8}) : 64'b0;")
      .mkString("  ", "\n  ", "\n")

    s"""
       |import "DPI-C" function longint trace_dyn_pt_dword_helper(input longint addr);
       |
       |module TraceDynPageTableHelper(
       |  input             clock,
       |  input             reset,
       |  input             enable,
       |$dataPorts
       |  input      [63:0] addr
       |);
       |  wire [63:0] addr_align = addr & 64'hffffffffffffffe0;
       |
       |$readData
       |endmodule
       |""".stripMargin
  }

  setInline(s"$desiredName.sv", getVerilog)
}

class TraceFakeDynPageTable(implicit p: Parameters) extends TraceModule {
  val io = IO(new Bundle {
    val req = Flipped(ValidIO(new Bundle {
      val paddr = UInt(PAddrBits.W)
    }))
    val resp = Valid(new TraceFakePageTableRespBundle)
  })

  private val helper = Module(new TraceDynPageTableHelper)
  helper.clock  := clock
  helper.reset  := reset
  helper.enable := io.req.valid
  helper.addr   := io.req.bits.paddr

  io.resp.valid := RegNext(io.req.valid)
  val helperData = Seq(
    helper.data_0,
    helper.data_1,
    helper.data_2,
    helper.data_3,
    helper.data_4,
    helper.data_5,
    helper.data_6,
    helper.data_7
  )
  val respData = helperData.map(RegEnable(_, io.req.valid))
  io.resp.bits.data(0) := Cat(respData(3), respData(2), respData(1), respData(0))
  io.resp.bits.data(1) := Cat(respData(7), respData(6), respData(5), respData(4))
  io.resp.bits.addr    := RegEnable(helper.addr, io.req.valid)
}
