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
package xiangshan.tracertl

import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._
import difftest.DifftestModule.createCppExtModule
import org.chipsalliance.cde.config.Parameters

/** C and SystemVerilog types of a DPI scalar that carries `width` bits. */
private object TraceDpiType {
  def storageWidth(width: Int): Int = width match {
    case w if w <= 8  => 8
    case w if w <= 16 => 16
    case w if w <= 32 => 32
    case w if w <= 64 => 64
  }
  def cpp(width: Int): String = s"uint${storageWidth(width)}_t"
  def sv(width: Int): String = storageWidth(width) match {
    case 8  => "byte"
    case 16 => "shortint"
    case 32 => "int"
    case 64 => "longint"
  }
}

/** Reads `width` trace instructions per enabled cycle, one output port per TraceInstrBundle field and lane. */
class TraceReaderHelper(width: Int)(implicit p: Parameters)
    extends ExtModule
    with HasExtModuleInline {
  val clock  = IO(Input(Clock()))
  val reset  = IO(Input(Reset()))
  val enable = IO(Input(Bool()))

  // Field order is the argument order of trace_read_one_instr.
  private val fields = new TraceInstrBundle().elements.toSeq.reverse.map { case (name, data) => name -> data.getWidth }
  // GSIM's C++ ExtModule path does not automatically clear bits above a
  // narrow output port. Receive the complete C++ scalar and narrow it in RTL.
  private val portWidths = fields.map { case (name, w) => name -> TraceDpiType.storageWidth(w) }

  /** Output ports by field name, one per lane. */
  val insts: Map[String, Seq[UInt]] = portWidths.map { case (name, w) =>
    name -> Seq.tabulate(width)(i => IO(Output(UInt(w.W))).suggestName(s"insts_${i}_${name}"))
  }.toMap

  private val cppExtModule = {
    val params = portWidths.flatMap { case (name, w) =>
      (0 until width).map(i => f"${TraceDpiType.cpp(w) + "&"}%-10s insts_${i}_${name}")
    }
    val calls = (0 until width).map { i =>
      val args = fields.map { case (name, _) => s"&insts_${i}_${name}" } :+ i.toString
      val zero = fields.map { case (name, _) => s"    insts_${i}_${name} = 0;" }.mkString("\n")
      s"""  if (!reset && enable) {
         |    trace_read_one_instr(${args.mkString(", ")});
         |  } else {
         |$zero
         |  }""".stripMargin
    }
    s"""
       |void TraceReaderHelper(
       |  uint8_t   reset,
       |  uint8_t   enable,
       |  ${params.mkString(",\n  ")}
       |) {
       |${calls.mkString("\n")}
       |}
       |""".stripMargin
  }
  createCppExtModule("TraceReaderHelper", cppExtModule, Some("\"tracertl.h\""))

  private def getVerilog: String = {
    // DPI outputs are written in an always block, so go through logic variables.
    def port(i:  Int, name: String) = s"insts_${i}_${name}"
    def logic(i: Int, name: String) = s"logicInsts_${i}_${name}"
    val lanes = (0 until width).map(i => portWidths.map { case (name, w) => (i, name, w) })
    val calls = (0 until width).map { i =>
      val args = fields.map { case (name, _) => logic(i, name) } :+ i.toString
      val zero = fields.map { case (name, _) => s"    ${logic(i, name)} = 0;" }.mkString("\n")
      s"""always @(negedge clock) begin
         |  if (!reset && enable) begin
         |    trace_read_one_instr(${args.mkString(", ")});
         |  end else begin
         |$zero
         |  end
         |end
         |""".stripMargin
    }
    val dpiArgs = portWidths.map { case (name, w) => s"  output ${TraceDpiType.sv(w)} $name," } :+ "  input  byte idx"
    // field-major, like the Chisel ports
    val ports = portWidths.flatMap { case (name, w) =>
      (0 until width).map(i => s"  output [${w - 1}:0] ${port(i, name)},")
    }
    val logics  = lanes.flatten.map { case (i, name, w) => s"  logic [${w - 1}:0] ${logic(i, name)};" }
    val assigns = lanes.flatten.map { case (i, name, _) => s"  assign ${port(i, name)} = ${logic(i, name)};" }

    s"""
       |import "DPI-C" function void trace_read_one_instr(
       |${dpiArgs.mkString("\n")}
       |);
       |
       |module TraceReaderHelper(
       |  input  clock,
       |  input  reset,
       |${ports.mkString("\n")}
       |  input  enable
       |);
       |
       |${logics.mkString("\n")}
       |${calls.mkString("\n")}
       |${assigns.mkString("\n")}
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

/** Calls `func(<args>..., idx)` at the negedge for every enabled lane. */
abstract class TraceCollectHelperBase(width: Int, func: String, args: Seq[(String, Int)])
    extends ExtModule
    with HasExtModuleInline {
  val clock  = IO(Input(Clock()))
  val reset  = IO(Input(Reset()))
  val enable = IO(Input(Vec(width, Bool())))

  /** The per-lane input port of an argument; declare them in `args` order. */
  protected def arg(name: String): Vec[UInt] = IO(Input(Vec(width, UInt(args.toMap.apply(name).W))))

  private def callArgs(i: Int) = (args.map { case (name, _) => s"${name}_$i" } :+ i.toString).mkString(", ")

  private val cppExtModule = {
    val params = ("enable", 1) +: args
    s"""
       |void $desiredName(
       |  uint8_t reset,
       |${params.flatMap { case (name, w) =>
        (0 until width).map(i => s"  ${TraceDpiType.cpp(w)} ${name}_$i")
      }.mkString(",\n")}
       |) {
       |${(0 until width).map(i => s"  if (!reset && enable_$i) $func(${callArgs(i)});").mkString("\n")}
       |}
       |""".stripMargin
  }
  createCppExtModule(desiredName, cppExtModule, Some("\"tracertl.h\""))

  private def getVerilog: String = {
    val ports = (0 until width).map(i => s"  input enable_$i,") ++
      args.flatMap { case (name, w) => (0 until width).map(i => s"  input [${w - 1}:0] ${name}_$i,") }
    val calls = (0 until width).map { i =>
      s"""    if (enable_$i) begin
         |      $func(${callArgs(i)});
         |    end""".stripMargin
    }
    val dpiArgs = args.map { case (name, w) => s"  input ${TraceDpiType.sv(w)} $name," } :+ "  input byte idx"

    s"""
       |import "DPI-C" function void $func(
       |${dpiArgs.mkString("\n")}
       |);
       |
       |module $desiredName(
       |  input  clock,
       |${ports.mkString("\n")}
       |  input  reset
       |);
       |
       |  always @(negedge clock) begin
       |    if (!reset) begin
       |${calls.mkString("\n")}
       |    end
       |  end
       |endmodule
       |""".stripMargin
  }

  setInline(s"$desiredName.sv", getVerilog)
}

/** Reports committed instructions from the ROB. */
class TraceCollectorHelper(width: Int)
    extends TraceCollectHelperBase(width, "trace_collect_commit", Seq("pc" -> 64, "inst" -> 32, "instNum" -> 8)) {
  val pc      = arg("pc")
  val inst    = arg("inst")
  val instNum = arg("instNum")
}

/** Reports instructions the IBuffer hands to decode. */
class TraceDriveCollectorHelper(width: Int)
    extends TraceCollectHelperBase(width, "trace_collect_drive", Seq("pc" -> 64, "inst" -> 32)) {
  val pc   = arg("pc")
  val inst = arg("inst")
}

class TraceATSBundle(implicit p: Parameters) extends TraceBundle {
  val valid = Input(Bool())
  val vaddr = Input(UInt(64.W))
  val paddr = Output(UInt(64.W))
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
  val workingState = TraceWarmUp()
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
  val data = Seq.tabulate(8)(i => IO(Output(UInt(64.W))).suggestName(s"data_$i"))

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
  val respData = helper.data.map(RegEnable(_, io.req.valid))
  io.resp.bits.data(0) := Cat(respData(3), respData(2), respData(1), respData(0))
  io.resp.bits.data(1) := Cat(respData(7), respData(6), respData(5), respData(4))
  io.resp.bits.addr    := RegEnable(helper.addr, io.req.valid)
}
