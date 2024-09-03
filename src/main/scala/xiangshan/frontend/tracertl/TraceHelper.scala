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
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.Annotated.resetVector

class TraceInstrInnerBundle(implicit p: Parameters) extends TraceBundle {
  val pcVA = UInt(VAddrBits.W)
  val pcPA = UInt(PAddrBits.W)
  val memoryAddrVA = UInt(VAddrBits.W)
  val memoryAddrPA = UInt(PAddrBits.W)
  val target = UInt(VAddrBits.W)
  val inst = UInt(TraceInstrWidth.W)
  val memoryType = UInt(8.W)
  val memorySize = UInt(8.W)
  val branchType = UInt(8.W)
  val branchTaken = UInt(8.W)
  val exception = UInt(8.W)

  val InstID = UInt(64.W)
}


object TraceInstrInnerBundle {
  def apply(pcVA: UInt, pcPA: UInt, memoryAddrVA: UInt, memoryAddrPA: UInt,
    target: UInt, inst: UInt, memoryType: UInt, memorySize: UInt,
    branchType: UInt, branchTaken: UInt,
    InstID: UInt)(implicit p: Parameters): TraceInstrInnerBundle = {

    val bundle = Wire(new TraceInstrInnerBundle)
    bundle.pcVA := pcVA
    bundle.pcPA := pcPA
    bundle.memoryAddrVA := memoryAddrVA
    bundle.memoryAddrPA := memoryAddrPA
    bundle.target := target
    bundle.inst := inst
    bundle.memoryType := memoryType
    bundle.memorySize := memorySize
    bundle.branchType := branchType
    bundle.branchTaken := branchTaken
    bundle.InstID := InstID
    bundle
  }
}

class TraceReaderHelper(width: Int)(implicit p: Parameters)
  extends ExtModule
  with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val enable = IO(Input(Bool()))

  val insts = IO(Output(Vec(width, new TraceInstrInnerBundle())))

  def getVerilog: String = {
    val nameList = new TraceInstrInnerBundle().elements.map(_._1).toSeq.reverse
    val sizeList = new TraceInstrInnerBundle().elements.map(_._2.getWidth).toSeq.reverse

    def genModulePort: String = {
      def genElementPort(size: Int, baseName: String): String = {
        (0 until width)
          .map(i => s"output [${size - 1}:0] insts_${i}_${baseName},")
          .mkString("  ", "\n  ", "\n")
      }
      s"""
         |  input  clock,
         |  input  reset,
         |${nameList.zip(sizeList).map{case (name, size) => genElementPort(size, name)}.mkString}
         |  input  enable
         |""".stripMargin
    }

    def genStructType: String = {
      def genElement(name: String, size: Int): String = {
        f"logic [${size-1}:0] $name;"
      }
      def genSingle: String = {
        s"""
           |typedef struct {
           |${nameList.zip(sizeList).map{ case (name, size) => genElement(name, size) }.mkString("  ", "\n  ", "\n")}
           |} SingleInstruction_t;
           |""".stripMargin
      }
      def genMany: String = {
        s"""
           |typedef struct {
           |  SingleInstruction_t insts[$width];
           |} ManyInstruction_t;
           |""".stripMargin
      }
      s"""
         |${genSingle}
         |
         |${genMany}
         |""".stripMargin
    }

    def genLogicDeclare: String = {
      (0 until width).map { case idx =>
        nameList.zip(sizeList).map{ case (name, size) =>
          s"""
             |logic [${size-1}:0] logicInsts_${idx}_${name};
             |""".stripMargin
        }.mkString("  ","\n  ","\n")
      }.mkString
    }

    def fromLogictoIO: String = {
      (0 until width).map { case idx =>
        nameList.zip(sizeList).map{ case (name, size) =>
          f"assign insts_${idx}_${name} = logicInsts_${idx}_${name};"
        }.mkString("  ","\n  ","\n")
      }.mkString
    }

    def funcDeclare: String = {
      //  |import "DPI-C" function void trace_read_insts(input byte enable, output ManyInstruction_t insts);
      //  |  input  byte enable
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
       |  output longint InstID,
       |  input  byte idx,
       |);
       |""".stripMargin
    }

    def callDPIC(destName: String): String = {
      (0 until width).map{ case i =>
        def assignTrace: String = {
          s"""
             |    trace_read_one_instr(
             |      ${destName}_${i}_pcVA, ${destName}_${i}_pcPA,
             |      ${destName}_${i}_memoryAddrVA, ${destName}_${i}_memoryAddrPA,
             |      ${destName}_${i}_target, ${destName}_${i}_inst,
             |      ${destName}_${i}_memoryType, ${destName}_${i}_memorySize,
             |      ${destName}_${i}_branchType, ${destName}_${i}_branchTaken,
             |      ${destName}_${i}_exception,
             |      ${destName}_${i}_InstID,
             |      $i);
             |""".stripMargin
        }
        def assignDummy: String = {
          nameList.map{ case name =>
            f"${destName}_${i}_${name} = 0;"
          }.mkString("    ", "    \n", "\n")
        }
        s"""
           |always @(negedge clock) begin
           |  if (!reset && enable) begin
           |    ${assignTrace}
           |  end
           |  else begin
           |    ${assignDummy}
           |  end
           |end
           """.stripMargin
      }.mkString("","\n","\n")
    }

    s"""
       |
       |${funcDeclare}
       |
       |module TraceReaderHelper(
       |${genModulePort}
       |);
       |
       |${genLogicDeclare}
       |${callDPIC("logicInsts")}
       |${fromLogictoIO}
       |
       |endmodule
       |
       """.stripMargin
  }

  setInline(s"$desiredName.sv", getVerilog)
}

class TraceRedirectHelper
  extends ExtModule
  with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val enable = IO(Input(Bool()))
  val InstID = IO(Input(UInt(64.W)))

  def getVerilog: String = {
    s"""
       |import "DPI-C" function void trace_redirect(input longint InstID);
       |
       |module TraceRedirectHelper(
       |  input clock,
       |  input reset,
       |  input enable,
       |  input [63:0] InstID
       |);
       |
       |  always @(negedge clock) begin
       |    if (enable && !reset) begin
       |      trace_redirect(InstID);
       |    end
       |  end
       |endmodule
       |""".stripMargin
  }
  setInline(s"$desiredName.v", getVerilog)
}

class TraceCollectorHelper(width: Int)
  extends ExtModule
  with HasExtModuleInline{
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))

  val enable = IO(Input(Vec(width, Bool())))
  val pc = IO(Input(Vec(width, UInt(64.W))))
  val inst = IO(Input(Vec(width, UInt(32.W))))
  // InstNum is used to deal with RoB Merge
  val instNum = IO(Input(Vec(width, UInt(8.W))))

  def getVerilog: String = {
    def genPort(size: Int, baseName: String): String = {
      (0 until width)
        .map(i => s"input [${size - 1}:0] ${baseName}_${i},")
        .mkString("  ", "\n  ", "\n")
    }
    def genBoolPort(baseName: String): String = {
      (0 until width)
        .map(i => s"input ${baseName}_${i},")
        .mkString("  ", "\n  ", "\n")
    }
    def callDPIC: String = {
      (0 until width)
        .map(i => s"""
                      | if (enable_${i}) begin
                      |   trace_collect_commit(pc_${i}, inst_${i}, instNum_${i}, $i);
                      | end
                      """.stripMargin)
        .mkString("      ", "\n      ", "\n")
    }
    s"""
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
       |
       |import "DPI-C" function void trace_collect_commit(
       |  input longint pc,
       |  input int inst,
       |  input byte instNum,
       |  input byte idx
       |);
       |
       |""".stripMargin
  }
  setInline(s"$desiredName.v", getVerilog)
}

class TraceDriveCollectorHelper(width: Int)
  extends ExtModule
    with HasExtModuleInline{
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
//  val enable = IO(Input(Bool()))

  val enable = IO(Input(Vec(width, Bool())))
  val pc = IO(Input(Vec(width, UInt(64.W))))
  val inst = IO(Input(Vec(width, UInt(32.W))))

  def getVerilog: String = {
    def genPort(size: Int, baseName: String): String = {
      (0 until width)
        .map(i => s"input [${size - 1}:0] ${baseName}_${i},")
        .mkString("  ", "\n  ", "\n")
    }
    def genBoolPort(baseName: String): String = {
      (0 until width)
        .map(i => s"input ${baseName}_${i},")
        .mkString("  ", "\n  ", "\n")
    }
    def callDPIC: String = {
      (0 until width)
        .map(i => s"""
                     | if (enable_${i}) begin
                     |   trace_collect_drive(pc_${i}, inst_${i}, $i);
                     | end
                      """.stripMargin)
        .mkString("      ", "\n      ", "\n")
    }
    s"""
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
       |
       |import "DPI-C" function void trace_collect_drive(
       |  input longint pc,
       |  input int inst,
       |  input byte idx
       |);
       |
       |""".stripMargin
  }
  setInline(s"$desiredName.v", getVerilog)
}



/** TraceICache
 *  TraceICache is a black box, we can only get the data from it
 */

class TraceICacheHelper extends ExtModule
  with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val enable = IO(Input(Bool()))
  val addr = IO(Input(UInt(64.W)))
  val data = IO(Output(Vec(512/64, UInt(64.W))))
  val legal_addr = IO(Output(Bool()))

  def getVerilog: String = {
    def getDataPort(): String = {
      (0 until 512/64)
        .map(i => s"output reg [63:0] data_$i,")
        .mkString("  ", "\n  ", "\n")
    }
    def genLogicDeclare: String = {
      (0 until 512/64)
        .map(i => s"logic [63:0] logicData_$i;")
        .mkString("    ", "\n  ", "\n")
    }
    def fromLogictoIO: String = {
      val assignData: String =
        (0 until 512/64)
          .map(i => s"data_$i <= logicData_$i;")
          .mkString("      ", "\n    ", "\n")
      s"""
         |always @(posedge clock) begin
         |  if (!reset && enable) begin
         |$assignData
         |    legal_addr <= logic_legal_addr;
         |  end
         |end
         |"""
    }
    def callDPIC: String = {
      def assignDPIC: String = {
        (0 until 512/64)
          .map(i => s"logicData_$i <= trace_icache_dword_helper(addr_align + $i * 8);")
          .mkString("      ", "\n      ", "\n")
      }
      def assignDummy: String = {
        (0 until 512/64)
          .map(i => s"logicData_$i <= 0;")
          .mkString("    ", "\n    ", "\n")
      }
      s"""
         |always @(negedge clock) begin
         |  if (!reset && enable) begin
         |${assignDPIC}
         |  end
         |  else begin
         |${assignDummy}
         |  end
         |end
         """.stripMargin
    }


    s"""
       |import "DPI-C" function byte trace_icache_legal_addr(
       |  input longint addr
       |);
       |import "DPI-C" function longint trace_icache_dword_helper(
       |  input longint addr
       |);
       |
       |module TraceICacheHelper(
       |  input             clock,
       |  input             reset,
       |  input             enable,
       |  input      [63:0] addr,
       |$getDataPort
       |  output reg [7:0]  legal_addr
       |);
       |
       |  logic [7:0] logic_legal_addr;
       |$genLogicDeclare
       |
       |$fromLogictoIO
       |
       |  wire [63:0] addr_align = addr & 64'hffffffffffffffe0;
       |  always @(negedge clock) begin
       |    if (!reset && enable) begin
       |      logic_legal_addr <= trace_icache_legal_addr(addr_align);
       |    end
       |    else begin
       |      logic_legal_addr <= 0;
       |    end
       |  end
       |
       |$callDPIC
       |
       |endmodule
       |
       |""".stripMargin
  }

  setInline(s"$desiredName.v", getVerilog)
}

class TraceFakeICacheRespBundle(implicit p: Parameters) extends TraceBundle {
  val data = Vec(2, UInt(256.W))
  val addr = UInt(VAddrBits.W)
}

// Replace ICache's data
// IFU1 --fire--> IFU2
class TraceFakeICache()(implicit p: Parameters) extends TraceModule {
  val io = IO(new Bundle {
    val req = Flipped(ValidIO(new Bundle {
      val addr = UInt(VAddrBits.W)
    }))
    val resp = Valid(new TraceFakeICacheRespBundle)
  })

  if (env.TraceRTLMode) {
    val helper = Module(new TraceICacheHelper)
    helper.clock := clock
    helper.reset := reset
    helper.enable := io.req.valid
    helper.addr := io.req.bits.addr
    io.resp.valid := helper.legal_addr(0) && RegNext(io.req.valid)
    io.resp.bits.data(0) := Cat(helper.data(3), helper.data(2), helper.data(1), helper.data(0))
    io.resp.bits.data(1) := Cat(helper.data(7), helper.data(6), helper.data(5), helper.data(4))
    io.resp.bits.addr := RegEnable(helper.addr, io.req.valid)
  } else {
    io.resp := 0.U.asTypeOf(io.resp)
  }
}

// Fake MMU, input is vaddr, output is paddr and hit

class TraceATSBundle(implicit p: Parameters) extends TraceBundle {
  val valid = Input(Bool())
  val vaddr = Input(UInt(VAddrBits.W))
  val paddr = Output(UInt(PAddrBits.W))
  val hit   = Output(Bool())
}

class TraceATSHelper extends ExtModule
  with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val asid  = IO(Input(UInt(16.W)))
  val vmid  = IO(Input(UInt(16.W)))
  val valid = IO(Input(Bool()))
  val vaddr = IO(Input(UInt(64.W)))
  val paddr = IO(Output(UInt(64.W)))
  val hit   = IO(Output(Bool()))

  def getVerilog: String = {
    def getDPIC: String = {
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
         |""".stripMargin
    }

    def logicDeclare: String = {
      s"""
         |  logic [63:0] logic_paddr;
         |  logic        logic_hit;
         |  reg [63:0]   reg_paddr;
         |  reg          reg_hit;
         |""".stripMargin
    }

    def callDPIC: String = {
      s"""
         |always @(negedge clock) begin
         |  if (!reset && valid) begin
         |    logic_hit   <= trace_tlb_ats_hit(vaddr, asid, vmid);
         |    logic_paddr <= trace_tlb_ats(vaddr, asid, vmid);
         |  end
         |  else begin
         |    logic_hit <= 0;
         |    logic_paddr <= 0;
         |  end
         |end
         |""".stripMargin
    }

    def assignToReg: String = {
      s"""
         |always @(posedge clock) begin
         |  if (!reset && valid) begin
         |    reg_paddr <= logic_paddr;
         |    reg_hit   <= logic_hit;
         |  end
         |end
         |""".stripMargin
    }

    def assignToIO: String = {
      s"""
         |assign paddr = reg_paddr;
         |assign hit   = reg_hit;
         |""".stripMargin
    }

    s"""
       |$getDPIC
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
       |$logicDeclare
       |
       |$callDPIC
       |
       |$assignToReg
       |
       |$assignToIO
       |
       |endmodule
       |
       |""".stripMargin
  }

  setInline(s"$desiredName.v", getVerilog)
}


class TraceFakeMMU()(implicit p: Parameters) extends TraceModule {
  val io = IO(new TraceATSBundle())

  val atsHelper = Module(new TraceATSHelper())
  atsHelper.clock := clock
  atsHelper.reset := reset
  atsHelper.vaddr := io.vaddr
  atsHelper.valid := io.valid
  atsHelper.asid := 0.U
  atsHelper.vmid := 0.U
  io.paddr := atsHelper.paddr
  io.hit   := atsHelper.hit
}