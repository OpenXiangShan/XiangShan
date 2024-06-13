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

class TraceInstrBundle(implicit p: Parameters) extends TraceBundle {
  val pc = UInt(TracePCWidth.W)
  val inst = UInt(TraceInstrWidth.W)
}

object TraceInstrBundle {
  def apply(pc: UInt, inst: UInt)(implicit p: Parameters): TraceInstrBundle = {
    val bundle = Wire(new TraceInstrBundle())
    bundle.pc := pc
    bundle.inst := inst
    bundle
  }
}

class TraceReaderHelper(width: Int, pcWidth: Int, instWidth: Int)
  extends ExtModule
  with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val enable = IO(Input(Bool()))

  val pc = IO(Output(Vec(width, UInt(pcWidth.W))))
  val instr = IO(Output(Vec(width, UInt(instWidth.W))))

  def getVerilog: String = {
    def genPort(size: Int, baseName: String): String = {
      (0 until width)
        .map(i => s"output [${size - 1}:0] ${baseName}_$i,")
        .mkString("  ", "\n  ", "\n")
    }

    def callDPIC: String = {
      (0 until width)
        .map(i => s"trace_read_one_instr(enable, pc_${i}, instr_${i});")
        .mkString("      ", "\n      ", "\n")
    }
    s"""
       |import "DPI-C" function void trace_read_one_instr(
       |  input byte enable,
       |  output longint pc,
       |  output int instr
       |);
       |
       |module TraceReaderHelper(
       |  input  clock,
       |  input  reset,
       |${genPort(pcWidth, "pc")}
       |${genPort(instWidth, "instr")}
       |  input  enable
       |);
       |
       |  always @(posedge clock) begin
       |    if (enable && !reset) begin
       |$callDPIC
       |    end
       |  end
       |endmodule
       |
       |""".stripMargin
  }

  setInline(s"$desiredName.v", getVerilog)
}

/*
class TraceCollectorHelper(width: Int, pcWidth: Int, instWidth: Int, robMergeWidth: Int)
  extends ExtModule{
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val enable = IO(Input(Bool()))

  val pc = IO(Input(Vec(width, UInt(pcWidth.W))))
  val inst = IO(Input(Vec(width, UInt(instWidth.W))))
  // InstNum is used to deal with RoB Merge
  val instNum = IO(Input(Vec(width, UInt(robMergeWidth.W))))
}
*/




/** TraceICache
 *  TraceICache is a black box, we can only get the data from it
 */

class TraceICacheHelper extends ExtModule
  with HasExtModuleInline {
  val clock = IO(Input(Clock()))
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
    def callDPIC: String = {
      (0 until 512/64)
        .map(i => s"data_$i <= trace_icache_dword_helper(addr_align + $i * 8);")
        .mkString("      ", "\n      ", "\n")
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
       |  input             enable,
       |  input      [63:0] addr,
       |$getDataPort
       |  output reg [7:0]  legal_addr
       |);
       |
       |  wire [63:0] addr_align = addr & 64'hffffffffffffffe0;
       |  always @(posedge clock) begin
       |    if (enable) begin
       |      legal_addr <= trace_icache_legal_addr(addr_align);
       |$callDPIC
       |    end
       |  end
       |endmodule
       |""".stripMargin
  }

  setInline(s"$desiredName.v", getVerilog)
}

// Replace ICache's data
// IFU1 --fire--> IFU2
class FakeICache()(implicit p: Parameters) extends TraceModule {
  val io = IO(new Bundle {
    val req = Flipped(ValidIO(new Bundle {
      val addr = UInt(VAddrBits.W)
    }))
    val resp = Valid(new Bundle {
      val data0 = UInt(256.W)
      val data1 = UInt(256.W)
      val addr = UInt(VAddrBits.W)
    })
  })

  val helper = Module(new TraceICacheHelper)
  helper.clock := clock
  helper.enable := io.req.valid
  helper.addr := io.req.bits.addr
  io.resp.valid := helper.legal_addr(0) && RegNext(io.req.valid)
  io.resp.bits.data0 := Cat(helper.data(3), helper.data(2), helper.data(1), helper.data(0))
  io.resp.bits.data1 := Cat(helper.data(7), helper.data(6), helper.data(5), helper.data(4))
  io.resp.bits.addr := RegEnable(helper.addr, io.req.valid)
}
