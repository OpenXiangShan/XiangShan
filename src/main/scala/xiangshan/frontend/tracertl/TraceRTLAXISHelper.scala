/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2024 Institute of Computing Technology, Chinese Academy of Sciences
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
package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import chisel3.experimental.ExtModule

// TraceRTLAXISHelper are used for software verification for TraceRTLOnFPGA
// read many trace instructions into one buffer, and package it to axis-stream bus

class TraceRTLAXISMaster(MAX_DATA_WIDTH: Int = 20000, AXIS_DATA_WIDTH: Int = 512) extends Module {
  val io = IO(new Bundle {
    val axis = new TraceRTLAXISIO(AXIS_DATA_WIDTH)
  })

  private val helper = Module(new TraceAXISMasterHelper)
  // TODO: connect io
  helper.clock := clock
  helper.reset := reset
  helper.enable := io.axis.tready
  io.axis.tvalid := helper.tvalid
  io.axis.tdata := helper.tdata
  io.axis.tkeep := helper.tkeep
  io.axis.tlast := helper.tlast
}

class TraceAXISMasterHelper()
  extends ExtModule()
  with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val enable = IO(Input(Bool()))

  val tvalid = IO(Output(Bool()))
  val tdata = IO(Output(UInt(512.W)))
  val tkeep = IO(Output(UInt(64.W))) // tkeep of axis_stream
  val tlast = IO(Output(Bool()))

  def getVerilog: String = {
    s"""
       |import "DPI-C" function void trace_axis_master_helper(
       |  output byte    logic_tvalid,
       |  output byte    last,
       |  output longint valid,
       |  output longint data_0,
       |  output longint data_1,
       |  output longint data_2,
       |  output longint data_3,
       |  output longint data_4,
       |  output longint data_5,
       |  output longint data_6,
       |  output longint data_7
       |);
       |
       |module TraceAXISMasterHelper(
       |  input clock,
       |  input reset,
       |  input enable,
       |  output tvalid,
       |  output [511:0] tdata,
       |  output [63:0]  tkeep,
       |  output tlast
       |);
       |logic [63:0] data_0;
       |logic [63:0] data_1;
       |logic [63:0] data_2;
       |logic [63:0] data_3;
       |logic [63:0] data_4;
       |logic [63:0] data_5;
       |logic [63:0] data_6;
       |logic [63:0] data_7;
       |logic [63:0] validVec;
       |logic [7:0] last;
       |logic [7:0] logic_tvalid;
       |
       |always @(negedge clock) begin
       |  if (!reset && enable)  begin
       |    trace_axis_master_helper(logic_tvalid, last, validVec,
       |      data_0, data_1, data_2, data_3, data_4, data_5, data_6, data_7);
       |  end
       |end
       |
       |assign tvalid = logic_tvalid;
       |assign tlast = last;
       |assign tkeep = validVec;
       |assign tdata = {data_7, data_6, data_5, data_4, data_3, data_2, data_1, data_0};
       |
       |endmodule
       |
       |""".stripMargin
  }

  setInline(s"$desiredName.sv", getVerilog)
}

class TraceRTLAXISSlave(AXIS_DATA_WIDTH: Int = 512) extends Module {
  val io = IO(new Bundle {
    val axis = Flipped(new TraceRTLAXISIO(AXIS_DATA_WIDTH))
  })

  private val helper = Module(new TraceAXISSlaveHelper)
  helper.clock := clock
  helper.reset := reset
  helper.tvalid := io.axis.tvalid
  helper.tdata := io.axis.tdata
  helper.tkeep := io.axis.tkeep
  helper.tlast := io.axis.tlast
  io.axis.tready := true.B // should always be true
}

class TraceAXISSlaveHelper()
  extends ExtModule()
  with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))

  val tvalid = IO(Input(Bool()))
  val tdata = IO(Input(UInt(512.W)))
  val tkeep = IO(Input(UInt(64.W))) // tkeep of axis_stream
  val tlast = IO(Input(Bool()))

  def getVerilog: String = {
    s"""
       |import "DPI-C" function void trace_axis_slave_helper(
       |  input byte    last,
       |  input longint valid,
       |  input longint data_0,
       |  input longint data_1,
       |  input longint data_2,
       |  input longint data_3,
       |  input longint data_4,
       |  input longint data_5,
       |  input longint data_6,
       |  input longint data_7
       |);
       |
       |module TraceAXISSlaveHelper(
       |  input clock,
       |  input reset,
       |  input tvalid,
       |  input [511:0] tdata,
       |  input [63:0]  tkeep,
       |  input tlast
       |);
       |
       |always @(negedge clock) begin
       |  if (!reset && tvalid) begin
       |    trace_axis_slave_helper(tlast, tkeep,
       |    tdata[63:0], tdata[127:64], tdata[191:128], tdata[255:192],
       |    tdata[319:256], tdata[383:320], tdata[447:384], tdata[511:448]);
       |  end
       |end
       |
       |endmodule
       |
       |""".stripMargin
  }

  setInline(s"$desiredName.sv", getVerilog)
}