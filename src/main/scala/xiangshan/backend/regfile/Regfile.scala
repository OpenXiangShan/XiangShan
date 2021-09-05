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

package xiangshan.backend.regfile

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._
import xiangshan._

class RfReadPort(len: Int)(implicit p: Parameters) extends XSBundle {
  val addr = Input(UInt(PhyRegIdxWidth.W))
  val data = Output(UInt(len.W))
  override def cloneType: RfReadPort.this.type =
    new RfReadPort(len).asInstanceOf[this.type]
}

class RfWritePort(len: Int)(implicit p: Parameters) extends XSBundle {
  val wen = Input(Bool())
  val addr = Input(UInt(PhyRegIdxWidth.W))
  val data = Input(UInt(len.W))
  override def cloneType: RfWritePort.this.type =
    new RfWritePort(len).asInstanceOf[this.type]
}

class Regfile
(
  numReadPorts: Int,
  numWirtePorts: Int,
  hasZero: Boolean,
  len: Int
)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val readPorts = Vec(numReadPorts, new RfReadPort(len))
    val writePorts = Vec(numWirtePorts, new RfWritePort(len))
    val debug_rports = Vec(32, new RfReadPort(len))
  })

  println("Regfile: size:" + NRPhyRegs + " read: " + numReadPorts + " write: " + numWirtePorts)

  val useBlackBox = false
  if (!useBlackBox) {
    val mem = Reg(Vec(NRPhyRegs, UInt(len.W)))
    for (r <- io.readPorts) {
      val rdata = if (hasZero) Mux(r.addr === 0.U, 0.U, mem(r.addr)) else mem(r.addr)
      r.data := RegNext(rdata)
    }
    for (w <- io.writePorts) {
      when(w.wen) {
        mem(w.addr) := w.data
      }
    }

    for (rport <- io.debug_rports) {
      val zero_rdata = Mux(rport.addr === 0.U, 0.U, mem(rport.addr))
      rport.data := (if (hasZero) zero_rdata else mem(rport.addr))
    }
    when (reset.asBool()) {
      mem.map(_ := 0.U)
    }
  } else {

    val regfile = Module(new regfile_160x64_10w16r_sim)

    regfile.clk := this.clock
    regfile.gpr := hasZero.B

    regfile.wen0   := io.writePorts(0).wen
    regfile.waddr0 := io.writePorts(0).addr
    regfile.wdata0 := io.writePorts(0).data

    regfile.wen1   := io.writePorts(1).wen
    regfile.waddr1 := io.writePorts(1).addr
    regfile.wdata1 := io.writePorts(1).data

    regfile.wen2   := io.writePorts(2).wen
    regfile.waddr2 := io.writePorts(2).addr
    regfile.wdata2 := io.writePorts(2).data

    regfile.wen3   := io.writePorts(3).wen
    regfile.waddr3 := io.writePorts(3).addr
    regfile.wdata3 := io.writePorts(3).data

    regfile.wen4   := io.writePorts(4).wen
    regfile.waddr4 := io.writePorts(4).addr
    regfile.wdata4 := io.writePorts(4).data

    regfile.wen5   := io.writePorts(5).wen
    regfile.waddr5 := io.writePorts(5).addr
    regfile.wdata5 := io.writePorts(5).data

    regfile.wen6   := io.writePorts(6).wen
    regfile.waddr6 := io.writePorts(6).addr
    regfile.wdata6 := io.writePorts(6).data

    regfile.wen7   := io.writePorts(7).wen
    regfile.waddr7 := io.writePorts(7).addr
    regfile.wdata7 := io.writePorts(7).data

    regfile.wen8   := false.B   //io.writePorts(8).wen
    regfile.waddr8 := DontCare  //io.writePorts(8).addr
    regfile.wdata8 := DontCare  //io.writePorts(8).data

    regfile.wen9   := false.B   //io.writePorts(9).wen
    regfile.waddr9 := DontCare  //io.writePorts(9).addr
    regfile.wdata9 := DontCare  //io.writePorts(9).data


    regfile.raddr0  := io.readPorts(0).addr
    regfile.raddr1  := io.readPorts(1).addr
    regfile.raddr2  := io.readPorts(2).addr
    regfile.raddr3  := io.readPorts(3).addr
    regfile.raddr4  := io.readPorts(4).addr
    regfile.raddr5  := io.readPorts(5).addr
    regfile.raddr6  := io.readPorts(6).addr
    regfile.raddr7  := io.readPorts(7).addr
    regfile.raddr8  := io.readPorts(8).addr
    regfile.raddr9  := io.readPorts(9).addr
    regfile.raddr10 := io.readPorts(10).addr
    regfile.raddr11 := io.readPorts(11).addr
    regfile.raddr12 := io.readPorts(12).addr
    regfile.raddr13 := io.readPorts(13).addr
    regfile.raddr14 := DontCare //io.readPorts(14).addr
    regfile.raddr15 := DontCare //io.readPorts(15).addr

    io.readPorts(0).data := regfile.rdata0
    io.readPorts(1).data := regfile.rdata1
    io.readPorts(2).data := regfile.rdata2
    io.readPorts(3).data := regfile.rdata3
    io.readPorts(4).data := regfile.rdata4
    io.readPorts(5).data := regfile.rdata5
    io.readPorts(6).data := regfile.rdata6
    io.readPorts(7).data := regfile.rdata7
    io.readPorts(8).data := regfile.rdata8
    io.readPorts(9).data := regfile.rdata9
    io.readPorts(10).data := regfile.rdata10
    io.readPorts(11).data := regfile.rdata11
    io.readPorts(12).data := regfile.rdata12
    io.readPorts(13).data := regfile.rdata13

    io.debug_rports := DontCare
  }

}

class regfile_160x64_10w16r_sim extends ExtModule with HasExtModuleResource {


  val clk = IO(Input(Clock()))
  val gpr = IO(Input(Bool()))

  // write
  val wen0, wen1, wen2, wen3, wen4, wen5, wen6, wen7, wen8, wen9 = IO(Input(Bool()))
  val waddr0, waddr1, waddr2, waddr3, waddr4, waddr5, waddr6, waddr7, waddr8, waddr9 = IO(Input(UInt(8.W)))
  val wdata0, wdata1, wdata2, wdata3, wdata4, wdata5, wdata6, wdata7, wdata8, wdata9 = IO(Input(UInt(64.W)))

  // read
  val raddr0, raddr1, raddr2, raddr3, raddr4, raddr5, raddr6, raddr7 = IO(Input(UInt(8.W)))
  val raddr8, raddr9, raddr10, raddr11, raddr12, raddr13, raddr14, raddr15 = IO(Input(UInt(8.W)))
  val rdata0, rdata1, rdata2, rdata3, rdata4, rdata5, rdata6, rdata7 = IO(Output(UInt(64.W)))
  val rdata8, rdata9, rdata10, rdata11, rdata12, rdata13, rdata14, rdata15 = IO(Output(UInt(64.W)))

  val vsrc = "/vsrc/regfile_160x64_10w16r_sim.v"
  println(s"Regfile: Using verilog source at: $vsrc")
  addResource(vsrc)

}

