package xiangshan.backend.regfile

import chipsalliance.rocketchip.config.Parameters
import chisel3._
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
  } else {

    val regfile = Module(new regfile_160x64_10w16r_sim)

    regfile.io.clk := this.clock
    regfile.io.gpr := hasZero.B

    regfile.io.wen0   := io.writePorts(0).wen
    regfile.io.waddr0 := io.writePorts(0).addr
    regfile.io.wdata0 := io.writePorts(0).data

    regfile.io.wen1   := io.writePorts(1).wen
    regfile.io.waddr1 := io.writePorts(1).addr
    regfile.io.wdata1 := io.writePorts(1).data

    regfile.io.wen2   := io.writePorts(2).wen
    regfile.io.waddr2 := io.writePorts(2).addr
    regfile.io.wdata2 := io.writePorts(2).data

    regfile.io.wen3   := io.writePorts(3).wen
    regfile.io.waddr3 := io.writePorts(3).addr
    regfile.io.wdata3 := io.writePorts(3).data

    regfile.io.wen4   := io.writePorts(4).wen
    regfile.io.waddr4 := io.writePorts(4).addr
    regfile.io.wdata4 := io.writePorts(4).data

    regfile.io.wen5   := io.writePorts(5).wen
    regfile.io.waddr5 := io.writePorts(5).addr
    regfile.io.wdata5 := io.writePorts(5).data

    regfile.io.wen6   := io.writePorts(6).wen
    regfile.io.waddr6 := io.writePorts(6).addr
    regfile.io.wdata6 := io.writePorts(6).data

    regfile.io.wen7   := io.writePorts(7).wen
    regfile.io.waddr7 := io.writePorts(7).addr
    regfile.io.wdata7 := io.writePorts(7).data

    regfile.io.wen8   := false.B   //io.writePorts(8).wen
    regfile.io.waddr8 := DontCare  //io.writePorts(8).addr
    regfile.io.wdata8 := DontCare  //io.writePorts(8).data

    regfile.io.wen9   := false.B   //io.writePorts(9).wen
    regfile.io.waddr9 := DontCare  //io.writePorts(9).addr
    regfile.io.wdata9 := DontCare  //io.writePorts(9).data


    regfile.io.raddr0  := io.readPorts(0).addr
    regfile.io.raddr1  := io.readPorts(1).addr
    regfile.io.raddr2  := io.readPorts(2).addr
    regfile.io.raddr3  := io.readPorts(3).addr
    regfile.io.raddr4  := io.readPorts(4).addr
    regfile.io.raddr5  := io.readPorts(5).addr
    regfile.io.raddr6  := io.readPorts(6).addr
    regfile.io.raddr7  := io.readPorts(7).addr
    regfile.io.raddr8  := io.readPorts(8).addr
    regfile.io.raddr9  := io.readPorts(9).addr
    regfile.io.raddr10 := io.readPorts(10).addr
    regfile.io.raddr11 := io.readPorts(11).addr
    regfile.io.raddr12 := io.readPorts(12).addr
    regfile.io.raddr13 := io.readPorts(13).addr
    regfile.io.raddr14 := DontCare //io.readPorts(14).addr
    regfile.io.raddr15 := DontCare //io.readPorts(15).addr

    io.readPorts(0).data := regfile.io.rdata0
    io.readPorts(1).data := regfile.io.rdata1
    io.readPorts(2).data := regfile.io.rdata2
    io.readPorts(3).data := regfile.io.rdata3
    io.readPorts(4).data := regfile.io.rdata4
    io.readPorts(5).data := regfile.io.rdata5
    io.readPorts(6).data := regfile.io.rdata6
    io.readPorts(7).data := regfile.io.rdata7
    io.readPorts(8).data := regfile.io.rdata8
    io.readPorts(9).data := regfile.io.rdata9
    io.readPorts(10).data := regfile.io.rdata10
    io.readPorts(11).data := regfile.io.rdata11
    io.readPorts(12).data := regfile.io.rdata12
    io.readPorts(13).data := regfile.io.rdata13

    io.debug_rports := DontCare
  }

}

class regfile_160x64_10w16r_sim extends BlackBox with HasBlackBoxResource {

  val io = IO(new Bundle{
    val clk = Input(Clock())
    val gpr = Input(Bool())

    // write
    val wen0, wen1, wen2, wen3, wen4, wen5, wen6, wen7, wen8, wen9 = Input(Bool())
    val waddr0, waddr1, waddr2, waddr3, waddr4, waddr5, waddr6, waddr7, waddr8, waddr9 = Input(UInt(8.W))
    val wdata0, wdata1, wdata2, wdata3, wdata4, wdata5, wdata6, wdata7, wdata8, wdata9 = Input(UInt(64.W))

    // read
    val raddr0, raddr1, raddr2, raddr3, raddr4, raddr5, raddr6, raddr7 = Input(UInt(8.W))
    val raddr8, raddr9, raddr10, raddr11, raddr12, raddr13, raddr14, raddr15 = Input(UInt(8.W))
    val rdata0, rdata1, rdata2, rdata3, rdata4, rdata5, rdata6, rdata7 = Output(UInt(64.W))
    val rdata8, rdata9, rdata10, rdata11, rdata12, rdata13, rdata14, rdata15 = Output(UInt(64.W))
  })

  val vsrc = "/vsrc/regfile_160x64_10w16r_sim.v"
  println(s"Regfile: Using verilog source at: $vsrc")
  setResource(vsrc)

}

