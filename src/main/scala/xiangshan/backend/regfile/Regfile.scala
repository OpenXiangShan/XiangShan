package xiangshan.backend.regfile

import chisel3._
import chisel3.util._
import xiangshan._

class RfReadPort extends XSBundle {
  val addr = Input(UInt(PhyRegIdxWidth.W))
  val data = Output(UInt(XLEN.W))
}

class RfWritePort extends XSBundle {
  val wen = Input(Bool())
  val addr = Input(UInt(PhyRegIdxWidth.W))
  val data = Input(UInt(XLEN.W))
}

class Regfile
(
  numReadPorts: Int,
  numWirtePorts: Int,
  hasZero: Boolean
) extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val readPorts = Vec(numReadPorts, new RfReadPort)
    val writePorts = Vec(numWirtePorts, new RfWritePort)
  })
}
