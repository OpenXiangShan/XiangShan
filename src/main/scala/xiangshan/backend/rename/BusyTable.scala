package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class BusyTableReadIO extends XSBundle {
  val req = Input(UInt(PhyRegIdxWidth.W))
  val resp = Output(Bool())
}

class BusyTable(numReadPorts: Int, numWritePorts: Int) extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    // set preg state to busy
    val allocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    // set preg state to ready (write back regfile + roq walk)
    val wbPregs = Vec(numWritePorts, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    // read preg state
    val read = Vec(numReadPorts, new BusyTableReadIO)
  })

  val table = RegInit(0.U(NRPhyRegs.W))

  def reqVecToMask(rVec: Vec[Valid[UInt]]): UInt = {
    ParallelOR(rVec.map(v => Mux(v.valid, UIntToOH(v.bits), 0.U)))
  }

  val wbMask = reqVecToMask(io.wbPregs)
  val allocMask = reqVecToMask(io.allocPregs)

  val tableAfterWb = table & (~wbMask).asUInt
  val tableAfterAlloc = tableAfterWb | allocMask

  io.read.map(r => r.resp := !table(r.req))

  table := tableAfterAlloc

  when(io.flush){
    table := 0.U(NRPhyRegs.W)
  }

  XSDebug(p"table    : ${Binary(table)}\n")
  XSDebug(p"tableNext: ${Binary(tableAfterAlloc)}\n")
  XSDebug(p"allocMask: ${Binary(allocMask)}\n")
  XSDebug(p"wbMask   : ${Binary(wbMask)}\n")
  for (i <- 0 until NRPhyRegs) {
    XSDebug(table(i), "%d is busy\n", i.U)
  }

  XSPerf("busy_count", PopCount(table))
}
