package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._
import utils.{ParallelOR, XSDebug}

class BusyTable(numReadPorts: Int, numWritePorts: Int) extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    // set preg state to busy
    val allocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    // set preg state to ready (write back regfile + roq walk)
    val wbPregs = Vec(numWritePorts, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    // read preg state
    val rfReadAddr = Vec(numReadPorts, Input(UInt(PhyRegIdxWidth.W)))
    val pregRdy = Vec(numReadPorts, Output(Bool()))
  })

  val table = RegInit(VecInit(Seq.fill(NRPhyRegs)(false.B)))

  for((raddr, rdy) <- io.rfReadAddr.zip(io.pregRdy)){
    rdy := !table(raddr) || ParallelOR(io.wbPregs.map(wb => wb.valid && (wb.bits===raddr))).asBool()
  }

  for((alloc, i) <- io.allocPregs.zipWithIndex){
    when(alloc.valid){
      table(alloc.bits) := true.B
    }
    XSDebug(alloc.valid, "Allocate %d\n", alloc.bits)
  }

  for((wb, i) <- io.wbPregs.zipWithIndex){
    when(wb.valid){
      table(wb.bits) := false.B
    }
    XSDebug(wb.valid, "writeback %d\n", wb.bits)
  }

  when(io.flush){
    table.foreach(_ := false.B)
  }

  for (i <- 0 until NRPhyRegs) {
    XSDebug(table(i), "%d is busy\n", i.U)
  }
}
