package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._
import utils.{ParallelOR, XSDebug}

class BusyTable extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    // set preg state to busy
    val allocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    // set preg state to ready (write back regfile + roq walk)
    val wbPregs = Vec(NRWritePorts + CommitWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    // read preg state
    val rfReadAddr = Vec(NRReadPorts, Input(UInt(PhyRegIdxWidth.W)))
    val pregRdy = Vec(NRReadPorts, Output(Bool()))
  })

  val table = RegInit(0.U(NRPhyRegs.W))

  val wbMask = ParallelOR(io.wbPregs.take(NRWritePorts).map(w => Mux(w.valid, UIntToOH(w.bits), 0.U)))
  val allocMask = ParallelOR(io.allocPregs.map(a => Mux(a.valid, UIntToOH(a.bits), 0.U)))

  val tableNext = table & (~wbMask).asUInt() | allocMask

  for((raddr, rdy) <- io.rfReadAddr.zip(io.pregRdy)){
    rdy := !tableNext(raddr)
  }

  table := tableNext

//  for((alloc, i) <- io.allocPregs.zipWithIndex){
//    when(alloc.valid){
//      table(alloc.bits) := true.B
//    }
//    XSDebug(alloc.valid, "Allocate %d\n", alloc.bits)
//  }


//  for((wb, i) <- io.wbPregs.zipWithIndex){
//    when(wb.valid){
//      table(wb.bits) := false.B
//    }
//    XSDebug(wb.valid, "writeback %d\n", wb.bits)
//  }

  when(io.flush){
    table := 0.U(NRPhyRegs.W)
  }

  XSDebug(p"table    : ${Binary(table)}\n")
  XSDebug(p"tableNext: ${Binary(tableNext)}\n")
  XSDebug(p"allocMask: ${Binary(allocMask)}\n")
  XSDebug(p"wbMask : ${Binary(wbMask)}\n")
  for (i <- 0 until NRPhyRegs) {
    XSDebug(table(i), "%d is busy\n", i.U)
  }
}
