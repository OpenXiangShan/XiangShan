package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils.ParallelOR

class BusyTable extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    // set preg state to busy
    val allocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    // set preg state to ready
    val wbPregs = Vec(NRWritePorts, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    // read preg state
    val rfReadAddr = Vec(NRReadPorts, Input(UInt(PhyRegIdxWidth.W)))
    val pregRdy = Vec(NRReadPorts, Output(Bool()))
  })

  val table = RegInit(VecInit(Seq.fill(NRPhyRegs)(false.B)))

  for((raddr, rdy) <- io.rfReadAddr.zip(io.pregRdy)){
    rdy := !table(raddr) || ParallelOR(io.wbPregs.map(wb => wb.valid && (wb.bits===raddr))).asBool()
  }

  for((alloc, i) <- io.allocPregs.zipWithIndex){
    when(alloc.valid){
      table(alloc.bits) := true.B
    }
  }

  for((wb, i) <- io.wbPregs.zipWithIndex){
    when(wb.valid){
      table(wb.bits) := false.B
    }
  }

  when(io.flush){
    table.foreach(_ := false.B)
  }
}
