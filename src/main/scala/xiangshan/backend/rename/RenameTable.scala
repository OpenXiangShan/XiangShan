package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._

class RatReadPort extends XSBundle {
  val addr = Input(UInt(5.W))
  val rdata = Output(UInt(XLEN.W))
}

class RatWritePort extends XSBundle {
  val wen = Input(Bool())
  val addr = Input(UInt(5.W))
  val wdata = Input(UInt(XLEN.W))
}

class RenameTable(float: Boolean) extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val readPorts = Vec({if(float) 4 else 3} * RenameWidth, new RatReadPort)
    val specWritePorts = Vec(RenameWidth, new RatWritePort)
    val archWritePorts = Vec(CommitWidth, new RatWritePort)
  })

  // speculative rename table
  val spec_table = RegInit(VecInit(Seq.tabulate(32)(i => i.U(PhyRegIdxWidth.W))))

  // arch state rename table
  val arch_table = RegInit(VecInit(Seq.tabulate(32)(i => i.U(PhyRegIdxWidth.W))))

  for(w <- io.specWritePorts){
    when(w.wen){ spec_table(w.addr) := w.wdata }
  }

  for((r, i) <- io.readPorts.zipWithIndex){
    r.rdata := spec_table(r.addr)
    for(w <- io.specWritePorts.take(i/{if(float) 4 else 3})){ // bypass
      when(w.wen && (w.addr === r.addr)){ r.rdata := w.wdata }
    }
  }

  for(w <- io.archWritePorts){
    when(w.wen){ arch_table(w.addr) := w.wdata }
  }

  when(io.flush){
    spec_table := arch_table
    for(w <- io.archWritePorts) {
      when(w.wen){ spec_table(w.addr) := w.wdata }
    }
  }

  if (!env.FPGAPlatform) {
    ExcitingUtils.addSource(
      arch_table,
      if(float) "DEBUG_FP_ARCH_RAT" else "DEBUG_INI_ARCH_RAT",
      ExcitingUtils.Debug
    )
  }
}