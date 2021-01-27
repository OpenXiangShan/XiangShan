package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._

class RatReadPort extends XSBundle {
  val addr = Input(UInt(5.W))
  val rdata = Output(UInt(PhyRegIdxWidth.W))
}

class RatWritePort extends XSBundle {
  val wen = Input(Bool())
  val addr = Input(UInt(5.W))
  val wdata = Input(UInt(PhyRegIdxWidth.W))
}

object hartIdRTInt extends (() => Int) {
  var x = 0
  def apply(): Int = {
    x = x + 1
    x-1
  }
}

object hartIdRTFp extends (() => Int) {
  var x = 0
  def apply(): Int = {
    x = x + 1
    x-1
  }
}

class RenameTable(float: Boolean) extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val walkWen = Input(Bool())
    val readPorts = Vec({if(float) 4 else 3} * RenameWidth, new RatReadPort)
    val specWritePorts = Vec(CommitWidth, new RatWritePort)
    val archWritePorts = Vec(CommitWidth, new RatWritePort)
  })

  // speculative rename table
  val spec_table = RegInit(VecInit(Seq.tabulate(32)(i => i.U(PhyRegIdxWidth.W))))

  // arch state rename table
  val arch_table = RegInit(VecInit(Seq.tabulate(32)(i => i.U(PhyRegIdxWidth.W))))

  // When redirect happens (mis-prediction), don't update the rename table
  // However, when mis-prediction and walk happens at the same time, rename table needs to be updated
  for(w <- io.specWritePorts){
    when(w.wen && (!io.redirect.valid || io.walkWen)) {
      spec_table(w.addr) := w.wdata
    }
  }

  for((r, i) <- io.readPorts.zipWithIndex){
    r.rdata := spec_table(r.addr)
    // for(w <- io.specWritePorts.take(i/{if(float) 4 else 3})){ // bypass
    //   when(w.wen && (w.addr === r.addr)){ r.rdata := w.wdata }
    // }
  }

  for(w <- io.archWritePorts){
    when(w.wen){ arch_table(w.addr) := w.wdata }
  }

  val flush = io.redirect.valid && io.redirect.bits.isUnconditional()
  when (flush) {
    spec_table := arch_table
    // spec table needs to be updated when flushPipe
    for (w <- io.archWritePorts) {
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

  if (env.DualCoreDifftest) {
    val id = if (float) hartIdRTFp() else hartIdRTInt()
    ExcitingUtils.addSource(
      arch_table,
      if(float) s"DEBUG_FP_ARCH_RAT$id" else s"DEBUG_INI_ARCH_RAT$id",
      ExcitingUtils.Debug
    )
  }
}
