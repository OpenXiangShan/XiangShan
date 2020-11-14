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
) extends XSModule {
  val io = IO(new Bundle() {
    val readPorts = Vec(numReadPorts, new RfReadPort)
    val writePorts = Vec(numWirtePorts, new RfWritePort)
  })

  val mem = Mem(NRPhyRegs, UInt(XLEN.W))
  
  for(r <- io.readPorts){
    val addr_reg = RegNext(r.addr)
    r.data := {if(hasZero) Mux(addr_reg===0.U, 0.U, mem(addr_reg)) else mem(addr_reg)}
  }

  for(w <- io.writePorts){
    when(w.wen){
      mem(w.addr) := w.data
    }
  }

  if (!env.FPGAPlatform) {
    val debugArchRat = WireInit(VecInit(Seq.fill(32)(0.U(PhyRegIdxWidth.W))))
    ExcitingUtils.addSink(
      debugArchRat,
      if(hasZero) "DEBUG_INI_ARCH_RAT" else "DEBUG_FP_ARCH_RAT",
      ExcitingUtils.Debug
    )

    val debugArchReg = WireInit(VecInit(debugArchRat.zipWithIndex.map(
      x => if(hasZero && x._2==0) 0.U else mem(x._1)
    )))
    ExcitingUtils.addSource(
      debugArchReg,
      if(hasZero) "DEBUG_INT_ARCH_REG" else "DEBUG_FP_ARCH_REG",
      ExcitingUtils.Debug
    )
  }

}
