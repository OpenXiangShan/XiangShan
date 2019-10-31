package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

// memory order unit
object MOUOpType {
  def fence  = "b0".U
  def fencei = "b1".U
}

class MOUIO extends FunctionUnitIO {
  val cfIn = Flipped(new CtrlFlowIO)
  val redirect = new RedirectIO
}

class MOU extends NOOPModule {
  val io = IO(new MOUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  io.redirect.target := io.cfIn.pc + 4.U
  io.redirect.valid := valid
  val flushICache = valid && (func === MOUOpType.fencei)
  BoringUtils.addSource(flushICache, "MOUFlushICache")
  Debug(true){
    when(flushICache){
      printf("[MOU] Flush I$ at %x\n", io.cfIn.pc)
    }
  }

  io.out.bits := 0.U
  io.in.ready := true.B
  io.out.valid := valid
}
