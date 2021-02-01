package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import utils._
import xiangshan.backend.exu.Exu.fmiscExeUnitCfg
import xiangshan.backend.fu.fpu._

class FmiscExeUnit extends Exu(fmiscExeUnitCfg) {

  val frm = IO(Input(UInt(3.W)))

  val f2i :: f2f :: fdivSqrt :: Nil = supportedFunctionUnits.map(fu => fu.asInstanceOf[FPUSubModule])
  val toFpUnits = Seq(f2f, fdivSqrt)
  val toIntUnits = Seq(f2i)

  assert(toFpUnits.size == 1 || fpArb.io.in.length == toFpUnits.size)
  assert(toIntUnits.size == 1 || intArb.io.in.length == toIntUnits.size)

  val input = io.fromFp
  val isRVF = input.bits.uop.ctrl.isRVF
  val instr_rm = input.bits.uop.ctrl.fpu.rm
  val (src1, src2) = (input.bits.src1, input.bits.src2)

  supportedFunctionUnits.foreach { module =>
    module.io.in.bits.src(0) := src1
    module.io.in.bits.src(1) := src2
    module.asInstanceOf[FPUSubModule].rm := Mux(instr_rm =/= 7.U, instr_rm, frm)
  }

  io.toFp.bits.fflags := MuxCase(
    0.U,
    toFpUnits.map(x => x.io.out.fire() -> x.fflags)
  )
  val fpOutCtrl = io.toFp.bits.uop.ctrl.fpu
  io.toFp.bits.data := box(fpArb.io.out.bits.data, fpOutCtrl.typeTagOut)

  io.toInt.bits.fflags := MuxCase(
    0.U,
    toIntUnits.map(x => x.io.out.fire() -> x.fflags)
  )
}
