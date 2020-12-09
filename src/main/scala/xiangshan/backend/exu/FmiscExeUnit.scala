package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import utils._
import xiangshan.backend.exu.Exu.fmiscExeUnitCfg
import xiangshan.backend.fu.fpu.FPUOpType._
import xiangshan.backend.fu.fpu._

class FmiscExeUnit extends Exu(fmiscExeUnitCfg) {

  val frm = IO(Input(UInt(3.W)))

  val fcmp :: fmin :: fmv :: fsgnj :: f2i :: f32toF64 :: f64toF32 :: fdivSqrt :: Nil = supportedFunctionUnits.map(fu => fu.asInstanceOf[FPUSubModule])
  val toFpUnits = Seq(fmin, fsgnj, f32toF64, f64toF32, fdivSqrt)
  val toIntUnits = Seq(fcmp, fmv, f2i)

  assert(fpArb.io.in.length == toFpUnits.size)
  assert(intArb.io.in.length == toIntUnits.size)

  val input = io.fromFp
  val fuOp = input.bits.uop.ctrl.fuOpType
  assert(fuOp.getWidth == 7) // when fuOp's WIDTH change, here must change too
  val fu = fuOp.head(4)
  val op = fuOp.tail(4)
  val isRVF = input.bits.uop.ctrl.isRVF
  val instr_rm = input.bits.uop.cf.instr(14, 12)
  val (src1, src2) = (input.bits.src1, input.bits.src2)

  supportedFunctionUnits.foreach { module =>
    module.io.in.bits.src(0) := Mux(
      (isRVF && fuOp =/= d2s && fuOp =/= fmv_f2i) || fuOp === s2d,
      unboxF64ToF32(src1),
      src1
    )
    module.io.in.bits.src(1) := Mux(isRVF, unboxF64ToF32(src2), src2)
    module.asInstanceOf[FPUSubModule].rm := Mux(instr_rm =/= 7.U, instr_rm, frm)
  }

  io.toFp.bits.fflags := MuxCase(
    0.U.asTypeOf(new Fflags),
    toFpUnits.map(x => x.io.out.fire() -> x.fflags)
  )
  val fpOutCtrl = io.toFp.bits.uop.ctrl
  io.toFp.bits.data := Mux(fpOutCtrl.isRVF,
    boxF32ToF64(fpArb.io.out.bits.data),
    fpArb.io.out.bits.data
  )
  val intOutCtrl = io.toInt.bits.uop.ctrl
  io.toInt.bits.data := Mux(
    (intOutCtrl.isRVF && intOutCtrl.fuOpType === fmv_f2i) ||
      intOutCtrl.fuOpType === f2w ||
      intOutCtrl.fuOpType === f2wu,
    SignExt(intArb.io.out.bits.data(31, 0), XLEN),
    intArb.io.out.bits.data
  )
  io.toInt.bits.fflags := MuxCase(
    0.U.asTypeOf(new Fflags),
    toIntUnits.map(x => x.io.out.fire() -> x.fflags)
  )
}
