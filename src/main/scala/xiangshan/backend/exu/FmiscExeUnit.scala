package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import utils._
import xiangshan.backend.fu.FunctionUnit
import xiangshan.backend.fu.FunctionUnit.fmiscSel
import xiangshan.backend.fu.fpu.FPUOpType._
import xiangshan.backend.fu.fpu._

class FmiscExeUnit extends Exu(
  exuName = "FmiscExeUnit",
  fuGen = {
    Seq[(() => FPUSubModule, FPUSubModule => Bool)](
      (FunctionUnit.fcmp _, fmiscSel(FU_FCMP)),
      (FunctionUnit.fmv _, fmiscSel(FU_FMV)),
      (FunctionUnit.f2i _, fmiscSel(FU_F2I)),
      (FunctionUnit.f32toF64 _, fmiscSel(FU_S2D)),
      (FunctionUnit.f64toF32 _, fmiscSel(FU_D2S)),
      (FunctionUnit.fdivSqrt _, fmiscSel(FU_DIVSQRT))
    )
  },
  wbIntPriority = Int.MaxValue,
  wbFpPriority = 1
){

  val frm = IO(Input(UInt(3.W)))

  val fcmp :: fmv :: f2i :: f32toF64 :: f64toF32 :: fdivSqrt :: Nil = supportedFunctionUnits

  val fuOp = io.in.bits.uop.ctrl.fuOpType
  assert(fuOp.getWidth == 7) // when fuOp's WIDTH change, here must change too
  val fu = fuOp.head(4)
  val op = fuOp.tail(4)
  val isRVF = io.in.bits.uop.ctrl.isRVF

  val instr_rm = io.in.bits.uop.cf.instr(14, 12)

  supportedFunctionUnits.foreach{ module =>
    module.io.in.bits.src(0) := Mux(
      (isRVF && fuOp=/=d2s && fuOp=/=fmv_f2i) || fuOp===s2d,
      unboxF64ToF32(src1),
      src1
    )
    if(module.cfg.srcCnt > 1){
      module.io.in.bits.src(1) := Mux(isRVF, unboxF64ToF32(src2), src2)
    }
    module.rm := Mux(instr_rm =/= 7.U, instr_rm, frm)
  }

  val arbiter = outputArb.get

  io.out.bits.fflags := Mux1H(arbiter.io.in.zip(supportedFunctionUnits).map(
    x => x._1.fire() -> x._2.fflags
  ))
  val outCtrl = io.out.bits.uop.ctrl
  io.out.bits.data := Mux(outCtrl.isRVF && outCtrl.fpWen,
    boxF32ToF64(arbiter.io.out.bits.data),
    Mux( (outCtrl.isRVF && outCtrl.fuOpType===fmv_f2i) || outCtrl.fuOpType===f2w || outCtrl.fuOpType===f2wu,
      SignExt(arbiter.io.out.bits.data(31, 0), XLEN),
      arbiter.io.out.bits.data
    )
  )
}
