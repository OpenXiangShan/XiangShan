package xiangshan.backend.exu


import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils._
import xiangshan.backend.exu.Exu.fmiscExeUnitCfg
import xiangshan.backend.fu.fpu.{F32toF64, F64toF32, FCMP, FMV, FPUSubModuleOutput, FloatToInt}
import xiangshan.backend.fu.fpu.divsqrt.DivSqrt
import xiangshan.backend.fu.fpu.FPUOpType._
import xiangshan.backend.fu.fpu._

class FmiscExeUnit extends Exu(fmiscExeUnitCfg){

  val fcmp = Module(new FCMP)
  val fmv = Module(new FMV(XLEN))
  val f2i = Module(new FloatToInt)
  val f32toF64 = Module(new F32toF64)
  val f64toF32 = Module(new F64toF32)
  val fdivSqrt = Module(new DivSqrt)

  val subModules = Array(
    (fcmp, FU_FCMP),
    (fmv, FU_FMV),
    (f2i, FU_F2I),
    (f32toF64, FU_S2D),
    (f64toF32, FU_D2S),
    (fdivSqrt, FU_DIVSQRT)
  ).map(x => (x._1, ("b" + x._2).U))

  val fuOp = io.in.bits.uop.ctrl.fuOpType
  assert(fuOp.getWidth == 7) // when fuOp's WIDTH change, here must change too
  val fu = fuOp.head(4)
  val op = fuOp.tail(4)
  val frm = WireInit(0.U(3.W))
  BoringUtils.addSink(frm, "Frm")
  val isRVF = io.in.bits.uop.ctrl.isRVF
  val (src1, src2) = (io.in.bits.src1, io.in.bits.src2)

  io.in.ready := Cat(subModules.map(x => fu===x._2 && x._1.io.in.ready)).orR()

  val instr_rm = io.in.bits.uop.cf.instr(14, 12)
  subModules.foreach{
    case (module, fuSel) =>
      module.io.in.valid := io.in.valid && fu===fuSel
      module.io.in.bits.uop := io.in.bits.uop
      module.io.in.bits.src(0) := Mux(
        (isRVF && fuOp=/=d2s && fuOp=/=fmv_f2i) || fuOp===s2d, 
        unboxF64ToF32(src1), 
        src1
      )
      module.io.in.bits.src(1) := Mux(isRVF, unboxF64ToF32(src2), src2)
      val extraInput = module.io.in.bits.ext.get
      extraInput.rm := Mux(instr_rm =/= 7.U, instr_rm, frm)
      extraInput.isDouble := !isRVF
      extraInput.op := op
      module.io.redirectIn := io.redirect
  }

  val wbArb = Module(new Arbiter(chiselTypeOf(subModules(0)._1.io.out.bits), subModules.length))

  wbArb.io.in <> VecInit(subModules.map(_._1.io.out))
  
  val out = wbArb.io.out

  out.ready := io.out.ready
  io.out.valid := out.valid
  io.out.bits.uop := out.bits.uop
  io.out.bits.fflags := out.bits.ext.get
  val outCtrl = out.bits.uop.ctrl
  io.out.bits.data := Mux(outCtrl.isRVF && outCtrl.fpWen, 
    boxF32ToF64(out.bits.data), 
    Mux( (outCtrl.isRVF && outCtrl.fuOpType===fmv_f2i) || outCtrl.fuOpType===f2w || outCtrl.fuOpType===f2wu,
      SignExt(out.bits.data(31, 0), XLEN),
      out.bits.data
    )
  )
  io.out.bits.redirectValid := DontCare
  io.out.bits.redirect := DontCare
}
