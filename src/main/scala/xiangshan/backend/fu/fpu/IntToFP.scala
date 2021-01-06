package xiangshan.backend.fu.fpu

import chisel3._
import hardfloat.INToRecFN
import utils.{SignExt, ZeroExt}

class IntToFP extends FPUSubModule {

  val ctrl = io.in.bits.uop.ctrl.fpu
  val tag = ctrl.typeTagIn
  val typ = ctrl.typ
  val wflags = ctrl.wflags
  val src1 = io.in.bits.src(0)(XLEN-1, 0)

  val mux = Wire(new Bundle() {
    val data = UInt((XLEN+1).W)
    val exc = UInt(5.W)
  })
  mux.data := recode(src1, tag)
  mux.exc := 0.U

  val intValue = Mux(typ(1),
    Mux(typ(0), ZeroExt(src1, XLEN), SignExt(src1, XLEN)),
    Mux(typ(0), ZeroExt(src1(31, 0), XLEN), SignExt(src1(31, 0), XLEN))
  )

  when(wflags){
    val i2fResults = for(t <- floatTypes) yield {
      val i2f = Module(new INToRecFN(XLEN, t.exp, t.sig))
      i2f.io.signedIn := ~typ(0)
      i2f.io.in := intValue
      i2f.io.roundingMode := rm
      i2f.io.detectTininess := hardfloat.consts.tininess_afterRounding
      (sanitizeNaN(i2f.io.out, t), i2f.io.exceptionFlags)
    }
    val (data, exc) = i2fResults.unzip
    mux.data := VecInit(data)(tag)
    mux.exc := VecInit(exc)(tag)
  }

  fflags := mux.exc
  io.out.bits.uop := io.in.bits.uop
  io.out.bits.data := box(mux.data, io.in.bits.uop.ctrl.fpu.typeTagOut)
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}
