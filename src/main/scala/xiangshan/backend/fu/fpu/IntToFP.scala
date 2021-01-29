// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import hardfloat.INToRecFN
import utils.{SignExt, ZeroExt}

class IntToFP extends FPUSubModule {

  val s_idle :: s_cvt :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)

  io.in.ready := state === s_idle
  io.out.valid := state === s_finish

  val src1 = RegEnable(io.in.bits.src(0)(XLEN-1, 0), io.in.fire())
  val uopReg = RegEnable(io.in.bits.uop, io.in.fire())
  val rmReg = RegEnable(rm, io.in.fire())

  switch(state){
    is(s_idle){
      when(io.in.fire() && !io.in.bits.uop.roqIdx.needFlush(io.redirectIn)){
        state := s_cvt
      }
    }
    is(s_cvt){
      state := s_finish
    }
    is(s_finish){
      when(io.out.fire()){
        state := s_idle
      }
    }
  }
  when(state =/= s_idle && uopReg.roqIdx.needFlush(io.redirectIn)){
    state := s_idle
  }

  /*
      s_cvt
   */
  val ctrl = uopReg.ctrl.fpu
  val tag = ctrl.typeTagIn
  val typ = ctrl.typ
  val wflags = ctrl.wflags

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
      i2f.io.roundingMode := rmReg
      i2f.io.detectTininess := hardfloat.consts.tininess_afterRounding
      (sanitizeNaN(i2f.io.out, t), i2f.io.exceptionFlags)
    }
    val (data, exc) = i2fResults.unzip
    mux.data := VecInit(data)(tag)
    mux.exc := VecInit(exc)(tag)
  }

  val muxReg = RegEnable(mux, enable = state === s_cvt)

  fflags := muxReg.exc
  io.out.bits.uop := uopReg
  io.out.bits.data := box(muxReg.data, ctrl.typeTagOut)
}
