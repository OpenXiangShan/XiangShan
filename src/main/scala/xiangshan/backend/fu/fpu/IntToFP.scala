// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import hardfloat.INToRecFN
import utils.{SignExt, ZeroExt}

class IntToFPDataModule extends FPUDataModule {

  val in_valid, out_ready = IO(Input(Bool()))
  val in_ready, out_valid = IO(Output(Bool()))
  val kill_w, kill_r = IO(Input(Bool()))

  val s_idle :: s_cvt :: s_ieee :: s_finish :: Nil = Enum(4)
  val state = RegInit(s_idle)


  val in_fire = in_valid && in_ready
  val out_fire = out_valid && out_ready
  in_ready := state === s_idle
  out_valid := state === s_finish

  val src1 = RegEnable(io.in.src(0)(XLEN-1, 0), in_fire)
  val rmReg = RegEnable(rm, in_fire)
  val ctrl = RegEnable(io.in.fpCtrl, in_fire)

  switch(state){
    is(s_idle){
      when(in_fire && !kill_w){
        state := s_cvt
      }
    }
    is(s_cvt){
      state := s_ieee
    }
    is(s_ieee){
      state := s_finish
    }
    is(s_finish){
      when(out_fire){
        state := s_idle
      }
    }
  }
  when(state =/= s_idle && kill_r){
    state := s_idle
  }

  /*
      s_cvt
   */
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

  val muxReg = Reg(mux.cloneType)
  when(state === s_cvt){
    muxReg := mux
  }.elsewhen(state === s_ieee){
    muxReg.data := ieee(box(muxReg.data, ctrl.typeTagOut))
  }

  fflags := muxReg.exc
  io.out.data := muxReg.data
}

class IntToFP extends FPUSubModule {
  override val dataModule = Module(new IntToFPDataModule)
  dataModule.in_valid := io.in.valid
  dataModule.out_ready := io.out.ready
  connectDataModule
  val uopReg = RegEnable(io.in.bits.uop, io.in.fire())
  dataModule.kill_w := io.in.bits.uop.roqIdx.needFlush(io.redirectIn, io.flushIn)
  dataModule.kill_r := uopReg.roqIdx.needFlush(io.redirectIn, io.flushIn)
  io.in.ready := dataModule.in_ready
  io.out.valid := dataModule.out_valid
  io.out.bits.uop := uopReg
}
