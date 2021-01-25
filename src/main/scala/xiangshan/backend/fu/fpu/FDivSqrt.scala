package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.FType
import hardfloat.{DivSqrtRecFNToRaw_small, RoundAnyRawFNToRecFN}

class FDivSqrt extends FPUSubModule {

  val s_idle :: s_div :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val divSqrt = Module(new DivSqrtRecFNToRaw_small(FType.D.exp, FType.D.sig, 0))
  val divSqrtRawValid = divSqrt.io.rawOutValid_sqrt || divSqrt.io.rawOutValid_div

  val fpCtrl = io.in.bits.uop.ctrl.fpu
  val tag = fpCtrl.typeTagIn
  val uopReg = RegEnable(io.in.bits.uop, io.in.fire())
  val single = RegEnable(tag === S, io.in.fire())
  val rmReg = RegEnable(rm, io.in.fire())
  val kill = uopReg.roqIdx.needFlush(io.redirectIn)
  val killReg = RegInit(false.B)

  switch(state){
    is(s_idle){
      when(io.in.fire() && !io.in.bits.uop.roqIdx.needFlush(io.redirectIn)){ state := s_div }
    }
    is(s_div){
      when(divSqrtRawValid){
        when(kill || killReg){
          state := s_idle
          killReg := false.B
        }.otherwise({
          state := s_finish
        })
      }.elsewhen(kill){
        killReg := true.B
      }
    }
    is(s_finish){
      when(io.out.fire() || kill){
        state := s_idle
      }
    }
  }


  val src1 = unbox(io.in.bits.src(0), tag, None)
  val src2 = unbox(io.in.bits.src(1), tag, None)
  divSqrt.io.inValid := io.in.fire() && !io.in.bits.uop.roqIdx.needFlush(io.redirectIn)
  divSqrt.io.sqrtOp := fpCtrl.sqrt
  divSqrt.io.a := src1
  divSqrt.io.b := src2
  divSqrt.io.roundingMode := rm



  val round32 = Module(new RoundAnyRawFNToRecFN(
    FType.D.exp, FType.D.sig+2, FType.S.exp, FType.S.sig, 0
  ))
  val round64 = Module(new RoundAnyRawFNToRecFN(
    FType.D.exp, FType.D.sig+2, FType.D.exp, FType.D.sig, 0
  ))

  for(rounder <- Seq(round32, round64)){
    rounder.io.invalidExc := divSqrt.io.invalidExc
    rounder.io.infiniteExc := divSqrt.io.infiniteExc
    rounder.io.in := divSqrt.io.rawOut
    rounder.io.roundingMode := rmReg
    rounder.io.detectTininess := hardfloat.consts.tininess_afterRounding
  }

  val data = Mux(single, round32.io.out, round64.io.out)
  val flags = Mux(single, round32.io.exceptionFlags, round64.io.exceptionFlags)

  io.in.ready := state===s_idle
  io.out.valid := state===s_finish && !killReg
  io.out.bits.uop := uopReg
  io.out.bits.data := RegNext(data, divSqrtRawValid)
  fflags := RegNext(flags, divSqrtRawValid)
}
