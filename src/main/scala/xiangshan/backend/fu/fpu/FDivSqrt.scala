package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.FType
import hardfloat.{DivSqrtRecFNToRaw_small, DivSqrtRecFNToRaw_srt4, RoundAnyRawFNToRecFN}

class FDivSqrtDataModule extends FPUDataModule {
  val in_valid, out_ready = IO(Input(Bool()))
  val in_ready, out_valid = IO(Output(Bool()))
  val kill_w = IO(Input(Bool()))
  val kill_r = IO(Input(Bool()))

  val in_fire = in_valid && in_ready
  val out_fire = out_valid && out_ready

  val s_idle :: s_div :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val divSqrt = Module(new DivSqrtRecFNToRaw_srt4(FType.D.exp, FType.D.sig))
  val divSqrtRawValid = divSqrt.io.rawOutValid_sqrt || divSqrt.io.rawOutValid_div

  val fpCtrl = io.in.fpCtrl
  val tag = fpCtrl.typeTagIn
  val single = RegEnable(tag === S, in_fire)
  val rmReg = RegEnable(rm, in_fire)

  switch(state){
    is(s_idle){
      when(in_fire && !kill_w){ state := s_div }
    }
    is(s_div){
      when(divSqrtRawValid){
        state := s_finish
      }
    }
    is(s_finish){
      when(out_fire){
        state := s_idle
      }
    }
  }
  when(kill_r){ state := s_idle }

  val src1 = unbox(io.in.src(0), tag, None)
  val src2 = unbox(io.in.src(1), tag, None)
  divSqrt.io.inValid := in_fire && !kill_w
  divSqrt.io.sqrtOp := fpCtrl.sqrt
  divSqrt.io.kill := kill_r
  divSqrt.io.sigBits := Mux(tag === S, FType.S.sig.U, FType.D.sig.U)
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

  assert(!(state === s_idle && !divSqrt.io.inReady))
  in_ready := state===s_idle
  out_valid := state===s_finish
  io.out.data := RegNext(data, divSqrtRawValid)
  fflags := RegNext(flags, divSqrtRawValid)
}


class FDivSqrt extends FPUSubModule {

  val uopReg = RegEnable(io.in.bits.uop, io.in.fire())
  val kill_r = !io.in.ready && uopReg.roqIdx.needFlush(io.redirectIn, io.flushIn)

  override val dataModule = Module(new FDivSqrtDataModule)
  connectDataModule
  dataModule.in_valid := io.in.valid
  dataModule.out_ready := io.out.ready
  dataModule.kill_w := io.in.bits.uop.roqIdx.needFlush(io.redirectIn, io.flushIn)
  dataModule.kill_r := kill_r
  io.in.ready := dataModule.in_ready
  io.out.valid := dataModule.out_valid
  io.out.bits.uop := uopReg
}
