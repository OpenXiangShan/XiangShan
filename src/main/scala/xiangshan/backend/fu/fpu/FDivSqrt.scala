/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.fu.fpu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.FType
import fudian.FPUpConverter
import hardfloat.{DivSqrtRecFNToRaw_small, DivSqrtRecFNToRaw_srt4, RoundAnyRawFNToRecFN}

class FDivSqrtDataModule(implicit p: Parameters) extends FPUDataModule {
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

  val in1_unboxed = FPU.unbox(io.in.src(0), tag)
  val in2_unboxed = FPU.unbox(io.in.src(1), tag)

  def up_convert_s_d(in: UInt): UInt = {
    val converter = Module(new FPUpConverter(
      FPU.f32.expWidth, FPU.f32.precision,
      FPU.f64.expWidth, FPU.f64.precision
    ))
    converter.io.in := in
    converter.io.rm := DontCare
    converter.io.result
  }

  val src1 = hardfloat.recFNFromFN(FType.D.exp, FType.D.sig,
    Mux(tag === FPU.S,
      up_convert_s_d(in1_unboxed),
      in1_unboxed
    )
  )
  val src2 = hardfloat.recFNFromFN(FType.D.exp, FType.D.sig,
    Mux(tag === FPU.S,
      up_convert_s_d(in2_unboxed),
      in2_unboxed
    )
  )

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

  val data = Mux(single,
    FPU.box(
      Cat(0.U(32.W), hardfloat.fNFromRecFN(FType.S.exp, FType.S.sig, round32.io.out)),
      FPU.S
    ),
    FPU.box(hardfloat.fNFromRecFN(FType.D.exp, FType.D.sig, round64.io.out), FPU.D)
  )
  val flags = Mux(single, round32.io.exceptionFlags, round64.io.exceptionFlags)

  assert(!(state === s_idle && !divSqrt.io.inReady))
  in_ready := state===s_idle
  out_valid := state===s_finish
  io.out.data := RegNext(data, divSqrtRawValid)
  fflags := RegNext(flags, divSqrtRawValid)

}


class FDivSqrt(implicit p: Parameters) extends FPUSubModule {

  val uopReg = RegEnable(io.in.bits.uop, io.in.fire())
  val kill_r = !io.in.ready && uopReg.robIdx.needFlush(io.redirectIn, io.flushIn)

  override val dataModule = Module(new FDivSqrtDataModule)
  connectDataModule
  dataModule.in_valid := io.in.valid
  dataModule.out_ready := io.out.ready
  dataModule.kill_w := io.in.bits.uop.robIdx.needFlush(io.redirectIn, io.flushIn)
  dataModule.kill_r := kill_r
  io.in.ready := dataModule.in_ready
  io.out.valid := dataModule.out_valid
  io.out.bits.uop := uopReg
}
