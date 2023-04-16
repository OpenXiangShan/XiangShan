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

// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package xiangshan.backend.fu.fpu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import fudian.{FCMP, FloatPoint}
import xiangshan.backend.fu.FuConfig

class FPToFPDataModule(latency: Int)(implicit p: Parameters) extends FPUDataModule {

  val regEnables = IO(Input(Vec(latency, Bool())))

  val ctrlIn = io.in.fpCtrl
  val ctrl = RegEnable(ctrlIn, regEnables(0))
  val inTag = ctrl.typeTagIn
  val outTag = ctrl.typeTagOut
  val wflags = ctrl.wflags
  val src1 = RegEnable(FPU.unbox(io.in.src(0), ctrlIn.typeTagIn), regEnables(0))
  val src2 = RegEnable(FPU.unbox(io.in.src(1), ctrlIn.typeTagIn), regEnables(0))
  val rmReg = RegEnable(rm, regEnables(0))

  val signNum = Mux(rmReg(1), src1 ^ src2, Mux(rmReg(0), ~src2, src2))
  val fsgnj = VecInit(FPU.ftypes.map { t =>
    Cat(signNum(t.len - 1), src1(t.len - 2, 0))
  })(inTag)

//  val signNum = Mux(rmReg(1), src1 ^ src2, Mux(rmReg(0), ~src2, src2))
//  val fsgnj = Cat(signNum(fLen - 1), src1(fLen - 2, 0))

  val fsgnjMux = Wire(new Bundle() {
    val data = UInt(XLEN.W)
    val exc = UInt(5.W)
  })
  fsgnjMux.data := fsgnj
  fsgnjMux.exc := 0.U

  val scmp = Module(new FCMP(FPU.f32.expWidth, FPU.f32.precision))
  val dcmp = Module(new FCMP(FPU.f64.expWidth, FPU.f64.precision))
  val lt = VecInit(Seq(scmp, dcmp).map { fcmp =>
    fcmp.io.a := src1
    fcmp.io.b := src2
    fcmp.io.signaling := !rmReg(1)
    fcmp.io.lt || (fcmp.io.a.asSInt() < 0.S && fcmp.io.b.asSInt() >= 0.S)
  })(inTag)

  val fminmax = FPU.ftypes map { t =>
    val fp_a = FloatPoint.fromUInt(src1, t.expWidth, t.precision).decode
    val fp_b = FloatPoint.fromUInt(src2, t.expWidth, t.precision).decode
    val isnan1 = fp_a.isNaN
    val isnan2 = fp_b.isNaN
    val isInv = fp_a.isSNaN || fp_b.isSNaN
    val isNaNOut = isnan1 && isnan2
    val isLHS = isnan2 || rmReg(0) =/= lt && !isnan1
    val data = Mux(isNaNOut,
      FloatPoint.defaultNaNUInt(t.expWidth, t.precision),
      Mux(isLHS, src1, src2)
    )
    val exc = Cat(isInv, 0.U(4.W))
    (data, exc)
  }
  val (fminmax_data, fminmax_exc) = fminmax.unzip
  when(wflags){
    fsgnjMux.exc := VecInit(fminmax_exc)(inTag)
    fsgnjMux.data := VecInit(fminmax_data)(inTag)
  }

//  val lt = dcmp.io.lt || (dcmp.io.a.asSInt() < 0.S && dcmp.io.b.asSInt() >= 0.S)

  val mux = WireInit(fsgnjMux)

  val s2d = Module(new fudian.FPToFP(
    FPU.f32.expWidth, FPU.f32.precision,
    FPU.f64.expWidth, FPU.f64.precision
  ))

  val d2s = Module(new fudian.FPToFP(
    FPU.f64.expWidth, FPU.f64.precision,
    FPU.f32.expWidth, FPU.f32.precision
  ))

  for(fcvt <- Seq(s2d, d2s)){
    fcvt.io.in := src1
    fcvt.io.rm := rmReg
  }

  val fcvt_data = Mux(inTag === FPU.D, d2s.io.result, s2d.io.result)
  val fcvt_exc = Mux(inTag === FPU.D, d2s.io.fflags, s2d.io.fflags)

  when(ctrl.fcvt){
    mux.data := fcvt_data
    mux.exc := fcvt_exc
  }

  val boxed_data = Mux(outTag === FPU.S,
    FPU.box(mux.data, FPU.S),
    FPU.box(mux.data, FPU.D)
  )

  io.out.data := RegEnable(boxed_data, regEnables(1))
  io.out.fflags := RegEnable(mux.exc, regEnables(1))
}

class FPToFP(cfg: FuConfig)(implicit p: Parameters) extends FPUPipelineModule(cfg) {

  override def latency: Int = cfg.latency.latencyVal.get

  override val dataModule = Module(new FPToFPDataModule(latency))
  connectDataModule
  dataModule.regEnables <> VecInit((1 to latency) map (i => regEnable(i)))
  connectNonPipedCtrlSingal // Todo: make it piped
}
