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
import fudian.FCMA
import xiangshan._

class FCMA_Module(ftype: FPU.FType)(implicit p: Parameters) extends FPUDataModule {

  val fpCtrl = io.in.fpCtrl
  val typeTagIn = fpCtrl.typeTagIn

  val src1 = FPU.unbox(io.in.src(0), typeTagIn)
  val src2 = FPU.unbox(io.in.src(1), typeTagIn)
  val src3 = FPU.unbox(io.in.src(2), typeTagIn)
  val (in1, in2, in3) = (
    WireInit(src1), WireInit(src2), WireInit(Mux(fpCtrl.isAddSub, src2, src3))
  )
  val one = Cat(
    0.U(1.W),
    fudian.FloatPoint.expBias(FPU.f64.expWidth).U(FPU.f64.expWidth.W),
    0.U(FPU.f64.sigWidth.W)
  )
  val zero = Cat(
    (src1 ^ src2).head(1),
    0.U((FPU.f64.len - 1).W)
  )
  when(fpCtrl.isAddSub){ in2 := one }
  when(!(fpCtrl.isAddSub || fpCtrl.ren3)){ in3 := zero }

  val fma = Module(new FCMA(ftype.expWidth, ftype.precision))
  fma.io.a := in1
  fma.io.b := in2
  fma.io.c := in3
  fma.io.rm := rm

  val (result, exc) = (fma.io.result, fma.io.fflags)

  io.out.data := result
  io.out.fflags := exc

}

class FMADataModule(latency: Int)(implicit p: Parameters) extends FPUDataModule {

  val regEnables = IO(Input(Vec(latency, Bool())))
  val typeTagOut = IO(Input(UInt(2.W)))

  val fpCtrl = io.in.fpCtrl
  val typeTagIn = fpCtrl.typeTagIn

  val sfma = Module(new FCMA_Module(FPU.f32))
  val dfma = Module(new FCMA_Module(FPU.f64))

  for(module <- Seq(sfma, dfma)){
    module.io.in := io.in
  }

  val singleOut = typeTagOut === FPU.S
  val result = Mux(singleOut,
    FPU.box(sfma.io.out.data, FPU.S),
    FPU.box(dfma.io.out.data, FPU.D)
  )
  val exc = Mux(singleOut,
    sfma.fflags,
    dfma.fflags
  )

  val stages = Wire(Vec(latency, new Bundle() {
    val data = UInt(XLEN.W)
    val exc = UInt(5.W)
  }))

  for((s, i) <- stages.zipWithIndex){
    if(i == 0){
      s.data := result
      s.exc := exc
    } else {
      s := RegEnable(stages(i - 1), regEnables(i - 1))
    }
  }

  io.out.data := stages.last.data
  fflags := stages.last.exc

}

class FMA(implicit p: Parameters) extends FPUPipelineModule {
  override def latency: Int = fmacCfg.latency.latencyVal.get

  override val dataModule = Module(new FMADataModule(latency))
  connectDataModule
  dataModule.regEnables <> VecInit((1 to latency) map (i => regEnable(i)))
  dataModule.typeTagOut := io.out.bits.uop.ctrl.fpu.typeTagOut
}
