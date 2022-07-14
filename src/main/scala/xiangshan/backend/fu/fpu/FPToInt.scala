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
import fudian.FCMP
import utils.SignExt
import xiangshan._


class FPToIntDataModule(latency: Int)(implicit p: Parameters) extends FPUDataModule {
  val regEnables = IO(Input(Vec(latency, Bool())))
  val (src1, src2) = (io.in.src(0), io.in.src(1))

  val ctrl = io.in.fpCtrl

  // stage 1: unbox inputs
  val src1_d_s1 = FPU.unbox(src1, ctrl.typeTagIn)
  val src2_d_s1 = FPU.unbox(src2, ctrl.typeTagIn)

  val src1_d = RegEnable(src1_d_s1, regEnables(0))
  val src2_d = RegEnable(src2_d_s1, regEnables(0))
  val ctrl_reg = RegEnable(ctrl, regEnables(0))
  val rm_reg = RegEnable(rm, regEnables(0))

  val src1_ieee = src1_d_s1
  val move_out = Mux(ctrl.typeTagIn === FPU.S,
    src1_ieee(FPU.f32.len - 1, 0),
    src1_ieee
  )

  def classify(x: UInt, ftype: FPU.FType): UInt = {
    val float = fudian.FloatPoint.fromUInt(x, ftype.expWidth, ftype.precision)
    val decode = float.decode
    val isNormal = !decode.expIsOnes && !decode.expIsZero
    Cat(
      decode.isQNaN,
      decode.isSNaN,
      decode.isInf && !float.sign,
      isNormal && !float.sign,
      decode.isSubnormal && !float.sign,
      decode.isZero && !float.sign,
      decode.isZero && float.sign,
      decode.isSubnormal && float.sign,
      isNormal && float.sign,
      decode.isInf && float.sign
    )
  }

  val classify_out = Mux(ctrl.typeTagIn === FPU.S,
    classify(src1_d_s1(31, 0), FPU.f32),
    classify(src1_d_s1, FPU.f64)
  )

  val mv_cls_out = RegEnable(Mux(rm(0), classify_out, move_out), regEnables(0))

  // stage2
  val mv_cls_out_s2 = RegEnable(mv_cls_out, regEnables(1))

  val scmp = Module(new FCMP(FPU.f32.expWidth, FPU.f32.precision))
  val dcmp = Module(new FCMP(FPU.f64.expWidth, FPU.f64.precision))

  for(mod <- Seq(scmp, dcmp)){
    mod.io.a := src1_d
    mod.io.b := src2_d
    mod.io.signaling := !rm_reg(1)
  }
  val lt = Mux(ctrl_reg.typeTagIn === FPU.S,
    scmp.io.lt,
    dcmp.io.lt
  )
  val eq = Mux(ctrl_reg.typeTagIn === FPU.S,
    scmp.io.eq,
    dcmp.io.eq
  )

  val cmp_out = RegEnable(((~rm_reg).asUInt() & Cat(lt, eq)).orR(), regEnables(1))
  val cmp_exc = RegEnable(Mux(ctrl_reg.typeTagIn === FPU.S,
    scmp.io.fflags,
    dcmp.io.fflags
  ), regEnables(1))

  val s2i = Module(new fudian.FPToInt(FPU.f32.expWidth, FPU.f32.precision))
  val d2i = Module(new fudian.FPToInt(FPU.f64.expWidth, FPU.f64.precision))

  for(f2i <- Seq(s2i, d2i)){
    f2i.io.a := src1_d
    f2i.io.rm := rm_reg
    f2i.io.op := Cat(
      ctrl_reg.typ(1),
      !ctrl_reg.typ(0)
    )
  }

  val conv_out = RegEnable(Mux(ctrl_reg.typeTagIn === FPU.S,
    s2i.io.result,
    d2i.io.result
  ), regEnables(1))
  val conv_exc = RegEnable(Mux(ctrl_reg.typeTagIn === FPU.S,
    s2i.io.fflags,
    d2i.io.fflags
  ), regEnables(1))

  val ctrl_reg_s2 = RegEnable(ctrl_reg, regEnables(1))

  // stage3

  val intData = Wire(UInt(XLEN.W))
  intData := Mux(ctrl_reg_s2.wflags,
    Mux(ctrl_reg_s2.fcvt, conv_out, cmp_out),
    mv_cls_out_s2
  )
  val long = Mux(ctrl_reg_s2.fcvt, ctrl_reg_s2.typ(1), ctrl_reg_s2.fmt(0))
  val intValue = Mux(long,
    SignExt(intData, XLEN),
    SignExt(intData(31, 0), XLEN)
  )

  val exc = Mux(ctrl_reg_s2.fcvt, conv_exc, cmp_exc)

  io.out.data := intValue
  fflags := exc
}

class FPToInt(implicit p: Parameters) extends FPUPipelineModule {

  override def latency = f2iCfg.latency.latencyVal.get

  override val dataModule = Module(new FPToIntDataModule(latency))
  connectDataModule
  dataModule.regEnables <> VecInit((1 to latency) map (i => regEnable(i)))
}
