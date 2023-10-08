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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import fudian.FCMP
import utility.SignExt
import xiangshan._


class FPToIntDataModule(latency: Int)(implicit p: Parameters) extends FPUDataModule {
  val regEnables = IO(Input(Vec(latency, Bool())))
  val (src1, src2) = (io.in.src(0), io.in.src(1))

  val ctrl = io.in.fpCtrl

  // stage 1: unbox inputs
  val src1_d = RegEnable(FPU.unbox(src1, ctrl.typeTagIn), regEnables(0))
  val src2_d = RegEnable(FPU.unbox(src2, ctrl.typeTagIn), regEnables(0))
  val ctrl_reg = RegEnable(ctrl, regEnables(0))
  val rm_reg = RegEnable(rm, regEnables(0))

  // stage2

  val src1_ieee = src1_d
  val move_out = Mux(ctrl_reg.typeTagIn === FPU.S,
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

  val classify_out = Mux(ctrl_reg.typeTagIn === FPU.S,
    classify(src1_d(31, 0), FPU.f32),
    classify(src1_d, FPU.f64)
  )

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

  val cmp_out = ((~rm_reg).asUInt & Cat(lt, eq)).orR
  val cmp_exc = Mux(ctrl_reg.typeTagIn === FPU.S,
    scmp.io.fflags,
    dcmp.io.fflags
  )

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

  val conv_out = Mux(ctrl_reg.typeTagIn === FPU.S,
    s2i.io.result,
    d2i.io.result
  )
  val conv_exc = Mux(ctrl_reg.typeTagIn === FPU.S,
    s2i.io.fflags,
    d2i.io.fflags
  )

  val intData = Wire(UInt(XLEN.W))
  intData := Mux(ctrl_reg.wflags,
    Mux(ctrl_reg.fcvt, conv_out, cmp_out),
    Mux(rm_reg(0), classify_out, move_out)
  )
  val long = Mux(ctrl_reg.fcvt, ctrl_reg.typ(1), ctrl_reg.fmt(0))
  val intValue = RegEnable(Mux(long,
    SignExt(intData, XLEN),
    SignExt(intData(31, 0), XLEN)
  ), regEnables(1))

  val exc = RegEnable(Mux(ctrl_reg.fcvt, conv_exc, cmp_exc), regEnables(1))

  io.out.data := intValue
  fflags := exc
}

class FPToInt(implicit p: Parameters) extends FPUPipelineModule {

  override def latency = f2iCfg.latency.latencyVal.get

  override val dataModule = Module(new FPToIntDataModule(latency))
  connectDataModule
  dataModule.regEnables <> VecInit((1 to latency) map (i => regEnable(i)))
}
