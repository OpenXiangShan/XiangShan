/****************************************************************************************
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
  ****************************************************************************************
  */


package xiangshan.backend.fu.vector

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import yunsuan.vector.VectorFloatAdder
import yunsuan.{OpType, VectorElementFormat, VfpuType}
import xiangshan.{FuType, MicroOp, MulDivExeUnitCfg, SrcType, XSCoreParamsKey}
import xiangshan.backend.fu.fpu.FPUSubModule

import scala.collection.Seq

class VFPU(implicit p: Parameters) extends FPUSubModule(p(XSCoreParamsKey).VLEN){
  val AdderWidth = XLEN
  val NumAdder = VLEN / XLEN
  XSError(io.in.valid && io.in.bits.uop.ctrl.fuOpType === VfpuType.dummy, "VFPU OpType not supported")
  XSError(io.in.valid && (io.in.bits.uop.ctrl.vconfig.vtype.vsew === 0.U), "8 bits not supported in FToVFPU")
  override val dataModule = null // Only use IO, not dataModule

  val uop = io.in.bits.uop
  val ctrl = uop.ctrl
  val vtype = ctrl.vconfig.vtype
  val src1Type = io.in.bits.uop.ctrl.srcType

  val pipe_valid = RegInit(VecInit(Seq.fill(2)(false.B)))
  val pipe_ready = WireInit(VecInit(Seq.fill(2)(true.B)))

  when(pipe_valid(0) & pipe_ready(1)){pipe_valid(0) := false.B}
  when(io.in.valid & pipe_ready(0)){pipe_valid(0) := true.B}
  when(pipe_valid(1) & io.out.ready) {pipe_valid(1) := false.B}
  when(pipe_valid(0) & pipe_ready(1)) {pipe_valid(1) := true.B}

  pipe_ready(1) := !pipe_valid(1) || io.out.ready
  pipe_ready(0) := !pipe_valid(0) || pipe_ready(1)

  val uopReg = Seq.fill(2)(Reg(new MicroOp))
  val v0 = Seq.fill(2)(Reg(UInt(16.W)))
  val pipe0Hs = io.in.valid && pipe_ready(0)
  val pipe1Hs = pipe_valid(0) && pipe_ready(1)
  uopReg(0) := DataHoldBypass(io.in.bits.uop, pipe0Hs)
  uopReg(1) := Mux(pipe1Hs, uopReg(0), uopReg(1))

  v0(0) := DataHoldBypass(Fill(8, 1.U(1.W)), pipe0Hs)
  v0(1) := Mux(pipe1Hs,v0(0), v0(1))
  val vmask = v0(1)

  val src1 = Mux(src1Type(0) === SrcType.vp, io.in.bits.src(0), VecExtractor(vtype.vsew, io.in.bits.src(0)))
  val src2 = Mux(src1Type(1) === SrcType.vp, io.in.bits.src(1), VecExtractor(vtype.vsew, io.in.bits.src(1)))

  val adder = Seq.fill(NumAdder)(Module(new VectorFloatAdder()))

  for(i <- 0 until NumAdder) {
    adder(i).io.fp_a := DataHoldBypass(src1(AdderWidth*(i+1)-1, AdderWidth*i), pipe0Hs)
    adder(i).io.fp_b := DataHoldBypass(src2(AdderWidth*(i+1)-1, AdderWidth*i), pipe0Hs)
    adder(i).io.is_vec := DataHoldBypass(true.B, pipe0Hs) // If you can enter, it must be vector
    adder(i).io.round_mode := DataHoldBypass(rm, pipe0Hs)
    val fp_format0 = DataHoldBypass(vtype.vsew(1,0), pipe0Hs)
    adder(i).io.fp_format := Mux(fp_format0.orR,fp_format0,3.U(2.W))
    adder(i).io.opb_widening := DataHoldBypass(false.B, pipe0Hs) // TODO
    adder(i).io.res_widening := DataHoldBypass(false.B, pipe0Hs) // TODO
    adder(i).io.op_code := DataHoldBypass(ctrl.fuOpType, pipe0Hs)
  }

  val adder_result = RegEnable(Mux1H(Seq(
    (vtype.vsew === 1.U) -> VecInit(adder.map(_.io.fp_f16_result)).asUInt(),
    (vtype.vsew === 2.U) -> VecInit(adder.map(_.io.fp_f32_result)).asUInt(),
    (vtype.vsew === 3.U) -> VecInit(adder.map(_.io.fp_f64_result)).asUInt(),
  )), pipe1Hs)

  val fflagsResult = VecInit(adder.map(_.io.fflags)).asUInt()
  val fflags16vl = fflagsGen(vmask, fflagsResult, List.range(0,8))
  val fflags32vl = fflagsGen(vmask, fflagsResult, List(0,1,4,5))
  val fflags64vl = fflagsGen(vmask, fflagsResult, List(0,4))

  val s0_sew = uopReg(0).asTypeOf(uop.cloneType).ctrl.vconfig.vtype.vsew
  val s0_vl = uopReg(0).ctrl.vconfig.vl
  val fflags_result = RegEnable(LookupTree(s0_sew(1, 0), List(
    "b01".U -> Mux(s0_vl.orR, fflags16vl(s0_vl-1.U),0.U(5.W)),
    "b10".U -> Mux(s0_vl.orR, fflags32vl(s0_vl-1.U),0.U(5.W)),
    "b11".U -> Mux(s0_vl.orR, fflags64vl(s0_vl-1.U),0.U(5.W)),
  )), pipe1Hs)

  fflags := fflags_result

  io.out.bits.data := adder_result
  io.out.bits.uop := uopReg(1)

  io.out.valid := pipe_valid(1)
  io.in.ready := pipe_ready(0)
}

object fflagsGen{
  def fflagsGen(vmask: UInt, fflagsResult:UInt, idx:List[Int] = List(0, 1, 4, 5)): Vec[UInt] = {
    var num = idx.length
    val fflags = Seq.fill(num)(Wire(UInt(5.W)))
    fflags.zip(vmask(num-1, 0).asBools().reverse).zip(idx).foreach {
      case ((fflags0, mask), id) =>
        fflags0 := Mux(mask, fflagsResult(id*5+4,id*5+0), 0.U)
    }
    val fflagsVl = Wire(Vec(num,UInt(5.W)))
    for (i <- 0 until num) {
      val _fflags = if (i == 0) fflags(i) else (fflagsVl(i - 1) | fflags(i))
      fflagsVl(i) := _fflags
    }
    fflagsVl
  }

  def apply(vmask: UInt, fflagsResult:UInt, idx:List[Int] = List(0, 1, 4, 5)): Vec[UInt] = {
    fflagsGen(vmask, fflagsResult, idx)
  }
}

object VecExtractor{
  def xf2v_sew(sew: UInt, xf:UInt): UInt = {
    LookupTree(sew(1, 0), List(
      "b00".U -> VecInit(Seq.fill(16)(xf(7, 0))).asUInt,
      "b01".U -> VecInit(Seq.fill(8)(xf(15, 0))).asUInt,
      "b10".U -> VecInit(Seq.fill(4)(xf(31, 0))).asUInt,
      "b11".U -> VecInit(Seq.fill(2)(xf(63, 0))).asUInt,
    ))
  }

  def apply(sew: UInt, xf: UInt): UInt = {
    xf2v_sew(sew, xf)
  }
}