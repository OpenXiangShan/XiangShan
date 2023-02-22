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
import chisel3.{Mux, _}
import chisel3.util._
import utils._
import utility._
import yunsuan.vector.VectorFloatAdder
import yunsuan.VfpuType
import xiangshan.{SrcType, XSCoreParamsKey}
import xiangshan.backend.fu.fpu.FPUSubModule

class VFPU(implicit p: Parameters) extends FPUSubModule(p(XSCoreParamsKey).VLEN){
  val Latency = 2
  val AdderWidth = XLEN
  val NumAdder = VLEN / XLEN
  XSError(io.in.valid && io.in.bits.uop.ctrl.fuOpType === VfpuType.dummy, "VFPU OpType not supported")
  XSError(io.in.valid && (io.in.bits.uop.ctrl.vconfig.vtype.vsew === 0.U), "8 bits not supported in FToVFPU")
  override val dataModule = null // Only use IO, not dataModule

// rename signal
  val ctrl = io.in.bits.uop.ctrl
  val vtype = ctrl.vconfig.vtype
  val src1Type = io.in.bits.uop.ctrl.srcType

// reg input signal
  val uopReg0 = Reg(io.in.bits.uop.cloneType)
  val valid0 = Seq.fill(Latency)(RegInit(false.B))
  val inHs = io.in.fire()
  when(inHs){
    uopReg0 := io.in.bits.uop
  }
  valid0.zipWithIndex.foreach{
    case (valid, idx) =>
      val _valid = if (idx == 0) Mux(inHs, true.B,false.B) else valid0(idx-1)
      valid := _valid
  }

  val vmask = Fill(8, 1.U(1.W)) // TODO:

// connect the input port of VectorFloatAdder
  val adder = Seq.fill(NumAdder)(Module(new VectorFloatAdder()))
  val src1 = Mux(src1Type(0) === SrcType.vp, io.in.bits.src(0), VecExtractor(vtype.vsew, io.in.bits.src(0)))
  val src2 = Mux(src1Type(1) === SrcType.vp, io.in.bits.src(1), VecExtractor(vtype.vsew, io.in.bits.src(1)))
  for(i <- 0 until NumAdder) {
    adder(i).io.fp_a := Mux(inHs, src1(AdderWidth*(i+1)-1, AdderWidth*i), 0.U)
    adder(i).io.fp_b := Mux(inHs, src2(AdderWidth*(i+1)-1, AdderWidth*i), 0.U)
    adder(i).io.is_vec := true.B // If you can enter, it must be vector
    adder(i).io.round_mode := rm
    adder(i).io.fp_format := Mux(inHs, vtype.vsew(1,0), 3.U(2.W))
    adder(i).io.opb_widening := false.B // TODO
    adder(i).io.res_widening := false.B // TODO
    adder(i).io.op_code := Mux(inHs, ctrl.fuOpType, VfpuType.dummy)
  }

// generate the output : s1_fflags_result s1_adder_result 
  val fflagsResult = VecInit(adder.map(_.io.fflags)).asUInt()
  val fflags16vl = fflagsGen(vmask, fflagsResult, List.range(0,8))
  val fflags32vl = fflagsGen(vmask, fflagsResult, List(0,1,4,5))
  val fflags64vl = fflagsGen(vmask, fflagsResult, List(0,4))
  val s0_sew = uopReg0.ctrl.vconfig.vtype.vsew
  val s0_vl = uopReg0.ctrl.vconfig.vl
  val s0_fflags_result = LookupTree(s0_sew(1, 0), List(
    "b01".U -> Mux(s0_vl.orR, fflags16vl(s0_vl-1.U),0.U(5.W)),
    "b10".U -> Mux(s0_vl.orR, fflags32vl(s0_vl-1.U),0.U(5.W)),
    "b11".U -> Mux(s0_vl.orR, fflags64vl(s0_vl-1.U),0.U(5.W)),
  ))
  val s1_fflags_result = RegEnable(s0_fflags_result, valid0(Latency-2))

  val s0_adder_result = LookupTree(s0_sew(1, 0), List(
    "b01".U -> VecInit(adder.map(_.io.fp_f16_result)).asUInt(),
    "b10".U -> VecInit(adder.map(_.io.fp_f32_result)).asUInt(),
    "b11".U -> VecInit(adder.map(_.io.fp_f64_result)).asUInt(),
  ))
  val s1_adder_result = RegEnable(s0_adder_result, valid0(Latency-2))

// connect the output port
  fflags := s1_fflags_result

  io.out.bits.data := s1_adder_result
  io.out.bits.uop := uopReg0

  io.out.valid := valid0(Latency-1)
  io.in.ready := !(valid0.foldLeft(false.B)(_|_)) && io.out.ready
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