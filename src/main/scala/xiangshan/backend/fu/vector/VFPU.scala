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
import xiangshan.{SrcType, XSCoreParamsKey, XSModule, FuOpType}
import xiangshan.backend.fu.fpu.FPUSubModule

class VFPU(implicit p: Parameters) extends FPUSubModule(p(XSCoreParamsKey).VLEN){
  XSError(io.in.valid && io.in.bits.uop.ctrl.fuOpType === VfpuType.dummy, "VFPU OpType not supported")
  XSError(io.in.valid && (io.in.bits.uop.ctrl.vconfig.vtype.vsew === 0.U), "8 bits not supported in VFPU")
  override val dataModule = null // Only use IO, not dataModule

// rename signal
  val in = io.in.bits
  val ctrl = io.in.bits.uop.ctrl
  val vtype = ctrl.vconfig.vtype
  val src1Type = io.in.bits.uop.ctrl.srcType

// reg input signal
  val s0_uopReg = Reg(io.in.bits.uop.cloneType)
  val s0_maskReg = Reg(UInt(8.W))
  val inHs = io.in.fire()
  when(inHs){
    s0_uopReg := io.in.bits.uop
    s0_maskReg := Fill(8, 1.U(1.W))
  }

// connect the input port of vfalu
  val vfalu = Module(new VfaluWrapper()(p))
  vfalu.io.in.bits.src <> in.src
  vfalu.io.in.bits.srcType <> in.uop.ctrl.srcType
  vfalu.io.in.bits.vmask := Fill(8, 1.U(1.W))
  vfalu.io.in.bits.vl := in.uop.ctrl.vconfig.vl
  vfalu.io.in.bits.round_mode := rm
  vfalu.io.in.bits.fp_format := vtype.vsew(1,0)
  vfalu.io.in.bits.opb_widening := false.B // TODO
  vfalu.io.in.bits.res_widening := false.B // TODO
  vfalu.io.in.bits.op_code := ctrl.fuOpType
  vfalu.io.ready_out.s0_mask := s0_maskReg
  vfalu.io.ready_out.s0_sew := s0_uopReg.ctrl.vconfig.vtype.vsew(1, 0)
  vfalu.io.ready_out.s0_vl := s0_uopReg.ctrl.vconfig.vl

// connect the output port
  fflags := vfalu.io.out.bits.fflags
  io.out.bits.data := vfalu.io.out.bits.result
  io.out.bits.uop := s0_uopReg
  // valid/ready
  vfalu.io.in.valid := io.in.valid
  io.out.valid := vfalu.io.out.valid
  vfalu.io.out.ready := io.out.ready
  io.in.ready := vfalu.io.in.ready
}

class VfaluWrapper(implicit p: Parameters)  extends XSModule{
  val Latency = 2
  val AdderWidth = XLEN
  val NumAdder = VLEN / XLEN

  val io = IO(new Bundle{
    val in = Flipped(DecoupledIO(Output(new Bundle{
      val src = Vec(3, Input(UInt(VLEN.W)))
      val srcType = Vec(4, SrcType())
      val vmask = UInt((VLEN/16).W)
      val vl = UInt(8.W)

      val round_mode = UInt(3.W)
      val fp_format = UInt(2.W) // vsew
      val opb_widening  = Bool()
      val res_widening  = Bool()
      val op_code       = FuOpType()
    })))

    val ready_out = Input(new Bundle {
      val s0_mask = UInt((VLEN / 16).W)
      val s0_sew = UInt(2.W)
      val s0_vl = UInt(8.W)
    })

    val out = DecoupledIO(Output(new Bundle{
      val result = UInt(128.W)
      val fflags = UInt(5.W)
    }))
  })

  val in = io.in.bits
  val out = io.out.bits
  val inHs = io.in.fire()

  // reg input signal
  val validPipe = Seq.fill(Latency)(RegInit(false.B))
  validPipe.zipWithIndex.foreach {
    case (valid, idx) =>
      val _valid = if (idx == 0) Mux(inHs, true.B, false.B) else validPipe(idx - 1)
      valid := _valid
  }
  val s0_mask = io.ready_out.s0_mask
  val s0_sew = io.ready_out.s0_sew
  val s0_vl = io.ready_out.s0_vl

  // connect the input signal
  val vfalu = Seq.fill(NumAdder)(Module(new VectorFloatAdder()))
  val src1 = Mux(in.srcType(0) === SrcType.vp, in.src(0), VecExtractor(in.fp_format, in.src(0)))
  val src2 = Mux(in.srcType(1) === SrcType.vp, in.src(1), VecExtractor(in.fp_format, in.src(1)))
  for (i <- 0 until NumAdder) {
    vfalu(i).io.fp_a := Mux(inHs, src1(AdderWidth * (i + 1) - 1, AdderWidth * i), 0.U)
    vfalu(i).io.fp_b := Mux(inHs, src2(AdderWidth * (i + 1) - 1, AdderWidth * i), 0.U)
    vfalu(i).io.is_vec := true.B // If you can enter, it must be vector
    vfalu(i).io.round_mode := in.round_mode
    vfalu(i).io.fp_format := Mux(inHs, in.fp_format, 3.U(2.W))
    vfalu(i).io.opb_widening := in.opb_widening // TODO
    vfalu(i).io.res_widening := in.res_widening // TODO
    vfalu(i).io.op_code := in.op_code
  }

  // output signal generation
  val s0_fflagsVec = VecInit(vfalu.map(_.io.fflags)).asUInt()
  val s0_fflags16vl = fflagsGen(s0_mask, s0_fflagsVec, List.range(0, 8))
  val s0_fflags32vl = fflagsGen(s0_mask, s0_fflagsVec, List(0, 1, 4, 5))
  val s0_fflags64vl = fflagsGen(s0_mask, s0_fflagsVec, List(0, 4))
  val s0_fflags = LookupTree(s0_sew(1, 0), List(
    "b01".U -> Mux(s0_vl.orR, s0_fflags16vl(s0_vl - 1.U), 0.U(5.W)),
    "b10".U -> Mux(s0_vl.orR, s0_fflags32vl(s0_vl - 1.U), 0.U(5.W)),
    "b11".U -> Mux(s0_vl.orR, s0_fflags64vl(s0_vl - 1.U), 0.U(5.W)),
  ))
  val s1_fflags = RegEnable(s0_fflags, validPipe(Latency-2))
  out.fflags := s1_fflags

  val s0_result = LookupTree(s0_sew(1, 0), List(
    "b01".U -> VecInit(vfalu.map(_.io.fp_f16_result)).asUInt(),
    "b10".U -> VecInit(vfalu.map(_.io.fp_f32_result)).asUInt(),
    "b11".U -> VecInit(vfalu.map(_.io.fp_f64_result)).asUInt(),
  ))
  val s1_result = RegEnable(s0_result, validPipe(Latency-2))
  out.result := s1_result

  io.in.ready := !(validPipe.foldLeft(false.B)(_|_)) && io.out.ready
  io.out.valid := validPipe(Latency-1)
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