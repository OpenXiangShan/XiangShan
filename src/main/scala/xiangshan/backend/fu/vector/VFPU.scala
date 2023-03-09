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
import yunsuan.vector.{VectorFloatAdder,VectorFloatFMA,VectorFloatDivider}
import yunsuan.VfpuType
import xiangshan.{FuOpType, SrcType, XSBundle, XSCoreParamsKey, XSModule}
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

// def some signal
  val fflagsReg = RegInit(0.U(5.W))
  val fflagsWire = WireInit(0.U(5.W))
  val dataReg = Reg(io.out.bits.data.cloneType)
  val dataWire = Wire(dataReg.cloneType)
  val s_idle :: s_compute :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)
  val vfalu = Module(new VfaluWrapper()(p))
  val vfmacc = Module(new VfmaccWrapper()(p))
  val vfdiv = Module(new VfdivWrapper()(p))
  val outValid = vfalu.io.out.valid || vfmacc.io.out.valid || vfdiv.io.out.valid
  val outFire = vfalu.io.out.fire() || vfmacc.io.out.fire() || vfdiv.io.out.fire()

// reg input signal
  val s0_uopReg = Reg(io.in.bits.uop.cloneType)
  val s0_maskReg = Reg(UInt(8.W))
  val inHs = io.in.fire()
  when(inHs && state===s_idle){
    s0_uopReg := io.in.bits.uop
    s0_maskReg := Fill(8, 1.U(1.W))
  }

// fsm
  switch (state) {
    is (s_idle) {
      state := Mux(inHs, s_compute, s_idle)
    }
    is (s_compute) {
      state := Mux(outValid, Mux(outFire, s_idle, s_finish),
                             s_compute)
    }
    is (s_finish) {
      state := Mux(io.out.fire(), s_idle, s_finish)
    }
  }
  fflagsReg := Mux(outValid, fflagsWire, fflagsReg)
  dataReg := Mux(outValid, dataWire, dataReg)

// connect the input port of vfalu
  vfalu.io.in.bits.src <> in.src
  vfalu.io.in.bits.srcType <> in.uop.ctrl.srcType
  vfalu.io.in.bits.round_mode := rm
  vfalu.io.in.bits.fp_format := vtype.vsew(1,0)
  vfalu.io.in.bits.opb_widening := false.B // TODO
  vfalu.io.in.bits.res_widening := false.B // TODO
  vfalu.io.in.bits.op_code := ctrl.fuOpType
  vfalu.io.ready_out.s0_mask := s0_maskReg
  vfalu.io.ready_out.s0_sew := s0_uopReg.ctrl.vconfig.vtype.vsew(1, 0)
  vfalu.io.ready_out.s0_vl := s0_uopReg.ctrl.vconfig.vl

//  connect the input port of vfmacc
  vfmacc.io.in.bits.src <> in.src
  vfmacc.io.in.bits.srcType <> in.uop.ctrl.srcType
  vfmacc.io.in.bits.round_mode := rm
  vfmacc.io.in.bits.fp_format := vtype.vsew(1, 0)
  vfmacc.io.in.bits.opb_widening := DontCare // TODO
  vfmacc.io.in.bits.res_widening := false.B // TODO
  vfmacc.io.in.bits.op_code := DontCare
  vfmacc.io.ready_out.s0_mask := s0_maskReg
  vfmacc.io.ready_out.s0_sew := s0_uopReg.ctrl.vconfig.vtype.vsew(1, 0)
  vfmacc.io.ready_out.s0_vl := s0_uopReg.ctrl.vconfig.vl

  //  connect the input port of vfdiv
  vfdiv.io.in.bits.src <> in.src
  vfdiv.io.in.bits.srcType <> in.uop.ctrl.srcType
  vfdiv.io.in.bits.round_mode := rm
  vfdiv.io.in.bits.fp_format := vtype.vsew(1, 0)
  vfdiv.io.in.bits.opb_widening := DontCare // TODO
  vfdiv.io.in.bits.res_widening := DontCare // TODO
  vfdiv.io.in.bits.op_code := DontCare
  vfdiv.io.ready_out.s0_mask := s0_maskReg
  vfdiv.io.ready_out.s0_sew := s0_uopReg.ctrl.vconfig.vtype.vsew(1, 0)
  vfdiv.io.ready_out.s0_vl := s0_uopReg.ctrl.vconfig.vl

// connect the output port
  fflagsWire := LookupTree(s0_uopReg.ctrl.fuOpType, List(
    VfpuType.fadd  -> vfalu.io.out.bits.fflags,
    VfpuType.fsub  -> vfalu.io.out.bits.fflags,
    VfpuType.fmacc -> vfmacc.io.out.bits.fflags,
    VfpuType.fdiv  -> vfdiv.io.out.bits.fflags,
  ))
  fflags := Mux(state === s_compute && outFire, fflagsWire, fflagsReg)
  dataWire := LookupTree(s0_uopReg.ctrl.fuOpType, List(
    VfpuType.fadd -> vfalu.io.out.bits.result,
    VfpuType.fsub -> vfalu.io.out.bits.result,
    VfpuType.fmacc -> vfmacc.io.out.bits.result,
    VfpuType.fdiv -> vfdiv.io.out.bits.result,
  ))
  io.out.bits.data := Mux(state === s_compute && outFire, dataWire, dataReg)
  io.out.bits.uop := s0_uopReg
  // valid/ready
  vfalu.io.in.valid := io.in.valid && VfpuType.isVfalu(in.uop.ctrl.fuOpType) && state === s_idle
  vfmacc.io.in.valid := io.in.valid && in.uop.ctrl.fuOpType === VfpuType.fmacc && state === s_idle
  vfdiv.io.in.valid := io.in.valid && in.uop.ctrl.fuOpType === VfpuType.fdiv && state === s_idle
  io.out.valid := state === s_compute && outValid || state === s_finish
  vfalu.io.out.ready := io.out.ready
  vfmacc.io.out.ready := io.out.ready
  vfdiv.io.out.ready := io.out.ready
  io.in.ready := state === s_idle
}

class VFPUWraaperBundle (implicit p: Parameters)  extends XSBundle{
  val in = Flipped(DecoupledIO(Output(new Bundle {
    val src = Vec(4, Input(UInt(VLEN.W)))
    val srcType = Vec(4, SrcType())

    val round_mode = UInt(3.W)
    val fp_format = UInt(2.W) // vsew
    val opb_widening = Bool()
    val res_widening = Bool()
    val op_code = FuOpType()
  })))

  val ready_out = Input(new Bundle {
    val s0_mask = UInt((VLEN / 16).W)
    val s0_sew = UInt(2.W)
    val s0_vl = UInt(8.W)
  })

  val out = DecoupledIO(Output(new Bundle {
    val result = UInt(128.W)
    val fflags = UInt(5.W)
  }))
}

class VfdivWrapper(implicit p: Parameters)  extends XSModule{
  val Latency = List(5, 7, 12)
  val AdderWidth = XLEN
  val NumAdder = VLEN / XLEN

  val io = IO(new VFPUWraaperBundle)

  val in = io.in.bits
  val out = io.out.bits
  val inHs = io.in.fire()

  val s0_mask = io.ready_out.s0_mask
  val s0_sew = io.ready_out.s0_sew
  val s0_vl = io.ready_out.s0_vl

  val vfdiv = Seq.fill(NumAdder)(Module(new VectorFloatDivider()))
  val src1 = Mux(in.srcType(0) === SrcType.vp, in.src(0), VecExtractor(in.fp_format, in.src(0)))
  val src2 = Mux(in.srcType(1) === SrcType.vp, in.src(1), VecExtractor(in.fp_format, in.src(1)))
  for (i <- 0 until NumAdder) {
    vfdiv(i).io.opa_i := Mux(inHs, src2(AdderWidth * (i + 1) - 1, AdderWidth * i), 0.U)
    vfdiv(i).io.opb_i := Mux(inHs, src1(AdderWidth * (i + 1) - 1, AdderWidth * i), 0.U)
    vfdiv(i).io.is_vec_i := true.B // If you can enter, it must be vector
    vfdiv(i).io.rm_i := in.round_mode
    vfdiv(i).io.fp_format_i := Mux(inHs, in.fp_format, 3.U(2.W))
    vfdiv(i).io.start_valid_i := io.in.valid
    vfdiv(i).io.finish_ready_i := io.out.ready
    vfdiv(i).io.flush_i := false.B  // TODO
  }

  val s4_fflagsVec = VecInit(vfdiv.map(_.io.fflags_o)).asUInt()
  val s4_fflags16vl = fflagsGen(s0_mask, s4_fflagsVec, List.range(0, 8))
  val s4_fflags32vl = fflagsGen(s0_mask, s4_fflagsVec, List(0, 1, 4, 5))
  val s4_fflags64vl = fflagsGen(s0_mask, s4_fflagsVec, List(0, 4))
  val s4_fflags = LookupTree(s0_sew(1, 0), List(
    "b01".U -> Mux(s0_vl.orR, s4_fflags16vl(s0_vl - 1.U), 0.U(5.W)),
    "b10".U -> Mux(s0_vl.orR, s4_fflags32vl(s0_vl - 1.U), 0.U(5.W)),
    "b11".U -> Mux(s0_vl.orR, s4_fflags64vl(s0_vl - 1.U), 0.U(5.W)),
  ))
  out.fflags := s4_fflags

  val s4_result = VecInit(vfdiv.map(_.io.fpdiv_res_o)).asUInt()
  out.result := s4_result

  io.in.ready := VecInit(vfdiv.map(_.io.start_ready_o)).asUInt().andR()
  io.out.valid := VecInit(vfdiv.map(_.io.finish_valid_o)).asUInt().andR()
}

class VfmaccWrapper(implicit p: Parameters)  extends XSModule{
  val Latency = 3
  val AdderWidth = XLEN
  val NumAdder = VLEN / XLEN

  val io = IO(new VFPUWraaperBundle)

  val in = io.in.bits
  val out = io.out.bits
  val inHs = io.in.fire()

  val validPipe = Seq.fill(Latency)(RegInit(false.B))
  validPipe.zipWithIndex.foreach {
    case (valid, idx) =>
      val _valid = if (idx == 0) Mux(inHs, true.B, false.B) else validPipe(idx - 1)
      valid := _valid
  }
  val s0_mask = io.ready_out.s0_mask
  val s0_sew = io.ready_out.s0_sew
  val s0_vl = io.ready_out.s0_vl

  val vfmacc = Seq.fill(NumAdder)(Module(new VectorFloatFMA()))
  val src1 = Mux(in.srcType(0) === SrcType.vp, in.src(0), VecExtractor(in.fp_format, in.src(0)))
  val src2 = Mux(in.srcType(1) === SrcType.vp, in.src(1), VecExtractor(in.fp_format, in.src(1)))
  val src3 = Mux(in.srcType(2) === SrcType.vp, in.src(2), VecExtractor(in.fp_format, in.src(2)))
  for (i <- 0 until NumAdder) {
    vfmacc(i).io.fp_a := Mux(inHs, src1(AdderWidth * (i + 1) - 1, AdderWidth * i), 0.U)
    vfmacc(i).io.fp_b := Mux(inHs, src2(AdderWidth * (i + 1) - 1, AdderWidth * i), 0.U)
    vfmacc(i).io.fp_c := Mux(inHs, src3(AdderWidth * (i + 1) - 1, AdderWidth * i), 0.U)
    vfmacc(i).io.is_vec := true.B // If you can enter, it must be vector
    vfmacc(i).io.round_mode := in.round_mode
    vfmacc(i).io.fp_format := Mux(inHs, in.fp_format, 3.U(2.W))
    vfmacc(i).io.res_widening := in.res_widening // TODO
  }

  // output signal generation
  val s2_fflagsVec = VecInit(vfmacc.map(_.io.fflags)).asUInt()
  val s2_fflags16vl = fflagsGen(s0_mask, s2_fflagsVec, List.range(0, 8))
  val s2_fflags32vl = fflagsGen(s0_mask, s2_fflagsVec, List(0, 1, 4, 5))
  val s2_fflags64vl = fflagsGen(s0_mask, s2_fflagsVec, List(0, 4))
  val s2_fflags = LookupTree(s0_sew(1, 0), List(
    "b01".U -> Mux(s0_vl.orR, s2_fflags16vl(s0_vl - 1.U), 0.U(5.W)),
    "b10".U -> Mux(s0_vl.orR, s2_fflags32vl(s0_vl - 1.U), 0.U(5.W)),
    "b11".U -> Mux(s0_vl.orR, s2_fflags64vl(s0_vl - 1.U), 0.U(5.W)),
  ))
  out.fflags := s2_fflags

  val s2_result = VecInit(vfmacc.map(_.io.fp_result)).asUInt()
  out.result := s2_result

  io.in.ready := true.B
  io.out.valid := validPipe(Latency - 1)
}

class VfaluWrapper(implicit p: Parameters)  extends XSModule{
  val Latency = 2
  val AdderWidth = XLEN
  val NumAdder = VLEN / XLEN

  val io = IO(new VFPUWraaperBundle)

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

  io.in.ready := true.B
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