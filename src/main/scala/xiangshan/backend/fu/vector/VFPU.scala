///****************************************************************************************
//  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
//  * Copyright (c) 2020-2021 Peng Cheng Laboratory
//  *
//  * XiangShan is licensed under Mulan PSL v2.
//  * You can use this software according to the terms and conditions of the Mulan PSL v2.
//  * You may obtain a copy of Mulan PSL v2 at:
//  *          http://license.coscl.org.cn/MulanPSL2
//  *
//  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
//  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
//  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//  *
//  * See the Mulan PSL v2 for more details.
//  ****************************************************************************************
//  */
//
//
//package xiangshan.backend.fu.vector
//
//import chipsalliance.rocketchip.config.Parameters
//import chisel3.{Mux, _}
//import chisel3.util._
//import utils._
//import utility._
//import yunsuan.vector.{VectorFloatAdder,VectorFloatFMA,VectorFloatDivider}
//import yunsuan.VfpuType
//import xiangshan.{FuType, XSCoreParamsKey}
//
//class VFPU(implicit p: Parameters) extends VPUSubModule(p(XSCoreParamsKey).VLEN) {
//  XSError(io.in.valid && io.in.bits.uop.ctrl.fuOpType === VfpuType.dummy, "VFPU OpType not supported")
//  XSError(io.in.valid && (io.in.bits.uop.ctrl.vconfig.vtype.vsew === 0.U), "8 bits not supported in VFPU")
//  override val dataModule = Seq(
//    Module(new VfaluWrapper),
//    Module(new VfmaccWrapper),
//    Module(new VfdivWrapper)
//  )
//  val select0 = io.in.bits.uop.ctrl.fuOpType === VfpuType.isVfalu
//  val select1 = io.in.bits.uop.ctrl.fuOpType === VfpuType.isVfmacc
//  val select2 = io.in.bits.uop.ctrl.fuOpType === VfpuType.isVfdiv
//  override val select = Seq(
//    io.in.bits.uop.ctrl.fuType === FuType.vfpu && select0,
//    io.in.bits.uop.ctrl.fuType === FuType.vfpu && select1,
//    io.in.bits.uop.ctrl.fuType === FuType.vfpu && select2
//  )
//  connectDataModule
//}
//
//class VfdivWrapper(implicit p: Parameters)  extends VPUDataModule{
//  needReverse := false.B
//  needClearMask := false.B
//
//  val Latency = List(5, 7, 12)
//  val AdderWidth = XLEN
//  val NumAdder = VLEN / XLEN
//
//  // TODO: Place these logic within the functional unit
//  val inHs = io.in.fire()
//  val s0_mask = DataHoldBypass(in.src(3), inHs)
//  val s0_sew = DataHoldBypass(in.uop.ctrl.vconfig.vtype.vsew(1,0), inHs)
//  val s0_vl = DataHoldBypass(in.uop.ctrl.vconfig.vl, inHs)
//
//  val vfdiv = Seq.fill(NumAdder)(Module(new VectorFloatDivider()))
//  for (i <- 0 until NumAdder) {
//    vfdiv(i).io.opb_i := Mux(inHs, vs1(AdderWidth * (i + 1) - 1, AdderWidth * i), 0.U)
//    vfdiv(i).io.opa_i := Mux(inHs, vs2(AdderWidth * (i + 1) - 1, AdderWidth * i), 0.U)
//    vfdiv(i).io.is_vec_i := true.B // If you can enter, it must be vector
//    vfdiv(i).io.frs2_i    := in.src(0)(63,0) // f[rs2]
//    vfdiv(i).io.frs1_i    := in.src(1)(63,0) // f[rs1]
//    vfdiv(i).io.is_frs1_i := false.B // if true, vs2 / f[rs1]
//    vfdiv(i).io.is_frs2_i := false.B // if true, f[rs2] / vs1
//    vfdiv(i).io.is_sqrt_i := false.B // must false, not support sqrt now
//    vfdiv(i).io.rm_i := rm
//    vfdiv(i).io.fp_format_i := Mux(inHs, in.uop.ctrl.vconfig.vtype.vsew(1,0), 3.U(2.W))
//    vfdiv(i).io.start_valid_i := io.in.valid
//    vfdiv(i).io.finish_ready_i := io.out.ready
//    vfdiv(i).io.flush_i := false.B  // TODO
//  }
//
//  val s4_fflagsVec = VecInit(vfdiv.map(_.io.fflags_o)).asUInt()
//  val s4_fflags16vl = fflagsGen(s0_mask, s4_fflagsVec, List.range(0, 8))
//  val s4_fflags32vl = fflagsGen(s0_mask, s4_fflagsVec, List(0, 1, 4, 5))
//  val s4_fflags64vl = fflagsGen(s0_mask, s4_fflagsVec, List(0, 4))
//  val s4_fflags = LookupTree(s0_sew(1, 0), List(
//    "b01".U -> Mux(s0_vl.orR, s4_fflags16vl(s0_vl - 1.U), 0.U(5.W)),
//    "b10".U -> Mux(s0_vl.orR, s4_fflags32vl(s0_vl - 1.U), 0.U(5.W)),
//    "b11".U -> Mux(s0_vl.orR, s4_fflags64vl(s0_vl - 1.U), 0.U(5.W)),
//  ))
//  val s4_result = VecInit(vfdiv.map(_.io.fpdiv_res_o)).asUInt()
//
//  io.out.bits.data := s4_result
//  fflags := s4_fflags
//  io.in.ready := VecInit(vfdiv.map(_.io.start_ready_o)).asUInt().andR()
//  io.out.valid := VecInit(vfdiv.map(_.io.finish_valid_o)).asUInt().andR()
//}
//
//class VfmaccWrapper(implicit p: Parameters)  extends VPUDataModule{
//  needReverse := false.B
//  needClearMask := false.B
//
//  val Latency = 3
//  val AdderWidth = XLEN
//  val NumAdder = VLEN / XLEN
//
//  val inHs = io.in.fire()
//
//  val validPipe = Seq.fill(Latency)(RegInit(false.B))
//  validPipe.zipWithIndex.foreach {
//    case (valid, idx) =>
//      val _valid = if (idx == 0) Mux(inHs, true.B, false.B) else validPipe(idx - 1)
//      valid := _valid
//  }
//  // TODO: Place these logic within the functional unit
//  val s0_mask = DataHoldBypass(in.src(3), inHs)
//  val s0_sew = DataHoldBypass(in.uop.ctrl.vconfig.vtype.vsew(1, 0), inHs)
//  val s0_vl = DataHoldBypass(in.uop.ctrl.vconfig.vl, inHs)
//
//  val vfmacc = Seq.fill(NumAdder)(Module(new VectorFloatFMA()))
//  for (i <- 0 until NumAdder) {
//    vfmacc(i).io.fp_a := Mux(inHs, vs1(AdderWidth * (i + 1) - 1, AdderWidth * i), 0.U)
//    vfmacc(i).io.fp_b := Mux(inHs, vs2(AdderWidth * (i + 1) - 1, AdderWidth * i), 0.U)
//    vfmacc(i).io.fp_c := Mux(inHs, in.src(2)(AdderWidth * (i + 1) - 1, AdderWidth * i), 0.U)
//    vfmacc(i).io.widen_b := Mux(inHs, Cat(vs1((AdderWidth / 2) * (i + 3) - 1, (AdderWidth / 2) * (i + 2)), vs1((AdderWidth / 2) * (i + 1) - 1, (AdderWidth / 2) * i)), 0.U)
//    vfmacc(i).io.widen_a := Mux(inHs, Cat(vs2((AdderWidth / 2) * (i + 3) - 1, (AdderWidth / 2) * (i + 2)), vs2((AdderWidth / 2) * (i + 1) - 1, (AdderWidth / 2) * i)), 0.U)
//    vfmacc(i).io.frs1 := in.src(0)(63,0)
//    vfmacc(i).io.is_frs1 := false.B // TODO: support vf inst
//    vfmacc(i).io.uop_idx := in.uop.ctrl.uopIdx // TODO
//    vfmacc(i).io.op_code := DontCare // TODO
//    vfmacc(i).io.is_vec := true.B // If you can enter, it must be vector
//    vfmacc(i).io.round_mode := rm
//    vfmacc(i).io.fp_format := Mux(inHs, in.uop.ctrl.vconfig.vtype.vsew(1,0), 3.U(2.W))
//    vfmacc(i).io.res_widening := false.B // TODO
//  }
//
//  // output signal generation
//  val s2_fflagsVec = VecInit(vfmacc.map(_.io.fflags)).asUInt()
//  val s2_fflags16vl = fflagsGen(s0_mask, s2_fflagsVec, List.range(0, 8))
//  val s2_fflags32vl = fflagsGen(s0_mask, s2_fflagsVec, List(0, 1, 4, 5))
//  val s2_fflags64vl = fflagsGen(s0_mask, s2_fflagsVec, List(0, 4))
//  val s2_fflags = LookupTree(s0_sew(1, 0), List(
//    "b01".U -> Mux(s0_vl.orR, s2_fflags16vl(s0_vl - 1.U), 0.U(5.W)),
//    "b10".U -> Mux(s0_vl.orR, s2_fflags32vl(s0_vl - 1.U), 0.U(5.W)),
//    "b11".U -> Mux(s0_vl.orR, s2_fflags64vl(s0_vl - 1.U), 0.U(5.W)),
//  ))
//
//  val s2_result = VecInit(vfmacc.map(_.io.fp_result)).asUInt()
//
//  io.out.bits.data := s2_result
//  fflags := s2_fflags
//
//  io.in.ready := true.B
//  io.out.valid := validPipe(Latency - 1)
//}
//
//class VfaluWrapper(implicit p: Parameters)  extends VPUDataModule{
//  needReverse := false.B
//  needClearMask := false.B
//
//  val Latency = 2
//  val AdderWidth = XLEN
//  val NumAdder = VLEN / XLEN
//
//  val inHs = io.in.fire()
//
//  // reg input signal
//  val validPipe = Seq.fill(Latency)(RegInit(false.B))
//  validPipe.zipWithIndex.foreach {
//    case (valid, idx) =>
//      val _valid = if (idx == 0) Mux(inHs, true.B, false.B) else validPipe(idx - 1)
//      valid := _valid
//  }
//  // TODO: Place these logic within the functional unit
//  val s0_mask = DataHoldBypass(in.src(3), inHs)
//  val s0_sew = DataHoldBypass(in.uop.ctrl.vconfig.vtype.vsew(1, 0), inHs)
//  val s0_vl = DataHoldBypass(in.uop.ctrl.vconfig.vl, inHs)
//
//  // connect the input signal
//  val vfalu = Seq.fill(NumAdder)(Module(new VectorFloatAdder()))
//  for (i <- 0 until NumAdder) {
//    vfalu(i).io.fp_b := Mux(inHs, vs1(AdderWidth * (i + 1) - 1, AdderWidth * i), 0.U)
//    vfalu(i).io.fp_a := Mux(inHs, vs2(AdderWidth * (i + 1) - 1, AdderWidth * i), 0.U)
//    vfalu(i).io.widen_b := Mux(inHs, Cat(vs1((AdderWidth / 2) * (i + 3) - 1, (AdderWidth / 2) * (i + 2)), vs1((AdderWidth / 2) * (i + 1) - 1, (AdderWidth / 2) * i)), 0.U)
//    vfalu(i).io.widen_a := Mux(inHs, Cat(vs2((AdderWidth / 2) * (i + 3) - 1, (AdderWidth / 2) * (i + 2)), vs2((AdderWidth / 2) * (i + 1) - 1, (AdderWidth / 2) * i)), 0.U)
//    vfalu(i).io.frs1 := in.src(0)(63, 0)
//    vfalu(i).io.is_frs1 := false.B // TODO: support vf inst
//    vfalu(i).io.mask := 0.U //TODO
//    vfalu(i).io.uop_idx := in.uop.ctrl.uopIdx // TODO
//    vfalu(i).io.is_vec := true.B // If you can enter, it must be vector
//    vfalu(i).io.round_mode := rm
//    vfalu(i).io.fp_format := Mux(inHs, in.uop.ctrl.vconfig.vtype.vsew(1,0), 3.U(2.W))
//    vfalu(i).io.opb_widening := false.B // TODO
//    vfalu(i).io.res_widening := false.B // TODO
//    vfalu(i).io.op_code := in.uop.ctrl.fuOpType
//  }
//
//  // output signal generation
//  val s0_fflagsVec = VecInit(vfalu.map(_.io.fflags)).asUInt()
//  val s0_fflags16vl = fflagsGen(s0_mask, s0_fflagsVec, List.range(0, 8))
//  val s0_fflags32vl = fflagsGen(s0_mask, s0_fflagsVec, List(0, 1, 4, 5))
//  val s0_fflags64vl = fflagsGen(s0_mask, s0_fflagsVec, List(0, 4))
//  val s0_fflags = LookupTree(s0_sew(1, 0), List(
//    "b01".U -> Mux(s0_vl.orR, s0_fflags16vl(s0_vl - 1.U), 0.U(5.W)),
//    "b10".U -> Mux(s0_vl.orR, s0_fflags32vl(s0_vl - 1.U), 0.U(5.W)),
//    "b11".U -> Mux(s0_vl.orR, s0_fflags64vl(s0_vl - 1.U), 0.U(5.W)),
//  ))
//  val s1_fflags = RegEnable(s0_fflags, validPipe(Latency-2))
//  val s0_result = VecInit(vfalu.map(_.io.fp_result)).asUInt()
//  val s1_result = RegEnable(s0_result, validPipe(Latency-2))
//
//  fflags := s1_fflags
//  io.out.bits.data := s1_result
//
//  io.in.ready := true.B
//  io.out.valid := validPipe(Latency-1)
//}
//
//object fflagsGen{
//  def fflagsGen(vmask: UInt, fflagsResult:UInt, idx:List[Int] = List(0, 1, 4, 5)): Vec[UInt] = {
//    var num = idx.length
//    val fflags = Seq.fill(num)(Wire(UInt(5.W)))
//    fflags.zip(vmask(num-1, 0).asBools().reverse).zip(idx).foreach {
//      case ((fflags0, mask), id) =>
//        fflags0 := Mux(mask, fflagsResult(id*5+4,id*5+0), 0.U)
//    }
//    val fflagsVl = Wire(Vec(num,UInt(5.W)))
//    for (i <- 0 until num) {
//      val _fflags = if (i == 0) fflags(i) else (fflagsVl(i - 1) | fflags(i))
//      fflagsVl(i) := _fflags
//    }
//    fflagsVl
//  }
//
//  def apply(vmask: UInt, fflagsResult:UInt, idx:List[Int] = List(0, 1, 4, 5)): Vec[UInt] = {
//    fflagsGen(vmask, fflagsResult, idx)
//  }
//}