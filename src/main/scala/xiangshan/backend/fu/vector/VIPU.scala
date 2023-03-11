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
import yunsuan.vector.VectorIntAdder
import yunsuan.vector.alu.{VAluOpcode, VIAlu}
import yunsuan.{VectorElementFormat, VipuType}
import xiangshan.{SelImm, SrcType, UopDivType, XSCoreParamsKey, XSModule}

import scala.collection.Seq

class VIPU(implicit p: Parameters) extends VPUSubModule(p(XSCoreParamsKey).VLEN) {
  XSError(io.in.valid && io.in.bits.uop.ctrl.fuOpType === VipuType.dummy, "VIPU OpType not supported")

// extra io
  val vxrm = IO(Input(UInt(2.W)))
  val vxsat = IO(Output(UInt(1.W)))

// def some signal
  val dataReg = Reg(io.out.bits.data.cloneType)
  val dataWire = Wire(dataReg.cloneType)
  val s_idle :: s_compute :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)
  val vialu = Module(new VIAluWrapper)
  val outValid = vialu.io.out.valid
  val outFire = vialu.io.out.fire()

// reg input signal
  val s0_uopReg = Reg(io.in.bits.uop.cloneType)
  val inHs = io.in.fire()
  when(inHs && state === s_idle){
    s0_uopReg := io.in.bits.uop
  }
  dataReg := Mux(outValid, dataWire, dataReg)

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

// connect VIAlu
  dataWire := vialu.io.out.bits.data
  vialu.io.in.bits <> io.in.bits
  vialu.io.redirectIn := DontCare  // TODO :
  vialu.vxrm := vxrm
  io.out.bits.data :=  Mux(state === s_compute && outFire, dataWire, dataReg)
  io.out.bits.uop := s0_uopReg
  vxsat := vialu.vxsat

  vialu.io.in.valid := io.in.valid && state === s_idle
  io.out.valid := state === s_compute && outValid || state === s_finish
  vialu.io.out.ready := io.out.ready
  io.in.ready := state === s_idle
}

class VIAluDecodeResultBundle extends Bundle {
  val opcode = UInt(6.W)
  val srcType = Vec(2, UInt(4.W))
  val vdType = UInt(4.W)
}

class VIAluDecoder (implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle{
    val in = Input(new Bundle{
      val fuOpType = UInt(8.W)
      val sew = UInt(2.W)
    })
    val out = Output(new VIAluDecodeResultBundle)
  })

//  val DecodeDefault = List(VAluOpcode.dummy,  VpuDataType.dummy, VpuDataType.dummy, VpuDataType.dummy)
//  val DecodeTable = Array(
//    BitPat("b" + Cat(VipuType.add, "b00".U).litValue().toString()) -> List(VAluOpcode.vadd,  VpuDataType.s8, VpuDataType.s8, VpuDataType.s8),
//    BitPat("b" + Cat(VipuType.add, "b01".U).litValue().toString()) -> List(VAluOpcode.vadd,  VpuDataType.s16, VpuDataType.s16, VpuDataType.s16),
//    BitPat("b" + Cat(VipuType.add, "b10".U).litValue().toString()) -> List(VAluOpcode.vadd,  VpuDataType.s32, VpuDataType.s32, VpuDataType.s32),
//    BitPat("b" + Cat(VipuType.add, "b11".U).litValue().toString()) -> List(VAluOpcode.vadd,  VpuDataType.s64, VpuDataType.s64, VpuDataType.s64),
//  )
//  val opcode :: srcType1 :: srcType2 :: vdType :: Nil = ListLookup(Cat(io.in.fuOpType, io.in.sew), DecodeDefault, DecodeTable)

// u 00 s 01 f 10 mask 1111
  val out = LookupTree(io.in.fuOpType, List(
    VipuType.add -> Cat(VAluOpcode.vadd, Cat(1.U(2.W), io.in.sew), Cat(1.U(2.W), io.in.sew), Cat(1.U(2.W), io.in.sew)).asUInt()
  )).asTypeOf(new VIAluDecodeResultBundle)

  io.out <> out
}

class VIAluWrapper(implicit p: Parameters)  extends VPUSubModule(p(XSCoreParamsKey).VLEN) {
  XSError(io.in.valid && io.in.bits.uop.ctrl.fuOpType === VipuType.dummy, "VIPU OpType not supported")

// extra io
  val vxrm = IO(Input(UInt(2.W)))
  val vxsat = IO(Output(UInt(1.W)))

// rename signal
  val in = io.in.bits
  val ctrl = in.uop.ctrl
  val vtype = ctrl.vconfig.vtype

// generate src1 and src2
  val imm = VecInit(Seq.fill(VLEN/XLEN)(VecImmExtractor(ctrl.selImm, vtype.vsew, ctrl.imm))).asUInt
  val _src1 = Mux(SrcType.isImm(ctrl.srcType(0)), imm, Mux(ctrl.uopDivType === UopDivType.VEC_MV_LMUL, VecExtractor(vtype.vsew, io.in.bits.src(0)), io.in.bits.src(0)))
  val _src2 = in.src(1)
  val src1 = Mux(VipuType.needReverse(ctrl.fuOpType), _src2, _src1)
  val src2 = Mux(VipuType.needReverse(ctrl.fuOpType), _src1, _src2)

// connect VIAlu
  val decoder = Module(new VIAluDecoder)
  val vialu = Module(new VIAlu)
  decoder.io.in.fuOpType := in.uop.ctrl.fuType
  decoder.io.in.sew := in.uop.ctrl.vconfig.vtype.vsew(1,0)

  vialu.io.in.bits.opcode := decoder.io.out.opcode
  vialu.io.in.bits.info.vm := in.uop.ctrl.vm
  vialu.io.in.bits.info.ma := in.uop.ctrl.vconfig.vtype.vma
  vialu.io.in.bits.info.ta := in.uop.ctrl.vconfig.vtype.vta
  vialu.io.in.bits.info.vlmul := in.uop.ctrl.vconfig.vtype.vlmul
  vialu.io.in.bits.info.vl := in.uop.ctrl.vconfig.vl
  vialu.io.in.bits.info.vstart := 0.U // TODO :
  vialu.io.in.bits.info.uopIdx := in.uop.ctrl.uopIdx
  vialu.io.in.bits.info.vxrm := vxrm
  vialu.io.in.bits.srcType(0) := decoder.io.out.srcType(0)
  vialu.io.in.bits.srcType(1) := decoder.io.out.srcType(1)
  vialu.io.in.bits.vdType := decoder.io.out.vdType
  vialu.io.in.bits.vs1 := src1
  vialu.io.in.bits.vs2 := src2
  vialu.io.in.bits.old_vd := in.src(2)
  vialu.io.in.bits.mask := in.src(3)

  val vdOut = vialu.io.out.bits.vd
  val vxsatOut = vialu.io.out.bits.vxsat

  vialu.io.in.valid := io.in.valid

// connect io
  io.out.bits.data := vdOut
  io.out.bits.uop := DontCare
  vxsat := vxsatOut
  io.out.valid := vialu.io.out.valid
  io.in.ready := DontCare
}

object VecImmExtractor {
  def Imm_OPIVIS(imm: UInt): UInt = {
    SignExt(imm(4,0), 8)
  }
  def Imm_OPIVIU(imm: UInt): UInt = {
    ZeroExt(imm(4,0), 8)
  }

  def imm_sew(sew: UInt, imm: UInt): UInt = {
    val _imm = SignExt(imm(7,0), 64)
    LookupTree(sew(1,0), List(
      "b00".U -> VecInit(Seq.fill(8)(_imm(7,0))).asUInt,
      "b01".U -> VecInit(Seq.fill(4)(_imm(15,0))).asUInt,
      "b10".U -> VecInit(Seq.fill(2)(_imm(31,0))).asUInt,
      "b11".U -> _imm(63,0),
    ))
  }

  def apply(immType: UInt, sew: UInt, imm: UInt): UInt = {
    val _imm = Mux(immType === SelImm.IMM_OPIVIS, Imm_OPIVIS(imm), Imm_OPIVIU(imm))
    imm_sew(sew, _imm(7,0))
  }
}
