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
import yunsuan.vector.alu.{VAluOpcode, VIAlu}
import yunsuan.{VectorElementFormat, VipuType}
import xiangshan.{SelImm, SrcType, XSCoreParamsKey, XSModule}

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
  val vialuWp = Module(new VIAluWrapper)
  val outValid = vialuWp.io.out.valid
  val outFire = vialuWp.io.out.fire()

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
  dataWire := vialuWp.io.out.bits.data
  vialuWp.io.in.bits <> io.in.bits
  vialuWp.io.redirectIn := DontCare  // TODO :
  vialuWp.vxrm := vxrm
  vialuWp.vstart := vstart
  io.out.bits.data :=  Mux(state === s_compute && outFire, dataWire, dataReg)
  io.out.bits.uop := s0_uopReg
  vxsat := vialuWp.vxsat

  vialuWp.io.in.valid := io.in.valid && state === s_idle
  io.out.valid := state === s_compute && outValid || state === s_finish
  vialuWp.io.out.ready := io.out.ready
  io.in.ready := state === s_idle
}

class VIAluDecodeResultBundle extends Bundle {
  val opcode = UInt(6.W)
  val srcType2 = UInt(4.W)
  val srcType1 = UInt(4.W)
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

  // u 00 s 01 f 10 mask 1111
  val uSew = Cat(0.U(2.W), io.in.sew)
  val uSew2 = Cat(0.U(2.W), (io.in.sew+1.U))
  val uSewf2 = Cat(0.U(2.W), (io.in.sew-1.U))
  val uSewf4 = Cat(0.U(2.W), (io.in.sew-2.U))
  val uSewf8 = Cat(0.U(2.W), (io.in.sew-3.U))
  val sSew = Cat(1.U(2.W), io.in.sew)
  val sSew2 = Cat(1.U(2.W), (io.in.sew+1.U))
  val sSewf2 = Cat(1.U(2.W), (io.in.sew - 1.U))
  val sSewf4 = Cat(1.U(2.W), (io.in.sew - 2.U))
  val sSewf8 = Cat(1.U(2.W), (io.in.sew - 3.U))
  val mask = "b1111".U(4.W)

  val out = LookupTree(io.in.fuOpType, List(
    // --------------------- opcode       srcType2 1 vdType
    VipuType.vadd_vv -> Cat(VAluOpcode.vadd, uSew, uSew, uSew).asUInt(),
    VipuType.vsub_vv -> Cat(VAluOpcode.vsub, uSew, uSew, uSew).asUInt(),
    VipuType.vrsub_vv -> Cat(VAluOpcode.vsub, uSew, uSew, uSew).asUInt(),

    VipuType.vwaddu_vv -> Cat(VAluOpcode.vadd, uSew, uSew, uSew2).asUInt(),
    VipuType.vwsubu_vv -> Cat(VAluOpcode.vsub, uSew, uSew, uSew2).asUInt(),
    VipuType.vwadd_vv -> Cat(VAluOpcode.vadd, sSew, sSew, sSew2).asUInt(),
    VipuType.vwsub_vv -> Cat(VAluOpcode.vsub, sSew, sSew, sSew2).asUInt(),
    VipuType.vwaddu_wv -> Cat(VAluOpcode.vadd, uSew2, uSew, uSew2).asUInt(),
    VipuType.vwsubu_wv -> Cat(VAluOpcode.vsub, uSew2, uSew, uSew2).asUInt(),
    VipuType.vwadd_wv -> Cat(VAluOpcode.vadd, sSew2, sSew, sSew2).asUInt(),
    VipuType.vwsub_wv -> Cat(VAluOpcode.vsub, sSew2, sSew, sSew2).asUInt(),

    VipuType.vzext_vf2 -> Cat(VAluOpcode.vext, uSewf2, uSewf2, uSew).asUInt(),
    VipuType.vsext_vf2 -> Cat(VAluOpcode.vext, sSewf2, sSewf2, sSew).asUInt(),
    VipuType.vzext_vf4 -> Cat(VAluOpcode.vext, uSewf4, uSewf4, uSew).asUInt(),
    VipuType.vsext_vf4 -> Cat(VAluOpcode.vext, sSewf4, sSewf4, sSew).asUInt(),
    VipuType.vzext_vf8 -> Cat(VAluOpcode.vext, uSewf8, uSewf8, uSew).asUInt(),
    VipuType.vsext_vf8 -> Cat(VAluOpcode.vext, sSewf8, sSewf8, sSew).asUInt(),

    VipuType.vadc_vvm -> Cat(VAluOpcode.vadc, uSew, uSew, uSew).asUInt(),
    VipuType.vmadc_vvm -> Cat(VAluOpcode.vmadc, uSew, uSew, mask).asUInt(),
    VipuType.vmadc_vv -> Cat(VAluOpcode.vmadc, uSew, uSew, mask).asUInt(),

    VipuType.vsbc_vvm -> Cat(VAluOpcode.vsbc, uSew, uSew, uSew).asUInt(),
    VipuType.vmsbc_vvm -> Cat(VAluOpcode.vmsbc, uSew, uSew, mask).asUInt(),
    VipuType.vmsbc_vv -> Cat(VAluOpcode.vmsbc, uSew, uSew, mask).asUInt(),

    VipuType.vand_vv -> Cat(VAluOpcode.vand, uSew, uSew, uSew).asUInt(),
    VipuType.vor_vv -> Cat(VAluOpcode.vor, uSew, uSew, uSew).asUInt(),
    VipuType.vxor_vv -> Cat(VAluOpcode.vxor, uSew, uSew, uSew).asUInt(),

    VipuType.vsll_vv -> Cat(VAluOpcode.vsll, uSew, uSew, uSew).asUInt(),
    VipuType.vsrl_vv -> Cat(VAluOpcode.vsrl, uSew, uSew, uSew).asUInt(),
    VipuType.vsra_vv -> Cat(VAluOpcode.vsra, uSew, uSew, uSew).asUInt(),

    VipuType.vnsrl_wv -> Cat(VAluOpcode.vsrl, uSew2, uSew, uSew).asUInt(),
    VipuType.vnsra_wv -> Cat(VAluOpcode.vsra, uSew2, uSew, uSew).asUInt(),

    VipuType.vmseq_vv -> Cat(VAluOpcode.vmseq, uSew, uSew, mask).asUInt(),
    VipuType.vmsne_vv -> Cat(VAluOpcode.vmsne, uSew, uSew, mask).asUInt(),
    VipuType.vmsltu_vv -> Cat(VAluOpcode.vmslt, uSew, uSew, mask).asUInt(),
    VipuType.vmslt_vv -> Cat(VAluOpcode.vmslt, sSew, sSew, mask).asUInt(),
    VipuType.vmsleu_vv -> Cat(VAluOpcode.vmsle, uSew, uSew, mask).asUInt(),
    VipuType.vmsle_vv -> Cat(VAluOpcode.vmsle, sSew, sSew, mask).asUInt(),
    VipuType.vmsgtu_vv -> Cat(VAluOpcode.vmsgt, uSew, uSew, mask).asUInt(),
    VipuType.vmsgt_vv -> Cat(VAluOpcode.vmsgt, sSew, sSew, mask).asUInt(),

    VipuType.vminu_vv -> Cat(VAluOpcode.vmin, uSew, uSew, uSew).asUInt(),
    VipuType.vmin_vv -> Cat(VAluOpcode.vmin, sSew, sSew, sSew).asUInt(),
    VipuType.vmaxu_vv -> Cat(VAluOpcode.vmax, uSew, uSew, uSew).asUInt(),
    VipuType.vmax_vv -> Cat(VAluOpcode.vmax, sSew, sSew, sSew).asUInt(),

    VipuType.vmerge_vvm -> Cat(VAluOpcode.vmerge, uSew, uSew, uSew).asUInt(),

    VipuType.vmv_v_v -> Cat(VAluOpcode.vmv, uSew, uSew, uSew).asUInt(),

    VipuType.vsaddu_vv -> Cat(VAluOpcode.vsadd, uSew, uSew, uSew).asUInt(),
    VipuType.vsadd_vv -> Cat(VAluOpcode.vsadd, sSew, sSew, sSew).asUInt(),
    VipuType.vssubu_vv -> Cat(VAluOpcode.vssub, uSew, uSew, uSew).asUInt(),
    VipuType.vssub_vv -> Cat(VAluOpcode.vssub, sSew, sSew, sSew).asUInt(),

    VipuType.vaaddu_vv -> Cat(VAluOpcode.vaadd, uSew, uSew, uSew).asUInt(),
    VipuType.vaadd_vv -> Cat(VAluOpcode.vaadd, sSew, sSew, sSew).asUInt(),
    VipuType.vasubu_vv -> Cat(VAluOpcode.vasub, uSew, uSew, uSew).asUInt(),
    VipuType.vasub_vv -> Cat(VAluOpcode.vasub, sSew, sSew, sSew).asUInt(),

    VipuType.vssrl_vv -> Cat(VAluOpcode.vssrl, uSew, uSew, uSew).asUInt(),
    VipuType.vssra_vv -> Cat(VAluOpcode.vssra, uSew, uSew, uSew).asUInt(),

    VipuType.vnclipu_wv -> Cat(VAluOpcode.vssrl, uSew2, uSew, uSew).asUInt(),
    VipuType.vnclip_wv -> Cat(VAluOpcode.vssra, uSew2, uSew, uSew).asUInt(),

    VipuType.vredsum_vs -> Cat(VAluOpcode.vredsum, uSew, uSew, uSew).asUInt(),
    VipuType.vredmaxu_vs -> Cat(VAluOpcode.vredmax, uSew, uSew, uSew).asUInt(),
    VipuType.vredmax_vs -> Cat(VAluOpcode.vredmax, sSew, sSew, sSew).asUInt(),
    VipuType.vredminu_vs -> Cat(VAluOpcode.vredmin, uSew, uSew, uSew).asUInt(),
    VipuType.vredmin_vs -> Cat(VAluOpcode.vredmin, sSew, sSew, sSew).asUInt(),
    VipuType.vredand_vs -> Cat(VAluOpcode.vredand, uSew, uSew, uSew).asUInt(),
    VipuType.vredor_vs -> Cat(VAluOpcode.vredor, uSew, uSew, uSew).asUInt(),
    VipuType.vredxor_vs -> Cat(VAluOpcode.vredxor, uSew, uSew, uSew).asUInt(),

    VipuType.vwredsumu_vs -> Cat(VAluOpcode.vredsum, uSew, uSew, uSew2).asUInt(),
    VipuType.vwredsum_vs -> Cat(VAluOpcode.vredsum, sSew, sSew, sSew2).asUInt(),

    VipuType.vmand_mm -> Cat(VAluOpcode.vand, mask, mask, mask).asUInt(),
    VipuType.vmnand_mm -> Cat(VAluOpcode.vnand, mask, mask, mask).asUInt(),
    VipuType.vmandn_mm -> Cat(VAluOpcode.vandn, mask, mask, mask).asUInt(),
    VipuType.vmxor_mm -> Cat(VAluOpcode.vxor, mask, mask, mask).asUInt(),
    VipuType.vmor_mm -> Cat(VAluOpcode.vor, mask, mask, mask).asUInt(),
    VipuType.vmnor_mm -> Cat(VAluOpcode.vnor, mask, mask, mask).asUInt(),
    VipuType.vmorn_mm -> Cat(VAluOpcode.vorn, mask, mask, mask).asUInt(),
    VipuType.vmxnor_mm -> Cat(VAluOpcode.vxnor, mask, mask, mask).asUInt(),

    VipuType.vcpop_m -> Cat(VAluOpcode.vcpop, mask, mask, mask).asUInt(),
    VipuType.vfirst_m -> Cat(VAluOpcode.vfirst, mask, mask, mask).asUInt(),
    VipuType.vmsbf_m -> Cat(VAluOpcode.vmsbf, mask, mask, mask).asUInt(),
    VipuType.vmsif_m -> Cat(VAluOpcode.vmsif, mask, mask, mask).asUInt(),
    VipuType.vmsof_m -> Cat(VAluOpcode.vmsof, mask, mask, mask).asUInt(),

    VipuType.viota_m -> Cat(VAluOpcode.viota, mask, mask, uSew).asUInt(),
    VipuType.vid_v -> Cat(VAluOpcode.vid, uSew, uSew, uSew).asUInt(),

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
  val _vs1 = Mux(SrcType.isImm(ctrl.srcType(0)), imm, 
             Mux(in.uop.ctrl.srcType(0) === SrcType.vp, io.in.bits.src(0), VecExtractor(vtype.vsew, io.in.bits.src(0))))
  val _vs2 = in.src(1)
  val vs1 = Mux(VipuType.needReverse(ctrl.fuOpType), _vs2, _vs1)
  val vs2 = Mux(VipuType.needReverse(ctrl.fuOpType), _vs1, _vs2)
  val mask = Mux(VipuType.needClearMask(ctrl.fuOpType), 0.U, in.src(3))

// connect VIAlu
  val decoder = Module(new VIAluDecoder)
  val vialu = Module(new VIAlu)
  decoder.io.in.fuOpType := in.uop.ctrl.fuOpType
  decoder.io.in.sew := in.uop.ctrl.vconfig.vtype.vsew(1,0)

  vialu.io.in.bits.opcode := decoder.io.out.opcode
  vialu.io.in.bits.info.vm := in.uop.ctrl.vm
  vialu.io.in.bits.info.ma := in.uop.ctrl.vconfig.vtype.vma
  vialu.io.in.bits.info.ta := in.uop.ctrl.vconfig.vtype.vta
  vialu.io.in.bits.info.vlmul := in.uop.ctrl.vconfig.vtype.vlmul
  vialu.io.in.bits.info.vl := in.uop.ctrl.vconfig.vl

  vialu.io.in.bits.info.vstart := vstart // TODO :
  vialu.io.in.bits.info.uopIdx := in.uop.ctrl.uopIdx

  vialu.io.in.bits.info.vxrm := vxrm
  vialu.io.in.bits.srcType(0) := decoder.io.out.srcType2
  vialu.io.in.bits.srcType(1) := decoder.io.out.srcType1
  vialu.io.in.bits.vdType := decoder.io.out.vdType
  vialu.io.in.bits.vs1 := vs1
  vialu.io.in.bits.vs2 := vs2
  vialu.io.in.bits.old_vd := in.src(2)
  vialu.io.in.bits.mask := mask

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
