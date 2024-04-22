///****************************************************************************************
// * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
// * Copyright (c) 2020-2021 Peng Cheng Laboratory
// *
// * XiangShan is licensed under Mulan PSL v2.
// * You can use this software according to the terms and conditions of the Mulan PSL v2.
// * You may obtain a copy of Mulan PSL v2 at:
// *          http://license.coscl.org.cn/MulanPSL2
// *
// * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
// *
// * See the Mulan PSL v2 for more details.
// ****************************************************************************************
// */
//
//
//package xiangshan.backend.fu.vector
//
//import org.chipsalliance.cde.config.Parameters
//import chisel3._
//import utils._
//import yunsuan.vector.alu.VIntFixpAlu
//import yunsuan.VialuFixType
//import xiangshan.{XSCoreParamsKey, FuType}
//import yunsuan.vector.{SewOH, MaskExtract}
//
//
//
//class VIAluFixWrapper(implicit p: Parameters)  extends VPUDataModule {
//
//  needReverse := VialuFixType.needReverse(ctrl.fuOpType)
//  needClearMask := VialuFixType.needClearMask(ctrl.fuOpType)
//
//  // connect VIAlu
//  val vIntFixpAlu = Module(new VIntFixpAlu)
//  vIntFixpAlu.io.in.opcode := VialuFixType.getOpcode(in.uop.ctrl.fuOpType).asTypeOf(vIntFixpAlu.io.in.opcode)
//  vIntFixpAlu.io.in.info.vm := in.uop.ctrl.vm
//  vIntFixpAlu.io.in.info.ma := in.uop.ctrl.vconfig.vtype.vma
//  vIntFixpAlu.io.in.info.ta := in.uop.ctrl.vconfig.vtype.vta
//  vIntFixpAlu.io.in.info.vlmul := in.uop.ctrl.vconfig.vtype.vlmul
//  vIntFixpAlu.io.in.info.vl := in.uop.ctrl.vconfig.vl
//
//  vIntFixpAlu.io.in.info.vstart := vstart // TODO :
//  vIntFixpAlu.io.in.info.uopIdx := in.uop.ctrl.uopIdx
//
//  vIntFixpAlu.io.in.info.vxrm := vxrm
//  val srcVdType = Wire(new Bundle{
//    val srcType2 = UInt(4.W)
//    val srcType1 = UInt(4.W)
//    val vdType   = UInt(4.W)
//  })
//  srcVdType := VialuFixType.getSrcVdType(in.uop.ctrl.fuOpType, in.uop.ctrl.vconfig.vtype.vsew(1,0)).asTypeOf(srcVdType.cloneType)
//  vIntFixpAlu.io.in.srcType(0) := srcVdType.srcType2
//  vIntFixpAlu.io.in.srcType(1) := srcVdType.srcType1
//  vIntFixpAlu.io.in.vdType := srcVdType.vdType
//  vIntFixpAlu.io.in.vs1 := vs1
//  vIntFixpAlu.io.in.vs2 := vs2
//  vIntFixpAlu.io.in.old_vd := in.src(2)
//
//  val eewVs1 = SewOH(srcVdType.srcType1(1, 0))
//  val eewVd = SewOH(srcVdType.vdType(1, 0))
//  val uopIdx = in.uop.ctrl.uopIdx
//  val narrow = srcVdType.srcType2(1, 0) === 3.U && srcVdType.vdType(1, 0) === 2.U ||
//    srcVdType.srcType2(1, 0) === 2.U && srcVdType.vdType(1, 0) === 1.U ||
//    srcVdType.srcType2(1, 0) === 1.U && srcVdType.vdType(1, 0) === 0.U
//  val eewVm = Mux(srcVdType.vdType === 15.U, eewVs1, eewVd)
//  val maskIdx = Mux(narrow, uopIdx >> 1, uopIdx)
//  vIntFixpAlu.io.in.mask16b := MaskExtract(mask, maskIdx, eewVm)
//  vIntFixpAlu.io.ctrl.narrow := narrow
//  vIntFixpAlu.io.ctrl.vstart_gte_vl := vstart >= in.uop.ctrl.vconfig.vl
//
//  // connect io
//  io.out.bits.data := vIntFixpAlu.io.out.vd
//  vxsat := vIntFixpAlu.io.out.vxsat
//  io.out.valid := RegNext(io.in.valid)
//}
//
//
