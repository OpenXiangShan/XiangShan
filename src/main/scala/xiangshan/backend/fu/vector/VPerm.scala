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
import chisel3.util.log2Up
import utils._
import yunsuan.VpermType
import xiangshan.{FuType, XSCoreParamsKey}
import yunsuan.vector.perm.Permutation

class VPermWrapper(implicit p: Parameters)  extends VPUDataModule {

  needReverse := false.B
  needClearMask := (VpermType.vcompress === in.uop.ctrl.fuOpType) && (in.uop.ctrl.uopIdx(log2Up(MaxUopSize)-1,1) === 0.U)

  // connect VPerm
  val VPerm = Module(new Permutation)
  VPerm.io.in.bits.opcode := VpermType.getOpcode(in.uop.ctrl.fuOpType).asTypeOf(VPerm.io.in.bits.opcode.cloneType)
  VPerm.io.in.bits.info.vm := in.uop.ctrl.vm
  VPerm.io.in.bits.info.ma := in.uop.ctrl.vconfig.vtype.vma
  VPerm.io.in.bits.info.ta := in.uop.ctrl.vconfig.vtype.vta
  VPerm.io.in.bits.info.vlmul := in.uop.ctrl.vconfig.vtype.vlmul
  VPerm.io.in.bits.info.vl := in.uop.ctrl.vconfig.vl

  VPerm.io.in.bits.info.vstart := vstart
  VPerm.io.in.bits.info.uopIdx := in.uop.ctrl.uopIdx

  VPerm.io.in.bits.info.vxrm := vxrm
  val srcVdType = Wire(new Bundle{
    val srcType2 = UInt(4.W)
    val srcType1 = UInt(4.W)
    val vdType = UInt(4.W)
  })
  srcVdType := VpermType.getSrcVdType(in.uop.ctrl.fuOpType, in.uop.ctrl.vconfig.vtype.vsew(1,0)).asTypeOf(srcVdType.cloneType)
  VPerm.io.in.bits.srcType(0) := srcVdType.srcType2
  src1Sew := srcVdType.srcType1(1,0)
  src1NeedSew := !VpermType.notNeedSew(in.uop.ctrl.fuOpType)
  VPerm.io.in.bits.srcType(1) := srcVdType.srcType1
  VPerm.io.in.bits.vdType := srcVdType.vdType
  // dirty code for VSLIDEUP_VX/VSLIDEDOWN_VX
  VPerm.io.in.bits.vs1 := vs1
  VPerm.io.in.bits.vs2 := vs2
  VPerm.io.in.bits.old_vd := in.src(2)
  VPerm.io.in.bits.mask := mask
  VPerm.io.in.valid := io.in.valid

  // connect io
  io.out.bits.data := VPerm.io.out.vd
  vxsat := VPerm.io.out.vxsat
  io.out.valid := RegNext(io.in.valid)
}

class VPerm(implicit p: Parameters) extends VPUSubModule(p(XSCoreParamsKey).VLEN) {
  XSError(io.in.valid && io.in.bits.uop.ctrl.fuOpType === VpermType.dummy, "VpermType OpType not supported")
  override val dataModule = Seq(
    Module(new VPermWrapper)
  )
  override val select = Seq(
    io.in.bits.uop.ctrl.fuType === FuType.vppu
  )
  connectDataModule
}
