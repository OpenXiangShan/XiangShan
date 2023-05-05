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
import utils._
import yunsuan.vector.mac.VIMac
import yunsuan.VimacType
import xiangshan.{XSCoreParamsKey, FuType}



class VIMacWrapper(implicit p: Parameters)  extends VPUDataModule {

  needReverse := false.B
  needClearMask := false.B

  // connect VIMac
  val vImac = Module(new VIMac)
  vImac.io.in.valid := io.in.valid
  vImac.io.in.bits.opcode := VimacType.getOpcode(in.uop.ctrl.fuOpType).asTypeOf(vImac.io.in.bits.opcode)
  vImac.io.in.bits.info.vm := in.uop.ctrl.vm
  vImac.io.in.bits.info.ma := in.uop.ctrl.vconfig.vtype.vma
  vImac.io.in.bits.info.ta := in.uop.ctrl.vconfig.vtype.vta
  vImac.io.in.bits.info.vlmul := in.uop.ctrl.vconfig.vtype.vlmul
  vImac.io.in.bits.info.vl := in.uop.ctrl.vconfig.vl
  vImac.io.in.bits.info.vstart := vstart // TODO :
  vImac.io.in.bits.info.uopIdx := in.uop.ctrl.uopIdx
  vImac.io.in.bits.info.vxrm := vxrm

  val srcVdType = Wire(new Bundle{
    val srcType2 = UInt(4.W)
    val srcType1 = UInt(4.W)
    val vdType = UInt(4.W)
  })
  srcVdType := VimacType.getSrcVdType(in.uop.ctrl.fuOpType, in.uop.ctrl.vconfig.vtype.vsew(1,0)).asTypeOf(srcVdType.cloneType)
  vImac.io.in.bits.srcType(0) := srcVdType.srcType2
  vImac.io.in.bits.srcType(1) := srcVdType.srcType1
  vImac.io.in.bits.vdType := srcVdType.vdType
  vImac.io.in.bits.vs1 := vs1
  vImac.io.in.bits.vs2 := vs2
  vImac.io.in.bits.old_vd := in.src(2)
  vImac.io.in.bits.mask := mask

  // connect io
  io.out.bits.data := vImac.io.out.bits.vd
  vxsat := vImac.io.out.bits.vxsat
  io.out.valid := vImac.io.out.valid
}

class VIMacU(implicit p: Parameters) extends VPUSubModule(p(XSCoreParamsKey).VLEN) {
  XSError(io.in.valid && io.in.bits.uop.ctrl.fuOpType === VimacType.dummy, "Vimac OpType not supported")
  override val dataModule = Seq(Module(new VIMacWrapper))
  override val select = Seq(
    io.in.bits.uop.ctrl.fuType === FuType.vimac
  )
  connectDataModule
}
