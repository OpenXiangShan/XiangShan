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
import utils.XSError
import xiangshan.backend.fu.FunctionUnit
import yunsuan.VppuType
import yunsuan.vector.{FToSIO,FToSModule,Vfslide1upModule}
import xiangshan.XSCoreParamsKey

class VPPU(implicit p: Parameters) extends FunctionUnit(p(XSCoreParamsKey).VLEN) {
  XSError(io.in.valid && io.in.bits.uop.ctrl.fuOpType === VppuType.dummy, "VPPU OpType not supported")

  val uop = io.in.bits.uop

  val f2s = Module(new FToSModule())
  val vfslide1up= Module(new Vfslide1upModule())

  f2s.io.in.valid := io.in.valid
  f2s.io.in.src0 := io.in.bits.src(0)
  f2s.io.in.src1 := io.in.bits.src(2)
  f2s.io.in.vstart := 0.U(32.W) // TODO: when vstart >0 , how to solve the problem
  f2s.io.in.vl := uop.ctrl.vconfig.vl
  f2s.io.in.vsew := uop.ctrl.vconfig.vtype.vsew
  f2s.io.in.vta := uop.ctrl.vconfig.vtype.vta
  f2s.io.in.vma := uop.ctrl.vconfig.vtype.vma
  f2s.io.in.vlmul := uop.ctrl.vconfig.vtype.vlmul
  f2s.io.in.v0 := Fill(128, 1.U(1.W))

  vfslide1up.io.in.valid := io.in.valid
  vfslide1up.io.in.src0 := io.in.bits.src(0)
  vfslide1up.io.in.src1 := io.in.bits.src(1)
  vfslide1up.io.in.vstart := 0.U(32.W) // TODO: when vstart >0 , how to solve the problem
  vfslide1up.io.in.vl := uop.ctrl.vconfig.vl
  vfslide1up.io.in.vsew := uop.ctrl.vconfig.vtype.vsew
  vfslide1up.io.in.vta := uop.ctrl.vconfig.vtype.vta
  vfslide1up.io.in.vma := uop.ctrl.vconfig.vtype.vma
  vfslide1up.io.in.vlmul := uop.ctrl.vconfig.vtype.vlmul
  vfslide1up.io.in.v0 := Fill(128, 1.U(1.W))

  io.out.bits.data := Mux(uop.ctrl.fuOpType === VppuType.f2s, f2s.io.out.vd,
                      Mux(uop.ctrl.fuOpType === VppuType.vslide1up, vfslide1up.io.out.vd, 0.U))
  io.out.bits.uop := io.in.bits.uop
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}