/***************************************************************************************
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
***************************************************************************************/

// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package xiangshan.backend.fu.fpu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{SignExt, ZeroExt}
import xiangshan.backend.fu.{FuConfig, PipedFuncUnit}
import xiangshan.backend.fu.fpu.FpPipedFuncUnit
import yunsuan.scalar

class IntToFP(cfg: FuConfig)(implicit p: Parameters) extends FpPipedFuncUnit(cfg) {
  val ctrl = inCtrl.fpu.get
  val src0 = inData.src(0)
  val ctrlReg = RegEnable(ctrl, regEnable(1))
  val rmReg = RegEnable(rm, regEnable(1))

  val typ = Cat(fuOpType(3), ~fuOpType(0))
  val tag = fuOpType(2, 1)

  val intValue = RegEnable(Mux(ctrl.wfflags,
    Mux(typ(1),
      Mux(typ(0), ZeroExt(src0, XLEN), SignExt(src0, XLEN)),
      Mux(typ(0), ZeroExt(src0(31, 0), XLEN), SignExt(src0(31, 0), XLEN))
    ),
    src0
  ), regEnable(1))

  //stage 2
  val s2_tag = RegEnable(tag, regEnable(1))
  val s2_typ = RegEnable(typ, regEnable(1))
  val s2_wfflags = ctrlReg.wfflags

  val mux = Wire(new Bundle() {
    val data = UInt(XLEN.W)
    val exc = UInt(5.W)
  })

  mux.data := intValue
  mux.exc := 0.U

  when(s2_wfflags){
    val i2fResults = for(t <- FPU.ftypes.take(3)) yield {
      val i2f = Module(new scalar.IntToFP(t.expWidth, t.precision))
      i2f.io.sign := ~s2_typ(0)
      i2f.io.long := s2_typ(1)
      i2f.io.int := intValue
      i2f.io.rm := rmReg
      (i2f.io.result, i2f.io.fflags)
    }
    val (data, exc) = i2fResults.unzip
    mux.data := VecInit(data)(s2_tag)
    mux.exc := VecInit(exc)(s2_tag)
  }

  //stage 3
  val s3_out = RegEnable(mux, regEnable(2))
  val s3_tag = RegEnable(s2_tag, regEnable(2))

  io.out.bits.res.fflags.get := s3_out.exc
  io.out.bits.res.data       := FPU.box(s3_out.data, s3_tag)

}