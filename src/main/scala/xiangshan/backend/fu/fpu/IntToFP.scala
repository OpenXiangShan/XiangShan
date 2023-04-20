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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utility.{SignExt, ZeroExt}
import xiangshan.backend.fu.FuConfig

class IntToFPDataModule(latency: Int)(implicit p: Parameters) extends FPUDataModule {
  val regEnables = IO(Input(Vec(latency, Bool())))

  //  stage1
  val ctrl = io.in.fpCtrl
  val in = io.in.src(0)
  val typ = ctrl.typ
  val intValue = RegEnable(Mux(ctrl.wflags,
    Mux(typ(1),
      Mux(typ(0), ZeroExt(in, XLEN), SignExt(in, XLEN)),
      Mux(typ(0), ZeroExt(in(31, 0), XLEN), SignExt(in(31, 0), XLEN))
    ),
    in
  ), regEnables(0))
  val ctrlReg = RegEnable(ctrl, regEnables(0))
  val rmReg = RegEnable(rm, regEnables(0))

  // stage2
  val s2_tag = ctrlReg.typeTagOut
  val s2_wflags = ctrlReg.wflags
  val s2_typ = ctrlReg.typ

  val mux = Wire(new Bundle() {
    val data = UInt(XLEN.W)
    val exc = UInt(5.W)
  })

  mux.data := intValue
  mux.exc := 0.U

  when(s2_wflags){
    val i2fResults = for(t <- FPU.ftypes) yield {
      val i2f = Module(new fudian.IntToFP(t.expWidth, t.precision))
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

  // stage3
  val s3_out = RegEnable(mux, regEnables(1))
  val s3_tag = RegEnable(s2_tag, regEnables(1))

  io.out.fflags := s3_out.exc
  io.out.data := FPU.box(s3_out.data, s3_tag)
}

class IntToFP(cfg: FuConfig)(implicit p: Parameters) extends FPUPipelineModule(cfg) {
  override def latency: Int = cfg.latency.latencyVal.get
  override val dataModule = Module(new IntToFPDataModule(latency))
  connectDataModule
  dataModule.regEnables <> VecInit((1 to latency) map (i => regEnable(i)))
  // connectNonPipedCtrlSingal // Todo: make it piped
}
