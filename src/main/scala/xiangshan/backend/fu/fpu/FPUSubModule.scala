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

package xiangshan.backend.fu.fpu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.v2backend.FuConfig
import xiangshan.v2backend.fu.{FuncUnit, HasPipelineReg}
import xiangshan.{FPUCtrlSignals, XSModule}

trait HasUIntToSIntHelper {
  implicit class UIntToSIntHelper(x: UInt){
    def toSInt: SInt = Cat(0.U(1.W), x).asSInt()
  }
}

abstract class FPUDataModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Input(new Bundle() {
      val src = Vec(3, UInt(64.W))
      val fpCtrl = new FPUCtrlSignals
      val rm = UInt(3.W)
    })
    val out = Output(new Bundle() {
      val data = UInt(64.W)
      val fflags = UInt(5.W)
    })
  })

  val rm = Mux(io.in.fpCtrl.rm === "b111".U, io.in.rm, io.in.fpCtrl.rm)
}

abstract class FPUSubModule(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg)
  with HasUIntToSIntHelper
{
  val dataModule: FPUDataModule
  def connectDataModule = {
    for (i <- 0 until dataModule.io.in.src.length) {
      dataModule.io.in.src(i) := (if (i < io.in.bits.src.length) io.in.bits.src(i) else 0.U)
    }
    io.in.bits.fpu.foreach(_ <> dataModule.io.in.fpCtrl)
    dataModule.io.in.rm <> io.frm.get
    io.out.bits.data := dataModule.io.out.data
    io.out.bits.fflags.get := dataModule.io.out.fflags
  }
  def invert_sign(x: UInt, len: Int) = {
    Cat(
      !x(len-1), x(len-2, 0)
    )
  }
}

abstract class FPUPipelineModule(cfg: FuConfig)(implicit p: Parameters)
  extends FPUSubModule(cfg)
  with HasPipelineReg
