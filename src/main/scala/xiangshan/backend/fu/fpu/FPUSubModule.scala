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
import xiangshan.{FPUCtrlSignals, XSModule}
import xiangshan.backend.fu.{FunctionUnit, HasPipelineReg}

trait HasUIntToSIntHelper {
  implicit class UIntToSIntHelper(x: UInt){
    def toSInt: SInt = Cat(0.U(1.W), x).asSInt
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

  val rm = io.in.rm
  val fflags = io.out.fflags
}

abstract class FPUSubModule(implicit p: Parameters) extends FunctionUnit
  with HasUIntToSIntHelper
{
  val rm = IO(Input(UInt(3.W)))
  val fflags = IO(Output(UInt(5.W)))
  val dataModule: FPUDataModule
  def connectDataModule = {
    dataModule.io.in.src <> io.in.bits.src
    dataModule.io.in.fpCtrl <> io.in.bits.uop.ctrl.fpu
    dataModule.io.in.rm <> rm
    io.out.bits.data := dataModule.io.out.data
    fflags := dataModule.io.out.fflags
  }
  def invert_sign(x: UInt, len: Int) = {
    Cat(
      !x(len-1), x(len-2, 0)
    )
  }
}

abstract class FPUPipelineModule(implicit p: Parameters)
  extends FPUSubModule
  with HasPipelineReg
