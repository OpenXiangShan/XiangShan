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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.hierarchy.{Definition, Instance, instantiable, public}
import chisel3.util._
import fudian.FDIV
import utility.MaskExpand

import scala.collection.mutable

/*
    Because fdiv use the decoder and decoder has 'Dedup' bug now,
    we use hierarchy API to force FDIV be deduped to avoid the bug.
 */
object FDivGen {
  val defMap = new mutable.HashMap[FPU.FType, Definition[InstantiableFDIV]]()
  def apply(t: FPU.FType) = {
    val divDef = defMap.getOrElseUpdate(t, Definition(new InstantiableFDIV(t)))
    Instance(divDef)
  }
}

@instantiable
class InstantiableFDIV(t: FPU.FType) extends Module {

  val div = Module(new FDIV(t.expWidth, t.precision))

  @public val io = IO(chiselTypeOf(div.io))

  io <> div.io

}

class FDivSqrtDataModule(implicit p: Parameters) extends FPUDataModule {
  val in_valid, out_ready = IO(Input(Bool()))
  val in_ready, out_valid = IO(Output(Bool()))
  val kill_w = IO(Input(Bool()))
  val kill_r = IO(Input(Bool()))

  val in_fire = in_valid && in_ready
  val out_fire = out_valid && out_ready

  val fpCtrl = io.in.fpCtrl
  val tag = fpCtrl.typeTagIn
  val src1 = FPU.unbox(io.in.src(0), tag)
  val src2 = FPU.unbox(io.in.src(1), tag)

  val typeSel = VecInit(FPU.ftypes.zipWithIndex.map(_._2.U === tag))
  val outSel = RegEnable(typeSel, VecInit.fill(typeSel.length)(true.B), in_fire)  // inelegant
  val outDataSel = RegEnable(MaskExpand(typeSel, 64), in_fire)

  val divSqrt = FPU.ftypes.map{ t =>
    val fdiv = FDivGen(t)
    fdiv.io.a := src1
    fdiv.io.b := src2
    fdiv.io.rm := rm
    fdiv.io.specialIO.in_valid := in_fire && !kill_w && (FPU.ftypes.indexOf(t).U === tag)
    fdiv.io.specialIO.out_ready := out_ready
    fdiv.io.specialIO.isSqrt := fpCtrl.sqrt
    fdiv.io.specialIO.kill := kill_r
    fdiv
  }

  in_ready := divSqrt.map(_.io.specialIO.in_ready).foldRight(true.B)(_ && _)
  out_valid := Mux1H(outSel, divSqrt.map(_.io.specialIO.out_valid))
  io.out.data := outDataSel.zip(divSqrt.zip(FPU.ftypes).map{
    case (mod, t) => FPU.box(mod.io.result, t)
  }).map(x => x._1 & x._2).reduce(_ | _)
  fflags := Mux1H(outSel, divSqrt.map(_.io.fflags))
}

class FDivSqrt(implicit p: Parameters) extends FPUSubModule {

  val uopReg = RegEnable(io.in.bits.uop, io.in.fire)
  val kill_r = !io.in.ready && uopReg.robIdx.needFlush(io.redirectIn)

  override val dataModule = Module(new FDivSqrtDataModule)
  connectDataModule
  dataModule.in_valid := io.in.valid
  dataModule.out_ready := io.out.ready
  dataModule.kill_w := io.in.bits.uop.robIdx.needFlush(io.redirectIn)
  dataModule.kill_r := kill_r
  io.in.ready := dataModule.in_ready
  io.out.valid := dataModule.out_valid
  io.out.bits.uop := uopReg
}
