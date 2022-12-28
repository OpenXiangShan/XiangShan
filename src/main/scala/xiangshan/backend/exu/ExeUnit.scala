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

package xiangshan.backend.exu


import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.hierarchy.{Definition, instantiable, public}
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.fu.fpu.{FMA, FPUSubModule}
import xiangshan.backend.fu.{CSR, FUWithRedirect, Fence, FenceToSbuffer}

class FenceIO(implicit p: Parameters) extends XSBundle {
  val sfence = Output(new SfenceBundle)
  val fencei = Output(Bool())
  val sbuffer = new FenceToSbuffer
}

@instantiable
class ExeUnit(config: ExuConfig)(implicit p: Parameters) extends Exu(config) {

  val disableSfence = WireInit(false.B)
  val csr_frm = WireInit(frm.getOrElse(0.U(3.W)))

  val hasRedirect = config.fuConfigs.zip(functionUnits).filter(_._1.hasRedirect).map(_._2)
  println(s"${functionUnits} ${hasRedirect} hasRedirect: ${hasRedirect.length}")
  if (hasRedirect.nonEmpty) {
    require(hasRedirect.length <= 1)
    io.out.bits.redirectValid := hasRedirect.head.asInstanceOf[FUWithRedirect].redirectOutValid
    io.out.bits.redirect := hasRedirect.head.asInstanceOf[FUWithRedirect].redirectOut
  }

  if (config.fuConfigs.contains(csrCfg)) {
    val csr = functionUnits.collectFirst{
      case c: CSR => c
    }.get
    csr.csrio <> csrio.get
    csrio.get.tlb := DelayN(csr.csrio.tlb, 2)
    csrio.get.customCtrl := DelayN(csr.csrio.customCtrl, 2)
    csrio.get.trapTarget := RegNext(csr.csrio.trapTarget)
    csr.csrio.exception := DelayN(csrio.get.exception, 2)
    disableSfence := csr.csrio.disableSfence
    csr_frm := csr.csrio.fpu.frm
    // setup skip for hpm CSR read
    io.out.bits.debug.isPerfCnt := RegNext(csr.csrio.isPerfCnt) // TODO: this is dirty
  }

  if (config.fuConfigs.contains(fenceCfg)) {
    val fence = functionUnits.collectFirst{
      case f: Fence => f
    }.get
    fenceio.get.sfence <> fence.sfence
    fenceio.get.fencei <> fence.fencei
    fenceio.get.sbuffer <> fence.toSbuffer
    fence.io.out.ready := true.B
    fence.disableSfence := disableSfence
  }

  val fpModules = functionUnits.zip(config.fuConfigs.zipWithIndex).filter(_._1.isInstanceOf[FPUSubModule])
  if (fpModules.nonEmpty) {
    // frm is from csr/frm (from CSR) or instr_rm (from instruction decoding)
    val fpSubModules = fpModules.map(_._1.asInstanceOf[FPUSubModule])
    fpSubModules.foreach(mod => {
      val instr_rm = mod.io.in.bits.uop.ctrl.fpu.rm
      mod.rm := Mux(instr_rm =/= 7.U, instr_rm, csr_frm)
    })
    // fflags is selected by arbSelReg
    require(config.hasFastUopOut, "non-fast not implemented")
    val fflagsSel = fpModules.map{ case (fu, (cfg, i)) =>
      val fflagsValid = arbSelReg(i)
      val fflags = fu.asInstanceOf[FPUSubModule].fflags
      val fflagsBits = if (cfg.fastImplemented) fflags else RegNext(fflags)
      (fflagsValid, fflagsBits)
    }
    io.out.bits.fflags := Mux1H(fflagsSel.map(_._1), fflagsSel.map(_._2))
  }

  val fmaModules = functionUnits.filter(_.isInstanceOf[FMA]).map(_.asInstanceOf[FMA])
  if (fmaModules.nonEmpty) {
    require(fmaModules.length == 1)
    fmaModules.head.midResult <> fmaMid.get
  }

  if (config.readIntRf) {
    val in = io.fromInt
    val out = io.out
    XSDebug(in.valid, p"fromInt(${in.valid} ${in.ready}) toInt(${out.valid} ${out.ready})\n")
    XSDebug(io.redirect.valid, p"Redirect:(${io.redirect.valid}) robIdx:${io.redirect.bits.robIdx}\n")
    XSDebug(in.valid, p"src1:${Hexadecimal(in.bits.src(0))} src2:${Hexadecimal(in.bits.src(1))} " +
      p"func:${Binary(in.bits.uop.ctrl.fuOpType)} pc:${Hexadecimal(in.bits.uop.cf.pc)} robIdx:${in.bits.uop.robIdx}\n")
    XSDebug(out.valid, p"out res:${Hexadecimal(out.bits.data)} robIdx:${out.bits.uop.robIdx}\n")
  }

}

class AluExeUnit(implicit p: Parameters) extends ExeUnit(AluExeUnitCfg)
class JumpCSRExeUnit(implicit p: Parameters) extends ExeUnit(JumpCSRExeUnitCfg)
class JumpExeUnit(implicit p: Parameters) extends ExeUnit(JumpExeUnitCfg)
class StdExeUnit(implicit p: Parameters) extends ExeUnit(StdExeUnitCfg)
class FmacExeUnit(implicit p: Parameters) extends ExeUnit(FmacExeUnitCfg)
class FmiscExeUnit(implicit p: Parameters) extends ExeUnit(FmiscExeUnitCfg)

object ExeUnitDef {
  def apply(cfg: ExuConfig)(implicit p: Parameters): Definition[ExeUnit] = {
    cfg match {
      case JumpExeUnitCfg => Definition(new JumpExeUnit)
      case AluExeUnitCfg => Definition(new AluExeUnit)
      case MulDivExeUnitCfg => Definition(new MulDivExeUnit)
      case JumpCSRExeUnitCfg => Definition(new JumpCSRExeUnit)
      case FmacExeUnitCfg => Definition(new FmacExeUnit)
      case FmiscExeUnitCfg => Definition(new FmiscExeUnit)
      case StdExeUnitCfg => Definition(new StdExeUnit)
      case _ => {
        println(s"cannot generate exeUnit from $cfg")
        null
      }
    }
  }
}

