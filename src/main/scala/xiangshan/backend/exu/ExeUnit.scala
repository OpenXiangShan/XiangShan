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
import xiangshan.backend.fu.vector.{VFPU, VPUSubModule}

class FenceIO(implicit p: Parameters) extends XSBundle {
  val sfence = Output(new SfenceBundle)
  val fencei = Output(Bool())
  val sbuffer = new FenceToSbuffer
}

@instantiable
class ExeUnit(config: ExuConfig)(implicit p: Parameters) extends Exu(config) {

  val disableSfence = WireInit(false.B)
  val csr_frm = WireInit(frm.getOrElse(0.U(3.W)))
  val csr_vxrm = WireInit(vxrm.getOrElse(0.U(2.W)))
  val csr_vstart = WireInit(vstart.getOrElse(0.U(XLEN.W)))

  val hasRedirect = config.fuConfigs.zip(functionUnits).filter(_._1.hasRedirect).map(_._2)
  println(s"ExeUnit: ${functionUnits.map(_.name).reduce(_ + " " + _)} ${hasRedirect} hasRedirect: ${hasRedirect.length}")
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
  val vfpModules = functionUnits.zip(config.fuConfigs.zipWithIndex).filter(_._1.isInstanceOf[VFPU])
  val vipuModules = functionUnits.zip(config.fuConfigs.zipWithIndex).filter(x => x._1.isInstanceOf[VPUSubModule])
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
  // Overwrite write operation of fpModules
  if (vfpModules.nonEmpty) {
    val vfpSubModules = vfpModules.map(_._1.asInstanceOf[VFPU])
    vfpSubModules.foreach(mod => {
      mod.rm := csr_frm
    })
  }
  if (vipuModules.nonEmpty) {
    vipuModules.map(_._1.asInstanceOf[VPUSubModule]).foreach(mod => {
      mod.vxrm := csr_vxrm
      mod.vstart := csr_vstart
    })
    // vxsat is selected by arbSelReg
    require(config.hasFastUopOut, "non-fast not implemented")
    val vxsatSel = vipuModules.map{ case (fu, (cfg, i)) =>
      val vxsatValid = arbSelReg(i)
      val vxsat = fu.asInstanceOf[VPUSubModule].vxsat
      val vxsatBits = if (cfg.fastImplemented) vxsat else RegNext(vxsat)
      (vxsatValid, vxsatBits)
    }
    io.out.bits.vxsat := Mux1H(vxsatSel.map(_._1), vxsatSel.map(_._2))
  }
  val fmaModules = functionUnits.filter(_.isInstanceOf[FMA]).map(_.asInstanceOf[FMA])
  if (fmaModules.nonEmpty) {
    require(fmaModules.length == 1)
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
      case _ => {
        println(s"cannot generate exeUnit from $cfg")
        null
      }
    }
  }
}

