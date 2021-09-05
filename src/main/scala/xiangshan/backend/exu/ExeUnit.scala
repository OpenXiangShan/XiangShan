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
import utils.{XSDebug, XSPerfAccumulate}
import xiangshan._
import xiangshan.backend.Std
import xiangshan.backend.fu.fpu.IntToFP
import xiangshan.backend.fu.{CSR, FUWithRedirect, Fence, FenceToSbuffer}

class FenceIO(implicit p: Parameters) extends XSBundle {
  val sfence = Output(new SfenceBundle)
  val fencei = Output(Bool())
  val sbuffer = new FenceToSbuffer
}

class ExeUnit(config: ExuConfig)(implicit p: Parameters) extends Exu(config: ExuConfig) {
  val disableSfence = WireInit(false.B)
  val csr_frm = WireInit(0.U(3.W))

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
    disableSfence := csr.csrio.disableSfence
    csr_frm := csr.csrio.fpu.frm
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

  if (config.fuConfigs.contains(i2fCfg)) {
    val i2f = functionUnits.collectFirst {
      case i: IntToFP => i
    }.get
    val instr_rm = io.fromInt.bits.uop.ctrl.fpu.rm
    i2f.rm := Mux(instr_rm =/= 7.U, instr_rm, csr_frm)
  }

  if (config.fuConfigs.contains(stdCfg)) {
    val std = functionUnits.collectFirst {
      case s: Std => s
    }.get
    stData.get.valid := std.io.out.valid
    stData.get.bits.uop := std.io.out.bits.uop
    stData.get.bits.data := std.io.out.bits.data
    io.out.valid := false.B
    io.out.bits := DontCare
  }
  if (config.readIntRf) {
    val in = io.fromInt
    val out = io.out
    XSDebug(in.valid, p"fromInt(${in.valid} ${in.ready}) toInt(${out.valid} ${out.ready})\n")
    XSDebug(io.redirect.valid, p"Redirect:(${io.redirect.valid}) roqIdx:${io.redirect.bits.roqIdx}\n")
    XSDebug(in.valid, p"src1:${Hexadecimal(in.bits.src(0))} src2:${Hexadecimal(in.bits.src(1))} " +
      p"func:${Binary(in.bits.uop.ctrl.fuOpType)} pc:${Hexadecimal(in.bits.uop.cf.pc)} roqIdx:${in.bits.uop.roqIdx}\n")
    XSDebug(out.valid, p"out res:${Hexadecimal(out.bits.data)} roqIdx:${out.bits.uop.roqIdx}\n")
  }

}

class AluExeUnit(implicit p: Parameters) extends ExeUnit(AluExeUnitCfg)
class JumpCSRExeUnit(implicit p: Parameters) extends ExeUnit(JumpCSRExeUnitCfg)
class JumpExeUnit(implicit p: Parameters) extends ExeUnit(JumpExeUnitCfg)
class StdExeUnit(implicit p: Parameters) extends ExeUnit(StdExeUnitCfg)

object ExeUnit {
  def apply(cfg: ExuConfig)(implicit p: Parameters): ExeUnit = {
    cfg match {
      case JumpExeUnitCfg => Module(new JumpExeUnit)
      case AluExeUnitCfg => Module(new AluExeUnit)
      case MulDivExeUnitCfg => Module(new MulDivExeUnit)
      case JumpCSRExeUnitCfg => Module(new JumpCSRExeUnit)
      case FmacExeUnitCfg => Module(new FmacExeUnit)
      case FmiscExeUnitCfg => Module(new FmiscExeUnit)
      case StdExeUnitCfg => Module(new StdExeUnit)
      case _ => {
        println(s"cannot generate exeUnit from $cfg")
        null
      }
    }
  }
}