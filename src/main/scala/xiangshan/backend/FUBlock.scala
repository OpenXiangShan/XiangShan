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

package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.hierarchy.Instance
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.exu._
import xiangshan.backend.fu.CSRFileIO

class WakeUpBundle(numFast: Int, numSlow: Int)(implicit p: Parameters) extends XSBundle {
  val fastUops = Vec(numFast, Flipped(ValidIO(new MicroOp)))
  val fast = Vec(numFast, Flipped(ValidIO(new ExuOutput))) //one cycle later than fastUops
  val slow = Vec(numSlow, Flipped(DecoupledIO(new ExuOutput)))

}

class FUBlockExtraIO(configs: Seq[(ExuConfig, Int)])(implicit p: Parameters) extends XSBundle {
  val hasCSR = configs.map(_._1).contains(JumpCSRExeUnitCfg)
  val hasFence = configs.map(_._1).contains(JumpCSRExeUnitCfg)
  val hasFrm = configs.map(_._1).contains(FmacExeUnitCfg) || configs.map(_._1).contains(FmiscExeUnitCfg)
  val numRedirectOut = configs.filter(_._1.hasRedirect).map(_._2).sum

  val exuRedirect = Vec(numRedirectOut, ValidIO(new ExuOutput))
  val csrio = if (hasCSR) Some(new CSRFileIO) else None
  val fenceio = if (hasFence) Some(new FenceIO) else None
  val frm = if (hasFrm) Some(Input(UInt(3.W))) else None

}

class FUBlock(configs: Seq[(ExuConfig, Int)])(implicit p: Parameters) extends XSModule {
  val numIn = configs.map(_._2).sum
  val numFma = configs.filter(_._1 == FmacExeUnitCfg).map(_._2).sum
  val isVpu = configs.map(_._1.isVPU).reduce(_ || _)

  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    // in
    val issue = Vec(numIn, Flipped(DecoupledIO(new ExuInput(isVpu))))
    // out
    val writeback = Vec(numIn, DecoupledIO(new ExuOutput(isVpu)))
    // misc
    val extra = new FUBlockExtraIO(configs)
  })

  val exuDefs = configs.map(_._1).map(ExeUnitDef(_))
  val exeUnits = configs.zip(exuDefs).map(x => Seq.fill(x._1._2)(Instance(x._2))).reduce(_ ++ _)
  val intExeUnits = exeUnits.filter(_.config.readIntRf)
  // TODO: deal with Std units
  val fpExeUnits = exeUnits.filterNot(_.config.readIntRf)
  val stdExeUnits = exeUnits.filter(_.config.readIntRf).filter(_.config.readFpRf)
  stdExeUnits.foreach(_.io.fromFp := DontCare)
  io.issue <> intExeUnits.map(_.io.fromInt) ++ fpExeUnits.map(_.io.fromFp)
  io.writeback <> exeUnits.map(_.io.out)

  // to please redirectGen
  io.extra.exuRedirect.zip(exeUnits.reverse.filter(_.config.hasRedirect).map(_.io.out)).foreach {
    case (x, y) =>
      x.valid := y.fire() && y.bits.redirectValid
      x.bits := y.bits
  }

  for ((exu, i) <- exeUnits.zipWithIndex) {
    exu.io.redirect <> RegNextWithEnable(io.redirect)

    if (exu.csrio.isDefined) {
      exu.csrio.get <> io.extra.csrio.get
      exu.csrio.get.perf <> RegNext(io.extra.csrio.get.perf)
      // RegNext customCtrl for better timing
      io.extra.csrio.get.customCtrl := RegNext(RegNext(exu.csrio.get.customCtrl))
      io.extra.csrio.get.tlb := RegNext(RegNext(exu.csrio.get.tlb))
      // RegNext csrUpdate
      exu.csrio.get.distributedUpdate := RegNext(io.extra.csrio.get.distributedUpdate)
    }

    if (exu.fenceio.isDefined) {
      exu.fenceio.get <> io.extra.fenceio.get
    }

    if (exu.frm.isDefined) {
      exu.frm.get := io.extra.frm.get
    }
  }

  for ((iss, i) <- io.issue.zipWithIndex) {
    XSPerfAccumulate(s"issue_count_$i", iss.fire())
  }
  XSPerfHistogram("writeback_count", PopCount(io.writeback.map(_.fire())), true.B, 0, numIn, 1)

}
