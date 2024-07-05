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
import xiangshan._
import xiangshan.backend.exu._
import xiangshan.backend.fu.CSRFileIO
import xiangshan.backend.fu.fpu.FMAMidResultIO
import xiangshan.backend.rob._

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

class FUBlock(configs: Seq[(ExuConfig, Int)])(implicit p: Parameters) extends XSModule with HasXSParameter {
  val numIn = configs.map(_._2).sum
  val numFma = configs.filter(_._1 == FmacExeUnitCfg).map(_._2).sum
  val numMatu = configs.filter(_._1 == MatuExeUnitCfg).map(_._2).sum

  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    // in
    val issue = Vec(numIn, Flipped(DecoupledIO(new ExuInput)))
    // out
    val writeback = Vec(numIn, DecoupledIO(new ExuOutput))
    // misc
    val extra = new FUBlockExtraIO(configs)
    val fmaMid = if (numFma > 0) Some(Vec(numFma, new FMAMidResultIO)) else None
    // from dispatch
    val dpUopIn = if(numMatu > 0) Some(Vec(RenameWidth, Flipped(ValidIO(new MicroOp)))) else None
    // from mem
    val ldIn = if (numMatu > 0) Some(Vec(2, Flipped(DecoupledIO(new ExuOutput)))) else None
    // to mem
    val mpuOut_data = if (numMatu > 0) Some(Output(UInt(XLEN.W))) else None
    val mpuOut_valid = if (numMatu > 0) Some(Output(Bool())) else None
    val mpuOut_uop = if (numMatu > 0) Some(Output(new MicroOp)) else None
    val mpuOut_pc = if (numMatu > 0) Some(Output(UInt(VAddrBits.W))) else None
    val mpuOut_robIdx = if (numMatu > 0) Some(Output(UInt(5.W))) else None
    // to dispatch
    val mpuOut_canAccept = if (numMatu > 0) Some(Output(Bool())) else None
    // from rob
    val commitsIn_pc = if (numMatu > 0) Some(Vec(CommitWidth, Input(UInt(VAddrBits.W)))) else None
    val commitsIn_valid = if(numMatu > 0) Some(Vec(CommitWidth, Input(Bool()))) else None
    val commitsIn_robIdx = if(numMatu > 0) Some(Vec(CommitWidth, Input(new RobPtr))) else None
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

  if (io.fmaMid.isDefined) {
    io.fmaMid.get <> exeUnits.map(_.fmaMid).filter(_.isDefined).map(_.get)
  }

  if (io.ldIn.isDefined) {
    io.ldIn.get <> exeUnits.map(_.ldio).filter(_.isDefined).map(_.get).flatten
  }

  if (io.dpUopIn.isDefined) {
    io.dpUopIn.get <> exeUnits.map(_.dp_uop_in).filter(_.isDefined).map(_.get).flatten
  }

  if (io.commitsIn_pc.isDefined) {
    io.commitsIn_pc.get <> exeUnits.map(_.commitio_pc).filter(_.isDefined).map(_.get).flatten
  }

  if (io.commitsIn_valid.isDefined) {
    io.commitsIn_valid.get <> exeUnits.map(_.commitio_valid).filter(_.isDefined).map(_.get).flatten
  }

  if (io.commitsIn_robIdx.isDefined) {
    io.commitsIn_robIdx.get <> exeUnits.map(_.commitio_robidx).filter(_.isDefined).map(_.get).flatten
  }
  if (io.mpuOut_data.isDefined) {
    val filteredPort = exeUnits.map(_.mpuout_data).filter(_.isDefined).map(_.get)
    if (filteredPort.nonEmpty) {
      io.mpuOut_data.get := filteredPort.head
    }
  }

  if (io.mpuOut_valid.isDefined) {
    io.mpuOut_valid.get := exeUnits.map(_.mpuout_valid).filter(_.isDefined).map(_.get).reduce(_||_)
  }

  if (io.mpuOut_pc.isDefined) {
    val filteredPort = exeUnits.map(_.mpuout_pc).filter(_.isDefined).map(_.get)
    if (filteredPort.nonEmpty) {
      io.mpuOut_pc.get := filteredPort.head
    }
  }

  if (io.mpuOut_robIdx.isDefined) {
    val filteredPort = exeUnits.map(_.mpuout_robidx).filter(_.isDefined).map(_.get)
    if (filteredPort.nonEmpty) {
      io.mpuOut_robIdx.get := filteredPort.head
    }
  }

  if (io.mpuOut_canAccept.isDefined) {
    val filteredPort = exeUnits.map(_.mpuout_canaccept).filter(_.isDefined).map(_.get)
    if (filteredPort.nonEmpty) {
      io.mpuOut_canAccept.get := filteredPort.head
    }
  }

  if (io.mpuOut_uop.isDefined) {
    val filteredPort = exeUnits.map(_.mpuout_uop).filter(_.isDefined).map(_.get)
    if (filteredPort.nonEmpty) {
      io.mpuOut_uop.get <> filteredPort.head
    }
  }


  for ((iss, i) <- io.issue.zipWithIndex) {
    XSPerfAccumulate(s"issue_count_$i", iss.fire())
  }
  XSPerfHistogram("writeback_count", PopCount(io.writeback.map(_.fire())), true.B, 0, numIn, 1)

}
