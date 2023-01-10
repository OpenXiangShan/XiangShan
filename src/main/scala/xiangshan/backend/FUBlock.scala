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
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

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

  override def toString: String = {
    s"FUBlockExtraIO: " + configs.map(a => a._1.name + "*" + a._2).reduce(_ + " " + _) + s" hasCSR:${hasCSR} hasFence:${hasFence} hasFrm:${hasFrm} numRedOut:${numRedirectOut}"
  }
}

abstract class FUBlock(configs: Seq[(ExuConfig, Int)])(implicit p: Parameters) extends LazyModule with HasXSParameter {

  lazy val module = new FUBlockImp(configs, this)
}

class FUBlockImp(configs: Seq[(ExuConfig, Int)], outer: FUBlock)(implicit p: Parameters)
extends LazyModuleImp(outer) with HasXSParameter {
  require(configs.map(_._1).filter(a => a.readFpRf && a.readIntRf && a.readVecRf).isEmpty)

  val configIntIn = configs.filter{a => a._1.readIntRf}
  val configVecIn = configs.filter{a => a._1.readVecRf || a._1.readFpRf}
  val configIntOut = configs.filter{a => a._1.readIntRf && a._1.writeIntRf}
  val configVecOut = configs.filter{a => (a._1.readVecRf || a._1.readFpRf) && (a._1.writeVecRf || a._1.writeFpRf)}

  val numIntIn = configIntIn.map(_._2).sum
  val numVecIn = configVecIn.map(_._2).sum
  // If only write but not read, the op is data move cross domain or i2f/f2i
  val numIntOut = configIntOut.map(_._2).sum
  val numVecOut = configVecOut.map(_._2).sum


  val numIn = configs.map(_._2).sum
  require(numIn == (numIntIn + numVecIn))
  // val numFma = configs.filter(_._1 == FmacExeUnitCfg).map(_._2).sum
  // val isVpu = configs.map(_._1.isVPU).reduce(_ || _)

  def SeqConnect[T <: Data](lhs: Seq[T], rhs: Seq[T]) {
    for ((l, r) <- lhs.zip(rhs)) { l <> r }
  }

  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    // in
    val issueInt = Vec(numIntIn, Flipped(DecoupledIO(new ExuInput(false))))
    val issueVec = Vec(numVecIn, Flipped(DecoupledIO(new ExuInput(true))))
    // out
    val writebackInt = Vec(numIntOut, DecoupledIO(new ExuOutput(false)))
    val writebackVec = Vec(numVecOut, DecoupledIO(new ExuOutput(true)))
    // misc
    val extra = new FUBlockExtraIO(configs)

    def issue = issueInt ++ issueVec
    def writeback = writebackInt ++ writebackVec
  })

  val exuDefs = configs.map(_._1).map(ExeUnitDef(_))
  val exeUnits = configs.zip(exuDefs).map(x => Seq.fill(x._1._2)(Instance(x._2))).reduce(_ ++ _)
  val intExeUnits = exeUnits.filter(_.config.readIntRf)
  val fpExeUnits = exeUnits.filterNot(_.config.readIntRf)
  SeqConnect(io.issue, intExeUnits.map(_.io.fromInt) ++ fpExeUnits.map(_.io.fromFp))
  SeqConnect(io.writeback, exeUnits.map(_.io.out))
  // io.issueInt <> intExeUnits.map(_.io.fromInt)
  // io.issueVec <> fpExeUnits.map(_.io.fromFp)
  // io.issue <> intExeUnits.map(_.io.fromInt) ++ fpExeUnits.map(_.io.fromFp)
  io.writebackInt <> intExeUnits.map(_.io.out)
  io.writebackVec <> fpExeUnits.map(_.io.out)
  // for ((w, e) <- io.writeback.zip(exeUnits.map(_.io.out))) { w <> e }

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

  for ((iss, i) <- (io.issueInt ++ io.issueVec).zipWithIndex) {
    XSPerfAccumulate(s"issue_count_$i", iss.fire())
  }
  XSPerfHistogram("writeback_count", PopCount((io.writebackInt ++ io.writebackVec).map(_.fire())), true.B, 0, numIn, 1)

  println("FUBlock IO.issue & IO.Writeback")
  if (numIntIn > 0) println(s"  numIntIn: ${numIntIn} " + configIntIn.map(a => a._1.name + "*" + a._2).reduce(_ + " " + _))
  if (numIntOut > 0) println(s"  numIntOut: ${numIntOut} " + configIntOut.map(a => a._1.name + "*" + a._2).reduce(_ + " " + _))
  if (numVecIn > 0) println(s"  numVecIn: ${numVecIn} " + configVecIn.map(a => a._1.name + "*" + a._2).reduce(_ + " " + _))
  if (numVecOut > 0) println(s"  numVecOut: ${numVecOut} " + configVecOut.map(a => a._1.name + "*" + a._2).reduce(_ + " " + _))
  println(io.extra)
}

class IntFUBlock(configVec: Seq[(ExuConfig, Int)])(implicit p: Parameters) extends FUBlock(configVec) {
  override lazy val module = new IntFUBlockImp(configVec, this)
}

class IntFUBlockImp(configVec: Seq[(ExuConfig, Int)], out: IntFUBlock)(implicit p: Parameters) extends FUBlockImp(configVec, out) {
}


class VecFUBlock(configVec: Seq[(ExuConfig, Int)])(implicit p: Parameters) extends FUBlock(configVec) {
  override lazy val module = new VecFUBlockImp(configVec, this)
}

class VecFUBlockImp(configVec: Seq[(ExuConfig, Int)], out: VecFUBlock)(implicit p: Parameters) extends FUBlockImp(configVec, out) {
}