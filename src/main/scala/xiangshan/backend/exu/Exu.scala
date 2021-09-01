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
import chisel3.util._
import utils.XSPerfAccumulate
import xiangshan._
import xiangshan.backend.fu._
import xiangshan.mem.StoreDataBundle

case class ExuParameters
(
  JmpCnt: Int,
  AluCnt: Int,
  MulCnt: Int,
  MduCnt: Int,
  FmacCnt: Int,
  FmiscCnt: Int,
  FmiscDivSqrtCnt: Int,
  LduCnt: Int,
  StuCnt: Int
) {
  assert(JmpCnt == 1, "Only support 1 JmpUnit now!")

  def IntExuCnt = AluCnt + MulCnt + MduCnt + JmpCnt

  def FpExuCnt = FmacCnt + FmiscCnt + FmiscDivSqrtCnt

  def LsExuCnt = LduCnt + StuCnt

  def ExuCnt = IntExuCnt + FpExuCnt + LduCnt + StuCnt

  def CriticalExuCnt = AluCnt + FmacCnt + LsExuCnt
}

case class ExuConfig
(
  name: String,
  blockName: String, // NOTE: for perf counter
  fuConfigs: Seq[FuConfig],
  wbIntPriority: Int,
  wbFpPriority: Int
) {
  def max(in: Seq[Int]): Int = in.reduce((x, y) => if (x > y) x else y)

  val intSrcCnt = max(fuConfigs.map(_.numIntSrc))
  val fpSrcCnt = max(fuConfigs.map(_.numFpSrc))
  val readIntRf = intSrcCnt > 0
  val readFpRf = fpSrcCnt > 0
  val writeIntRf = fuConfigs.map(_.writeIntRf).reduce(_ || _)
  val writeFpRf = fuConfigs.map(_.writeFpRf).reduce(_ || _)
  val hasRedirect = fuConfigs.map(_.hasRedirect).reduce(_ || _)
  val hasFastUopOut = fuConfigs.map(_.fastUopOut).reduce(_ || _)

  val latency: HasFuLatency = {
    val lats = fuConfigs.map(_.latency)
    if (lats.exists(x => x.latencyVal.isEmpty)) {
      UncertainLatency()
    } else {
      if(
        lats.drop(1).map(_.latencyVal.get == lats.head.latencyVal.get).forall(eq => eq)
      ) {
        lats.head
      } else {
        UncertainLatency()
      }
    }
  }
  // NOTE: dirty code for MulDivExeUnit
  val hasCertainLatency = if (name == "MulDivExeUnit") true else latency.latencyVal.nonEmpty
  val hasUncertainlatency = if (name == "MulDivExeUnit") true else latency.latencyVal.isEmpty
  val wakeupFromRS = hasCertainLatency && (wbIntPriority <= 1 || wbFpPriority <= 1)
  val allWakeupFromRS = !hasUncertainlatency && (wbIntPriority <= 1 || wbFpPriority <= 1)
  val wakeupFromExu = !wakeupFromRS
  val hasExclusiveWbPort = (wbIntPriority == 0 && writeIntRf) || (wbFpPriority == 0 && writeFpRf)

  def canAccept(fuType: UInt): Bool = {
    Cat(fuConfigs.map(_.fuType === fuType)).orR()
  }
}

abstract class Exu(val config: ExuConfig)(implicit p: Parameters) extends XSModule {

  val functionUnits = config.fuConfigs.map(cfg => {
    val mod = Module(cfg.fuGen(p))
    mod.suggestName(cfg.name)
    mod
  })

  val fuSel = functionUnits.zip(config.fuConfigs.map(_.fuSel)).map {
    case (fu, sel) => sel(fu)
  }

  val io = IO(new Bundle() {
    val fromInt = if (config.readIntRf) Flipped(DecoupledIO(new ExuInput)) else null
    val fromFp = if (config.readFpRf) Flipped(DecoupledIO(new ExuInput)) else null
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    val out = DecoupledIO(new ExuOutput)
  })
  val csrio = if (config == JumpCSRExeUnitCfg) Some(IO(new CSRFileIO)) else None
  val fenceio = if (config == JumpCSRExeUnitCfg) Some(IO(new FenceIO)) else None
  val frm = if (config == FmacExeUnitCfg || config == FmiscExeUnitCfg) Some(IO(Input(UInt(3.W)))) else None
  val stData = if (config == StdExeUnitCfg) Some(IO(ValidIO(new StoreDataBundle))) else None

  for ((fuCfg, (fu, sel)) <- config.fuConfigs.zip(functionUnits.zip(fuSel))) {

    val in = if (fuCfg.numIntSrc > 0) {
      assert(fuCfg.numFpSrc == 0 || config == StdExeUnitCfg)
      io.fromInt
    } else {
      assert(fuCfg.numFpSrc > 0)
      io.fromFp
    }

    val src1 = in.bits.src(0)
    val src2 = in.bits.src(1)
    val src3 = in.bits.src(2)

    fu.io.in.valid := in.valid && sel
    fu.io.in.bits.uop := in.bits.uop
    fu.io.in.bits.src.foreach(_ <> DontCare)
    if (fuCfg.srcCnt > 0) {
      fu.io.in.bits.src(0) := src1
    }
    if (fuCfg.srcCnt > 1 || fuCfg == jmpCfg) { // jump is special for jalr target
      fu.io.in.bits.src(1) := src2
    }
    if (fuCfg.srcCnt > 2) {
      fu.io.in.bits.src(2) := src3
    }
    fu.io.redirectIn := io.redirect
    fu.io.flushIn := io.flush
  }


  val needArbiter = !(config.latency.latencyVal.nonEmpty && (config.latency.latencyVal.get == 0))

  def writebackArb(in: Seq[DecoupledIO[FuOutput]], out: DecoupledIO[ExuOutput]): Seq[Bool] = {
    if (needArbiter) {
      if(in.size == 1){
        in.head.ready := out.ready
        out.bits.data := in.head.bits.data
        out.bits.uop := in.head.bits.uop
        out.valid := in.head.valid
      } else {
        val arb = Module(new Arbiter(new ExuOutput, in.size))
        in.zip(arb.io.in).foreach{ case (l, r) =>
          l.ready := r.ready
          r.valid := l.valid
          r.bits := DontCare
          r.bits.uop := l.bits.uop
          r.bits.data := l.bits.data
        }
        arb.io.out <> out
      }
    } else {
      in.foreach(_.ready := out.ready)
      val sel = Mux1H(in.map(x => x.valid -> x))
      out.bits.data := sel.bits.data
      out.bits.uop := sel.bits.uop
      out.valid := sel.valid
    }
    in.map(_.fire)
  }

  val arbSel = writebackArb(functionUnits.map(_.io.out), io.out)

  val arbSelReg = arbSel.map(RegNext(_))
  val dataRegVec = functionUnits.map(_.io.out.bits.data).zip(config.fuConfigs).map{ case (i, cfg) =>
    if (config.hasFastUopOut && (!cfg.fastUopOut || !cfg.fastImplemented)) {
      println(s"WARNING: fast not implemented!! ${cfg.name} will be delayed for one cycle.")
    }
    (if (cfg.fastUopOut && cfg.fastImplemented) i else RegNext(i))
  }
  val dataReg = Mux1H(arbSelReg, dataRegVec)

  if (config.hasFastUopOut) {
    io.out.bits.data := dataReg
  }

  val readIntFu = config.fuConfigs
    .zip(functionUnits.zip(fuSel))
    .filter(_._1.numIntSrc > 0)
    .map(_._2)

  val readFpFu = config.fuConfigs
    .zip(functionUnits.zip(fuSel))
    .filter(_._1.numFpSrc > 0)
    .map(_._2)

  def inReady(s: Seq[(FunctionUnit, Bool)]): Bool = {
    if (s.size == 1) {
      s.head._1.io.in.ready
    } else {
      if (needArbiter) {
        Cat(s.map(x => x._1.io.in.ready && x._2)).orR()
      } else {
        Cat(s.map(x => x._1.io.in.ready)).andR()
      }
    }
  }

  if (config.readIntRf) {
    XSPerfAccumulate("from_int_fire", io.fromInt.fire())
    XSPerfAccumulate("from_int_valid", io.fromInt.valid)
    io.fromInt.ready := !io.fromInt.valid || inReady(readIntFu)
  }

  if (config.readFpRf) {
    XSPerfAccumulate("from_fp_fire", io.fromFp.fire())
    XSPerfAccumulate("from_fp_valid", io.fromFp.valid)
    io.fromFp.ready := !io.fromFp.valid || inReady(readFpFu)
  }

  def assignDontCares(out: ExuOutput) = {
    out.fflags := DontCare
    out.debug <> DontCare
    out.debug.isMMIO := false.B
    out.debug.isPerfCnt := false.B
    out.debug.paddr := DontCare
    out.redirect <> DontCare
    out.redirectValid := false.B
  }

  assignDontCares(io.out.bits)
  XSPerfAccumulate("out_fire", io.out.fire)
  XSPerfAccumulate("out_valid", io.out.valid)
}
