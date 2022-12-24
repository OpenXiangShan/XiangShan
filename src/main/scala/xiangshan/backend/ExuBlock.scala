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
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils._
import utility._
import xiangshan._
import xiangshan.backend.exu._

class ExuBlock(
  val configs: Seq[(ExuConfig, Int, Seq[ExuConfig], Seq[ExuConfig])],
  val dpPorts: Seq[Seq[(Int, Int)]],
  val intRfWbPorts: Seq[Seq[ExuConfig]],
  val fpRfWbPorts: Seq[Seq[ExuConfig]],
  val outFastPorts: Seq[Seq[Int]],
  val outIntRfReadPorts: Int,
  val outFpRfReadPorts: Int,
  val hasIntRf: Boolean,
  val hasFpRf: Boolean
)(implicit p: Parameters) extends LazyModule with HasWritebackSource with HasExuWbHelper {
  val scheduler = LazyModule(new Scheduler(configs, dpPorts, intRfWbPorts, fpRfWbPorts, outFastPorts, outIntRfReadPorts, outFpRfReadPorts, hasIntRf, hasFpRf))

  val allRfWbPorts = intRfWbPorts ++ fpRfWbPorts
  def getWbIndex(cfg: ExuConfig): Seq[Int] = allRfWbPorts.zipWithIndex.filter(_._1.contains(cfg)).map(_._2)

  val fuConfigs = configs.map(c => (c._1, c._2)).filter(_._1.extendsExu)
  val numOutFu = configs.filterNot(_._1.extendsExu).map(_._2).sum

  lazy val module = new ExuBlockImp(this)

  override val writebackSourceParams: Seq[WritebackSourceParams] = {
    val params = new WritebackSourceParams
    params.exuConfigs = fuConfigs.flatMap(cfg => Seq.fill(cfg._2)(Seq(cfg._1)))
    Seq(params)
  }
  override lazy val writebackSourceImp: HasWritebackSourceImp = module
}

class ExuBlockImp(outer: ExuBlock)(implicit p: Parameters) extends LazyModuleImp(outer)
  with HasWritebackSourceImp with HasPerfEvents {
  val scheduler = outer.scheduler.module

  val fuConfigs = outer.fuConfigs
  val fuBlock = Module(new FUBlock(fuConfigs))

  val numOutFu = outer.numOutFu

  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))
    // global control
    val redirect = Flipped(ValidIO(new Redirect))
    // dispatch ports
    val allocPregs = scheduler.io.allocPregs.cloneType
    val in = scheduler.io.in.cloneType
    // issue and wakeup ports
    val issue = if (numOutFu > 0) Some(Vec(numOutFu, DecoupledIO(new ExuInput))) else None
    val fastUopOut = scheduler.io.fastUopOut.cloneType
    val rfWriteback = scheduler.io.writeback.cloneType
    val fastUopIn = scheduler.io.fastUopIn.cloneType
    val fuWriteback = fuBlock.io.writeback.cloneType
    // extra
    val scheExtra = scheduler.io.extra.cloneType
    val fuExtra = fuBlock.io.extra.cloneType
  })
  override def writebackSource1: Option[Seq[Seq[DecoupledIO[ExuOutput]]]] = Some(Seq(io.fuWriteback))

  // IO for the scheduler
  scheduler.io.hartId := io.hartId
  scheduler.io.redirect <> io.redirect
  scheduler.io.allocPregs <> io.allocPregs
  scheduler.io.in <> io.in
  scheduler.io.fastUopOut <> io.fastUopOut
  scheduler.io.writeback <> io.rfWriteback
  scheduler.io.fastUopIn <> io.fastUopIn
  scheduler.io.extra <> io.scheExtra

  val perfEvents = scheduler.getPerfEvents
  generatePerfEvent()

  // the scheduler issues instructions to function units
  scheduler.io.issue <> fuBlock.io.issue ++ io.issue.getOrElse(Seq())
  if (scheduler.io.fmaMid.isDefined) {
    scheduler.io.fmaMid.get <> fuBlock.io.fmaMid.get
  }

  // IO for the function units
  fuBlock.io.redirect <> io.redirect
  fuBlock.io.writeback <> io.fuWriteback
  fuBlock.io.extra <> io.fuExtra

  // To reduce fanout, we add registers here for redirect.
  val redirect = RegNextWithEnable(io.redirect)

  val flattenFuConfigs = fuConfigs.flatMap(c => Seq.fill(c._2)(c._1))
  require(flattenFuConfigs.length == fuBlock.io.writeback.length)

  // TODO: add an attribute to ExuConfig for fast wakeup
  for (((cfg, fuOut), fastOut) <- flattenFuConfigs.zip(fuBlock.io.writeback).zip(io.fastUopOut)) {
    if (cfg == FmacExeUnitCfg) {
      fastOut.valid := fuOut.valid
      fastOut.bits := fuOut.bits.uop
      XSError(fuOut.valid && !fuOut.ready, "fastUopOut should not be blocked\n")
      println(s"Enable fast wakeup from function unit ${cfg.name}")
    }
  }

  // Optimizations for wakeup and writeback timing
  // Timing priority: RegNext(rs.fastUopOut) > fu.writeback > arbiter.out(--> io.rfWriteback --> rs.writeback)
  // Filter condition: allWakeupFromRS > hasExclusiveWbPort > None
  // The higher priority, the better timing.

  // (1) When function units have exclusive writeback ports, their wakeup ports for
  // reservation stations can be connected directly from function units' writeback ports.
  // Special case: when the function unit has fastUopOut, valid and uop should be RegNext.
  val exclusiveFuWb = flattenFuConfigs.zip(fuBlock.io.writeback).filter(_._1.hasExclusiveWbPort)
  val exclusiveRfWbIdx = fuConfigs.map(_._1).filter(_.hasExclusiveWbPort).flatMap(cfg => outer.getWbIndex(cfg))
  require(exclusiveFuWb.length == exclusiveRfWbIdx.length, s"${exclusiveFuWb.length} != ${exclusiveRfWbIdx.length}")
  for ((i, (cfg, wb)) <- exclusiveRfWbIdx.zip(exclusiveFuWb)) {
    val scheWb = scheduler.io.writeback(i)
    scheWb.valid := wb.valid
    scheWb.bits := wb.bits
    if (cfg.hasFastUopOut) {
      val isFlushed = wb.bits.uop.robIdx.needFlush(redirect)
      scheWb.valid := RegNext(wb.valid && !isFlushed)
      scheWb.bits.uop := RegNext(wb.bits.uop)
    }

    println(s"scheduler.writeback($i) is connected from exu ${cfg.name}")
    val outerWb = io.rfWriteback(i)
    val hasWb = outerWb.valid || scheWb.valid
    XSError(hasWb && outerWb.bits.uop.robIdx =/= scheWb.bits.uop.robIdx,
      "different instruction between io.rfWriteback and fu.writeback\n")
    XSError(hasWb && outerWb.bits.data =/= scheWb.bits.data,
      "different data between io.rfWriteback and fu.writeback\n")
  }

  // (2) If the reservation station has fastUopOut for all instructions in this exu,
  // we should replace io.fuWriteback with RegNext(fastUopOut).
  // In this case, the corresponding execution units must have exclusive writeback ports,
  // unless it's impossible that rs can ensure the instruction is able to write the regfile.
  val allWakeupFromRs = flattenFuConfigs.zipWithIndex.filter(_._1.allWakeupFromRS)
  for ((cfg, i) <- allWakeupFromRs) {
    // When the exu has fastUopOut, we still let rs have higher priority,
    // assuming the rs has better timing for wakeup.
    if (!cfg.hasFastUopOut) {
    val wbOut = io.fuWriteback(i)
    val fastWakeup = scheduler.io.fastUopOut(i)
    if (cfg.hasFastUopOut) {
      wbOut.valid := fastWakeup.valid
      wbOut.bits.uop := fastWakeup.bits
    }
    else {
      val isFlushed = fastWakeup.bits.robIdx.needFlush(redirect)
      wbOut.valid := RegNext(fastWakeup.valid && !isFlushed)
      wbOut.bits.uop := RegNext(fastWakeup.bits)
    }

    println(s"writeback from exu $i is replaced by RegNext(rs.fastUopOut)")
    XSError(wbOut.valid && !wbOut.ready, "fast uop wb should not be blocked\n")
    require(cfg.hasExclusiveWbPort, "it's impossible to have allWakeupFromRs if it doesn't have exclusive rf ports")
    val fuWb = fuBlock.io.writeback(i)
    val fuWbValid = if (cfg.hasFastUopOut) RegNext(fuWb.valid) else fuWb.valid
    val fuWbRobIdx = if (cfg.hasFastUopOut) RegNext(fuWb.bits.uop.robIdx) else fuWb.bits.uop.robIdx
    XSError((wbOut.valid || fuWbValid) && wbOut.bits.uop.robIdx =/= fuWbRobIdx,
      "different instruction between rs.fastUopOut and fu.writeback\n")}
  }

  // (3) If the reservation station has fastUopOut for all instructions in this exu,
  // we should replace io.rfWriteback (rs.writeback) with RegNext(rs.wakeupOut).
  val allWakeFromRsCfgs = fuConfigs.map(_._1).filter(_.allWakeupFromRS)
  for (cfg <- allWakeFromRsCfgs) {
    val wakeupIdx = flattenFuConfigs.zipWithIndex.filter(_._1 == cfg).map(_._2)
    val wbIdx = outer.getWbIndex(cfg)
    require(wakeupIdx.length == wbIdx.length)
    for ((i, j) <- wakeupIdx.zip(wbIdx)) {
      val scheWb = scheduler.io.writeback(j)
      val isFlushed = scheduler.io.fastUopOut(i).bits.robIdx.needFlush(redirect)
      scheWb.valid := RegNext(scheduler.io.fastUopOut(i).valid && !isFlushed)
      scheWb.bits.uop := RegNext(scheduler.io.fastUopOut(i).bits)
    }
  }

  // By default, instructions do not have exceptions when they enter the function units.
  fuBlock.io.issue.map(_.bits.uop.clearExceptions())
  // For exe units that don't have exceptions, we assign zeroes to their exception vector.
  for ((cfg, wb) <- flattenFuConfigs.zip(io.fuWriteback)) {
    wb.bits.uop.clearExceptions(cfg.exceptionOut, cfg.flushPipe, cfg.replayInst)
  }

}
