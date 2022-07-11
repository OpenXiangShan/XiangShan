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
import difftest.{DifftestFpWriteback, DifftestIntWriteback}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.HasExuWbHelper

class ExuWbArbiter(n: Int, hasFastUopOut: Boolean, fastVec: Seq[Boolean])(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val in = Vec(n, Flipped(DecoupledIO(new ExuOutput)))
    val out = DecoupledIO(new ExuOutput)
  })

  class ExuCtrl extends Bundle{
    val uop = new MicroOp
    val fflags = UInt(5.W)
    val redirectValid = Bool()
    val redirect = new Redirect
    val debug = new DebugBundle
  }
  val ctrl_arb = Module(new Arbiter(new ExuCtrl, n))
  val data_arb = Module(new Arbiter(UInt((XLEN+1).W), n))

  ctrl_arb.io.out.ready := io.out.ready
  data_arb.io.out.ready := io.out.ready

  for(((in, ctrl), data) <- io.in.zip(ctrl_arb.io.in).zip(data_arb.io.in)){
    ctrl.valid := in.valid
    for((name, d) <- ctrl.bits.elements) {
      d := in.bits.elements(name)
    }
    data.valid := in.valid
    data.bits := in.bits.data
    in.ready := ctrl.ready
    assert(ctrl.ready === data.ready)
  }
  assert(ctrl_arb.io.chosen === data_arb.io.chosen)

  io.out.bits.data := data_arb.io.out.bits
  for((name, d) <- ctrl_arb.io.out.bits.elements){
    io.out.bits.elements(name) := d
  }
  io.out.valid := ctrl_arb.io.out.valid
  assert(ctrl_arb.io.out.valid === data_arb.io.out.valid)

  if (hasFastUopOut) {
    val uop = ctrl_arb.io.out.bits.uop
    io.out.valid := RegNext(ctrl_arb.io.out.valid && !uop.robIdx.needFlush(io.redirect))
    // When hasFastUopOut, only uop comes at the same cycle with valid.
    // Other bits like data, fflags come at the next cycle after valid,
    // and they need to be selected with the fireVec.
    val dataVec = VecInit(io.in.map(_.bits).zip(fastVec).map{ case (d, f) => if (f) d else RegNext(d) })
    val sel = VecInit(io.in.map(_.fire)).asUInt
    io.out.bits := Mux1H(RegNext(sel), dataVec)
    // uop comes at the same cycle with valid and only RegNext is needed.
    io.out.bits.uop := RegEnable(uop, ctrl_arb.io.out.valid)
  }
}

class WbArbiter(cfgs: Seq[ExuConfig], numOut: Int, isFp: Boolean)(implicit p: Parameters) extends LazyModule {
  val priorities = cfgs.map(c => if(isFp) c.wbFpPriority else c.wbIntPriority)

  // NOTE:
  // 0 for direct connect (exclusive);
  // 1 for shared connect but non-blocked;
  // other for shared and may blocked
  val exclusivePorts = priorities.zipWithIndex.filter(_._1 == 0).map(_._2)
  val sharedPorts = priorities.zipWithIndex.filter(_._1 == 1).map(_._2)
  val otherPorts = priorities.zipWithIndex.filter(_._1 > 1).map(_._2)

  // Dirty code for Load2Fp: should be delayed for one more cycle
  val needRegNext = exclusivePorts.map(i => cfgs(i) == LdExeUnitCfg && isFp)

  val numInPorts = cfgs.length
  val numOutPorts = exclusivePorts.length + sharedPorts.length
  require(numOutPorts <= numOut)
  if (numOutPorts < numOut) {
    println(s"Warning: only $numOutPorts of $numOut ports are used!")
  }

  def splitN(in: Seq[Int], n: Int): Seq[Seq[Int]] = {
    if (n == 0) {
      Seq()
    }
    else if (n == 1) {
      Seq(in)
    } else {
      if (in.size < n) {
        Seq(in) ++ Seq.fill(n - 1)(Seq())
      } else {
        (0 until n).map(i => in.zipWithIndex.filter(_._2 % n == i).map(_._1).toSeq)
      }
    }
  }

  val otherConnections = splitN(otherPorts, sharedPorts.length)
  val sharedConnections = sharedPorts.zip(otherConnections).map{ case (s, o) => s +: o }
  val allConnections: Seq[Seq[Int]] = exclusivePorts.map(Seq(_)) ++ sharedConnections
  val hasWbPipeline = allConnections.map(_.map(cfgs(_).needWbPipeline(isFp)))
  val cfgHasFast = cfgs.map(_.hasFastUopOut)
  val hasFastUopOutVec = allConnections.map(_.map(cfgs(_).hasFastUopOut))
  val hasFastUopOut: Seq[Boolean] = hasFastUopOutVec.map(_.reduce(_ || _))
  hasFastUopOutVec.zip(hasFastUopOut).foreach{ case (vec, fast) =>
    if (fast && vec.contains(false)) {
      println("Warning: some exu does not have fastUopOut. It has extra one-cycle latency.")
    }
  }

  val sb = new StringBuffer(s"\n${if(isFp) "fp" else "int"} wb arbiter:\n")
  for ((port, i) <- exclusivePorts.zipWithIndex) {
    val hasFastUopOutS = if (hasFastUopOut(i)) s" (hasFastUopOut)" else ""
    sb.append(s"[ ${cfgs(port).name} ] -> out$hasFastUopOutS #$i\n")
  }
  for ((port, i) <- sharedPorts.zipWithIndex) {
    sb.append(s"[ ${cfgs(port).name} ")
    val useArb = otherConnections(i).nonEmpty
    for ((req, j) <- otherConnections(i).zipWithIndex) {
      val hasBuffer = if (hasWbPipeline(exclusivePorts.length + i)(j + 1)) "(buffered)" else ""
      sb.append(s"${cfgs(req).name}$hasBuffer ")
    }
    val hasFastUopOutS = if (hasFastUopOut(i + exclusivePorts.length)) s" (hasFastUopOut)" else ""
    sb.append(s"] -> ${if(useArb) "arb ->" else ""} out$hasFastUopOutS #${exclusivePorts.size + i}\n")
  }
  println(sb)

  lazy val module = new WbArbiterImp(this)
}

class WbArbiterImp(outer: WbArbiter)(implicit p: Parameters) extends LazyModuleImp(outer) {

  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val in = Vec(outer.numInPorts, Flipped(DecoupledIO(new ExuOutput)))
    val out = Vec(outer.numOutPorts, ValidIO(new ExuOutput))
  })

  val redirect = RegNextWithEnable(io.redirect)

  val exclusiveIn = outer.exclusivePorts.map(io.in(_))
  val sharedIn = outer.sharedPorts.map(io.in(_))

  // exclusive ports are connected directly
  io.out.take(exclusiveIn.size).zip(exclusiveIn).zipWithIndex.foreach{
    case ((out, in), i) =>
      val hasFastUopOut = outer.hasFastUopOut(i)
      out.valid := in.valid
      out.bits := in.bits
      require(!hasFastUopOut || !outer.needRegNext(i))
      if (hasFastUopOut) {
        // When hasFastUopOut, only uop comes at the same cycle with valid.
        out.valid := RegNext(in.valid && !in.bits.uop.robIdx.needFlush(redirect))
        out.bits.uop := RegEnable(in.bits.uop, in.valid)
      }
      if (outer.needRegNext(i)) {
        out.valid := RegNext(in.valid && !in.bits.uop.robIdx.needFlush(redirect))
        out.bits := RegEnable(in.bits, in.valid)
      }
      in.ready := true.B
  }

  // shared ports are connected with an arbiter
  for (i <- sharedIn.indices) {
    val portIndex = exclusiveIn.length + i
    val out = io.out(portIndex)
    val shared = outer.sharedConnections(i).zip(outer.hasWbPipeline(portIndex)).map { case (i, hasPipe) =>
      if (hasPipe) {
        // Some function units require int/fp sources and write to the other register file, such as f2i, i2f.
        // Their out.ready depends on the other function units and may cause timing issues.
        // For the function units that operate across int and fp, we add a buffer after their output.
        val flushFunc = (o: ExuOutput, r: Valid[Redirect]) => o.uop.robIdx.needFlush(r)
        if (outer.cfgHasFast(i)) {
          val ctrl_pipe = Wire(io.in(i).cloneType)
          val buffer = PipelineConnect(io.in(i), ctrl_pipe, flushFunc, redirect, io.in(i).bits, 1)
          buffer.extra.in := io.in(i).bits
          val buffer_out = Wire(io.in(i).cloneType)
          ctrl_pipe.ready := buffer_out.ready
          buffer_out.valid := ctrl_pipe.valid
          buffer_out.bits := buffer.extra.out
          buffer_out.bits.uop := ctrl_pipe.bits.uop
          buffer_out
        }
        else {
          PipelineNext(io.in(i), flushFunc, redirect)
        }
      }
      else io.in(i)
    }
    val hasFastUopOut = outer.hasFastUopOut(portIndex)
    val fastVec = outer.hasFastUopOutVec(portIndex)
    val arb = Module(new ExuWbArbiter(shared.size, hasFastUopOut, fastVec))
    arb.io.redirect <> redirect
    arb.io.in <> shared
    out.valid := arb.io.out.valid
    out.bits := arb.io.out.bits
    arb.io.out.ready := true.B
  }

  for (i <- 0 until outer.numInPorts) {
    XSPerfAccumulate(s"in_valid_$i", io.in(i).valid)
    XSPerfAccumulate(s"in_fire_$i", io.in(i).fire)
  }
  XSPerfHistogram("in_count", PopCount(io.in.map(_.valid)), true.B, 0, outer.numInPorts, 1)
  XSPerfHistogram("out_count", PopCount(io.out.map(_.valid)), true.B, 0, outer.numInPorts, 1)
}

class WbArbiterWrapper(
  exuConfigs: Seq[ExuConfig],
  numIntOut: Int,
  numFpOut: Int
)(implicit p: Parameters) extends LazyModule with HasWritebackSource {
  val numInPorts = exuConfigs.length

  val intConfigs = exuConfigs.filter(_.writeIntRf)
  val intArbiter = LazyModule(new WbArbiter(intConfigs, numIntOut, isFp = false))
  val intWbPorts = intArbiter.allConnections.map(c => c.map(intConfigs(_)))
  val numIntWbPorts = intWbPorts.length
  val intConnections = intArbiter.allConnections

  val fpConfigs = exuConfigs.filter(_.writeFpRf)
  val fpArbiter = LazyModule(new WbArbiter(fpConfigs, numFpOut, isFp = true))
  val fpWbPorts = fpArbiter.allConnections.map(c => c.map(fpConfigs(_)))
  val numFpWbPorts = fpWbPorts.length
  val fpConnections = fpArbiter.allConnections

  val numOutPorts = intArbiter.numOutPorts + fpArbiter.numOutPorts

  override val writebackSourceParams: Seq[WritebackSourceParams] = {
    // To optimize write ports, we can remove the duplicate ports.
    val duplicatePorts = fpWbPorts.filter(cfgs => cfgs.length == 1 && intWbPorts.contains(cfgs))
    val duplicateSource = exuConfigs.zipWithIndex.filter(cfg => duplicatePorts.contains(Seq(cfg._1))).map(_._2)
    val duplicateSink = intWbPorts.zipWithIndex.filter(cfgs => duplicatePorts.contains(cfgs._1)).map(_._2)
    require(duplicateSource.length == duplicatePorts.length)
    require(duplicateSink.length == duplicatePorts.length)
    val effectiveConfigs = intWbPorts ++ fpWbPorts.filterNot(cfg => duplicatePorts.contains(cfg))
    val simpleConfigs = exuConfigs.filter(cfg => !cfg.writeFpRf && !cfg.writeIntRf).map(p => Seq(p))
    Seq(new WritebackSourceParams(effectiveConfigs ++ simpleConfigs))
  }
  override lazy val writebackSourceImp: HasWritebackSourceImp = module

  lazy val module = new LazyModuleImp(this)
    with HasXSParameter with HasWritebackSourceImp with HasExuWbHelper {

    val io = IO(new Bundle() {
      val hartId = Input(UInt(8.W))
      val redirect = Flipped(ValidIO(new Redirect))
      val in = Vec(numInPorts, Flipped(DecoupledIO(new ExuOutput)))
      val out = Vec(numOutPorts, ValidIO(new ExuOutput))
    })

    override def writebackSource: Option[Seq[Seq[Valid[ExuOutput]]]] = {
      // To optimize write ports, we can remove the duplicate ports.
      val duplicatePorts = fpWbPorts.zipWithIndex.filter(cfgs => cfgs._1.length == 1 && intWbPorts.contains(cfgs._1))
      val duplicateSource = exuConfigs.zipWithIndex.filter(cfg => duplicatePorts.map(_._1).contains(Seq(cfg._1))).map(_._2)
      val duplicateSink = intWbPorts.zipWithIndex.filter(cfgs => duplicatePorts.map(_._1).contains(cfgs._1)).map(_._2)
      require(duplicateSource.length == duplicatePorts.length)
      require(duplicateSink.length == duplicatePorts.length)
      // effectivePorts: distinct write-back ports that write to the regfile
      val effectivePorts = io.out.zipWithIndex.filterNot(i => duplicatePorts.map(_._2).contains(i._2 - numIntWbPorts))
      // simplePorts: write-back ports that don't write to the regfile but update the ROB states
      val simplePorts = exuConfigs.zip(io.in).filter(cfg => !cfg._1.writeFpRf && !cfg._1.writeIntRf)
      val simpleWriteback = simplePorts.map(_._2).map(decoupledIOToValidIO)
      val writeback = WireInit(VecInit(effectivePorts.map(_._1) ++ simpleWriteback))
      for ((sink, source) <- duplicateSink.zip(duplicateSource)) {
        writeback(sink).valid := io.in(source).valid
      }
      Some(Seq(writeback))
    }

    // ready is set to true.B as default (to be override later)
    io.in.foreach(_.ready := true.B)

    intArbiter.module.io.redirect <> io.redirect
    val intWriteback = io.in.zip(exuConfigs).filter(_._2.writeIntRf)
    intArbiter.module.io.in.zip(intWriteback).foreach { case (arb, (wb, cfg)) =>
      // When the function unit does not write fp regfile, we don't need to check fpWen
      arb.valid := wb.valid && (!cfg.writeFpRf.B || !wb.bits.uop.ctrl.fpWen)
      arb.bits := wb.bits
      when (arb.valid) {
        wb.ready := arb.ready
      }
    }
    if (env.EnableDifftest || env.AlwaysBasicDiff) {
      intArbiter.module.io.out.foreach(out => {
        val difftest = Module(new DifftestIntWriteback)
        difftest.io.clock := clock
        difftest.io.coreid := io.hartId
        difftest.io.valid := out.valid && out.bits.uop.ctrl.rfWen
        difftest.io.dest := out.bits.uop.pdest
        difftest.io.data := out.bits.data
      })
    }

    fpArbiter.module.io.redirect <> io.redirect
    val fpWriteback = io.in.zip(exuConfigs).filter(_._2.writeFpRf)
    fpArbiter.module.io.in.zip(fpWriteback).foreach{ case (arb, (wb, cfg)) =>
      // When the function unit does not write fp regfile, we don't need to check fpWen
      arb.valid := wb.valid && (!cfg.writeIntRf.B || wb.bits.uop.ctrl.fpWen)
      arb.bits := wb.bits
      when (arb.valid) {
        wb.ready := arb.ready
      }
    }
    if (env.EnableDifftest || env.AlwaysBasicDiff) {
      fpArbiter.module.io.out.foreach(out => {
        val difftest = Module(new DifftestFpWriteback)
        difftest.io.clock := clock
        difftest.io.coreid := io.hartId
        difftest.io.valid := out.valid // all fp instr will write fp rf
        difftest.io.dest := out.bits.uop.pdest
        difftest.io.data := out.bits.data
      })
    }

    io.out <> intArbiter.module.io.out ++ fpArbiter.module.io.out
  }
}

class Wb2Ctrl(configs: Seq[ExuConfig])(implicit p: Parameters) extends LazyModule
  with HasWritebackSource with HasWritebackSink {
  override def generateWritebackIO(
    thisMod: Option[HasWritebackSource],
    thisModImp: Option[HasWritebackSourceImp]
  ): Unit = {
    require(writebackSinks.length == 1)
    val sink = writebackSinks.head
    val sourceMod = writebackSinksMod(thisMod, thisModImp).head
    module.io.in := sink._1.zip(sink._2).zip(sourceMod).flatMap(x => x._1._1.writebackSource1(x._2)(x._1._2))
  }

  lazy val module = new LazyModuleImp(this)
    with HasWritebackSourceImp
    with HasXSParameter
  {
    val io = IO(new Bundle {
      val redirect = Flipped(ValidIO(new Redirect))
      val in = Vec(configs.length, Input(Decoupled(new ExuOutput)))
      val out = Vec(configs.length, ValidIO(new ExuOutput))
      val delayedLoadError = Vec(LoadPipelineWidth, Input(Bool())) // Dirty fix of data ecc error timing
    })
    val redirect = RegNextWithEnable(io.redirect)

    for (((out, in), config) <- io.out.zip(io.in).zip(configs)) {
      out.valid := in.fire
      out.bits := in.bits
      if (config.hasFastUopOut || config.hasLoadError) {
        out.valid := RegNext(in.fire && !in.bits.uop.robIdx.needFlush(redirect))
        out.bits.uop := RegEnable(in.bits.uop, in.fire)
      }
    }

    if(EnableAccurateLoadError){
      for ((((out, in), config), delayed_error) <- io.out.zip(io.in).zip(configs)
        .filter(_._2.hasLoadError)
        .zip(io.delayedLoadError)
      ){
        // overwrite load exception writeback
        out.bits.uop.cf.exceptionVec(loadAccessFault) := delayed_error ||
          RegEnable(in.bits.uop.cf.exceptionVec(loadAccessFault), in.valid)
      }
    }

    override def writebackSource: Option[Seq[Seq[ValidIO[ExuOutput]]]] = Some(Seq(io.out))
  }

  override val writebackSourceParams: Seq[WritebackSourceParams] = {
    Seq(new WritebackSourceParams(configs.map(cfg => Seq(cfg))))
  }
  override lazy val writebackSourceImp: HasWritebackSourceImp = module
}
