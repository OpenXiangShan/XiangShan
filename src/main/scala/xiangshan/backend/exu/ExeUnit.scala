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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.hierarchy.{Definition, instantiable}
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import xiangshan.frontend.ftq
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.fu.{CSRFileIO, FenceIO, FuType, FuncUnit, FuncUnitInput, UncertainLatency}
import xiangshan.backend.Bundles._
import xiangshan.{AddrTransType, HasXSParameter, Redirect, Resolve, XSBundle, XSModule, ExceptSparseVec}
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.fu.vector.Bundles.{VType, Vxrm}
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.wrapper.{CSRInput, CSRToDecode}
import xiangshan.backend.fu.FuConfig._
import xiangshan._

class ExeUnitIO(params: ExeUnitParams)(implicit p: Parameters) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect()))
  val in = Flipped(DecoupledIO(new NewExuInput(params)))
  val faluIn = Option.when(params.hasFaluFu)(Flipped(DecoupledIO(new NewExuInput(params))))
  val out = DecoupledIO(new NewExuOutput(params))
  val outToFalu = Option.when(params.hasFmulFu)(ValidIO(new NewExuInput(params)))
  val uncertainWakeupOut = Option.when(params.needUncertainWakeup)(DecoupledIO(new IssueQueueIQWakeUpBundle(params.exuIdx, params.backendParam, params.copyWakeupOut, params.copyNum)))
  val csrin = Option.when(params.hasCSR)(new CSRInput)
  val csrio = Option.when(params.hasCSR)(new CSRFileIO)
  val toFrontendBJUResolve = Option.when(params.hasBrhFu)(Valid(new Resolve))
  val I2FDataIn = Option.when(params.needDataFromI2F)(Flipped(ValidIO(UInt(XLEN.W))))
  val F2IDataIn = Option.when(params.needDataFromF2I)(Flipped(ValidIO(UInt(XLEN.W))))
  val csrToDecode = Option.when(params.hasCSR)(Output(new CSRToDecode))
  val fenceio = Option.when(params.hasFence)(new FenceIO)
  val frm = Option.when(params.needSrcFrm)(Input(Frm()))
  val vxrm = Option.when(params.needSrcVxrm)(Input(Vxrm()))
  val vtype = Option.when(params.writeVlRf)((Valid(new VType)))
  val vlIsZero = Option.when(params.writeVlRf)(Output(Bool()))
  val vlIsVlmax = Option.when(params.writeVlRf)(Output(Bool()))
  val instrAddrTransType = Option.when(params.hasNewJmpFu || params.hasLinkFu || params.hasBrhFu)(Input(new AddrTransType))
}

class ExeUnitImp(implicit p: Parameters, val exuParams: ExeUnitParams) extends XSModule with HasXSParameter with HasCriticalErrors {
  private val fuCfgs = exuParams.fuConfigs

  val io = IO(new ExeUnitIO(exuParams))

  val funcUnits = fuCfgs.map(cfg => {
    assert(cfg.fuGen != null, cfg.name + "Cfg'fuGen is null !!!")
    val module = cfg.fuGen(p, cfg)
    module
  })
  private val faluFuncUnit = funcUnits.find(_.cfg.isFalu)
  io.faluIn.foreach { _ =>
    assert(funcUnits.count(_.cfg.isFalu) == 1, "ExeUnit should contain exactly one FALU when ExeUnit.faluIn is available")
  }

  if (EnableClockGate) {
    fuCfgs.zip(funcUnits).foreach { case (cfg, fu) =>
      val clk_en = WireInit(false.B)
      val fuVld_en = WireInit(false.B)
      val fuVld_en_reg = RegInit(false.B)
      val uncer_en_reg = RegInit(false.B)

      def latReal: Int = cfg.latency.latencyVal.getOrElse(0)
      def extralat: Int = cfg.latency.extraLatencyVal.getOrElse(0)

      val uncerLat = cfg.latency.uncertainEnable.nonEmpty
      val lat0 = (latReal == 0 && !uncerLat).asBool
      val latN = (latReal >  0 && !uncerLat).asBool

      val fuVldVec = (fu.io.in.valid && latN) +: Seq.fill(latReal)(RegInit(false.B))
      val fuRdyVec = Seq.fill(latReal)(Wire(Bool())) :+ fu.io.out.ready

      for (i <- 0 until latReal) {
        fuRdyVec(i) := !fuVldVec(i + 1) || fuRdyVec(i + 1)
      }

      for (i <- 1 to latReal) {
        when(fuRdyVec(i - 1) && fuVldVec(i - 1)) {
          fuVldVec(i) := fuVldVec(i - 1)
        }.elsewhen(fuRdyVec(i)) {
          fuVldVec(i) := false.B
        }
      }
      fuVld_en := fuVldVec.map(v => v).reduce(_ || _)
      fuVld_en_reg := fuVld_en

      when(uncerLat.asBool && fu.io.in.fire) {
        uncer_en_reg := true.B
      }.elsewhen(uncerLat.asBool && fu.io.out.fire) {
        uncer_en_reg := false.B
      }

      when(lat0 && fu.io.in.fire) {
        clk_en := true.B
      }.elsewhen(latN && fuVld_en || fuVld_en_reg) {
        clk_en := true.B
      }.elsewhen(uncerLat.asBool && fu.io.in.fire || uncer_en_reg) {
        clk_en := true.B
      }

      if (cfg.ckAlwaysEn) {
        clk_en := true.B
      }

      if (latReal != 0 || uncerLat) {
        fu.clock := ClockGate(ClockGate.genTeSink.cgen, clk_en, clock)
      }
      XSPerfAccumulate(s"clock_gate_en_${fu.cfg.name}", clk_en)
    }
  }

  exuParams.wbPortConfigs.map{
    x => x match {
      case IntWB(port, priority) => assert(priority >= 0 && priority <= 2,
        s"${exuParams.name}: WbPort must priority=0 or priority=1")
      case FpWB(port, priority) => assert(priority >= 0 && priority <= 2,
        s"${exuParams.name}: WbPort must priority=0 or priority=1")
      case VfWB (port, priority) => assert(priority >= 0 && priority <= 2,
        s"${exuParams.name}: WbPort must priority=0 or priority=1")
      case _ =>
    }
  }
  if(backendParams.debugEn) {
    dontTouch(io.out.ready)
  }
  // rob flush --> funcUnits
  funcUnits.zipWithIndex.foreach { case (fu, i) =>
    fu.io.flush <> io.flush
  }

  def acceptCond(input: NewExuInput): Seq[Bool] = {
    input.params.fuConfigs.map(_.fuSel(input))
  }

  def connectFuInput(source: DecoupledIO[NewExuInput], sink: DecoupledIO[FuncUnitInput]): Unit = {
    sink.valid := source.valid
    source.ready := sink.ready

    sink.bits.data.pc          .foreach(x => x := source.bits.data.pc.get)
    sink.bits.data.nextPcOffset.foreach(x => x := source.bits.data.nextPcOffset.get)
    sink.bits.data.imm         := source.bits.data.imm
    sink.bits.ctrl.fuOpType    := source.bits.ctrl.fuOpType
    sink.bits.ctrl.robIdx      := source.bits.robIdx
    sink.bits.ctrl.pdest       := source.bits.toRF.pdest
    sink.bits.ctrl.pdestV0     .foreach(x => x := source.bits.toRF.pdestV0.get)
    sink.bits.ctrl.pdestVl     .foreach(x => x := source.bits.toRF.pdestVl.get)
    sink.bits.ctrl.rfWen       .foreach(x => x := source.bits.ctrl.rfWen.get)
    sink.bits.ctrl.fpWen       .foreach(x => x := source.bits.ctrl.fpWen.get)
    sink.bits.ctrl.vecWen      .foreach(x => x := source.bits.ctrl.vecWen.get)
    sink.bits.ctrl.v0Wen       .foreach(x => x := source.bits.ctrl.v0Wen.get)
    sink.bits.ctrl.vlWen       .foreach(x => x := source.bits.ctrl.vlWen.get)
    sink.bits.ctrl.flushPipe   .foreach(x => x := source.bits.ctrl.flushPipe.get)
    sink.bits.ctrl.isRVC       .foreach(x => x := source.bits.ctrl.isRVC.get)
    sink.bits.ctrl.rasAction   .foreach(x => x := source.bits.ctrl.rasAction.get)
    sink.bits.ctrl.ftqIdx      .foreach(x => x := source.bits.ctrl.ftqIdx.get)
    sink.bits.ctrl.ftqOffset   .foreach(x => x := source.bits.ctrl.ftqOffset.get)
    sink.bits.ctrl.predictInfo .foreach(x => x := source.bits.ctrl.predictInfo.get)
    sink.bits.ctrl.fflagsWen   .foreach(x => x := source.bits.ctrl.fflagsWen.get)
    sink.bits.ctrl.vpu         .foreach(x => x := source.bits.ctrl.vpu.get)
    sink.bits.ctrl.vpu         .foreach(x => x.fpu.isFpToVecInst := 0.U)
    sink.bits.ctrl.vpu         .foreach(x => x.fpu.isFP32Instr   := 0.U)
    sink.bits.ctrl.vpu         .foreach(x => x.fpu.isFP64Instr   := 0.U)
    sink.bits.ctrl.frm         .foreach(x => x := source.bits.ctrl.frm.get)
    sink.bits.ctrl.oldVType    .foreach(x => x := source.bits.ctrl.oldVType.get)
    sink.bits.perfDebugInfo    .foreach(_ := source.bits.perfDebugInfo.get)
    sink.bits.debug_seqNum     .foreach(_ := source.bits.debug_seqNum.get)
  }

  def connectFuPipe(sourcePipe: (Seq[NewExuInput], Seq[Bool]), fu: FuncUnit): Unit = {
    val latency = fu.cfg.latency.latencyVal.getOrElse(0)
    for (i <- 0 until (latency + 1)) {
      val sink = fu.io.in.bits.ctrlPipe.get(i)
      val source = sourcePipe._1(i)
      fu.io.in.bits.validPipe.get(i) := sourcePipe._2(i)
      sink.fuOpType := source.ctrl.fuOpType
      sink.robIdx := source.robIdx
      sink.pdest := source.toRF.pdest
      sink.pdestV0.foreach(_ := source.toRF.pdestV0.get)
      sink.pdestVl.foreach(_ := source.toRF.pdestVl.get)
      sink.rfWen.foreach(      x => x := source.ctrl.rfWen.get)
      sink.fpWen.foreach(      x => x := source.ctrl.fpWen.get)
      sink.vecWen.foreach(     x => x := source.ctrl.vecWen.get)
      sink.v0Wen.foreach(      x => x := source.ctrl.v0Wen.get)
      sink.vlWen.foreach(      x => x := source.ctrl.vlWen.get)
      sink.flushPipe.foreach(  x => x := source.ctrl.flushPipe.get)
      sink.isRVC.foreach(      x => x := source.ctrl.isRVC.get)
      sink.rasAction.foreach(  x => x := source.ctrl.rasAction.get)
      sink.ftqIdx.foreach(     x => x := source.ctrl.ftqIdx.get)
      sink.ftqOffset.foreach(  x => x := source.ctrl.ftqOffset.get)
      sink.predictInfo.foreach(x => x := source.ctrl.predictInfo.get)
      sink.fflagsWen.foreach(x => x := source.ctrl.fflagsWen.get)
      sink.vpu.foreach(x => x := source.ctrl.vpu.get)
      sink.vpu.foreach(x => x.fpu.isFpToVecInst := 0.U)
      sink.vpu.foreach(x => x.fpu.isFP32Instr := 0.U)
      sink.vpu.foreach(x => x.fpu.isFP64Instr := 0.U)
      sink.vpu.foreach(x => x.maskVecGen := 0.U)
      sink.oldVType.foreach(x => x := source.ctrl.oldVType.get)
      sink.frm.foreach(_ := source.ctrl.frm.get)
      val sinkData = fu.io.in.bits.dataPipe.get(i)
      sinkData.src.zip(source.data.src).foreach { case (fuSrc, exuSrc) => fuSrc := exuSrc }
      sinkData.FmulToFaluDataInput.foreach(x => x := source.data.FaluInputFromFmul.get)
      sinkData.v0.foreach(_ := source.data.v0.get)
      sinkData.vl.foreach(_ := source.data.vl.get)
      sinkData.pc.foreach(x => x := source.data.pc.get)
      sinkData.nextPcOffset.foreach(x => x := source.data.nextPcOffset.get)
      sinkData.imm := source.data.imm
    }
  }

  def connectFuCopySrc(source: NewExuInput, fu: FuncUnit): Unit = {
    fu.io.in.bits.data.src.zip(source.data.src).foreach { case (fuSrc, exuSrc) => fuSrc := exuSrc }
    fu.io.in.bits.data.FmulToFaluDataInput.foreach(x => x := source.data.FaluInputFromFmul.get)
    fu.io.in.bits.data.v0.foreach(_ := source.data.v0.get)
    fu.io.in.bits.data.vl.foreach(_ := source.data.vl.get)
  }

  val in1ToN = Module(new Dispatcher(new NewExuInput(exuParams), funcUnits.length, acceptCond))

  // ExeUnit.in <---> Dispatcher.in
  in1ToN.io.in.valid := io.in.valid
  in1ToN.io.in.bits := io.in.bits
  io.in.ready := in1ToN.io.in.ready

  def pipelineReg(init: NewExuInput, valid: Bool, latency: Int, flush: ValidIO[Redirect]): (Seq[NewExuInput], Seq[Bool]) = {
    val validVec = valid +: Seq.fill(latency)(RegInit(false.B))
    val inVec = init +: Seq.fill(latency)(Reg(new NewExuInput(exuParams)))
    val robIdxVec = inVec.map(_.robIdx)
    // if flush(0), valid 0 will not given, so set flushVec(0) to false.B
    val flushVec = validVec.zip(robIdxVec).map(x => x._1 && x._2.needFlush(flush))
    for (i <- 1 to latency) {
      validVec(i) := validVec(i - 1) && !flushVec(i - 1)
      inVec(i) := inVec(i - 1)
    }
    (inVec, validVec)
  }
  val latencyMax = fuCfgs.map(_.latency.latencyVal.getOrElse(0)).max
  val inPipe = pipelineReg(io.in.bits, io.in.valid, latencyMax, io.flush)
  val faluPipe = io.faluIn.map(faluIn => pipelineReg(faluIn.bits, faluIn.valid, faluFuncUnit.get.cfg.latency.latencyVal.getOrElse(0), io.flush))
  // Dispatcher.out <---> FunctionUnits
  in1ToN.io.out.zip(funcUnits).foreach {
    case (source: DecoupledIO[NewExuInput], fu) if io.faluIn.nonEmpty && fu.cfg.isFalu =>
      source.ready := true.B
    case (source: DecoupledIO[NewExuInput], fu) =>
      connectFuInput(source, fu.io.in)
  }
  io.faluIn.foreach { faluIn =>
    XSError(io.in.valid && io.in.bits.ctrl.fuType === FuType.falu.U, "[ExeUnit] FALU input should use dedicated io.faluIn")
    connectFuInput(faluIn, faluFuncUnit.get.io.in)
  }
  funcUnits.filter(_.cfg.latency.latencyVal.nonEmpty).map{ fu =>
    if (fu.cfg == I2fCfg){
      println(s"I2fCfg latency = ${fu.cfg.latency.latencyVal.getOrElse(0)}")
    }
    val sourcePipe = if (io.faluIn.nonEmpty && fu.cfg.isFalu) faluPipe.get else inPipe
    connectFuPipe(sourcePipe, fu)
  }

  funcUnits.zip(exuParams.idxCopySrc).map{ case(fu, idx) =>
    val source = if (io.faluIn.nonEmpty && fu.cfg.isFalu) io.faluIn.get.bits else io.in.bits
    connectFuCopySrc(source, fu)
  }

  private val OutresVecs = funcUnits.map { fu =>
    def latDiff :Int = fu.cfg.latency.extraLatencyVal.getOrElse(0)
    val OutresVec = fu.io.out.bits.res +: Seq.fill(latDiff)(Reg(chiselTypeOf(fu.io.out.bits.res)))
    for (i <- 1 to latDiff) {
      OutresVec(i) := OutresVec(i - 1)
    }
    OutresVec
  }
  OutresVecs.foreach(vec => vec.foreach(res =>dontTouch(res)))

  private val fuOutValidOH = Wire(Vec(funcUnits.length, Bool()))
  fuOutValidOH := funcUnits.map{ case fu => {
    if (needUncertainWakeupFuConfigs.contains(fu.cfg)){
      println(p"${exuParams.name}: ${fu.cfg.name} is needUncertainWakeupFuConfig")
      !funcUnits.filterNot(x => needUncertainWakeupFuConfigs.contains(x.cfg)).map(y => y.io.out.valid).fold(false.B)(_ || _) && fu.io.out.valid
    }
    else {
      fu.io.out.valid
    }
  }
  }
  dontTouch(fuOutValidOH)
  XSError(PopCount(fuOutValidOH) > 1.U, p"fuOutValidOH ${Binary(fuOutValidOH.asUInt)} should be one-hot)\n")
  private val fuOutBitsVec = funcUnits.map(_.io.out.bits)
  private val fuOutresVec = OutresVecs.map(_.last)
  private val fuRedirectVec: Seq[Option[ValidIO[Redirect]]] = fuOutresVec.map(_.redirect)

  // Assume that one fu can only write int or fp or vec,
  // otherwise, wenVec should be assigned to wen in fu.
  private val fuIntWenVec = funcUnits.map(x => x.cfg.needIntWen.B && x.io.out.bits.ctrl.rfWen.getOrElse(false.B))
  private val fuFpWenVec  = funcUnits.map( x => x.cfg.needFpWen.B  && x.io.out.bits.ctrl.fpWen.getOrElse(false.B))
  private val fuVecWenVec = funcUnits.map(x => x.cfg.needVecWen.B && x.io.out.bits.ctrl.vecWen.getOrElse(false.B))
  private val fuV0WenVec = funcUnits.map(x => x.cfg.needV0Wen.B && x.io.out.bits.ctrl.v0Wen.getOrElse(false.B))
  private val fuVlWenVec = funcUnits.map(x => x.cfg.needVlWen.B && x.io.out.bits.ctrl.vlWen.getOrElse(false.B))
  // FunctionUnits <---> ExeUnit.out

  private val outData = fuOutresVec.map(_.data)

  private val outIntDataValid = Option.when(funcUnits.exists(_.cfg.writeIntRf))(WireInit(false.B))
  private val outFpDataValid  = Option.when(funcUnits.exists(_.cfg.writeFpRf)) (WireInit(false.B))
  private val outVecDataValid = Option.when(funcUnits.exists(_.cfg.writeVecRf))(WireInit(false.B))
  private val outV0DataValid  = Option.when(funcUnits.exists(_.cfg.writeV0Rf)) (WireInit(false.B))
  private val outVlDataValid  = Option.when(funcUnits.exists(_.cfg.writeVlRf)) (WireInit(false.B))
  private val outIntData = Option.when(funcUnits.exists(_.cfg.writeIntRf))(WireInit(0.U(exuParams.destDataBitsMax.W)))
  private val outFpData  = Option.when(funcUnits.exists(_.cfg.writeFpRf)) (WireInit(0.U(exuParams.destDataBitsMax.W)))
  private val outVecData = Option.when(funcUnits.exists(_.cfg.writeVecRf))(WireInit(0.U(exuParams.destDataBitsMax.W)))
  private val outV0Data  = Option.when(funcUnits.exists(_.cfg.writeV0Rf)) (WireInit(0.U(exuParams.destDataBitsMax.W)))
  private val outVlData  = Option.when(funcUnits.exists(_.cfg.writeVlRf)) (WireInit(0.U(exuParams.destDataBitsMax.W)))

  Option.when(funcUnits.exists(_.cfg.writeIntRf)) {
    val vld = if (exuParams.needDataFromF2I) {
        (funcUnits.zip(fuOutValidOH).filter { case (fu, _) => fu.cfg.writeIntRf }.map { case (fu, fuoutOH) =>
          !io.F2IDataIn.get.valid && fuoutOH && fu.io.out.bits.ctrl.rfWen.getOrElse(false.B) } :+ io.F2IDataIn.get.valid)
      } else {
        (funcUnits.zip(fuOutValidOH).filter { case (fu, _) => fu.cfg.writeIntRf }.map { case (fu, fuoutOH) =>
          fuoutOH && fu.io.out.bits.ctrl.rfWen.getOrElse(false.B) })
      }
    val data = if (exuParams.needDataFromF2I) {
        (funcUnits.zip(fuOutresVec).filter { case (fu, _) => fu.cfg.writeIntRf }.map { case (_, fuout) => fuout.data } :+ io.F2IDataIn.get.bits)
      } else {
        (funcUnits.zip(fuOutresVec).filter { case (fu, _) => fu.cfg.writeIntRf }.map { case (_, fuout) => fuout.data })
      }
    outIntDataValid.foreach(_ := Cat(vld).orR)
    outIntData.foreach(_ := Mux1H(vld, data))
  }
  Option.when(funcUnits.exists(_.cfg.writeFpRf)) {
    val vld = if (exuParams.needDataFromI2F) {
        (funcUnits.zip(fuOutValidOH).filter { case (fu, _) => fu.cfg.writeFpRf }.map { case (fu, fuoutOH) =>
          !io.I2FDataIn.get.valid && fuoutOH && fu.io.out.bits.ctrl.fpWen.getOrElse(false.B) } :+ io.I2FDataIn.get.valid)
      } else {
        (funcUnits.zip(fuOutValidOH).filter { case (fu, _) => fu.cfg.writeFpRf }.map { case (fu, fuoutOH) =>
          fuoutOH && fu.io.out.bits.ctrl.fpWen.getOrElse(false.B) })
      }
    val data = if (exuParams.needDataFromI2F) {
        (funcUnits.zip(fuOutresVec).filter { case (fu, _) => fu.cfg.writeFpRf }.map { case (_, fuout) => fuout.data } :+ io.I2FDataIn.get.bits)
      } else {
        (funcUnits.zip(fuOutresVec).filter { case (fu, _) => fu.cfg.writeFpRf }.map { case (_, fuout) => fuout.data })
      }
    outFpDataValid.foreach(_ := Cat(vld).orR)
    outFpData.foreach(_ := Mux1H(vld, data))
  }
  Option.when(funcUnits.exists(_.cfg.writeVecRf)) {
    val vld = (funcUnits.zip(fuOutValidOH).filter { case (fu, _) => fu.cfg.writeVecRf }.map { case (fu, fuoutOH) =>
      fuoutOH && fu.io.out.bits.ctrl.vecWen.getOrElse(false.B) })
    val data = (funcUnits.zip(fuOutresVec).filter { case (fu, _) => fu.cfg.writeVecRf }.map { case (_, fuout) => fuout.data })
    outVecDataValid.foreach(_ := Cat(vld).orR)
    outVecData.foreach(_ := Mux1H(vld, data))
  }
  Option.when(funcUnits.exists(_.cfg.writeV0Rf)) {
    val vld = (funcUnits.zip(fuOutValidOH).filter { case (fu, _) => fu.cfg.writeV0Rf }.map { case (fu, fuoutOH) =>
      fuoutOH && fu.io.out.bits.ctrl.v0Wen.getOrElse(false.B) })
    val data = (funcUnits.zip(fuOutresVec).filter { case (fu, _) => fu.cfg.writeV0Rf }.map { case (_, fuout) => fuout.data })
    outV0DataValid.foreach(_ := Cat(vld).orR)
    outV0Data.foreach(_ := Mux1H(vld, data))
  }
  Option.when(funcUnits.exists(_.cfg.writeVlRf)) {
    val vld = (funcUnits.zip(fuOutValidOH).filter { case (fu, _) => fu.cfg.writeVlRf }.map { case (fu, fuoutOH) =>
      fuoutOH && fu.io.out.bits.ctrl.vlWen.getOrElse(false.B) })
    val data = (funcUnits.zip(fuOutresVec).filter { case (fu, _) => fu.cfg.writeVlRf }.map { case (_, fuout) => fuout.data })
    outVlDataValid.foreach(_ := Cat(vld).orR)
    outVlData.foreach(_ := Mux1H(vld, data))
  }

  val criticalErrors = funcUnits.filter(fu => fu.cfg.needCriticalErrors).flatMap(fu => fu.getCriticalErrors)
  generateCriticalErrors()

  val F2IIntWen = io.F2IDataIn.getOrElse(0.U.asTypeOf(ValidIO(UInt(XLEN.W)))).valid

  io.out.valid := Cat(fuOutValidOH).orR
  funcUnits.foreach{ fu =>
    fu.io.out.ready := io.out.ready
    fu.io.wakeupSuccess.foreach(_ := false.B)
  }
  io.uncertainWakeupOut.foreach{ out => {
    val uncertainFus = funcUnits.filter(x => needUncertainWakeupFuConfigs.contains(x.cfg))
    if (uncertainFus.length == 1) {
      val fu = uncertainFus(0)
      out.valid := fu.io.outValidAhead3Cycle.get
      fu.io.wakeupSuccess.get := out.ready
      out.bits := 0.U.asTypeOf(out.bits)
      // div
      fu.io.out.bits.ctrl.rfWen.foreach(x => out.bits.rfWen := x)
      fu.io.out.bits.ctrl.fpWen.foreach(x => out.bits.fpWen := x)
      fu.io.out.bits.ctrl.vecWen.foreach(x => out.bits.vecWen := x)
      fu.io.out.bits.ctrl.v0Wen.foreach(x => out.bits.v0Wen := x)
      fu.io.out.bits.ctrl.vlWen.foreach(x => out.bits.vlWen := x)
      out.bits.pdest := fu.io.out.bits.ctrl.pdest
      // csr
      if (fu.cfg.isCsr) {
        out.bits.rfWen := fu.io.outRFWenAhead3Cycle.get
        out.bits.pdest := fu.io.outPdestAhead3Cycle.get
      }
    }
    else {
      val outOH = VecInit(uncertainFus.map(_.io.outValidAhead3Cycle.get))
      val outBits = uncertainFus.map(_.io.out.bits)
      out.valid := outOH.asUInt.orR
      out.bits := 0.U.asTypeOf(out.bits)
      outBits(0).ctrl.rfWen.foreach(x =>  out.bits.rfWen  := Mux1H(outOH, outBits.map(_.ctrl.rfWen .get)))
      outBits(0).ctrl.fpWen.foreach(x =>  out.bits.fpWen  := Mux1H(outOH, outBits.map(_.ctrl.fpWen .get)))
      outBits(0).ctrl.vecWen.foreach(x => out.bits.vecWen := Mux1H(outOH, outBits.map(_.ctrl.vecWen.get)))
      outBits(0).ctrl.v0Wen.foreach(x =>  out.bits.v0Wen  := Mux1H(outOH, outBits.map(_.ctrl.v0Wen .get)))
      outBits(0).ctrl.vlWen.foreach(x =>  out.bits.vlWen  := Mux1H(outOH, outBits.map(_.ctrl.vlWen .get)))
      out.bits.pdest := Mux1H(outOH, outBits.map(_.ctrl.pdest))
      out.bits.pdestV0 := Mux1H(outOH, outBits.map(_.ctrl.pdestV0.getOrElse(0.U)))
      out.bits.pdestVl := Mux1H(outOH, outBits.map(_.ctrl.pdestVl.getOrElse(0.U)))
    }
  }
  }
  // select one fu's result

  io.out.bits.toIntRf.          foreach(x => x.valid := Mux1H(fuOutValidOH, fuIntWenVec) || F2IIntWen)
  io.out.bits.toIntRf.          foreach(x => x.bits  := outIntData.get)
  io.out.bits.toFpRf.           foreach(x => x.valid := Mux1H(fuOutValidOH, fuFpWenVec))
  io.out.bits.toFpRf.           foreach(x => x.bits  := outFpData.get)
  io.out.bits.toVecRf.          foreach(x => x.valid := Mux1H(fuOutValidOH, fuVecWenVec))
  io.out.bits.toVecRf.          foreach(x => x.bits  := outVecData.get)
  io.out.bits.toV0Rf.           foreach(x => x.valid := Mux1H(fuOutValidOH, fuV0WenVec))
  io.out.bits.toV0Rf.           foreach(x => x.bits  := outV0Data.get)
  io.out.bits.toVlRf.           foreach(x => x.valid := Mux1H(fuOutValidOH, fuVlWenVec))
  io.out.bits.toVlRf.           foreach(x => x.bits  := outVlData.get)
  io.out.bits.pdest                                  := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.pdest))
  io.out.bits.pdestV0.                foreach(x => x := Mux1H(fuOutValidOH, funcUnits.map(_.io.out.bits.ctrl.pdestV0.getOrElse(0.U))))
  io.out.bits.pdestVl.                foreach(x => x := Mux1H(fuOutValidOH, funcUnits.map(_.io.out.bits.ctrl.pdestVl.getOrElse(0.U))))
  io.out.bits.redirect.               foreach(x => x := Mux1H((fuOutValidOH zip fuRedirectVec).filter(_._2.isDefined).map(x => (x._1, x._2.get))))
  io.out.bits.toRob.valid                            := Mux1H(fuOutValidOH, funcUnits.map(_.io.out.valid))
  io.out.bits.toRob.bits.robIdx                      := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.robIdx))
  io.out.bits.toRob.bits.fflags.      foreach(x => x := Mux1H(fuOutValidOH, fuOutresVec.map(_.fflags.getOrElse(0.U.asTypeOf(io.out.bits.toRob.bits.fflags.get)))))
  io.out.bits.toRob.bits.fflagsWen.   foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.fflagsWen.getOrElse(false.B))))
  io.out.bits.toRob.bits.vxsat.       foreach(x => x := Mux1H(fuOutValidOH, fuOutresVec.map(_.vxsat.getOrElse(0.U.asTypeOf(io.out.bits.toRob.bits.vxsat.get)))))
  io.out.bits.toRob.bits.exceptionVec := ExceptSparseVec.mux1h(fuOutValidOH, fuOutBitsVec.map(_.ctrl.exceptionVec))
  io.out.bits.toRob.bits.flushPipe.   foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.flushPipe.getOrElse(0.U.asTypeOf(io.out.bits.toRob.bits.flushPipe.get)))))
  io.out.bits.toRob.bits.replay.      foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.replay.getOrElse(0.U.asTypeOf(io.out.bits.toRob.bits.replay.get)))))
  io.out.bits.toRob.bits.isRVC.       foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.isRVC.getOrElse(false.B))))

  io.outToFalu.foreach { exuOutToFalu => {
    assert(funcUnits.filter(_.cfg.isFmul).size == 1, "ExeUnit should contain FMUL when ExeUnit.outToFalu is available")
    val fmulOutToFalu = funcUnits.filter(_.cfg.isFmul).head.io.outToFaluFromFmul.get
    exuOutToFalu.valid := fmulOutToFalu.valid

    // newExuInput ctrl bundle
    // useful signals
    connectSamePort(exuOutToFalu.bits.ctrl, fmulOutToFalu.bits.ctrl)
    exuOutToFalu.bits.ctrl.fuType := fmulOutToFalu.bits.fuType
    // useless signals, but may exist in some ExeUnits
    exuOutToFalu.bits.ctrl.rfWen.foreach(_ := false.B)
    exuOutToFalu.bits.ctrl.vecWen.foreach(_ := false.B)
    exuOutToFalu.bits.ctrl.v0Wen.foreach(_ := false.B)
    exuOutToFalu.bits.ctrl.dataSources.foreach(_ := 0.U.asTypeOf(DataSource()))
    exuOutToFalu.bits.ctrl.exuSources.foreach { exuSourceVec => exuSourceVec.foreach(_ := 0.U.asTypeOf(ExuSource(exuParams))) }
    exuOutToFalu.bits.ctrl.loadDependency.foreach(_ := RegEnable(io.in.bits.ctrl.loadDependency.get, io.in.fire))

    // newExuInput data bundle
    exuOutToFalu.bits.data.src.zip(fmulOutToFalu.bits.data.src).foreach { case (exuSrc, fmulSrc) => exuSrc := fmulSrc }
    // TODO
    exuOutToFalu.bits.data.src(2) := 0.U
    exuOutToFalu.bits.data.FaluInputFromFmul.foreach(exuFaluInput => exuFaluInput := fmulOutToFalu.bits.data.FmulToFaluDataInput.get)
    exuOutToFalu.bits.data.imm := fmulOutToFalu.bits.data.imm

    // other newExuInput signals
    // useful signals
    exuOutToFalu.bits.robIdx := fmulOutToFalu.bits.ctrl.robIdx
    exuOutToFalu.bits.toRF.pdest := fmulOutToFalu.bits.ctrl.pdest
    exuOutToFalu.bits.perfDebugInfo.foreach(_ := fmulOutToFalu.bits.perfDebugInfo.get)
    exuOutToFalu.bits.debug_seqNum.foreach(_ := fmulOutToFalu.bits.debug_seqNum.get)
    // useless signals
    exuOutToFalu.bits.iqIdx := 0.U
    exuOutToFalu.bits.isFirstIssue := false.B
    funcUnits.zip(exuParams.idxCopySrc).map { case (fu, idx) =>
      if (fu.cfg.isFalu) {
        exuOutToFalu.bits.data.src.zip(fmulOutToFalu.bits.data.src).foreach { case (faluSrc, fmulSrc) => faluSrc := fmulSrc }
      }
    }
  }}

  io.toFrontendBJUResolve.foreach{ case resolve => {
    val bjus = funcUnits.filter(x => x.cfg.isNewJmp || x.cfg.isBrh)
    val resolveVec = VecInit(bjus.map(_.io.toFrontendBJUResolve.get))
    resolve.valid := resolveVec.map(_.valid).reduce(_ || _)
    resolve.bits := Mux1H(resolveVec.map(_.valid), resolveVec.map(_.bits))
  }}
  io.csrio.foreach(exuio => funcUnits.foreach(fu => fu.io.csrio.foreach{
    fuio =>
      exuio <> fuio
      fuio.exception := DelayN(exuio.exception, 2)
      fuio.robDeqPtr := DelayN(exuio.robDeqPtr, 2)
  }))
  io.csrin.foreach(exuio => funcUnits.foreach(fu => fu.io.csrin.foreach{fuio => fuio := exuio}))
  io.csrToDecode.foreach(toDecode => funcUnits.foreach(fu => fu.io.csrToDecode.foreach(fuOut => toDecode := fuOut)))

  io.vtype.foreach(exuio => funcUnits.foreach(fu => fu.io.vtype.foreach(fuio => exuio := fuio)))
  io.fenceio.foreach(exuio => funcUnits.foreach(fu => fu.io.fenceio.foreach(fuio => fuio <> exuio)))
  io.frm.foreach(exuio => funcUnits.foreach(fu => fu.io.frm.foreach(fuio => fuio <> exuio)))
  io.vxrm.foreach(exuio => funcUnits.foreach(fu => fu.io.vxrm.foreach(fuio => fuio <> exuio)))
  io.vlIsZero.foreach(exuio => funcUnits.foreach(fu => fu.io.vlIsZero.foreach(fuio => exuio := fuio)))
  io.vlIsVlmax.foreach(exuio => funcUnits.foreach(fu => fu.io.vlIsVlmax.foreach(fuio => exuio := fuio)))
  // RegNext for better timing and it should be fine
  io.instrAddrTransType.foreach(exuio => funcUnits.foreach(fu => fu.io.instrAddrTransType.foreach(fuio => fuio := RegNext(exuio))))

  // debug info
  io.out.bits.debug     := 0.U.asTypeOf(io.out.bits.debug)
  io.out.bits.debug.isPerfCnt := funcUnits.map(_.io.csrio.map(_.isPerfCnt)).map(_.getOrElse(false.B)).reduce(_ || _)
  io.out.bits.perfDebugInfo.foreach(_ := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.perfDebugInfo.getOrElse(0.U.asTypeOf(new PerfDebugInfo)))))
  io.out.bits.debug_seqNum.foreach(_ := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.debug_seqNum.getOrElse(0.U.asTypeOf(InstSeqNum())))))
}

class DispatcherIO[T <: Data](private val gen: T, n: Int) extends Bundle {
  val in = Flipped(DecoupledIO(gen))

  val out = Vec(n, DecoupledIO(gen))
}

class Dispatcher[T <: Data](private val gen: T, n: Int, acceptCond: T => Seq[Bool])
  (implicit p: Parameters)
  extends Module {

  val io = IO(new DispatcherIO(gen, n))

  private val acceptVec: Vec[Bool] = VecInit(acceptCond(io.in.bits))

  XSError(io.in.valid && PopCount(acceptVec) > 1.U, p"[ExeUnit] accept vec should no more than 1, ${Binary(acceptVec.asUInt)} ")
  XSError(io.in.valid && PopCount(acceptVec) === 0.U, "[ExeUnit] there is a inst not dispatched to any fu")

  io.out.zipWithIndex.foreach { case (out, i) =>
    out.valid := acceptVec(i) && io.in.valid
    out.bits := io.in.bits
  }

  io.in.ready := Cat(io.out.map(_.ready)).andR
}
