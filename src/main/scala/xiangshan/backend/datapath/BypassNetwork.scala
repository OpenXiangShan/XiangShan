package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{GatedValidRegNext, SignExt, ZeroExt}
import utils.SeqUtils._
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.{ExuBypassBundle, ExuInput, ExuVec, ImmInfo}
import xiangshan.backend.issue._
import xiangshan.backend.datapath.DataConfig.RegDataMaxWidth
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.regcache._
import xiangshan.backend.Bundles._
import xiangshan.backend.decode.opcode.Opcode.VIAluOpcodes
import xiangshan.backend.fu._
import xiangshan.backend.fu.vector.Utils.{SplitMask, VecDataToMaskDataVec}
import yunsuan.VialuFixType
import xiangshan.backend.vector.datapath.VecImmExtractor

class BypassNetworkIO()(implicit p: Parameters, params: BackendParams) extends XSBundle {
  // params
  private val intSchdParams = params.schdParams(IntScheduler())
  private val fpSchdParams = params.schdParams(FpScheduler())
  private val vecSchdParams = params.schdParams(VecScheduler())

  val fromDataPath = new FromDataPath
  val toExus = new ToExus
  val fromExus = new FromExus

  class FromDataPath extends Bundle {
    val int: MixedVec[MixedVec[DecoupledIO[Og1InUop]]] = Flipped(intSchdParams.genOg1InUopBundle)
    val fp : MixedVec[MixedVec[DecoupledIO[Og1InUop]]] = Flipped(fpSchdParams.genOg1InUopBundle)
    val vf : MixedVec[MixedVec[DecoupledIO[Og1InUop]]] = Flipped(vecSchdParams.genOg1InUopBundle)
    val rcData: MixedVec[MixedVec[Vec[UInt]]] = MixedVec(
      Seq(intSchdParams, fpSchdParams, vecSchdParams).map(schd => schd.issueBlockParams.map(iq =>
        MixedVec(iq.exuBlockParams.map(exu => Input(Vec(exu.numRegSrc, UInt(exu.srcDataBitsMax.W)))))
      )).flatten
    )
  }

  class ToExus extends Bundle {
    val int: MixedVec[MixedVec[DecoupledIO[NewExuInput]]] = intSchdParams.genNewExuInputCopySrcBundle
    val fp : MixedVec[MixedVec[DecoupledIO[NewExuInput]]] = fpSchdParams.genNewExuInputCopySrcBundle
    val vf : MixedVec[MixedVec[DecoupledIO[NewExuInput]]] = vecSchdParams.genNewExuInputCopySrcBundle
  }

  class FromExus extends Bundle {
    val int: MixedVec[MixedVec[ValidIO[ExuBypassBundle]]] = Flipped(intSchdParams.genExuBypassValidBundle)
    val fp : MixedVec[MixedVec[ValidIO[ExuBypassBundle]]] = Flipped(fpSchdParams.genExuBypassValidBundle)
    val vf : MixedVec[MixedVec[ValidIO[ExuBypassBundle]]] = Flipped(vecSchdParams.genExuBypassValidBundle)
    val fpFromFmulToFalu : MixedVec[MixedVec[ValidIO[NewExuInput]]] = Flipped(fpSchdParams.genExuOutToFaluBundleNoMemBlock)

    def connectExuOutput(
      getSinkVecN: FromExus => MixedVec[MixedVec[ValidIO[ExuBypassBundle]]]
    )(
      sourceVecN: MixedVec[MixedVec[DecoupledIO[NewExuOutput]]]
    ): Unit = {
      getSinkVecN(this).zip(sourceVecN).foreach { case (sinkVec, sourcesVec) =>
        sinkVec.zip(sourcesVec).foreach { case (sink, source) =>
          sink.valid := source.valid || source.bits.params.needDataFromF2I.B && source.bits.toIntRf.map(_.valid).getOrElse(false.B)
          sink.bits.intWen := source.bits.toIntRf.map(_.valid).getOrElse(false.B) && source.bits.isFromLoadUnit.getOrElse(true.B)
          sink.bits.pdest := source.bits.pdest
          // int i2f wakeup fstore from fpRegion, so there is not need bypass fp data in int region
          sink.bits.data := {if (source.bits.params.isIntExeUnit) { source.bits.toIntRf.map(_.bits).getOrElse(0.U(source.bits.params.destDataBitsMax.W)) }
                        else if (source.bits.params.isFpExeUnit)  { source.bits.toFpRf.map(_.bits).getOrElse(0.U(source.bits.params.destDataBitsMax.W))  }
                        else if (source.bits.params.isVfExeUnit && source.bits.params.writeVecRf) { source.bits.toVecRf.map(_.bits).getOrElse(0.U(source.bits.params.destDataBitsMax.W)) }
                        else if (source.bits.params.isVfExeUnit && source.bits.params.writeV0Rf)  { source.bits.toV0Rf.map(_.bits).getOrElse(0.U(source.bits.params.destDataBitsMax.W))  }
                        else if (source.bits.params.isVfExeUnit && source.bits.params.writeVlRf)  { source.bits.toVlRf.map(_.bits).getOrElse(0.U(source.bits.params.destDataBitsMax.W))  }
                        else { source.bits.toIntRf.map(_.bits).getOrElse(0.U(source.bits.params.destDataBitsMax.W)) }}
        }
      }
    }
  }

  val toDataPath: Vec[RCWritePort] = Vec(params.getIntExuRCWriteSize + params.getMemExuRCWriteSize,
    Flipped(new RCWritePort(params.intSchdParams.get.rfDataWidth, RegCacheIdxWidth, params.intSchdParams.get.pregIdxWidth, params.debugEn)))
}

class BypassNetwork()(implicit p: Parameters, params: BackendParams) extends XSModule {
  val io: BypassNetworkIO = IO(new BypassNetworkIO)

  private val fromDPs: Seq[DecoupledIO[Og1InUop]] = (io.fromDataPath.int ++ io.fromDataPath.fp ++ io.fromDataPath.vf).flatten.toSeq
  private val fromExus: Seq[ValidIO[ExuBypassBundle]] = (io.fromExus.int ++ io.fromExus.fp ++ io.fromExus.vf).flatten.toSeq
  private val toExusInt: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Wire(params.intSchdParams.get.genExuInputCopySrcBundle)
  private val toExusFp: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Wire(params.fpSchdParams.get.genExuInputCopySrcBundle)
  private val toExusVf: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Wire(params.vecSchdParams.get.genExuInputCopySrcBundle)
  private val toExus: Seq[DecoupledIO[ExuInput]] = (toExusInt ++ toExusFp ++ toExusVf).flatten.toSeq
  private val toNewExusIntRelay: MixedVec[MixedVec[DecoupledIO[NewExuInput]]] = Wire(params.intSchdParams.get.genNewExuInputCopySrcBundle)
  private val toNewExusFpRelay: MixedVec[MixedVec[DecoupledIO[NewExuInput]]] = Wire(params.fpSchdParams.get.genNewExuInputCopySrcBundle)
  private val toNewExusVfRelay: MixedVec[MixedVec[DecoupledIO[NewExuInput]]] = Wire(params.vecSchdParams.get.genNewExuInputCopySrcBundle)
  private val toNewExusRelay: Seq[DecoupledIO[NewExuInput]] = (toNewExusIntRelay ++ toNewExusFpRelay ++ toNewExusVfRelay).flatten.toSeq
  private val fromDPsRCData: Seq[Vec[UInt]] = io.fromDataPath.rcData.flatten.toSeq

  println(s"[BypassNetwork] RCData num: ${fromDPsRCData.size}")

  // (exuIdx, srcIdx, bypassExuIdx)
  private val forwardOrBypassValidVec3: MixedVec[Vec[Vec[Bool]]] = MixedVecInit(
    fromDPs.map { (x: DecoupledIO[Og1InUop]) =>
      println(s"[BypassNetwork] ${x.bits.exuParams.name} numRegSrc: ${x.bits.exuParams.numRegSrc}")
      VecInit(x.bits.exuSources.map(_.map(_.toExuOH(x.bits.exuParams))).getOrElse(
        // TODO: remove tmp max 1 for fake HYU1
        VecInit(Seq.fill(x.bits.exuParams.numRegSrc max 1)(VecInit(0.U(params.numExu.W).asBools)))
      ))
    }
  )

  private val forwardDataVec: Vec[UInt] = VecInit(
    fromExus.map(x => ZeroExt(x.bits.data, RegDataMaxWidth))
  )

  private val bypassDataVec = VecInit(
    // remove fp exu which need i2f data's clock gate
    // remove int exu which need f2i data's clock gate
    fromExus.map(x => {
      if (x.bits.params.needDataFromI2F || x.bits.params.needDataFromF2I) {
        // because RegNext unset width, canot ZeroExt
        val tempWire = Wire(UInt(RegDataMaxWidth.W))
        tempWire := RegNext(x.bits.data)
        ZeroExt(tempWire, RegDataMaxWidth)
      }
      else ZeroExt(RegEnable(x.bits.data, x.valid), RegDataMaxWidth)
    })
  )

  private val intExuNum = params.intSchdParams.get.numExu
  private val fpExuNum  = params.fpSchdParams.get.numExu
  private val vfExuNum  = params.vecSchdParams.get.numExu

  println(s"[BypassNetwork] allExuNum: ${toExus.size} intExuNum: ${intExuNum} fpExuNum: ${fpExuNum} vfExuNum: ${vfExuNum}")

  private val fromDPsHasBypass2Source = fromDPs.filter(x => x.bits.exuParams.isIQWakeUpSource && x.bits.exuParams.writeVfRf && (x.bits.exuParams.isVfExeUnit || x.bits.exuParams.hasLoadExu)).map(_.bits.exuParams.exuIdx)
  private val fromDPsHasBypass2Sink   = fromDPs.filter(x => x.bits.exuParams.isIQWakeUpSink && x.bits.exuParams.readVfRf && (x.bits.exuParams.isVfExeUnit || x.bits.exuParams.isMemExeUnit)).map(_.bits.exuParams.exuIdx)

  private val bypass2ValidVec3 = MixedVecInit(
    fromDPsHasBypass2Sink.map(forwardOrBypassValidVec3(_)).map(exu => VecInit(exu.map(exuOH =>
      VecInit(fromDPsHasBypass2Source.map(exuOH(_))).asUInt
    )))
  )
  if(params.debugEn){
    dontTouch(bypass2ValidVec3)
  }
  private val bypass2DateEn = VecInit(
    fromExus.map(x => RegNext(x.valid))
  ).asUInt
  private val bypass2DataVec = if (fromDPsHasBypass2Source.length == 0) VecInit(Seq(0.U)) else VecInit(
    fromDPsHasBypass2Source.map(x => RegNext(bypassDataVec(x)))
  )

  println(s"[BypassNetwork] HasBypass2SourceExuNum: ${fromDPsHasBypass2Source.size} HasBypass2SinkExuNum: ${fromDPsHasBypass2Sink.size} bypass2DataVecSize: ${bypass2DataVec.length}")
  println(s"[BypassNetwork] HasBypass2SourceExu: ${fromDPsHasBypass2Source}")
  println(s"[BypassNetwork] HasBypass2SinkExu: ${fromDPsHasBypass2Sink}")

  toExus.zip(fromDPs).foreach { case (sink, source: DecoupledIO[Og1InUop]) =>
    connectSamePort(sink.bits, source.bits)
    sink.bits.FaluInputFromFmul.foreach(_ := 0.U.asTypeOf(new FuncUnitFaluInputFromFmul))
    sink.bits.imm := source.bits.imm.getOrElse(0.U)
    sink.bits.selImm := source.bits.selImm.getOrElse(0.U)
    // dirty code, is0Lat only be used in wakeupQueue
    sink.bits.is0Lat.foreach(_ := 0.U)
    sink.bits.predictInfo.foreach{ case x =>
      x.target := source.bits.predTarget.get
      x.fixedTaken := source.bits.fixedTaken.get
      x.predTaken := source.bits.predTaken.get
    }
    sink.bits.frm.foreach(_ := source.bits.frm.get)
    sink.valid := source.valid
    source.ready := sink.ready
  }

  toExus.zipWithIndex.foreach { case (exuInput, exuIdx) =>
    val imm = ImmExtractor(
      fromDPs(exuIdx).bits.imm.getOrElse(0.U),
      fromDPs(exuIdx).bits.selImm.getOrElse(0.U),
      exuInput.bits.params.destDataBitsMax,
      exuInput.bits.params.immType,
    )

    val vecImm = Option.when(
      exuInput.bits.params.hasVecFu
    )(
      VecImmExtractor(
        VLEN,
        exuInput.bits.params.immType,
      )(
        fromDPs(exuIdx).bits.imm.getOrElse(0.U),
        fromDPs(exuIdx).bits.selImm.getOrElse(0.U),
        exuInput.bits.vtype.get.vsew
      )
    )

    val exuParm = exuInput.bits.params
    val immLoadSrc0 = Option.when(exuParm.hasLoadFu)(SignExt(ImmUnion.U.toImm32(fromDPs(exuIdx).bits.imm.get(31, ImmUnion.I.len)), XLEN))
    val isIntScheduler = exuParm.isIntExeUnit
    val isReadVfRf = exuParm.readVfRf
    val fuOpType = exuInput.bits.fuOpType
    val fuType = exuInput.bits.fuType
    val isAlu = FuType.isAlu(fuType)

    exuInput.bits.src.zipWithIndex.foreach { case (src, srcIdx) =>
      val dataSource = exuInput.bits.dataSources(srcIdx)
      val isWakeUpSink = params.allIssueParams.filter(_.exuBlockParams.contains(exuParm)).head.exuBlockParams.map(_.isIQWakeUpSink).reduce(_ || _)
      val readForward = if (isWakeUpSink) dataSource.readForward else false.B
      val readBypass = if (isWakeUpSink) dataSource.readBypass else false.B
      val readZero = if (isIntScheduler) dataSource.readZero else false.B
      val readV0 = if (srcIdx < 3 && isReadVfRf) dataSource.readV0 else false.B
      val readRegOH = exuInput.bits.dataSources(srcIdx).readRegOH
      val readRegCache = if (exuParm.needReadRegCache) exuInput.bits.dataSources(srcIdx).readRegCache else false.B
      val readImm = {
        if (exuParm.issueBlockParam.inVfSchd)
          exuInput.bits.dataSources(srcIdx).readImm
        else if (exuParm.immType.nonEmpty && srcIdx == 1 || exuParm.hasLoadExu && srcIdx == 0)
          exuInput.bits.dataSources(srcIdx).readImm
        else {
          false.B
        }
      }
      val bypass2ExuIdx = fromDPsHasBypass2Sink.indexOf(exuIdx)
      println(s"${exuParm.name}: bypass2ExuIdx is ${bypass2ExuIdx}")
      val readBypass2 = if (bypass2ExuIdx >= 0) dataSource.readBypass2 else false.B
      println(s"[BypassNetWork] ${exuParm.name}")
      println(s"[BypassNetWork] exuIdx = ${exuIdx}")
      println(s"[BypassNetWork] srcIdx = ${srcIdx}")
      val immALU = Wire(UInt(XLEN.W))
      immALU := imm
      if (exuParm.aluNeedPc && srcIdx == 1) {
        val isJmp = ALUOpType.isJmp(fuOpType)
        when(isAlu && isJmp) {
          // jalr's fuOpType(1) == 0
          val isAuipc = JumpOpType.jumpUopisAuipc(fuOpType)
          val thisPcOffset = exuInput.bits.getPcOffset()
          val nextPcOffset = exuInput.bits.getNextPcOffset()
          immALU := Mux(isJmp, Mux(isAuipc, imm + SignExt(thisPcOffset, imm.getWidth), ZeroExt(nextPcOffset, imm.getWidth)), imm)
        }
      }
      val originSrc = Mux1H(
        Seq(
          readForward    -> Mux1H(forwardOrBypassValidVec3(exuIdx)(srcIdx), forwardDataVec),
          readBypass     -> Mux1H(forwardOrBypassValidVec3(exuIdx)(srcIdx), bypassDataVec),
          readBypass2    -> (if (bypass2ExuIdx >= 0) Mux1H(bypass2ValidVec3(bypass2ExuIdx)(srcIdx), bypass2DataVec) else 0.U),
          readZero       -> 0.U,
          readRegOH      -> fromDPs(exuIdx).bits.src(srcIdx),
          readRegCache   -> fromDPsRCData(exuIdx)(srcIdx),
          readImm        -> (if (exuParm.hasLoadExu && srcIdx == 0) immLoadSrc0.get else if (exuParm.aluNeedPc) immALU else if (vecImm.nonEmpty) vecImm.get else imm)
        )
      )
      src := originSrc
    }
    exuInput.bits.vl.foreach { _ := fromDPs(exuIdx).bits.vl.get }
    exuInput.bits.v0.foreach { _ := fromDPs(exuIdx).bits.v0.get }

    if (exuParm.hasBrhFu || exuParm.hasCSR || exuParm.hasFence) {
      val thisPcOffset = exuInput.bits.getPcOffset()
      val nextPcOffset = exuInput.bits.getNextPcOffset()
      val isJALR = FuType.isJump(fuType) && JumpOpType.jumpUopisJalr(fuOpType)
      val isJR = FuType.isNewJump(fuType) && NewJumpOpType.jumpUopisjr(fuOpType)
      val immBJU = imm + Mux(isJALR || isJR, 0.U, SignExt(thisPcOffset, imm.getWidth))
      val immCsrFence = fromDPs(exuIdx).bits.imm.get
      exuInput.bits.imm := Mux((FuType.isCsr(fuType) || FuType.isFence(fuType)) && exuParm.hasCSR.B, immCsrFence, immBJU)
      exuInput.bits.nextPcOffset.foreach(_ := nextPcOffset)
      dontTouch(isJALR)
      dontTouch(isJR)
      dontTouch(immBJU)
      dontTouch(immCsrFence)
    }
    exuInput.bits.copySrc.foreach(_.map(copysrc =>
      copysrc.zip(exuInput.bits.src).foreach{ case(copy, src) => copy := src}
    ))
  }

  // to reg cache
  private val forwardIntWenVec = VecInit(
    fromExus.filter(_.bits.params.needWriteRegCache).map(x => x.valid && x.bits.intWen)
  )
  private val forwardTagVec = VecInit(
    fromExus.filter(_.bits.params.needWriteRegCache).map(x => x.bits.pdest)
  )

  private val bypassIntWenVec = VecInit(
    forwardIntWenVec.map(x => GatedValidRegNext(x))
  )
  private val bypassTagVec = VecInit(
    forwardTagVec.zip(forwardIntWenVec).map(x => RegEnable(x._1, x._2))
  )
  private val bypassRCDataVec = VecInit(
    fromExus.zip(bypassDataVec).filter(_._1.bits.params.needWriteRegCache).map(_._2)
  )

  println(s"[BypassNetwork] WriteRegCacheExuNum: ${forwardIntWenVec.size}")

  io.toDataPath.zipWithIndex.foreach{ case (x, i) =>
    x.wen := bypassIntWenVec(i)
    x.addr := DontCare
    x.data := bypassRCDataVec(i)
    x.tag.foreach(_ := bypassTagVec(i))
  }

  toNewExusRelay.zip(toExus).foreach { case (sink, source) => connectNewExuInput(sink, source) }
  io.toExus.int.flatten.zip(toNewExusIntRelay.flatten).foreach { case (intExuOut, intExuRelay) =>
    intExuOut.valid := intExuRelay.valid
    intExuRelay.ready := intExuOut.ready
    intExuOut.bits := intExuRelay.bits
  }

  io.toExus.fp.flatten.filter(_.bits.params.hasFmulFu).lazyZip(toNewExusFpRelay.flatten.filter(_.bits.params.hasFmulFu)).lazyZip(io.fromExus.fpFromFmulToFalu.flatten).foreach { case (fpExuOut, fpExuRelay, fmulToFaluBypassInput) =>
    fpExuOut.valid := fpExuRelay.valid || fmulToFaluBypassInput.valid
    fpExuRelay.ready := fpExuOut.ready
    fpExuOut.bits := Mux(fmulToFaluBypassInput.valid, fmulToFaluBypassInput.bits, fpExuRelay.bits)
  }

  io.toExus.fp.flatten.filterNot(_.bits.params.hasFmulFu).zip(toNewExusFpRelay.flatten.filterNot(_.bits.params.hasFmulFu)).foreach { case (fpExuOut, fpExuRelay) =>
    fpExuOut.valid := fpExuRelay.valid
    fpExuRelay.ready := fpExuOut.ready
    fpExuOut.bits := fpExuRelay.bits
  }

  io.toExus.vf.flatten.zip(toNewExusVfRelay.flatten).foreach { case (vfExuOut, vfExuRelay) =>
    vfExuOut.valid := vfExuRelay.valid
    vfExuRelay.ready := vfExuOut.ready
    vfExuOut.bits := vfExuRelay.bits
  }
}
