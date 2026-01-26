package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{GatedValidRegNext, SignExt, ZeroExt}
import utils.SeqUtils._
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.{ExuBypassBundle, ExuInput, ExuOutput, ExuVec, ImmInfo}
import xiangshan.backend.issue._
import xiangshan.backend.datapath.DataConfig.RegDataMaxWidth
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.regcache._
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.fu.vector.Utils.{SplitMask, VecDataToMaskDataVec}
import yunsuan.VialuFixType

class BypassNetworkIO()(implicit p: Parameters, params: BackendParams) extends XSBundle {
  // params
  private val intSchdParams = params.schdParams(IntScheduler())
  private val fpSchdParams = params.schdParams(FpScheduler())
  private val vecSchdParams = params.schdParams(VecScheduler())

  val fromDataPath = new FromDataPath
  val toExus = new ToExus
  val fromExus = new FromExus

  class FromDataPath extends Bundle {
    val int: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Flipped(intSchdParams.genExuInputBundle)
    val fp : MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Flipped(fpSchdParams.genExuInputBundle)
    val vf : MixedVec[MixedVec[DecoupledIO[ExuInput]]] = Flipped(vecSchdParams.genExuInputBundle)
    val immInfo: Vec[ImmInfo] = Input(Vec(params.allExuParams.size, new ImmInfo))
    val rcData: MixedVec[MixedVec[Vec[UInt]]] = MixedVec(
      Seq(intSchdParams, fpSchdParams, vecSchdParams).map(schd => schd.issueBlockParams.map(iq =>
        MixedVec(iq.exuBlockParams.map(exu => Input(Vec(exu.numRegSrc, UInt(exu.srcDataBitsMax.W)))))
      )).flatten
    )
  }

  class ToExus extends Bundle {
    val int: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = intSchdParams.genExuInputCopySrcBundle
    val fp : MixedVec[MixedVec[DecoupledIO[ExuInput]]] = fpSchdParams.genExuInputCopySrcBundle
    val vf : MixedVec[MixedVec[DecoupledIO[ExuInput]]] = vecSchdParams.genExuInputCopySrcBundle
  }

  class FromExus extends Bundle {
    val int: MixedVec[MixedVec[ValidIO[ExuBypassBundle]]] = Flipped(intSchdParams.genExuBypassValidBundle)
    val fp : MixedVec[MixedVec[ValidIO[ExuBypassBundle]]] = Flipped(fpSchdParams.genExuBypassValidBundle)
    val vf : MixedVec[MixedVec[ValidIO[ExuBypassBundle]]] = Flipped(vecSchdParams.genExuBypassValidBundle)

    def connectExuOutput(
      getSinkVecN: FromExus => MixedVec[MixedVec[ValidIO[ExuBypassBundle]]]
    )(
      sourceVecN: MixedVec[MixedVec[DecoupledIO[ExuOutput]]]
    ): Unit = {
      getSinkVecN(this).zip(sourceVecN).foreach { case (sinkVec, sourcesVec) =>
        sinkVec.zip(sourcesVec).foreach { case (sink, source) =>
          sink.valid := source.valid || source.bits.params.needDataFromF2I.B && source.bits.intWen.getOrElse(false.B)
          sink.bits.intWen := source.bits.intWen.getOrElse(false.B) && source.bits.isFromLoadUnit.getOrElse(true.B)
          sink.bits.pdest := source.bits.pdest
          // int i2f wakeup fstore from fpRegion, so there is not need bypass fp data in int region
          sink.bits.data := source.bits.data(source.bits.params.getForwardIndex())
        }
      }
    }
  }

  val toDataPath: Vec[RCWritePort] = Vec(params.getIntExuRCWriteSize + params.getMemExuRCWriteSize, 
    Flipped(new RCWritePort(params.intSchdParams.get.rfDataWidth, RegCacheIdxWidth, params.intSchdParams.get.pregIdxWidth, params.debugEn)))
}

class BypassNetwork()(implicit p: Parameters, params: BackendParams) extends XSModule {
  val io: BypassNetworkIO = IO(new BypassNetworkIO)

  private val fromDPs: Seq[DecoupledIO[ExuInput]] = (io.fromDataPath.int ++ io.fromDataPath.fp ++ io.fromDataPath.vf).flatten.toSeq
  private val fromExus: Seq[ValidIO[ExuBypassBundle]] = (io.fromExus.int ++ io.fromExus.fp ++ io.fromExus.vf).flatten.toSeq
  private val toExus: Seq[DecoupledIO[ExuInput]] = (io.toExus.int ++ io.toExus.fp ++ io.toExus.vf).flatten.toSeq
  private val fromDPsRCData: Seq[Vec[UInt]] = io.fromDataPath.rcData.flatten.toSeq
  private val immInfo = io.fromDataPath.immInfo

  println(s"[BypassNetwork] RCData num: ${fromDPsRCData.size}")

  // (exuIdx, srcIdx, bypassExuIdx)
  private val forwardOrBypassValidVec3: MixedVec[Vec[Vec[Bool]]] = MixedVecInit(
    fromDPs.map { (x: DecoupledIO[ExuInput]) =>
      println(s"[BypassNetwork] ${x.bits.params.name} numRegSrc: ${x.bits.params.numRegSrc}")
      VecInit(x.bits.exuSources.map(_.map(_.toExuOH(x.bits.params))).getOrElse(
        // TODO: remove tmp max 1 for fake HYU1
        VecInit(Seq.fill(x.bits.params.numRegSrc max 1)(VecInit(0.U(params.numExu.W).asBools)))
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

  private val fromDPsHasBypass2Source = fromDPs.filter(x => x.bits.params.isIQWakeUpSource && x.bits.params.writeVfRf && (x.bits.params.isVfExeUnit || x.bits.params.hasLoadExu)).map(_.bits.params.exuIdx)
  private val fromDPsHasBypass2Sink   = fromDPs.filter(x => x.bits.params.isIQWakeUpSink && x.bits.params.readVfRf && (x.bits.params.isVfExeUnit || x.bits.params.isMemExeUnit)).map(_.bits.params.exuIdx)

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

  toExus.zip(fromDPs).foreach { case (sink, source) =>
    connectSamePort(sink.bits, source.bits)
    sink.valid := source.valid
    source.ready := sink.ready
  }

  toExus.zipWithIndex.foreach { case (exuInput, exuIdx) =>
    val imm = ImmExtractor(
      immInfo(exuIdx).imm,
      immInfo(exuIdx).immType,
      exuInput.bits.params.destDataBitsMax,
      exuInput.bits.params.immType,
    )
    val immLoadSrc0 = SignExt(ImmUnion.U.toImm32(immInfo(exuIdx).imm(immInfo(exuIdx).imm.getWidth - 1, ImmUnion.I.len)), XLEN)
    val exuParm = exuInput.bits.params
    val isIntScheduler = exuParm.isIntExeUnit
    val isReadVfRf = exuParm.readVfRf
    val fuOpType = exuInput.bits.fuOpType
    val fuType = exuInput.bits.fuType
    val isAlu = FuType.isAlu(fuType)
    val isViAlu = FuType.isVIAluF(fuType)

    exuInput.bits.src.zipWithIndex.foreach { case (src, srcIdx) =>
      val dataSource = exuInput.bits.dataSources(srcIdx)
      val isWakeUpSink = params.allIssueParams.filter(_.exuBlockParams.contains(exuParm)).head.exuBlockParams.map(_.isIQWakeUpSink).reduce(_ || _)
      val readForward = if (isWakeUpSink) dataSource.readForward else false.B
      val readBypass = if (isWakeUpSink) dataSource.readBypass else false.B
      val readZero = if (isIntScheduler) dataSource.readZero else false.B
      val readV0 = if (srcIdx < 3 && isReadVfRf) dataSource.readV0 else false.B
      val readRegOH = exuInput.bits.dataSources(srcIdx).readRegOH
      val readRegCache = if (exuParm.needReadRegCache) exuInput.bits.dataSources(srcIdx).readRegCache else false.B
      val readImm = if (exuParm.immType.nonEmpty && srcIdx == 1 || exuParm.hasLoadExu && srcIdx == 0) exuInput.bits.dataSources(srcIdx).readImm else false.B
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
          val isAuipc = fuOpType(1)
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
          readV0         -> (if (srcIdx < 3 && isReadVfRf) exuInput.bits.src(3) else 0.U),
          readRegOH      -> fromDPs(exuIdx).bits.src(srcIdx),
          readRegCache   -> fromDPsRCData(exuIdx)(srcIdx),
          readImm        -> (if (exuParm.hasLoadExu && srcIdx == 0) immLoadSrc0 else if (exuParm.aluNeedPc) immALU else imm)
        )
      )
      src := originSrc
    }
    exuInput.bits.vl.foreach { _ := fromDPs(exuIdx).bits.vl.get }

    if (exuParm.hasBrhFu || exuParm.hasCSR || exuParm.hasFence) {
      val thisPcOffset = exuInput.bits.getPcOffset()
      val nextPcOffset = exuInput.bits.getNextPcOffset()
      val isJALR = FuType.isJump(fuType) && JumpOpType.jumpOpisJalr(fuOpType)
      val immBJU = imm + Mux(isJALR, 0.U, SignExt(thisPcOffset, imm.getWidth))
      val immCsrFence = immInfo(exuIdx).imm
      exuInput.bits.imm := Mux((FuType.isCsr(fuType) || FuType.isFence(fuType))&& exuParm.hasCSR.B, immCsrFence, immBJU)
      exuInput.bits.nextPcOffset.foreach(_ := nextPcOffset)
      dontTouch(isJALR)
      dontTouch(immBJU)
      dontTouch(immCsrFence)
    }
    exuInput.bits.copySrc.get.map( copysrc =>
      copysrc.zip(exuInput.bits.src).foreach{ case(copy, src) => copy := src}
    )

    if (exuParm.needVPUCtrl) {
      val allMaskTrue = VecInit(Seq.fill(VLEN)(true.B)).asUInt
      val allMaskFalse = VecInit(Seq.fill(VLEN)(false.B)).asUInt
      val srcMask = Wire(UInt(VLEN.W))
      val vpu = exuInput.bits.vpu.get
      val vsew = vpu.vsew
      val vuopIdx = vpu.vuopIdx
      srcMask := vpu.vmask
      if (exuParm.hasMaskWakeUp) {
        val maskWakeUpFus = exuParm.fuConfigs.distinct.filter(_.maskWakeUp).map(x => x.fuType.U)
        val maskWakeUpFu = maskWakeUpFus.map(_ === fuType).reduce(_ || _)
        when (maskWakeUpFu) {
          val maskIn = exuInput.bits.src(3)
          val vm = vpu.vm
          val needClearMask = isViAlu & VialuFixType.needClearMask(fuOpType)
          srcMask := MuxCase(maskIn, Seq(
            needClearMask -> allMaskFalse,
            vm -> allMaskTrue,
          ))
        }
      }
      val maskDataVec = VecDataToMaskDataVec(srcMask, vsew)
      val maskVec = Wire(UInt((VLEN / 8).W))
      maskVec := SplitMask(maskDataVec(vuopIdx), vsew).asUInt

      val sew8  = !vsew(1) & !vsew(0)
      val sew16 = !vsew(1) &  vsew(0)
      val sew32 =  vsew(1) & !vsew(0)
      val sew64 =  vsew(1) &  vsew(0)

      vpu.maskVecGen := maskVec
      vpu.sew8  := sew8
      vpu.sew16 := sew16
      vpu.sew32 := sew32
      vpu.sew64 := sew64
    }

    if (exuParm.hasVIAluFu) {
      when (isViAlu) {
        val isExt = exuInput.bits.vpu.get.isExt
        val vialuCtrl = exuInput.bits.vialuCtrl.get
        val widenVs2 = VialuFixType.fmtIsVVW(fuOpType) & VialuFixType.isAddSub(fuOpType)
        val widen = (VialuFixType.fmtIsWVW(fuOpType) | VialuFixType.fmtIsVVW(fuOpType)) & VialuFixType.isAddSub(fuOpType)
        val isVf2 = VialuFixType.fmtIsVF2(fuOpType) & isExt
        val isVf4 = VialuFixType.fmtIsVF4(fuOpType) & isExt
        val isVf8 = VialuFixType.fmtIsVF8(fuOpType) & isExt
        val isAddCarry = VialuFixType.isAddCarry(fuOpType)

        vialuCtrl.widenVs2 := widenVs2
        vialuCtrl.widen := widen
        vialuCtrl.isVf2 := isVf2
        vialuCtrl.isVf4 := isVf4
        vialuCtrl.isVf8 := isVf8
        vialuCtrl.isAddCarry := isAddCarry
      }
    }
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
}
