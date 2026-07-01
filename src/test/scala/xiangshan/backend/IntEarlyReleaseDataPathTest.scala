package xiangshan.backend

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import chisel3.util.{Valid, ValidIO}
import circt.stage.ChiselStage
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import top.DefaultConfig
import xiangshan._
import xiangshan.backend.datapath.{DataPathIO, DataPathIntEROps, DataSource, WbArbiterIntEROps}
import xiangshan.backend.datapath.WbConfig.IntWB
import xiangshan.backend.issue.{IntScheduler, SchdBlockParams}

class IntERDataPathIOShapeProbe(expectedER: Boolean)(implicit p: Parameters) extends XSModule {
  private implicit val dataPathBackendParams: BackendParams = p(XSCoreParamsKey).backendParams
  for ((exu, idx) <- dataPathBackendParams.allExuParams.zipWithIndex) {
    exu.bindBackendParam(dataPathBackendParams)
    exu.updateIQWakeUpConfigs(dataPathBackendParams.iqWakeUpParams)
    exu.updateExuIdx(idx)
  }
  dataPathBackendParams.allSchdParams.foreach(_.bindBackendParam(dataPathBackendParams))
  dataPathBackendParams.allIssueParams.foreach { issue =>
    issue.bindBackendParam(dataPathBackendParams)
    issue.allExuParams.foreach(_.bindBackendParam(dataPathBackendParams))
    issue.exuBlockParams.foreach(_.bindIssueBlockParam(issue))
  }
  private implicit val dataPathSchdParams: SchdBlockParams = dataPathBackendParams.schdParams(IntScheduler())

  val bundle = Wire(new DataPathIO()(p, dataPathBackendParams, dataPathSchdParams))
  bundle := DontCare

  val readDone = bundle.elements.get("intERReadDone")
  require(readDone.isDefined == expectedER, "DataPath read observation IO presence must follow Int ER enable")
  readDone.foreach { value =>
    require(
      value.asInstanceOf[Vec[ValidIO[IntERSrcValueReadDone]]].length == IntERReadDoneWidth,
      "DataPath read observation IO width must follow backend issue dequeue topology"
    )
  }
}

class IntERDataPathReadDoneProbe(localSrc: Int, replayProne: Boolean)(implicit p: Parameters) extends XSModule {
  require(localSrc > 0, "read observation probe needs at least one source")

  val io = IO(new Bundle {
    val s1Valid = Input(Bool())
    val og1Failed = Input(Bool())
    val uncertainReadPath = Input(Bool())
    val isWakeupSink = Input(Bool())
    val robIdx = Input(new rob.RobPtr)
    val srcValid = Input(Vec(localSrc, Bool()))
    val srcTrackId = Input(Vec(localSrc, UInt(IntERTrackIdWidth.W)))
    val srcTrackGen = Input(Vec(localSrc, UInt(IntERTrackGenBits.W)))
    val srcIdx = Input(Vec(localSrc, UInt(IntERSrcIdxWidth.W)))
    val psrc = Input(Vec(localSrc, UInt(PhyRegIdxWidth.W)))
    val dataSources = Input(Vec(localSrc, DataSource()))
    val intReadSrc = Input(Vec(localSrc, Bool()))
    val out = Output(Valid(new IntERSrcValueReadDone))
    val status = Output(new IntERDataPathReadDoneStatus)
  })

  val shadow = Wire(Vec(localSrc, new IntERSrcTrack))
  for (src <- 0 until localSrc) {
    shadow(src).valid := io.srcValid(src)
    shadow(src).trackId := io.srcTrackId(src)
    shadow(src).trackGen := io.srcTrackGen(src)
    shadow(src).srcIdx := io.srcIdx(src)
    shadow(src).psrc := io.psrc(src)
  }

  DataPathIntEROps.emitReadDoneObservation(
    out = io.out,
    robIdx = io.robIdx,
    srcShadow = shadow,
    dataSources = io.dataSources,
    intReadSources = io.intReadSrc,
    s1Valid = io.s1Valid,
    og1Failed = io.og1Failed,
    replayPronePath = replayProne.B,
    uncertainReadPath = io.uncertainReadPath,
    isWakeupSink = io.isWakeupSink,
    enableForwardReadDone = IntEREnableForwardReadDone.B,
    enableBypassReadDone = IntEREnableBypassReadDone.B,
    enableBypass2ReadDone = IntEREnableBypass2ReadDone.B,
    status = Some(io.status)
  )
}

class IntERBackendReadDoneMergeProbe(regionCount: Int, laneCount: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val region = Input(Vec(regionCount, Vec(laneCount, Valid(new IntERSrcValueReadDone))))
    val out = Output(Vec(laneCount, Valid(new IntERSrcValueReadDone)))
  })

  BackendIntEROps.mergeReadDoneRegions(io.out, io.region)
}

class IntERWbProducerReadyProbe(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val portValid = Input(Bool())
    val rfWen = Input(Bool())
    val pdest = Input(UInt(PhyRegIdxWidth.W))
    val robIdx = Input(new rob.RobPtr)
    val out = Output(Valid(new IntERProducerReady))
  })

  val wb = Wire(new Bundles.WriteBackRFBundle(IntWB(port = 0), backendParams))
  wb := 0.U.asTypeOf(wb)
  wb.rfWen := io.rfWen
  wb.pdest := io.pdest
  wb.intERRobIdx.get := io.robIdx

  WbArbiterIntEROps.emitProducerReady(io.out, io.portValid, wb)
}

class IntERBackendIntRfOwnerProbe(regionCount: Int, laneCount: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val ownerCommit = Input(Vec(laneCount, Valid(new IntCommitWriteback)))
    val otherCommit = Input(Vec(regionCount - 1, Vec(laneCount, Valid(new IntCommitWriteback))))
    val ownerProducer = Input(Vec(laneCount, Valid(new IntERProducerReady)))
    val otherProducer = Input(Vec(regionCount - 1, Vec(laneCount, Valid(new IntERProducerReady))))
    val commitOut = Output(Vec(laneCount, Valid(new IntCommitWriteback)))
    val producerOut = Output(Vec(laneCount, Valid(new IntERProducerReady)))
  })

  require(regionCount > 1, "integer RF owner probe needs at least one non-owner region")
  BackendIntEROps.connectIntRfOwnerCommitWriteback(io.commitOut, io.ownerCommit)
  BackendIntEROps.connectIntRfOwnerProducerReady(io.producerOut, io.ownerProducer)
}

class IntEarlyReleaseDataPathTest extends AnyFlatSpec with Matchers with ChiselSim {
  behavior of "Int early-release DataPath observation"

  private def configWith(params: IntEarlyReleaseParams): Parameters = {
    val defaultConfig = new DefaultConfig
    defaultConfig.alterPartial({
      case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy(
        intEarlyRelease = params
      )
    })
  }

  private def optimizedReadDoneParams: IntEarlyReleaseParams = IntEarlyReleaseParams(
    enable = true,
    trackEntries = 2,
    enableForwardReadDone = true,
    enableBypassReadDone = true
  )

  private def setRobPtr(ptr: rob.RobPtr, value: Int): Unit = {
    ptr.flag.poke(false.B)
    ptr.value.poke(value.U)
  }

  private def clearProbe(dut: IntERDataPathReadDoneProbe): Unit = {
    dut.io.s1Valid.poke(false.B)
    dut.io.og1Failed.poke(false.B)
    dut.io.uncertainReadPath.poke(false.B)
    dut.io.isWakeupSink.poke(true.B)
    setRobPtr(dut.io.robIdx, 0)
    for (src <- dut.io.srcValid.indices) {
      dut.io.srcValid(src).poke(false.B)
      dut.io.srcTrackId(src).poke(0.U)
      dut.io.srcTrackGen(src).poke(0.U)
      dut.io.srcIdx(src).poke(src.U)
      dut.io.psrc(src).poke(0.U)
      dut.io.dataSources(src).value.poke(DataSource.zero)
      dut.io.intReadSrc(src).poke(false.B)
    }
  }

  private def setTrackedSource(
    dut: IntERDataPathReadDoneProbe,
    localSrc: Int,
    logicalSrc: Int,
    trackId: Int,
    trackGen: Int,
    psrc: Int,
    dataSource: UInt,
    intReadSrc: Boolean = true
  ): Unit = {
    dut.io.srcValid(localSrc).poke(true.B)
    dut.io.srcTrackId(localSrc).poke(trackId.U)
    dut.io.srcTrackGen(localSrc).poke(trackGen.U)
    dut.io.srcIdx(localSrc).poke(logicalSrc.U)
    dut.io.psrc(localSrc).poke(psrc.U)
    dut.io.dataSources(localSrc).value.poke(dataSource)
    dut.io.intReadSrc(localSrc).poke(intReadSrc.B)
  }

  it should "gate DataPath read observation IO with Int ER enable" in {
    ChiselStage.elaborate(new IntERDataPathIOShapeProbe(expectedER = false)(configWith(IntEarlyReleaseParams())))
    ChiselStage.elaborate(new IntERDataPathIOShapeProbe(expectedER = true)(configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2))))
  }

  it should "map readDone lanes by global issue-queue order" in {
    val params = configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2))(XSCoreParamsKey).backendParams
    val intFirst = 0
    val fpFirst = params.allIssueParams.indexWhere(issue => params.fpSchdParams.get.issueBlockParams.exists(_ eq issue))
    val vecFirst = params.allIssueParams.indexWhere(issue => params.vecSchdParams.get.issueBlockParams.exists(_ eq issue))
    fpFirst should be > intFirst
    vecFirst should be > fpFirst

    val firstFpLane = params.allIssueParams.take(fpFirst).map(_.numDeq).sum
    val firstVecLane = params.allIssueParams.take(vecFirst).map(_.numDeq).sum
    DataPathIntEROps.readDoneLaneIndex(params.allIssueParams, fpFirst, 0) shouldBe firstFpLane
    DataPathIntEROps.readDoneLaneIndex(params.allIssueParams, vecFirst, 0) shouldBe firstVecLane
    val multiDeqIssue = params.allIssueParams.zipWithIndex.collectFirst { case (issue, idx) if issue.numDeq > 1 => idx }.get
    val multiDeqFirstLane = params.allIssueParams.take(multiDeqIssue).map(_.numDeq).sum
    DataPathIntEROps.readDoneLaneIndex(params.allIssueParams, multiDeqIssue, 1) shouldBe multiDeqFirstLane + 1
  }

  it should "merge cross-region readDone observations for the same global lane" in {
    simulate(new IntERBackendReadDoneMergeProbe(regionCount = 3, laneCount = 2)(configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2)))) { dut =>
      dut.io.region.poke(0.U.asTypeOf(dut.io.region))
      setRobPtr(dut.io.region(0)(0).bits.robIdx, 12)
      dut.io.region(0)(0).valid.poke(true.B)
      dut.io.region(0)(0).bits.src(0).valid.poke(true.B)
      dut.io.region(0)(0).bits.src(0).trackId.poke(1.U)
      dut.io.region(0)(0).bits.src(0).trackGen.poke(2.U)
      dut.io.region(0)(0).bits.src(0).srcIdx.poke(0.U)
      dut.io.region(0)(0).bits.src(0).psrc.poke(21.U)

      setRobPtr(dut.io.region(1)(0).bits.robIdx, 12)
      dut.io.region(1)(0).valid.poke(true.B)
      dut.io.region(1)(0).bits.fallback.poke(true.B)
      dut.io.region(1)(0).bits.reason.poke(IntERFallbackReason.unsupportedReadPath)
      dut.io.region(1)(0).bits.src(1).valid.poke(true.B)
      dut.io.region(1)(0).bits.src(1).trackId.poke(0.U)
      dut.io.region(1)(0).bits.src(1).trackGen.poke(3.U)
      dut.io.region(1)(0).bits.src(1).srcIdx.poke(1.U)
      dut.io.region(1)(0).bits.src(1).psrc.poke(22.U)

      dut.io.out(0).valid.expect(true.B)
      dut.io.out(0).bits.robIdx.value.expect(12.U)
      dut.io.out(0).bits.fallback.expect(true.B)
      dut.io.out(0).bits.reason.expect(IntERFallbackReason.unsupportedReadPath)
      dut.io.out(0).bits.src(0).valid.expect(true.B)
      dut.io.out(0).bits.src(0).trackId.expect(1.U)
      dut.io.out(0).bits.src(1).valid.expect(true.B)
      dut.io.out(0).bits.src(1).trackGen.expect(3.U)
      dut.io.out(1).valid.expect(false.B)
    }
  }

  it should "emit producer-ready only for accepted integer RF writeback ports" in {
    simulate(new IntERWbProducerReadyProbe()(configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2)))) { dut =>
      dut.io.portValid.poke(false.B)
      dut.io.rfWen.poke(true.B)
      dut.io.pdest.poke(21.U)
      setRobPtr(dut.io.robIdx, 7)
      dut.io.out.valid.expect(false.B)

      dut.io.portValid.poke(true.B)
      dut.io.rfWen.poke(false.B)
      dut.io.out.valid.expect(false.B)

      dut.io.rfWen.poke(true.B)
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.valid.expect(true.B)
      dut.io.out.bits.pdest.expect(21.U)
      dut.io.out.bits.robIdx.value.expect(7.U)
    }
  }

  it should "use integer RF owner commit writebacks and ignore non-owner local lanes" in {
    simulate(new IntERBackendIntRfOwnerProbe(regionCount = 3, laneCount = 2)(configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2)))) { dut =>
      dut.io.ownerCommit.poke(0.U.asTypeOf(dut.io.ownerCommit))
      dut.io.otherCommit.poke(0.U.asTypeOf(dut.io.otherCommit))
      dut.io.ownerProducer.poke(0.U.asTypeOf(dut.io.ownerProducer))
      dut.io.otherProducer.poke(0.U.asTypeOf(dut.io.otherProducer))

      dut.io.ownerCommit(0).valid.poke(true.B)
      setRobPtr(dut.io.ownerCommit(0).bits.robIdx, 9)
      dut.io.ownerCommit(0).bits.pdest.poke(21.U)
      dut.io.ownerCommit(0).bits.data.poke(0x1111.U)
      dut.io.otherCommit(1)(0).valid.poke(true.B)
      setRobPtr(dut.io.otherCommit(1)(0).bits.robIdx, 10)
      dut.io.otherCommit(1)(0).bits.pdest.poke(22.U)
      dut.io.otherCommit(1)(0).bits.data.poke(0x2222.U)
      dut.io.otherCommit(1)(1).valid.poke(true.B)
      setRobPtr(dut.io.otherCommit(1)(1).bits.robIdx, 13)
      dut.io.otherCommit(1)(1).bits.pdest.poke(23.U)
      dut.io.otherCommit(1)(1).bits.data.poke(0x3333.U)

      dut.io.ownerProducer(0).valid.poke(true.B)
      dut.io.ownerProducer(0).bits.valid.poke(true.B)
      dut.io.ownerProducer(0).bits.pdest.poke(31.U)
      setRobPtr(dut.io.ownerProducer(0).bits.robIdx, 11)
      dut.io.otherProducer(1)(1).valid.poke(true.B)
      dut.io.otherProducer(1)(1).bits.valid.poke(true.B)
      dut.io.otherProducer(1)(1).bits.pdest.poke(32.U)
      setRobPtr(dut.io.otherProducer(1)(1).bits.robIdx, 12)

      dut.io.commitOut(0).valid.expect(true.B)
      dut.io.commitOut(0).bits.robIdx.value.expect(9.U)
      dut.io.commitOut(0).bits.pdest.expect(21.U)
      dut.io.commitOut(0).bits.data.expect(0x1111.U)
      dut.io.commitOut(1).valid.expect(false.B)

      dut.io.producerOut(0).valid.expect(true.B)
      dut.io.producerOut(0).bits.valid.expect(true.B)
      dut.io.producerOut(0).bits.pdest.expect(31.U)
      dut.io.producerOut(0).bits.robIdx.value.expect(11.U)
      dut.io.producerOut(1).valid.expect(false.B)
    }
  }

  it should "emit readDone for supported register and regcache reads after final success" in {
    simulate(new IntERDataPathReadDoneProbe(localSrc = 2, replayProne = false)(configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2)))) { dut =>
      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      setRobPtr(dut.io.robIdx, 7)
      setTrackedSource(dut, localSrc = 0, logicalSrc = 1, trackId = 1, trackGen = 3, psrc = 21, dataSource = DataSource.reg)
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.fallback.expect(false.B)
      dut.io.out.bits.reason.expect(IntERFallbackReason.none)
      dut.io.out.bits.robIdx.value.expect(7.U)
      dut.io.out.bits.src(0).valid.expect(false.B)
      dut.io.out.bits.src(1).valid.expect(true.B)
      dut.io.out.bits.src(1).trackId.expect(1.U)
      dut.io.out.bits.src(1).trackGen.expect(3.U)
      dut.io.out.bits.src(1).srcIdx.expect(1.U)
      dut.io.out.bits.src(1).psrc.expect(21.U)

      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      setRobPtr(dut.io.robIdx, 8)
      setTrackedSource(dut, localSrc = 1, logicalSrc = 0, trackId = 0, trackGen = 4, psrc = 22, dataSource = DataSource.regcache)
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.fallback.expect(false.B)
      dut.io.out.bits.robIdx.value.expect(8.U)
      dut.io.out.bits.src(0).valid.expect(true.B)
      dut.io.out.bits.src(0).trackGen.expect(4.U)
      dut.io.out.bits.src(0).psrc.expect(22.U)
      dut.io.out.bits.src(1).valid.expect(false.B)
    }
  }

  it should "optionally emit readDone for forward and bypass reads after final success" in {
    simulate(new IntERDataPathReadDoneProbe(localSrc = 2, replayProne = false)(configWith(optimizedReadDoneParams))) { dut =>
      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      setRobPtr(dut.io.robIdx, 15)
      setTrackedSource(dut, localSrc = 0, logicalSrc = 0, trackId = 1, trackGen = 3, psrc = 21, dataSource = DataSource.forward)
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.fallback.expect(false.B)
      dut.io.out.bits.reason.expect(IntERFallbackReason.none)
      dut.io.out.bits.src(0).valid.expect(true.B)
      dut.io.out.bits.src(0).psrc.expect(21.U)
      dut.io.status.accepted.expect(true.B)
      dut.io.status.acceptedForwardSourceCount.expect(1.U)
      dut.io.status.fallbackForwardSourceCount.expect(0.U)

      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      setRobPtr(dut.io.robIdx, 16)
      setTrackedSource(dut, localSrc = 1, logicalSrc = 1, trackId = 0, trackGen = 4, psrc = 22, dataSource = DataSource.bypass)
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.fallback.expect(false.B)
      dut.io.out.bits.reason.expect(IntERFallbackReason.none)
      dut.io.out.bits.src(1).valid.expect(true.B)
      dut.io.out.bits.src(1).psrc.expect(22.U)
      dut.io.status.accepted.expect(true.B)
      dut.io.status.acceptedBypassSourceCount.expect(1.U)
      dut.io.status.fallbackBypassSourceCount.expect(0.U)
    }
  }

  it should "keep forward and bypass reads conservative when disabled or not wakeup sinks" in {
    simulate(new IntERDataPathReadDoneProbe(localSrc = 1, replayProne = false)(configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2)))) { dut =>
      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      setTrackedSource(dut, localSrc = 0, logicalSrc = 0, trackId = 1, trackGen = 3, psrc = 21, dataSource = DataSource.forward)
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.fallback.expect(true.B)
      dut.io.status.unsupportedReadPath.expect(true.B)
      dut.io.status.unsupportedForwardSourceCount.expect(1.U)

      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      setTrackedSource(dut, localSrc = 0, logicalSrc = 0, trackId = 1, trackGen = 3, psrc = 22, dataSource = DataSource.bypass)
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.fallback.expect(true.B)
      dut.io.status.unsupportedReadPath.expect(true.B)
      dut.io.status.unsupportedBypassSourceCount.expect(1.U)
    }

    simulate(new IntERDataPathReadDoneProbe(localSrc = 2, replayProne = false)(configWith(optimizedReadDoneParams))) { dut =>
      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      dut.io.isWakeupSink.poke(false.B)
      setTrackedSource(dut, localSrc = 0, logicalSrc = 0, trackId = 1, trackGen = 3, psrc = 21, dataSource = DataSource.forward)
      setTrackedSource(dut, localSrc = 1, logicalSrc = 1, trackId = 0, trackGen = 4, psrc = 22, dataSource = DataSource.bypass)
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.fallback.expect(true.B)
      dut.io.status.unsupportedReadPath.expect(true.B)
      dut.io.status.unsupportedForwardSourceCount.expect(1.U)
      dut.io.status.unsupportedBypassSourceCount.expect(1.U)
    }
  }

  it should "keep bypass2 unsupported in the default optimized phase" in {
    simulate(new IntERDataPathReadDoneProbe(localSrc = 1, replayProne = false)(configWith(optimizedReadDoneParams))) { dut =>
      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      setTrackedSource(dut, localSrc = 0, logicalSrc = 0, trackId = 1, trackGen = 3, psrc = 21, dataSource = DataSource.bypass2)
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.fallback.expect(true.B)
      dut.io.status.trackedBypass2SourceCount.expect(1.U)
      dut.io.status.unsupportedReadPath.expect(true.B)
    }
  }

  it should "suppress observations for cancelled admissions and og1 failures" in {
    simulate(new IntERDataPathReadDoneProbe(localSrc = 1, replayProne = false)(configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2)))) { dut =>
      clearProbe(dut)
      setTrackedSource(dut, localSrc = 0, logicalSrc = 0, trackId = 1, trackGen = 3, psrc = 21, dataSource = DataSource.reg)
      dut.io.s1Valid.poke(false.B)
      dut.io.og1Failed.poke(false.B)
      dut.io.out.valid.expect(false.B)

      dut.io.s1Valid.poke(true.B)
      dut.io.og1Failed.poke(true.B)
      dut.io.out.valid.expect(false.B)
    }
  }

  it should "suppress tracked sources that this DataPath did not read as integer values" in {
    simulate(new IntERDataPathReadDoneProbe(localSrc = 1, replayProne = false)(configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2)))) { dut =>
      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      setRobPtr(dut.io.robIdx, 12)
      setTrackedSource(
        dut,
        localSrc = 0,
        logicalSrc = 0,
        trackId = 1,
        trackGen = 3,
        psrc = 21,
        dataSource = DataSource.reg,
        intReadSrc = false
      )
      dut.io.out.valid.expect(false.B)
    }

    simulate(new IntERDataPathReadDoneProbe(localSrc = 2, replayProne = false)(configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2)))) { dut =>
      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      setRobPtr(dut.io.robIdx, 13)
      setTrackedSource(dut, localSrc = 0, logicalSrc = 0, trackId = 1, trackGen = 3, psrc = 21, dataSource = DataSource.forward)
      setTrackedSource(
        dut,
        localSrc = 1,
        logicalSrc = 1,
        trackId = 0,
        trackGen = 4,
        psrc = 22,
        dataSource = DataSource.reg,
        intReadSrc = false
      )
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.fallback.expect(true.B)
      dut.io.out.bits.src(0).valid.expect(true.B)
      dut.io.out.bits.src(1).valid.expect(false.B)
    }
  }

  it should "emit keyed fallback for unsupported read paths and replay-prone paths" in {
    simulate(new IntERDataPathReadDoneProbe(localSrc = 2, replayProne = false)(configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2)))) { dut =>
      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      setRobPtr(dut.io.robIdx, 9)
      setTrackedSource(dut, localSrc = 0, logicalSrc = 0, trackId = 1, trackGen = 3, psrc = 21, dataSource = DataSource.forward)
      setTrackedSource(dut, localSrc = 1, logicalSrc = 1, trackId = 0, trackGen = 4, psrc = 22, dataSource = DataSource.reg)
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.fallback.expect(true.B)
      dut.io.out.bits.reason.expect(IntERFallbackReason.unsupportedReadPath)
      dut.io.out.bits.robIdx.value.expect(9.U)
      dut.io.out.bits.src(0).valid.expect(true.B)
      dut.io.out.bits.src(0).trackId.expect(1.U)
      dut.io.out.bits.src(1).valid.expect(true.B)
      dut.io.out.bits.src(1).trackGen.expect(4.U)
    }

    simulate(new IntERDataPathReadDoneProbe(localSrc = 1, replayProne = true)(configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2)))) { dut =>
      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      setRobPtr(dut.io.robIdx, 10)
      setTrackedSource(dut, localSrc = 0, logicalSrc = 0, trackId = 1, trackGen = 5, psrc = 23, dataSource = DataSource.reg)
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.fallback.expect(true.B)
      dut.io.out.bits.reason.expect(IntERFallbackReason.unsupportedReadPath)
      dut.io.out.bits.src(0).valid.expect(true.B)
      dut.io.out.bits.src(0).trackGen.expect(5.U)
    }
  }

  it should "emit keyed fallback for uncertain-latency supported read paths" in {
    simulate(new IntERDataPathReadDoneProbe(localSrc = 2, replayProne = false)(configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2)))) { dut =>
      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      dut.io.uncertainReadPath.poke(true.B)
      setRobPtr(dut.io.robIdx, 11)
      setTrackedSource(dut, localSrc = 0, logicalSrc = 0, trackId = 1, trackGen = 5, psrc = 24, dataSource = DataSource.reg)
      setTrackedSource(dut, localSrc = 1, logicalSrc = 1, trackId = 0, trackGen = 6, psrc = 25, dataSource = DataSource.regcache)
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.fallback.expect(true.B)
      dut.io.out.bits.reason.expect(IntERFallbackReason.unsupportedReadPath)
      dut.io.out.bits.robIdx.value.expect(11.U)
      dut.io.out.bits.src(0).valid.expect(true.B)
      dut.io.out.bits.src(0).trackId.expect(1.U)
      dut.io.out.bits.src(0).trackGen.expect(5.U)
      dut.io.out.bits.src(0).srcIdx.expect(0.U)
      dut.io.out.bits.src(0).psrc.expect(24.U)
      dut.io.out.bits.src(1).valid.expect(true.B)
      dut.io.out.bits.src(1).trackId.expect(0.U)
      dut.io.out.bits.src(1).trackGen.expect(6.U)
      dut.io.out.bits.src(1).srcIdx.expect(1.U)
      dut.io.out.bits.src(1).psrc.expect(25.U)
    }
  }

  it should "fallback forward and bypass reads on replay-prone or uncertain paths" in {
    simulate(new IntERDataPathReadDoneProbe(localSrc = 2, replayProne = true)(configWith(optimizedReadDoneParams))) { dut =>
      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      setTrackedSource(dut, localSrc = 0, logicalSrc = 0, trackId = 1, trackGen = 3, psrc = 21, dataSource = DataSource.forward)
      setTrackedSource(dut, localSrc = 1, logicalSrc = 1, trackId = 0, trackGen = 4, psrc = 22, dataSource = DataSource.bypass)
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.fallback.expect(true.B)
      dut.io.status.accepted.expect(false.B)
      dut.io.status.replayProne.expect(true.B)
      dut.io.status.replayForwardSourceCount.expect(1.U)
      dut.io.status.replayBypassSourceCount.expect(1.U)
      dut.io.status.fallbackForwardSourceCount.expect(1.U)
      dut.io.status.fallbackBypassSourceCount.expect(1.U)
    }

    simulate(new IntERDataPathReadDoneProbe(localSrc = 2, replayProne = false)(configWith(optimizedReadDoneParams))) { dut =>
      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      dut.io.uncertainReadPath.poke(true.B)
      setTrackedSource(dut, localSrc = 0, logicalSrc = 0, trackId = 1, trackGen = 3, psrc = 23, dataSource = DataSource.forward)
      setTrackedSource(dut, localSrc = 1, logicalSrc = 1, trackId = 0, trackGen = 4, psrc = 24, dataSource = DataSource.bypass)
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.fallback.expect(true.B)
      dut.io.status.accepted.expect(false.B)
      dut.io.status.uncertain.expect(true.B)
      dut.io.status.uncertainForwardSourceCount.expect(1.U)
      dut.io.status.uncertainBypassSourceCount.expect(1.U)
      dut.io.status.fallbackForwardSourceCount.expect(1.U)
      dut.io.status.fallbackBypassSourceCount.expect(1.U)
    }
  }

  it should "classify accepted fallback and suppressed read observations for perf" in {
    simulate(new IntERDataPathReadDoneProbe(localSrc = 1, replayProne = false)(configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2)))) { dut =>
      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      setRobPtr(dut.io.robIdx, 14)
      setTrackedSource(dut, localSrc = 0, logicalSrc = 0, trackId = 1, trackGen = 3, psrc = 21, dataSource = DataSource.reg)
      dut.io.status.tracked.expect(true.B)
      dut.io.status.accepted.expect(true.B)
      dut.io.status.fallback.expect(false.B)
      dut.io.status.suppressed.expect(false.B)
      dut.io.status.unsupportedReadPath.expect(false.B)

      dut.io.og1Failed.poke(true.B)
      dut.io.status.tracked.expect(true.B)
      dut.io.status.accepted.expect(false.B)
      dut.io.status.fallback.expect(false.B)
      dut.io.status.suppressed.expect(true.B)

      dut.io.s1Valid.poke(false.B)
      dut.io.og1Failed.poke(false.B)
      dut.io.status.tracked.expect(false.B)
      dut.io.status.accepted.expect(false.B)
      dut.io.status.fallback.expect(false.B)
      dut.io.status.suppressed.expect(false.B)

      dut.io.s1Valid.poke(true.B)
      dut.io.og1Failed.poke(false.B)
      dut.io.dataSources(0).value.poke(DataSource.forward)
      dut.io.status.tracked.expect(true.B)
      dut.io.status.accepted.expect(false.B)
      dut.io.status.fallback.expect(true.B)
      dut.io.status.unsupportedReadPath.expect(true.B)
    }

    simulate(new IntERDataPathReadDoneProbe(localSrc = 1, replayProne = true)(configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2)))) { dut =>
      clearProbe(dut)
      dut.io.s1Valid.poke(true.B)
      setTrackedSource(dut, localSrc = 0, logicalSrc = 0, trackId = 1, trackGen = 3, psrc = 21, dataSource = DataSource.reg)
      dut.io.status.tracked.expect(true.B)
      dut.io.status.fallback.expect(true.B)
      dut.io.status.replayProne.expect(true.B)
    }
  }
}
