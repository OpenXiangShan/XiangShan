package xiangshan.backend

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import chisel3.util.log2Ceil
import circt.stage.ChiselStage
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import top.{DefaultConfig, IntERFunctionalConfig, IntERFunctionalMinimalConfig, MediumConfig}
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan._
import xiangshan.TopDownCounters._
import xiangshan.backend.Bundles.{DecodeOutUop, DispatchOutUop, DynInst, EnqRobUop, IssueQueueDeqOg1Payload, IssueQueuePayload, RegionInUop, RenameOutUop, connectSamePort}
import xiangshan.backend.decode.{DecodeStage, DecodeStageIO, FusionDecodeInfo}
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.issue.{EnqEntry, EntryBundles, IssueBlockParams, OthersEntry}
import xiangshan.backend.rename.{RatReadPort, Reg_I, Rename, RenameIntEROps, RenameTable, RenameTableWrapper}
import xiangshan.backend.regfile.{FpPregParams, IntPregParams, V0PregParams, VfPregParams, VlPregParams}
import xiangshan.backend.rob.RobBundles.{RobCommitEntryBundle, RobEntryBundle, connectCommitEntry, connectEnq}

class IntEarlyReleaseBundleProbe(
  localSrc: Int,
  expectedTrackIdWidth: Int,
  expectedLogicalSrcWidth: Int,
  expectedRenameWidth: Int,
  expectedCommitWidth: Int,
  expectedIntPhyRegs: Int,
  expectedRobSize: Int,
  expectedEnable: Boolean,
  expectedObserveOnly: Boolean
)(implicit p: Parameters) extends XSModule {
  require(localSrc > 0, "localSrc must be positive")

  val logical = Wire(IntERBundleHelper.logicalSrcVec)
  val local = Wire(IntERBundleHelper.localSrcVec(localSrc))
  val robSrc = Wire(IntERBundleHelper.logicalRobSrcVec)
  val readDone = Wire(new IntERSrcValueReadDone)
  val squash = Wire(new IntERSquashSource)
  val stGuard = Wire(new IntERSTGuardDec)
  val earlyFree = Wire(new IntEREarlyFreeReq)
  val suppress = Wire(new IntERCommitSuppress)
  val src = Wire(new IntERSrcTrack)

  logical := 0.U.asTypeOf(logical)
  local := 0.U.asTypeOf(local)
  robSrc := 0.U.asTypeOf(robSrc)
  readDone := 0.U.asTypeOf(readDone)
  squash := 0.U.asTypeOf(squash)
  stGuard := 0.U.asTypeOf(stGuard)
  earlyFree := 0.U.asTypeOf(earlyFree)
  suppress := 0.U.asTypeOf(suppress)
  src := 0.U.asTypeOf(src)

  require(EnableIntEarlyRegRelease == expectedEnable, "Int ER feature enable must follow the selected config")
  require(IntERObserveOnly == expectedObserveOnly, "Int ER observe-only mode must follow the selected config")
  require(IntERTrackIdWidth == expectedTrackIdWidth, "track id width must follow trackEntries")
  require(logical.length == expectedLogicalSrcWidth, "logical helper must use full backend source width")
  require(local.length == localSrc, "local helper must use caller-provided local source width")
  require(src.srcIdx.getWidth == IntERBundleHelper.sourceIndexWidth(expectedLogicalSrcWidth), "srcIdx width must preserve original logical source slot")
  require(IntERFallbackReason.width == 4, "fallback reason encoding width changed unexpectedly")
  require(RenameWidth == expectedRenameWidth, "IntER trackEntries must not change RenameWidth")
  require(CommitWidth == expectedCommitWidth, "IntER trackEntries must not change CommitWidth")
  require(IntPhyRegs == expectedIntPhyRegs, "IntER trackEntries must not change integer physical register count")
  require(RobSize == expectedRobSize, "IntER trackEntries must not change ROB size")
}

class RenameTableWrapperOldDestPortShapeProbe(
  expectedOldDestPort: Boolean
)(implicit p: Parameters) extends RenameTableWrapper {
  require(io.intReadPorts.length == RenameWidth, "wrapper int RAT lane count must follow RenameWidth")
  require(io.intReadPorts.head.length == backendParams.numIntRegSrc, "wrapper int RAT source ports must follow backend topology")

  val oldDestPort = io.elements.get("intOldDestReadPorts")
  require(oldDestPort.isDefined == expectedOldDestPort, "wrapper old-dest RAT port presence must follow Int ER enable")
  oldDestPort.foreach { port =>
    require(port.asInstanceOf[Vec[RatReadPort]].length == RenameWidth, "wrapper old-dest RAT port must have one entry per rename lane")
  }
}

class RenameOutUopERMetaShapeProbe(
  expectedERMeta: Boolean
)(implicit p: Parameters) extends XSModule {
  val uop = Wire(new RenameOutUop)
  uop := 0.U.asTypeOf(uop)

  val erMeta = uop.elements.get("intER")
  require(erMeta.isDefined == expectedERMeta, "rename output ER metadata presence must follow Int ER enable")
  erMeta.foreach { meta =>
    val typed = meta.asInstanceOf[IntERUopMeta]
    require(typed.src.length == backendParams.numSrc, "rename output ER metadata must use full logical source width")
  }
}

class IntERDownstreamPayloadShapeProbe(
  expectedERMeta: Boolean
)(implicit p: Parameters) extends XSModule {
  private val issueParams = backendParams.allIssueParams
  issueParams.flatMap(_.exuBlockParams).foreach(_.bindBackendParam(backendParams))
  issueParams.foreach { issue =>
    issue.bindBackendParam(backendParams)
    issue.allExuParams.foreach(_.bindBackendParam(backendParams))
    issue.exuBlockParams.foreach(_.bindIssueBlockParam(issue))
  }
  private val issueParam = issueParams.find { issue =>
    issue.numSrc > 0 && issue.numRegSrc > 0 && issue.numSrc < backendParams.numSrc
  }.getOrElse(issueParams.find(issue => issue.numSrc > 0 && issue.numRegSrc > 0).get)
  private val exuParam = issueParam.exuBlockParams.find(_.numRegSrc > 0).get
  private implicit val implicitIssueParam: IssueBlockParams = issueParam

  val dynInst = Wire(new DynInst)
  val dispatchUop = Wire(new DispatchOutUop)
  val regionUop = Wire(new RegionInUop(issueParam))
  val issuePayload = Wire(new IssueQueuePayload(issueParam))
  val srcStatus = Wire(new EntryBundles.SrcStatus)
  val deqPayload = Wire(new IssueQueueDeqOg1Payload(exuParam))

  dynInst := 0.U.asTypeOf(dynInst)
  dispatchUop := 0.U.asTypeOf(dispatchUop)
  regionUop := 0.U.asTypeOf(regionUop)
  issuePayload := 0.U.asTypeOf(issuePayload)
  srcStatus := 0.U.asTypeOf(srcStatus)
  deqPayload := 0.U.asTypeOf(deqPayload)

  val dynER = dynInst.elements.get("intER")
  val dispatchER = dispatchUop.elements.get("intER")
  val regionER = regionUop.elements.get("intER")
  val payloadER = issuePayload.elements.get("intER")
  val statusER = srcStatus.elements.get("intER")
  val deqER = deqPayload.elements.get("intER")

  require(dynER.isDefined == expectedERMeta, "DynInst ER metadata presence must follow feature enable")
  require(dispatchER.isDefined == expectedERMeta, "DispatchOutUop ER metadata presence must follow feature enable")
  require(regionER.isDefined == expectedERMeta, "RegionInUop ER metadata presence must follow feature enable")
  require(payloadER.isDefined == expectedERMeta, "IssueQueuePayload ER metadata presence must follow feature enable")
  require(statusER.isDefined == expectedERMeta, "SrcStatus ER metadata presence must follow feature enable")
  require(deqER.isDefined == expectedERMeta, "IssueQueueDeqOg1Payload ER metadata presence must follow feature enable")

  if (expectedERMeta) {
    def srcLength(meta: Data): Int = {
      meta.asInstanceOf[Bundle].elements("src").asInstanceOf[Vec[IntERSrcTrack]].length
    }

    require(dynER.get.asInstanceOf[IntERUopMeta].src.length == backendParams.numSrc, "DynInst must use full logical ER source width")
    require(dispatchER.get.asInstanceOf[IntERUopMeta].src.length == backendParams.numSrc, "DispatchOutUop must use full logical ER source width")
    require(srcLength(regionER.get) == issueParam.numSrc, "RegionInUop must use issue-local ER source width")
    require(srcLength(payloadER.get) == issueParam.numSrc, "IssueQueuePayload must use issue-local ER source width")
    require(statusER.get.asInstanceOf[IntERSrcTrack].srcIdx.getWidth == IntERSrcIdxWidth, "SrcStatus must preserve full logical source index width")
    require(deqER.get.asInstanceOf[Vec[IntERSrcTrack]].length == exuParam.numRegSrc, "IssueQueueDeqOg1Payload must use execute-local register source width")
    require(
      srcLength(regionER.get) < dynER.get.asInstanceOf[IntERUopMeta].src.length,
      "shape probe must distinguish local issue source width from full logical source width"
    )
  }
}

class IntERRobMetadataShapeProbe(
  expectedERMeta: Boolean
)(implicit p: Parameters) extends XSModule {
  val robEntry = Wire(new RobEntryBundle)
  val commitEntry = Wire(new RobCommitEntryBundle)
  val rabInfo = Wire(new RabCommitInfo)

  robEntry := 0.U.asTypeOf(robEntry)
  commitEntry := 0.U.asTypeOf(commitEntry)
  rabInfo := 0.U.asTypeOf(rabInfo)

  val robEntryER = robEntry.elements.get("intER")
  val commitEntryER = commitEntry.elements.get("intER")
  val rabRedef = rabInfo.elements.get("intERCommitRedef")

  require(robEntryER.isDefined == expectedERMeta, "RobEntryBundle ER metadata presence must follow feature enable")
  require(commitEntryER.isDefined == expectedERMeta, "RobCommitEntryBundle ER metadata presence must follow feature enable")
  require(rabRedef.isDefined == expectedERMeta, "RabCommitInfo ER redef metadata presence must follow feature enable")

  if (expectedERMeta) {
    val robEntrySrc = robEntryER.get.asInstanceOf[Bundle].elements("src").asInstanceOf[Vec[IntERSrcRobState]]
    val commitEntrySrc = commitEntryER.get.asInstanceOf[Bundle].elements("src").asInstanceOf[Vec[IntERSrcRobState]]
    require(robEntrySrc.length == backendParams.numSrc, "ROB entry ER metadata must use full logical source width")
    require(commitEntrySrc.length == backendParams.numSrc, "ROB commit entry ER metadata must use full logical source width")
  }
}

class IntERRobMetadataPropagationProbe(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val inSrcValid = Input(Bool())
    val inTrackId = Input(UInt(IntERTrackIdWidth.W))
    val inTrackGen = Input(UInt(IntERTrackGenBits.W))
    val inSrcIdx = Input(UInt(IntERSrcIdxWidth.W))
    val inPsrc = Input(UInt(PhyRegIdxWidth.W))
    val inDestValid = Input(Bool())
    val inPdest = Input(UInt(PhyRegIdxWidth.W))
    val inRedefValid = Input(Bool())
    val inOldPdest = Input(UInt(PhyRegIdxWidth.W))
    val inMoveSrcLReg = Input(UInt(LogicRegsWidth.W))
    val entrySrcValid = Output(Bool())
    val entrySrcReadDone = Output(Bool())
    val entryTrackId = Output(UInt(IntERTrackIdWidth.W))
    val entryTrackGen = Output(UInt(IntERTrackGenBits.W))
    val entrySrcIdx = Output(UInt(IntERSrcIdxWidth.W))
    val entryPsrc = Output(UInt(PhyRegIdxWidth.W))
    val entryDestValid = Output(Bool())
    val entryPdest = Output(UInt(PhyRegIdxWidth.W))
    val entryRedefValid = Output(Bool())
    val entryOldPdest = Output(UInt(PhyRegIdxWidth.W))
    val entryMoveSrcLReg = Output(UInt(LogicRegsWidth.W))
    val commitSrcValid = Output(Bool())
    val commitSrcReadDone = Output(Bool())
    val commitTrackId = Output(UInt(IntERTrackIdWidth.W))
    val commitTrackGen = Output(UInt(IntERTrackGenBits.W))
    val commitSrcIdx = Output(UInt(IntERSrcIdxWidth.W))
    val commitPsrc = Output(UInt(PhyRegIdxWidth.W))
    val commitDestValid = Output(Bool())
    val commitPdest = Output(UInt(PhyRegIdxWidth.W))
    val commitRedefValid = Output(Bool())
    val commitOldPdest = Output(UInt(PhyRegIdxWidth.W))
    val commitMoveSrcLReg = Output(UInt(LogicRegsWidth.W))
  })

  val enq = Wire(new EnqRobUop)
  val robEntry = Wire(new RobEntryBundle)
  val commitEntry = Wire(new RobCommitEntryBundle)

  enq := 0.U.asTypeOf(enq)
  robEntry := 0.U.asTypeOf(robEntry)
  commitEntry := 0.U.asTypeOf(commitEntry)

  enq.firstUop := true.B
  enq.lastUop := true.B
  enq.numUops := 1.U
  enq.numWB := 1.U
  enq.moveSrcLReg := io.inMoveSrcLReg
  enq.intER.get.src(1).valid := io.inSrcValid
  enq.intER.get.src(1).trackId := io.inTrackId
  enq.intER.get.src(1).trackGen := io.inTrackGen
  enq.intER.get.src(1).srcIdx := io.inSrcIdx
  enq.intER.get.src(1).psrc := io.inPsrc
  enq.intER.get.dest.valid := io.inDestValid
  enq.intER.get.dest.trackId := io.inTrackId
  enq.intER.get.dest.trackGen := io.inTrackGen
  enq.intER.get.dest.pdest := io.inPdest
  enq.intER.get.redef.valid := io.inRedefValid
  enq.intER.get.redef.trackId := io.inTrackId
  enq.intER.get.redef.trackGen := io.inTrackGen
  enq.intER.get.redef.oldPdest := io.inOldPdest

  connectEnq(robEntry, enq)
  connectCommitEntry(commitEntry, robEntry)

  io.entrySrcValid := robEntry.intER.get.src(1).valid
  io.entrySrcReadDone := robEntry.intER.get.src(1).readDone
  io.entryTrackId := robEntry.intER.get.src(1).trackId
  io.entryTrackGen := robEntry.intER.get.src(1).trackGen
  io.entrySrcIdx := robEntry.intER.get.src(1).srcIdx
  io.entryPsrc := robEntry.intER.get.src(1).psrc
  io.entryDestValid := robEntry.intER.get.dest.valid
  io.entryPdest := robEntry.intER.get.dest.pdest
  io.entryRedefValid := robEntry.intER.get.redef.valid
  io.entryOldPdest := robEntry.intER.get.redef.oldPdest
  io.entryMoveSrcLReg := robEntry.debug_move_src.get

  io.commitSrcValid := commitEntry.intER.get.src(1).valid
  io.commitSrcReadDone := commitEntry.intER.get.src(1).readDone
  io.commitTrackId := commitEntry.intER.get.src(1).trackId
  io.commitTrackGen := commitEntry.intER.get.src(1).trackGen
  io.commitSrcIdx := commitEntry.intER.get.src(1).srcIdx
  io.commitPsrc := commitEntry.intER.get.src(1).psrc
  io.commitDestValid := commitEntry.intER.get.dest.valid
  io.commitPdest := commitEntry.intER.get.dest.pdest
  io.commitRedefValid := commitEntry.intER.get.redef.valid
  io.commitOldPdest := commitEntry.intER.get.redef.oldPdest
  io.commitMoveSrcLReg := commitEntry.debug_move_src.get
}

class IntERAllLocalPayloadShapeProbe(
  expectedERMeta: Boolean
)(implicit p: Parameters) extends XSModule {
  private val issueParams = backendParams.allIssueParams
  issueParams.flatMap(_.exuBlockParams).foreach(_.bindBackendParam(backendParams))
  issueParams.foreach { issue =>
    issue.bindBackendParam(backendParams)
    issue.allExuParams.foreach(_.bindBackendParam(backendParams))
    issue.exuBlockParams.foreach(_.bindIssueBlockParam(issue))
  }

  require(issueParams.exists(_.inIntSchd), "shape probe must cover integer scheduler payloads")
  require(issueParams.exists(_.inFpSchd), "shape probe must cover floating-point scheduler payloads")
  require(issueParams.exists(_.inVfSchd), "shape probe must cover vector scheduler payloads")
  require(
    issueParams.flatMap(_.exuBlockParams).exists(exu => exu.numRegSrc > 0 && exu.numRegSrc < backendParams.numSrc),
    "shape probe must include execute payloads narrower than full logical source width"
  )
  private val realDeqParams = issueParams.flatMap(issue => issue.exuBlockParams.filterNot(_.fakeUnit).map(issue -> _))
  require(realDeqParams.exists(_._1.inIntSchd), "shape probe must cover integer deq payloads")
  require(realDeqParams.exists(_._1.inFpSchd), "shape probe must cover floating-point deq payloads")
  require(realDeqParams.exists(_._1.inVfSchd), "shape probe must cover vector deq payloads")

  issueParams.foreach { issue =>
    implicit val implicitIssueParam: IssueBlockParams = issue
    val regionUop = Wire(new RegionInUop(issue))
    val issuePayload = Wire(new IssueQueuePayload(issue))
    val entry = Wire(new EntryBundles.EntryBundle)

    regionUop := 0.U.asTypeOf(regionUop)
    issuePayload := 0.U.asTypeOf(issuePayload)
    entry := 0.U.asTypeOf(entry)

    val regionER = regionUop.elements.get("intER")
    val payloadER = issuePayload.elements.get("intER")
    require(regionER.isDefined == expectedERMeta, s"RegionInUop ER metadata presence mismatch for ${issue.getIQName}")
    require(payloadER.isDefined == expectedERMeta, s"IssueQueuePayload ER metadata presence mismatch for ${issue.getIQName}")
    require(entry.status.srcStatus.length == issue.numRegSrc, s"SrcStatus width mismatch for ${issue.getIQName}")

    if (expectedERMeta) {
      require(regionER.get.asInstanceOf[IntERLocalUopMeta].src.length == issue.numSrc, s"RegionInUop ER source width mismatch for ${issue.getIQName}")
      require(payloadER.get.asInstanceOf[IntERLocalUopMeta].src.length == issue.numSrc, s"IssueQueuePayload ER source width mismatch for ${issue.getIQName}")
      entry.status.srcStatus.foreach { status =>
        require(status.elements.get("intER").isDefined, s"SrcStatus ER metadata missing for ${issue.getIQName}")
      }
    }

    issue.exuBlockParams.filterNot(_.fakeUnit).foreach { exu =>
      val deqPayload = Wire(new IssueQueueDeqOg1Payload(exu))
      deqPayload := 0.U.asTypeOf(deqPayload)
      val deqER = deqPayload.elements.get("intER")
      require(deqER.isDefined == expectedERMeta, s"IssueQueueDeqOg1Payload ER metadata presence mismatch for ${exu.name}")
      if (expectedERMeta) {
        require(deqER.get.asInstanceOf[Vec[IntERSrcTrack]].length == exu.numRegSrc, s"deq ER source width mismatch for ${exu.name}")
      }
    }
  }
}

class IntERIssueQueueOwnershipProbe(
  expectedERMeta: Boolean
)(implicit p: Parameters) extends XSModule {
  private val issueParams = backendParams.allIssueParams
  backendParams.allExuParams.zipWithIndex.foreach { case (exu, idx) =>
    exu.bindBackendParam(backendParams)
    exu.updateIQWakeUpConfigs(backendParams.iqWakeUpParams)
    exu.updateExuIdx(idx)
  }
  issueParams.foreach { issue =>
    issue.bindBackendParam(backendParams)
    issue.allExuParams.foreach(_.bindBackendParam(backendParams))
    issue.exuBlockParams.foreach(_.bindIssueBlockParam(issue))
  }

  private def hasForbiddenERName(name: String, insideER: Boolean): Boolean = {
    val lower = name.toLowerCase
    val erContext = insideER || lower.contains("inter")
    erContext && (lower.contains("readdone") || lower.contains("uca") || lower.contains("decrement"))
  }

  private def hasForbiddenERPort(name: String, data: Data, insideER: Boolean = false): Boolean = {
    val lower = name.toLowerCase
    val erContext = insideER || lower.contains("inter")
    hasForbiddenERName(name, insideER) || (data match {
      case record: Record => record.elements.exists { case (fieldName, fieldData) =>
        hasForbiddenERPort(fieldName, fieldData, erContext)
      }
      case vec: Vec[_] => vec.getElements.zipWithIndex.exists { case (fieldData, fieldIdx) =>
        hasForbiddenERPort(fieldIdx.toString, fieldData, erContext)
      }
      case _ => false
    })
  }

  issueParams.foreach { issue =>
    implicit val implicitIssueParam: IssueBlockParams = issue
    val regionUop = new RegionInUop(issue)
    val issuePayload = new IssueQueuePayload(issue)
    val entry = new EntryBundles.EntryBundle
    val deqEntry = new EntryBundles.EntryBundle(isDeq = true)
    val commonIn = new EntryBundles.CommonInBundle
    val commonOut = new EntryBundles.CommonOutBundle
    val commonWire = new EntryBundles.CommonWireBundle
    val srcStatus = new EntryBundles.SrcStatus

    require(!regionUop.elements.exists { case (name, data) => hasForbiddenERPort(name, data) }, s"RegionInUop owns ER side-effect port for ${issue.getIQName}")
    require(!issuePayload.elements.exists { case (name, data) => hasForbiddenERPort(name, data) }, s"IssueQueuePayload owns ER side-effect port for ${issue.getIQName}")
    require(!entry.elements.exists { case (name, data) => hasForbiddenERPort(name, data) }, s"EntryBundle owns ER side-effect port for ${issue.getIQName}")
    require(!deqEntry.elements.exists { case (name, data) => hasForbiddenERPort(name, data) }, s"deq EntryBundle owns ER side-effect port for ${issue.getIQName}")
    require(!commonIn.elements.exists { case (name, data) => hasForbiddenERPort(name, data) }, s"IssueQueue common input owns ER side-effect port for ${issue.getIQName}")
    require(!commonOut.elements.exists { case (name, data) => hasForbiddenERPort(name, data) }, s"IssueQueue common output owns ER side-effect port for ${issue.getIQName}")
    require(!commonWire.elements.exists { case (name, data) => hasForbiddenERPort(name, data) }, s"IssueQueue common wire owns ER side-effect port for ${issue.getIQName}")
    issue.exuBlockParams.filterNot(_.fakeUnit).foreach { exu =>
      val deqPayload = new IssueQueueDeqOg1Payload(exu)
      require(!deqPayload.elements.exists { case (name, data) => hasForbiddenERPort(name, data) }, s"IssueQueue deq payload owns ER side-effect port for ${exu.name}")
    }

    val srcER = srcStatus.elements.get("intER")
    require(srcER.isDefined == expectedERMeta, s"SrcStatus ER metadata presence mismatch for ${issue.getIQName}")
    srcER.foreach { meta =>
      val fields = meta.asInstanceOf[IntERSrcTrack].elements.keySet
      require(fields == Set("valid", "trackId", "trackGen", "srcIdx", "psrc"), s"SrcStatus ER metadata carries side-effect state for ${issue.getIQName}")
    }
  }
}

class IntERIssueEntryInitializationProbe(implicit p: Parameters) extends XSModule {
  private val issueParams = backendParams.allIssueParams
  backendParams.allExuParams.zipWithIndex.foreach { case (exu, idx) =>
    exu.bindBackendParam(backendParams)
    exu.updateIQWakeUpConfigs(backendParams.iqWakeUpParams)
    exu.updateExuIdx(idx)
  }
  issueParams.foreach { issue =>
    issue.bindBackendParam(backendParams)
    issue.allExuParams.foreach(_.bindBackendParam(backendParams))
    issue.exuBlockParams.foreach(_.bindIssueBlockParam(issue))
  }
  private val issueParam = issueParams.find { issue =>
    issue.numRegSrc > 0 && !issue.needReadRegCache && !issue.isVecMemIQ
  }.get
  private implicit val implicitIssueParam: IssueBlockParams = issueParam

  val enqEntry = Module(new EnqEntry(isComp = false))
  val othersEntry = Module(new OthersEntry(isComp = false))

  enqEntry.io.commonIn := 0.U.asTypeOf(enqEntry.io.commonIn)
  enqEntry.io.enqDelayIn1 := 0.U.asTypeOf(enqEntry.io.enqDelayIn1)
  enqEntry.io.enqDelayIn2 := 0.U.asTypeOf(enqEntry.io.enqDelayIn2)
  othersEntry.io.commonIn := 0.U.asTypeOf(othersEntry.io.commonIn)
}

class IntERDownstreamPropagationProbe(implicit p: Parameters) extends XSModule {
  private val issueParams = backendParams.allIssueParams
  issueParams.flatMap(_.exuBlockParams).foreach(_.bindBackendParam(backendParams))
  issueParams.foreach { issue =>
    issue.bindBackendParam(backendParams)
    issue.exuBlockParams.foreach(_.bindIssueBlockParam(issue))
  }
  private val issueParam = issueParams.find { issue =>
    issue.numSrc > 0 && issue.numRegSrc > 0 && issue.numSrc < backendParams.numSrc
  }.getOrElse(issueParams.find(issue => issue.numSrc > 0 && issue.numRegSrc > 0).get)
  private val deqIdx = issueParam.exuBlockParams.indexWhere(_.numRegSrc > 0)
  require(deqIdx >= 0, "propagation probe requires an execute unit with register sources")
  private implicit val implicitIssueParam: IssueBlockParams = issueParam

  val io = IO(new Bundle {
    val inValid = Input(Bool())
    val inTrackId = Input(UInt(IntERTrackIdWidth.W))
    val inTrackGen = Input(UInt(IntERTrackGenBits.W))
    val inSrcIdx = Input(UInt(IntERSrcIdxWidth.W))
    val inPsrc = Input(UInt(PhyRegIdxWidth.W))

    val dispatchValid = Output(Bool())
    val regionValid = Output(Bool())
    val payloadValid = Output(Bool())
    val statusValid = Output(Bool())
    val deqValid = Output(Bool())
    val deqTrackId = Output(UInt(IntERTrackIdWidth.W))
    val deqTrackGen = Output(UInt(IntERTrackGenBits.W))
    val deqSrcIdx = Output(UInt(IntERSrcIdxWidth.W))
    val deqPsrc = Output(UInt(PhyRegIdxWidth.W))
  })

  val renameUop = Wire(new RenameOutUop)
  val dispatchUop = Wire(new DispatchOutUop)
  val regionUop = Wire(new RegionInUop(issueParam))
  val entry = Wire(new EntryBundles.EntryBundle)
  val renameMeta = renameUop.elements("intER").asInstanceOf[IntERUopMeta]
  val dispatchMeta = dispatchUop.elements("intER").asInstanceOf[IntERUopMeta]
  val regionMeta = regionUop.elements("intER").asInstanceOf[Bundle]
  val regionSrc = regionMeta.elements("src").asInstanceOf[Vec[IntERSrcTrack]]
  val payloadMeta = entry.payload.elements("intER").asInstanceOf[Bundle]
  val payloadSrc = payloadMeta.elements("src").asInstanceOf[Vec[IntERSrcTrack]]

  renameUop := 0.U.asTypeOf(renameUop)
  dispatchUop := 0.U.asTypeOf(dispatchUop)
  regionUop := 0.U.asTypeOf(regionUop)
  entry := 0.U.asTypeOf(entry)

  renameMeta.src(0).valid := io.inValid
  renameMeta.src(0).trackId := io.inTrackId
  renameMeta.src(0).trackGen := io.inTrackGen
  renameMeta.src(0).srcIdx := io.inSrcIdx
  renameMeta.src(0).psrc := io.inPsrc

  connectSamePort(dispatchUop, renameUop)
  IntERBundleHelper.connectLocalUopMeta(regionMeta.asInstanceOf[IntERLocalUopMeta], dispatchMeta)
  connectSamePort(entry.payload, regionUop)
  for (src <- 0 until issueParam.numRegSrc) {
    entry.status.srcStatus(src).elements("intER").asInstanceOf[IntERSrcTrack] := regionSrc(src)
  }

  val deqPayload = entry.toDeqOg1Payload(deqIdx)
  val deqSrc = deqPayload.elements("intER").asInstanceOf[Vec[IntERSrcTrack]]
  io.dispatchValid := dispatchMeta.src(0).valid
  io.regionValid := regionSrc(0).valid
  io.payloadValid := payloadSrc(0).valid
  io.statusValid := entry.status.srcStatus(0).elements("intER").asInstanceOf[IntERSrcTrack].valid
  io.deqValid := deqSrc(0).valid
  io.deqTrackId := deqSrc(0).trackId
  io.deqTrackGen := deqSrc(0).trackGen
  io.deqSrcIdx := deqSrc(0).srcIdx
  io.deqPsrc := deqSrc(0).psrc
}

class IntERStaToStdCopyProbe(implicit p: Parameters) extends XSModule {
  private val issueParams = backendParams.allIssueParams
  issueParams.flatMap(_.exuBlockParams).foreach(_.bindBackendParam(backendParams))
  issueParams.foreach { issue =>
    issue.bindBackendParam(backendParams)
    issue.exuBlockParams.foreach(_.bindIssueBlockParam(issue))
  }
  private val staParam = issueParams.find(_.isStAddrIQ).get
  private val stdParam = issueParams.find(_.isStdIQ).get
  require(staParam.numSrc > 1, "STA source vector must include the store-data source")
  require(stdParam.numSrc > 0, "STD source vector must include the store-data source")

  val io = IO(new Bundle {
    val inValid = Input(Bool())
    val inTrackId = Input(UInt(IntERTrackIdWidth.W))
    val inTrackGen = Input(UInt(IntERTrackGenBits.W))
    val inPsrc = Input(UInt(PhyRegIdxWidth.W))

    val outValid = Output(Bool())
    val outTrackId = Output(UInt(IntERTrackIdWidth.W))
    val outTrackGen = Output(UInt(IntERTrackGenBits.W))
    val outSrcIdx = Output(UInt(IntERSrcIdxWidth.W))
    val outPsrc = Output(UInt(PhyRegIdxWidth.W))
    val deqValid = Output(Bool())
    val deqTrackId = Output(UInt(IntERTrackIdWidth.W))
    val deqTrackGen = Output(UInt(IntERTrackGenBits.W))
    val deqSrcIdx = Output(UInt(IntERSrcIdxWidth.W))
    val deqPsrc = Output(UInt(PhyRegIdxWidth.W))
  })

  private val stdDeqIdx = stdParam.exuBlockParams.indexWhere(_.numRegSrc > 0)
  require(stdDeqIdx >= 0, "STD payload must have a deq source vector")
  private implicit val implicitStdParam: IssueBlockParams = stdParam
  val staUop = Wire(new RegionInUop(staParam))
  val stdUop = Wire(new RegionInUop(stdParam))
  val stdEntry = Wire(new EntryBundles.EntryBundle)
  val staER = staUop.intER.get
  val stdER = stdUop.intER.get

  staUop := 0.U.asTypeOf(staUop)
  stdUop := 0.U.asTypeOf(stdUop)
  stdEntry := 0.U.asTypeOf(stdEntry)

  staUop.psrc(0) := 31.U
  staUop.psrc(1) := io.inPsrc
  staER.src(0).valid := true.B
  staER.src(0).trackId := 0.U
  staER.src(0).trackGen := 0.U
  staER.src(0).srcIdx := 0.U
  staER.src(0).psrc := 31.U
  staER.src(1).valid := io.inValid
  staER.src(1).trackId := io.inTrackId
  staER.src(1).trackGen := io.inTrackGen
  staER.src(1).srcIdx := 1.U
  staER.src(1).psrc := io.inPsrc

  Region.connectStaToStdUop(stdUop, staUop)
  for (src <- 0 until stdParam.numRegSrc) {
    stdEntry.status.srcStatus(src).intER.get := stdER.src(src)
  }
  val deqER = stdEntry.toDeqOg1Payload(stdDeqIdx).intER.get

  io.outValid := stdER.src(0).valid
  io.outTrackId := stdER.src(0).trackId
  io.outTrackGen := stdER.src(0).trackGen
  io.outSrcIdx := stdER.src(0).srcIdx
  io.outPsrc := stdER.src(0).psrc
  io.deqValid := deqER(0).valid
  io.deqTrackId := deqER(0).trackId
  io.deqTrackGen := deqER(0).trackGen
  io.deqSrcIdx := deqER(0).srcIdx
  io.deqPsrc := deqER(0).psrc
}

class IntERIntRfBankSelectProbe(implicit p: Parameters) extends XSModule {
  private val issueParams = backendParams.allIssueParams
  issueParams.flatMap(_.exuBlockParams).foreach(_.bindBackendParam(backendParams))
  issueParams.foreach { issue =>
    issue.bindBackendParam(backendParams)
    issue.exuBlockParams.foreach(_.bindIssueBlockParam(issue))
  }
  private val issueParam = issueParams.find { issue =>
    issue.readIntRf && issue.numRegSrc > 0 && issue.rdPregIdxWidth > coreParams.intPreg.addrWidth
  }.getOrElse(throw new IllegalArgumentException("probe requires a mixed-width integer RF reader"))
  private implicit val implicitIssueParam: IssueBlockParams = issueParam

  require(coreParams.intPreg.numEntries == 128, "probe is specific to the 128-entry integer RF configuration")
  require(coreParams.intPreg.numBank == 4, "probe expects four integer RF banks")
  require(coreParams.intPreg.addrWidth == 7, "128-entry integer RF must use a 7-bit physical address")

  val io = IO(new Bundle {
    val psrc = Input(UInt(PhyRegIdxWidth.W))
    val bankRen = Output(Vec(coreParams.intPreg.numBank, Bool()))
  })

  val entry = Wire(new EntryBundles.EntryBundle)
  val deqEntry = Wire(new EntryBundles.EntryBundle(isDeq = true))
  entry := 0.U.asTypeOf(entry)
  deqEntry := 0.U.asTypeOf(deqEntry)

  entry.payload.srcType(0) := SrcType.xp
  entry.status.srcStatus(0).dataSources.value := DataSource.reg
  entry.status.srcStatus(0).psrc := io.psrc

  deqEntry.genXrfRen(entry)
  io.bankRen := deqEntry.rfBankRen.get(0)
}

class RenameOldDestBypassProbe(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val base = Input(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    val ldest = Input(Vec(RenameWidth, UInt(log2Ceil(IntLogicRegs).W)))
    val wen = Input(Vec(RenameWidth, Bool()))
    val data = Input(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    val out = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
  })

  io.out := RenameIntEROps.sameGroupOldDest(io.base, io.ldest, io.wen, io.data)
}

class RenameERDebugShapeProbe(
  expectedDebug: Boolean
)(implicit p: Parameters) extends Rename {
  val erDebug = io.elements.get("intERDebug")
  require(erDebug.isDefined == expectedDebug, "rename ER debug output presence must follow Int ER enable")
  erDebug.foreach { debug =>
    require(debug.asInstanceOf[IntERDebugBundle].entries.length == IntERTrackEntries, "rename ER debug entries must follow trackEntries")
  }
}

class RenameERPolicyProbe(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val renameFire = Input(Bool())
    val redirect = Input(Bool())
    val rabWalk = Input(Bool())
    val singleUop = Input(Bool())
    val numUops = Input(UInt(log2Ceil(MaxUopSize).W))
    val hasException = Input(Bool())
    val flushPipe = Input(Bool())
    val singleStep = Input(Bool())
    val isMove = Input(Bool())
    val needIntDest = Input(Bool())
    val ldest = Input(UInt(log2Ceil(IntLogicRegs).W))
    val sourceValid = Input(Bool())
    val sourcePsrc = Input(UInt(PhyRegIdxWidth.W))
    val pdest = Input(UInt(PhyRegIdxWidth.W))
    val oldPdest = Input(UInt(PhyRegIdxWidth.W))
    val robIdx = Input(new xiangshan.backend.rob.RobPtr)

    val sourceProbeValid = Output(Bool())
    val sourceFallback = Output(Bool())
    val producerEligible = Output(Bool())
    val redefProbeValid = Output(Bool())
    val srcValid = Output(Bool())
    val fallbackMark = Output(Bool())
    val destValid = Output(Bool())
    val redefValid = Output(Bool())
    val redefOldPdest = Output(UInt(PhyRegIdxWidth.W))
    val debugActiveCount = Output(UInt(log2Ceil(IntERTrackEntries + 1).W))
    val debugFallbackCount = Output(UInt(32.W))
    val debugRedirectKillCount = Output(UInt(32.W))
  })

  val uca = Module(new IntSparseUCA)
  val blocked = RenameIntEROps.redirectBlocked(io.redirect, io.rabWalk)
  val sourceProbeAllowed = RenameIntEROps.sourceProbeAllowed(io.renameFire, blocked)
  val robSingleUop = RenameIntEROps.singleRobEntryUop(io.singleUop, io.numUops)
  val sourceFallback = RenameIntEROps.sourceFallback(
    renameFire = io.renameFire,
    isMove = io.isMove,
    singleUop = robSingleUop,
    hasException = io.hasException,
    flushPipe = io.flushPipe,
    singleStep = io.singleStep,
    blocked = blocked
  )
  val producerEligible = RenameIntEROps.producerEligible(
    renameFire = io.renameFire,
    needIntDest = io.needIntDest,
    isMove = io.isMove,
    logicLdest = io.ldest,
    singleUop = robSingleUop,
    hasException = io.hasException,
    flushPipe = io.flushPipe,
    singleStep = io.singleStep,
    blocked = blocked
  )
  val redefProbeValid = RenameIntEROps.redefinitionProbeValid(
    renameFire = io.renameFire,
    needIntDest = io.needIntDest,
    logicLdest = io.ldest,
    blocked = blocked
  )

  uca.io.redirectKill := blocked
  uca.io.producerReady := 0.U.asTypeOf(uca.io.producerReady)
  uca.io.readDone := 0.U.asTypeOf(uca.io.readDone)
  uca.io.squash := 0.U.asTypeOf(uca.io.squash)
  uca.io.stGuardDec := 0.U.asTypeOf(uca.io.stGuardDec)
  uca.io.commitOldPdest := 0.U.asTypeOf(uca.io.commitOldPdest)
  uca.io.commitNeedFree := 0.U.asTypeOf(uca.io.commitNeedFree)
  uca.io.commitRedef := 0.U.asTypeOf(uca.io.commitRedef)
  uca.io.rename.source := 0.U.asTypeOf(uca.io.rename.source)
  uca.io.rename.sourceFallback := 0.U.asTypeOf(uca.io.rename.sourceFallback)
  uca.io.rename.alloc := 0.U.asTypeOf(uca.io.rename.alloc)
  uca.io.rename.redef := 0.U.asTypeOf(uca.io.rename.redef)
  uca.io.rename.redefFallback := 0.U.asTypeOf(uca.io.rename.redefFallback)

  uca.io.rename.sourceFallback(0) := sourceFallback
  uca.io.rename.alloc(0).valid := producerEligible
  uca.io.rename.alloc(0).bits.pdest := io.pdest
  uca.io.rename.alloc(0).bits.robIdx := io.robIdx
  uca.io.rename.redef(0).valid := redefProbeValid
  uca.io.rename.redef(0).bits.oldPdest := io.oldPdest
  uca.io.rename.redef(0).bits.robIdx := io.robIdx
  uca.io.rename.redefFallback(0) := RenameIntEROps.redefinitionFallback(
    renameFire = io.renameFire,
    needIntDest = io.needIntDest,
    logicLdest = io.ldest,
    singleUop = robSingleUop,
    hasException = io.hasException,
    flushPipe = io.flushPipe,
    singleStep = io.singleStep,
    blocked = blocked
  )
  uca.io.rename.source(0)(0).valid := sourceProbeAllowed && io.sourceValid && io.sourcePsrc =/= 0.U
  uca.io.rename.source(0)(0).psrc := io.sourcePsrc
  uca.io.rename.source(0)(0).srcIdx := 0.U

  io.sourceProbeValid := uca.io.rename.source(0)(0).valid
  io.sourceFallback := sourceFallback
  io.producerEligible := producerEligible
  io.redefProbeValid := redefProbeValid
  io.srcValid := uca.io.rename.srcMatch(0)(0).valid
  io.fallbackMark := uca.io.rename.fallbackMark(0)
  io.destValid := uca.io.rename.destTrack(0).valid
  io.redefValid := uca.io.rename.redefTrack(0).valid
  io.redefOldPdest := uca.io.rename.redefTrack(0).oldPdest
  io.debugActiveCount := uca.io.debug.activeCount
  io.debugFallbackCount := uca.io.debug.fallbackCount
  io.debugRedirectKillCount := uca.io.debug.redirectKillCount
}

class RenameERStoreDataPolicyProbe(implicit p: Parameters) extends XSModule {
  require(EnableIntEarlyRegRelease, "probe requires Int ER metadata")
  require(IntERLogicalSrcWidth > 1, "store-data policy probe requires source slot 1")

  val io = IO(new Bundle {
    val renameFire = Input(Bool())
    val allocProducer = Input(Bool())
    val singleUop = Input(Bool())
    val isScalarStore = Input(Bool())
    val source1Valid = Input(Bool())
    val source1Psrc = Input(UInt(PhyRegIdxWidth.W))
    val pdest = Input(UInt(PhyRegIdxWidth.W))
    val robIdx = Input(new xiangshan.backend.rob.RobPtr)

    val sourceFallback = Output(Bool())
    val src1Valid = Output(Bool())
    val fallbackMark = Output(Bool())
    val destValid = Output(Bool())
    val debugFallbackCount = Output(UInt(32.W))
  })

  val uca = Module(new IntSparseUCA)
  val sourceProbeAllowed = RenameIntEROps.sourceProbeAllowed(io.renameFire, blocked = false.B)
  val robSingleUop = RenameIntEROps.singleRobEntryUop(io.singleUop, 1.U)
  val sourceFallback = RenameIntEROps.sourceFallback(
    renameFire = io.renameFire,
    isMove = false.B,
    singleUop = robSingleUop,
    hasException = false.B,
    flushPipe = false.B,
    singleStep = false.B,
    blocked = false.B,
    conservativeStoreDataConsumer = io.isScalarStore && io.source1Valid && io.source1Psrc =/= 0.U
  )

  uca.io.redirectKill := false.B
  uca.io.producerReady := 0.U.asTypeOf(uca.io.producerReady)
  uca.io.readDone := 0.U.asTypeOf(uca.io.readDone)
  uca.io.squash := 0.U.asTypeOf(uca.io.squash)
  uca.io.stGuardDec := 0.U.asTypeOf(uca.io.stGuardDec)
  uca.io.commitOldPdest := 0.U.asTypeOf(uca.io.commitOldPdest)
  uca.io.commitNeedFree := 0.U.asTypeOf(uca.io.commitNeedFree)
  uca.io.commitRedef := 0.U.asTypeOf(uca.io.commitRedef)
  uca.io.rename.source := 0.U.asTypeOf(uca.io.rename.source)
  uca.io.rename.sourceFallback := 0.U.asTypeOf(uca.io.rename.sourceFallback)
  uca.io.rename.alloc := 0.U.asTypeOf(uca.io.rename.alloc)
  uca.io.rename.redef := 0.U.asTypeOf(uca.io.rename.redef)
  uca.io.rename.redefFallback := 0.U.asTypeOf(uca.io.rename.redefFallback)

  uca.io.rename.sourceFallback(0) := sourceFallback
  uca.io.rename.alloc(0).valid := io.allocProducer
  uca.io.rename.alloc(0).bits.pdest := io.pdest
  uca.io.rename.alloc(0).bits.robIdx := io.robIdx
  uca.io.rename.source(0)(1).valid := sourceProbeAllowed && io.source1Valid && io.source1Psrc =/= 0.U
  uca.io.rename.source(0)(1).psrc := io.source1Psrc
  uca.io.rename.source(0)(1).srcIdx := 1.U

  io.sourceFallback := sourceFallback
  io.src1Valid := uca.io.rename.srcMatch(0)(1).valid
  io.fallbackMark := uca.io.rename.fallbackMark(0)
  io.destValid := uca.io.rename.destTrack(0).valid
  io.debugFallbackCount := uca.io.debug.fallbackCount
}

class RenameERFullPathProbe(implicit p: Parameters) extends XSModule {
  private val intLogicWidth = log2Ceil(IntLogicRegs)

  val io = IO(new Bundle {
    val valid = Input(Vec(RenameWidth, Bool()))
    val outReady = Input(Bool())
    val readHold = Input(Vec(RenameWidth, Bool()))
    val redirect = Input(Bool())
    val rabWalk = Input(Bool())
    val ldest = Input(Vec(RenameWidth, UInt(intLogicWidth.W)))
    val lsrc0 = Input(Vec(RenameWidth, UInt(intLogicWidth.W)))
    val lsrc1 = Input(Vec(RenameWidth, UInt(intLogicWidth.W)))
    val oldDestAddr = Input(Vec(RenameWidth, UInt(intLogicWidth.W)))
    val src0Xp = Input(Vec(RenameWidth, Bool()))
    val src1Xp = Input(Vec(RenameWidth, Bool()))
    val rfWen = Input(Vec(RenameWidth, Bool()))
    val isMove = Input(Vec(RenameWidth, Bool()))
    val externalRedirectKill = Input(Bool())
    val producerReadyValid = Input(Bool())
    val producerReadyPdest = Input(UInt(PhyRegIdxWidth.W))
    val producerReadyRobIdx = Input(UInt(log2Ceil(RobSize).W))
    val readDoneValid = Input(Bool())
    val readDoneRobIdx = Input(UInt(log2Ceil(RobSize).W))
    val readDoneTrackId = Input(UInt(IntERTrackIdWidth.W))
    val readDoneTrackGen = Input(UInt(IntERTrackGenBits.W))
    val readDoneSrcIdx = Input(UInt(IntERSrcIdxWidth.W))
    val readDonePsrc = Input(UInt(PhyRegIdxWidth.W))
    val guardDecValid = Input(Bool())
    val guardDecRobIdx = Input(UInt(log2Ceil(RobSize).W))
    val guardDecTrackId = Input(UInt(IntERTrackIdWidth.W))
    val guardDecTrackGen = Input(UInt(IntERTrackGenBits.W))
    val guardDecOldPdest = Input(UInt(PhyRegIdxWidth.W))

    val inReady = Output(Vec(RenameWidth, Bool()))
    val outValid = Output(Vec(RenameWidth, Bool()))
    val outFire = Output(Vec(RenameWidth, Bool()))
    val outFirstUop = Output(Vec(RenameWidth, Bool()))
    val outLastUop = Output(Vec(RenameWidth, Bool()))
    val outNumUops = Output(Vec(RenameWidth, UInt(log2Ceil(MaxUopSize).W)))
    val outPdest = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    val outPsrc0 = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    val erDestValid = Output(Vec(RenameWidth, Bool()))
    val erDestPdest = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    val erRedefValid = Output(Vec(RenameWidth, Bool()))
    val erRedefOldPdest = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    val erSrc0Valid = Output(Vec(RenameWidth, Bool()))
    val erSrc0Psrc = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    val debugActiveCount = Output(UInt(log2Ceil(IntERTrackEntries + 1).W))
    val debugReadDoneDecCount = Output(UInt(32.W))
    val debugProducerReadyCount = Output(UInt(32.W))
    val debugGuardDecCount = Output(UInt(32.W))
    val debugEarlyFreeOpportunityCount = Output(UInt(32.W))
    val debugEarlyFreeCount = Output(UInt(32.W))
  })

  val rename = Module(new Rename)
  private def connectRobPtr(ptr: xiangshan.backend.rob.RobPtr, value: UInt): Unit = {
    ptr.flag := false.B
    ptr.value := value
  }

  rename.io.redirect.valid := io.redirect
  rename.io.redirect.bits := 0.U.asTypeOf(new Redirect)
  rename.io.rabCommits := 0.U.asTypeOf(new RabCommitIO)
  rename.io.rabCommits.isWalk := io.rabWalk
  rename.io.vlCommits := 0.U.asTypeOf(new VlCommitBundle(RabCommitWidth))
  rename.io.singleStep := false.B
  rename.io.validVec := io.valid
  rename.io.isFusionVec := 0.U.asTypeOf(rename.io.isFusionVec)
  rename.io.fusionCross2FtqVec := 0.U.asTypeOf(rename.io.fusionCross2FtqVec)
  rename.io.fusionInfo.foreach(_ := 0.U.asTypeOf(new FusionDecodeInfo))
  rename.io.ssit := 0.U.asTypeOf(rename.io.ssit)
  rename.io.waittable := 0.U.asTypeOf(rename.io.waittable)
  rename.io.snpt := 0.U.asTypeOf(rename.io.snpt)
  rename.io.snptLastEnq.valid := false.B
  rename.io.snptLastEnq.bits := 0.U.asTypeOf(rename.io.snptLastEnq.bits)
  rename.io.snptIsFull := false.B
  rename.io.hartId := 0.U
  rename.io.ratSnpt := 0.U.asTypeOf(rename.io.ratSnpt)
  rename.io.ratDiffCommits.foreach(_ := 0.U.asTypeOf(new DiffCommitIO))
  rename.io.ratDiffVlCommits.foreach(_ := 0.U.asTypeOf(new DiffVlCommitBundle(CommitWidth)))
  rename.io.debugDispatchAllFire.foreach(_ := false.B)
  rename.io.debugOutValidVec.foreach(_ := 0.U.asTypeOf(rename.io.debugOutValidVec.get))
  rename.io.debugRobHeadFuType.foreach(_ := 0.U)
  rename.io.debugRobHeadStall.foreach(_ := false.B)
  rename.io.debugLoadReason.foreach(_ := 0.U)
  rename.io.stallReason.in.reason.foreach(_ := NoStall.id.U)
  rename.io.intER.get := 0.U.asTypeOf(rename.io.intER.get)
  rename.io.intER.get.redirectKill := io.externalRedirectKill
  rename.io.intER.get.producerReady(0).valid := io.producerReadyValid
  rename.io.intER.get.producerReady(0).bits.valid := io.producerReadyValid
  rename.io.intER.get.producerReady(0).bits.pdest := io.producerReadyPdest
  connectRobPtr(rename.io.intER.get.producerReady(0).bits.robIdx, io.producerReadyRobIdx)
  rename.io.intER.get.readDone(0).valid := io.readDoneValid
  rename.io.intER.get.readDone(0).bits.fallback := false.B
  rename.io.intER.get.readDone(0).bits.reason := IntERFallbackReason.none
  connectRobPtr(rename.io.intER.get.readDone(0).bits.robIdx, io.readDoneRobIdx)
  rename.io.intER.get.readDone(0).bits.src(0).valid := io.readDoneValid
  rename.io.intER.get.readDone(0).bits.src(0).trackId := io.readDoneTrackId
  rename.io.intER.get.readDone(0).bits.src(0).trackGen := io.readDoneTrackGen
  rename.io.intER.get.readDone(0).bits.src(0).srcIdx := io.readDoneSrcIdx
  rename.io.intER.get.readDone(0).bits.src(0).psrc := io.readDonePsrc
  rename.io.intER.get.stGuardDec(0).valid := io.guardDecValid
  rename.io.intER.get.stGuardDec(0).bits.valid := io.guardDecValid
  connectRobPtr(rename.io.intER.get.stGuardDec(0).bits.robIdx, io.guardDecRobIdx)
  rename.io.intER.get.stGuardDec(0).bits.trackId := io.guardDecTrackId
  rename.io.intER.get.stGuardDec(0).bits.trackGen := io.guardDecTrackGen
  rename.io.intER.get.stGuardDec(0).bits.oldPdest := io.guardDecOldPdest
  rename.io.intER.get.stGuardDec(0).bits.fallback := false.B
  rename.io.intER.get.stGuardDec(0).bits.reason := IntERFallbackReason.none

  for (i <- 0 until RenameWidth) {
    rename.io.in(i).valid := io.valid(i)
    rename.io.in(i).bits := 0.U.asTypeOf(new DecodeOutUop)
    rename.io.in(i).bits.srcType.foreach(_ := SrcType.no)
    rename.io.in(i).bits.srcType(0) := Mux(io.src0Xp(i), SrcType.xp, SrcType.no)
    rename.io.in(i).bits.srcType(1) := Mux(io.src1Xp(i), SrcType.xp, SrcType.no)
    rename.io.in(i).bits.lsrc.foreach(_ := 0.U)
    rename.io.in(i).bits.lsrc(0) := io.lsrc0(i)
    rename.io.in(i).bits.lsrc(1) := io.lsrc1(i)
    rename.io.in(i).bits.ldest := io.ldest(i)
    rename.io.in(i).bits.fuType := xiangshan.backend.fu.FuType.alu.U
    rename.io.in(i).bits.fuOpType := ALUOpType.add
    rename.io.in(i).bits.rfWen := io.rfWen(i)
    rename.io.in(i).bits.fpWen := false.B
    rename.io.in(i).bits.vecWen := false.B
    rename.io.in(i).bits.v0Wen := false.B
    rename.io.in(i).bits.vlWen := false.B
    rename.io.in(i).bits.canRobCompress := false.B
    rename.io.in(i).bits.selImm := SelImm.IMM_X
    rename.io.in(i).bits.isMove := io.isMove(i)
    rename.io.in(i).bits.firstUop := true.B
    rename.io.in(i).bits.lastUop := true.B
    rename.io.in(i).bits.numWB := 1.U
    rename.io.in(i).bits.isLastInFtqEntry := true.B

    for (s <- 0 until backendParams.numIntRegSrc) {
      rename.io.intReadPorts(i)(s).addr := (if (s == 0) io.lsrc0(i) else io.lsrc1(i))
      rename.io.intReadPorts(i)(s).hold := io.readHold(i)
    }
    rename.io.intOldDestReadPorts.get(i).addr := io.oldDestAddr(i)
    rename.io.intOldDestReadPorts.get(i).hold := io.readHold(i)

    for (s <- 0 until backendParams.numFpRegSrc) {
      rename.io.fpReadPorts(i)(s).addr := 0.U
      rename.io.fpReadPorts(i)(s).hold := io.readHold(i)
    }
    for (s <- 0 until backendParams.numVecRegSrc) {
      rename.io.vecReadPorts(i)(s).addr := 0.U
      rename.io.vecReadPorts(i)(s).hold := io.readHold(i)
    }
    rename.io.v0ReadPorts(i).addr := 0.U
    rename.io.v0ReadPorts(i).hold := io.readHold(i)
    rename.io.vlReadPorts(i).addr := 0.U
    rename.io.vlReadPorts(i).hold := io.readHold(i)

    rename.io.out(i).ready := io.outReady
    io.inReady(i) := rename.io.in(i).ready
    io.outValid(i) := rename.io.out(i).valid
    io.outFire(i) := rename.io.out(i).fire
    io.outFirstUop(i) := rename.io.out(i).bits.firstUop
    io.outLastUop(i) := rename.io.out(i).bits.lastUop
    io.outNumUops(i) := rename.io.out(i).bits.numUops
    io.outPdest(i) := rename.io.out(i).bits.pdest
    io.outPsrc0(i) := rename.io.out(i).bits.psrc(0)
    io.erDestValid(i) := rename.io.out(i).bits.intER.get.dest.valid
    io.erDestPdest(i) := rename.io.out(i).bits.intER.get.dest.pdest
    io.erRedefValid(i) := rename.io.out(i).bits.intER.get.redef.valid
    io.erRedefOldPdest(i) := rename.io.out(i).bits.intER.get.redef.oldPdest
    io.erSrc0Valid(i) := rename.io.out(i).bits.intER.get.src(0).valid
    io.erSrc0Psrc(i) := rename.io.out(i).bits.intER.get.src(0).psrc
  }

  io.debugActiveCount := rename.io.intERDebug.get.activeCount
  io.debugReadDoneDecCount := rename.io.intERDebug.get.readDoneDecCount
  io.debugProducerReadyCount := rename.io.intERDebug.get.producerReadyCount
  io.debugGuardDecCount := rename.io.intERDebug.get.guardDecCount
  io.debugEarlyFreeOpportunityCount := rename.io.intERDebug.get.earlyFreeOpportunityCount
  io.debugEarlyFreeCount := rename.io.intERDebug.get.earlyFreeCount
}

class IntEarlyReleaseBundlesTest extends AnyFlatSpec with Matchers with ChiselSim {
  behavior of "IntEarlyReleaseParams and IntEarlyReleaseBundles"

  private def sourceText(path: String): String = {
    val source = Source.fromFile(repoPath(path).toFile)
    try {
      source.mkString
    } finally {
      source.close()
    }
  }

  private def repoPath(path: String): Path = {
    val relative = Paths.get(path)
    Iterator.iterate(Paths.get("").toAbsolutePath)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve(relative))
      .find(Files.exists(_))
      .getOrElse(relative)
  }

  private def configWith(params: IntEarlyReleaseParams): Parameters = {
    configWith(params, fastSim = false)
  }

  private def configWith(params: IntEarlyReleaseParams, fastSim: Boolean): Parameters = {
    val defaultConfig = new DefaultConfig
    defaultConfig.alterPartial({
      case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy(
        intEarlyRelease = params
      )
    }).alter((site, here, up) => {
      case DebugOptionsKey if fastSim => up(DebugOptionsKey).copy(
        AlwaysBasicDiff = false,
        EnableDifftest = false,
        EnablePerfDebug = false,
        EnableDebug = false
      )
      case LogUtilsOptionsKey => LogUtilsOptions(
        here(DebugOptionsKey).EnableDebug,
        here(DebugOptionsKey).EnablePerfDebug,
        here(DebugOptionsKey).FPGAPlatform,
        here(DebugOptionsKey).EnableXMR
      )
      case PerfCounterOptionsKey => PerfCounterOptions(
        enablePerfPrint = here(DebugOptionsKey).EnablePerfDebug && !here(DebugOptionsKey).FPGAPlatform,
        enablePerfDB = here(DebugOptionsKey).EnableRollingDB && !here(DebugOptionsKey).FPGAPlatform,
        perfLevel = XSPerfLevel.withName(here(DebugOptionsKey).PerfLevel),
        perfDBHartID = 0
      )
    })
  }

  private def fastSimConfigWith(params: IntEarlyReleaseParams): Parameters = {
    configWith(params, fastSim = true)
  }

  private def intPreg128ConfigWith(params: IntEarlyReleaseParams): Parameters = {
    val defaultConfig = new DefaultConfig
    val baseTile = defaultConfig(XSTileKey).head
    defaultConfig.alterPartial({
      case XSCoreParamsKey => baseTile.copy(
        intPreg = IntPregParams(
          numEntries = 128,
          numBank = baseTile.intPreg.numBank,
          numRead = baseTile.intPreg.numRead,
          numWrite = baseTile.intPreg.numWrite
        ),
        intEarlyRelease = params
      )
    }).alter((site, here, up) => {
      case DebugOptionsKey => up(DebugOptionsKey).copy(
        AlwaysBasicDiff = false,
        EnableDifftest = false,
        EnablePerfDebug = false,
        EnableDebug = false
      )
      case LogUtilsOptionsKey => LogUtilsOptions(
        here(DebugOptionsKey).EnableDebug,
        here(DebugOptionsKey).EnablePerfDebug,
        here(DebugOptionsKey).FPGAPlatform,
        here(DebugOptionsKey).EnableXMR
      )
      case PerfCounterOptionsKey => PerfCounterOptions(
        enablePerfPrint = here(DebugOptionsKey).EnablePerfDebug && !here(DebugOptionsKey).FPGAPlatform,
        enablePerfDB = here(DebugOptionsKey).EnableRollingDB && !here(DebugOptionsKey).FPGAPlatform,
        perfLevel = XSPerfLevel.withName(here(DebugOptionsKey).PerfLevel),
        perfDBHartID = 0
      )
    })
  }

  private def smallRenameConfigWith(params: IntEarlyReleaseParams): Parameters = {
    val defaultConfig = new DefaultConfig
    defaultConfig.alterPartial({
      case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy(
        DecodeWidth = 2,
        RenameWidth = 2,
        CommitWidth = 2,
        RobCommitWidth = 2,
        RabCommitWidth = 2,
        RobSize = 32,
        RabSize = 32,
        RenameSnapshotNum = 2,
        VTypeBufferSize = 8,
        intPreg = IntPregParams(numEntries = 64, numBank = 1, numRead = None, numWrite = None),
        fpPreg = FpPregParams(numEntries = 64, numBank = 1, numRead = None, numWrite = None),
        vfPreg = VfPregParams(numEntries = 64, numBank = 1, numRead = None, numWrite = None),
        v0Preg = V0PregParams(numEntries = 8, numBank = 1, numRead = None, numWrite = None),
        vlPreg = VlPregParams(numEntries = 8, numBank = 1, numRead = None, numWrite = None),
        intEarlyRelease = params
      )
    }).alter((site, here, up) => {
      case DebugOptionsKey => up(DebugOptionsKey).copy(
        AlwaysBasicDiff = false,
        EnableDifftest = false,
        EnablePerfDebug = false,
        EnableDebug = false
      )
      case LogUtilsOptionsKey => LogUtilsOptions(
        here(DebugOptionsKey).EnableDebug,
        here(DebugOptionsKey).EnablePerfDebug,
        here(DebugOptionsKey).FPGAPlatform,
        here(DebugOptionsKey).EnableXMR
      )
      case PerfCounterOptionsKey => PerfCounterOptions(
        enablePerfPrint = false,
        enablePerfDB = false,
        perfLevel = XSPerfLevel.withName(here(DebugOptionsKey).PerfLevel),
        perfDBHartID = 0
      )
    })
  }

  private def elaborateProbe(params: IntEarlyReleaseParams, localSrc: Int, expectedTrackIdWidth: Int): Unit = {
    val config = configWith(params)
    val coreParams = config(XSCoreParamsKey)
    ChiselStage.elaborate(
      new IntEarlyReleaseBundleProbe(
        localSrc = localSrc,
        expectedTrackIdWidth = expectedTrackIdWidth,
        expectedLogicalSrcWidth = coreParams.backendParams.numSrc,
        expectedRenameWidth = coreParams.RenameWidth,
        expectedCommitWidth = coreParams.CommitWidth,
        expectedIntPhyRegs = coreParams.intPreg.numEntries,
        expectedRobSize = coreParams.RobSize,
        expectedEnable = params.enable,
        expectedObserveOnly = params.observeOnly
      )(config)
    )
  }

  private def elaborateOldDestRatProbe(
    params: IntEarlyReleaseParams,
    expectedOldDestPort: Boolean,
    expectedPortsPerLane: Int
  ): Unit = {
    val config = smallRenameConfigWith(params)
    ChiselStage.elaborate(new DecodeOldDestRatPortShapeProbe(expectedOldDestPort)(config))
    ChiselStage.elaborate(new IntRatReadPortCountProbe(expectedPortsPerLane)(config))
    ChiselStage.elaborate(new RenameTableWrapperOldDestPortShapeProbe(expectedOldDestPort)(config))
  }

  private def elaborateRenameOutUopERMetaProbe(params: IntEarlyReleaseParams, expectedERMeta: Boolean): Unit = {
    ChiselStage.elaborate(new RenameOutUopERMetaShapeProbe(expectedERMeta)(configWith(params)))
  }

  private def elaborateDownstreamPayloadShapeProbe(params: IntEarlyReleaseParams, expectedERMeta: Boolean): Unit = {
    ChiselStage.elaborate(new IntERDownstreamPayloadShapeProbe(expectedERMeta)(configWith(params)))
  }

  private def elaborateRobMetadataShapeProbe(params: IntEarlyReleaseParams, expectedERMeta: Boolean): Unit = {
    ChiselStage.elaborate(new IntERRobMetadataShapeProbe(expectedERMeta)(configWith(params)))
  }

  private def elaborateAllLocalPayloadShapeProbe(params: IntEarlyReleaseParams, expectedERMeta: Boolean): Unit = {
    ChiselStage.elaborate(new IntERAllLocalPayloadShapeProbe(expectedERMeta)(configWith(params)))
  }

  private def elaborateIssueQueueOwnershipProbe(params: IntEarlyReleaseParams, expectedERMeta: Boolean): Unit = {
    ChiselStage.elaborate(new IntERIssueQueueOwnershipProbe(expectedERMeta)(configWith(params)))
  }

  private def checkIssueEntryInitialization(params: IntEarlyReleaseParams): Unit = {
    simulate(new IntERIssueEntryInitializationProbe()(configWith(params))) { _ => }
  }

  private def elaborateRenameERDebugProbe(params: IntEarlyReleaseParams, expectedDebug: Boolean): Unit = {
    ChiselStage.elaborate(new RenameERDebugShapeProbe(expectedDebug)(smallRenameConfigWith(params)))
  }

  private def setRobPtr(ptr: xiangshan.backend.rob.RobPtr, value: Int): Unit = {
    ptr.flag.poke(false.B)
    ptr.value.poke(value.U)
  }

  private def clearPolicyProbe(dut: RenameERPolicyProbe): Unit = {
    dut.io.renameFire.poke(false.B)
    dut.io.redirect.poke(false.B)
    dut.io.rabWalk.poke(false.B)
    dut.io.singleUop.poke(true.B)
    dut.io.numUops.poke(1.U)
    dut.io.hasException.poke(false.B)
    dut.io.flushPipe.poke(false.B)
    dut.io.singleStep.poke(false.B)
    dut.io.isMove.poke(false.B)
    dut.io.needIntDest.poke(false.B)
    dut.io.ldest.poke(0.U)
    dut.io.sourceValid.poke(false.B)
    dut.io.sourcePsrc.poke(0.U)
    dut.io.pdest.poke(0.U)
    dut.io.oldPdest.poke(0.U)
    setRobPtr(dut.io.robIdx, 0)
  }

  private def resetPolicyProbe(dut: RenameERPolicyProbe): Unit = {
    clearPolicyProbe(dut)
    dut.reset.poke(true.B)
    dut.clock.step()
    dut.reset.poke(false.B)
    dut.clock.step()
    clearPolicyProbe(dut)
  }

  private def allocatePolicyProbe(dut: RenameERPolicyProbe, pdest: Int): Unit = {
    dut.io.renameFire.poke(true.B)
    dut.io.singleUop.poke(true.B)
    dut.io.numUops.poke(1.U)
    dut.io.needIntDest.poke(true.B)
    dut.io.ldest.poke(5.U)
    dut.io.pdest.poke(pdest.U)
    dut.io.oldPdest.poke(2.U)
    setRobPtr(dut.io.robIdx, 1)
  }

  private def addi(rd: Int, rs1: Int, imm: Int = 0): BigInt = {
    val imm12 = imm & 0xfff
    BigInt((imm12 << 20) | (rs1 << 15) | (0 << 12) | (rd << 7) | 0x13)
  }

  private def clearRenameProbe(dut: RenameERFullPathProbe): Unit = {
    dut.io.outReady.poke(true.B)
    dut.io.redirect.poke(false.B)
    dut.io.rabWalk.poke(false.B)
    dut.io.externalRedirectKill.poke(false.B)
    dut.io.producerReadyValid.poke(false.B)
    dut.io.producerReadyPdest.poke(0.U)
    dut.io.producerReadyRobIdx.poke(0.U)
    dut.io.readDoneValid.poke(false.B)
    dut.io.readDoneRobIdx.poke(0.U)
    dut.io.readDoneTrackId.poke(0.U)
    dut.io.readDoneTrackGen.poke(0.U)
    dut.io.readDoneSrcIdx.poke(0.U)
    dut.io.readDonePsrc.poke(0.U)
    dut.io.guardDecValid.poke(false.B)
    dut.io.guardDecRobIdx.poke(0.U)
    dut.io.guardDecTrackId.poke(0.U)
    dut.io.guardDecTrackGen.poke(0.U)
    dut.io.guardDecOldPdest.poke(0.U)
    for (i <- 0 until dut.io.valid.length) {
      dut.io.valid(i).poke(false.B)
      dut.io.readHold(i).poke(false.B)
      dut.io.ldest(i).poke(0.U)
      dut.io.lsrc0(i).poke(0.U)
      dut.io.lsrc1(i).poke(0.U)
      dut.io.oldDestAddr(i).poke(0.U)
      dut.io.src0Xp(i).poke(false.B)
      dut.io.src1Xp(i).poke(false.B)
      dut.io.rfWen(i).poke(false.B)
      dut.io.isMove(i).poke(false.B)
    }
  }

  private def resetRenameProbe(dut: RenameERFullPathProbe): Unit = {
    clearRenameProbe(dut)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    dut.clock.step(3)
    clearRenameProbe(dut)
  }

  private def primeRenameRead(
    dut: RenameERFullPathProbe,
    lane: Int,
    ldest: Int,
    lsrc0: Int = 0,
    lsrc1: Int = 0
  ): Unit = {
    primeRenameReads(dut, Seq((lane, ldest, lsrc0, lsrc1)))
  }

  private def primeRenameReads(
    dut: RenameERFullPathProbe,
    lanes: Seq[(Int, Int, Int, Int)]
  ): Unit = {
    clearRenameProbe(dut)
    lanes.foreach { case (lane, ldest, lsrc0, lsrc1) =>
      dut.io.ldest(lane).poke(ldest.U)
      dut.io.lsrc0(lane).poke(lsrc0.U)
      dut.io.lsrc1(lane).poke(lsrc1.U)
      dut.io.oldDestAddr(lane).poke(ldest.U)
    }
    dut.clock.step()
  }

  private def driveRenameLane(
    dut: RenameERFullPathProbe,
    lane: Int,
    ldest: Int,
    lsrc0: Int = 0,
    lsrc1: Int = 0,
    src0Xp: Boolean = false,
    src1Xp: Boolean = false,
    rfWen: Boolean = true,
    isMove: Boolean = false
  ): Unit = {
    dut.io.valid(lane).poke(true.B)
    dut.io.ldest(lane).poke(ldest.U)
    dut.io.lsrc0(lane).poke(lsrc0.U)
    dut.io.lsrc1(lane).poke(lsrc1.U)
    dut.io.oldDestAddr(lane).poke(ldest.U)
    dut.io.src0Xp(lane).poke(src0Xp.B)
    dut.io.src1Xp(lane).poke(src1Xp.B)
    dut.io.rfWen(lane).poke(rfWen.B)
    dut.io.isMove(lane).poke(isMove.B)
  }

  private def fireSingleRenameLane(
    dut: RenameERFullPathProbe,
    lane: Int,
    ldest: Int,
    lsrc0: Int = 0,
    lsrc1: Int = 0,
    src0Xp: Boolean = false,
    src1Xp: Boolean = false,
    rfWen: Boolean = true,
    isMove: Boolean = false
  ): BigInt = {
    driveRenameLane(dut, lane, ldest, lsrc0, lsrc1, src0Xp, src1Xp, rfWen, isMove)
    dut.io.outValid(lane).expect(true.B)
    dut.io.outFire(lane).expect(true.B)
    dut.io.outFirstUop(lane).expect(true.B)
    dut.io.outLastUop(lane).expect(true.B)
    dut.io.outNumUops(lane).expect(1.U)
    val pdest = dut.io.outPdest(lane).peek().litValue
    dut.clock.step()
    clearRenameProbe(dut)
    pdest
  }

  private def writeDecodeRatMapping(dut: DecodeOldDestHoldProbe, ldest: Int, pdest: Int): Unit = {
    dut.io.inValid.poke(false.B)
    dut.io.outReady.poke(true.B)
    dut.io.instr.poke(addi(0, 0).U)
    dut.io.writeValid.poke(true.B)
    dut.io.writeAddr.poke(ldest.U)
    dut.io.writeData.poke(pdest.U)
    dut.clock.step()
    dut.io.writeValid.poke(false.B)
    dut.clock.step()
  }

  private def checkFullRenameOldDestIdentity(dut: RenameERFullPathProbe): Unit = {
    resetRenameProbe(dut)

    primeRenameRead(dut, lane = 0, ldest = 6)
    val sourcePdest = fireSingleRenameLane(dut, lane = 0, ldest = 6)
    sourcePdest should not be BigInt(0)

    primeRenameRead(dut, lane = 0, ldest = 5)
    val oldDestPdest = fireSingleRenameLane(dut, lane = 0, ldest = 5)
    oldDestPdest should not be BigInt(0)
    oldDestPdest should not be sourcePdest

    primeRenameRead(dut, lane = 0, ldest = 5, lsrc0 = 6)
    driveRenameLane(dut, lane = 0, ldest = 5, lsrc0 = 6, src0Xp = true)

    dut.io.outPsrc0(0).expect(sourcePdest.U)
    dut.io.erRedefValid(0).expect(true.B)
    dut.io.erRedefOldPdest(0).expect(oldDestPdest.U)
    dut.io.erRedefOldPdest(0).peek().litValue should not be dut.io.outPsrc0(0).peek().litValue
  }

  private def checkFullRenameSameGroupRedef(dut: RenameERFullPathProbe): Unit = {
    resetRenameProbe(dut)

    primeRenameReads(dut, Seq((0, 7, 0, 0), (1, 7, 0, 0)))
    driveRenameLane(dut, lane = 0, ldest = 7)
    driveRenameLane(dut, lane = 1, ldest = 7)

    val olderPdest = dut.io.outPdest(0).peek().litValue
    val youngerPdest = dut.io.outPdest(1).peek().litValue
    olderPdest should not be BigInt(0)
    youngerPdest should not be BigInt(0)
    youngerPdest should not be olderPdest

    dut.io.erDestValid(0).expect(true.B)
    dut.io.erRedefValid(0).expect(false.B)
    dut.io.erDestValid(1).expect(true.B)
    dut.io.erRedefValid(1).expect(true.B)
    dut.io.erRedefOldPdest(1).expect(olderPdest.U)
  }

  private def checkFullRenameMoveThenRedef(dut: RenameERFullPathProbe): Unit = {
    resetRenameProbe(dut)

    primeRenameRead(dut, lane = 0, ldest = 8)
    val moveSourcePdest = fireSingleRenameLane(dut, lane = 0, ldest = 8)
    moveSourcePdest should not be BigInt(0)

    primeRenameReads(dut, Seq((0, 9, 8, 0), (1, 9, 0, 0)))
    driveRenameLane(dut, lane = 0, ldest = 9, lsrc0 = 8, src0Xp = true, isMove = true)
    driveRenameLane(dut, lane = 1, ldest = 9)

    dut.io.outPsrc0(0).expect(moveSourcePdest.U)
    dut.io.outPdest(0).expect(moveSourcePdest.U)
    dut.io.erDestValid(0).expect(false.B)
    dut.io.erRedefValid(0).expect(false.B)
    dut.io.erRedefValid(1).expect(true.B)
    dut.io.erRedefOldPdest(1).expect(moveSourcePdest.U)
  }

  private def checkFullRenameHeldOldDestRead(dut: RenameERFullPathProbe): Unit = {
    resetRenameProbe(dut)

    primeRenameRead(dut, lane = 0, ldest = 5)
    val heldOldDestPdest = fireSingleRenameLane(dut, lane = 0, ldest = 5)
    heldOldDestPdest should not be BigInt(0)

    primeRenameRead(dut, lane = 0, ldest = 9)
    val laterOldDestPdest = fireSingleRenameLane(dut, lane = 0, ldest = 9)
    laterOldDestPdest should not be BigInt(0)
    laterOldDestPdest should not be heldOldDestPdest

    clearRenameProbe(dut)
    dut.io.ldest(0).poke(5.U)
    dut.io.oldDestAddr(0).poke(5.U)
    dut.clock.step()

    dut.io.readHold(0).poke(true.B)
    dut.io.oldDestAddr(0).poke(9.U)
    driveRenameLane(dut, lane = 0, ldest = 5)

    dut.io.erRedefValid(0).expect(true.B)
    dut.io.erRedefOldPdest(0).expect(heldOldDestPdest.U)
    dut.io.erRedefOldPdest(0).peek().litValue should not be laterOldDestPdest
  }

  it should "reject illegal trackEntries values" in {
    assertThrows[IllegalArgumentException] {
      IntEarlyReleaseParams(trackEntries = 0)
    }
    assertThrows[IllegalArgumentException] {
      IntEarlyReleaseParams(trackEntries = 3)
    }
  }

  it should "elaborate legal parameterized trackEntries values" in {
    Seq(
      IntEarlyReleaseParams(trackEntries = 1) -> 1,
      IntEarlyReleaseParams(trackEntries = 2) -> 1,
      IntEarlyReleaseParams(trackEntries = 16) -> 4,
      IntEarlyReleaseParams(trackEntries = 64) -> 6
    ).foreach { case (params, expectedTrackIdWidth) =>
      elaborateProbe(params, localSrc = 1, expectedTrackIdWidth)
    }
  }

  it should "distinguish full logical and local source vector widths" in {
    val params = IntEarlyReleaseParams(trackEntries = 2)

    Seq(1, 2).foreach { localSrc =>
      elaborateProbe(params, localSrc, expectedTrackIdWidth = 1)
    }
  }

  it should "keep IntEarlyReleaseParams default disabled when explicitly selected" in {
    elaborateProbe(IntEarlyReleaseParams(), localSrc = 1, expectedTrackIdWidth = 4)
  }

  it should "keep baseline configs disabled and enable functional Int ER only in explicit configs" in {
    val defaultParams = (new DefaultConfig)(XSTileKey).head.intEarlyRelease
    val mediumParams = (new MediumConfig)(XSTileKey).head.intEarlyRelease
    val functionalParams = (new IntERFunctionalConfig)(XSTileKey).head.intEarlyRelease
    val minimalFunctionalParams = (new IntERFunctionalMinimalConfig)(XSTileKey).head.intEarlyRelease

    defaultParams.enable shouldBe false
    mediumParams.enable shouldBe false

    functionalParams.enable shouldBe true
    functionalParams.observeOnly shouldBe false
    functionalParams.trackEntries shouldBe 128
    functionalParams.earlyFreeWidth shouldBe 8

    minimalFunctionalParams.enable shouldBe true
    minimalFunctionalParams.observeOnly shouldBe false
    minimalFunctionalParams.trackEntries shouldBe 128
    minimalFunctionalParams.earlyFreeWidth shouldBe 8
  }

  it should "not expose unimplemented Int ER configuration knobs" in {
    val paramsSource = sourceText("src/main/scala/xiangshan/Parameters.scala")

    Seq(
      "readDoneQueueDepth",
      "eventQueueDepth",
      "allowNonIntSchedulerConsumers",
      "conservativeRedirectKill",
      "IntERReadDoneQueueDepth",
      "IntEREventQueueDepth",
      "IntERAllowNonIntSchedulerConsumers",
      "IntERConservativeRedirectKill"
    ).foreach { removedName =>
      paramsSource should not include removedName
    }
  }

  it should "define a correctness-first functional ER minimal config" in {
    val params = (new IntERFunctionalMinimalConfig)(XSTileKey).head.intEarlyRelease

    params.enable shouldBe true
    params.observeOnly shouldBe false
    params.trackEntries shouldBe 128
    params.earlyFreeWidth shouldBe 8
  }

  it should "gate old-destination integer RAT read ports with feature enable" in {
    elaborateOldDestRatProbe(
      IntEarlyReleaseParams(),
      expectedOldDestPort = false,
      expectedPortsPerLane = 2
    )
    elaborateOldDestRatProbe(
      IntEarlyReleaseParams(enable = true, trackEntries = 2),
      expectedOldDestPort = true,
      expectedPortsPerLane = 3
    )
  }

  it should "gate rename output ER metadata with feature enable" in {
    elaborateRenameOutUopERMetaProbe(IntEarlyReleaseParams(), expectedERMeta = false)
    elaborateRenameOutUopERMetaProbe(IntEarlyReleaseParams(enable = true, trackEntries = 2), expectedERMeta = true)
  }

  it should "gate downstream ER metadata carriers with feature enable and local widths" in {
    elaborateDownstreamPayloadShapeProbe(IntEarlyReleaseParams(), expectedERMeta = false)
    elaborateDownstreamPayloadShapeProbe(IntEarlyReleaseParams(enable = true, trackEntries = 2), expectedERMeta = true)
  }

  it should "gate ROB and RAB ER metadata storage with feature enable" in {
    elaborateRobMetadataShapeProbe(IntEarlyReleaseParams(), expectedERMeta = false)
    elaborateRobMetadataShapeProbe(IntEarlyReleaseParams(enable = true, trackEntries = 2), expectedERMeta = true)
  }

  it should "check all scheduler local ER metadata payload widths" in {
    elaborateAllLocalPayloadShapeProbe(IntEarlyReleaseParams(), expectedERMeta = false)
    elaborateAllLocalPayloadShapeProbe(IntEarlyReleaseParams(enable = true, trackEntries = 2), expectedERMeta = true)
  }

  it should "keep IssueQueue ER ownership to metadata carriers" in {
    elaborateIssueQueueOwnershipProbe(IntEarlyReleaseParams(), expectedERMeta = false)
    elaborateIssueQueueOwnershipProbe(IntEarlyReleaseParams(enable = true, trackEntries = 2), expectedERMeta = true)
  }

  it should "fully initialize IssueQueue ER metadata across entry updates" in {
    checkIssueEntryInitialization(IntEarlyReleaseParams(enable = true, trackEntries = 2))
  }

  it should "propagate local ER source metadata through dispatch and issue payload carriers" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2))

    simulate(new IntERDownstreamPropagationProbe()(config)) { dut =>
      dut.io.inValid.poke(true.B)
      dut.io.inTrackId.poke(1.U)
      dut.io.inTrackGen.poke(1.U)
      dut.io.inSrcIdx.poke(2.U)
      dut.io.inPsrc.poke(19.U)

      dut.io.dispatchValid.expect(true.B)
      dut.io.regionValid.expect(true.B)
      dut.io.payloadValid.expect(true.B)
      dut.io.statusValid.expect(true.B)
      dut.io.deqValid.expect(true.B)
      dut.io.deqTrackId.expect(1.U)
      dut.io.deqTrackGen.expect(1.U)
      dut.io.deqSrcIdx.expect(2.U)
      dut.io.deqPsrc.expect(19.U)
    }
  }

  it should "copy STA source one ER metadata to STD source zero preserving original source index" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2))

    simulate(new IntERStaToStdCopyProbe()(config)) { dut =>
      dut.io.inValid.poke(true.B)
      dut.io.inTrackId.poke(1.U)
      dut.io.inTrackGen.poke(1.U)
      dut.io.inPsrc.poke(23.U)

      dut.io.outValid.expect(true.B)
      dut.io.outTrackId.expect(1.U)
      dut.io.outTrackGen.expect(1.U)
      dut.io.outSrcIdx.expect(1.U)
      dut.io.outPsrc.expect(23.U)
      dut.io.deqValid.expect(true.B)
      dut.io.deqTrackId.expect(1.U)
      dut.io.deqTrackGen.expect(1.U)
      dut.io.deqSrcIdx.expect(1.U)
      dut.io.deqPsrc.expect(23.U)
    }
  }

  it should "select integer RF bank using the active 128-entry integer physical address width" in {
    val config = intPreg128ConfigWith(IntEarlyReleaseParams(enable = true, trackEntries = 2))

    simulate(new IntERIntRfBankSelectProbe()(config)) { dut =>
      dut.io.psrc.poke(47.U)

      dut.io.bankRen(0).expect(false.B)
      dut.io.bankRen(1).expect(true.B)
      dut.io.bankRen(2).expect(false.B)
      dut.io.bankRen(3).expect(false.B)
    }
  }

  it should "store ROB ER metadata from enqueue through commit entry conversion" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2))

    simulate(new IntERRobMetadataPropagationProbe()(config)) { dut =>
      dut.io.inSrcValid.poke(true.B)
      dut.io.inTrackId.poke(1.U)
      dut.io.inTrackGen.poke(3.U)
      dut.io.inSrcIdx.poke(1.U)
      dut.io.inPsrc.poke(21.U)
      dut.io.inDestValid.poke(true.B)
      dut.io.inPdest.poke(25.U)
      dut.io.inRedefValid.poke(true.B)
      dut.io.inOldPdest.poke(17.U)
      dut.io.inMoveSrcLReg.poke(6.U)

      dut.io.entrySrcValid.expect(true.B)
      dut.io.entrySrcReadDone.expect(false.B)
      dut.io.entryTrackId.expect(1.U)
      dut.io.entryTrackGen.expect(3.U)
      dut.io.entrySrcIdx.expect(1.U)
      dut.io.entryPsrc.expect(21.U)
      dut.io.entryDestValid.expect(true.B)
      dut.io.entryPdest.expect(25.U)
      dut.io.entryRedefValid.expect(true.B)
      dut.io.entryOldPdest.expect(17.U)
      dut.io.entryMoveSrcLReg.expect(6.U)

      dut.io.commitSrcValid.expect(true.B)
      dut.io.commitSrcReadDone.expect(false.B)
      dut.io.commitTrackId.expect(1.U)
      dut.io.commitTrackGen.expect(3.U)
      dut.io.commitSrcIdx.expect(1.U)
      dut.io.commitPsrc.expect(21.U)
      dut.io.commitDestValid.expect(true.B)
      dut.io.commitPdest.expect(25.U)
      dut.io.commitRedefValid.expect(true.B)
      dut.io.commitOldPdest.expect(17.U)
      dut.io.commitMoveSrcLReg.expect(6.U)
      dut.clock.step()
    }
  }

  it should "gate rename ER debug output with feature enable" in {
    elaborateRenameERDebugProbe(IntEarlyReleaseParams(), expectedDebug = false)
    elaborateRenameERDebugProbe(IntEarlyReleaseParams(enable = true, trackEntries = 2), expectedDebug = true)
  }

  it should "expose UCA saturated fallback through a stable Rename perf counter name" in {
    val renameSource = sourceText("src/main/scala/xiangshan/backend/rename/Rename.scala")

    renameSource should include("XSPerfAccumulate(\"int_er_uc_saturated_fallback\"")
    renameSource should include("intUCA.io.debug.saturatedFallbackCount")
  }

  it should "expose stable UCA event perf counter names" in {
    val renameSource = sourceText("src/main/scala/xiangshan/backend/rename/Rename.scala")

    Seq(
      "int_er_uc_producer_ready" -> "producerReadyCount",
      "int_er_uc_read_done_dec" -> "readDoneDecCount",
      "int_er_uc_squash_dec" -> "squashDecCount",
      "int_er_uc_guard_dec" -> "guardDecCount",
      "int_er_uc_early_free_opportunity" -> "earlyFreeOpportunityCount",
      "int_er_uc_early_free" -> "earlyFreeCount",
      "int_er_uc_commit_suppress" -> "commitSuppressCount",
      "int_er_uc_commit_identity_mismatch" -> "commitIdentityMismatchCount",
      "int_er_uc_gen_mismatch" -> "genMismatchCount",
      "int_er_uc_redirect_kill" -> "redirectKillCount",
      "int_er_uc_full_untracked" -> "fullUntrackedCount",
      "int_er_uc_source_duplicate" -> "sourceDuplicateCount"
    ).foreach { case (counterName, debugField) =>
      renameSource should include {
        s"""XSPerfAccumulate("$counterName", intERDebugDelta(intUCA.io.debug.$debugField))"""
      }
    }
    renameSource should include("def intERDebugDelta")
    renameSource should include("intUCA.io.debug.producerReadyCount")
    renameSource should include("intUCA.io.debug.genMismatchCount")
  }

  it should "expose stable UCA occupancy perf counter names" in {
    val renameSource = sourceText("src/main/scala/xiangshan/backend/rename/Rename.scala")

    Seq(
      "int_er_uc_active_count_sum",
      "int_er_uc_full_cycle",
      "int_er_uc_high_occupancy_cycle",
      "int_er_uc_counting_count_sum",
      "int_er_uc_fallback_wait_count_sum",
      "int_er_uc_released_wait_count_sum"
    ).foreach { counterName =>
      renameSource should include(s"""XSPerfAccumulate("$counterName"""")
    }
    renameSource should include("IntEREntryState.counting")
    renameSource should include("IntEREntryState.fallbackWaitCommit")
    renameSource should include("IntEREntryState.releasedWaitCommit")
  }

  it should "expose Rename fallback reason perf counters with stable names" in {
    val renameSource = sourceText("src/main/scala/xiangshan/backend/rename/Rename.scala")

    renameSource should include("XSPerfAccumulate(\"int_er_rename_fallback_move\"")
    renameSource should include("XSPerfAccumulate(\"int_er_rename_fallback_unsupported_consumer\"")
    renameSource should include("XSPerfAccumulate(\"int_er_rename_fallback_store_data_consumer\"")
  }

  it should "expose stable Rename integer free-list pressure perf counter names outside the IntER block" in {
    val renameSource = sourceText("src/main/scala/xiangshan/backend/rename/Rename.scala")

    Seq(
      "int_er_rename_int_alloc_req",
      "int_er_rename_int_alloc_fire",
      "int_er_rename_int_freelist_stall_cycle",
      "int_er_rename_int_freelist_stall_uops",
      "int_er_rename_int_freelist_not_can_allocate",
      "int_er_rename_int_alloc_pressure"
    ).foreach { counterName =>
      renameSource should include(s"""XSPerfAccumulate("$counterName"""")
    }

    val stallDefinition = renameSource.indexOf("private val stallForIntFL")
    val pressureCounter = renameSource.indexOf("XSPerfAccumulate(\"int_er_rename_int_freelist_stall_cycle\"")
    stallDefinition should be >= 0
    pressureCounter should be > stallDefinition
    renameSource should include("private val noRedirectOrWalk = !io.redirect.valid && !io.rabCommits.isWalk")
    renameSource should include("private val stallForIntFL     = inHeadValid && noRedirectOrWalk")
    renameSource should include("noRedirectOrWalk && intFreeList.io.allocateReq(i) && io.out(i).fire")
  }

  it should "select same-group old destination from older final integer RAT writes" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2))

    simulate(new RenameOldDestBypassProbe()(config)) { dut =>
      for (i <- 0 until dut.io.base.length) {
        dut.io.base(i).poke((10 + i).U)
        dut.io.ldest(i).poke((i + 1).U)
        dut.io.wen(i).poke(false.B)
        dut.io.data(i).poke((40 + i).U)
      }

      dut.io.ldest(0).poke(5.U)
      dut.io.wen(0).poke(true.B)
      dut.io.data(0).poke(21.U)
      dut.io.ldest(1).poke(6.U)
      dut.io.wen(1).poke(true.B)
      dut.io.data(1).poke(31.U)
      dut.io.ldest(2).poke(5.U)
      dut.io.wen(2).poke(true.B)
      dut.io.data(2).poke(41.U)
      dut.io.ldest(3).poke(5.U)

      dut.io.out(0).expect(10.U)
      dut.io.out(1).expect(11.U)
      dut.io.out(2).expect(21.U)
      dut.io.out(3).expect(41.U)
    }
  }

  it should "fallback unsupported rename consumers instead of counting source metadata" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2))

    simulate(new RenameERPolicyProbe()(config)) { dut =>
      resetPolicyProbe(dut)

      allocatePolicyProbe(dut, pdest = 7)
      dut.io.producerEligible.expect(true.B)
      dut.io.destValid.expect(true.B)
      dut.clock.step()
      clearPolicyProbe(dut)
      dut.io.debugActiveCount.expect(1.U)

      dut.io.renameFire.poke(true.B)
      dut.io.singleUop.poke(false.B)
      dut.io.sourceValid.poke(true.B)
      dut.io.sourcePsrc.poke(7.U)
      setRobPtr(dut.io.robIdx, 2)
      dut.io.sourceProbeValid.expect(true.B)
      dut.io.sourceFallback.expect(true.B)
      dut.io.srcValid.expect(false.B)
      dut.io.fallbackMark.expect(true.B)
      dut.clock.step()
      clearPolicyProbe(dut)

      dut.io.debugActiveCount.expect(1.U)
      dut.io.debugFallbackCount.expect(1.U)
    }
  }

  it should "treat first-and-last uops with numUops greater than one as unsupported for ER tracking" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2))

    simulate(new RenameERPolicyProbe()(config)) { dut =>
      resetPolicyProbe(dut)

      allocatePolicyProbe(dut, pdest = 7)
      dut.clock.step()
      clearPolicyProbe(dut)
      dut.io.debugActiveCount.expect(1.U)

      dut.io.renameFire.poke(true.B)
      dut.io.singleUop.poke(true.B)
      dut.io.numUops.poke(2.U)
      dut.io.needIntDest.poke(true.B)
      dut.io.ldest.poke(6.U)
      dut.io.sourceValid.poke(true.B)
      dut.io.sourcePsrc.poke(7.U)
      dut.io.pdest.poke(8.U)
      dut.io.oldPdest.poke(7.U)
      setRobPtr(dut.io.robIdx, 2)

      dut.io.sourceProbeValid.expect(true.B)
      dut.io.sourceFallback.expect(true.B)
      dut.io.producerEligible.expect(false.B)
      dut.io.srcValid.expect(false.B)
      dut.io.destValid.expect(false.B)
      dut.io.redefValid.expect(true.B)
      dut.io.redefOldPdest.expect(7.U)
      dut.io.fallbackMark.expect(true.B)
      dut.clock.step()
      clearPolicyProbe(dut)

      dut.io.debugFallbackCount.expect(1.U)
    }
  }

  it should "ignore non-register and zero integer sources before ER source matching" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2))

    simulate(new RenameERPolicyProbe()(config)) { dut =>
      resetPolicyProbe(dut)

      allocatePolicyProbe(dut, pdest = 7)
      dut.clock.step()
      clearPolicyProbe(dut)
      dut.io.debugActiveCount.expect(1.U)

      dut.io.renameFire.poke(true.B)
      dut.io.sourceValid.poke(false.B)
      dut.io.sourcePsrc.poke(7.U)
      setRobPtr(dut.io.robIdx, 2)
      dut.io.sourceProbeValid.expect(false.B)
      dut.io.sourceFallback.expect(false.B)
      dut.io.srcValid.expect(false.B)
      dut.io.fallbackMark.expect(false.B)
      dut.clock.step()
      clearPolicyProbe(dut)

      dut.io.debugActiveCount.expect(1.U)
      dut.io.debugFallbackCount.expect(0.U)

      dut.io.renameFire.poke(true.B)
      dut.io.sourceValid.poke(true.B)
      dut.io.sourcePsrc.poke(0.U)
      setRobPtr(dut.io.robIdx, 3)
      dut.io.sourceProbeValid.expect(false.B)
      dut.io.sourceFallback.expect(false.B)
      dut.io.srcValid.expect(false.B)
      dut.io.fallbackMark.expect(false.B)
      dut.clock.step()
      clearPolicyProbe(dut)

      dut.io.debugActiveCount.expect(1.U)
      dut.io.debugFallbackCount.expect(0.U)
    }
  }

  it should "fallback move consumers that read a tracked integer source" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2))

    simulate(new RenameERPolicyProbe()(config)) { dut =>
      resetPolicyProbe(dut)

      allocatePolicyProbe(dut, pdest = 7)
      dut.clock.step()
      clearPolicyProbe(dut)
      dut.io.debugActiveCount.expect(1.U)

      dut.io.renameFire.poke(true.B)
      dut.io.singleUop.poke(true.B)
      dut.io.isMove.poke(true.B)
      dut.io.needIntDest.poke(true.B)
      dut.io.ldest.poke(5.U)
      dut.io.sourceValid.poke(true.B)
      dut.io.sourcePsrc.poke(7.U)
      dut.io.pdest.poke(7.U)
      setRobPtr(dut.io.robIdx, 2)
      dut.io.sourceProbeValid.expect(true.B)
      dut.io.sourceFallback.expect(true.B)
      dut.io.producerEligible.expect(false.B)
      dut.io.srcValid.expect(false.B)
      dut.io.fallbackMark.expect(true.B)
      dut.clock.step()
      clearPolicyProbe(dut)

      dut.io.debugActiveCount.expect(1.U)
      dut.io.debugFallbackCount.expect(1.U)
    }
  }

  it should "fallback scalar store-data consumers instead of tracking them through STD" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2))

    simulate(new RenameERStoreDataPolicyProbe()(config)) { dut =>
      def clear(): Unit = {
        dut.io.renameFire.poke(false.B)
        dut.io.allocProducer.poke(false.B)
        dut.io.singleUop.poke(true.B)
        dut.io.isScalarStore.poke(false.B)
        dut.io.source1Valid.poke(false.B)
        dut.io.source1Psrc.poke(0.U)
        dut.io.pdest.poke(0.U)
        setRobPtr(dut.io.robIdx, 0)
      }

      clear()
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()
      clear()

      dut.io.allocProducer.poke(true.B)
      dut.io.pdest.poke(7.U)
      setRobPtr(dut.io.robIdx, 1)
      dut.io.destValid.expect(true.B)
      dut.clock.step()
      clear()

      dut.io.renameFire.poke(true.B)
      dut.io.singleUop.poke(true.B)
      dut.io.isScalarStore.poke(true.B)
      dut.io.source1Valid.poke(true.B)
      dut.io.source1Psrc.poke(7.U)
      setRobPtr(dut.io.robIdx, 2)

      dut.io.sourceFallback.expect(true.B)
      dut.io.src1Valid.expect(false.B)
      dut.io.fallbackMark.expect(true.B)
      dut.clock.step()
      clear()

      dut.io.debugFallbackCount.expect(1.U)
    }
  }

  it should "mark redef for ineligible integer writers and fallback the tracked old destination" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2))

    simulate(new RenameERPolicyProbe()(config)) { dut =>
      resetPolicyProbe(dut)

      allocatePolicyProbe(dut, pdest = 7)
      dut.io.producerEligible.expect(true.B)
      dut.io.destValid.expect(true.B)
      dut.clock.step()
      clearPolicyProbe(dut)
      dut.io.debugActiveCount.expect(1.U)

      dut.io.renameFire.poke(true.B)
      dut.io.singleUop.poke(false.B)
      dut.io.needIntDest.poke(true.B)
      dut.io.ldest.poke(5.U)
      dut.io.oldPdest.poke(7.U)
      setRobPtr(dut.io.robIdx, 2)
      dut.io.producerEligible.expect(false.B)
      dut.io.redefProbeValid.expect(true.B)
      dut.io.destValid.expect(false.B)
      dut.io.redefValid.expect(true.B)
      dut.io.redefOldPdest.expect(7.U)
      dut.clock.step()
      clearPolicyProbe(dut)

      dut.io.debugActiveCount.expect(1.U)
      dut.io.debugFallbackCount.expect(1.U)
    }
  }

  it should "block redirect and walk from rename-side ER source events" in {
    val config = configWith(IntEarlyReleaseParams(enable = true, trackEntries = 2))

    Seq(true, false).foreach { useRedirect =>
      simulate(new RenameERPolicyProbe()(config)) { dut =>
        resetPolicyProbe(dut)

        allocatePolicyProbe(dut, pdest = 9)
        dut.clock.step()
        clearPolicyProbe(dut)
        dut.io.debugActiveCount.expect(1.U)

        dut.io.renameFire.poke(true.B)
        dut.io.redirect.poke(useRedirect.B)
        dut.io.rabWalk.poke((!useRedirect).B)
        dut.io.sourceValid.poke(true.B)
        dut.io.sourcePsrc.poke(9.U)
        setRobPtr(dut.io.robIdx, 3)
        dut.io.sourceProbeValid.expect(false.B)
        dut.io.sourceFallback.expect(false.B)
        dut.io.srcValid.expect(false.B)
        dut.io.fallbackMark.expect(false.B)
        dut.clock.step()
        clearPolicyProbe(dut)

        dut.io.debugActiveCount.expect(0.U)
        dut.io.debugRedirectKillCount.expect(1.U)
      }
    }
  }

  it should "track full Rename old-destination and same-group ordering" in {
    val config = smallRenameConfigWith(IntEarlyReleaseParams(enable = true, trackEntries = 4))

    simulate(new RenameERFullPathProbe()(config)) { dut =>
      checkFullRenameOldDestIdentity(dut)
      checkFullRenameSameGroupRedef(dut)
      checkFullRenameMoveThenRedef(dut)
      checkFullRenameHeldOldDestRead(dut)
    }
  }

  it should "feed validated external events into Rename UCA observe-only release accounting" in {
    val config = smallRenameConfigWith(IntEarlyReleaseParams(enable = true, observeOnly = true, trackEntries = 4))

    simulate(new RenameERFullPathProbe()(config)) { dut =>
      resetRenameProbe(dut)

      primeRenameRead(dut, lane = 0, ldest = 5)
      val producerPdest = fireSingleRenameLane(dut, lane = 0, ldest = 5)
      producerPdest should not be BigInt(0)
      dut.io.debugActiveCount.expect(1.U)

      primeRenameRead(dut, lane = 0, ldest = 6, lsrc0 = 5)
      driveRenameLane(dut, lane = 0, ldest = 6, lsrc0 = 5, src0Xp = true, rfWen = false)
      dut.io.erSrc0Valid(0).expect(true.B)
      dut.io.erSrc0Psrc(0).expect(producerPdest.U)
      dut.clock.step()
      clearRenameProbe(dut)

      dut.io.readDoneValid.poke(true.B)
      dut.io.readDoneRobIdx.poke(1.U)
      dut.io.readDoneTrackId.poke(0.U)
      dut.io.readDoneTrackGen.poke(1.U)
      dut.io.readDoneSrcIdx.poke(0.U)
      dut.io.readDonePsrc.poke(producerPdest.U)
      dut.clock.step()
      clearRenameProbe(dut)
      dut.io.debugReadDoneDecCount.expect(1.U)

      dut.io.producerReadyValid.poke(true.B)
      dut.io.producerReadyPdest.poke(producerPdest.U)
      dut.io.producerReadyRobIdx.poke(0.U)
      dut.clock.step()
      clearRenameProbe(dut)
      dut.io.debugProducerReadyCount.expect(1.U)

      primeRenameRead(dut, lane = 0, ldest = 5)
      driveRenameLane(dut, lane = 0, ldest = 5)
      dut.io.erRedefValid(0).expect(true.B)
      dut.io.erRedefOldPdest(0).expect(producerPdest.U)
      dut.clock.step()
      clearRenameProbe(dut)

      dut.io.guardDecValid.poke(true.B)
      dut.io.guardDecRobIdx.poke(2.U)
      dut.io.guardDecTrackId.poke(0.U)
      dut.io.guardDecTrackGen.poke(1.U)
      dut.io.guardDecOldPdest.poke(producerPdest.U)
      dut.clock.step()
      clearRenameProbe(dut)

      dut.io.debugGuardDecCount.expect(1.U)
      dut.io.debugEarlyFreeOpportunityCount.expect(1.U)
      dut.io.debugEarlyFreeCount.expect(0.U)
    }
  }

  it should "let external recovery kill non-released Rename UCA entries" in {
    val config = smallRenameConfigWith(IntEarlyReleaseParams(enable = true, trackEntries = 4))

    simulate(new RenameERFullPathProbe()(config)) { dut =>
      resetRenameProbe(dut)

      primeRenameRead(dut, lane = 0, ldest = 5)
      val producerPdest = fireSingleRenameLane(dut, lane = 0, ldest = 5)
      producerPdest should not be BigInt(0)
      dut.io.debugActiveCount.expect(1.U)

      dut.io.externalRedirectKill.poke(true.B)
      dut.clock.step()
      clearRenameProbe(dut)

      dut.io.debugActiveCount.expect(0.U)
    }
  }

  it should "hold decode-timed old-destination RAT data under decode backpressure" in {
    val config = fastSimConfigWith(IntEarlyReleaseParams(enable = true, trackEntries = 2))

    simulate(new DecodeOldDestHoldProbe()(config)) { dut =>
      dut.io.inValid.poke(false.B)
      dut.io.outReady.poke(true.B)
      dut.io.instr.poke(addi(0, 0).U)
      dut.io.writeValid.poke(false.B)
      dut.io.writeAddr.poke(0.U)
      dut.io.writeData.poke(0.U)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      writeDecodeRatMapping(dut, ldest = 5, pdest = 17)
      writeDecodeRatMapping(dut, ldest = 9, pdest = 29)

      dut.io.inValid.poke(true.B)
      dut.io.outReady.poke(true.B)
      dut.io.instr.poke(addi(rd = 5, rs1 = 1).U)
      dut.io.oldDestAddr.expect(5.U)
      dut.io.oldDestHold.expect(false.B)
      dut.clock.step()

      dut.io.outReady.poke(false.B)
      dut.io.instr.poke(addi(rd = 9, rs1 = 2).U)
      dut.io.oldDestHold.expect(true.B)
      dut.io.oldDestData.expect(17.U)
    }
  }
}
