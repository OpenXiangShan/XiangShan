package xiangshan.backend.vector.vagq

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import top.{ArgParser, Generator}
import utility._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.NewCSR.CSRConfig
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.vector.{XSVectorParamKey, XSVectorParameters}
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.mem.{LqPtr, SqPtr}

import VAGQConstants._

object VAGQConstants {
  val VAGQSize = 8
  val VAGQEntryIdxWidth = log2Ceil(VAGQSize)
  val FlowBytes = 16
  val FlowByteWidth = log2Ceil(FlowBytes)
  val UvlByteWidth = FlowByteWidth + 1
  val UopIdxWidth = 3
  val FaultVstartWidth = UopIdxWidth + FlowByteWidth
  val EewWidth = 2
  val AlignedTypeWidth = EewWidth + 1
  val NfWidth = 3
  val ExceptionNumberWidth = 6

  val AddrIssueWidth = 2
  val DataIssueWidth = 4
  val ActiveIssueWidth = 2
  val LduRespWidth = 3
  val StaRespWidth = 2
  val ActiveRespWidth = LduRespWidth + StaRespWidth
  val VrfWriteWidth = 1
  val SplitUpdateWidth = 2
  val MergeRespWidth = ActiveRespWidth + 1
}

trait HasVAGQParameters extends HasXSParameter {
  import VAGQConstants._

  def vagqSize: Int = VAGQSize
  def vagqEntryIdxWidth: Int = VAGQEntryIdxWidth
  def vagqFlowBytes: Int = FlowBytes
  def vagqFlowByteWidth: Int = FlowByteWidth
  def vagqUvlByteWidth: Int = UvlByteWidth
  def vagqUopIdxWidth: Int = UopIdxWidth

  require(VLEN == 128, s"VAGQ currently assumes VLEN=128, got VLEN=$VLEN")
  require(VDataBytes == FlowBytes, s"VAGQ FlowBytes must match VDataBytes, got $FlowBytes and $VDataBytes")
  require(vagqSize == 4 || vagqSize == 8, s"VAGQSize must be 4 or 8, got $vagqSize")
}

abstract class VAGQBundle(implicit p: Parameters) extends XSBundle with HasVAGQParameters

abstract class VAGQModule(implicit p: Parameters) extends XSModule with HasVAGQParameters with HasVAGQHelper

class VAGQMeta(implicit p: Parameters) extends VAGQBundle {
  val pc = UInt(VAddrBits.W)
  val isRVC = Bool()
  val ftqPtr = new FtqPtr
  val ftqOffset = UInt(FetchBlockInstOffsetWidth.W)
  val lqIdx = new LqPtr
  val sqIdx = new SqPtr
  val trigger = TriggerAction()
  val perfDebugInfo = new PerfDebugInfo
  val debug_seqNum = InstSeqNum()
}

class VAGQAddrSideUop(implicit p: Parameters) extends VAGQBundle {
  val meta = new VAGQMeta
  val entryIdx = UInt(vagqEntryIdxWidth.W)
  val uopType = UInt(3.W)
  val robIdx = new RobPtr
  val pdest = UInt(VfPhyRegIdxWidth.W)
  val baseAddr = UInt(XLEN.W)
  val uvlByte = UInt(5.W)
  val vstart = UInt((CSRConfig.VlWidth-1).W)
  val useVstart = Bool()
  val vm = Bool()
  val v0Mask = UInt(vagqFlowBytes.W)
  val deew = UInt(EewWidth.W)
  val ieew = UInt(EewWidth.W)
  val vma = Bool()
  val vta = Bool()
  val uopIdx = UInt(UopIdxWidth.W)
  val nf = UInt(NfWidth.W)
}

class VAGQDataSideUop(implicit p: Parameters) extends VAGQBundle {
  val entryIdx = UInt(vagqEntryIdxWidth.W)
  val robIdx = new RobPtr
  val op2Data = UInt(VLEN.W)
  val psrc2 = UInt(VfPhyRegIdxWidth.W)
}

class VAGQLsuReq(implicit p: Parameters) extends VAGQBundle {
  val entryIdx    = UInt(vagqEntryIdxWidth.W)
  val robIdx      = new RobPtr
  val isLoad      = Bool()
  val isStore     = Bool()
  val lqIdx       = new LqPtr
  val sqIdx       = new SqPtr
  val byteOffset  = UInt(vagqFlowByteWidth.W)
  val elemIdx     = UInt(vagqFlowByteWidth.W)
  val mask        = UInt(vagqFlowBytes.W)
  val alignedType = UInt(AlignedTypeWidth.W)  // deew
  val vaddr       = UInt(XLEN.W)
  val data        = UInt(VLEN.W)
  val pdest       = UInt(VfPhyRegIdxWidth.W)
  val nf          = UInt(NfWidth.W)
}

class VAGQLsqEmptyReq(implicit p: Parameters) extends VAGQBundle {
  val entryIdx = UInt(vagqEntryIdxWidth.W)
  val robIdx   = new RobPtr
  val isLoad   = Bool()
  val isStore  = Bool()
  val lqIdx    = new LqPtr
  val sqIdx    = new SqPtr
  val emptyMask = UInt(vagqFlowBytes.W)
  val entryMask = UInt(vagqFlowBytes.W)
}

class VAGQLsqEmptyResp(implicit p: Parameters) extends VAGQBundle {
  val entryIdx = UInt(vagqEntryIdxWidth.W)
  val robIdx   = new RobPtr
  val isLoad   = Bool()
  val isStore  = Bool()
  val mask = UInt(vagqFlowBytes.W)
  val isNACK = Bool()
  val exception = Bool()
  val exceptionNumber = UInt(ExceptionNumberWidth.W)
}

class VAGQResp(implicit p: Parameters) extends VAGQBundle {
  val entryIdx = UInt(vagqEntryIdxWidth.W)
  val robIdx = new RobPtr
  val isLoad = Bool()
  val isStore = Bool()
  val byteOffset = UInt(vagqFlowByteWidth.W)
  val mask = UInt(vagqFlowBytes.W)
  val data = UInt(VLEN.W)
  val isNACK = Bool()
  val exception = Bool()
  val exceptionNumber = UInt(ExceptionNumberWidth.W)
}

class VAGQVRFReadReq(implicit p: Parameters) extends VAGQBundle {
  val entryIdx = UInt(vagqEntryIdxWidth.W)
  val robIdx = new RobPtr
  val psrc = UInt(VfPhyRegIdxWidth.W)
}

class VAGQVRFReadResp(implicit p: Parameters) extends VAGQBundle {
  val entryIdx = UInt(vagqEntryIdxWidth.W)
  val robIdx = new RobPtr
  val data = UInt(VLEN.W)
}

class VAGQVRFWriteReq(implicit p: Parameters) extends VAGQBundle {
  val entryIdx = UInt(vagqEntryIdxWidth.W)
  val pdest = UInt(VfPhyRegIdxWidth.W)
  val data = UInt(VLEN.W)
  val mask = UInt(vagqFlowBytes.W)
}

class VAGQWritebackReq(implicit p: Parameters) extends VAGQBundle {
  val meta = new VAGQMeta
  val entryIdx = UInt(vagqEntryIdxWidth.W)
  val robIdx = new RobPtr
  val exception = Bool()
  val exceptionNumber = UInt(ExceptionNumberWidth.W)
  val faultElemIdx = UInt(vagqFlowByteWidth.W)
  val faultVstart = UInt(VAGQConstants.FaultVstartWidth.W)
  val uopType = UInt(3.W)
  val uopIdx = UInt(UopIdxWidth.W)
  val deew = UInt(EewWidth.W)
  val nf = UInt(NfWidth.W)
}

class CtrlInput(implicit p: Parameters) extends VAGQBundle {
  val entryIdx = UInt(vagqEntryIdxWidth.W)
  val entry = new VAGQEntryMeta
}

class VAGQIO(implicit p: Parameters) extends VAGQBundle {
  // from iq
  val addrUop = Flipped(Vec(VAGQConstants.AddrIssueWidth, Decoupled(new VAGQAddrSideUop)))
  val dataUop = Flipped(Vec(VAGQConstants.DataIssueWidth, Decoupled(new VAGQDataSideUop)))
  // active req to ldu
  val lsuReq = Vec(VAGQConstants.ActiveIssueWidth, Decoupled(new VAGQLsuReq))
  val lduResp = Flipped(Vec(VAGQConstants.LduRespWidth, Valid(new VAGQResp)))
  val staResp = Flipped(Vec(VAGQConstants.StaRespWidth, Valid(new VAGQResp)))
  // unactive req to lsq
  val lsqEmptyReq = Decoupled(new VAGQLsqEmptyReq)
  val lsqEmptyResp = Flipped(Valid(new VAGQLsqEmptyResp))
  // load read oldvd, store read storeData
  val vrfReadReq = Valid(new VAGQVRFReadReq)
  val vrfReadResp = Flipped(Valid(new VAGQVRFReadResp))
  // write vrf with mask
  val vrfWriteReq = ValidIO(new VAGQVRFWriteReq)
  // to rob
  val robWriteback = Decoupled(new VAGQWritebackReq)
  val redirect = Flipped(Valid(new Redirect))
}

class VAGQ(implicit p: Parameters) extends VAGQModule {
  val io = IO(new VAGQIO)

  private val entryTable = Module(new VAGQEntryTable)
  private val splitCtrl = Module(new SplitCtrl(vagqSize))
  private val mergeCtrl = Module(new MergeCtrl(vagqSize))

  entryTable.io.addrUop          <> io.addrUop
  entryTable.io.dataUop          <> io.dataUop
  entryTable.io.mergeReqUpdate   := mergeCtrl.io.reqUpdate
  entryTable.io.mergeStateUpdate := mergeCtrl.io.stateUpdate
  entryTable.io.redirect         := io.redirect

  splitCtrl.io.redirect := io.redirect
  mergeCtrl.io.redirect := io.redirect

  for (i <- 0 until vagqSize) {
    splitCtrl.io.in(i).entryIdx := i.U(vagqEntryIdxWidth.W)
    splitCtrl.io.in(i).entry := entryTable.io.entries(i)

    mergeCtrl.io.entry(i).entryIdx := i.U(vagqEntryIdxWidth.W)
    mergeCtrl.io.entry(i).entry := entryTable.io.entries(i)
  }

  val lsuReqQueue = Seq.fill(VAGQConstants.ActiveIssueWidth)(Module(new Queue(new VAGQLsuReq, 1)))
  lsuReqQueue.zip(splitCtrl.io.lsuReq).map { case (out, in) =>
    out.io.enq <> in
  }

  val lsqEmptyReqQueue = Module(new Queue(new VAGQLsqEmptyReq, 1))
  lsqEmptyReqQueue.io.enq <> splitCtrl.io.lsqEmptyReq

  io.lsuReq.zip(lsuReqQueue).foreach { case (out, in) => out <> in.io.deq }
  io.lsqEmptyReq <> lsqEmptyReqQueue.io.deq

  entryTable.io.splitUpdate := splitCtrl.io.update

  mergeCtrl.io.lduResp := io.lduResp
  mergeCtrl.io.staResp := io.staResp
  val lsqEmptyResp = Wire(Valid(new VAGQResp))
  lsqEmptyResp.valid := io.lsqEmptyResp.valid
  lsqEmptyResp.bits  := 0.U.asTypeOf(lsqEmptyResp.bits)
  lsqEmptyResp.bits.entryIdx        := io.lsqEmptyResp.bits.entryIdx
  lsqEmptyResp.bits.robIdx          := io.lsqEmptyResp.bits.robIdx
  lsqEmptyResp.bits.isLoad          := io.lsqEmptyResp.bits.isLoad
  lsqEmptyResp.bits.isStore         := io.lsqEmptyResp.bits.isStore
  lsqEmptyResp.bits.byteOffset      := 0.U
  lsqEmptyResp.bits.mask            := io.lsqEmptyResp.bits.mask
  lsqEmptyResp.bits.isNACK          := io.lsqEmptyResp.bits.isNACK
  lsqEmptyResp.bits.exception       := io.lsqEmptyResp.bits.exception
  lsqEmptyResp.bits.exceptionNumber := io.lsqEmptyResp.bits.exceptionNumber
  mergeCtrl.io.lsqEmptyResp := lsqEmptyResp

  mergeCtrl.io.vrfReadResp := io.vrfReadResp

  io.vrfReadReq.valid := mergeCtrl.io.vrfReadReq.valid
  io.vrfReadReq.bits  := mergeCtrl.io.vrfReadReq.bits

  io.vrfWriteReq  := mergeCtrl.io.vrfWriteReq
  io.robWriteback <> mergeCtrl.io.robWriteback
}

object VAGQMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform" :+ "--target" :+ "verilog")

  val defaultConfig = config.alterPartial({
    // Get XSCoreParams and vector params for compiling VAGQ independently.
    case XSCoreParamsKey => config(XSTileKey).head
    case XSVectorParamKey => XSVectorParameters(128)
  })

  Generator.execute(
    firrtlOpts :+ "--full-stacktrace" :+ "--target-dir" :+ "vagq",
    new VAGQ()(defaultConfig),
    firtoolOpts :+ "-O=release" :+ "--disable-annotation-unknown" :+
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )

  println("done")
}
