package xiangshan.v2backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend._
import xiangshan.v2backend.issue.IssueQueueJumpBundle

object Bundles {
  // vector inst need vs1, vs2, v0, vd, vl&vtype, 5 psrcs
  def numSrc          = 5

  // frontend -> backend
  class StaticInst(implicit p: Parameters) extends XSBundle {
    val instr           = UInt(32.W)
    val pc              = UInt(VAddrBits.W)
    val foldpc          = UInt(MemPredPCWidth.W)
    val exceptionVec    = ExceptionVec()
    val trigger         = new TriggerCf
    val preDecodeInfo   = new PreDecodeInfo
    val pred_taken      = Bool()
    val crossPageIPFFix = Bool()
    val ftqPtr          = new FtqPtr
    val ftqOffset       = UInt(log2Up(PredictWidth).W)
  }

  // StaticInst --[Decode]--> DecodedInst
  class DecodedInst(implicit p: Parameters) extends XSBundle {
    val srcType       = Vec(numSrc, SrcType())
    val lsrc          = Vec(numSrc, UInt(6.W))
    val ldest         = UInt(6.W)
    val fuType        = FuType()
    val fuOpType      = FuOpType()
    val rfWen         = Bool()
    val fpWen         = Bool()
    val vecWen        = Bool()
    val isXSTrap      = Bool()
    val waitForward   = Bool() // no speculate execution
    val blockBackward = Bool()
    val flushPipe     = Bool() // This inst will flush all the pipe when commit, like exception but can commit
    val selImm        = SelImm()
    val imm           = UInt(ImmUnion.maxLen.W)
    val commitType    = CommitType()
    val fpu           = new FPUCtrlSignals
    val vpu           = new VPUCtrlSignals
    val isMove        = Bool()

    def isLUI: Bool = this.selImm === SelImm.IMM_U && this.fuType === FuType.alu.U
  }

  // DecodedInst --[Rename]--> DynInst
  class DynInst(implicit p: Parameters) extends XSBundle {
    val imm             = ValidIO(UInt(XLEN.W)) // Todo: check if it need minimized

    val fuType          = FuType()
    val fuOpType        = FuOpType()
    val rfWen           = Bool()
    val fpWen           = Bool()
    val vecWen          = Bool()
    val exceptionVec    = ExceptionVec()
    val flushPipe       = Bool()

    val srcType         = Vec(numSrc, SrcType())
    val srcState        = Vec(numSrc, SrcState())
    val psrc            = Vec(numSrc, UInt(PhyRegIdxWidth.W))
    val pdest           = UInt(PhyRegIdxWidth.W)
    val oldPdest        = UInt(PhyRegIdxWidth.W)
    val robIdx          = new RobPtr
    val eliminatedMove  = Bool()
    val debugInfo       = new PerfDebugInfo
    val storeSetHit     = Bool() // inst has been allocated an store set
    val waitForRobIdx   = new RobPtr // store set predicted previous store robIdx
    // Load wait is needed
    // load inst will not be executed until former store (predicted by mdp) addr calcuated
    val loadWaitBit     = Bool()
    // If (loadWaitBit && loadWaitStrict), strict load wait is needed
    // load inst will not be executed until ALL former store addr calcuated
    val loadWaitStrict  = Bool()
    val ssid            = UInt(SSIDWidth.W)
    // debug module
    val singleStep      = Bool()
    // schedule
    val replayInst      = Bool()

    def srcIsReady: Vec[Bool] = {
      VecInit(this.srcType.zip(this.srcState).map {
        case (t, s) => SrcType.isNotReg(t) || SrcState.isReady(s)
      })
    }

    def clearExceptions(
      exceptionBits: Seq[Int] = Seq(),
      flushPipe    : Boolean = false,
      replayInst   : Boolean = false
    ): DynInst = {
      this.exceptionVec.zipWithIndex.filterNot(x => exceptionBits.contains(x._2)).foreach(_._1 := false.B)
      if (!flushPipe) { this.flushPipe := false.B }
      if (!replayInst) { this.replayInst := false.B }
      this
    }

    def asWakeUpBundle: IssueQueueWakeUpBundle = {
      val wakeup = Output(new IssueQueueWakeUpBundle)
      wakeup.rfWen := this.rfWen
      wakeup.fpWen := this.fpWen
      wakeup.vecWen := this.vecWen
      wakeup.pdest := this.pdest
      wakeup
    }
  }

  trait BundleSource {
    var source = "not exist"
  }

  class IssueQueueWakeUpBundle(implicit p: Parameters) extends XSBundle with BundleSource {
    val rfWen = Bool()
    val fpWen = Bool()
    val vecWen = Bool()
    val pdest = UInt(PhyRegIdxWidth.W)

    /**
      * @param successor Seq[(psrc, srcType)]
      * @return Seq[if wakeup psrc]
      */
    def wakeUp(successor: Seq[(UInt, UInt)]): Seq[Bool]= {
      successor.map { case (thatPsrc, srcType) =>
        val pdestMatch = pdest === thatPsrc
        pdestMatch && (
          SrcType.isFp(srcType) && this.fpWen ||
          SrcType.isXp(srcType) && this.rfWen ||
          SrcType.isVp(srcType) && this.vecWen)
      }
    }
  }

  object VsewBundle {
    def apply()   = UInt(2.W)   // 8/16/32/64 --> 0/1/2/3
  }

  class VPUCtrlSignals(implicit p: Parameters) extends XSBundle {
    val vlmul     = SInt(3.W) // 1/8~8      --> -3~3
    val vsew      = VsewBundle()
    val vta       = Bool()    // 1: agnostic, 0: undisturbed
    val vma       = Bool()    // 1: agnostic, 0: undisturbed
    val vm        = Bool()    // 0: need v0.t
    val vill      = Bool()
    // vector load/store
    val nf        = UInt(3.W)
    val lsumop    = UInt(5.W) // lumop or sumop
    // used for vector index load/store and vrgatherei16.vv
    val idxEmul   = UInt(3.W)
  }

  // DynInst --[IssueQueue]--> DataPath
  class IssueQueueIssueBundle(exuParams: ExeUnitParams, addrWidth: Int, vaddrBits: Int) extends Bundle {
    private val rfReadDataCfgSet: Seq[Set[DataConfig]] = exuParams.getRfReadDataCfgSet

    val rf: MixedVec[MixedVec[RfReadPortWithConfig]] = Flipped(MixedVec(
      rfReadDataCfgSet.map((set: Set[DataConfig]) =>
        MixedVec(set.map((x: DataConfig) => new RfReadPortWithConfig(x, addrWidth)).toSeq)
      )
    ))
    val common = new ExuInput(exuParams)
    val jmp = if (exuParams.hasJmpFu) Some(Flipped(new IssueQueueJumpBundle(vaddrBits))) else None

    def getSource: SchedulerType = exuParams.getWBSource
    def getIntRfReadBundle: Seq[RfReadPortWithConfig] = rf.flatten.filter(_.readInt)
    def getFpRfReadBundle: Seq[RfReadPortWithConfig] = rf.flatten.filter(_.readFp)
  }

  // DataPath --[ExuInput]--> ExuInput
  class ExuInput(params: ExeUnitParams) extends Bundle {
    val fuType    = FuType()
    val fuOpType  = FuOpType()
    val rfWen     = Bool()
    val fpWen     = Bool()
    val vecWen    = Bool()
    val data      = Vec(params.numIntSrc, UInt(params.dataBits.W))
  }

  // ExuInput --[FuncUnit]--> ExuOutput
  class ExuOutput(
    params: ExeUnitParams,
  ) extends Bundle with BundleSource {
    val data = UInt(params.dataBits.W)
//    val debug = new DebugBundle
//    val redirect = if(params.hasRedirect) Some(ValidIO(new Redirect)) else None
    val fflag = if(params.writeFflags) Some(UInt(5.W)) else None
  }

  // ExuOutput + DynInst --> WriteBackBundle
  class WriteBackBundle(dataBits: Int)(implicit p: Parameters) extends XSBundle with BundleSource {
    val rfWen = Bool()
    val fpWen = Bool()
    val vecWen = Bool()
    val pdest = UInt(PhyRegIdxWidth.W)
    val data = UInt(dataBits.W)

    def asWakeUpBundle: IssueQueueWakeUpBundle = {
      val wakeup = Output(new IssueQueueWakeUpBundle)
      wakeup.rfWen := this.rfWen
      wakeup.fpWen := this.fpWen
      wakeup.vecWen := this.vecWen
      wakeup.pdest := this.pdest
      wakeup.source = this.source
      wakeup
    }
  }
}
