package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util._
import xiangshan._
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.WbConfig.WbConfig
import xiangshan.backend.decode.{ImmUnion, XDecode}
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.vector.Bundles.{Category, Nf, VConfig, VLmul, VSew, VType, Vl, Vxrm}
import xiangshan.backend.issue.{IssueBlockParams, IssueQueueJumpBundle, SchedulerType, StatusArrayDeqRespBundle}
import xiangshan.backend.regfile.{RfReadPortWithConfig, RfWritePortWithConfig}
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend._
import xiangshan.mem.{LqPtr, SqPtr}

object Bundles {

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

    def connectCtrlFlow(source: CtrlFlow): Unit = {
      this.instr            := source.instr
      this.pc               := source.pc
      this.foldpc           := source.foldpc
      this.exceptionVec     := source.exceptionVec
      this.trigger          := source.trigger
      this.preDecodeInfo    := source.pd
      this.pred_taken       := source.pred_taken
      this.crossPageIPFFix  := source.crossPageIPFFix
      this.ftqPtr           := source.ftqPtr
      this.ftqOffset        := source.ftqOffset
    }
  }

  // StaticInst --[Decode]--> DecodedInst
  class DecodedInst(implicit p: Parameters) extends XSBundle {
    def numSrc = backendParams.numSrc
    // passed from StaticInst
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
    // decoded
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
    val fpu           = new FPUCtrlSignals
    val vpu           = new VPUCtrlSignals
    val isMove        = Bool()
    val uopIdx        = UInt(5.W)
    val uopSplitType  = UopSplitType()
    val isVset        = Bool()
    val firstUop      = Bool()
    val lastUop       = Bool()
    val numUops       = UInt(log2Up(MaxUopSize).W) // rob need this
    val commitType    = CommitType() // Todo: remove it

    private def allSignals = srcType.take(3) ++ Seq(fuType, fuOpType, rfWen, fpWen, vecWen,
      isXSTrap, waitForward, blockBackward, flushPipe, uopSplitType, selImm)

    def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]): DecodedInst = {
      val decoder: Seq[UInt] = ListLookup(
        inst, XDecode.decodeDefault.map(bitPatToUInt),
        table.map{ case (pat, pats) => (pat, pats.map(bitPatToUInt)) }.toArray
      )
      allSignals zip decoder foreach { case (s, d) => s := d }
      this
    }

    def isSoftPrefetch: Bool = {
      fuType === FuType.alu.U && fuOpType === ALUOpType.or && selImm === SelImm.IMM_I && ldest === 0.U
    }

    def connectStaticInst(source: StaticInst): Unit = {
      for ((name, data) <- this.elements) {
        if (source.elements.contains(name)) {
          data := source.elements(name)
        }
      }
    }
  }

  // DecodedInst --[Rename]--> DynInst
  class DynInst(implicit p: Parameters) extends XSBundle {
    def numSrc          = backendParams.numSrc
    // passed from StaticInst
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
    // passed from DecodedInst
    val srcType         = Vec(numSrc, SrcType())
    val lsrc            = Vec(numSrc, UInt(6.W))
    val ldest           = UInt(6.W)
    val fuType          = FuType()
    val fuOpType        = FuOpType()
    val rfWen           = Bool()
    val fpWen           = Bool()
    val vecWen          = Bool()
    val isXSTrap        = Bool()
    val waitForward     = Bool() // no speculate execution
    val blockBackward   = Bool()
    val flushPipe       = Bool() // This inst will flush all the pipe when commit, like exception but can commit
    val selImm          = SelImm()
    val imm             = UInt(XLEN.W) // Todo: check if it need minimized
    val fpu             = new FPUCtrlSignals
    val vpu             = new VPUCtrlSignals
    val isMove          = Bool()
    val uopIdx          = UInt(5.W)
    val isVset          = Bool()
    val firstUop        = Bool()
    val lastUop         = Bool()
    val numUops         = UInt(log2Up(MaxUopSize).W) // rob need this
    val commitType      = CommitType()
    // rename
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
    // Todo
    val lqIdx = new LqPtr
    val sqIdx = new SqPtr
    // debug module
    val singleStep      = Bool()
    // schedule
    val replayInst      = Bool()

    def isLUI: Bool = this.fuType === FuType.alu.U && this.selImm === SelImm.IMM_U
    def isWFI: Bool = this.fuType === FuType.csr.U && fuOpType === CSROpType.wfi

    def isSvinvalBegin(flush: Bool) = FuType.isFence(fuType) && fuOpType === FenceOpType.nofence && !flush
    def isSvinval(flush: Bool) = FuType.isFence(fuType) && fuOpType === FenceOpType.sfence && !flush
    def isSvinvalEnd(flush: Bool) = FuType.isFence(fuType) && fuOpType === FenceOpType.nofence && flush

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
      val wakeup = Output(new IssueQueueWakeUpBundle(pdest.getWidth))
      wakeup.rfWen := this.rfWen
      wakeup.fpWen := this.fpWen
      wakeup.vecWen := this.vecWen
      wakeup.pdest := this.pdest
      wakeup
    }

    def needWriteRf: Bool = (rfWen && ldest =/= 0.U) || fpWen || vecWen
  }

  trait BundleSource {
    var source = "not exist"
  }

  class IssueQueueWakeUpBundle(PregIdxWidth: Int) extends Bundle with BundleSource {
    val rfWen = Bool()
    val fpWen = Bool()
    val vecWen = Bool()
    val pdest = UInt(PregIdxWidth.W)

    /**
      * @param successor Seq[(psrc, srcType)]
      * @return Seq[if wakeup psrc]
      */
    def wakeUp(successor: Seq[(UInt, UInt)], valid: Bool): Seq[Bool]= {
      successor.map { case (thatPsrc, srcType) =>
        val pdestMatch = pdest === thatPsrc
        pdestMatch && (
          SrcType.isFp(srcType) && this.fpWen ||
          SrcType.isXp(srcType) && this.rfWen ||
          SrcType.isVp(srcType) && this.vecWen
        ) && valid
      }
    }
  }

  class VPUCtrlSignals(implicit p: Parameters) extends XSBundle {
    // vtype
    val vill      = Bool()
    val vma       = Bool()    // 1: agnostic, 0: undisturbed
    val vta       = Bool()    // 1: agnostic, 0: undisturbed
    val vsew      = VSew()
    val vlmul     = VLmul()   // 1/8~8      --> -3~3

    val vm        = Bool()    // 0: need v0.t
    val vstart    = Vl()

    // float rounding mode
    val frm       = Frm()
    // vector fix int rounding mode
    val vxrm      = Vxrm()
    // vector uop index, exclude other non-vector uop
    val vuopIdx   = UInt(log2Up(p(XSCoreParamsKey).MaxUopSize).W)
    // maybe used if data dependancy
    val vmask     = UInt(MaskSrcData().dataWidth.W)
    val vl        = Vl()

    // vector load/store
    val nf        = Nf()

    val needScalaSrc = Bool()

    val isReverse = Bool() // vrsub, vrdiv
    val isExt     = Bool()
    val isNarrow  = Bool()
    val isDstMask = Bool() // vvm, vvvm, mmm
    val isMove    = Bool() // vmv.s.x, vmv.v.v, vmv.v.x, vmv.v.i

    def vtype: VType = {
      val res = Wire(VType())
      res.illegal := this.vill
      res.vma     := this.vma
      res.vta     := this.vta
      res.vsew    := this.vsew
      res.vlmul   := this.vlmul
      res
    }

    def vconfig: VConfig = {
      val res = Wire(VConfig())
      res.vtype := this.vtype
      res.vl    := this.vl
      res
    }
  }

  // DynInst --[IssueQueue]--> DataPath
  class IssueQueueIssueBundle(
    iqParams: IssueBlockParams,
    exuParams: ExeUnitParams,
    addrWidth: Int,
    vaddrBits: Int
  )(implicit
    p: Parameters
  ) extends Bundle {
    private val rfReadDataCfgSet: Seq[Set[DataConfig]] = exuParams.getRfReadDataCfgSet

    val rf: MixedVec[MixedVec[RfReadPortWithConfig]] = Flipped(MixedVec(
      rfReadDataCfgSet.map((set: Set[DataConfig]) =>
        MixedVec(set.map((x: DataConfig) => new RfReadPortWithConfig(x, addrWidth)).toSeq)
      )
    ))
    val srcType = Vec(exuParams.numRegSrc, SrcType()) // used to select imm or reg data
    val immType = SelImm()                         // used to select imm extractor
    val common = new ExuInput(exuParams)
    val jmp = if (exuParams.needPc) Some(Flipped(new IssueQueueJumpBundle)) else None
    val addrOH = UInt(iqParams.numEntries.W)

    def getSource: SchedulerType = exuParams.getWBSource
    def getIntRfReadBundle: Seq[RfReadPortWithConfig] = rf.flatten.filter(_.readInt)
    def getVfRfReadBundle: Seq[RfReadPortWithConfig] = rf.flatten.filter(_.readVf)
  }

  class OGRespBundle(implicit p:Parameters, params: IssueBlockParams) extends XSBundle {
    val og0resp = Valid(new StatusArrayDeqRespBundle)
    val og1resp = Valid(new StatusArrayDeqRespBundle)
  }

  // DataPath --[ExuInput]--> Exu
  class ExuInput(val params: ExeUnitParams)(implicit p: Parameters) extends XSBundle {
    val fuType        = FuType()
    val fuOpType      = FuOpType()
    val src           = Vec(params.numRegSrc, UInt(params.dataBitsMax.W))
    val imm           = UInt(XLEN.W)
    val robIdx        = new RobPtr
    val iqIdx         = UInt(log2Up(MemIQSizeMax).W)// Only used by store yet
    val isFirstIssue  = Bool()                      // Only used by store yet
    val pdest         = UInt(params.wbPregIdxWidth.W)
    val rfWen         = if (params.writeIntRf)    Some(Bool())                        else None
    val fpWen         = if (params.writeFpRf)     Some(Bool())                        else None
    val vecWen        = if (params.writeVecRf)    Some(Bool())                        else None
    val fpu           = if (params.needFPUCtrl)   Some(new FPUCtrlSignals)            else None
    val vpu           = if (params.needVPUCtrl)   Some(new VPUCtrlSignals)            else None
    val flushPipe     = if (params.flushPipe)     Some(Bool())                        else None
    val pc            = if (params.needPc)        Some(UInt(VAddrData().dataWidth.W)) else None
    val jalrTarget    = if (params.hasJmpFu)      Some(UInt(VAddrData().dataWidth.W)) else None
    val preDecode     = if (params.hasPredecode)  Some(new PreDecodeInfo)             else None
    val ftqIdx        = if (params.needPc || params.replayInst)
                                                  Some(new FtqPtr)                    else None
    val ftqOffset     = if (params.needPc || params.replayInst)
                                                  Some(UInt(log2Up(PredictWidth).W))  else None
    val predictInfo   = if (params.hasPredecode)  Some(new Bundle {
      val target = UInt(VAddrData().dataWidth.W)
      val taken = Bool()
    }) else None
    val sqIdx = if (params.hasMemAddrFu || params.hasStdFu) Some(new SqPtr) else None
    val lqIdx = if (params.hasMemAddrFu) Some(new LqPtr) else None

    def fromIssueBundle(source: IssueQueueIssueBundle): Unit = {
      // src is assigned to rfReadData
      this.fuType       := source.common.fuType
      this.fuOpType     := source.common.fuOpType
      this.imm          := source.common.imm
      this.robIdx       := source.common.robIdx
      this.pdest        := source.common.pdest
      this.isFirstIssue := source.common.isFirstIssue // Only used by mem debug log
      this.iqIdx        := source.common.iqIdx        // Only used by mem feedback
      this.rfWen        .foreach(_ := source.common.rfWen.get)
      this.fpWen        .foreach(_ := source.common.fpWen.get)
      this.vecWen       .foreach(_ := source.common.vecWen.get)
      this.fpu          .foreach(_ := source.common.fpu.get)
      this.vpu          .foreach(_ := source.common.vpu.get)
      this.flushPipe    .foreach(_ := source.common.flushPipe.get)
      this.pc           .foreach(_ := source.jmp.get.pc)
      this.jalrTarget   .foreach(_ := source.jmp.get.target)
      this.preDecode    .foreach(_ := source.common.preDecode.get)
      this.ftqIdx       .foreach(_ := source.common.ftqIdx.get)
      this.ftqOffset    .foreach(_ := source.common.ftqOffset.get)
      this.predictInfo  .foreach(_ := source.common.predictInfo.get)
      this.lqIdx        .foreach(_ := source.common.lqIdx.get)
      this.sqIdx        .foreach(_ := source.common.sqIdx.get)
    }
  }

  // ExuInput --[FuncUnit]--> ExuOutput
  class ExuOutput(
    val params: ExeUnitParams,
  )(implicit
    val p: Parameters
  ) extends Bundle with BundleSource with HasXSParameter {
    val data         = UInt(params.dataBitsMax.W)
    val pdest        = UInt(params.wbPregIdxWidth.W)
    val robIdx       = new RobPtr
    val intWen       = if (params.writeIntRf)   Some(Bool())                  else None
    val fpWen        = if (params.writeFpRf)    Some(Bool())                  else None
    val vecWen       = if (params.writeVecRf)   Some(Bool())                  else None
    val redirect     = if (params.hasRedirect)  Some(ValidIO(new Redirect))   else None
    val fflags       = if (params.writeFflags)  Some(UInt(5.W))               else None
    val vxsat        = if (params.writeVxsat)   Some(Bool())                  else None
    val exceptionVec = if (params.exceptionOut.nonEmpty) Some(ExceptionVec()) else None
    val flushPipe    = if (params.flushPipe)    Some(Bool())                  else None
    val replay       = if (params.replayInst)   Some(Bool())                  else None
    val lqIdx        = if (params.hasLoadFu)    Some(new LqPtr())             else None
    val sqIdx        = if (params.hasStoreAddrFu || params.hasStdFu)
                                                Some(new SqPtr())             else None
    val ftqIdx       = if (params.needPc || params.replayInst)
                                                Some(new FtqPtr)                    else None
    val ftqOffset    = if (params.needPc || params.replayInst)
                                                Some(UInt(log2Up(PredictWidth).W))  else None
    // uop info
    val predecodeInfo = if(params.hasPredecode) Some(new PreDecodeInfo) else None
    val debug = new DebugBundle
    val debugInfo = new PerfDebugInfo
  }

  // ExuOutput + DynInst --> WriteBackBundle
  class WriteBackBundle(val params: WbConfig)(implicit p: Parameters) extends Bundle with BundleSource {
    val rfWen = Bool()
    val fpWen = Bool()
    val vecWen = Bool()
    val pdest = UInt(params.pregIdxWidth.W)
    val data = UInt(params.dataWidth.W)
    val robIdx = new RobPtr()(p)
    val flushPipe = Bool()
    val replayInst = Bool()
    val redirect = ValidIO(new Redirect)
    val fflags = UInt(5.W)
    val exceptionVec = ExceptionVec()
    val debug = new DebugBundle
    val debugInfo = new PerfDebugInfo

    def fromExuOutput(source: ExuOutput) = {
      this.rfWen  := source.intWen.getOrElse(false.B)
      this.fpWen  := source.fpWen.getOrElse(false.B)
      this.vecWen := source.vecWen.getOrElse(false.B)
      this.pdest  := source.pdest
      this.data   := source.data
      this.robIdx := source.robIdx
      this.flushPipe := source.flushPipe.getOrElse(false.B)
      this.replayInst := source.replay.getOrElse(false.B)
      this.redirect := source.redirect.getOrElse(0.U.asTypeOf(this.redirect))
      this.fflags := source.fflags.getOrElse(0.U.asTypeOf(this.fflags))
      this.exceptionVec := source.exceptionVec.getOrElse(0.U.asTypeOf(this.exceptionVec))
      this.debug := source.debug
      this.debugInfo := source.debugInfo
    }

    def asWakeUpBundle: IssueQueueWakeUpBundle = {
      val wakeup = Output(new IssueQueueWakeUpBundle(params.pregIdxWidth))
      wakeup.rfWen := this.rfWen
      wakeup.fpWen := this.fpWen
      wakeup.vecWen := this.vecWen
      wakeup.pdest := this.pdest
      wakeup.source = this.source
      wakeup
    }

    def asIntRfWriteBundle(fire: Bool): RfWritePortWithConfig = {
      val rfWrite = Wire(Output(new RfWritePortWithConfig(this.params.dataCfg, this.params.pregIdxWidth)))
      rfWrite.wen := this.rfWen && fire
      rfWrite.addr := this.pdest
      rfWrite.data := this.data
      rfWrite.intWen := this.rfWen
      rfWrite.fpWen := false.B
      rfWrite.vecWen := false.B
      rfWrite
    }

    def asVfRfWriteBundle(fire: Bool): RfWritePortWithConfig = {
      val rfWrite = Wire(Output(new RfWritePortWithConfig(this.params.dataCfg, this.params.pregIdxWidth)))
      rfWrite.wen := (this.fpWen || this.vecWen) && fire
      rfWrite.addr := this.pdest
      rfWrite.data := this.data
      rfWrite.intWen := false.B
      rfWrite.fpWen := this.fpWen
      rfWrite.vecWen := this.vecWen
      rfWrite
    }
  }

  class ExceptionInfo extends Bundle {
    val pc = UInt(VAddrData().dataWidth.W)
    val instr = UInt(32.W)
    val commitType = CommitType()
    val exceptionVec = ExceptionVec()
    val singleStep = Bool()
    val crossPageIPFFix = Bool()
    val isInterrupt = Bool()
  }

  class MemExuInput(isVector: Boolean = false)(implicit p: Parameters) extends XSBundle {
    val uop = new DynInst
    val src = if(isVector) Vec(5, UInt(VLEN.W)) else Vec(3, UInt(XLEN.W))
    val iqIdx = UInt(log2Up(MemIQSizeMax).W)
    val isFirstIssue = Bool()
  }

  class MemExuOutput(isVector: Boolean = false)(implicit p: Parameters) extends XSBundle {
    val uop = new DynInst
    val data = if(isVector) UInt(VLEN.W) else UInt(XLEN.W)
    val debug = new DebugBundle
  }

  class MemMicroOpRbExt(implicit p: Parameters) extends XSBundle {
    val uop = new DynInst
    val flag = UInt(1.W)
  }
}
