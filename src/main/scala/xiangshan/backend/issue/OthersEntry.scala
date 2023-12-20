package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.HasCircularQueuePtrHelper
import utils.{MathUtils, OptionWrapper}
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.FuType
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.issue.EntryBundles._
import xiangshan.mem.{MemWaitUpdateReq, SqPtr, LqPtr}


class OthersEntryIO(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
  //input
  val commonIn        = new CommonInBundle
  //output
  val commonOut       = new CommonOutBundle
  val enqReady        = Output(Bool())

  def wakeup          = commonIn.wakeUpFromWB ++ commonIn.wakeUpFromIQ
}

class OthersEntry(implicit p: Parameters, params: IssueBlockParams) extends XSModule {
  val io = IO(new OthersEntryIO)

  val validReg        = RegInit(false.B)
  val entryReg        = Reg(new EntryBundle)

  val common          = Wire(new CommonWireBundle)
  val entryRegNext    = Wire(new EntryBundle)
  val hasWakeupIQ     = OptionWrapper(params.hasIQWakeUp, Wire(new CommonIQWakeupBundle))

  //Reg
  validReg := common.validRegNext
  entryReg := entryRegNext

  //Wire
  CommonWireConnect(common, hasWakeupIQ, validReg, entryReg.status, io.commonIn, false)

  if (params.hasIQWakeUp) {
    ShiftLoadDependency(hasWakeupIQ.get)
    CommonIQWakeupConnect(common, hasWakeupIQ.get, validReg, entryReg.status, io.commonIn, false)
  }

  when(io.commonIn.enq.valid && io.commonIn.transSel) {
    entryRegNext := io.commonIn.enq.bits
  }.otherwise {
    EntryRegCommonConnect(common, hasWakeupIQ, validReg, entryRegNext, entryReg, entryReg.status, io.commonIn, false)
  }

  //output
  CommonOutConnect(io.commonOut, common, hasWakeupIQ, validReg, entryReg, entryReg.status, io.commonIn, false)
  io.enqReady         := common.enqReady
}

class OthersEntryMem()(implicit p: Parameters, params: IssueBlockParams) extends OthersEntry
  with HasCircularQueuePtrHelper {
  EntryMemConnect(io.commonIn, common, validReg, entryReg, entryRegNext, false)
}

class OthersEntryVecMemAddr()(implicit p: Parameters, params: IssueBlockParams) extends OthersEntryMem {

  require(params.isVecMemAddrIQ, "OthersEntryVecMemAddr can only be instance of VecMemAddr IQ")

  val commonIn = io.commonIn
  val enqValid = commonIn.enq.valid && commonIn.transSel
  val fromMem = commonIn.fromMem.get
  val memStatus = entryReg.status.mem.get
  val memStatusNext = entryRegNext.status.mem.get
  val shouldBlock = Mux(commonIn.enq.valid && commonIn.transSel, commonIn.enq.bits.status.blocked, entryReg.status.blocked)
  // load cannot be issued before older store, unless meet some condition
  val blockedByOlderStore = isAfter(memStatusNext.sqIdx, fromMem.stIssuePtr)

  val vecMemStatus = entryReg.status.vecMem.get
  val vecMemStatusNext = entryRegNext.status.vecMem.get
  val fromLsq = io.commonIn.fromLsq.get

  when(enqValid) {
    vecMemStatusNext := io.commonIn.enq.bits.status.vecMem.get
  }.otherwise {
    vecMemStatusNext := vecMemStatus
  }

  val isLsqHead = {
    entryRegNext.status.vecMem.get.lqIdx <= fromLsq.lqDeqPtr &&
    entryRegNext.status.vecMem.get.sqIdx <= fromLsq.sqDeqPtr
  }
  dontTouch(isLsqHead)

  entryRegNext.status.blocked := !isLsqHead
}

class OthersEntryVecMemData()(implicit p: Parameters, params: IssueBlockParams) extends OthersEntry
  with HasCircularQueuePtrHelper {

  require(params.isVecStDataIQ, "OthersEntryVecMemData can only be instance of VecMemData IQ")

  val commonIn = io.commonIn
  val enqValid = commonIn.enq.valid && commonIn.transSel
  val vecMemStatus = entryReg.status.vecMem.get
  val vecMemStatusNext = entryRegNext.status.vecMem.get
  val fromLsq = io.commonIn.fromLsq.get

  when(enqValid) {
    vecMemStatusNext.sqIdx := commonIn.enq.bits.status.vecMem.get.sqIdx
    vecMemStatusNext.lqIdx := commonIn.enq.bits.status.vecMem.get.lqIdx
    vecMemStatusNext.uopIdx := commonIn.enq.bits.status.vecMem.get.uopIdx
  }.otherwise {
    vecMemStatusNext := vecMemStatus
  }

  val isLsqHead = entryRegNext.status.vecMem.get.sqIdx.value === fromLsq.sqDeqPtr.value

  entryRegNext.status.blocked := !isLsqHead
}

object OthersEntry {
  def apply(implicit p: Parameters, iqParams: IssueBlockParams): OthersEntry = {
    iqParams.schdType match {
      case IntScheduler() => new OthersEntry()
      case MemScheduler() =>
        if (iqParams.isLdAddrIQ || iqParams.isStAddrIQ || iqParams.isHyAddrIQ) new OthersEntryMem()
        else if (iqParams.isVecMemAddrIQ) new OthersEntryVecMemAddr()
        else if (iqParams.isVecStDataIQ) new OthersEntryVecMemData()
        else new OthersEntry()
      case VfScheduler() => new OthersEntry()
      case _ => null
    }
  }
}