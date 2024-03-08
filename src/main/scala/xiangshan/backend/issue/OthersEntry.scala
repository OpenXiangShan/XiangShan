package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{HasCircularQueuePtrHelper, GatedValidRegNext}
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

  def wakeup          = commonIn.wakeUpFromWB ++ commonIn.wakeUpFromIQ
}

class OthersEntry(isComp: Boolean)(implicit p: Parameters, params: IssueBlockParams) extends XSModule {
  val io = IO(new OthersEntryIO)

  val common          = Wire(new CommonWireBundle)
  val entryUpdate     = Wire(new EntryBundle)
  val entryRegNext    = Wire(new EntryBundle)
  val hasWakeupIQ     = OptionWrapper(params.hasIQWakeUp, Wire(new CommonIQWakeupBundle))

  //Reg
  val validReg = GatedValidRegNext(common.validRegNext, false.B)
  val entryReg = RegEnable(entryRegNext, validReg || common.validRegNext)

  //Wire
  CommonWireConnect(common, hasWakeupIQ, validReg, entryReg.status, io.commonIn, false)

  if (params.hasIQWakeUp) {
    ShiftLoadDependency(hasWakeupIQ.get)
    CommonIQWakeupConnect(common, hasWakeupIQ.get, validReg, entryReg.status, io.commonIn, false)
  }

  when(io.commonIn.enq.valid) {
    assert(common.enqReady, "Entry is not ready when enq is valid\n")
  }

  when(io.commonIn.enq.valid) {
    entryRegNext := io.commonIn.enq.bits
  }.otherwise {
    entryRegNext := entryUpdate
  }

  EntryRegCommonConnect(common, hasWakeupIQ, validReg, entryUpdate, entryReg, entryReg.status, io.commonIn, false)

  //output
  CommonOutConnect(io.commonOut, common, hasWakeupIQ, validReg, entryUpdate, entryReg, entryReg.status, io.commonIn, false, isComp)
}

class OthersEntryVecMem(isComp: Boolean)(implicit p: Parameters, params: IssueBlockParams) extends OthersEntry(isComp)
  with HasCircularQueuePtrHelper {

  require(params.isVecMemIQ, "OthersEntryVecMem can only be instance of VecMem IQ")

  EntryVecMemConnect(io.commonIn, common, validReg, entryReg, entryRegNext, entryUpdate)
}

object OthersEntry {
  def apply(isComp: Boolean)(implicit p: Parameters, iqParams: IssueBlockParams): OthersEntry = {
    iqParams.schdType match {
      case IntScheduler() => new OthersEntry(isComp)
      case MemScheduler() =>
        if (iqParams.isVecMemIQ) new OthersEntryVecMem(isComp)
        else new OthersEntry(isComp)
      case VfScheduler() => new OthersEntry(isComp)
      case _ => null
    }
  }
}