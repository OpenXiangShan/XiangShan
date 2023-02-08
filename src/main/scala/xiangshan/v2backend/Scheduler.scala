package xiangshan.v2backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.backend.rename.{BusyTable, BusyTableReadIO}
import xiangshan.v2backend.Bundles.{DynInst, IssueQueueWakeUpBundle}
import xiangshan.v2backend.dispatch.{Dispatch2Iq, Dispatch2IqImp}
import xiangshan.v2backend.issue._
import xiangshan.{HasXSParameter, Redirect, ResetPregStateReq, XSBundle}

sealed trait SchedulerType

case class IntScheduler() extends SchedulerType
case class MemScheduler() extends SchedulerType
case class VfScheduler() extends SchedulerType
case class NoScheduler() extends SchedulerType

class Scheduler(val params: SchdBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val dispatch2Iq = LazyModule(new Dispatch2Iq(params))
  val issueQueue = params.issueBlockParams.map(x => LazyModule(new IssueQueue(x.genIqParams)))

  lazy val module = new SchedulerImp(this)(params, p)
}

class SchedulerIO()(implicit params: SchdBlockParams, p: Parameters) extends XSBundle {
  val fromTop = new Bundle {
    val hartId = Input(UInt(8.W))
  }
  val fromRob = new Bundle {
    val flush = Flipped(ValidIO(new Redirect))
  }
  val fromCtrlBlock = new Bundle {
    val jmpPc = Input(UInt(VAddrBits.W))
    val targetPc = Input(UInt(VAddrBits.W))
  }
  val fromDispatch = new Bundle {
    val allocPregs = Vec(RenameWidth, Input(new ResetPregStateReq))
    val uops =  Vec(params.numUopIn, Flipped(DecoupledIO(new DynInst)))
  }
  val writeback = Vec(params.numWakeupFromWB, Flipped(ValidIO(new IssueQueueWakeUpBundle())))
  val toDataPath: MixedVec[MixedVec[DecoupledIO[Bundles.IssueQueueIssueBundle]]] = MixedVec(params.issueBlockParams.map(_.genIqParams.genIssueBundle))
}

class SchedulerImp(wrapper: Scheduler)(implicit params: SchdBlockParams, p: Parameters)
  extends LazyModuleImp(wrapper)
    with HasXSParameter
{
  val io = IO(new SchedulerIO())

  // alias
  private val schdType = params.schdType
  private val (numRfRead, numRfWrite) = params.numRfReadWrite.getOrElse((0, 0))
  private val numPregs = params.numPregs

  // Modules
  val dispatch2Iq: Dispatch2IqImp = wrapper.dispatch2Iq.module
  val issueQueues: Seq[IssueQueueImp] = wrapper.issueQueue.map(_.module)

  private val (vfBusyTableRead, vfBusyTableWb) = schdType match {
    case VfScheduler() => (
      Some(Vec(params.numRfRead, new BusyTableReadIO)),
      Some(Vec(numRfWrite, ValidIO(UInt(params.pregIdxWidth.W))))
    )
    case _ => (None, None)
  }

  // BusyTable Modules
  val intBusyTable = schdType match {
    case IntScheduler() => Some(Module(new BusyTable(dispatch2Iq.numIntStateRead, params.numRfWrite)))
    case _ => None
  }

  val vfBusyTable = schdType match {
    case VfScheduler() => Some(Module(new BusyTable(vfBusyTableRead.get.length, vfBusyTableWb.get.length)))
    case _ => None
  }

  dispatch2Iq.io match { case dp2iq =>
    dp2iq.redirect <> io.fromRob.flush
    dp2iq.in <> io.fromDispatch.uops
    dp2iq.readIntState.foreach(_ <> intBusyTable.get.io.read)
  }

  issueQueues.zipWithIndex.foreach { case (iq, i) =>
    iq.io.flush <> io.fromRob.flush
    iq.io.enq <> dispatch2Iq.io.out(i)
    iq.io.wakeup <> io.writeback
    iq match {
      case imp: IssueQueueIntImp =>
        if (imp.io.jmp.nonEmpty) {
          imp.io.jmp.get.target := io.fromCtrlBlock.targetPc
          imp.io.jmp.get.pc := io.fromCtrlBlock.jmpPc
        }
      case _ =>
    }
    iq.io.deqResp := 0.U.asTypeOf(iq.io.deqResp) // Todo
  }

  intBusyTable match {
    case Some(bt) =>
      bt.io.allocPregs.zip(io.fromDispatch.allocPregs).foreach { case (btAllocPregs, dpAllocPregs) =>
        btAllocPregs.valid := dpAllocPregs.isInt
        btAllocPregs.bits := dpAllocPregs.preg
      }
      bt.io.wbPregs.zipWithIndex.foreach { case (wb, i) =>
        wb.valid := io.writeback(i).valid
        wb.bits := io.writeback(i).bits.pdest
      }
    case None =>
  }

  vfBusyTable match {
    case Some(bt) =>
      bt.io.read <> vfBusyTableRead.get
      bt.io.allocPregs.zip(io.fromDispatch.allocPregs).foreach { case (btAllocPregs, dpAllocPregs) =>
        btAllocPregs.valid := dpAllocPregs.isFp
        btAllocPregs.bits := dpAllocPregs.preg
      }
      bt.io.wbPregs <> vfBusyTableWb.get
    case None =>
  }

  io.toDataPath.zipWithIndex.foreach { case (toDp, i) =>
    toDp <> issueQueues(i).io.deq
  }
}
