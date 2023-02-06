package xiangshan.v2backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.backend.rename.{BusyTable, BusyTableReadIO}
import xiangshan.v2backend.Bundles.{DynInst, ExuInput, WriteBackBundle}
import xiangshan.v2backend.issue._
import xiangshan.{HasXSParameter, Redirect, ResetPregStateReq, XSBundle}

sealed trait SchedulerType

case class IntScheduler() extends SchedulerType
case class MemScheduler() extends SchedulerType
case class VfScheduler() extends SchedulerType
case class NoScheduler() extends SchedulerType

class Scheduler(val params: SchdBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {

  lazy val module = new SchedulerImp(this)(params, p)
}

class SchedulerIO()(implicit params: SchdBlockParams, p: Parameters) extends XSBundle {
  val fromTop = new Bundle {
    val hartId = Input(UInt(8.W))
  }
  val fromRob = new Bundle {
    val flush = Flipped(ValidIO(new Redirect))
  }
  val fromDispatch = new Bundle {
    val allocPregs = Vec(RenameWidth, Input(new ResetPregStateReq))
    val uops =  Vec(params.numUopIn, Flipped(DecoupledIO(new DynInst)))
  }
  val writeback = params.numRfReadWrite.map(x => Vec(x._2, new WriteBackBundle(params.rfDataWidth)))
  val toDataPath = new Bundle {
    val exuInput: MixedVec[MixedVec[DecoupledIO[ExuInput]]] = MixedVec(params.issueBlockParams.map(_.genIqParams.genIssueBundle))

    val rfRead = MixedVec(
      params.issueBlockParams.map(_.genIqParams.genReadRfBundle)
    )
  }
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
  private val splitNum = schdType match {
    case VfScheduler() => VLEN / XLEN
    case _ => 1
  }

  private val rfRaddr = Wire(Vec(numRfRead, UInt(VAddrBits.W)))
  private val rfRdata = Wire(Vec(numRfRead, UInt(params.rfDataWidth.W)))
  private val rfWen = Wire(Vec(splitNum, Vec(numRfWrite, Bool())))
  private val rfWaddr = Wire(Vec(numRfWrite, UInt(VAddrBits.W)))
  private val rfWdata = Wire(Vec(numRfWrite, UInt(params.rfDataWidth.W)))

  // RegFile Modules
  schdType match {
    case IntScheduler() => IntRegFile("IntRegFile", numPregs, rfRaddr, rfRdata, rfWen(0), rfWaddr, rfWdata, None, None)
    case VfScheduler() => VfRegFile("VfRegFile", numPregs, splitNum, rfRaddr, rfRdata, rfWen, rfWaddr, rfWdata, None, None)
    case _ =>
  }

  // BusyTable Modules
  val intBusyTable = schdType match {
    case IntScheduler() => Some(Module(new BusyTable(intBusyTableRead.get.length, intBusyTableWb.get.length)))
    case _ => None
  }

  val vfBusyTable = schdType match {
    case VfScheduler() => Some(Module(new BusyTable(vfBusyTableRead.get.length, vfBusyTableWb.get.length)))
    case _ => None
  }

  val issueQueueWrappers = wrapper.params.issueBlockParams.map(x => LazyModule(new IssueQueue(x.genIqParams)))
  val issueQueues = issueQueueWrappers.map(_.module)

  private val (intBusyTableRead, intBusyTableWb) = schdType match {
    case IntScheduler() => (
      Some(Vec(params.numRfRead, new BusyTableReadIO)),
      Some(Vec(numRfWrite, ValidIO(UInt(params.pregIdxWidth.W))))
    )
    case _ => (None, None)
  }

  private val (vfBusyTableRead, vfBusyTableWb) = schdType match {
    case VfScheduler() => (
      Some(Vec(params.numRfRead, new BusyTableReadIO)),
      Some(Vec(numRfWrite, ValidIO(UInt(params.pregIdxWidth.W))))
    )
    case _ => (None, None)
  }

  intBusyTable match {
    case Some(bt) =>
      bt.io.read <> intBusyTableRead.get
      bt.io.allocPregs.zip(io.fromDispatch.allocPregs).foreach { case (btAllocPregs, dpAllocPregs) =>
        btAllocPregs.valid := dpAllocPregs.isInt
        btAllocPregs.bits := dpAllocPregs.preg
      }
      bt.io.wbPregs <> intBusyTableWb.get
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

//  issueQueues.zipWithIndex.foreach { case (iq, i) =>
//    iq.io.flush <> io.fromRob.flush
//    iq.io.enq.zip(io.fromDispatch.uops).foreach { case (iqEnq, dpUop) =>
//      iqEnq.valid := s1
//
//    }
//  }

  // valid singals need initialize, while uop data need not
  val s1_uopsValid: Vec[Bool] = RegInit(VecInit(Seq.fill(params.numUopIn)(false.B)))
  val s1_uopsVec = Reg(Vec(params.numUopIn, new DynInst))

  val uopsInFired = io.fromDispatch.uops.map(_.fire)
  val uopsOutFired = io.toDataPath.exuInput.flatten.zip(io.toDataPath.rfRead).map{ case (exuInput, rfRead) => exuInput.fire && rfRead.fire}

  s1_uopsValid.zipWithIndex.foreach { case (valid, i) =>
    when (io.fromRob.flush.valid) {
      valid := false.B
    }.elsewhen(uopsOutFired(i)) {
      valid := uopsInFired(i)
    }
  }

  s1_uopsVec.zipWithIndex.foreach { case (uop, i) =>
    when (!io.fromRob.flush.valid) {
      when (uopsInFired(i) && (uopsOutFired(i) || !s1_uopsValid(i))) {
        uop := io.fromDispatch.uops(i)
      }
    }
  }


}
