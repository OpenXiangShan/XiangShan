package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{SignExt, ZeroExt}
import xiangshan.{ExceptionNO, HasXSParameter}
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, OneFieldBundle, PrivState}
import xiangshan.backend.fu.NewCSR.CSRConfig._
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.util.CSRConst

trait CSREvents { self: NewCSR =>
  val trapEntryMEvent = Module(new TrapEntryMEventModule)

  val trapEntryHSEvent = Module(new TrapEntryHSEventModule)

  val trapEntryVSEvent = Module(new TrapEntryVSEventModule)

  val mretEvent = Module(new MretEventModule)

  val sretEvent = Module(new SretEventModule)

  val dretEvent = Module(new DretEventModule)

  val events: Seq[Module with CSREventBase] = Seq(
    trapEntryMEvent,
    trapEntryHSEvent,
    trapEntryVSEvent,
    mretEvent,
    sretEvent,
    dretEvent,
  )

  events.foreach(x => dontTouch(x.out))

  val trapEntryEvents: Seq[Module with CSREventBase] = Seq(
    trapEntryMEvent,
    trapEntryHSEvent,
    trapEntryVSEvent,
  )
}

trait EventUpdatePrivStateOutput {
  val privState = ValidIO(new PrivState)
}

trait EventOutputBase {
  def getBundleByName(name: String): Valid[CSRBundle]
}

trait CSREventBase {
  val valid = IO(Input(Bool()))
  val in: Bundle
  val out: Bundle
}

class TrapEntryEventInput(implicit val p: Parameters) extends Bundle with HasXSParameter {
  val causeNO = Input(new CauseBundle)
  val trapPc = Input(UInt(VaddrMaxWidth.W))
  val isCrossPageIPF = Input(Bool())

  // always current privilege
  val iMode = Input(new PrivState())
  // take MRPV into consideration
  val dMode = Input(new PrivState())
  // status
  val privState = Input(new PrivState)
  val mstatus = Input(new MstatusBundle)
  val hstatus = Input(new HstatusBundle)
  val sstatus = Input(new SstatusBundle)
  val vsstatus = Input(new SstatusBundle)
  val satp = Input(new SatpBundle)
  val vsatp = Input(new SatpBundle)
  // from mem
  val memExceptionVAddr = Input(UInt(VAddrBits.W))
  val memExceptionGPAddr = Input(UInt(GPAddrBits.W))
}
