package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{SignExt, ZeroExt}
import xiangshan.HasXSParameter
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, PrivState}
import xiangshan.backend.fu.NewCSR.CSRConfig._
import xiangshan.backend.fu.NewCSR.CSRDefines.{HgatpMode, SatpMode}
import xiangshan.backend.fu.NewCSR._

trait CSREvents { self: NewCSR =>
  val trapEntryDEvent = Module(new TrapEntryDEventModule)

  val trapEntryMEvent = Module(new TrapEntryMEventModule)

  val trapEntryHSEvent = Module(new TrapEntryHSEventModule)

  val trapEntryVSEvent = Module(new TrapEntryVSEventModule)

  val mretEvent = Module(new MretEventModule)

  val sretEvent = Module(new SretEventModule)

  val dretEvent = Module(new DretEventModule)

  val events: Seq[Module with CSREventBase] = Seq(
    trapEntryDEvent,
    trapEntryMEvent,
    trapEntryHSEvent,
    trapEntryVSEvent,
    mretEvent,
    sretEvent,
    dretEvent,
  )

  events.foreach(x => dontTouch(x.out))

  val trapEntryEvents: Seq[Module with CSREventBase] = Seq(
    trapEntryDEvent,
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

  def genTrapVA(
    transMode: PrivState,
    satp: SatpBundle,
    vsatp: SatpBundle,
    hgatp: HgatpBundle,
    addr: UInt,
  ) = {
    require(addr.getWidth >= 41)

    val isBare =
      transMode.isModeM ||
      transMode.isModeHSorHU &&  satp.MODE === SatpMode.Bare ||
      transMode.isVirtual    && vsatp.MODE === SatpMode.Bare && hgatp.MODE === HgatpMode.Bare
    val isSv39 =
      transMode.isModeHSorHU &&  satp.MODE === SatpMode.Sv39 ||
      transMode.isVirtual    && vsatp.MODE === SatpMode.Sv39
    val isSv48 =
      transMode.isModeHSorHU &&  satp.MODE === SatpMode.Sv48 ||
      transMode.isVirtual    && vsatp.MODE === SatpMode.Sv48
    val isSv39x4 =
      transMode.isVirtual && vsatp.MODE === SatpMode.Bare && hgatp.MODE === HgatpMode.Sv39x4
    val isSv48x4 =
      transMode.isVirtual && vsatp.MODE === SatpMode.Bare && hgatp.MODE === HgatpMode.Sv48x4

    val bareAddr   = ZeroExt(addr(PAddrWidth - 1, 0), XLEN)
    // When enable virtual memory, the higher bit should fill with the msb of address of Sv39/Sv48/Sv57
    val sv39Addr   = SignExt(addr(38, 0), XLEN)
    val sv39x4Addr = SignExt(addr(40, 0), XLEN)

    val trapAddr = Mux1H(Seq(
      isBare -> bareAddr,
      isSv39 -> sv39Addr,
      isSv39x4 -> sv39x4Addr,
    ))

    trapAddr
  }
}

class TrapEntryEventInput(implicit val p: Parameters) extends Bundle with HasXSParameter {
  val causeNO = Input(new CauseBundle)
  val trapPc = Input(UInt(VaddrMaxWidth.W))
  val trapPcGPA = Input(UInt(GPAddrBits.W))
  val isCrossPageIPF = Input(Bool())
  val isHls = Input(Bool())

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

  val tcontrol = Input(new TcontrolBundle)

  val pcFromXtvec = Input(UInt(VaddrMaxWidth.W))

  val satp = Input(new SatpBundle)
  val vsatp = Input(new SatpBundle)
  val hgatp = Input(new HgatpBundle)
  // from mem
  val memExceptionVAddr = Input(UInt(VAddrBits.W))
  val memExceptionGPAddr = Input(UInt(GPAddrBits.W))
}
