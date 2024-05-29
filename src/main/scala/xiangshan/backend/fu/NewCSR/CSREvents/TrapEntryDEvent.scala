package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util.{MuxCase, _}
import org.chipsalliance.cde.config.Parameters
import utility.{SignExt, ZeroExt}
import xiangshan.{ExceptionNO, HasXSParameter, TriggerCf}
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.NewCSR
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, OneFieldBundle, PrivState}
import xiangshan.backend.fu.NewCSR.CSRConfig.{VaddrMaxWidth, XLEN}
import xiangshan.backend.fu.NewCSR.CSRDefines.SatpMode
import xiangshan.backend.fu.NewCSR._


class TrapEntryDEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase {
  val dcsr            = ValidIO((new DcsrBundle).addInEvent(_.CAUSE, _.PRV))
  val dpc             = ValidIO((new Dpc       ).addInEvent(_.ALL))
  val targetPc        = ValidIO(UInt(VaddrMaxWidth.W))
  val debugMode       = ValidIO(Bool())
  val debugIntrEnable = ValidIO(Bool())

  def getBundleByName(name: String): Valid[CSRBundle] = {
    name match {
      case "dcsr" => this.dcsr
      case "dpc"  => this.dpc
    }
  }
}

class TrapEntryDEventInput(implicit override val p: Parameters) extends TrapEntryEventInput{
  val hasTrap                 = Input(Bool())
  val debugMode               = Input(Bool())
  val hasDebugIntr            = Input(Bool())
  val hasTriggerFire          = Input(Bool())
  val hasDebugEbreakException = Input(Bool())
  val hasSingleStep           = Input(Bool())
  val breakPoint              = Input(Bool())
}

class TrapEntryDEventModule(implicit val p: Parameters) extends Module with CSREventBase with DebugMMIO {
  val in = IO(new TrapEntryDEventInput)
  val out = IO(new TrapEntryDEventOutput)

  private val current                 = in
  private val hasTrap                 = in.hasTrap
  private val debugMode               = in.debugMode
  private val hasDebugIntr            = in.hasDebugIntr
  private val breakPoint              = in.breakPoint
  private val hasTriggerFire          = in.hasTriggerFire
  private val hasDebugEbreakException = in.hasDebugEbreakException
  private val hasSingleStep           = in.hasSingleStep

  private val hasExceptionInDmode = debugMode && hasTrap
  val causeIntr = DcsrCause.Haltreq.asUInt
  val causeExp = MuxCase(0.U, Seq(
    hasTriggerFire          -> DcsrCause.Trigger.asUInt,
    hasDebugEbreakException -> DcsrCause.Ebreak.asUInt,
    hasSingleStep           -> DcsrCause.Step.asUInt
  ))

  private val trapPC = Wire(UInt(XLEN.W))
  private val ivmHS = !current.iMode.isModeHS && current.satp.MODE =/= SatpMode.Bare
  private val ivmVS = !current.iMode.isModeVS && current.vsatp.MODE =/= SatpMode.Bare
  // When enable virtual memory, the higher bit should fill with the msb of address of Sv39/Sv48/Sv57
  trapPC := Mux(ivmHS || ivmVS, SignExt(in.trapPc, XLEN), ZeroExt(in.trapPc, XLEN))

  // ebreak jump debugEntry not debugException in dmode
  // debug rom make hart write 0 to DebugMMIO.EXCEPTION when exception happened in debugMode.
  // let debug module known hart got an exception.
  // note: Need't know exception number in debugMode.
  //       exception(EX_BP) must be ebreak here!
  val debugPc = Mux(hasExceptionInDmode && !breakPoint, DebugException.U, DebugEntry.U)

  out := DontCare
  // output
  out.dcsr.valid            := valid
  out.dpc.valid             := valid
  // !debugMode trap || debugMode hasExp
  out.targetPc.valid        := valid || hasExceptionInDmode
  out.debugMode.valid       := valid
  out.privState.valid       := valid
  out.debugIntrEnable.valid := valid

  out.dcsr.bits.PRV         := current.privState.PRVM.asUInt
  out.dcsr.bits.CAUSE       := Mux(hasDebugIntr, causeIntr, causeExp)
  out.dpc.bits              := trapPC

  out.targetPc.bits         := debugPc
  out.debugMode.bits        := true.B
  out.privState.bits        := PrivState.ModeM
  out.debugIntrEnable.bits  := false.B

}

trait TrapEntryDEventSinkBundle { self: CSRModule[_] =>
  val trapToD = IO(Flipped(new TrapEntryDEventOutput))

  private val updateBundle: ValidIO[CSRBundle] = trapToD.getBundleByName(self.modName.toLowerCase())

  (reg.asInstanceOf[CSRBundle].getFields zip updateBundle.bits.getFields).foreach { case (sink, source) =>
    if (updateBundle.bits.eventFields.contains(source)) {
      when(updateBundle.valid) {
        sink := source
      }
    }
  }
}
