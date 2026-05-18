package xiangshan.backend.fu.NewCSR

import freechips.rocketchip.devices.debug.DebugModuleKey
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils.ConsecutiveOnes
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRDefines.{
  CSRWARLField => WARL,
  CSRRWField => RW,
  CSRROField => RO,
}
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.backend.fu.NewCSR.CSREvents._
import xiangshan.backend.fu.NewCSR.CSRBundles._
import xiangshan.backend.decode.isa.CSRs
import CSRConfig._
import utility.SignExt
import xiangshan.TriggerAction

import scala.collection.immutable.SeqMap


trait DebugLevel { self: NewCSR =>
  val tselect = Module(new CSRModule("Tselect", new TselectBundle(TriggerNum)) {
    when (this.w.wen && this.w.wdata < TriggerNum.U) {
      reg := this.w.wdata
    }.otherwise {
      reg := reg
    }
  })
    .setAddr(CSRs.tselect)

  val tdata1 = Module(new CSRModule("Tdata1", new Tdata1Bundle) with HasTdataSink {
    regOut := tdataRead.tdata1
  })
    .setAddr(CSRs.tdata1)

  val tdata2 = Module(new CSRModule("Tdata2", new Tdata2Bundle) with HasTdataSink {
    regOut := tdataRead.tdata2
  })
    .setAddr(CSRs.tdata2)

  val tdata3 = Module(new CSRModule("Tdata3", new Tdata3Bundle) with HasTdataSink {
    regOut := tdataRead.tdata3
  })
    .setAddr(CSRs.tdata3)

  val tdata1RegVec: Seq[CSRModule[_]] = Range(0, TriggerNum).map(i =>
    Module(new CSRModule(s"Trigger$i" + s"_Tdata1", new Tdata1Bundle) with HasTriggerBundle {
      when(wen){
        reg := wdata.writeTdata1(canWriteDmode, chainable, dmodeNextTrigger).asUInt
      }
    })
  )
  val tdata2RegVec: Seq[CSRModule[_]] = Range(0, TriggerNum).map(i =>
    Module(new CSRModule(s"Trigger$i" + s"_Tdata2", new Tdata2Bundle))
  )
  val tdata3RegVec: Seq[CSRModule[_]] = Range(0, TriggerNum).map(i =>
    Module(new CSRModule(s"Trigger$i" + s"_Tdata3", new Tdata3Bundle))
  )

  val tinfo = Module(new CSRModule("Tinfo", new TinfoBundle))
    .setAddr(CSRs.tinfo)

  val dcsr = Module(new CSRModule("Dcsr", new DcsrBundle) with TrapEntryDEventSinkBundle with DretEventSinkBundle with HasNmipBundle {
    regOut.NMIP := nmip
  })
    .setAddr(CSRs.dcsr)

  val dpc = Module(new CSRModule("Dpc", new Epc) with TrapEntryDEventSinkBundle)
    .setAddr(CSRs.dpc)

  val dscratch0 = Module(new CSRModule("Dscratch0", new DscratchBundle))
    .setAddr(CSRs.dscratch0)

  val dscratch1 = Module(new CSRModule("Dscratch1", new DscratchBundle))
    .setAddr(CSRs.dscratch1)

  val debugCSRMods = Seq(
    tdata1,
    tdata2,
    tdata3,
    tselect,
    tinfo,
    dcsr,
    dpc,
    dscratch0,
    dscratch1,
  )

  val debugCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_ <: CSRBundle], UInt)] = SeqMap.from(
    debugCSRMods.map(csr => csr.addr -> (csr.w -> csr.rdata)).iterator
  )

  val debugCSROutMap: SeqMap[Int, UInt] = SeqMap.from(
    debugCSRMods.map(csr => csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt).iterator
  )

  private val tdata1Rdata = Mux1H(
    tdata1RegVec.zipWithIndex.map{case (mod, idx) => (tselect.rdata === idx.U) -> mod.rdata}
  )

  private val tdata2Rdata = Mux1H(
    tdata2RegVec.zipWithIndex.map{case (mod, idx) => (tselect.rdata === idx.U) -> mod.rdata}
  )

  private val tdata3Rdata = Mux1H(
    tdata3RegVec.zipWithIndex.map{case (mod, idx) => (tselect.rdata === idx.U) -> mod.rdata}
  )

  debugCSRMods.foreach { mod =>
    mod match {
      case m: HasTdataSink =>
        m.tdataRead.tdata1 := tdata1Rdata
        m.tdataRead.tdata2 := tdata2Rdata
        m.tdataRead.tdata3 := tdata3Rdata
      case _ =>
    }
  }

}

// tselect
class TselectBundle(triggerNum: Int) extends CSRBundle{
  override val len: Int = log2Up(triggerNum)
  val ALL = WARL(len - 1, 0, wNoEffectWhen(WriteTselect)).withReset(0.U)
    .withDescription("Selects the active trigger slot for tdata CSR accesses.")
    .withWarlConstraint(s"Legal write values are 0 to ${triggerNum - 1}; larger values leave the current selection unchanged.")
  def WriteTselect(wdata: UInt) = {
    wdata >= triggerNum.U
  }
}

// tdata1
class Tdata1Bundle extends CSRBundle{
  val TYPE    = Tdata1Type(63, 60, wNoFilter).withReset(Tdata1Type.Disabled)
    .withDescription("Trigger data format encoded in tdata1.")
  val DMODE   = RW(59).withReset(0.U)
    .withDescription("Only debug mode can write this trigger when set.")
  val DATA    = RW(58, 0).withReset(0.U)
    .withDescription("Trigger-format-specific payload. XiangShan uses the mcontrol6 layout.")

  def getTriggerAction: CSREnumType = {
    val res = Wire(new Mcontrol6)
    res := this.asUInt
    res.ACTION
  }

  def writeTdata1(canWriteDmode: Bool, chainable: Bool, dmodeNextTrigger: Bool): Tdata1Bundle = {
    val res = Wire(new Tdata1Bundle)
    res := this.asUInt
    val dmode = this.DMODE.asBool && canWriteDmode
    res.TYPE := this.TYPE.legalize.asUInt
    res.DMODE := dmode
    when(this.TYPE.isLegal) {
      val mcontrol6Res = Wire(new Mcontrol6)
      mcontrol6Res := this.DATA.asUInt
      val chain = chainable && !(!dmode && dmodeNextTrigger)
      res.DATA := mcontrol6Res.writeData(dmode, chain).asUInt
    }.otherwise{
      res.DATA := 0.U
    }
   res
  }
}

class Mcontrol6 extends CSRBundle{
  override val len: Int = 59
  // xiangshan don't support match = NAPOT
  val UNCERTAIN   = RO(26).withReset(0.U)
    .withDescription("Indicates whether address match uncertainty is reported.")
  val HIT1        = RO(25).withReset(0.U)
    .withDescription("Upper hit indication for chained triggers.")
  val VS          = RW(24).withReset(0.U)
    .withDescription("Enable this trigger in VS-mode.")
  val VU          = RW(23).withReset(0.U)
    .withDescription("Enable this trigger in VU-mode.")
  val HIT0        = RO(22).withReset(0.U)
    .withDescription("Primary hit indication for this trigger.")
  val SELECT      = RO(21).withReset(0.U)
    .withDescription("Selects between address and data matching. XiangShan fixes this to address matching.")
  val SIZE        = RO(18, 16).withReset(0.U)
    .withDescription("Access-size match control.")
  val ACTION      = TrigAction(15, 12, wNoFilter).withReset(TrigAction.BreakpointExp)
    .withDescription("Action taken when the trigger fires.")
  val CHAIN       = RW(11).withReset(0.U)
    .withDescription("Chain this trigger with the next trigger slot.")
  val MATCH       = TrigMatch(10, 7, wNoFilter).withReset(TrigMatch.EQ)
    .withDescription("Address matching mode.")
  val M           = RW(6).withReset(0.U)
    .withDescription("Enable this trigger in M-mode.")
  val UNCERTAINEN = RO(5).withReset(0.U)
    .withDescription("Enable reporting of uncertain matches.")
  val S           = RW(4).withReset(0.U)
    .withDescription("Enable this trigger in HS-mode.")
  val U           = RW(3).withReset(0.U)
    .withDescription("Enable this trigger in HU-mode.")
  val EXECUTE     = RW(2).withReset(0.U)
    .withDescription("Match instruction execution addresses.")
  val STORE       = RW(1).withReset(0.U)
    .withDescription("Match store addresses.")
  val LOAD        = RW(0).withReset(0.U)
    .withDescription("Match load addresses.")

  def writeData(dmode: Bool, chainable: Bool): Mcontrol6 = {
    val res = Wire(new Mcontrol6)
    res := this.asUInt
    res.UNCERTAIN   := 0.U
    res.HIT1        := 0.U
    res.HIT0        := 0.U
    res.SELECT      := 0.U
    res.SIZE        := 0.U
    res.ACTION      := this.ACTION.legalize(dmode).asUInt
    res.CHAIN       := this.CHAIN.asBool && chainable
    res.MATCH       := this.MATCH.legalize.asUInt
    res.UNCERTAINEN := 0.U
    res
  }
  def isFetchTrigger: Bool = this.EXECUTE.asBool
  def isMemAccTrigger: Bool = this.STORE || this.LOAD
}


object Tdata1Type extends CSREnum with WARLApply {
  val None         = Value(0.U)
  val Legacy       = Value(1.U)
  val Mcontrol     = Value(2.U)
  val Icount       = Value(3.U)
  val Itrigger     = Value(4.U)
  val Etrigger     = Value(5.U)
  val Mcontrol6    = Value(6.U)
  val Tmexttrigger = Value(7.U)
  val Disabled     = Value(15.U)

  override protected def legalValues: Seq[EnumType] = Seq(Mcontrol6)
  override protected def illegalValueBehavior: Option[String] =
    Some(s"Other writes are legalized to ${Disabled.litValue}=disabled.")

  override def legalize(enumeration: CSREnumType): CSREnumType = {
    val res = WireInit(enumeration)
    when(!enumeration.isLegal){
      res := Disabled.asUInt
    }
    res
  }
}

object TrigAction extends CSREnum with WARLApply {
  val BreakpointExp = Value(0.U) // raise breakpoint exception
  val DebugMode     = Value(1.U) // enter debug mode
  val TraceOn       = Value(2.U)
  val TraceOff      = Value(3.U)
  val TraceNotify   = Value(4.U)

  override def isLegal(enumeration: CSREnumType, dmode: Bool): Bool = enumeration.isOneOf(BreakpointExp) || enumeration.isOneOf(DebugMode) && dmode
  override protected def legalValues: Seq[EnumType] = Seq(BreakpointExp, DebugMode)
  override protected def illegalValueBehavior: Option[String] =
    Some(s"Other values are legalized to ${BreakpointExp.litValue}.")

  override def legalize(enumeration: CSREnumType, dmode: Bool): CSREnumType = {
    val res = WireInit(enumeration)
    when(!enumeration.isLegal(dmode)){
      res := BreakpointExp
    }
    res.asInstanceOf[CSREnumType]
  }
}

object TrigMatch extends CSREnum with WARLApply {
  val EQ        = Value(0.U)
  val NAPOT     = Value(1.U)
  val GE        = Value(2.U)
  val LT        = Value(3.U)
  val MASK_LO   = Value(4.U)
  val MASK_HI   = Value(5.U)
  val NE        = Value(8.U)  // not eq
  val NNAPOT    = Value(9.U)  // not napot
  val NMASK_LO  = Value(12.U) // not mask low
  val NMASK_HI  = Value(13.U) // not mask high
  def isRVSpecLegal(enumeration: CSREnumType) : Bool = enumeration.isOneOf(
    EQ, NAPOT, GE, LT, MASK_LO, MASK_HI,
    NE, NNAPOT, NMASK_LO, NMASK_HI,
  )
  override protected def legalValues: Seq[EnumType] = Seq(EQ, GE, LT)
  override protected def illegalValueBehavior: Option[String] =
    Some(s"Other values are legalized to ${EQ.litValue}.")

  override def legalize(enumeration: CSREnumType): CSREnumType = {
    val res = WireInit(enumeration)
    when(!enumeration.isLegal){
      res := EQ
    }
    res.asInstanceOf[CSREnumType]
  }
}


// tdata2
class Tdata2Bundle extends CSRBundle {
  val ALL = RW(63, 0).withDescription("Second trigger data register.")
}

object Tdata3Sselect extends CSREnum with WARLApply {
  val Ignore   = Value(0.U)
  val Scontext = Value(1.U)
  val Asid     = Value(2.U)

  override protected def legalValues: Seq[EnumType] = Seq(Ignore, Scontext, Asid)
}

object Tdata3Mhselect extends CSREnum with WARLApply {
  val Ignore           = Value(0.U)
  val McontextSelectLo = Value(1.U)
  val VmidSelectLo     = Value(2.U)
  val Mcontext         = Value(4.U)
  val McontextSelectHi = Value(5.U)
  val VmidSelectHi     = Value(6.U)
  override protected def legalValues: Seq[EnumType] =
    Seq(Ignore, McontextSelectLo, VmidSelectLo, Mcontext, McontextSelectHi, VmidSelectHi)
}

class Tdata3Bundle extends CSRBundle {
  val SSELECT   = Tdata3Sselect(1, 0, wNoFilter).withReset(Tdata3Sselect.Ignore)
    .withDescription("Supervisor-side context selector: 0=ignore, 1=scontext, 2=asid, 3=reserved.")
  val SVALUE    = RW(33, 2).withReset(0.U).withDescription("Data used together with sselect.")
  val SBYTEMASK = RW(39, 36).withReset(0.U)
    .withDescription("Per-byte ignore mask for scontext matching. Bit i masks byte i of scontext when sselect=scontext.")
  val MHSELECT  = Tdata3Mhselect(50, 48, wNoFilter).withReset(Tdata3Mhselect.Ignore)
    .withDescription("Machine/hypervisor-side context selector: 0=ignore, 1/5=mcontext_select, 2/6=vmid_select, 3/7=reserved, 4=mcontext.")
  val MHVALUE   = RW(63, 51).withReset(0.U).withDescription("Data used together with mhselect.")
}

// Tinfo
class TinfoBundle extends CSRBundle{
  val VERSION     = TriggerVer(31, 24).withReset(TriggerVer.Spec_1dot0)
    .withDescription("Trigger-information format version field. XiangShan reports version 1, matching the ratified Debug Spec 1.0 trigger encoding.")
  val MCONTROL6EN = RO(6).withReset(1.U)
    .withDescription("Indicates that the mcontrol6 trigger format is supported.")
}

object TriggerVer extends CSREnum with ROApply {
  val Spec_2302  = Value(0.U)
  val Spec_1dot0 = Value(1.U)
}

// Dscratch
class DscratchBundle extends OneFieldBundle(Some("Debug scratch register."))


class DcsrBundle extends CSRBundle {
  override val len: Int = 32
  val DEBUGVER  = DcsrDebugVer(31, 28).withReset(DcsrDebugVer.Spec).withDescription("Debug specification version implemented by this hart.")
  val EXTCAUSE  =           RO(26, 24).withReset(0.U).withDescription("Additional cause detail for debug entry.")
  val CETRIG    =           RW(    19).withReset(0.U).withDescription("Trigger re-entry control for critical-error debug entry.")
  val EBREAKVS  =           RW(    17).withReset(0.U).withDescription("Enter Debug Mode on VS-mode EBREAK.")
  val EBREAKVU  =           RW(    16).withReset(0.U).withDescription("Enter Debug Mode on VU-mode EBREAK.")
  val EBREAKM   =           RW(    15).withReset(0.U).withDescription("Enter Debug Mode on M-mode EBREAK.")
  val EBREAKS   =           RW(    13).withReset(0.U).withDescription("Enter Debug Mode on HS-mode EBREAK.")
  val EBREAKU   =           RW(    12).withReset(0.U).withDescription("Enter Debug Mode on HU-mode EBREAK.")
  val STEPIE    =           RW(    11).withReset(0.U).withDescription("Keep interrupts enabled during single-step execution.")
  val STOPCOUNT =           RW(    10).withReset(0.U).withDescription("Stop architectural counters while in Debug Mode.")
  val STOPTIME  =           RW(     9).withReset(0.U).withDescription("Stop the time counter while in Debug Mode.")
  val CAUSE     =    DcsrCause( 8,  6).withReset(DcsrCause.None).withDescription("Cause of the most recent entry into Debug Mode.")
  val V         =     VirtMode(     5).withReset(VirtMode.Off).withDescription("Virtualization mode active before entering Debug Mode.")
  val MPRVEN    =           RW(     4).withReset(0.U).withDescription("Allow mstatus.MPRV to apply while in Debug Mode.")
  val NMIP      =           RO(     3).withReset(0.U).withDescription("Indicates pending non-maskable interrupt state while in Debug Mode.")
  val STEP      =           RW(     2).withReset(0.U).withDescription("Enable single-step execution.")
  val PRV       =     PrivMode( 1,  0).withReset(PrivMode.M).withDescription("Privilege mode active before entering Debug Mode.")
}

object DcsrDebugVer extends CSREnum with ROApply {
  val None    = Value(0.U)
  val Spec    = Value(4.U)
  val Custom  = Value(15.U)
}

object DcsrCause extends CSREnum with ROApply {
  val None         = Value(0.U)
  val Ebreak       = Value(1.U)
  val Trigger      = Value(2.U)
  val Haltreq      = Value(3.U)
  val Step         = Value(4.U)
  val Resethaltreq = Value(5.U)
  val Group        = Value(6.U)
  val Other        = Value(7.U)
}

trait HasTdataSink { self: CSRModule[_] =>
  val tdataRead = IO(Input(new Bundle {
    val tdata1 = UInt(XLEN.W)
    val tdata2 = UInt(XLEN.W)
    val tdata3 = UInt(XLEN.W)
  }))
}
trait HasTriggerBundle { self: CSRModule[_] =>
  val canWriteDmode = IO(Input(Bool()))
  val chainable = IO(Input(Bool()))
  val dmodeNextTrigger = IO(Input(Bool()))
}

trait HasNmipBundle { self: CSRModule[_] =>
  val nmip = IO(Input(Bool()))
}

/**
 * debug Module MMIO Addr
 */
trait DebugMMIO {
  implicit val p: Parameters

  def debugMMIO = p(DebugModuleKey).get

  def BASE = debugMMIO.baseAddress
  def DebugEntry     = BASE + 0x800
  def DebugException = BASE + 0x808
  def HALTED         = BASE + 0x100
  def GOING          = BASE + 0x104
  def RESUMING       = BASE + 0x108
  def EXCEPTION      = BASE + 0x10C
  def WHERETO        = BASE + 0x300
  def DATA           = BASE + 0x380
  def IMPEBREAK      = DATA - 0x4
  def PROGBUF        = DATA - 4 * debugMMIO.nProgramBufferWords
  def ABSTRACT       = PROGBUF - 4 * (if(debugMMIO.atzero) 2 else 5)
  def FLAGS          = BASE + 0x400
}

object TriggerUtil {
  /**
   * Check if chain vector is legal
   * @param chainVec
   * @param chainLen
   * @return true.B if the max length of chain don't exceed the permitted length
   */
  def TriggerCheckChainLegal(chainVec: Seq[Bool], chainLen: Int): Bool = {
    !ConsecutiveOnes(chainVec, chainLen)
  }

  def triggerActionMatchVec(triggerCanFireVec: Vec[Bool], actionVec: Vec[UInt], targetAction: UInt): Vec[Bool] = {
    VecInit(triggerCanFireVec.zip(actionVec).map {
      case (canFire, action) => canFire && (action === targetAction)
    })
  }

  /**
   * Generate Trigger action
   * @return triggerAction return
   * @param  triggerCanFireVec
   * @param  actionVec tdata.action
   * @param  triggerCanRaiseBpExp from csr
   */
  def triggerActionGen(triggerAction: UInt, triggerCanFireVec: Vec[Bool], actionVec: Vec[UInt], triggerCanRaiseBpExp: Bool): Unit = {
    val fireDebugModeVec = triggerActionMatchVec(triggerCanFireVec, actionVec, TriggerAction.DebugMode)
    val fireBreakpointExpVec = triggerActionMatchVec(triggerCanFireVec, actionVec, TriggerAction.BreakpointExp)
    val fireDebugMode = fireDebugModeVec.asUInt.orR
    val breakPointExp = fireBreakpointExpVec.asUInt.orR && triggerCanRaiseBpExp

    // todo: add more for trace
    triggerAction := MuxCase(TriggerAction.None, Seq(
      fireDebugMode -> TriggerAction.DebugMode,
      breakPointExp -> TriggerAction.BreakpointExp,
    ))
  }

  def textraSMatch(sselect: UInt, svalue: UInt, sbytemask: UInt, scontext: UInt, asid: UInt): Bool = {
    val scontextWire = WireInit(0.U.asTypeOf(Vec(4, UInt(8.W))))
    val scontextEq = (scontext.asTypeOf(scontextWire)).zip(svalue.asTypeOf(scontextWire)).zip(sbytemask.asBools).map{
      case((scon, sval), mask) => (scon === sval || mask)
    }.reduce(_ && _)

    MuxLookup(sselect, false.B)(Seq(
      Tdata3Sselect.Ignore.asUInt -> true.B,
      Tdata3Sselect.Scontext.asUInt -> scontextEq,
      Tdata3Sselect.Asid.asUInt -> (asid === svalue(ASIDLEN - 1, 0))
    ))
  }

  def textraMhMatch(mhselect: UInt, mhvalue: UInt, mhcontext: UInt, vmid: UInt): Bool = {
    val highSelValue = Cat(mhvalue, mhselect(2))
    MuxLookup(mhselect, false.B)(Seq(
      Tdata3Mhselect.Ignore.asUInt -> true.B,
      Tdata3Mhselect.Mcontext.asUInt -> (mhcontext(12, 0) === mhvalue),
      Tdata3Mhselect.McontextSelectLo.asUInt -> (mhcontext === highSelValue),
      Tdata3Mhselect.McontextSelectHi.asUInt -> (mhcontext === highSelValue),
      Tdata3Mhselect.VmidSelectLo.asUInt -> (vmid === highSelValue),
      Tdata3Mhselect.VmidSelectHi.asUInt -> (vmid === highSelValue),
    ))
  }

  def textraMatch(tdata3: Tdata3Bundle, scontext: UInt, asid: UInt, mhcontext: UInt, vmid: UInt): Bool = {
    textraSMatch(tdata3.SSELECT.asUInt, tdata3.SVALUE.asUInt, tdata3.SBYTEMASK.asUInt, scontext, asid) &&
    textraMhMatch(tdata3.MHSELECT.asUInt, tdata3.MHVALUE.asUInt, mhcontext, vmid)
  }
}
