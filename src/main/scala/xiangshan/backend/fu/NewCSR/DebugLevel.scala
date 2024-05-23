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
import CSRConfig._
import scala.collection.immutable.SeqMap


trait DebugLevel { self: NewCSR =>
  val tselect = Module(new CSRModule("Tselect", new TselectBundle(TriggerNum)))
    .setAddr(0x7A0)

  val tdata1 = Module(new CSRModule("Tdata1") with HasTdataSink {
    rdata := tdataRead.tdata1
    regOut := 0.U
  })
    .setAddr(0x7A1)

  val tdata2 = Module(new CSRModule("Tdata2") with HasTdataSink {
    rdata := tdataRead.tdata2
    regOut := 0.U
  })
    .setAddr(0x7A2)

  val tdata1RegVec: Seq[CSRModule[_]] = Range(0, TriggerNum).map(i =>
    Module(new CSRModule(s"Trigger$i" + s"_Tdata1", new Tdata1Bundle) with HasdebugModeBundle{
      when(wen){
        reg := wdata.writeTdata1(debugMode, chainable).asUInt
      }
    })
  )
  val tdata2RegVec: Seq[CSRModule[_]] = Range(0, TriggerNum).map(i =>
    Module(new CSRModule(s"Trigger$i" + s"_Tdata2", new Tdata2Bundle))
  )

  val tinfo = Module(new CSRModule("Tinfo", new TinfoBundle))
    .setAddr(0x7A4)

  val dcsr = Module(new CSRModule("Dcsr", new DcsrBundle) with TrapEntryDEventSinkBundle with DretEventSinkBundle)
    .setAddr(0x7B0)

  val dpc = Module(new CSRModule("Dpc", new Dpc) with TrapEntryDEventSinkBundle)
    .setAddr(0x7B1)

  val dscratch0 = Module(new CSRModule("Dscratch0", new DscratchBundle))
    .setAddr(0x7B2)

  val dscratch1 = Module(new CSRModule("Dscratch1", new DscratchBundle))
    .setAddr(0x7B3)

  val debugCSRMods = Seq(
    tdata1,
    tdata2,
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

  debugCSRMods.foreach { mod =>
    mod match {
      case m: HasTdataSink =>
        m.tdataRead.tdata1 := tdata1Rdata
        m.tdataRead.tdata2 := tdata2Rdata
      case _ =>
    }
  }

}

// tselect
class TselectBundle(triggerNum: Int) extends CSRBundle{
  override val len: Int = log2Up(triggerNum)
  val ALL = WARL(tselectWidth - 1, 0, wNoEffectWhen(WriteTselect)) // todo: paramterize
  def WriteTselect(wdata: UInt) = {
    wdata >= triggerNum.U
  }
}

// tdata1
class Tdata1Bundle extends CSRBundle{
  val TYPE    = Tdata1Type(63, 60, wNoFilter).withReset(Tdata1Type.Disabled)
  val DMODE   = RW(59).withReset(0.U)
  val DATA    = RW(58, 0)

  def getTriggerAction: CSREnumType = {
    val res = Wire(new Mcontrol)
    res := this.asUInt
    res.ACTION
  }

  def writeTdata1(debugMode: Bool, chainable: Bool): Tdata1Bundle = {
    val res = Wire(new Tdata1Bundle)
    res := this.asUInt
    val dmode = this.DMODE.asBool && debugMode
    res.TYPE := this.TYPE.legalize.asUInt
    res.DMODE := dmode
    when(this.TYPE.isLegal) {
      val mcontrolRes = Wire(new Mcontrol)
      mcontrolRes := this.DATA.asUInt
      res.DATA := mcontrolRes.writeData(dmode, chainable).asUInt
    }.otherwise{
      res.DATA := 0.U
    }
   res
  }
}

class Mcontrol extends CSRBundle{
  override val len: Int = 59
  // xiangshan don't support match = NAPOT
  val MASKMAX = RO(58, 53).withReset(0.U)
  val SIZEHI  = RW(22, 21).withReset(0.U)
  val HIT     = RW(20).withReset(0.U)
  val SELECT  = RW(19).withReset(0.U)
  val TIMING  = RW(18).withReset(0.U)
  val SIZELO  = RW(17, 16).withReset(0.U)
  val ACTION  = TrigAction(15, 12, wNoFilter).withReset(TrigAction.BreakpointExp)
  val CHAIN   = RW(11).withReset(0.U)
  val MATCH   = TrigMatch(10, 7, wNoFilter).withReset(TrigMatch.EQ)
  val M       = RW(6).withReset(0.U)
  val S       = RW(4).withReset(0.U)
  val U       = RW(3).withReset(0.U)
  val EXECUTE = RW(2).withReset(0.U)
  val STORE   = RW(1).withReset(0.U)
  val LOAD    = RW(0).withReset(0.U)

  def writeData(dmode: Bool, chainable: Bool): Mcontrol = {
    val res = Wire(new Mcontrol)
    res := this.asUInt
    res.MASKMAX     := 0.U
    res.SIZEHI      := 0.U
    res.HIT         := false.B
    res.SELECT      := this.EXECUTE.asBool && this.SELECT.asBool
    res.TIMING      := false.B
    res.SIZELO      := 0.U
    res.ACTION      := this.ACTION.legalize(dmode).asUInt
    res.CHAIN       := this.CHAIN.asBool && chainable
    res.MATCH       := this.MATCH.legalize.asUInt
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

  override def isLegal(enum: CSREnumType): Bool = enum.isOneOf(Mcontrol)

  override def legalize(enum: CSREnumType): CSREnumType = {
    val res = WireInit(enum)
    when(!enum.isLegal){
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

  override def isLegal(enum: CSREnumType, dmode: Bool): Bool = enum.isOneOf(BreakpointExp) || enum.isOneOf(DebugMode) && dmode

  override def legalize(enum: CSREnumType, dmode: Bool): CSREnumType = {
    val res = WireInit(enum)
    when(!enum.isLegal(dmode)){
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
  def isRVSpecLegal(enum: CSREnumType) : Bool = enum.isOneOf(
    EQ, NAPOT, GE, LT, MASK_LO, MASK_HI,
    NE, NNAPOT, NMASK_LO, NMASK_HI,
  )
  override def isLegal(enum: CSREnumType): Bool = enum.isOneOf(EQ, GE, LT)

  override def legalize(enum: CSREnumType): CSREnumType = {
    val res = WireInit(enum)
    when(!enum.isLegal){
      res := EQ
    }
    res.asInstanceOf[CSREnumType]
  }
}


// tdata2
class Tdata2Bundle extends CSRBundle{
  val ALL = RW(63, 0)
}

// Tinfo
class TinfoBundle extends CSRBundle{
  // Version isn't in version 0.13
  val VERSION     = RO(31, 24).withReset(0.U)
  // only support mcontrol
  val MCONTROLEN  = RO(2).withReset(1.U)
}

// Dscratch
class DscratchBundle extends CSRBundle{
  val ALL   = RW(63, 0)
}


class DcsrBundle extends CSRBundle {
  override val len: Int = 32
  val DEBUGVER  = DcsrDebugVer(31, 28).withReset(DcsrDebugVer.Spec) // Debug implementation as it described in 0.13 draft // todo
  // All ebreak Privileges are RW, instead of WARL, since XiangShan support U/S/VU/VS.
  val EBREAKVS  =           RW(    17).withReset(0.U)
  val EBREAKVU  =           RW(    16).withReset(0.U)
  val EBREAKM   =           RW(    15).withReset(0.U)
  val EBREAKS   =           RW(    13).withReset(0.U)
  val EBREAKU   =           RW(    12).withReset(0.U)
  // STEPIE is RW, instead of WARL, since XiangShan support interrupts being enabled single stepping.
  val STEPIE    =           RW(    11).withReset(0.U)
  val STOPCOUNT =           RO(    10).withReset(0.U) // Stop count updating has not been supported
  val STOPTIME  =           RO(     9).withReset(0.U) // Stop time updating has not been supported
  val CAUSE     =    DcsrCause( 8,  6).withReset(DcsrCause.None)
  val V         =     VirtMode(     5).withReset(VirtMode.Off)
  // MPRVEN is RW, instead of WARL, since XiangShan support use mstatus.mprv in debug mode
  // Whether use mstatus.mprv
  val MPRVEN    =           RW(     4).withReset(0.U)
  // TODO: support non-maskable interrupt
  val NMIP      =           RO(     3).withReset(0.U)
  // MPRVEN is RW, instead of WARL, since XiangShan support use mstatus.mprv in debug mode
  val STEP      =           RW(     2).withReset(0.U)
  val PRV       =     PrivMode( 1,  0).withReset(PrivMode.M)
}

class Dpc extends CSRBundle {
  val ALL = RW(63, 1)
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
}

trait HasTdataSink { self: CSRModule[_] =>
  val tdataRead = IO(Input(new Bundle {
    val tdata1 = UInt(XLEN.W) // Todo: check if use ireg bundle, and shrink the width
    val tdata2 = UInt(XLEN.W)
  }))
}
trait HasdebugModeBundle { self: CSRModule[_] =>
  val debugMode = IO(Input(Bool()))
  val chainable = IO(Input(Bool()))
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
}