package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import CSRConfig._
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRDefines.{
  CSRWARLField => WARL,
  CSRRWField => RW,
  CSRROField => RO,
}
import xiangshan.backend.fu.NewCSR.CSRFunc._

import scala.collection.immutable.SeqMap

trait CSRDebugTrigger { self: NewCSR =>
  val dcsr = Module(new CSRModule("dcsr", new DcsrBundle))
    .setAddr(0x7B0)

  val dpc = Module(new CSRModule("dpc", new Dpc))
    .setAddr(0x7B1)

  val dscratch0 = Module(new CSRModule("dscratch0"))
    .setAddr(0x7B2)

  val dscratch1 = Module(new CSRModule("dscratch1"))
    .setAddr(0x7B3)

  val debugCSRMods = Seq(
    dcsr,
    dpc,
    dscratch0,
    dscratch1,
  )

  val debugCSRMap = SeqMap.from(
    debugCSRMods.map(csr => (csr.addr -> (csr.w -> csr.rdata.asInstanceOf[CSRBundle].asUInt))).iterator
  )

  val debugCSROutMap: SeqMap[Int, UInt] = SeqMap.from(
    debugCSRMods.map(csr => (csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt)).iterator
  )
}

class DcsrBundle extends CSRBundle {
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
  val CAUSE     =    DcsrCause( 8,  6).withReset(DcsrCause.none)
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
  val None = Value(0.U)
  val Spec = Value(4.U)
  val Custom = Value(15.U)
}

object DcsrCause extends CSREnum with ROApply {
  val none         = Value(0.U)
  val ebreak       = Value(1.U)
  val trigger      = Value(2.U)
  val haltreq      = Value(3.U)
  val step         = Value(4.U)
  val resethaltreq = Value(5.U)
  val group        = Value(6.U)
}
