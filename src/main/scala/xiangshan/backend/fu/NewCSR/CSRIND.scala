package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import CSRConfig._
import system.HasSoCParameter
import xiangshan.backend.fu.NewCSR.CSRBundles._
import xiangshan.backend.fu.NewCSR.CSRConfig._
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW, _}
import xiangshan.backend.decode.isa.CSRs
import xiangshan.XSBundle

import scala.collection.immutable.SeqMap

trait CSRIND { self: NewCSR with HypervisorLevel =>
  val mireg2 = Module(new CSRModule("Mireg2") {
    rdata := 0.U
  })
    .setAddr(CSRs.mireg2)

  val mireg3 = Module(new CSRModule("Mireg3") {
    rdata := 0.U
  })
    .setAddr(CSRs.mireg3)

  val mireg4 = Module(new CSRModule("Mireg4") {
    rdata := 0.U
  })
    .setAddr(CSRs.mireg4)

  val mireg5 = Module(new CSRModule("Mireg5") {
    rdata := 0.U
  })
    .setAddr(CSRs.mireg5)

  val mireg6 = Module(new CSRModule("Mireg6") {
    rdata := 0.U
  })
    .setAddr(CSRs.mireg6)

  val sireg2 = Module(new CSRModule("Sireg2") {
    rdata := 0.U
  })
    .setAddr(CSRs.sireg2)

  val sireg3 = Module(new CSRModule("Sireg3") {
    rdata := 0.U
  })
    .setAddr(CSRs.sireg3)

  val sireg4 = Module(new CSRModule("Sireg4") {
    rdata := 0.U
  })
    .setAddr(CSRs.sireg4)

  val sireg5 = Module(new CSRModule("Sireg5") {
    rdata := 0.U
  })
    .setAddr(CSRs.sireg5)

  val sireg6 = Module(new CSRModule("Sireg6") {
    rdata := 0.U
  })
    .setAddr(CSRs.sireg6)

  val vsireg2 = Module(new CSRModule("VSireg2") {
    rdata := 0.U
  })
    .setAddr(CSRs.vsireg2)

  val vsireg3 = Module(new CSRModule("VSireg3") {
    rdata := 0.U
  })
    .setAddr(CSRs.vsireg3)

  val vsireg4 = Module(new CSRModule("VSireg4") {
    rdata := 0.U
  })
    .setAddr(CSRs.vsireg4)

  val vsireg5 = Module(new CSRModule("VSireg5") {
    rdata := 0.U
  })
    .setAddr(CSRs.vsireg5)

  val vsireg6 = Module(new CSRModule("VSireg6") {
    rdata := 0.U
  })
    .setAddr(CSRs.vsireg6)

  val indCSRMods = Seq(
    mireg2,
    mireg3,
    mireg4,
    mireg5,
    mireg6,
    sireg2,
    sireg3,
    sireg4,
    sireg5,
    sireg6,
    vsireg2,
    vsireg3,
    vsireg4,
    vsireg5,
    vsireg6,
  )

  val indCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_ <: CSRBundle], UInt)] = SeqMap.from(
    indCSRMods.map(csr => (csr.addr -> (csr.w -> csr.rdata))).iterator
  )

  val indCSROutMap: SeqMap[Int, UInt] = SeqMap.from(
    indCSRMods.map(csr => (csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt)).iterator
  )

}

object Iselect {
  def isInAIA(iselect: UInt): Bool = iselect >= 0x30.U && iselect <= 0x3f.U
  def isInImsic(iselect: UInt): Bool = iselect >= 0x70.U && iselect <= 0xff.U
  def isInOthers(iselect: UInt): Bool = !(isInAIA(iselect) || isInImsic(iselect))
  def isOdd(iselect: UInt): Bool = iselect(0) === 1.U
}

object Ireg {
  def isInMCsrInd(ireg: UInt): Bool = ireg >= CSRs.miselect.U && ireg <= CSRs.mireg6.U && ireg =/= CSRs.miph.U
  def isInSCsrInd(ireg: UInt): Bool = ireg >= CSRs.siselect.U && ireg <= CSRs.sireg6.U && ireg =/= CSRs.siph.U
  def isInVSCsrInd(ireg: UInt): Bool = ireg >= CSRs.vsiselect.U && ireg <= CSRs.vsireg6.U && ireg =/= CSRs.vsiph.U
  def isInMiregX(ireg: UInt): Bool = ireg >= CSRs.mireg.U && ireg <= CSRs.mireg6.U && ireg =/= CSRs.miph.U
  def isInSiregX(ireg: UInt): Bool = ireg >= CSRs.sireg.U && ireg <= CSRs.sireg6.U && ireg =/= CSRs.siph.U
  def isInVSiregX(ireg: UInt): Bool = ireg >= CSRs.vsireg.U && ireg <= CSRs.vsireg6.U && ireg =/= CSRs.vsiph.U
  def isInMireg2_6(ireg: UInt): Bool = ireg >= CSRs.mireg2.U && ireg <= CSRs.mireg6.U && ireg =/= CSRs.miph.U
  def isInSireg2_6(ireg: UInt): Bool = ireg >= CSRs.sireg2.U && ireg <= CSRs.sireg6.U && ireg =/= CSRs.siph.U
  def isInVSireg2_6(ireg: UInt): Bool = ireg >= CSRs.vsireg2.U && ireg <= CSRs.vsireg6.U && ireg =/= CSRs.vsiph.U
}
