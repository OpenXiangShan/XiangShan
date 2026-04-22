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
  val miselect = Module(new CSRModule("Miselect", new MISelectBundle) with HasISelectBundle {
    private val value = reg.ALL.asUInt
    inIMSICRange := Iselect.isInImsic(value)
  })
    .setAddr(CSRs.miselect)

  val mireg = Module(new CSRModule("Mireg", new ZeroFieldBundle("Machine indirect data register selected by miselect.")) with HasIregSink {
    regOut := iregRead.mireg
  })
    .setAddr(CSRs.mireg)

  val mireg2 = Module(new CSRModule("Mireg2", new ZeroFieldBundle("Reserved machine indirect data register; reads as zero.")) {
    regOut := 0.U
  })
    .setAddr(CSRs.mireg2)

  val mireg3 = Module(new CSRModule("Mireg3", new ZeroFieldBundle("Reserved machine indirect data register; reads as zero.")) {
    regOut := 0.U
  })
    .setAddr(CSRs.mireg3)

  val mireg4 = Module(new CSRModule("Mireg4", new ZeroFieldBundle("Reserved machine indirect data register; reads as zero.")) {
    regOut := 0.U
  })
    .setAddr(CSRs.mireg4)

  val mireg5 = Module(new CSRModule("Mireg5", new ZeroFieldBundle("Reserved machine indirect data register; reads as zero.")) {
    regOut := 0.U
  })
    .setAddr(CSRs.mireg5)

  val mireg6 = Module(new CSRModule("Mireg6", new ZeroFieldBundle("Reserved machine indirect data register; reads as zero.")) {
    regOut := 0.U
  })
    .setAddr(CSRs.mireg6)

  val siselect = Module(new CSRModule("Siselect", new SISelectBundle) with HasISelectBundle {
    private val value = reg.ALL.asUInt
    inIMSICRange := Iselect.isInImsic(value)
  })
    .setAddr(CSRs.siselect)

  val sireg = Module(new CSRModule("Sireg", new ZeroFieldBundle("Supervisor indirect data register selected by siselect.")) with HasIregSink {
    val smcdelegSelect = IO(Input(UInt(csrindSelectWidth.W)))
    val fromMcounter = IO(Input(Vec(SmcdelegIreg.selects.length, UInt(XLEN.W))))
    val toMcounter   = IO(Vec(SmcdelegIreg.selects.length, ValidIO(UInt(XLEN.W))))

    private val smcdelegRData = Mux1H(SmcdelegIreg.selects.zipWithIndex.map { case (select, i) =>
      (smcdelegSelect === select.U) -> fromMcounter(i)
    })

    private val readData = Mux(Iselect.isInSmcdeleg(smcdelegSelect), smcdelegRData, iregRead.sireg)
    regOut := readData

    SmcdelegIreg.selects.zipWithIndex.foreach { case (select, i) =>
      toMcounter(i).valid := wen && smcdelegSelect === select.U
      toMcounter(i).bits := wdata.asUInt
    }
  })
    .setAddr(CSRs.sireg)

  val sireg2 = Module(new CSRModule("Sireg2", new ZeroFieldBundle("Supervisor indirect data register2 selected by siselect.")) {
    val smcdelegSelect = IO(Input(UInt(csrindSelectWidth.W)))
    val fromMcfg = IO(Input(Vec(SmcdelegIreg.selects.length, UInt(XLEN.W))))
    val toMcfg   = IO(Vec(SmcdelegIreg.selects.length, ValidIO(UInt(XLEN.W))))

    private val smcdelegRData = Mux1H(SmcdelegIreg.selects.zipWithIndex.map { case (select, i) =>
      (smcdelegSelect === select.U) -> (fromMcfg(i) & SmcdelegIreg.cfgMask)
    })

    regOut := Mux(Iselect.isInSmcdeleg(smcdelegSelect), smcdelegRData, 0.U)

    SmcdelegIreg.selects.zipWithIndex.foreach { case (select, i) =>
      toMcfg(i).valid := wen && smcdelegSelect === select.U
      toMcfg(i).bits := (wdata.asUInt & SmcdelegIreg.cfgMask) | (fromMcfg(i) & ~SmcdelegIreg.cfgMask)
    }
  })
    .setAddr(CSRs.sireg2)

  val sireg3 = Module(new CSRModule("Sireg3", new ZeroFieldBundle("Reserved supervisor indirect data register; reads as zero.")) {
    regOut := 0.U
  })
    .setAddr(CSRs.sireg3)

  val sireg4 = Module(new CSRModule("Sireg4", new ZeroFieldBundle("Reserved supervisor indirect data register; reads as zero.")) {
    regOut := 0.U
  })
    .setAddr(CSRs.sireg4)

  val sireg5 = Module(new CSRModule("Sireg5", new ZeroFieldBundle("Reserved supervisor indirect data register; reads as zero.")) {
    regOut := 0.U
  })
    .setAddr(CSRs.sireg5)

  val sireg6 = Module(new CSRModule("Sireg6", new ZeroFieldBundle("Reserved supervisor indirect data register; reads as zero.")) {
    regOut := 0.U
  })
    .setAddr(CSRs.sireg6)

  val vsiselect = Module(new CSRModule("VSiselect", new VSISelectBundle) with HasISelectBundle {
    private val value = reg.ALL.asUInt
    inIMSICRange := Iselect.isInImsic(value)
  })
    .setAddr(CSRs.vsiselect)

  val vsireg = Module(new CSRModule("VSireg", new ZeroFieldBundle("Virtual supervisor indirect data register selected by vsiselect.")) with HasIregSink {
    regOut := iregRead.sireg
  })
    .setAddr(CSRs.vsireg)

  val vsireg2 = Module(new CSRModule("VSireg2", new ZeroFieldBundle("Reserved virtual supervisor indirect data register; reads as zero.")) {
    regOut := 0.U
  })
    .setAddr(CSRs.vsireg2)

  val vsireg3 = Module(new CSRModule("VSireg3", new ZeroFieldBundle("Reserved virtual supervisor indirect data register; reads as zero.")) {
    regOut := 0.U
  })
    .setAddr(CSRs.vsireg3)

  val vsireg4 = Module(new CSRModule("VSireg4", new ZeroFieldBundle("Reserved virtual supervisor indirect data register; reads as zero.")) {
    regOut := 0.U
  })
    .setAddr(CSRs.vsireg4)

  val vsireg5 = Module(new CSRModule("VSireg5", new ZeroFieldBundle("Reserved virtual supervisor indirect data register; reads as zero.")) {
    regOut := 0.U
  })
    .setAddr(CSRs.vsireg5)

  val vsireg6 = Module(new CSRModule("VSireg6", new ZeroFieldBundle("Reserved virtual supervisor indirect data register; reads as zero.")) {
    regOut := 0.U
  })
    .setAddr(CSRs.vsireg6)

  val indCSRMods = Seq(
    miselect,
    mireg,
    mireg2,
    mireg3,
    mireg4,
    mireg5,
    mireg6,
    siselect,
    sireg,
    sireg2,
    sireg3,
    sireg4,
    sireg5,
    sireg6,
    vsiselect,
    vsireg,
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

  private val miregRData: UInt = Mux1H(
    miregiprios.map(prio => (miselect.rdata.asUInt === prio.addr.U) -> prio.rdata)
  )

  private val siregRData: UInt = Mux1H(
    siregiprios.map(prio => (siselect.rdata.asUInt === prio.addr.U) -> prio.rdata)
  )

  indCSRMods.foreach { mod =>
    mod match {
      case m: HasIregSink =>
        m.iregRead.mireg := miregRData
        m.iregRead.sireg := siregRData
        m.iregRead.vsireg := 0.U // Todo: IMSIC
      case _ =>
    }
  }
}

object Iselect {
  def isInAIA(iselect: UInt): Bool = iselect >= 0x30.U && iselect <= 0x3f.U
  def isInSmcdeleg(iselect: UInt): Bool = iselect >= 0x40.U && iselect <= 0x5f.U
  def isSmcdelegTime(iselect: UInt): Bool = iselect === 0x41.U
  def isInImsic(iselect: UInt): Bool = iselect >= 0x70.U && iselect <= 0xff.U
  def isInOthers(iselect: UInt): Bool = !(isInAIA(iselect) || isInImsic(iselect) || isInSmcdeleg(iselect))
  def isOdd(iselect: UInt): Bool = iselect(0) === 1.U
}

object SmcdelegIreg {
  val selects: Seq[Int] = Seq(0x40, 0x42) ++ (0x43 to 0x5f)
  val mInhIdx = 62
  def cfgMask = ~(BigInt(1) << mInhIdx).U(XLEN.W)
}

object Ireg {
  def isInMCsrInd(ireg: UInt): Bool = ireg >= CSRs.miselect.U && ireg <= CSRs.mireg6.U && ireg =/= CSRs.miph.U
  def isInSCsrInd(ireg: UInt): Bool = ireg >= CSRs.siselect.U && ireg <= CSRs.sireg6.U && ireg =/= CSRs.siph.U
  def isInVSCsrInd(ireg: UInt): Bool = ireg >= CSRs.vsiselect.U && ireg <= CSRs.vsireg6.U && ireg =/= CSRs.vsiph.U

  def isInMireg(ireg: UInt): Bool = ireg === CSRs.mireg.U
  def isInSireg(ireg: UInt): Bool = ireg === CSRs.sireg.U
  def isInVSireg(ireg: UInt): Bool = ireg === CSRs.vsireg.U

  def isInMireg2(ireg: UInt): Bool = ireg === CSRs.mireg2.U
  def isInSireg2(ireg: UInt): Bool = ireg === CSRs.sireg2.U
  def isInVSireg2(ireg: UInt): Bool = ireg === CSRs.vsireg2.U

  def isInMiregX(ireg: UInt): Bool = ireg >= CSRs.mireg.U && ireg <= CSRs.mireg6.U && ireg =/= CSRs.miph.U
  def isInSiregX(ireg: UInt): Bool = ireg >= CSRs.sireg.U && ireg <= CSRs.sireg6.U && ireg =/= CSRs.siph.U
  def isInVSiregX(ireg: UInt): Bool = ireg >= CSRs.vsireg.U && ireg <= CSRs.vsireg6.U && ireg =/= CSRs.vsiph.U

  def isInMireg2_6(ireg: UInt): Bool = ireg >= CSRs.mireg2.U && ireg <= CSRs.mireg6.U && ireg =/= CSRs.miph.U
  def isInSireg2_6(ireg: UInt): Bool = ireg >= CSRs.sireg2.U && ireg <= CSRs.sireg6.U && ireg =/= CSRs.siph.U
  def isInVSireg2_6(ireg: UInt): Bool = ireg >= CSRs.vsireg2.U && ireg <= CSRs.vsireg6.U && ireg =/= CSRs.vsiph.U

  def isInMireg3_6(ireg: UInt): Bool = ireg >= CSRs.mireg3.U && ireg <= CSRs.mireg6.U && ireg =/= CSRs.miph.U
  def isInSireg3_6(ireg: UInt): Bool = ireg >= CSRs.sireg3.U && ireg <= CSRs.sireg6.U && ireg =/= CSRs.siph.U
  def isInVSireg3_6(ireg: UInt): Bool = ireg >= CSRs.vsireg3.U && ireg <= CSRs.vsireg6.U && ireg =/= CSRs.vsiph.U
}
