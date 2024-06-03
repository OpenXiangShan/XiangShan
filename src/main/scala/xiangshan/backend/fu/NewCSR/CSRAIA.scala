package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState
import xiangshan.backend.fu.NewCSR.CSRConfig._
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW, _}

import scala.collection.immutable.SeqMap

trait CSRAIA { self: NewCSR with HypervisorLevel =>
  val miselect = Module(new CSRModule("Miselevt", new MISelectBundle))
    .setAddr(0x350)

  val mireg = Module(new CSRModule("Mireg") with HasIregSink {
    rdata := iregRead.mireg
  })
    .setAddr(0x351)

  val mtopei = Module(new CSRModule("Mtopei", new CSRBundle {
    val id   = RW(26, 16)
    val prio = RW(10,  0)
  }))
    .setAddr(0x35C)

  val mtopi = Module(new CSRModule("Mtopi", new TopIBundle) with HasInterruptFilterSink {
    regOut.IID   := topIR.mtopi.IID
    regOut.IPRIO := topIR.mtopi.IPRIO
  })
    .setAddr(0xFB0)

  val siselect = Module(new CSRModule("Siselect", new SISelectBundle))
    .setAddr(0x150)

  val sireg = Module(new CSRModule("Sireg") with HasIregSink {
    rdata := iregRead.sireg
  })
    .setAddr(0x151)

  val stopei = Module(new CSRModule("Stopei", new CSRBundle {
    val id   = RW(26, 16)
    val prio = RW(10,  0)
  }))
    .setAddr(0x15C)

  val stopi = Module(new CSRModule("Stopi", new TopIBundle) with HasInterruptFilterSink {
    regOut.IID   := topIR.stopi.IID
    regOut.IPRIO := topIR.stopi.IPRIO
  })
    .setAddr(0xDB0)

  val vsiselect = Module(new CSRModule("VSiselect", new VSISelectBundle))
    .setAddr(0x250)

  val vsireg    = Module(new CSRModule("VSireg") with HasIregSink {
    rdata := iregRead.sireg
  })
    .setAddr(0x251)

  val vstopei   = Module(new CSRModule("VStopei", new CSRBundle {
    val id   = RW(26, 16)
    val prio = RW(10,  0)
  }))
    .setAddr(0x25C)

  val vstopi = Module(new CSRModule("VStopi", new TopIBundle) with HasInterruptFilterSink {
    regOut.IID   := topIR.vstopi.IID
    regOut.IPRIO := topIR.vstopi.IPRIO
  })
    .setAddr(0xEB0)

  val miregiprios: Seq[CSRModule[_]] = Range(0, 0xF, 2).map(num =>
    Module(new CSRModule(s"Iprio$num"))
      .setAddr(0x30 + num)
  )

  val siregiprios: Seq[CSRModule[_]] = Range(0, 0xF, 2).map(num =>
    Module(new CSRModule(s"Iprio$num"))
      .setAddr(0x30 + num)
  )

  val aiaCSRMods = Seq(
    miselect,
    mireg,
    mtopei,
    mtopi,
    siselect,
    sireg,
    stopei,
    stopi,
    vsiselect,
    vsireg,
    vstopi,
    vstopei,
  )

  val aiaCSRMap = SeqMap.from(
    aiaCSRMods.map(csr => (csr.addr -> (csr.w -> csr.rdata))).iterator
  )

  val aiaCSROutMap: SeqMap[Int, UInt] = SeqMap.from(
    aiaCSRMods.map(csr => (csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt)).iterator
  )

  private val miregRData: UInt = Mux1H(
    miregiprios.map(prio => (miselect.rdata.asUInt === prio.addr.U) -> prio.rdata)
  )

  private val siregRData: UInt = Mux1H(
    siregiprios.map(prio => (siselect.rdata.asUInt === prio.addr.U) -> prio.rdata)
  )

  aiaCSRMods.foreach { mod =>
    mod match {
      case m: HasIregSink =>
        m.iregRead.mireg := miregRData
        m.iregRead.sireg := siregRData
        m.iregRead.vsireg := 0.U // Todo: IMSIC
      case _ =>
    }
  }
}

class ISelectField(final val maxValue: Int, reserved: Seq[Range]) extends CSREnum with WARLApply {
  override def isLegal(enum: CSREnumType): Bool = {
    !reserved.map(range => enum.asUInt >= range.start.U && enum.asUInt <= range.end.U).reduce(_ || _)
  }
}

object VSISelectField extends ISelectField(
  0x1FF,
  reserved = Seq(
    Range.inclusive(0x000, 0x02F),
    Range.inclusive(0x040, 0x06F),
    Range.inclusive(0x100, 0x1FF),
  ),
)

object MISelectField extends ISelectField(
  maxValue = 0xFF,
  reserved = Seq(
    Range.inclusive(0x00, 0x2F),
    Range.inclusive(0x40, 0x6F),
  ),
)

object SISelectField extends ISelectField(
  maxValue = 0xFF,
  reserved = Seq(
    Range.inclusive(0x00, 0x2F),
    Range.inclusive(0x40, 0x6F),
  ),
)

class VSISelectBundle extends CSRBundle {
  val ALL = VSISelectField(log2Up(0x1FF), 0, null)
}

class MISelectBundle extends CSRBundle {
  val ALL = MISelectField(log2Up(0xFF), 0, null)
}

class SISelectBundle extends CSRBundle {
  val ALL = SISelectField(log2Up(0xFF), 0, null)
}

class TopIBundle extends CSRBundle {
  val IID   = RO(27, 16)
  val IPRIO = RO(7, 0)
}

class TopEIBundle extends CSRBundle {
  val IID   = RW(26, 16)
  val IPRIO = RW(10, 0)
}

class IprioBundle extends CSRBundle {
  val ALL = RW(63, 0).withReset(0.U)
}

class CSRToAIABundle extends Bundle {
  private final val AddrWidth = 12

  val addr = ValidIO(new Bundle {
    val addr = UInt(AddrWidth.W)
    val v = VirtMode()
    val prvm = PrivMode()
  })

  val vgein = UInt(VGEINWidth.W)

  val wdata = ValidIO(new Bundle {
    val data = UInt(XLEN.W)
  })

  val mClaim = Bool()
  val sClaim = Bool()
  val vsClaim = Bool()
}

class AIAToCSRBundle extends Bundle {
  val rdata = ValidIO(new Bundle {
    val data = UInt(XLEN.W)
    val illegal = Bool()
  })
  val mtopei = ValidIO(new TopEIBundle)
  val stopei = ValidIO(new TopEIBundle)
  val vstopei = ValidIO(new TopEIBundle)
}

trait HasAIABundle { self: CSRModule[_] =>
  val aiaToCSR = IO(Input(new AIAToCSRBundle))
}

trait HasInterruptFilterSink { self: CSRModule[_] =>
  val topIR = IO(new Bundle {
    val mtopi  = Input(new TopIBundle)
    val stopi  = Input(new TopIBundle)
    val vstopi = Input(new TopIBundle)
  })
}

trait HasISelectBundle { self: CSRModule[_] =>
  val privState = IO(Input(new PrivState))
  val miselect = IO(Input(new MISelectBundle))
  val siselect = IO(Input(new SISelectBundle))
  val mireg = IO(Input(UInt(XLEN.W)))
  val sireg = IO(Input(UInt(XLEN.W)))
}

trait HasIregSink { self: CSRModule[_] =>
  val iregRead = IO(Input(new Bundle {
    val mireg = UInt(XLEN.W) // Todo: check if use ireg bundle, and shrink the width
    val sireg = UInt(XLEN.W)
    val vsireg = UInt(XLEN.W)
  }))
}