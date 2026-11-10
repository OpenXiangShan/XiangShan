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

trait CSRAIA extends HasSoCParameter { self: NewCSR with HypervisorLevel =>
  private def fieldWritableMap(fields: Seq[CSREnumType]): Map[Int, Boolean] =
    fields.map(f => f.lsb -> !f.isRO).toMap

  private def validFieldWritableMap(fields: Seq[Valid[CSREnumType]]): Map[Int, Boolean] =
    fields.map(f => f.bits.lsb -> !f.bits.isRO).toMap

  private def iprioMask(base: Int, writableMaps: Seq[Map[Int, Boolean]]): UInt = {
    val mask = (0 until 8).foldLeft(BigInt(0)) { case (m, i) =>
      val intNo = base + i
      val writable = writableMaps.exists(_.getOrElse(intNo, false))
      if (writable) m | (BigInt(0xff) << (8 * i)) else m
    }
    mask.U(64.W)
  }

  private lazy val mieFieldWritable: Map[Int, Boolean] = {
    val m = fieldWritableMap(new MieBundle().getFields)
    if (soc.IMSICParams.HasTEEIMSIC) m else m.updated(InterruptNO.ASNI, false)
  }

  /*
    For a given interrupt number, if the corresponding bit in mie is read-only zero,
    then the interrupt’s priority number in the iprio array must be read-only zero as well.
    static mask
  */
  private def miprioMask(base: Int): UInt =
    iprioMask(base, Seq(mieFieldWritable))

  private def gatedWritable(writableMap: Map[Int, Boolean], intNo: Int, gate: Bool): Bool =
    if (writableMap.getOrElse(intNo, false)) gate else false.B

  /*
    For a given interrupt number, if the corresponding bit is not writable either in sie or,
    if the H extension is implemented, in hie,
    then the interrupt’s priority number in the supervisor-level iprio array must be read-only zero as well.
    Dynamic mask
  */
  private def siprioMask(base: Int, midelegBits: UInt, mvienBits: UInt): UInt = {
    val sieRegWritable = fieldWritableMap(new SieBundle().getFields)
    val sieToMieWritable = validFieldWritableMap(new SieToMie().getAll)
    val hieToMieWritable = validFieldWritableMap(new HieToMie().getAll)

    Cat((0 until 8).reverse.map { i =>
      val intNo = base + i
      val delegated = midelegBits(intNo)
      val virtualized = mvienBits(intNo)
      val writableInSie =
        gatedWritable(sieToMieWritable, intNo, delegated) ||
        gatedWritable(sieRegWritable, intNo, !delegated && virtualized)
      val writableInHie =
        gatedWritable(hieToMieWritable, intNo, delegated)

      Fill(8, writableInSie || writableInHie)
    })
  }

  val mtopei = Module(new CSRModule("Mtopei", new TopEIBundle) with HasAIABundle {
    regOut := aiaToCSR.mtopei
  })
    .setAddr(CSRs.mtopei)

  val mtopi = Module(new CSRModule("Mtopi", new TopIBundle) with HasInterruptFilterSink {
    regOut.IID   := topIR.mtopi.IID
    regOut.IPRIO := topIR.mtopi.IPRIO
  })
    .setAddr(CSRs.mtopi)

  val stopei = Module(new CSRModule("Stopei", new TopEIBundle) with HasAIABundle {
    regOut := aiaToCSR.stopei
  })
    .setAddr(CSRs.stopei)

  val stopi = Module(new CSRModule("Stopi", new TopIBundle) with HasInterruptFilterSink {
    regOut.IID   := topIR.stopi.IID
    regOut.IPRIO := topIR.stopi.IPRIO
  })
    .setAddr(CSRs.stopi)

  val vstopei   = Module(new CSRModule("VStopei", new TopEIBundle) with HasAIABundle {
    regOut := aiaToCSR.vstopei
  })
    .setAddr(CSRs.vstopei)

  val vstopi = Module(new CSRModule("VStopi", new TopIBundle) with HasInterruptFilterSink {
    regOut.IID   := topIR.vstopi.IID
    regOut.IPRIO := topIR.vstopi.IPRIO
  })
    .setAddr(CSRs.vstopi)

  val miprio0 = Module(new CSRModule(s"Iprio0", new Iprio0Bundle) {
    regOut := reg & miprioMask(0)
  })
    .setAddr(0x30)

  val miprio2 = Module(new CSRModule(s"Iprio2", new MIprio2Bundle) {
    regOut := reg & miprioMask(8)
  })
    .setAddr(0x32)

  val miprios: Seq[CSRModule[_]] = (4 to (0xF, 2)).map(num =>
    Module(new CSRModule(s"Iprio$num", new IprioBundle) {
      regOut := reg & miprioMask(num * 4)
    })
      .setAddr(0x30 + num)
  )

  val siprio0 = Module(new CSRModule(s"Iprio0", new Iprio0Bundle) with HasSiprios {
    regOut := reg & siprioMask(0, mideleg.asUInt, mvien.asUInt)
  })
    .setAddr(0x30)

  val siprio2 = Module(new CSRModule(s"Iprio2", new SIprio2Bundle) with HasSiprios {
    regOut := reg & siprioMask(8, mideleg.asUInt, mvien.asUInt)
  })
    .setAddr(0x32)

  val siprios: Seq[CSRModule[_]] = (4 to (0xF, 2)).map(num =>
    Module(new CSRModule(s"Iprio$num", new IprioBundle) with HasSiprios {
      regOut := reg & siprioMask(num * 4, mideleg.asUInt, mvien.asUInt)
    })
    .setAddr(0x30 + num)
  )

  val miregiprios: Seq[CSRModule[_]] = Seq(miprio0, miprio2) ++: miprios

  val siregiprios: Seq[CSRModule[_]] = Seq(siprio0, siprio2) ++: siprios

  val aiaCSRMods = Seq(
    mtopei,
    mtopi,
    stopei,
    stopi,
    vstopi,
    vstopei,
  )

  val aiaCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], UInt)] = SeqMap.from(
    aiaCSRMods.map(csr => (csr.addr -> (csr.w -> csr.rdata))).iterator
  )

  val aiaCSROutMap: SeqMap[Int, UInt] = SeqMap.from(
    aiaCSRMods.map(csr => (csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt)).iterator
  )
}

class ISelectField(final val maxValue: Int, reserved: Seq[Range]) extends CSREnum with WARLApply {
  override protected def legalRange: Option[(BigInt, BigInt)] = Some(0, maxValue)
  override protected def legalBoundString(value: BigInt): String = f"0x$value%x"
  override def warlConstraintDescription(enumeration: CSREnumType): Option[String] = {
    val reservedText = reserved match {
      case Nil => ""
      case rs =>
        val text = rs.map(range => s"${legalBoundString(range.start)} to ${legalBoundString(range.last)}").mkString(", ")
        s" Reserved subranges: $text."
    }
    Some(s"Legal values are in the range ${legalBoundString(0)} to ${legalBoundString(maxValue)}.$reservedText")
  }
}

object VSISelectField extends ISelectField(
  0xFFF,
  reserved = Seq(
    Range.inclusive(0x000, 0x02F),
    Range.inclusive(0x040, 0x06F),
    Range.inclusive(0x100, 0xFFF),
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
  maxValue = 0xFFF,
  reserved = Seq(
    Range.inclusive(0x000, 0x02F),
    Range.inclusive(0x040, 0x06F),
    Range.inclusive(0x100, 0xFFF),
  ),
)

class VSISelectBundle extends CSRBundle {
  val ALL = VSISelectField(log2Up(0xFFF), 0, null).withReset(0.U)
    .withDescription("Virtual supervisor interrupt selector for indirect AIA CSR accesses.")
}

class MISelectBundle extends CSRBundle {
  val ALL = MISelectField(log2Up(0xFF), 0, null).withReset(0.U)
    .withDescription("Machine interrupt selector for indirect AIA CSR accesses.")
}

class SISelectBundle extends CSRBundle {
  val ALL = SISelectField(log2Up(0xFFF), 0, null).withReset(0.U)
    .withDescription("Supervisor interrupt selector for indirect AIA CSR accesses.")
}

class TopIBundle extends CSRBundle {
  val IID   = RO(27, 16).withDescription("Identity of the highest-priority pending interrupt.")
  val IPRIO = RO(7, 0).withDescription("Priority of the highest-priority pending interrupt.")
}

class TopEIBundle extends CSRBundle {
  val IID   = RW(26, 16).withDescription("Interrupt identity returned by a top-of-interrupt claim.")
  val IPRIO = RW(10, 0).withDescription("Priority returned by a top-of-interrupt claim.")
}

class IprioBundle extends FieldInitBundle(Some("Interrupt-priority register contents."))

class Iprio0Bundle extends CSRBundle {
  val PrioSSI  = RW(15,  8).withReset(0.U).withDescription("Priority value for supervisor software interrupt.")
  val PrioVSSI = RW(23, 16).withReset(0.U).withDescription("Priority value for virtual supervisor software interrupt.")
  val PrioMSI  = RW(31, 24).withReset(0.U).withDescription("Priority value for machine software interrupt.")
  val PrioSTI  = RW(47, 40).withReset(0.U).withDescription("Priority value for supervisor timer interrupt.")
  val PrioVSTI = RW(55, 48).withReset(0.U).withDescription("Priority value for virtual supervisor timer interrupt.")
  val PrioMTI  = RW(63, 56).withReset(0.U).withDescription("Priority value for machine timer interrupt.")
}

class MIprio2Bundle extends CSRBundle {
  val PrioSEI   = RW(15,  8).withReset(0.U).withDescription("Priority value for supervisor external interrupt.")
  val PrioVSEI  = RW(23, 16).withReset(0.U).withDescription("Priority value for virtual supervisor external interrupt.")
  val PrioMEI   = RO(31, 24).withReset(0.U).withDescription("Priority value for machine external interrupt.")
  val PrioSGEI  = RW(39, 32).withReset(0.U).withDescription("Priority value for supervisor guest external interrupt.")
  val PrioLCOFI = RW(47, 40).withReset(0.U).withDescription("Priority value for local counter-overflow interrupt.")
  val Prio14    = RW(55, 48).withReset(0.U).withDescription("Priority value for local interrupt 14.")
  val Prio15    = RW(63, 56).withReset(0.U).withDescription("Priority value for local interrupt 15.")
}

class SIprio2Bundle extends CSRBundle {
  val PrioSEI   = RO(15,  8).withReset(0.U).withDescription("Priority value for supervisor external interrupt.")
  val PrioVSEI  = RW(23, 16).withReset(0.U).withDescription("Priority value for virtual supervisor external interrupt.")
  val PrioMEI   = RW(31, 24).withReset(0.U).withDescription("Priority value for machine external interrupt.")
  val PrioSGEI  = RW(39, 32).withReset(0.U).withDescription("Priority value for supervisor guest external interrupt.")
  val PrioLCOFI = RW(47, 40).withReset(0.U).withDescription("Priority value for local counter-overflow interrupt.")
  val Prio14    = RW(55, 48).withReset(0.U).withDescription("Priority value for local interrupt 14.")
  val Prio15    = RW(63, 56).withReset(0.U).withDescription("Priority value for local interrupt 15.")
}

class CSRToAIABundle(implicit p: Parameters) extends XSBundle with HasSoCParameter {
  val addr = ValidIO(new Bundle {
    val addr = UInt(soc.IMSICParams.iselectWidth.W)
    val v = VirtMode()
    val prvm = PrivMode()
  })

  val vgein = UInt(soc.IMSICParams.vgeinWidth.W)

  val wdata = ValidIO(new Bundle {
    val op = UInt(2.W)
    val data = UInt(XLEN.W)
  })

  val mClaim = Bool()
  val sClaim = Bool()
  val vsClaim = Bool()
}

class AIAToCSRBundle(implicit p: Parameters) extends XSBundle with HasSoCParameter {
  val rdata = ValidIO(new Bundle {
    val data = UInt(XLEN.W)
    val illegal = Bool()
  })
  val meip    = Bool()
  val seip    = Bool()
  val notice_pending = Bool()
  val vseip   = UInt(soc.IMSICParams.geilen.W)
  val mtopei  = new TopEIBundle
  val stopei  = new TopEIBundle
  val vstopei = new TopEIBundle
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
  val inIMSICRange = IO(Output(Bool()))
}

trait HasIregSink { self: CSRModule[_] =>
  val iregRead = IO(Input(new Bundle {
    val mireg = UInt(XLEN.W) // Todo: check if use ireg bundle, and shrink the width
    val sireg = UInt(XLEN.W)
    val vsireg = UInt(XLEN.W)
  }))
}

trait HasSiprios { self: CSRModule[_] =>
  val mideleg = IO(Input(new MidelegBundle))
  val mvien = IO(Input(new MvienBundle))
}
