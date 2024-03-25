package xiangshan.backend.fu.NewCSR

import chisel3._
import xiangshan.backend.fu.NewCSR.CSRDefines.{
  CSRRWField => RW,
  CSRROField => RO,
  CSRWLRLField => WLRL,
  _
}
import xiangshan.backend.fu.NewCSR.CSRFunc._

import scala.collection.immutable.SeqMap

trait Hypervisor { self: NewCSR =>

  val hstatus = Module(new HstatusModule)

  val hedeleg = Module(new CSRModule("Hedeleg", new CSRBundle {
    val EX_IAM = RW(0)
    val EX_IAF = RW(1)
    val EX_II  = RW(2)
    val EX_BP  = RW(3)
    val EX_LAM = RW(4)
    val EX_LAF = RW(5)
    val EX_SAM = RW(6)
    val EX_SAF = RW(7)
    val EX_UCALL  = RW(8)
    val EX_HSCALL = RO(9)
    val EX_VSCALL = RO(10)
    val EX_MCALL  = RO(11)
    val EX_IPF    = RW(12)
    val EX_LPF    = RW(13)
    val EX_SPF    = RW(15)
    val EX_IGPF   = RO(20)
    val EX_LGPF   = RO(21)
    val EX_VI     = RO(22)
    val EX_SGPF   = RO(23)
  }) {})

  val hideleg = Module(new CSRModule("Hideleg", new CSRBundle {
    // Software Interrupt
    val IR_SSI    = RO(1)
    val IR_VSSI   = RW(2)
    val IR_MSI    = RO(3)
    // Time Interrupt
    val IR_STI    = RO(5)
    val IR_VSTI   = RW(6)
    val IR_MTI    = RO(7)
    // External Interrupt
    val IR_SEI    = RO(9)
    val IR_VSEI   = RW(10)
    val IR_MEI    = RO(11)
    val IR_SGEI   = RO(12)
    // SoC
    val IR_COI    = RW(13) // Counter overflow interrupt
    val IR_LPRASE = RW(35) // Low-priority RAS event interrupt
    val IR_HPRASE = RW(43) // High-priority RAS event interrupt
  }) {})

  val hie = Module(new CSRModule("Hie", new CSRBundle {
    val VSSIE = RW( 2)
    val VSTIE = RW( 6)
    val VSEIE = RW(10)
    val SGEIE = RW(12)
  }) {})

  // 0x605
  val htimedelta = Module(new CSRModule("Htimedelta", new CSRBundle {
    val VALUE = RW(63, 0)
  }) {})

  // 0x606
  val hcounteren = Module(new CSRModule("Hcounteren", new CSRBundle {
    val CY = RW(0)
    val TM = RW(1)
    val IR = RW(2)
    val HPM = RW(31, 3)
  }) {})

  // 0x
  val henvcfg = Module(new CSRModule("Henvcfg", new CSRBundle {
    val FIOM  = RW(0)     // Fence of I/O implies Memory
    val CBIE  = RW(5, 4)  //
    val CBCFE = RW(6)     // Zicbom Enable
    val CBZE  = RW(7)     // Zicboz Enable
    val PBMTE = RW(62)    // Svpbmt Enable
    val STCE  = RW(63)    // Sstc Enable
  }) {})

  // 0x644
  val hip = Module(new CSRModule("Hip", new CSRBundle {
    val VSSIP = RW( 2) // alias of hvip.VSSIP
    val VSTIP = RO( 6) // hvip.VSTIP |　PLIC.VSTIP
    val VSEIP = RO(10) // hvip.VSEIP | hgeip(hstatus.VGEIN) | PLIC.VSEIP
    val SGEIP = RO(12) // |(hgeip & hegie)
  }) with HypervisorBundle {
    rdata.VSSIP := hvip.VSSIP
    rdata.VSTIP := hvip.VSTIP.asUInt.asBool | vsi.tip
    rdata.VSEIP := hvip.VSEIP.asUInt.asBool | vsi.eip // | hgeip(hstatus.VGEIN)
    // rdata.SGEIP := (hgeip | hegie).orR
  })

  // 0x645
  val hvip = Module(new CSRModule("Hvip", new CSRBundle {
    val VSSIP = RW( 2)
    val VSTIP = RW( 6)
    val VSEIP = RW(10)
  }) {})

  hip.hvip := hvip.rdata

  val hypervisorCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], Data)] = SeqMap(
    0x600 -> (hstatus.w -> hstatus.rdata),
    0x602 -> (hedeleg.w -> hedeleg.rdata),
    0x603 -> (hideleg.w -> hideleg.rdata),
    0x604 -> (hie.w -> hie.rdata),
    0x605 -> (htimedelta.w, htimedelta.rdata),
    0x606 -> (hcounteren.w, hcounteren.rdata),
    0x60A -> (henvcfg.w -> henvcfg.rdata),
    0x644 -> (hip.w -> hip.rdata),
    0x645 -> (hvip.w -> hvip.rdata),
  )

  val hypervisorCSRMods: Seq[CSRModule[_]] = Seq(
    hstatus,
    hedeleg,
    hideleg,
    hie,
    htimedelta,
    hcounteren,
    henvcfg,
    hip,
    hvip,
  )
}

class HstatusBundle extends CSRBundle {

  val VSBE  = RO(5).withReset(0.U)
  val GVA   = RW(6)
  val SPV   = RW(7)
  val SPVP  = RW(8)
  val HU    = RW(9)
  val VGEIN = WLRL(17, 12, wNoFilter)
  val VTVM  = RW(20)
  val VTM   = RW(21)
  val VTSR  = RW(22)
  val VSXL  = XLENField(33, 32).withReset(XLENField.XLEN64)

}

object HstatusVgeinField extends CSREnum with CSRWLRLApply with CSRConfig {
  override def isLegal(enum: CSREnumType): Bool = enum.asUInt <= this.GEILEN.U
}

class HstatusModule extends CSRModule("Hstatus", new HstatusBundle)

class HvipBundle extends CSRBundle {
  val VSSIP = RW(2)
  val VSTIP = RW(6)
  val VSEIP = RW(10)
}

class Hvip extends CSRModule("Hvip", new HvipBundle)

trait HypervisorBundle { self: CSRModule[_] =>
  val hvip = IO(Input(new HvipBundle))
}