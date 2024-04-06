package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.Mux1H
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRBundles._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor

class CSRModule[T <: CSRBundle](
  val modName: String,
  val bundle: T = new OneFieldBundle,
) extends Module {

  override def desiredName: String = modName + "Module"

  val commonIn = IO(Input(new CSRCommonIn))
  val w = IO(Input(new CSRAddrWriteBundle(bundle)))
  val vsi = IO(Input(new CSRIRCBundle))

  // read data with mask
  val rdata = IO(Output(bundle))
  // read data without mask
  val regOut = IO(Output(bundle))

  val reg = (if (bundle.needReset) RegInit(bundle, bundle.init) else Reg(bundle))

  protected val wen = w.wen
  protected val wdata = w.wdataFields

  protected val status = commonIn.status
  protected val v = commonIn.v
  protected val prvm = commonIn.prvm

  reg.elements.foreach { case (str, field: CSREnumType) =>
    val wfield = wdata.elements(str).asInstanceOf[CSREnumType]
    field.rwType match {
      case WARLType(wfn, _) =>
        when(wen && wfield.isLegal)(field := wdata.elements(str))
      case WLRLType(wfn, _) =>
        when(wen && wfield.isLegal)(field := wdata.elements(str))
      case RWType() =>
        when(wen)(field := wdata.elements(str))
      case ROType(_) =>
      case RefROType(ref, rfn) =>
      case RefRWType(ref) =>
      case RefWARLType(ref, wfn, rfn) =>
      case RefWLRLType(ref, wfn, rfn) =>
      case _ =>
    }
  }

  rdata :|= reg
  regOut := reg

  def wfnField(field: CSREnumType, str: String)(wAliasSeq: Seq[CSRAddrWriteBundle[_]]) = {
    val wfield: CSREnumType = wdata.elements(str).asInstanceOf[CSREnumType]

    when(wen && wfield.isLegal | wAliasSeq.map(x => x.wen && x.wdataFields.asInstanceOf[CSRBundle].elements(str).asInstanceOf[CSREnumType].isLegal).fold(false.B)(_ | _)) {
      field := Mux1H(
        wAliasSeq.map(wAlias => wAlias.wen -> wAlias.wdataFields.asInstanceOf[CSRBundle].elements(str)) :+
        wen -> wdata.elements(str)
      )
    }
  }

  def wfn(reg: T)(wAliasSeq: Seq[CSRAddrWriteBundle[_]]) = {
    reg.elements.foreach { case (str, field: CSREnumType) =>
      if (!field.isRef) {
        val fieldWAliasSeq = wAliasSeq.filter(_.wdataFields.asInstanceOf[CSRBundle].elements.contains(str))
        wfnField(field, str)(fieldWAliasSeq)
      }
    }
  }

  def dumpFields = {
    this.bundle.getFields.mkString("\n")
  }

  var addr = 0

  def setAddr(addr_ : Int): this.type = {
    this.addr = addr_
    this
  }
}

class CSRAddrWriteBundle[T <: CSRBundle](bundle: T) extends Bundle {
  val wen = Bool()
  val wdata = UInt(64.W)

  def wdataFields: T = {
    val wdataField = (Wire(bundle))
    wdataField := wdata
    wdataField
  }
}

class CSRCommonIn extends Bundle {
  val status = new MstatusBundle
  val prvm = PrivMode()
  val v = VirtMode()
  val hstatus = new HstatusBundle
}

// Interrupt Controller
class CSRIRCBundle extends Bundle {
  val sip = Input(Bool())
  val tip = Input(Bool())
  val eip = Input(Bool())
}
