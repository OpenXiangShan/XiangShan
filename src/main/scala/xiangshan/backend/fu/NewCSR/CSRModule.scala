package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.Mux1H
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRBundles._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import org.chipsalliance.cde.config.Parameters

class CSRModule[T <: CSRBundle](
  val modName: String,
  val bundle: T = new OneFieldBundle,
)(implicit val p: Parameters) extends Module {

  override def desiredName: String = modName + "Module"

  val w = IO(Input(new CSRAddrWriteBundle(bundle)))

  // read data with mask, the same as the value of CSRR
  val rdata = IO(Output(UInt(bundle.len.W)))
  // read data without mask
  val regOut = IO(Output(bundle))

  protected val reg = (if (bundle.needReset) RegInit(bundle, bundle.init) else Reg(bundle))

  protected val wen = w.wen
  protected val wdata = w.wdataFields

  bundle.elements.foreach { case (str, field: CSREnumType) =>
    val wfield = wdata.elements(str).asInstanceOf[CSREnumType]
    field.rwType match {
      case WARLType(wfn, _) =>
        field.addOtherUpdate(wen && wfield.isLegal, wdata.elements(str).asInstanceOf[CSREnumType])
      case WLRLType(wfn, _) =>
        field.addOtherUpdate(wen && wfield.isLegal, wdata.elements(str).asInstanceOf[CSREnumType])
      case RWType() =>
        field.addOtherUpdate(wen, wdata.elements(str).asInstanceOf[CSREnumType])
      case ROType(_) =>
      case _ =>
    }
  }

  reconnectReg()

  val rdataFields = IO(Output(bundle))
  rdataFields :|= regOut

  rdata := rdataFields.asUInt
  regOut := reg

  private def wfnField(field: CSREnumType, str: String): Unit = {
    val wfield: CSREnumType = wdata.elements(str).asInstanceOf[CSREnumType]

    when (wen && wfield.isLegal || field.otherUpdateSeq.map(_._1).fold(false.B)(_ || _)) {
      field := Mux1H(
        field.otherUpdateSeq.map { case (valid, data) =>
          valid -> data
        } :+ (wen -> wdata.elements(str)),
      )
    }.otherwise {
      field := field
    }
  }

  private def wfn(reg: CSRBundle): Unit = {
    reg.elements.foreach { case (str, field: CSREnumType) =>
      if (!field.isRef) {
        wfnField(field, str)
      }
    }
  }

  protected def reconnectReg(): Unit = {
    this.wfn(this.reg)
  }

  def dumpFields: String = {
    this.reg.getFields.map(_.dumpName).mkString("\n")
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

// Interrupt Controller
class CSRIRCBundle extends Bundle {
  val sip = Input(Bool())
  val tip = Input(Bool())
  val eip = Input(Bool())
}
