package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.Mux1H
import xiangshan.backend.fu.NewCSR.CSRDefines._

abstract class CSRModule[T <: CSRBundle](
  val modName: String,
  val bundle: T
) extends Module {

  override def desiredName: String = modName + "Module"

  val commonIn = IO(Input(new CSRCommonIn))
  val w = IO(Input(new CSRAddrWriteBundle(bundle)))
  val rdata = IO(Output(UInt()))

  val reg = Reg(bundle)

  protected val wen = w.wen
  protected val wdata = w.wdataFields

  protected val status = commonIn.status
  protected val v = commonIn.v
  protected val prvm = commonIn.prvm

  reg.elements.foreach { case (str, field: CSREnumType) =>
    if (!field.isRefField)
      when(wen)(field := wdata.elements(str))
  }

  rdata := reg.asUInt

  def wfnField(field: CSREnumType, str: String)(wAliasSeq: Seq[CSRAddrWriteBundle[_]]) = {

    when(wen | wAliasSeq.map(_.wen).reduce(_ | _)) {
      field := Mux1H(
        wAliasSeq.map(wAlias => wAlias.wen -> wAlias.wdataFields.asInstanceOf[CSRBundle].elements(str)) :+
        wen -> wdata.elements(str)
      )
    }.otherwise(field := field)
  }

  def wfn(reg: T)(wAliasSeq: Seq[CSRAddrWriteBundle[_]]) = {
    reg.elements.foreach { case (str, field: CSREnumType) =>
      if (!field.isRefField) {
        val fieldWAliasSeq = wAliasSeq.filter(_.wdataFields.asInstanceOf[CSRBundle].elements.contains(str))
        if (fieldWAliasSeq.nonEmpty) {
          wfnField(field, str)(fieldWAliasSeq)
        } else {
          when(wen)(field := wdata.elements(str))
        }
      }
    }
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
}

class MstatusBundle extends CSRBundle {

  val SIE = CSRFieldWARLBits(1, wNoFilter)
  val MIE = CSRFieldWARLBits(3, wNoFilter)
  val SPIE = CSRFieldWARLBits(5, wNoFilter)
  val UBE = CSRFieldROBits(6, rFixValue(0.U))
  val MPIE = CSRFieldWARLBits(7, wNoFilter)
  val SPP = CSRFieldWARLBits(8, wNoFilter)
  val VS = ContextStatus(10, 9, wNoFilter)
  val MPP = CSRFieldWARLBits(12, 11, wNoFilter)
  val FS = ContextStatus(14, 13, wNoFilter)
  val XS = ContextStatus(16, 15, rFixValue(0.U))
  val MPRV = CSRFieldWARLBits(17, wNoFilter)
  val SUM = CSRFieldWARLBits(18, wNoFilter)
  val MXR = CSRFieldWARLBits(19, wNoFilter)
  val TVM = CSRFieldWARLBits(20, wNoFilter)
  val TW = CSRFieldWARLBits(21, wNoFilter)
  val TSR = CSRFieldWARLBits(22, wNoFilter)
  val UXL = UXLField(33, 32, rFixValue("b10".U))
  val SXL = SXLField(35, 34, rFixValue("b10".U))
  val SBE = CSRFieldROBits(36, rFixValue(0.U))
  val MBE = CSRFieldROBits(37, rFixValue(0.U))
  val GVA = CSRFieldWARLBits(38, wNoFilter)
  val MPV = CSRFieldWARLBits(39, wNoFilter)
  val SD = CSRFieldROBits(
    63,
    (_, _) =>
      FS === FS.factory.asInstanceOf[ContextStatusDef].Dirty ||
        VS === VS.factory.asInstanceOf[ContextStatusDef].Dirty
  )
}

class MstatusModule extends CSRModule("MStatus", new MstatusBundle) {
  val mstatus = IO(Output(bundle.asInstanceOf[MstatusBundle]))
  val sstatus = IO(Output(new SstatusBundle))

  val wAliasSstatus = IO(Input(new CSRAddrWriteBundle(new SstatusBundle)))

  // write connection
  this.wfn(reg)(Seq(wAliasSstatus))

  // read connection
  mstatus :|= reg
  sstatus := mstatus
  rdata := mstatus.asUInt

  class SstatusBundle extends CSRBundle {
    val SIE  = CSRFieldWARLBits(1, wNoFilter)
    val SPIE = CSRFieldWARLBits(5, wNoFilter)
    val SPP  = CSRFieldWARLBits(8, wNoFilter)
    val VS   = ContextStatus(10,  9, wNoFilter)
    val FS   = ContextStatus(14, 13, wNoFilter)
    val SUM  = CSRFieldWARLBits(18, wNoFilter)
    val MXR  = CSRFieldWARLBits(19, wNoFilter)
    val SD   = CSRFieldROBits(63, rNoFilter)
  }
}