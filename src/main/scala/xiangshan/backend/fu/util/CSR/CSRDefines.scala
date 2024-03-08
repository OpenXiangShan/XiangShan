package xiangshan.backend.fu.util.CSR

import chisel3._
import xiangshan.backend.fu.util.CSR.CSRDefines.MtvecMode
import xiangshan.macros.CSRMacros.CSRFieldsImpl

import scala.language.experimental.macros

object CSRDefines extends CSRFuncTrait {
  object CSRWARLField1Bits extends CSRWARLField

  object CSRWARLField2Bits extends CSRWARLField

  object CSRWARLField3Bits extends CSRWARLField

  object CSRWARLField4Bits extends CSRWARLField

  object CSRWARLField5Bits extends CSRWARLField

  object CSRWARLField6Bits extends CSRWARLField

  object CSRWARLField7Bits extends CSRWARLField

  object CSRWARLField8Bits extends CSRWARLField

  object CSRWARLField9Bits extends CSRWARLField

  object CSRWARLField10Bits extends CSRWARLField

  object CSRWARLField11Bits extends CSRWARLField

  object CSRWARLField12Bits extends CSRWARLField

  object CSRWARLField13Bits extends CSRWARLField

  object CSRWARLField14Bits extends CSRWARLField

  object CSRWARLField15Bits extends CSRWARLField

  object CSRWARLField16Bits extends CSRWARLField

  object CSRWARLField17Bits extends CSRWARLField

  object CSRWARLField18Bits extends CSRWARLField

  object CSRWARLField19Bits extends CSRWARLField

  object CSRWARLField20Bits extends CSRWARLField

  object CSRWARLField21Bits extends CSRWARLField

  object CSRWARLField22Bits extends CSRWARLField

  object CSRWARLField23Bits extends CSRWARLField

  object CSRWARLField24Bits extends CSRWARLField

  object CSRWARLField25Bits extends CSRWARLField

  object CSRWARLField26Bits extends CSRWARLField

  object CSRWARLField27Bits extends CSRWARLField

  object CSRWARLField28Bits extends CSRWARLField

  object CSRWARLField29Bits extends CSRWARLField

  object CSRWARLField30Bits extends CSRWARLField

  object CSRWARLField31Bits extends CSRWARLField

  object CSRWARLField32Bits extends CSRWARLField

  object CSRWARLField33Bits extends CSRWARLField

  object CSRWARLField34Bits extends CSRWARLField

  object CSRWARLField35Bits extends CSRWARLField

  object CSRWARLField36Bits extends CSRWARLField

  object CSRWARLField37Bits extends CSRWARLField

  object CSRWARLField38Bits extends CSRWARLField

  object CSRWARLField39Bits extends CSRWARLField

  object CSRWARLField40Bits extends CSRWARLField

  object CSRWARLField41Bits extends CSRWARLField

  object CSRWARLField42Bits extends CSRWARLField

  object CSRWARLField43Bits extends CSRWARLField

  object CSRWARLField44Bits extends CSRWARLField

  object CSRWARLField45Bits extends CSRWARLField

  object CSRWARLField46Bits extends CSRWARLField

  object CSRWARLField47Bits extends CSRWARLField

  object CSRWARLField48Bits extends CSRWARLField

  object CSRWARLField49Bits extends CSRWARLField

  object CSRWARLField50Bits extends CSRWARLField

  object CSRWARLField51Bits extends CSRWARLField

  object CSRWARLField52Bits extends CSRWARLField

  object CSRWARLField53Bits extends CSRWARLField

  object CSRWARLField54Bits extends CSRWARLField

  object CSRWARLField55Bits extends CSRWARLField

  object CSRWARLField56Bits extends CSRWARLField

  object CSRWARLField57Bits extends CSRWARLField

  object CSRWARLField58Bits extends CSRWARLField

  object CSRWARLField59Bits extends CSRWARLField

  object CSRWARLField60Bits extends CSRWARLField

  object CSRWARLField61Bits extends CSRWARLField

  object CSRWARLField62Bits extends CSRWARLField

  object CSRWARLField63Bits extends CSRWARLField

  object CSRWARLField64Bits extends CSRWARLField

  object CSRROField1Bits extends CSRROField

  object CSRROField2Bits extends CSRROField

  object CSRROField3Bits extends CSRROField

  object CSRRefField1Bits extends CSRRefField

  object CSRRefField2Bits extends CSRRefField

  object CSRRefROField1Bits extends CSRRefROField

  object CSRRefROField2Bits extends CSRRefROField

  object ContextStatus extends CSRWARLField with ContextStatusDef
  trait ContextStatusDef { this: ChiselEnum =>
    val Off = Value(0.U)
    val Initial = Value(1.U)
    val Clean = Value(2.U)
    val Dirty = Value(3.U)
  }

  sealed abstract class XLENField extends CSRWARLField {
    val XLEN32 = Value(1.U)
    val XLEN64 = Value(2.U)
    val XLEN128 = Value(3.U)
  }

  object MXLField extends XLENField

  object SXLField extends XLENField

  object UXLField extends XLENField

  object VSXLField extends XLENField

  object MtvecMode extends CSRWARLField {
    val Direct = Value(0.U)
    val Vectored = Value(1.U)

    override def isLegal(enum: CSREnumType): Bool = Seq(Direct, Vectored).map(_ === enum).reduce(_ || _)
  }

  def CSRFieldWARLBits(msb: Int, lsb: Int, wfn: CSRWfnType): CSREnumType = macro CSRFieldsImpl.CSRFieldWARLBitsRange

  def CSRFieldWARLBits(bit: Int, wfn: CSRWfnType): CSREnumType = macro CSRFieldsImpl.CSRFieldWARLBitsBit

  def CSRFieldROBits(msb: Int, lsb: Int, rfn: CSRRfnType): CSREnumType = macro CSRFieldsImpl.CSRFieldROBitsRange

  def CSRFieldROBits(bit: Int, rfn: CSRRfnType): CSREnumType = macro CSRFieldsImpl.CSRFieldROBitsBit

  def CSRFieldRefBits(msb: Int, lsb: Int, ref: CSREnumType, rfn: CSRRfnType): CSREnumType = macro CSRFieldsImpl.CSRFieldRefBitsRange

  def CSRFieldRefBits(bit: Int, ref: CSREnumType, rfn: CSRRfnType): CSREnumType = macro CSRFieldsImpl.CSRFieldRefBitsBit

  def CSRFieldRefROBits(msb: Int, lsb: Int, ref: CSREnumType, rfn: CSRRfnType): CSREnumType = macro CSRFieldsImpl.CSRFieldRefROBitsRange

  def CSRFieldRefROBits(bit: Int, ref: CSREnumType, rfn: CSRRfnType): CSREnumType = macro CSRFieldsImpl.CSRFieldRefROBitsBit

  object PrivMode extends ChiselEnum {
    val U = Value(0.U)
    val S = Value(1.U)
    val M = Value(3.U)
  }

  object VirtMode extends ChiselEnum {
    val Off = Value(0.U)
    val On = Value(1.U)
  }
}
