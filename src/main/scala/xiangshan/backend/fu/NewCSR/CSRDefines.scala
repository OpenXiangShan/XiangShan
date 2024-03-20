package xiangshan.backend.fu.NewCSR

import chisel3._
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.macros.CSRMacros.CSRFieldsImpl

import scala.language.experimental.macros

object CSRDefines {
  object CSRField1Bits extends CSREnum with CSRMacroApply

  object CSRField2Bits extends CSREnum with CSRMacroApply

  object CSRField3Bits extends CSREnum with CSRMacroApply

  object CSRField4Bits extends CSREnum with CSRMacroApply

  object CSRField5Bits extends CSREnum with CSRMacroApply

  object CSRField6Bits extends CSREnum with CSRMacroApply

  object CSRField7Bits extends CSREnum with CSRMacroApply

  object CSRField8Bits extends CSREnum with CSRMacroApply

  object CSRField9Bits extends CSREnum with CSRMacroApply

  object CSRField10Bits extends CSREnum with CSRMacroApply

  object CSRField11Bits extends CSREnum with CSRMacroApply

  object CSRField12Bits extends CSREnum with CSRMacroApply

  object CSRField13Bits extends CSREnum with CSRMacroApply

  object CSRField14Bits extends CSREnum with CSRMacroApply

  object CSRField15Bits extends CSREnum with CSRMacroApply

  object CSRField16Bits extends CSREnum with CSRMacroApply

  object CSRField17Bits extends CSREnum with CSRMacroApply

  object CSRField18Bits extends CSREnum with CSRMacroApply

  object CSRField19Bits extends CSREnum with CSRMacroApply

  object CSRField20Bits extends CSREnum with CSRMacroApply

  object CSRField21Bits extends CSREnum with CSRMacroApply

  object CSRField22Bits extends CSREnum with CSRMacroApply

  object CSRField23Bits extends CSREnum with CSRMacroApply

  object CSRField24Bits extends CSREnum with CSRMacroApply

  object CSRField25Bits extends CSREnum with CSRMacroApply

  object CSRField26Bits extends CSREnum with CSRMacroApply

  object CSRField27Bits extends CSREnum with CSRMacroApply

  object CSRField28Bits extends CSREnum with CSRMacroApply

  object CSRField29Bits extends CSREnum with CSRMacroApply

  object CSRField30Bits extends CSREnum with CSRMacroApply

  object CSRField31Bits extends CSREnum with CSRMacroApply

  object CSRField32Bits extends CSREnum with CSRMacroApply

  object CSRField33Bits extends CSREnum with CSRMacroApply

  object CSRField34Bits extends CSREnum with CSRMacroApply

  object CSRField35Bits extends CSREnum with CSRMacroApply

  object CSRField36Bits extends CSREnum with CSRMacroApply

  object CSRField37Bits extends CSREnum with CSRMacroApply

  object CSRField38Bits extends CSREnum with CSRMacroApply

  object CSRField39Bits extends CSREnum with CSRMacroApply

  object CSRField40Bits extends CSREnum with CSRMacroApply

  object CSRField41Bits extends CSREnum with CSRMacroApply

  object CSRField42Bits extends CSREnum with CSRMacroApply

  object CSRField43Bits extends CSREnum with CSRMacroApply

  object CSRField44Bits extends CSREnum with CSRMacroApply

  object CSRField45Bits extends CSREnum with CSRMacroApply

  object CSRField46Bits extends CSREnum with CSRMacroApply

  object CSRField47Bits extends CSREnum with CSRMacroApply

  object CSRField48Bits extends CSREnum with CSRMacroApply

  object CSRField49Bits extends CSREnum with CSRMacroApply

  object CSRField50Bits extends CSREnum with CSRMacroApply

  object CSRField51Bits extends CSREnum with CSRMacroApply

  object CSRField52Bits extends CSREnum with CSRMacroApply

  object CSRField53Bits extends CSREnum with CSRMacroApply

  object CSRField54Bits extends CSREnum with CSRMacroApply

  object CSRField55Bits extends CSREnum with CSRMacroApply

  object CSRField56Bits extends CSREnum with CSRMacroApply

  object CSRField57Bits extends CSREnum with CSRMacroApply

  object CSRField58Bits extends CSREnum with CSRMacroApply

  object CSRField59Bits extends CSREnum with CSRMacroApply

  object CSRField60Bits extends CSREnum with CSRMacroApply

  object CSRField61Bits extends CSREnum with CSRMacroApply

  object CSRField62Bits extends CSREnum with CSRMacroApply

  object CSRField63Bits extends CSREnum with CSRMacroApply

  object CSRField64Bits extends CSREnum with CSRMacroApply

  object ContextStatus extends CSREnum with ContextStatusDef with CSRRWApply
  object ContextStatusRO extends CSREnum with ContextStatusDef with CSRROApply
  trait ContextStatusDef { this: CSREnum =>
    val Off = Value(0.U)
    val Initial = Value(1.U)
    val Clean = Value(2.U)
    val Dirty = Value(3.U)
  }

  sealed abstract class XLENField extends CSREnum {
    val XLEN32 = Value(1.U)
    val XLEN64 = Value(2.U)
    val XLEN128 = Value(3.U)
  }

  object MXLField extends XLENField with CSRROApply

  object SXLField extends XLENField with CSRROApply

  object UXLField extends XLENField with CSRROApply

  object VSXLField extends XLENField with CSRROApply

  object MtvecMode extends CSREnum with CSRWARLApply {
    val Direct = Value(0.U)
    val Vectored = Value(1.U)

    override def isLegal(enum: CSREnumType): Bool = Seq(Direct, Vectored).map(_ === enum).reduce(_ || _)
  }

  object CSRWARLField {
    def apply(msb: Int, lsb: Int, fn: CSRRfnType): CSREnumType = macro CSRFieldsImpl.CSRWARLFieldRange

    def apply(bit: Int, fn: CSRRfnType): CSREnumType = macro CSRFieldsImpl.CSRWARLFieldBit

    def apply(msb: Int, lsb: Int, fn: CSRWfnType): CSREnumType = macro CSRFieldsImpl.CSRWARLFieldRange

    def apply(bit: Int, fn: CSRWfnType): CSREnumType = macro CSRFieldsImpl.CSRWARLFieldBit
  }

  object CSRROField {
    def apply(msb: Int, lsb: Int, rfn: CSRRfnType): CSREnumType = macro CSRFieldsImpl.CSRROFieldRange

    def apply(bit: Int, rfn: CSRRfnType): CSREnumType = macro CSRFieldsImpl.CSRROFieldBit
  }

  object CSRWARLRefField {
    def apply(ref: CSREnumType, msb: Int, lsb: Int, wfn: CSRWfnType): CSREnumType = macro CSRFieldsImpl.CSRRefWARLFieldRange

    def apply(ref: CSREnumType, bit: Int, wfn: CSRWfnType): CSREnumType = macro CSRFieldsImpl.CSRRefWARLFieldBit
  }

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
