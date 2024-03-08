package xiangshan.backend.fu.util.CSR

import chisel3._

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import chisel3.internal.firrtl.Arg
import utils.XSInfo

abstract class CSRRWType
case object WARL extends CSRRWType
case object RO extends CSRRWType
case object WLRL extends CSRRWType
case object RW extends CSRRWType

trait CSRFuncTrait {
  type CSRWfnType = (UInt, UInt, Seq[Data]) => UInt

  def wNoFilter: CSRWfnType =
    (newV: UInt, oldV: UInt, _: Seq[Data]) => newV

  def wNoEffectWhen(keepCond: UInt => Bool): CSRWfnType =
    (newV: UInt, oldV: UInt, _: Seq[Data]) => {
      Mux(keepCond(newV), oldV, newV)
    }

  def wNoEffect: CSRWfnType =
    (_: UInt, oldV: UInt, _: Seq[Data]) => { oldV }

  type CSRRfnType = (UInt, Seq[Data]) => UInt

  def rNoFilter: CSRRfnType =
    (oriV: UInt, _: Seq[Data]) => oriV

  def rWithFilter(rFilter: (UInt, Seq[Data]) => UInt): CSRRfnType =
    (oriV: UInt, seq: Seq[Data]) => rFilter(oriV, seq)

  def rFixValue(value: UInt): CSRRfnType = {
    (_, _) => value
  }

  type CSRImplicitWfnType = (UInt, Seq[Data]) => UInt
}

class CSREnumType(
  val msb: Int,
  val lsb: Int,
  val refField: Option[CSREnumType] = None,
)(
  val wfn: CSRFuncTrait#CSRWfnType,
  val rfn: CSRFuncTrait#CSRRfnType,
  val implicitWfn: CSRFuncTrait#CSRImplicitWfnType,
)(
  override val factory: ChiselEnum
) extends EnumType(factory) with CSRFuncTrait {
  var refedFields: Seq[CSREnumType] = Seq()

  // bind CSRField with its CSRModule
  private var csrMod: CSROldModule[_] = null

  if (factory.all.size == 0) {
    factory.asInstanceOf[CSREnum].addMinValue
  }

  if (!factory.all.exists(_.litValue == ((BigInt(1) << (msb - lsb + 1)) - 1))) {
    factory.asInstanceOf[CSREnum].addMaxValue
  }

  println(s"factory.all.size: ${factory.all.size}")

  println(s"A new CSREnumType is created, factory: $factory, width: ${factory.getWidth}")

  if (msb - lsb + 1 > this.getWidth)
    println(
      s"[CSRInfo] $this: " +
      s"the setting range($msb, $lsb) of bitfield is widen than EnumType's width(${this.getWidth}), " +
      s"the higher bits will be optimized"
    )

  def bindCSRModule(mod: CSROldModule[_]): Unit = {
    this.csrMod = mod
  }

  def getCSRModule: CSROldModule[_] = this.csrMod

  def localName = Arg.earlyLocalName(this)

  def registerRefedField(field: CSREnumType): Unit = {
    this.refedFields :+= field
  }

  def isRefField = this.refField.nonEmpty

  def isLegal: Bool = this.factory.asInstanceOf[CSREnum].isLegal(this)
}

abstract class CSREnum extends ChiselEnum with CSRFuncTrait {
  def apply(msb: Int, lsb: Int)(wfn: CSRWfnType, rfn: CSRRfnType, impWfn: CSRImplicitWfnType)(factory: ChiselEnum): CSREnumType = {
    this.msb = msb
    this.lsb = lsb
    new CSREnumType(msb, lsb)(wfn, rfn, impWfn)(factory)
  }

  var msb, lsb: Int = 0


  /**
   * Used to allow 0.U.asTypeOf(CSREnumInstance) convertion
   */
  def addMinValue: Unit = {
    Value(0.U)
  }

  /**
   * A trick to expand the width of Enum to (msb - lsb + 1)
   */
  def addMaxValue: Unit = {
    Value(((BigInt(1) << (msb - lsb + 1)) - 1).U)
  }

  def isLegal(enum: CSREnumType): Bool = enum.isValid

  println(s"A new CSREnum is created, factory: $this")
}

abstract class CSRWARLField extends CSREnum {
  def apply(msb: Int, lsb: Int, wfn: CSRWfnType): CSREnumType =
    super.apply (msb, lsb)(wfn, rNoFilter, null)(this)

  def apply(msb: Int, lsb: Int, rfn: CSRRfnType): CSREnumType =
    super.apply(msb, lsb)(wNoFilter, rfn, null)(this)
}

abstract class CSRROField extends CSREnum {
  def apply(msb: Int, lsb: Int, rfn: CSRRfnType): CSREnumType =
    super.apply (msb, lsb)(wNoFilter, rfn, null)(this)
}

abstract class CSRRefField extends CSREnum {
  def apply[T <: CSREnumType](msb: Int, lsb: Int, ref: T, wfn: CSRWfnType, rfn: CSRRfnType): CSREnumType =
    new CSREnumType(msb, lsb, Some(ref))(wfn, rfn, null)(ref.factory)

  def apply[T <: CSREnumType](msb: Int, lsb: Int, ref: T, rfn: CSRRfnType): CSREnumType =
    new CSREnumType(msb, lsb, Some(ref))(wNoFilter, rfn, null)(ref.factory)
}

abstract class CSRRefROField extends CSREnum {
  def apply[T <: CSREnumType](msb: Int, lsb: Int, ref: T, rfn: CSRRfnType): CSREnumType =
    new CSREnumType(msb, lsb, Some(ref))(wNoFilter, rfn, null)(ref.factory)
}


