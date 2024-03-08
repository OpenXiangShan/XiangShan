package xiangshan.backend.fu.NewCSR

import chisel3._

import xiangshan.backend.fu.NewCSR.CSRFunc._

import scala.language.experimental.macros

abstract class CSRRWType {
  val wfn: CSRWfnType
  val rfn: CSRRfnType
  val ref: Option[CSREnumType] = None

  def isRO: Boolean = this.isInstanceOf[ROType] || this.isInstanceOf[RefROType]

  def isRW: Boolean = this.isInstanceOf[RWType] || this.isInstanceOf[RefRWType]

  def isWARL: Boolean = this.isInstanceOf[WARLType] || this.isInstanceOf[RefWARLType]

  def isWLRL: Boolean = this.isInstanceOf[WLRLType] || this.isInstanceOf[RefWLRLType]

  def isRef: Boolean = this.isInstanceOf[RefROType] || this.isInstanceOf[RefRWType] || this.isInstanceOf[RefWARLType] ||
    this.isInstanceOf[RefWLRLType]

  override def toString: String = {
    val typeString = this match {
      case WARLType(_, _) => "WARL"
      case ROType(_)      => "RO"
      case WLRLType(_, _) => "WLRL"
      case RWType()       => "RW"
    }
    typeString + (if (isRef) " Ref" else "")
  }
}

case class WARLType(
  override val wfn: CSRWfnType,
  override val rfn: CSRRfnType = null,
) extends CSRRWType

case class ROType(
  override val rfn: CSRRfnType = null,
) extends CSRRWType {
  override final val wfn: CSRWfnType = wNoEffect
}

case class WLRLType(
  override val wfn: CSRWfnType,
  override val rfn: CSRRfnType,
) extends CSRRWType

case class RWType() extends CSRRWType {
  override final val wfn: CSRWfnType = wNoFilter
  override final val rfn: CSRRfnType = null
}

trait CheckRef { self: CSRRWType =>
  require(ref.nonEmpty)
}

case class RefWARLType(
  override val ref: Option[CSREnumType],
  override val wfn: CSRWfnType,
  override val rfn: CSRRfnType = null,
) extends CSRRWType with CheckRef

case class RefROType(
  override val ref: Option[CSREnumType],
  override val rfn: CSRRfnType = null,
) extends CSRRWType with CheckRef {
  override final val wfn: CSRWfnType = wNoEffect
}

case class RefWLRLType(
  override val ref: Option[CSREnumType],
  override val wfn: CSRWfnType,
  override val rfn: CSRRfnType,
) extends CSRRWType with CheckRef

case class RefRWType(
  override val ref: Option[CSREnumType],
) extends CSRRWType with CheckRef {
  override final val wfn: CSRWfnType = wNoFilter
  override final val rfn: CSRRfnType = null
}

object CSRFunc {
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

  def rNoFilter: CSRRfnType = null

  def rWithFilter(rFilter: (UInt, Seq[Data]) => UInt): CSRRfnType =
    (oriV: UInt, seq: Seq[Data]) => rFilter(oriV, seq)

  def rFixValue(value: UInt): CSRRfnType = {
    (_, _) => value
  }
}

class CSREnumType(
  val msb: Int,
  val lsb: Int,
)(
  var rwType: CSRRWType,
)(
  override val factory: ChiselEnum
) extends EnumType(factory) {
  var init: Option[EnumType] = None

  if (factory.all.size == 0) {
    factory.asInstanceOf[CSREnum].addMinValue
  }

  if (!factory.all.exists(_.litValue == ((BigInt(1) << (msb - lsb + 1)) - 1))) {
    factory.asInstanceOf[CSREnum].addMaxValue
  }

  if (msb - lsb + 1 > this.getWidth)
    println(
      s"[CSRInfo] $this: " +
      s"the setting range($msb, $lsb) of bitfield is widen than EnumType's width(${this.getWidth}), " +
      s"the higher bits will be optimized"
    )

  def isRef = this.rwType.isRef

  def isRO = this.rwType.isRO

  def isWARL = this.rwType.isWARL

  // Check if the write data is legal that can update the regfield.
  // Also check if the write field is not Read Only.
  def isLegal: Bool = this.factory.asInstanceOf[CSREnum].isLegal(this) && (!this.isRO).B

  def needReset: Boolean = init.nonEmpty

  def rfn: CSRRfnType = rwType.rfn

  def wfn: CSRWfnType = rwType.wfn

  protected def resetCheck: Unit = {
    rwType match {
      case ROType(rfn) => require(rfn == null)
      case _ =>
    }
  }

  def withReset[T <: EnumType](init: T): this.type = {
    resetCheck
    this.init = Some(init)
    this
  }

  def withReset(init: UInt): this.type = {
    resetCheck
    this.init = Some(this.factory(init))
    this
  }

  def := (that: UInt): Unit = {
    this := this.factory(that)
  }

  def dumpName = {
    s"${chisel3.reflect.DataMirror.queryNameGuess(this)} ${rwType} [$msb, $lsb] reset($init)"
  }

  def asBool: Bool = {
    this.asUInt.asBool
  }

  private def setRwType(newType: CSRRWType): this.type = {
    this.rwType = newType
    this
  }

  def setRO(rfn: CSRRfnType = null): this.type = {
    this.setRwType(ROType(rfn))
  }

  def setRW(): this.type = {
    this.setRwType(RWType())
  }

  def setWARL(wfn: CSRWfnType): this.type = {
    this.setRwType(WARLType(wfn))
  }

  // override cloneType to make ValidIO etc function return CSREnumType not EnumType
  override def cloneType: this.type = factory.asInstanceOf[CSREnum].makeType.asInstanceOf[this.type]
}

abstract class CSREnum extends ChiselEnum {
  type Type = CSREnumType

  protected def apply(rwType: CSRRWType)(msb: Int, lsb: Int)(factory: ChiselEnum): CSREnumType = {
    this.msb = msb
    this.lsb = lsb
    new CSREnumType(msb, lsb)(rwType)(factory)
  }

  var msb, lsb: Int = 0

  def makeType: Type = {
    new CSREnumType(msb, lsb)(RWType())(this)
  }

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

  def isLegal(enum: CSREnumType): Bool = true.B

  println(s"A new CSREnum is created, factory: $this")
}

trait CSRROApply { self: CSREnum =>
  def apply(msb: Int, lsb: Int): CSREnumType = self
    .apply(ROType())(msb, lsb)(this)
}

trait CSRRWApply { self: CSREnum =>
  def apply(msb: Int, lsb: Int): CSREnumType = self
    .apply(RWType())(msb, lsb)(this)
}

trait CSRWARLApply { self: CSREnum =>
  def apply(msb: Int, lsb: Int, wfn: CSRWfnType, rfn: CSRRfnType): CSREnumType = self
    .apply(WARLType(wfn, rfn))(msb, lsb)(this)

  def apply(msb: Int, lsb: Int, wfn: CSRWfnType): CSREnumType = self
    .apply(WARLType(wfn))(msb, lsb)(this)
}

trait CSRWLRLApply { self: CSREnum =>
  def apply(msb: Int, lsb: Int, wfn: CSRWfnType, rfn: CSRRfnType): CSREnumType = self
    .apply(WLRLType(wfn, rfn))(msb, lsb)(this)
}

trait CSRMacroApply { self: CSREnum =>
  def RO(msb: Int, lsb: Int, rfn: CSRRfnType): CSREnumType = self
    .apply(ROType(rfn))(msb, lsb)(this)

  def RO(msb: Int, lsb: Int): CSREnumType = self
    .apply(ROType())(msb, lsb)(this)

  def RW(msb: Int, lsb: Int): CSREnumType = self
    .apply(RWType())(msb, lsb)(this)

  def WARL(msb: Int, lsb: Int, wfn: CSRWfnType, rfn: CSRRfnType): CSREnumType = self
    .apply(WARLType(wfn, rfn))(msb, lsb)(this)

  def WARL(msb: Int, lsb: Int, wfn: CSRWfnType): CSREnumType = self
    .apply(WARLType(wfn))(msb, lsb)(this)

  def WLRL(msb: Int, lsb: Int, wfn: CSRWfnType, rfn: CSRRfnType): CSREnumType = self
    .apply(WLRLType(wfn, rfn))(msb, lsb)(this)

  def RefRO(ref: CSREnumType, msb: Int, lsb: Int, rfn: CSRRfnType): CSREnumType = self
    .apply(RefROType(Some(ref) ,rfn))(msb, lsb)(ref.factory)

  def RefRO(ref: CSREnumType, msb: Int, lsb: Int): CSREnumType = self
    .apply(RefROType(Some(ref)))(msb, lsb)(ref.factory)

  def RefWARL(ref: CSREnumType, msb: Int, lsb: Int, wfn: CSRWfnType, rfn: CSRRfnType): CSREnumType = self
    .apply(RefWARLType(Some(ref), wfn, rfn))(msb, lsb)(ref.factory)

  def RefWARL(ref: CSREnumType, msb: Int, lsb: Int, wfn: CSRWfnType): CSREnumType = self
    .apply(RefWARLType(Some(ref), wfn))(msb, lsb)(ref.factory)
}

object CSREnumTypeImplicitCast {
  implicit def CSREnumTypeToUInt(field: CSREnumType): UInt = {
    field.asUInt
  }

  class BoolField(val value: Bool) {
    def && (field: CSREnumType): Bool = {
      this.value && field.asBool
    }

    def || (field: CSREnumType): Bool = {
      this.value || field.asBool
    }
  }

  implicit def BoolToBoolField(bool: Bool): BoolField = new BoolField(bool)
}

