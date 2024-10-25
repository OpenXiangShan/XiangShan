package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.Fill
import xiangshan.backend.fu.NewCSR.CSRFunc._
import scala.collection.mutable

import scala.language.implicitConversions

abstract class CSRRWType {
  val wfn: CSRWfnType
  var rfn: CSRRfnType
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
  override var rfn: CSRRfnType = null,
) extends CSRRWType

case class ROType(
  override var rfn: CSRRfnType = null,
) extends CSRRWType {
  override final val wfn: CSRWfnType = wNoEffect
  var isHardWired = false
  var hardWiredValue = 0.U

  def setHardWired(value: UInt): Unit = {
    this.isHardWired = true
    this.hardWiredValue = value
  }
}

case class WLRLType(
  override val wfn: CSRWfnType,
  override var rfn: CSRRfnType,
) extends CSRRWType

case class RWType() extends CSRRWType {
  override final val wfn: CSRWfnType = wNoFilter
  override final var rfn: CSRRfnType = null
}

trait CheckRef { self: CSRRWType =>
  require(ref.nonEmpty)
}

case class RefWARLType(
  override val ref: Option[CSREnumType],
  override val wfn: CSRWfnType,
  override var rfn: CSRRfnType = null,
) extends CSRRWType with CheckRef

case class RefROType(
  override val ref: Option[CSREnumType],
  override var rfn: CSRRfnType = null,
) extends CSRRWType with CheckRef {
  override final val wfn: CSRWfnType = wNoEffect
}

case class RefWLRLType(
  override val ref: Option[CSREnumType],
  override val wfn: CSRWfnType,
  override var rfn: CSRRfnType,
) extends CSRRWType with CheckRef

case class RefRWType(
  override val ref: Option[CSREnumType],
) extends CSRRWType with CheckRef {
  override final val wfn: CSRWfnType = wNoFilter
  override final var rfn: CSRRfnType = null
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
  var init: Data = null
)(
  override val factory: ChiselEnum
) extends EnumType(factory) {

  var otherUpdateSeq: mutable.Seq[Tuple2[Bool, Data]] = mutable.Seq()

  if (factory.all.isEmpty) {
    factory.asInstanceOf[CSREnum].addMinValue
  }

  if (this.init != null && !factory.all.exists(_.litValue == this.init.litValue)) {
    factory.asInstanceOf[CSREnum].addNewValue(init.asUInt)
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

  def isRW = this.rwType.isRW

  def isWARL = this.rwType.isWARL

  def isHardWired = this.isRO && this.rwType.asInstanceOf[ROType].isHardWired

  def getHardWireValue: UInt = this.rwType.asInstanceOf[ROType].hardWiredValue

  // Check if the write data is legal that can update the regfield.
  // Also check if the write field is not Read Only.
  def isLegal: Bool = this.factory.asInstanceOf[CSREnum].isLegal(this) && (!this.isRO).B

  def isLegal(dmode: Bool): Bool = this.factory.asInstanceOf[CSREnum].isLegal(this, dmode) && (!this.isRO).B

  // make the illegal wdata legalize
  def legalize: CSREnumType = this.factory.asInstanceOf[CSREnum].legalize(this)

  def legalize(dmode: Bool): CSREnumType = this.factory.asInstanceOf[CSREnum].legalize(this, dmode)

  def needReset: Boolean = init != null

  def rfn: CSRRfnType = rwType.rfn

  def wfn: CSRWfnType = rwType.wfn

  // Check if reset with a enum value in factory.all
  protected def resetCheck[T <: EnumType](init: T): Unit = {
    resetCheckRWType
    require(this.factory.all.contains(init),
      s"""
      | The value ${init.litValue} is NOT in ${factory.all}.
      | Please check if $init is the enum in the $factory")
      """.stripMargin
    )
  }

  // Check if reset with a enum value in factory.all
  protected def resetCheck(init: UInt): Unit = {
    resetCheckRWType
    require(this.factory.all.exists(_.litValue == init.litValue),
      s"""
      |The value ${init.litValue} is not in ${factory.all}.
      |Please add reset value as the tail of (msb,lsb, HERE) or (bit, HERE), If you need reset field with the value NOT in enum set.
      |                                                ^              ^
      |""".stripMargin
    )
  }

  protected def resetCheckRWType: Unit = {
    rwType match {
      case ROType(rfn) => require(rfn == null)
      case _ =>
    }
  }

  def withReset[T <: EnumType](init: T): this.type = {
    resetCheck(init)
    this.init = init
    if (!factory.all.exists(_.litValue == ((BigInt(1) << (msb - lsb + 1)) - 1))) {
      factory.asInstanceOf[CSREnum].addMaxValue
    }
    this
  }

  def withReset(init: UInt): this.type = {
    resetCheck(init)
    this.init = this.factory(init)
    if (!factory.all.exists(_.litValue == ((BigInt(1) << (msb - lsb + 1)) - 1))) {
      factory.asInstanceOf[CSREnum].addMaxValue
    }
    this
  }

  // Reset using the value not in factory.all
  def withNonEnumReset(init: UInt): this.type = {
    resetCheckRWType
    if (!this.factory.all.exists(_.litValue == init.litValue)) {
      this.factory.asInstanceOf[CSREnum].addNewValue(init)
      println(s"[CSR-info] add reset value ${init.litValue} into $this")
    }
    if (!factory.all.exists(_.litValue == ((BigInt(1) << (msb - lsb + 1)) - 1))) {
      factory.asInstanceOf[CSREnum].addMaxValue
    }
    this
  }

  def := (that: UInt): Unit = {
    suppressEnumCastWarning {
      this := this.factory(that)
    }
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

  def setHardWired(value: UInt): this.type = {
    require(this.isRO)
    this.rwType.asInstanceOf[ROType].setHardWired(value)
    this
  }

  def setRW(): this.type = {
    this.setRwType(RWType())
  }

  def setWARL(wfn: CSRWfnType): this.type = {
    this.setRwType(WARLType(wfn))
  }

  def ||(that: Bool): Bool = {
    require(this.getWidth == 1, s"Only 1 bit field can use operator ||. The width of left operand is ${this.getWidth}")
    this.asBool || that
  }

  def ||(that: CSREnumType): Bool = {
    require(this.getWidth == 1, s"Only 1 bit field can use operator ||. The width of left operand is ${this.getWidth}")
    require(that.getWidth == 1, s"Only 1 bit field can use operator ||. The width of right operand is ${that.getWidth}")
    this.asBool || that.asBool
  }

  def &&(that: Bool): Bool = {
    require(this.getWidth == 1, s"Only 1 bit field can use operator &&. The width of left operand is ${this.getWidth}")
    this.asBool && that
  }

  def &&(that: CSREnumType): Bool = {
    require(this.getWidth == 1, s"Only 1 bit field can use operator &&. The width of left operand is ${this.getWidth}")
    require(that.getWidth == 1, s"Only 1 bit field can use operator &&. The width of right operand is ${that.getWidth}")
    this.asBool && that.asBool
  }

  def unary_! : Bool = {
    require(this.getWidth == 1, s"Only 1 bit field can use operator &&. The width of left operand is ${this.getWidth}")
    !this.asBool
  }

  def & (that: UInt): UInt = {
    require(this.getWidth == that.getWidth || !that.widthKnown)
    this.asUInt & that
  }

  def &> (that: Bool): UInt = {
    this.asUInt & Fill(this.getWidth, that)
  }

  def |> (that: Bool): UInt = {
    this.asUInt | Fill(this.getWidth, that)
  }

  def addOtherUpdate(cond: Bool, value: CSREnumType): this.type = {
    this.otherUpdateSeq :+= (cond, value)
    this
  }

  // override cloneType to make ValidIO etc function return CSREnumType not EnumType
  override def cloneType: this.type = factory.asInstanceOf[CSREnum].makeType.asInstanceOf[this.type].setRwType(this.rwType)
}

class CSREnum extends ChiselEnum {
  protected def apply(rwType: CSRRWType)(msb: Int, lsb: Int)(factory: ChiselEnum): CSREnumType = {
    this.msb = msb
    this.lsb = lsb
    new CSREnumType(msb, lsb)(rwType, null)(factory)
  }

  var msb, lsb: Int = 0

  def makeType: CSREnumType = {
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

  /**
   *
   * @param value: A new value need to add in Enum set
   * @return this
   */
  def addNewValue(value: UInt): this.type = {
    Value(value)
    this
  }

  def isLegal(enumeration: CSREnumType): Bool = true.B

  def isLegal(enumeration: CSREnumType, dmode: Bool): Bool = true.B

  def legalize(enumeration: CSREnumType): CSREnumType = makeType

  def legalize(enumeration: CSREnumType, dmode: Bool): CSREnumType = makeType
}

trait RWApply { self: CSREnum =>
  def apply(msb: Int, lsb: Int): CSREnumType = self
    .apply(RWType())(msb, lsb)(this)

  def apply(bit: Int): CSREnumType = apply(bit, bit)
}

trait ROApply { self: CSREnum =>
  def apply(msb: Int, lsb: Int): CSREnumType = self
    .apply(ROType())(msb, lsb)(this)
}

trait WARLApply { self: CSREnum =>
  def apply(msb: Int, lsb: Int, wfn: CSRWfnType, rfn: CSRRfnType): CSREnumType = self
    .apply(WARLType(wfn, rfn))(msb, lsb)(this)

  def apply(msb: Int, lsb: Int, wfn: CSRWfnType): CSREnumType = self
    .apply(WARLType(wfn))(msb, lsb)(this)

  def apply(bit: Int, wfn: CSRWfnType): CSREnumType = apply(bit, bit, wfn)
}

trait WLRLApply { self: CSREnum =>
  def apply(msb: Int, lsb: Int, wfn: CSRWfnType, rfn: CSRRfnType): CSREnumType = self
    .apply(WLRLType(wfn, rfn))(msb, lsb)(this)
}

trait CSRMacroApply { self: CSREnum =>
  def RO(msb: Int, lsb: Int, rfn: CSRRfnType): CSREnumType = self
    .apply(ROType(rfn))(msb, lsb)(this)

  def RW(msb: Int, lsb: Int): CSREnumType = self
    .apply(RWType())(msb, lsb)(this)

  def WARL(msb: Int, lsb: Int, wfn: CSRWfnType, rfn: CSRRfnType): CSREnumType = self
    .apply(WARLType(wfn, rfn))(msb, lsb)(this)

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
  class BoolField(val value: Bool) {
    def && (field: CSREnumType): Bool = {
      this.value && field.asBool
    }

    def || (field: CSREnumType): Bool = {
      this.value || field.asBool
    }

    def &<(that: UInt): UInt = {
      require(that.widthKnown, "The width of the right operand should be known when using &< operator")
      Fill(that.getWidth, this.value) & that
    }

    def &<(that: CSREnumType): UInt = {
      this &< that.asUInt
    }

    def |<(that: UInt): UInt = {
      require(that.widthKnown, "The width of the right operand should be known when using |< operator")
      Fill(that.getWidth, this.value) | that
    }

    def |<(that: CSREnumType): UInt = {
      this |< that.asUInt
    }
  }

  implicit def BoolToBoolField(bool: Bool): BoolField = new BoolField(bool)
}

