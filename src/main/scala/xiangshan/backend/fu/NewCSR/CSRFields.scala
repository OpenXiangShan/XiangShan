package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.Fill
import xiangshan.backend.fu.NewCSR.CSRFunc._
import scala.collection.mutable
import scala.util.Try

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
  private var descriptionText: Option[String] = None
  private var warlConstraintText: Option[String] = None

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

  /**
   * Overrides the field description used when exporting CSR documentation.
   *
   * If this method is not called, the exporter falls back to the Scala field name.
   */
  def withDescription(description: String): this.type = {
    descriptionText = Option(description).map(_.trim).filter(_.nonEmpty)
    this
  }

  /**
   * Returns the field description, falling back to the provided field name when no explicit
   * description has been attached with [[withDescription]].
   */
  def getDescription(defaultFieldName: String): String = {
    descriptionText.getOrElse(defaultFieldName)
  }

  /**
   * Attaches an explicit WARL constraint string to this field.
   *
   * This is intended for cases where the legal values cannot be described by a simple enum
   * set, for example dynamic ranges or context-dependent write rules.
   */
  def withWarlConstraint(description: String): this.type = {
    warlConstraintText = Option(description).map(_.trim).filter(_.nonEmpty)
    this
  }

  /**
   * Returns the WARL constraint string for this field.
   *
   * Explicit field-local constraints added by [[withWarlConstraint]] take precedence over the
   * enum-level documentation generated by [[CSREnum.warlConstraintDescription]].
   */
  def getWarlConstraint: Option[String] = {
    if (!this.isWARL) None
    else warlConstraintText.orElse(factory.asInstanceOf[CSREnum].warlConstraintDescription(this))
  }

  /**
   * Returns the full documentation string exported for this field.
   *
   * The result combines the semantic description with the WARL constraint text when present.
   */
  def getDocumentedDescription(defaultFieldName: String): String = {
    val description = getDescription(defaultFieldName)
    getWarlConstraint match {
      case Some(constraint) if !description.contains(constraint) => s"$description WARL: $constraint"
      case _ => description
    }
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

  override def _fromUInt(that: UInt)(implicit sourceInfo: experimental.SourceInfo): Data = {
    val result = Wire(factory.asInstanceOf[CSREnum].makeType.asInstanceOf[this.type].setRwType(this.rwType))
    result := that
    result
  }

  // override cloneType to make ValidIO etc function return CSREnumType not EnumType
  override def cloneType: this.type = {
    val cloned = factory.asInstanceOf[CSREnum].makeType.asInstanceOf[this.type].setRwType(this.rwType)
    descriptionText.foreach(cloned.withDescription)
    warlConstraintText.foreach(cloned.withWarlConstraint)
    cloned
  }
}

class CSREnum extends ChiselEnum {
  /**
   * Internal pair used to render a legal enum value and its display label in WARL text.
   */
  protected case class WarlValueDoc(value: BigInt, label: String)

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

  /**
   * Declares the legal values of this enum when the legality rule can be described as a
   * finite set of enum literals.
   *
   * This is the default single source of truth used by [[isLegal(enumeration: CSREnumType)]]
   * and by CSR documentation
   * generation. Enums with context-dependent legality can keep overriding [[isLegal]].
   */
  protected def legalValues: Seq[EnumType] = Seq.empty

  /**
   * Declares a legal contiguous range for enums whose legality is described by a numeric
   * interval instead of a finite set of literals.
   *
   * This is intended for cases such as selector fields whose accepted values are in
   * `[min, max]`.
   */
  protected def legalRange: Option[(BigInt, BigInt)] = None

  /**
   * Best-effort mapping from enum literal value to the corresponding Scala member name,
   * for example `Direct`, `Sv39`, or `NAPOT`.
   */
  private lazy val reflectedLegalValueLabels: Map[BigInt, String] = {
    this.getClass.getDeclaredMethods.toSeq
      .filter(method =>
        method.getParameterCount == 0 &&
        classOf[EnumType].isAssignableFrom(method.getReturnType) &&
        !method.isSynthetic &&
        !method.getName.contains("$")
      )
      .flatMap { method =>
        method.setAccessible(true)
        Try(method.invoke(this)).toOption.collect {
          case enumValue: EnumType => enumValue.litValue -> method.getName
        }
      }
      .foldLeft(Map.empty[BigInt, String]) { case (acc, (value, name)) =>
        acc.updated(value, acc.getOrElse(value, name))
      }
  }

  /**
   * Returns the label used in generated WARL documentation for a legal enum value.
   */
  protected def legalValueLabel(value: EnumType): String = {
    reflectedLegalValueLabels
      .get(value.litValue)
      .getOrElse(value.litValue.toString)
  }

  /**
   * Formats a numeric legal bound in generated WARL documentation.
   *
   * Subclasses can override this to use hexadecimal or other domain-specific formatting.
   */
  protected def legalBoundString(value: BigInt): String = value.toString

  /**
   * Default legality check for enums whose legal states are listed in [[legalValues]].
   *
   * Subclasses with runtime- or context-dependent legality can override this method.
   */
  def isLegal(enumeration: CSREnumType): Bool = {
    if (legalValues.nonEmpty) {
      legalValues.map(enumeration === _).reduce(_ || _)
    } else {
      legalRange match {
        case Some((min, max)) => enumeration.asUInt >= min.U && enumeration.asUInt <= max.U
        case None => true.B
      }
    }
  }

  /**
   * Variant of [[isLegal]] for enums whose legality depends on debug mode or similar context.
   *
   * The default implementation keeps the original always-legal behavior for enums that do not
   * explicitly use a `dmode`-dependent legality rule. Subclasses can override it when legality
   * depends on `dmode`.
   */
  def isLegal(enumeration: CSREnumType, dmode: Bool): Bool = true.B

  /**
   * Internal representation of the legal values used when rendering WARL documentation text.
   */
  protected def legalValueDocs: Seq[WarlValueDoc] = {
    legalValues.map(value => WarlValueDoc(value.litValue, legalValueLabel(value)))
  }

  /**
   * Optional suffix describing how illegal values are handled, for example legalizing them to
   * a specific supported value.
   */
  protected def illegalValueBehavior: Option[String] = None

  private def formatWarlValueDoc(doc: WarlValueDoc): String = s"${doc.value}=${doc.label}"

  /**
   * Builds the WARL constraint text shown in exported CSR documentation.
   *
   * By default this is generated from [[legalValues]] and [[illegalValueBehavior]].
   */
  def warlConstraintDescription(enumeration: CSREnumType): Option[String] = {
    if (legalValueDocs.nonEmpty) {
      val legalValueText = legalValueDocs.map(formatWarlValueDoc).mkString(", ")
      val suffix = illegalValueBehavior.map(_.trim).filter(_.nonEmpty).map(" " + _).getOrElse("")
      Some(s"Legal values: $legalValueText.$suffix".trim)
    } else {
      legalRange.map { case (min, max) =>
        s"Legal values are in the range ${legalBoundString(min)} to ${legalBoundString(max)}."
      }
    }
  }

  /**
   * Converts an arbitrary enum value into a legal one.
   *
   * Subclasses should override this when illegal writes are sanitized to a supported value.
   */
  def legalize(enumeration: CSREnumType): CSREnumType = makeType

  /**
   * Context-sensitive variant of [[legalize]] for enums whose legality depends on debug mode or
   * similar external state.
   */
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
