package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.util.{Cat, Fill}
import chisel3.experimental.BundleLiterals._

import scala.language.experimental.macros


abstract class CSRBundle extends Bundle {
  val len: Int = 64

  var eventFields: Set[CSREnumType] = Set()

  override def do_asUInt(implicit sourceInfo: SourceInfo): UInt = {
    // sorted from msb to lsb
    val fields = this.getFields.sortWith((l, r) => l.lsb > r.lsb)
    var paddedFields: Seq[Data] = Seq()
    var lsb = len

    for (field <- fields) {
      val diffWidth = lsb - field.lsb - field.getWidth
      if (diffWidth > 0)
        paddedFields :+= 0.U((diffWidth).W)
      paddedFields :+= field
      lsb = field.lsb
    }

    if (fields.last.lsb > 0) {
      paddedFields :+= 0.U(fields.last.lsb.W)
    }

    Cat(paddedFields.map(x => x.asUInt))
  }

  def := (that: UInt): Unit = {
    val fields = this.getFields

    for (field <- fields) {
      field := field.factory.apply(that(field.lsb + field.getWidth - 1, field.lsb))
    }
  }

  @inline
  def init: this.type = {
    val init = Wire(this)
    init.elements.foreach { case (str, field: CSREnumType) =>
      field := (if (field.init != null) field.factory(field.init.asUInt) else field.factory(0.U))
    }
    init.asInstanceOf[this.type]
  }

  /**
   * filtered read connection
   *
   * CSRBundle will be filtered by CSRFields' read filter function.
   */
  def :|= [T <: CSRBundle](that: T): Unit = {
    if (this.getClass != that.getClass) {
      throw MonoConnectException(s"Type miss match! (sink :|= source) " +
        s"sink type: ${this.getClass}, " +
        s"source type: ${that.getClass}")
    }

    for ((sink: CSREnumType, source: CSREnumType)  <- this.getFields.zip(that.getFields)) {
      if (sink.rfn == null)
        sink := source // .factory.apply(sink.rfn(source.asUInt, Seq()))
      else
        sink := sink.factory(sink.rfn(source.asUInt, Seq()))
    }
  }

  def getFields: Seq[CSREnumType] = this.getElements.map(_.asInstanceOf[CSREnumType])

  def needReset: Boolean = this.getFields.exists(_.needReset)

  // used by event bundle to filter the fields need to update
  def addInEvent(fieldFns: (this.type => CSREnumType)*): this.type = {
    this.eventFields ++= fieldFns.map(fn => fn(this))
    this
  }

  override def cloneType: CSRBundle.this.type = {
    val ret = super.cloneType
    //
    (ret.getFields zip this.getFields).foreach { case (l, r) =>
      if (this.eventFields.contains(r)) {
        ret.eventFields += l
      }
    }
    ret
  }

  def & (that: CSRBundle): UInt = {
    this.asUInt & that.asUInt
  }

  def & (that: UInt): UInt = {
    require(this.asUInt.getWidth == that.getWidth,
      s"The width between left $this(${this.getWidth}) and right $that(${that.getWidth}) should be equal.")
    this.asUInt & that
  }

  def | (that: CSRBundle): UInt = {
    this.asUInt | that.asUInt
  }

  def | (that: UInt): UInt = {
    require(this.getWidth == that.getWidth)
    this.asUInt | that
  }

  def unary_~ : UInt = {
    (~this.asUInt).asUInt
  }

  def apply(num: Int) : CSREnumType = {
    this.getFields.find(x => x.lsb == num && x.msb == num).get
  }

  def apply(str: String) : CSREnumType = {
    elements(str).asInstanceOf[CSREnumType]
  }
}

object CSRBundleImplicitCast {
  class UIntField(val value: UInt) {
    def &[T <: CSRBundle] (field: T): UInt = {
      this.value & field.asUInt
    }

    def |[T <: CSRBundle] (field: T): UInt = {
      this.value | field.asUInt
    }

    def &>(that: Bool): UInt = {
      require(value.widthKnown, "The width of the left operand should be known when using >& operator")
      this.value & Fill(value.getWidth, that)
    }

    def |>(that: Bool): UInt = {
      require(value.widthKnown, "The width of the left operand should be known when using >| operator")
      this.value | Fill(value.getWidth, that)
    }
  }

  implicit def UIntToUIntField(uint: UInt): UIntField = new UIntField(uint)
}

object ChiselRecordForField {
  implicit class AddRecordSpecifyFields[T <: Record](val x: T) {
    def specifyField(elemFns: (T => Unit)*): Unit = {
      elemFns.foreach(_.apply(x))
    }
  }
}
