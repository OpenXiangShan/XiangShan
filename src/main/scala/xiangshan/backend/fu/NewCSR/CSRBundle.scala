package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.util.Cat
import chisel3.experimental.BundleLiterals._

import scala.language.experimental.macros


abstract class CSRBundle extends Bundle {
  val len: Int = 64

  var eventFields: Set[CSREnumType] = Set()

  override def do_asUInt(implicit sourceInfo: SourceInfo): UInt = {
    val fields = this.getFields
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
      field := (if (field.init.nonEmpty) field.init.get else field.factory(0.U))
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
}
