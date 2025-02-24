package utils

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._


class ResizeCircularQueuePtr[T <: ResizeCircularQueuePtr[T]](val entries: Int) extends Bundle {

  def this(f: Parameters => Int)(implicit p: Parameters) = this(f(p))

  val PTR_WIDTH = log2Up(entries)
  val flag = Bool()
  val value = UInt(PTR_WIDTH.W)
  val psize = Valid(UInt((PTR_WIDTH + 1).W))

  override def toPrintable: Printable = {
    p"$flag:$value"
  }

  final def +(v: UInt): T = {
    val new_ptr = Wire(this.asInstanceOf[T].cloneType)
    val effectiveSize = Mux(psize.valid,
      psize.bits,
      entries.U)

    if(isPow2(entries)){
      // 对于2的幂的情况，我们仍需要检查是否达到psize
      val new_value = (this.value + v)(PTR_WIDTH-1, 0)
      val would_exceed = (new_value >= effectiveSize)
      new_ptr.flag := Mux(would_exceed, !this.flag, this.flag)
      new_ptr.value := Mux(would_exceed,
        new_value - effectiveSize,
        new_value)
    } else {
      val new_value = this.value +& v
      val diff = Cat(0.U(1.W), new_value).asSInt - Cat(0.U(1.W), effectiveSize).asSInt
      val reverse_flag = diff >= 0.S
      new_ptr.flag := Mux(reverse_flag, !this.flag, this.flag)
      new_ptr.value := Mux(reverse_flag,
        diff.asUInt,
        new_value
      )
    }
    // 复制动态边界设置
    new_ptr.psize := this.psize
    new_ptr
  }

  final def -(v: UInt): T = {
    val effectiveSize = Mux(psize.valid,
      psize.bits,
      entries.U)
    val flipped_new_ptr = this + (effectiveSize - v)
    val new_ptr = Wire(this.asInstanceOf[T].cloneType)
    new_ptr.flag := !flipped_new_ptr.flag
    new_ptr.value := flipped_new_ptr.value
    new_ptr.psize := this.psize
    new_ptr
  }

  final def === (that_ptr: T): Bool = this.asUInt===that_ptr.asUInt

  final def =/= (that_ptr: T): Bool = this.asUInt=/=that_ptr.asUInt

  def toOH: UInt = UIntToOH(value, entries)

  def resize(v: UInt, p: UInt): T = {
    val new_ptr = Wire(this.asInstanceOf[T].cloneType)
    new_ptr.flag := false.B
    new_ptr.value := v
    new_ptr.psize.valid := true.B
    new_ptr.psize.bits := p
    new_ptr
  }
}

// 辅助函数也需要相应修改
trait HasResizeCircularQueuePtrHelper {

  def isEmpty[T <: ResizeCircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): Bool = {
    enq_ptr === deq_ptr
  }

  def isFull[T <: ResizeCircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): Bool = {
    val effectiveSize = Mux(enq_ptr.psize.valid,
      enq_ptr.psize.bits,
      enq_ptr.entries.U)
    val distance = distanceBetween(enq_ptr, deq_ptr)
    distance === (effectiveSize - 1.U)
  }

  def distanceBetween[T <: ResizeCircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): UInt = {
    assert(enq_ptr.entries == deq_ptr.entries)
    val effectiveSize = Mux(enq_ptr.psize.valid,
      enq_ptr.psize.bits,
      enq_ptr.entries.U)
    Mux(enq_ptr.flag === deq_ptr.flag,
      enq_ptr.value - deq_ptr.value,
      effectiveSize + enq_ptr.value - deq_ptr.value)
  }

  def isAfter[T <: ResizeCircularQueuePtr[T]](left: T, right: T): Bool = {
    val differentFlag = left.flag ^ right.flag
    val compare = left.value > right.value
    differentFlag ^ compare
  }

  def isBefore[T <: ResizeCircularQueuePtr[T]](left: T, right: T): Bool = {
    val differentFlag = left.flag ^ right.flag
    val compare = left.value < right.value
    differentFlag ^ compare
  }

}