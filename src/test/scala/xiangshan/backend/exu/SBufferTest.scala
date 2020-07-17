package xiangshan.backend.exu

import chiseltest.ChiselScalatestTester
import org.scalatest.{FlatSpec, Matchers, ParallelTestExecution}
import xiangshan.HasXSParameter
import xiangshan.mem.pipeline.HasSBufferConst
import xiangshan.testutils.HasPartialDecoupledDriver

import scala.collection.mutable.ArrayBuffer

class CacheLine extends HasSBufferConst {
  var valid = false
  var tag = 0
  val data: ArrayBuffer[Int] = ArrayBuffer.fill[Int](cacheMaskWidth)(0) // 64 bytes
  val mask: ArrayBuffer[Boolean] = ArrayBuffer.fill[Boolean](cacheMaskWidth)(false) // corresponding valid bits
  var lruCnt = 0

  def inc(): Unit = lruCnt += 1
  def clear(): Unit = lruCnt = 0

  def evict(): Unit = valid = false

  def addNew(tag: Int, data: Array[Int], mask: Array[Boolean]): Unit = {
    assert(!valid)
    assert(data.length == cacheMaskWidth && mask.length == cacheMaskWidth)

    valid = true
    this.tag = tag
    (0 until cacheMaskWidth).foreach(i => {
      this.data(i) = data(i)
      this.mask(i) = mask(i)
    })
    this.lruCnt = 0
  }

  def update(data: Array[Int], mask: Array[Boolean]): Unit = {
    assert(valid)
    assert(data.length == cacheMaskWidth && mask.length == cacheMaskWidth)

    (0 until cacheMaskWidth).foreach(i => {
      if (mask(i)) {
        this.mask(i) = true
        this.data(i) = data(i)
      }
    })
    lruCnt = 0
  }
}

class StoreBuffer extends HasXSParameter with HasSBufferConst {
  // TODO
}

class SBufferTest extends FlatSpec
  with ChiselScalatestTester
  with Matchers
  with ParallelTestExecution
  with HasPartialDecoupledDriver
{
  it should "behave like its emulator" in {
    // TODO
  }
}
