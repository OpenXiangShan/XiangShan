package xiangshan.backend.exu

import chisel3.{Bundle, Flipped, Module, Output, Vec}
import chisel3.internal.DontCareBinding
import chisel3.util.Decoupled
import chisel3.util.experimental.BoringUtils
import chiseltest.ChiselScalatestTester
import org.scalatest.{FlatSpec, Matchers, ParallelTestExecution}
import xiangshan.{HasXSParameter, XSConfig}
import xiangshan.mem.HasMEMConst
import xiangshan.mem.pipeline.{HasSBufferConst, LsPipelineBundle, SBufferCacheLine, Sbuffer}
import xiangshan.testutils.{AddSinks, HasPartialDecoupledDriver}

import scala.collection.mutable.ArrayBuffer

class SBufferToDCache

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

class StoreBuffer(implicit dCacheDelay: Int = 1)
  extends HasXSParameter
    with HasSBufferConst
    with HasMEMConst {
  val cache: Array[CacheLine] = Array.fill[CacheLine](SbufferSize)(new CacheLine)
  val cacheNext: Array[CacheLine] = Array.fill[CacheLine](SbufferSize)(new CacheLine)

  def cacheCopy(src: Array[CacheLine], dst: Array[CacheLine]): Unit = {
    assert(src.length == SbufferSize && dst.length == SbufferSize)

    for (i <- 0 until SbufferSize) {
//      dst(i) = src(i)
      dst(i).valid = src(i).valid
      dst(i).tag = src(i).tag
//      dst(i).data = src(i).data
      for (j <- dst(i).data.indices) {
        dst(i).data(j) = src(i).data(j)
      }
//      dst(i).mask = src(i).mask
      for (j <- dst(i).mask.indices) {
        dst(i).mask(j) = src(i).mask(j)
      }
      dst(i).lruCnt = src(i).lruCnt
    }
  }

  def getTag(paddr: Int): Int = paddr >> offsetWidth

  def hasEmpty: Boolean = {
    !cache.reduce((x, y) => x.valid & y.valid)
  }

  // get index of empty line which has the biggest index
  def nextEmpty(): Int = {
    var rev = -1
    for (i <- 0 until SbufferSize) {
      if (!cache(i).valid)
        rev = i
    }
    rev
  }

  // pretend to receive ready from d-cache
  var dCacheDelayCounter: Int = 0
  def setDelayCounter(): Unit = dCacheDelayCounter = dCacheDelay
  def decDelayCounter(): Unit = dCacheDelayCounter -= 1
  def dCacheReady: Boolean = dCacheDelayCounter == 0


  def pokeIn(pa: Array[Int], mask: Array[Boolean], data: Array[Int]) = {
    assert(pa.length == StorePipelineWidth && mask.length == StorePipelineWidth && data.length == StorePipelineWidth)

    for (queryIdx <- 0 until StorePipelineWidth)
      for (bufIdx <- 0 until SbufferSize) {
        // First, check if there exists a line for this pa
        if (getTag(pa(queryIdx)) == cache(bufIdx).tag) {
          isUpdate(bufIdx)
        }
      }
  }

  def nextCycle(): Unit = {
    decDelayCounter()
    // cache = cacheNext
    cacheCopy(cacheNext, cache)
  }
  /*
  Todo: implement in / d-cache / forward as functions
  Todo: compare cache line data to verify for now, so implement a getAllData function
   */
}

class SBufferDut(dispBegin: Int, dispEnd: Int) extends Module with HasMEMConst with HasSBufferConst {
  val io = IO(new Bundle() {
    val in = Vec(StorePipelineWidth, Flipped(Decoupled(new LsPipelineBundle)))
    val cacheLines = Vec(SbufferSize, Output(new SBufferCacheLine))
  })
  val sBuffer = Module(new Sbuffer)
  // in:
  io.in <> sBuffer.io.in

  // cacheLines contain data and mask and valid and tag in cache lines
  BoringUtils.bore(sBuffer.cache, Seq(io.cacheLines))

  AddSinks(dispBegin, dispEnd)
}

class SBufferTest extends FlatSpec
  with ChiselScalatestTester
  with Matchers
  with ParallelTestExecution
  with HasPartialDecoupledDriver
{
  it should "behave like its emulator" in {
    // TODO: 1. add a test case generator (only generate in for now)
    // TODO: 2. use a s-buffer instance to compare with emu
  }
}
