package xiangshan.backend.exu

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import bus.axi4.AXI4Delayer
import bus.simplebus.{SimpleBusCrossbarNto1, SimpleBusUC}
import device.AXI4RAM
import noop.{Cache, CacheConfig, MemMMUIO, TLB, TLBConfig}
import system.CoherenceManager
import utils.XSLog
import xiangshan._
import xiangshan.backend.fu.FunctionUnit.lsuCfg
import xiangshan.testutils._
import xiangshan.testutils.TestCaseGenerator._

import scala.util.Random

class LsuDut(dispBegin: Int, dispEnd: Int) extends Exu(Exu.lsuExeUnitCfg) {

  io.dmem <> DontCare

  val lsu = Module(new LsExeUnit)

  lsu.io.in <> io.in
  lsu.io.redirect <> io.redirect
  lsu.io.mcommit <> io.mcommit
  io.out <> lsu.io.out
  lsu.io.exception := DontCare

  val dmemXbar = Module(new SimpleBusCrossbarNto1(2))

  val memMMU = WireInit(0.U.asTypeOf(new MemMMUIO))

  val dtlb = TLB(
    in = lsu.io.dmem,
    mem = dmemXbar.io.in(1),
    flush = false.B,
    csrMMU = memMMU.dmem
  )(TLBConfig(name = "dtlb", totalEntry = 64))

  dmemXbar.io.in(0) <> dtlb.io.out

  val axi4Mem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, useBlackBox = false))
  val memdelay = Module(new AXI4Delayer(0))

  val mmio = WireInit(0.U.asTypeOf(new SimpleBusUC()))

  val cacheOut = Cache(
    in = dmemXbar.io.out,
    mmio = Seq(mmio),
    flush = "b00".U,
    empty = dtlb.io.cacheEmpty,
    enable = HasDcache
  )(CacheConfig(name = "dcache"))

  val cohMg = Module(new CoherenceManager)
  val xbar = Module(new SimpleBusCrossbarNto1(2))

  cohMg.io.in <> DontCare

  xbar.io.in(0) <> cohMg.io.out.mem
  xbar.io.in(1) <> cacheOut.mem

  cacheOut.coh <> cohMg.io.out.coh

  memdelay.io.in <> xbar.io.out.toAXI4()
  axi4Mem.io.in <> memdelay.io.out

  AddSinks(dispBegin, dispEnd)
}


class LsuTest
  extends FlatSpec
    with ChiselScalatestTester
    with Matchers
    with ParallelTestExecution
    with HasPartialDecoupledDriver {


  XSLog.generateLog = false

  def BASE_ADDR = 0x80000000L

  def USE_VERILATOR = false

  val annos = if(USE_VERILATOR) Seq(VerilatorBackendAnnotation) else Seq()

  it should "store and load correctly" in {


    test(new LsuDut(500, 550)).withAnnotations(annos) { c =>

      c.io.in.initSource().setSourceClock(c.clock)
      c.io.out.initSink().setSinkClock(c.clock)

      c.clock.step(500)

      def TEST_SIZE = 200

      val stDataSeq = (0 until TEST_SIZE).map(_ => Random.nextLong() >>> 1)

      val storeSeq = (0 until TEST_SIZE).map(i => {
        genLsuSd(c.io.in.bits, BASE_ADDR, 8*i, stDataSeq(i))
      })

      val loadSeq = (0 until TEST_SIZE).map(i =>{
        genLsuLd(c.io.in.bits, BASE_ADDR, 8*i)
      })

      c.io.pokePartial(chiselTypeOf(c.io).Lit(_.mcommit -> 1.U))

      fork{
        // enq stores
        c.io.in.enqueuePartialSeq(storeSeq)
        // load data form same addr
        c.io.in.enqueuePartialSeq(loadSeq)
      }.fork{
        // skip stores
        c.io.out.expectDequeuePartialSeq(
          (0 until TEST_SIZE).map(_ => chiselTypeOf(c.io.out.bits).Lit())
        )
        // expect load correct data
        c.io.out.expectDequeuePartialSeq(
          (0 until TEST_SIZE).map(i => chiselTypeOf(c.io.out.bits).Lit(_.data -> stDataSeq(i).U))
        )
      }.join()
    }
  }


}
