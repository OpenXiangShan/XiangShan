package cache.TLCTest

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{LineCoverageAnnotation, ToggleCoverageAnnotation, VerilatorBackendAnnotation}
import chiseltest.legacy.backends.verilator.VerilatorFlags

import chiseltest._
import chiseltest.ChiselScalatestTester
import firrtl.stage.RunFirrtlTransformAnnotation
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBuffer, TLDelayer, TLXbar}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import utils.{DebugIdentityNode, XSDebug}
import xiangshan.testutils.AddSinks
import xstransforms.PrintModuleName

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Seq}
import scala.util.Random
import scala.collection.mutable.ListBuffer

case class TLCCacheTestParams
(
  ways: Int = 4,
  banks: Int = 1,
  capacityKB: Int = 4,
  blockBytes: Int = 64,
  beatBytes: Int = 32
) {
  require(blockBytes >= beatBytes)
}

case object TLCCacheTestKey extends Field[TLCCacheTestParams]

class TLCCacheTestTopIO(implicit p: Parameters) extends Bundle {
  val mastersIO = Vec(2, new TLCTestMasterMMIO())
  val ulIO = new TLULMMIO()
  val slaveIO = new TLCTestSlaveMMIO()
  val fuzzerBlockAddr = Input(UInt(64.W))
}

class TLCCacheTestTop()(implicit p: Parameters) extends LazyModule {

  val masters = Array.fill(2)(LazyModule(new TLCMasterMMIO()))
  val ULmaster = LazyModule(new TLCSnoopMMIONode())

  val l2params = p(TLCCacheTestKey)

  val l2 = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 2,
      ways = l2params.ways,
      sets = l2params.capacityKB * 1024 / (l2params.blockBytes * l2params.ways * l2params.banks),
      blockBytes = l2params.blockBytes,
      beatBytes = l2params.beatBytes,
      debug = true,
      verification = true
    ),
    InclusiveCacheMicroParameters(
      writeBytes = l2params.beatBytes
    )
  ))
  val fuzz = LazyModule(new FixedBlockFuzzer(0))

  val l1_idents = Array.fill(2)(LazyModule(new DebugIdentityNode()))
  val l1_ul_ident = LazyModule(new DebugIdentityNode())
  val l1_xbar_ident = LazyModule(new DebugIdentityNode())
  val l2_inner_ident = LazyModule(new DebugIdentityNode())
  val l2_outer_ident = LazyModule(new DebugIdentityNode())
  val l3_ident = LazyModule(new DebugIdentityNode())

  val xbar = TLXbar()

  xbar := l1_ul_ident.node := ULmaster.node := fuzz.node

  for ((master, ident) <- (masters zip l1_idents)) {
    xbar := ident.node := master.node
  }
  l2.node := l2_inner_ident.node := TLBuffer() := l1_xbar_ident.node := xbar

  val slave = LazyModule(new TLCSlaveMMIO())
  slave.node := l3_ident.node := TLBuffer() := l2_outer_ident.node := l2.node

  lazy val module = new LazyModuleImp(this) {

    val io = IO(new TLCCacheTestTopIO)

    fuzz.module.io.blockAddr := io.fuzzerBlockAddr
    slave.module.io <> io.slaveIO
    io.ulIO <> ULmaster.module.io
    masters zip io.mastersIO map { case (m, i) =>
      m.module.io <> i
    }
  }
}

class TLCCacheTestTopWrapper()(implicit p: Parameters) extends LazyModule {

  val testTop = LazyModule(new TLCCacheTestTop())

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new TLCCacheTestTopIO)
    AddSinks()
    io <> testTop.module.io
  }
}

trait RandomSampleUtil {
  def getRandomElement[A](l: List[A], r: scala.util.Random): A = {
    l(r.nextInt(l.length))
  }

  final def sample[A](dist: Map[A, Double], r: scala.util.Random): A = {
    val p = r.nextDouble
    val it = dist.iterator
    var accum = 0.0
    while (it.hasNext) {
      val (item, itemProb) = it.next
      accum += itemProb
      if (accum >= p)
        return item // return so that we don't have to search through the whole distribution
    }
    sys.error(f"this should never happen") // needed so it will compile
  }
}

class TLCCacheTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with TLCOp with RandomSampleUtil {
  val slave_safe = 0
  val slave_granting = 1
  val slave_probing = 2

  it should "run" in {

    implicit val p = Parameters((site, up, here) => {
      case TLCCacheTestKey =>
        TLCCacheTestParams()
    })

    val rand = new Random(0xbeef)

    val addr_pool = {
      for (_ <- 0 to 128) yield BigInt(rand.nextInt(0xffff) << 6)
    }.distinct.toList // align to block size
    val ul_addr_pool = {
      {
        for (_ <- 0 to 64) yield BigInt(rand.nextInt(0xffff) << 6)
      }.toList ++ addr_pool
    }.distinct
    val addr_list_len = addr_pool.length
    val acquireProbMap = Map(branch -> 0.3, trunk -> 0.7)
    val releaseProbMap = Map(nothing -> 0.6, branch -> 0.3, trunk -> 0.1)
    val probeProbMap = Map(nothing -> 0.5, branch -> 0.4, trunk -> 0.1)

    def peekBigInt(source: Data): BigInt = {
      source.peek().litValue()
    }

    def peekBoolean(source: Bool): Boolean = {
      source.peek().litToBoolean
    }

    test(LazyModule(new TLCCacheTestTopWrapper()).module)
      .withAnnotations(Seq(VerilatorBackendAnnotation,
        LineCoverageAnnotation,
        ToggleCoverageAnnotation,
        VerilatorFlags(Seq("--output-split 5000", "--output-split-cfuncs 5000")),
        RunFirrtlTransformAnnotation(new PrintModuleName))) { c =>
        c.io.mastersIO.foreach { mio =>
          mio.AChannel.initSource().setSourceClock(c.clock)
          mio.CChannel.initSource().setSourceClock(c.clock)
          mio.EChannel.initSource().setSourceClock(c.clock)
          mio.BChannel.initSink().setSinkClock(c.clock)
          mio.DChannel.initSink().setSinkClock(c.clock)
        }
        c.io.slaveIO.AChannel.initSink().setSinkClock(c.clock)
        c.io.slaveIO.CChannel.initSink().setSinkClock(c.clock)
        c.io.slaveIO.EChannel.initSink().setSinkClock(c.clock)
        c.io.slaveIO.BChannel.initSource().setSourceClock(c.clock)
        c.io.slaveIO.DChannel.initSource().setSourceClock(c.clock)

        val total_clock = 150000

        c.io.ulIO.isOn.poke(false.B)
        c.reset.poke(true.B)
        c.clock.step(1000)
        c.reset.poke(false.B)
        c.clock.step(100)
        c.io.ulIO.isOn.poke(true.B)

        c.clock.setTimeout(200)

        val mastersIO = c.io.mastersIO
        val slaveIO = c.io.slaveIO
        val ulIO = c.io.ulIO

        val scoreboard = mutable.Map[BigInt, ScoreboardData]()
        val serialList = ArrayBuffer[(Int, TLCTrans)]()
        val masterStateList = List.fill(2)(mutable.Map[BigInt, AddrState]())
        val masterAgentList = List.tabulate(2)(i => new TLCMasterAgent(i, f"l1_$i", 8, masterStateList(i)
          , serialList, scoreboard))

        val tlState = mutable.Map[BigInt, AddrState]()
        val ulAgent = new TLULMasterAgent(3, "l1_UL", tlState, serialList, scoreboard)

        val slaveState = mutable.Map() ++ {
          addr_pool zip List.fill(addr_list_len)(new AddrState())
        }.toMap
        val slaveAgent = new TLCSlaveAgent(2, name = "l3", 16, slaveState, serialList, scoreboard)
        //must set order here
        fork {
          for (_ <- 0 to total_clock) {
            val ulio = ulIO
            val ulBlockAddr = getRandomElement(ul_addr_pool, rand)
            c.io.fuzzerBlockAddr.poke(ulBlockAddr.U)

            val AChannel_valids = ArrayBuffer.fill(2)(false)
            val CChannel_valids = ArrayBuffer.fill(2)(false)
            val EChannel_valids = ArrayBuffer.fill(2)(false)
            val BChannel_readys = ArrayBuffer.fill(2)(true)
            val DChannel_readys = ArrayBuffer.fill(2)(true)

            for (i <- 0 to 1) {
              val mio = mastersIO(i)
              val masterAgent = masterAgentList(i)

              //randomly add when size is small
              if (masterAgent.outerAcquire.size <= 6) {
                if (true) {
                  for (i <- 0 until 16) {
                    val addr = getRandomElement(addr_pool, rand)
                    val targetPerm = sample(acquireProbMap, rand)
                    masterAgent.addAcquire(addr, targetPerm)
                  }
                }
              }
              if (masterAgent.outerRelease.size <= 6) {
                if (true) {
                  for (i <- 0 until 16) {
                    val addr = getRandomElement(addr_pool, rand)
                    val targetPerm = sample(releaseProbMap, rand)
                    masterAgent.addRelease(addr, targetPerm)
                  }
                }
              }

              //E channel
              val tmpE = masterAgent.peekE()
              if (tmpE.isDefined) {
                EChannel_valids(i) = true
                mio.EChannel.bits.sink.poke(tmpE.get.sink.U)
              }
              mio.EChannel.valid.poke(EChannel_valids(i).B)
              //D channel
              mio.DChannel.ready.poke(DChannel_readys(i).B)
              //C channel
              masterAgent.issueC()
              val tmpC = masterAgent.peekC()
              if (tmpC.isDefined) {
                CChannel_valids(i) = true
                mio.CChannel.bits.opcode.poke(tmpC.get.opcode.U)
                mio.CChannel.bits.param.poke(tmpC.get.param.U)
                mio.CChannel.bits.size.poke(tmpC.get.size.U)
                mio.CChannel.bits.source.poke(tmpC.get.source.U)
                mio.CChannel.bits.address.poke(tmpC.get.address.U)
                mio.CChannel.bits.data.poke(tmpC.get.data.U)
              }
              mio.CChannel.valid.poke(CChannel_valids(i).B)
              //B channel
              mio.BChannel.ready.poke(BChannel_readys(i).B)
              //A channel
              masterAgent.issueA()
              val tmpA = masterAgent.peekA()
              if (tmpA.isDefined) {
                AChannel_valids(i) = true
                mio.AChannel.bits.opcode.poke(tmpA.get.opcode.U)
                mio.AChannel.bits.param.poke(tmpA.get.param.U)
                mio.AChannel.bits.size.poke(tmpA.get.size.U)
                mio.AChannel.bits.source.poke(tmpA.get.source.U)
                mio.AChannel.bits.address.poke(tmpA.get.address.U)
                mio.AChannel.bits.mask.poke(tmpA.get.mask.U)
                mio.AChannel.bits.data.poke(tmpA.get.data.U)
              }
              mio.AChannel.valid.poke(AChannel_valids(i).B)
            }

            for (i <- 0 to 1) {
              val mio = mastersIO(i)
              val masterAgent = masterAgentList(i)
              //E channel
              val EChannel_ready = peekBoolean(mio.EChannel.ready)
              if (EChannel_valids(i) && EChannel_ready) {
                masterAgent.fireE()
              }
              //D channel
              val DChannel_valid = peekBoolean(mio.DChannel.valid)
              if (DChannel_valid && DChannel_readys(i)) { //fire
                val dCh = new TLCScalaD()
                dCh.opcode = peekBigInt(mio.DChannel.bits.opcode)
                dCh.param = peekBigInt(mio.DChannel.bits.param)
                dCh.size = peekBigInt(mio.DChannel.bits.size)
                dCh.source = peekBigInt(mio.DChannel.bits.source)
                dCh.sink = peekBigInt(mio.DChannel.bits.sink)
                dCh.denied = peekBoolean(mio.DChannel.bits.denied)
                dCh.data = peekBigInt(mio.DChannel.bits.data)
                masterAgent.fireD(dCh)
              }
              //C channel
              val CChannel_ready = peekBoolean(mio.CChannel.ready)
              if (CChannel_valids(i) && CChannel_ready) {
                masterAgent.fireC()
              }
              //B channel
              val BChannel_valid = peekBoolean(mio.BChannel.valid)
              if (BChannel_valid && BChannel_readys(i)) { //fire
                val bCh = new TLCScalaB()
                bCh.opcode = peekBigInt(mio.BChannel.bits.opcode)
                bCh.param = peekBigInt(mio.BChannel.bits.param)
                bCh.size = peekBigInt(mio.BChannel.bits.size)
                bCh.source = peekBigInt(mio.BChannel.bits.source)
                bCh.address = peekBigInt(mio.BChannel.bits.address)
                bCh.mask = peekBigInt(mio.BChannel.bits.mask)
                bCh.data = peekBigInt(mio.BChannel.bits.data)
                masterAgent.fireB(bCh)
              }
              masterAgent.tickB()
              //A channel
              val AChannel_ready = peekBoolean(mio.AChannel.ready)
              if (AChannel_valids(i) && AChannel_ready) {
                masterAgent.fireA()
              }
              masterAgent.step()
            }

            if (peekBoolean(ulio.DFire)) {
              val dCh = new TLCScalaD()
              dCh.opcode = peekBigInt(ulio.DChannel.opcode)
              dCh.param = peekBigInt(ulio.DChannel.param)
              dCh.size = peekBigInt(ulio.DChannel.size)
              dCh.source = peekBigInt(ulio.DChannel.source)
              dCh.sink = peekBigInt(ulio.DChannel.sink)
              dCh.denied = peekBoolean(ulio.DChannel.denied)
              dCh.data = peekBigInt(ulio.DChannel.data)
              ulAgent.fireD(dCh)
            }
            if (peekBoolean(ulio.AFire)) {
              val aCh = new TLCScalaA()
              aCh.opcode = peekBigInt(ulio.AChannel.opcode)
              aCh.param = peekBigInt(ulio.AChannel.param)
              aCh.size = peekBigInt(ulio.AChannel.size)
              aCh.source = peekBigInt(ulio.AChannel.source)
              aCh.address = peekBigInt(ulio.AChannel.address)
              aCh.mask = peekBigInt(ulio.AChannel.mask)
              aCh.data = peekBigInt(ulio.AChannel.data)
              ulAgent.fireA(aCh)
            }
            ulAgent.step()

            c.clock.step()
          }
        }

        fork {
          val sio = slaveIO
          for (_ <- 0 to total_clock) {
            //randomly add when empty
            if (slaveAgent.innerProbe.size <= 6) {
              if (true) {
                for (i <- 0 until 16) {
                  val addr = getRandomElement(addr_pool, rand)
                  val targetPerm = sample(probeProbMap, rand)
                  slaveAgent.addProbe(addr, targetPerm)
                }
              }
            }

            val AChannel_ready = true
            val CChannel_ready = true
            val EChannel_ready = true
            var BChannel_valid = false
            var DChannel_valid = false

            //E channel
            sio.EChannel.ready.poke(EChannel_ready.B)
            //D channel
            slaveAgent.issueD()
            val tmpD = slaveAgent.peekD()
            if (tmpD.isDefined) {
              DChannel_valid = true
              sio.DChannel.bits.opcode.poke(tmpD.get.opcode.U)
              sio.DChannel.bits.param.poke(tmpD.get.param.U)
              sio.DChannel.bits.size.poke(tmpD.get.size.U)
              sio.DChannel.bits.source.poke(tmpD.get.source.U)
              sio.DChannel.bits.sink.poke(tmpD.get.sink.U)
              sio.DChannel.bits.denied.poke(tmpD.get.denied.B)
              sio.DChannel.bits.data.poke(tmpD.get.data.U)
            }
            sio.DChannel.valid.poke(DChannel_valid.B)
            //C channel
            sio.CChannel.ready.poke(CChannel_ready.B)
            //B channel
            slaveAgent.issueB()
            val tmpB = slaveAgent.peekB()
            if (tmpB.isDefined) {
              BChannel_valid = true
              sio.BChannel.bits.opcode.poke(tmpB.get.opcode.U)
              sio.BChannel.bits.param.poke(tmpB.get.param.U)
              sio.BChannel.bits.size.poke(tmpB.get.size.U)
              sio.BChannel.bits.source.poke(tmpB.get.source.U)
              sio.BChannel.bits.address.poke(tmpB.get.address.U)
              sio.BChannel.bits.mask.poke(tmpB.get.mask.U)
              sio.BChannel.bits.data.poke(tmpB.get.data.U)
            }
            sio.BChannel.valid.poke(BChannel_valid.B)
            //A channel
            sio.AChannel.ready.poke(AChannel_ready.B)


            //E channel
            val EChannel_valid = peekBoolean(sio.EChannel.valid)
            if (EChannel_valid && EChannel_ready) {
              val eCh = new TLCScalaE()
              eCh.sink = peekBigInt(sio.EChannel.bits.sink)
              slaveAgent.fireE(eCh)
            }
            //D channel
            val DChannel_ready = peekBoolean(sio.DChannel.ready)
            if (DChannel_valid && DChannel_ready) { //fire
              slaveAgent.fireD()
            }

            //C channel
            val CChannel_valid = peekBoolean(sio.CChannel.valid)
            if (CChannel_valid && CChannel_ready) { //fire
              val cCh = new TLCScalaC()
              cCh.opcode = peekBigInt(sio.CChannel.bits.opcode)
              cCh.param = peekBigInt(sio.CChannel.bits.param)
              cCh.size = peekBigInt(sio.CChannel.bits.size)
              cCh.source = peekBigInt(sio.CChannel.bits.source)
              cCh.address = peekBigInt(sio.CChannel.bits.address)
              cCh.data = peekBigInt(sio.CChannel.bits.data)
              slaveAgent.fireC(cCh)
            }
            slaveAgent.tickC()
            //B channel
            val BChannel_ready = peekBoolean(sio.BChannel.ready)
            if (BChannel_valid && BChannel_ready) {
              slaveAgent.fireB()
            }
            //A channel
            val AChannel_valid = peekBoolean(sio.AChannel.valid)
            if (AChannel_valid && AChannel_ready) { //fire
              val aCh = new TLCScalaA()
              aCh.opcode = peekBigInt(sio.AChannel.bits.opcode)
              aCh.param = peekBigInt(sio.AChannel.bits.param)
              aCh.size = peekBigInt(sio.AChannel.bits.size)
              aCh.source = peekBigInt(sio.AChannel.bits.source)
              aCh.address = peekBigInt(sio.AChannel.bits.address)
              aCh.mask = peekBigInt(sio.AChannel.bits.mask)
              aCh.data = peekBigInt(sio.AChannel.bits.data)
              slaveAgent.fireA(aCh)
            }
            slaveAgent.tickA()

            slaveAgent.step()
            c.clock.step()
          }
        }.join()
      }
  }

}
