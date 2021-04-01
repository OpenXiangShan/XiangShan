package cache.L1DTest

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{LineCoverageAnnotation, ToggleCoverageAnnotation, VerilatorBackendAnnotation}
import chiseltest.legacy.backends.verilator.VerilatorFlags
import chiseltest._
import firrtl.stage.RunFirrtlTransformAnnotation
import chiseltest.ChiselScalatestTester
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, BufferParams}
import freechips.rocketchip.tilelink.{TLBuffer, TLCacheCork, TLToAXI4, TLXbar}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import utils.{DebugIdentityNode, HoldUnless, XSDebug}
import xiangshan.EnviromentParameters
import xiangshan.cache.{DCache, DCacheLineReq, DCacheToLsuIO, DCacheWordReq, MemoryOpConstants}
import xiangshan.testutils.AddSinks
import xstransforms.PrintModuleName

import scala.util.Random
import cache.TLCTest._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Seq}

class L1DTestTopIO extends Bundle {
  val slaveIO = new TLCTestSlaveMMIO()
  val dcacheIO = new DCacheToLsuIO
}

class L1DTestTop()(implicit p: Parameters) extends LazyModule {
  val dcache = LazyModule(new DCache())
  val dcache_outer = LazyModule(new DebugIdentityNode())
  val slave = LazyModule(new TLCSlaveMMIO())

  val c_buffer = TLBuffer(a = BufferParams.none, b = BufferParams.none, c = BufferParams.pipe, d = BufferParams.none, e = BufferParams.none)
  slave.node := dcache_outer.node := c_buffer := dcache.clientNode

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new L1DTestTopIO())
    dcache.module.io.lsu <> io.dcacheIO
    slave.module.io <> io.slaveIO
  }
}

class L1DTestTopWrapper()(implicit p: Parameters) extends LazyModule {
  val testTop = LazyModule(new L1DTestTop())
  lazy val module = new LazyModuleImp(this) {
    val io = IO(new L1DTestTopIO)
    AddSinks()
    io <> testTop.module.io
  }
}

class L1DCacheTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with TLCOp with RandomSampleUtil {
  top.Parameters.set(top.Parameters.debugParameters)

  it should "run" in {
    implicit val p = Parameters((site, up, here) => {
      case TLCCacheTestKey =>
        TLCCacheTestParams()
    })

    val rand = new Random(0xbeef)
    val addr_pool = {
      for (_ <- 0 until 256) yield BigInt(rand.nextInt(0xfffff) << 6) | 0x80000000L.U.litValue()
    }.distinct.toList // align to block size
    val addr_list_len = addr_pool.length
    println(f"addr pool length: $addr_list_len")
    val probeProbMap = Map(nothing -> 0.4, branch -> 0.5, trunk -> 0.1)

    def peekBigInt(source: Data): BigInt = {
      source.peek().litValue()
    }

    def peekBoolean(source: Bool): Boolean = {
      source.peek().litToBoolean
    }

    test(LazyModule(new L1DTestTopWrapper()).module)
      .withAnnotations(Seq(VerilatorBackendAnnotation,
        LineCoverageAnnotation,
        ToggleCoverageAnnotation,
        VerilatorFlags(Seq("--output-split 5000", "--output-split-cfuncs 5000",
          "+define+RANDOMIZE_REG_INIT", "+define+RANDOMIZE_MEM_INIT")),
        RunFirrtlTransformAnnotation(new PrintModuleName))) { c =>
        c.io.dcacheIO.load.foreach { l =>
          l.req.initSource().setSourceClock(c.clock)
          l.resp.initSink().setSinkClock(c.clock)
          l.req.valid.poke(false.B)
          l.resp.ready.poke(false.B)
        }
        val loadPortNum = c.io.dcacheIO.load.size

        c.io.dcacheIO.lsq.initSink().setSinkClock(c.clock)
        c.io.dcacheIO.store.req.initSource().setSourceClock(c.clock)
        c.io.dcacheIO.store.resp.initSink().setSinkClock(c.clock)
        c.io.dcacheIO.store.req.valid.poke(false.B)
        c.io.dcacheIO.store.resp.ready.poke(false.B)
        c.io.dcacheIO.atomics.req.initSource().setSourceClock(c.clock)
        c.io.dcacheIO.atomics.resp.initSink().setSinkClock(c.clock)
        c.io.dcacheIO.atomics.req.valid.poke(false.B)
        c.io.dcacheIO.atomics.resp.ready.poke(false.B)

        c.io.slaveIO.AChannel.initSink().setSinkClock(c.clock)
        c.io.slaveIO.CChannel.initSink().setSinkClock(c.clock)
        c.io.slaveIO.EChannel.initSink().setSinkClock(c.clock)
        c.io.slaveIO.BChannel.initSource().setSourceClock(c.clock)
        c.io.slaveIO.DChannel.initSource().setSourceClock(c.clock)
        c.io.slaveIO.AChannel.ready.poke(false.B)
        c.io.slaveIO.CChannel.ready.poke(false.B)
        c.io.slaveIO.EChannel.ready.poke(false.B)
        c.io.slaveIO.BChannel.valid.poke(false.B)
        c.io.slaveIO.DChannel.valid.poke(false.B)


        val total_clock = 50000

        c.reset.poke(true.B)
        c.clock.step(100)
        c.reset.poke(false.B)
        c.clock.setTimeout(8192)

        val slaveIO = c.io.slaveIO

        val scoreboard = mutable.Map[BigInt, ScoreboardData]()
        val serialList = ArrayBuffer[(Int, TLCTrans)]()

        val slaveState = mutable.Map[BigInt, AddrState]()
        val slaveAgent = new TLCSlaveAgent(2, name = "l3", 8, slaveState, serialList, scoreboard)

        val coreIO = c.io.dcacheIO
        val coreStateList = mutable.Map[BigInt, AddrState]()
        val coreAgent = new CoreAgent(ID = 0, name = "core", coreStateList, serialList, scoreboard)

        val sio = slaveIO
        for (cl <- 0 until total_clock) {
          //========= core trans ===========
          //randomly add when low size
          if (true) {
            if (coreAgent.outerLoad.size <= 4) {
              for (i <- 0 until 8) {
                val addr = getRandomElement(addr_pool, rand)
                coreAgent.addLoad(addr)
              }
            }
          }
          if (true) {
            if (coreAgent.outerStore.size <= 4) {
              for (i <- 0 until 8) {
                val addr = getRandomElement(addr_pool, rand)
                coreAgent.addStore(addr)
              }
            }
          }
          if (false) {
            if (coreAgent.outerAMO.size <= 0) {
              for (i <- 0 until 4) {
                val addr = getRandomElement(addr_pool, rand)
                coreAgent.addAMO(addr)
              }
            }
          }
          //========= core poke ============
          val loadPortsReqValid = ListBuffer.fill(loadPortNum)(false)
          val loadPortsRespReady = ListBuffer.fill(loadPortNum)(true)
          coreAgent.issueLoadReq()
          for (i <- 0 until loadPortNum) {
            val loadReq = coreAgent.peekLoadReq(i)
            if (loadReq.isDefined) {
              loadPortsReqValid(i) = true
              coreIO.load(i).req.bits.cmd.poke(loadReq.get.cmd.U)
              coreIO.load(i).req.bits.addr.poke(loadReq.get.addr.U)
              coreIO.load(i).req.bits.mask.poke(loadReq.get.mask.U)
            }
            coreIO.load(i).req.valid.poke(loadPortsReqValid(i).B)
            coreIO.load(i).resp.ready.poke(loadPortsRespReady(i).B)
            coreIO.load(i).s1_paddr.poke(coreAgent.s1Paddr(i).U)
            //TODO: random kill
            coreIO.load(i).s1_kill.poke(false.B)
          }
          var storePortReqValid = false
          val storePortRespReady = true
          coreAgent.issueStoreReq()
          val storeReq = coreAgent.peekStoreReq()
          if (storeReq.isDefined) {
            storePortReqValid = true
            coreIO.store.req.bits.cmd.poke(storeReq.get.cmd.U)
            coreIO.store.req.bits.addr.poke(storeReq.get.addr.U)
            coreIO.store.req.bits.data.poke(storeReq.get.data.U)
            coreIO.store.req.bits.mask.poke(storeReq.get.mask.U)
            coreIO.store.req.bits.id.poke(storeReq.get.id.U)
          }
          coreIO.store.req.valid.poke(storePortReqValid.B)
          coreIO.store.resp.ready.poke(storePortRespReady.B)

          var amoPortReqValid = false
          val amoPortRespReady = true
          coreAgent.issueAMOReq()
          val amoReq = coreAgent.peekAMOReq()
          if (amoReq.isDefined) {
            amoPortReqValid = true
            coreIO.atomics.req.bits.cmd.poke(amoReq.get.cmd.U)
            coreIO.atomics.req.bits.addr.poke(amoReq.get.addr.U)
            coreIO.atomics.req.bits.data.poke(amoReq.get.data.U)
            coreIO.atomics.req.bits.mask.poke(amoReq.get.mask.U)
            coreIO.atomics.req.bits.id.poke(amoReq.get.id.U)
          }
          coreIO.atomics.req.valid.poke(amoPortReqValid.B)
          coreIO.atomics.resp.ready.poke(amoPortRespReady.B)

          //========= slave ============
          //randomly add when empty
          if (slaveAgent.innerProbe.size <= 2) {
            if (true) {
              for (i <- 0 until 8) {
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

          //========== slave peek ============

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

          //============ core peek ============
          for (i <- 0 until loadPortNum) {
            val portReady = peekBoolean(coreIO.load(i).req.ready)
            if (loadPortsReqValid(i) && portReady) {
              coreAgent.fireLoadReq(i)
            }
            val respValid = peekBoolean(coreIO.load(i).resp.valid)
            if (loadPortsRespReady(i) && respValid) {
              val loadM = new LitDCacheWordResp(
                data = peekBigInt(coreIO.load(i).resp.bits.data),
                miss = peekBoolean(coreIO.load(i).resp.bits.miss),
                replay = peekBoolean(coreIO.load(i).resp.bits.replay),
              )
              coreAgent.fireLoadResp(i, loadM)
            }
          }

          val storePortReqReady = peekBoolean(coreIO.store.req.ready)
          if (storePortReqValid && storePortReqReady) {
            coreAgent.fireStoreReq()
          }
          val storePortRespValid = peekBoolean(coreIO.store.resp.valid)
          if (storePortRespValid && storePortRespReady) {
            val storeM = new LitDCacheLineResp(
              data = peekBigInt(coreIO.store.resp.bits.data),
              paddr = BigInt(0),
              id = peekBigInt(coreIO.store.resp.bits.id)
            )
            coreAgent.fireStoreResp(storeM)
          }

          val amoPortReqReady = peekBoolean(coreIO.atomics.req.ready)
          if (amoPortReqValid && amoPortReqReady) {
            coreAgent.fireAMOReq()
          }
          val amoPortRespValid = peekBoolean(coreIO.atomics.resp.valid)
          if (amoPortRespValid && amoPortRespReady) {
            val amoM = new LitDCacheWordResp(
              data = peekBigInt(coreIO.atomics.resp.bits.data),
              miss = false,
              replay = false,
              id = peekBigInt(coreIO.atomics.resp.bits.id)
            )
            coreAgent.fireAMOResp(amoM)
          }

          val lsqRespValid = peekBoolean(coreIO.lsq.valid)
          if (lsqRespValid) {
            val lsqM = new LitDCacheLineResp(
              data = peekBigInt(coreIO.lsq.bits.data),
              paddr = peekBigInt(coreIO.lsq.bits.addr),
              id = BigInt(0)
            )
            coreAgent.fireLsqResp(lsqM)
          }

          coreAgent.step()

          c.clock.step()
        }
        c.io.dcacheIO.load.foreach { l =>
          l.s1_kill.poke(true.B)
          l.req.valid.poke(false.B)
          l.resp.ready.poke(true.B)
        }
        c.io.dcacheIO.store.req.valid.poke(false.B)
        c.io.dcacheIO.store.resp.ready.poke(true.B)
        c.io.dcacheIO.atomics.req.valid.poke(false.B)
        c.io.dcacheIO.atomics.resp.ready.poke(true.B)
        c.clock.step(10)
      }
  }
}
