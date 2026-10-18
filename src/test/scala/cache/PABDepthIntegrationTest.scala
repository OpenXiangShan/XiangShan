package cache

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top._
import utility._
import xiangshan.{XSCoreParamsKey, XSTileKey, XSModule}
import xiangshan.cache._
import xiangshan.mem.prefetch._

class PABControlledStreamTop(implicit p: Parameters) extends XSModule {
  Constantin.init(false); ChiselDB.init(false)
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val address = Input(UInt(VAddrBits.W))
    val events = Input(new PABDepthEvents)
    val ready, enabled = Output(Bool())
    val depth, strideDepth = Output(UInt(12.W))
    val l1, l2, l3 = Output(Valid(new StreamPrefetchReqBundle))
  })
  val monitor = Module(new PrefetcherMonitor)
  monitor.io.loadinfo := 0.U.asTypeOf(monitor.io.loadinfo)
  monitor.io.maininfo := 0.U.asTypeOf(monitor.io.maininfo)
  monitor.io.missinfo := 0.U.asTypeOf(monitor.io.missinfo)
  monitor.io.replinfo := 0.U.asTypeOf(monitor.io.replinfo)
  monitor.io.clear_flag := 0.U.asTypeOf(monitor.io.clear_flag)
  monitor.io.debugRolling := 0.U.asTypeOf(monitor.io.debugRolling)
  monitor.io.pabinfo := io.events
  val stream = Module(new StreamBitVectorArray)
  stream.io.enable := monitor.io.pf_ctrl(0).enable
  stream.io.flush := monitor.io.pf_ctrl(0).flush
  stream.io.confidence := monitor.io.pf_ctrl(0).confidence
  stream.io.dynamic_depth := monitor.io.pf_ctrl(0).dynamic_depth
  stream.io.train_req.valid := io.valid
  stream.io.train_req.bits := 0.U.asTypeOf(stream.io.train_req.bits)
  stream.io.train_req.bits.vaddr := io.address
  stream.io.train_req.bits.miss := true.B
  stream.io.stream_lookup_req.valid := false.B; stream.io.stream_lookup_req.bits := 0.U
  io.ready := stream.io.train_req.ready
  io.enabled := monitor.io.pf_ctrl(0).enable
  io.depth := monitor.io.pf_ctrl(0).dynamic_depth
  io.strideDepth := monitor.io.pf_ctrl(1).dynamic_depth
  io.l1 := stream.io.l1_prefetch_req; io.l2 := stream.io.l2_prefetch_req; io.l3 := stream.io.l3_prefetch_req
}

class PABFeedbackTop extends Module {
  val io = IO(new Bundle {
    val refill = Input(Valid(new PABLine(20)))
    val demand = Input(Vec(3, Valid(UInt(20.W))))
    val late = Input(UInt(4.W))
    val depth = Output(UInt(12.W))
    val window = Output(Valid(new PABDepthWindow))
  })
  val pab = Module(new PrefetchAddrBuffer(2, 20))
  pab.io.refill := io.refill; pab.io.demand := io.demand
  pab.io.retire := 0.U.asTypeOf(pab.io.retire)
  val victims = Module(new PABVictimTable(8, 20))
  victims.io.victim := pab.io.victim; victims.io.demand := pab.io.observedDemand
  val control = Module(new PABDepthController(PABDepthParameters(windowInserts = 3,
    lateLow = 0, lateHigh = 1, unusedLow = 0, unusedHigh = 1, settleWindows = 0)))
  control.io.enabled := true.B; control.io.fixedDepth := 64.U
  control.io.events.insert := ShiftRegister(pab.io.inserted.valid && pab.io.inserted.bits.source === 3.U,
    2, false.B, true.B)
  control.io.events.late := ShiftRegister(io.late, 3, 0.U(4.W), true.B)
  control.io.events.unused := victims.io.streamUnusedHits
  io.depth := control.io.depth; io.window := control.io.window
}

class PABDepthIntegrationTest extends AnyFlatSpec with ChiselSim {
  private def parameters(config: Parameters): Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
  }
  behavior of "PAB feedback integration"
  it should "align closing-edge reuse and retain FIFO history across control windows" in {
    simulate(new PABFeedbackTop) { c =>
      c.io.refill.valid.poke(false.B); c.io.refill.bits.blockAddr.poke(0.U)
      c.io.refill.bits.source.poke(3.U); c.io.refill.bits.used.poke(false.B)
      c.io.demand.foreach { d => d.valid.poke(false.B); d.bits.poke(0.U) }
      c.io.late.poke(0.U); c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
      var windows = Vector.empty[(Int, Int, Int)]
      def tick(): Unit = {
        if (c.io.window.valid.peek().litToBoolean) {
          windows :+= ((c.io.window.bits.depthBefore.peek().litValue.toInt,
            c.io.window.bits.depthAfter.peek().litValue.toInt, c.io.window.bits.unused.peek().litValue.toInt))
        }
        c.clock.step()
      }
      def idle(n: Int = 5): Unit = { c.io.refill.valid.poke(false.B); for (_ <- 0 until n) tick() }
      def fill(a: Int, source: Int = 3): Unit = {
        c.io.refill.valid.poke(true.B); c.io.refill.bits.blockAddr.poke(a.U)
        c.io.refill.bits.source.poke(source.U); tick(); idle()
      }
      fill(100); fill(200, 2); fill(300)
      c.io.demand.foreach { d => d.valid.poke(true.B); d.bits.poke(100.U) }
      c.io.refill.valid.poke(true.B); c.io.refill.bits.blockAddr.poke(400.U); tick()
      c.io.demand.foreach(_.valid.poke(false.B)); idle()
      assert(windows == Vector((16, 8, 1))); c.io.depth.expect(8.U)
      fill(500) // Leaves Stream300 in the FIFO after the first control window.
      idle(600)
      c.io.demand(0).valid.poke(true.B); c.io.demand(0).bits.poke(300.U); tick()
      c.io.demand(0).valid.poke(false.B); idle()
      fill(600); fill(700)
      assert(windows == Vector((16, 8, 1), (8, 4, 1)))
    }
  }
  for ((name, config) <- Seq("dynamic" -> new DefaultConfig,
    "fixed" -> new PABFixedDepthConfig,
    "disabled" -> (new WithPABDepth(PABDepthParameters()) ++ new DefaultConfig))) {
    it should s"generate real $name Stream addresses while preserving outer distances and Stride control" in {
      implicit val p: Parameters = parameters(config)
      simulate(new PABControlledStreamTop) { c =>
        c.io.valid.poke(false.B); c.io.address.poke(0.U)
        c.io.events.insert.poke(false.B); c.io.events.late.poke(0.U); c.io.events.unused.poke(0.U)
        c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
        def window(up: Boolean): Unit = {
          var count = if (up) 40 else 80
          while (count > 0) {
            val n = count.min(if (up) 15 else 3)
            if (up) c.io.events.late.poke(n.U) else c.io.events.unused.poke(n.U)
            c.clock.step(); count -= n
          }
          c.io.events.late.poke(0.U); c.io.events.unused.poke(0.U)
          c.io.events.insert.poke(true.B); c.clock.step(1000)
          c.io.events.insert.poke(false.B); c.clock.step(3)
        }
        if (name == "dynamic") { window(false); window(false); c.io.depth.expect(4.U) }
        var block = BigInt("80000", 16)
        for (depth <- (if (name == "dynamic") PABDepthPolicy.levels else Seq(64))) {
          if (name == "dynamic" && depth != 4) window(true)
          c.io.strideDepth.expect(32.U); c.io.enabled.expect(true.B)
          if (name != "disabled") c.io.depth.expect(depth.U)
          var observed = 0
          def check(): Unit = {
            for ((req, distance) <- Seq((c.io.l1, depth), (c.io.l2, 640), (c.io.l3, 960))) {
              if (req.valid.peek().litToBoolean) {
                val trigger = req.bits.trigger_va.peek().litValue >> 6
                val bits = req.bits.bit_vec.peek().litValue
                val first = (req.bits.region.peek().litValue << 4) + bits.lowestSetBit
                assert(first == trigger + distance)
                if (distance == depth) observed += 1
              }
            }
          }
          for (_ <- 0 until 48) {
            c.io.valid.poke(true.B); c.io.address.poke((block << 6).U)
            var wait = 0
            while (!c.io.ready.peek().litToBoolean && wait < 10) { check(); c.clock.step(); wait += 1 }
            c.io.ready.expect(true.B); check(); c.clock.step(); block += 1
          }
          c.io.valid.poke(false.B); for (_ <- 0 until 5) { check(); c.clock.step() }
          assert(observed > 0)
        }
      }
    }
  }
}
