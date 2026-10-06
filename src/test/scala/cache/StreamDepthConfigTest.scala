package cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.{DefaultConfig, WithStreamDepth}
import utility._
import xiangshan.{XSCoreParamsKey, XSTileKey, XSModule}
import xiangshan.mem.prefetch._

class StreamDepthTestTop(implicit p: Parameters) extends XSModule {
  Constantin.init(false)
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val address = Input(UInt(VAddrBits.W))
    val depth = Input(UInt(12.W))
    val ready = Output(Bool())
    val l1, l2, l3 = Output(chisel3.util.Valid(new StreamPrefetchReqBundle))
  })
  val stream = Module(new StreamBitVectorArray)
  stream.io.enable := true.B
  stream.io.flush := false.B
  stream.io.dynamic_depth := io.depth
  stream.io.confidence := 1.U
  stream.io.train_req.valid := io.valid
  stream.io.train_req.bits := 0.U.asTypeOf(stream.io.train_req.bits)
  stream.io.train_req.bits.vaddr := io.address
  stream.io.train_req.bits.miss := true.B
  stream.io.stream_lookup_req.valid := false.B
  stream.io.stream_lookup_req.bits := 0.U
  io.ready := stream.io.train_req.ready
  io.l1 := stream.io.l1_prefetch_req
  io.l2 := stream.io.l2_prefetch_req
  io.l3 := stream.io.l3_prefetch_req
}

class StreamMonitorDepthTestTop(implicit p: Parameters) extends XSModule {
  Constantin.init(false)
  val io = IO(new Bundle {
    val event = Input(Bool())
    val depth = Output(UInt(12.W))
    val enabled = Output(Bool())
  })
  val monitor = Module(new L1PrefetchMonitor(new StreamMonitorParam {
    override val TIMELY_CHECK_INTERVAL = 4
    override val LATE_MISS_THRESHOLD = 2
  }))
  monitor.io.prefetch_info := 0.U.asTypeOf(monitor.io.prefetch_info)
  monitor.io.prefetch_info.loadinfo(0).total_prefetch := io.event
  monitor.io.prefetch_info.loadinfo(0).pf_source := 3.U
  monitor.io.prefetch_info.missinfo.pf_late_in_mshr := io.event
  monitor.io.prefetch_info.missinfo.pf_source := 3.U
  io.depth := monitor.io.pf_ctrl.dynamic_depth
  io.enabled := monitor.io.pf_ctrl.enable
}

class StreamDepthConfigTest extends AnyFlatSpec with ChiselSim {
  private def parameters(depth: StreamDepthParameters): Parameters = {
    val base = new WithStreamDepth(depth) ++ new DefaultConfig
    base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head
      case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
      case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
    }
  }

  behavior of "Stream depth source selection"
  for (fixed <- Seq(4, 16, 64); monitor <- Seq(false, true)) {
    it should s"generate addresses with fixed=$fixed monitor=$monitor and keep outer levels fixed" in {
      implicit val p: Parameters = parameters(StreamDepthParameters(useMonitor = monitor, fixedL1 = fixed))
      simulate(new StreamDepthTestTop) { c =>
        c.io.valid.poke(false.B); c.io.address.poke(0.U); c.io.depth.poke(16.U)
        c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
        var block = BigInt("80000", 16)
        for (dynamic <- Seq(4, 8, 16, 24, 32, 48, 64)) {
          c.io.depth.poke(dynamic.U)
          var observed = 0
          def check(): Unit = {
            for ((req, offset) <- Seq((c.io.l1, if (monitor) dynamic else fixed),
              (c.io.l2, 640), (c.io.l3, 960))) {
              if (req.valid.peek().litToBoolean) {
                val trigger = req.bits.trigger_va.peek().litValue >> 6
                val bits = req.bits.bit_vec.peek().litValue
                val firstBlock = (req.bits.region.peek().litValue << 4) + bits.lowestSetBit
                assert(firstBlock == trigger + offset,
                  s"expected ${trigger + offset}, got $firstBlock")
                if (offset == (if (monitor) dynamic else fixed)) observed += 1
              }
            }
          }
          for (_ <- 0 until 48) {
            c.io.address.poke((block << 6).U); c.io.valid.poke(true.B)
            var wait = 0
            while (!c.io.ready.peek().litToBoolean && wait < 10) {
              check(); c.clock.step(); wait += 1
            }
            c.io.ready.expect(true.B)
            check(); c.clock.step(); block += 1
          }
          c.io.valid.poke(false.B)
          for (_ <- 0 until 5) { check(); c.clock.step() }
          assert(observed > 0, "test must observe real stream prefetch requests")
        }
      }
    }
  }

  for (legacy <- Seq(false, true)) {
    it should s"make legacy monitor control explicit without enabling automatic shutoff (legacy=$legacy)" in {
      implicit val p: Parameters = parameters(StreamDepthParameters(useMonitor = true, enableLegacyControl = legacy))
      simulate(new StreamMonitorDepthTestTop) { c =>
        c.io.event.poke(false.B)
        c.reset.poke(true.B); c.clock.step(2); c.reset.poke(false.B); c.clock.step()
        c.io.depth.expect(16.U)
        c.io.event.poke(true.B); c.clock.step(4)
        c.io.event.poke(false.B); c.clock.step()
        c.io.depth.expect((if (legacy) 32 else 16).U)
        c.io.enabled.expect(true.B)
      }
    }
  }
}
