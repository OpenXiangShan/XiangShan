package prefetch

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility.{Constantin, LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey}
import xiangshan._
import xiangshan.mem.prefetch._

class StrideLearningOnlyHarness(implicit p: Parameters) extends XSModule with HasStridePrefetchHelper {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val pc = Input(UInt(VAddrBits.W))
    val address = Input(UInt(VAddrBits.W))
    val source = Input(UInt(2.W)) // ordinary hit, demand miss, stride-prefetch hit
    val ready = Output(Bool())
    val l1 = Output(Bool())
    val l2 = Output(Bool())
  })
  val dut = Module(new StrideMetaArray)
  dut.io.enable := true.B
  dut.io.fdbkDegree := 1.U
  dut.io.flush := false.B
  dut.io.dynamic_depth := 0.U
  dut.io.confidence := 1.U
  dut.io.stream_lookup_resp := false.B
  dut.io.train_req.valid := io.valid
  dut.io.train_req.bits := 0.U.asTypeOf(dut.io.train_req.bits)
  dut.io.train_req.bits.pc := io.pc
  dut.io.train_req.bits.vaddr := io.address
  dut.io.train_req.bits.miss := io.source === 1.U
  dut.io.train_req.bits.metaSource := Mux(io.source === 2.U, L1_HW_PREFETCH_STRIDE, L1_HW_PREFETCH_NULL)
  dut.io.train_req.bits.isFirstIssue := true.B
  io.ready := dut.io.train_req.ready
  io.l1 := dut.io.l1_prefetch_req.valid
  io.l2 := dut.io.l2_prefetch_req.valid
}

class StrideLearningOnlyTest extends AnyFlatSpec with ChiselSim {
  behavior of "stride learning-only score defaults"
  it should "learn hits without issuing, preserve trained PCs, and issue for both original sources" in {
    val config = new DefaultConfig
    implicit val p: Parameters = config.alterPartial {
      case XSCoreParamsKey => config(XSTileKey).head.copy()
      case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = true)
      case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false,
        perfLevel = utility.XSPerfLevel.VERBOSE, perfDBHartID = 0)
    }
    Constantin.init(false)
    try {
      simulate(new StrideLearningOnlyHarness) { c =>
        c.io.valid.poke(false.B)
        c.io.pc.poke(0.U)
        c.io.address.poke(0.U)
        c.io.source.poke(0.U)
        c.clock.step(3)
        def train(pc: Int, address: Int, source: Int): (Int, Int) = {
          c.io.pc.poke(pc.U)
          c.io.address.poke(address.U)
          c.io.source.poke(source.U)
          c.io.valid.poke(true.B)
          c.io.ready.expect(true.B)
          c.clock.step()
          c.io.valid.poke(false.B)
          var l1 = 0
          var l2 = 0
          for (_ <- 0 until 6) {
            l1 += c.io.l1.peek().litValue.toInt
            l2 += c.io.l2.peek().litValue.toInt
            c.clock.step()
          }
          (l1, l2)
        }
        train(0x1000, 0x100000, 1)
        for (i <- 1 to 12) {
          assert(train(0x1000, 0x100000 + i * 128, 0) == (0, 0), "ordinary hit issued a prefetch")
        }
        assert(train(0x1000, 0x100000 + 13 * 128, 1) == (1, 1), "hit learning did not train both levels")
        for (i <- 1 to 32) train(0x2000 + i * 4, 0x180000 + i * 128, 0)
        assert(train(0x1000, 0x100000 + 14 * 128, 1) == (1, 1), "unallocated hit PCs displaced learned history")
        assert(train(0x1000, 0x100000 + 15 * 128, 2) == (1, 1), "stride-prefetch hit could not issue")
        train(0x5000, 0x200000, 2)
        for (i <- 1 to 12) assert(train(0x5000, 0x200000 + i * 128, 0) == (0, 0))
        assert(train(0x5000, 0x200000 + 13 * 128, 1) == (1, 1), "stride-prefetch hit could not allocate")
      }
    } finally Constantin.init(true)
  }
}
