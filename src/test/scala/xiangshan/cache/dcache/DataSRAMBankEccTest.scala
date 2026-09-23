package xiangshan.cache

import chisel3._
import chisel3.util.OHToUInt
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.util.Random
import top.DefaultConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{DFTOptions, DFTOptionsKey, XSCoreParamsKey, XSTileKey}

class DataSRAMBankEccHarness(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val write = Input(Bool())
    val read = Input(Bool())
    val addr = Input(UInt(idxBits.W))
    val way = Input(UInt(nWays.W))
    val raw = Input(UInt(DCacheSRAMRowBits.W))
    val fault = Input(UInt(encDataBits.W))
    val correctable = Output(Bool())
    val uncorrectable = Output(Bool())
    val corrected = Output(UInt(DCacheSRAMRowBits.W))
  })

  val bank = Module(new DataSRAMBank(0))
  bank.io.w.en := io.write
  bank.io.w.addr := io.addr
  bank.io.w.way_en := io.way
  bank.io.w.data := cacheParams.dataCode.encode(io.raw) ^ io.fault
  bank.io.r.en := io.read
  bank.io.r.addr := io.addr

  val decoder = Module(new DCacheEccDetect(DCacheSRAMRowBits, cacheParams.dataCode))
  decoder.io.encoded := bank.io.r.data(OHToUInt(io.way))
  decoder.io.valid := RegNext(io.read, false.B)
  io.correctable := decoder.io.correctable
  io.uncorrectable := decoder.io.uncorrectable
  io.corrected := decoder.io.corrected
}

class DataSRAMBankEccTest extends AnyFlatSpec with ChiselSim {
  private def config = {
    val base = new DefaultConfig
    base.alterPartial({ case XSCoreParamsKey => base(XSTileKey).head.copy() }).alterPartial({
      case DFTOptionsKey => DFTOptions(EnableMbist = false, EnableSramCtl = false)
      case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
      case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    })
  }

  private def mask(random: Random, width: Int, count: Int): BigInt = {
    val bits = mutable.LinkedHashSet.empty[Int]
    while (bits.size < count) bits += random.nextInt(width)
    bits.foldLeft(BigInt(0))((value, bit) => value | (BigInt(1) << bit))
  }

  private def write(d: DataSRAMBankEccHarness, raw: BigInt, fault: BigInt): Unit = {
    d.io.raw.poke(raw.U)
    d.io.fault.poke(fault.U)
    d.io.write.poke(true.B)
    d.clock.step()
    d.io.write.poke(false.B)
  }

  private def observe(d: DataSRAMBankEccHarness): (Boolean, Boolean, BigInt) = {
    var ce = false
    var uec = false
    var corrected = BigInt(0)
    for (_ <- 0 until 4) {
      if (d.io.correctable.peek().litToBoolean) {
        ce = true
        corrected = d.io.corrected.peek().litValue
      }
      if (d.io.uncorrectable.peek().litToBoolean) uec = true
      d.clock.step()
    }
    (ce, uec, corrected)
  }

  behavior of "DataSRAMBank ECC"

  it should "classify encoded SRAM flips deterministically and under random stress" in {
    implicit val p = config
    simulate(new DataSRAMBankEccHarness) { d =>
      d.io.addr.poke(0.U)
      d.io.way.poke(1.U)
      d.io.write.poke(false.B)
      d.io.read.poke(false.B)
      d.io.raw.poke(0.U)
      d.io.fault.poke(0.U)
      d.reset.poke(true.B)
      d.clock.step(2)
      d.reset.poke(false.B)
      d.clock.step()

      write(d, 0x1234, 1)
      d.io.read.poke(true.B)
      d.clock.step()
      d.io.read.poke(false.B)
      val (ce, uec, corrected) = observe(d)
      assert(ce, "single SRAM bit flip must be CE")
      assert(!uec)
      assert(corrected == 0x1234)

      write(d, 0xabcd, 3)
      d.io.read.poke(true.B)
      d.clock.step()
      d.io.read.poke(false.B)
      val (ce2, uec2, _) = observe(d)
      assert(!ce2)
      assert(uec2, "two SRAM bit flips must be UEC")

      val random = new Random(Random.nextLong())
      println("DataSRAMBank ECC stress seed=" + random.nextLong())
      for (cycle <- 0 until 1000) {
        val way = BigInt(1) << random.nextInt(d.io.way.getWidth)
        val raw = BigInt(random.nextInt(1 << d.io.raw.getWidth))
        val kind = random.nextInt(3)
        val fault = if (kind == 0) BigInt(0) else mask(random, d.io.fault.getWidth, if (kind == 1) 1 else 2)
        d.io.way.poke(way.U)
        d.io.addr.poke((cycle % 4).U)
        write(d, raw, fault)
        d.io.read.poke(true.B)
        d.clock.step()
        d.io.read.poke(false.B)
        val (gotCe, gotUec, gotData) = observe(d)
        assert(gotCe == (kind == 1), "cycle=" + cycle + " CE")
        assert(gotUec == (kind == 2), "cycle=" + cycle + " UEC")
        if (kind == 1) assert(gotData == raw, "cycle=" + cycle + " correction")
      }
    }
  }
}
