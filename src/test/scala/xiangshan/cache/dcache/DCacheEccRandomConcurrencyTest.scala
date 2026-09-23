package xiangshan.cache

import chisel3._
import chisel3.util.{Cat, Fill}
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.util.Random
import top.DefaultConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}

class DCacheEccRandomConcurrencyHarness(val requestWidth: Int = 4)(implicit p: Parameters)
    extends DCacheModule {
  val io = IO(new Bundle {
    val rawTagLo = Input(Vec(requestWidth, Vec(nWays, UInt(32.W))))
    val rawTagHi = Input(Vec(requestWidth, Vec(nWays, UInt(32.W))))
    val tagFaultLo = Input(Vec(requestWidth, Vec(nWays, UInt(32.W))))
    val tagFaultHi = Input(Vec(requestWidth, Vec(nWays, UInt(32.W))))
    val rawData = Input(Vec(requestWidth, Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W))))
    val dataFault = Input(Vec(requestWidth, Vec(DCacheBanks, UInt(encDataBits.W))))
    val tagWidth = Output(UInt(8.W))
    val tagFaultWidth = Output(UInt(8.W))
    val tagCorrectable = Output(Vec(requestWidth, UInt(nWays.W)))
    val tagUncorrectable = Output(Vec(requestWidth, UInt(nWays.W)))
    val dataCorrectable = Output(Vec(requestWidth, UInt(DCacheBanks.W)))
    val dataUncorrectable = Output(Vec(requestWidth, UInt(DCacheBanks.W)))
    val correctedTagLo = Output(Vec(requestWidth, Vec(nWays, UInt(32.W))))
    val correctedTagHi = Output(Vec(requestWidth, Vec(nWays, UInt(32.W))))
    val correctedData = Output(Vec(requestWidth, Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W))))
  })

  io.tagWidth := tagBits.U
  io.tagFaultWidth := encTagBits.U
  for (r <- 0 until requestWidth) {
    val corrector = Module(new DCacheEccCorrect)
    corrector.io.tagValid := Fill(nWays, 1.U(1.W))
    corrector.io.dataValid := Fill(DCacheBanks, 1.U(1.W))
    for (w <- 0 until nWays) {
      val rawTag = Cat(io.rawTagHi(r)(w), io.rawTagLo(r)(w))(tagBits - 1, 0)
      val fault = Cat(io.tagFaultHi(r)(w), io.tagFaultLo(r)(w))(encTagBits - 1, 0)
      corrector.io.tag(w) := cacheParams.tagCode.encode(rawTag) ^ fault
      io.correctedTagLo(r)(w) := corrector.io.correctedTag(w)(math.min(tagBits, 32) - 1, 0)
      if (tagBits > 32) {
        io.correctedTagHi(r)(w) := corrector.io.correctedTag(w)(tagBits - 1, 32)
      } else {
        io.correctedTagHi(r)(w) := 0.U
      }
    }
    for (b <- 0 until DCacheBanks) {
      val encoded = cacheParams.dataCode.encode(io.rawData(r)(b)) ^ io.dataFault(r)(b)
      corrector.io.data(b).ecc := encoded(encDataBits - 1, DCacheSRAMRowBits)
      corrector.io.data(b).raw_data := encoded(DCacheSRAMRowBits - 1, 0)
      corrector.io.data(b).error_delayed := false.B
      corrector.io.data(b).correctable_delayed := false.B
      corrector.io.data(b).uncorrectable_delayed := false.B
      io.correctedData(r)(b) := corrector.io.correctedData(b)
    }
    io.tagCorrectable(r) := corrector.io.tagCorrectable
    io.tagUncorrectable(r) := corrector.io.tagUncorrectable
    io.dataCorrectable(r) := corrector.io.dataCorrectable
    io.dataUncorrectable(r) := corrector.io.dataUncorrectable
  }
}

class DCacheEccRandomConcurrencyTest extends AnyFlatSpec with ChiselSim {
  private val RequestWidth = 4
  private val Amo = 3
  private val Lrsc = 4
  private val LowMask = (BigInt(1) << 32) - 1

  private def randomMask(r: Random, width: Int, count: Int): BigInt = {
    val bits = mutable.LinkedHashSet.empty[Int]
    while (bits.size < count) bits += r.nextInt(width)
    bits.foldLeft(BigInt(0))((value, bit) => value | (BigInt(1) << bit))
  }

  private def pokeSplit(lo: UInt, hi: UInt, value: BigInt): Unit = {
    lo.poke((value & LowMask).U)
    hi.poke((value >> 32).U)
  }

  private def readSplit(lo: UInt, hi: UInt): BigInt = lo.peek().litValue | (hi.peek().litValue << 32)

  private def clearFaults(d: DCacheEccRandomConcurrencyHarness): Unit = {
    for (req <- 0 until RequestWidth; way <- 0 until d.io.tagFaultLo(req).length) {
      d.io.tagFaultLo(req)(way).poke(0.U)
      d.io.tagFaultHi(req)(way).poke(0.U)
    }
    for (req <- 0 until RequestWidth; bank <- 0 until d.io.dataFault(req).length) {
      d.io.dataFault(req)(bank).poke(0.U)
    }
  }

  private def configureCleanInputs(d: DCacheEccRandomConcurrencyHarness, tagWidth: Int): Unit = {
    for (req <- 0 until RequestWidth; way <- 0 until d.io.rawTagLo(req).length) {
      val value = (BigInt(req + 1) << 20) | (way + 1)
      pokeSplit(d.io.rawTagLo(req)(way), d.io.rawTagHi(req)(way), value & ((BigInt(1) << tagWidth) - 1))
    }
    for (req <- 0 until RequestWidth; bank <- 0 until d.io.rawData(req).length) {
      d.io.rawData(req)(bank).poke((req * 0x100 + bank).U)
    }
    clearFaults(d)
  }

  behavior of "DCache ECC decoder"

  it should "correct single-bit faults and classify multi-bit faults deterministically" in {
    val defaultConfig = new DefaultConfig
    implicit val p = defaultConfig.alterPartial({ case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy() })
    simulate(new DCacheEccRandomConcurrencyHarness(RequestWidth)) { d =>
      val tagWidth = d.io.tagWidth.peek().litValue.toInt
      configureCleanInputs(d, tagWidth)
      d.clock.step()
      for (req <- 0 until RequestWidth) {
        assert(d.io.tagCorrectable(req).peek().litValue == 0)
        assert(d.io.tagUncorrectable(req).peek().litValue == 0)
        assert(d.io.dataCorrectable(req).peek().litValue == 0)
        assert(d.io.dataUncorrectable(req).peek().litValue == 0)
      }

      configureCleanInputs(d, tagWidth)
      pokeSplit(d.io.tagFaultLo(0)(0), d.io.tagFaultHi(0)(0), 1)
      d.io.dataFault(1)(d.io.dataFault(1).length - 1).poke(1.U)
      pokeSplit(d.io.tagFaultLo(2)(d.io.tagFaultLo(2).length - 1), d.io.tagFaultHi(2)(d.io.tagFaultHi(2).length - 1), 3)
      pokeSplit(d.io.tagFaultLo(3)(1), d.io.tagFaultHi(3)(1), 1)
      d.io.dataFault(3)(0).poke(3.U)
      d.clock.step()

      assert(d.io.tagCorrectable(0).peek().litValue == 1, "tag CE way")
      assert(readSplit(d.io.correctedTagLo(0)(0), d.io.correctedTagHi(0)(0)) == ((BigInt(1) << 20) | 1), "tag CE corrected value")
      assert(d.io.dataCorrectable(1).peek().litValue == (BigInt(1) << (d.io.dataFault(1).length - 1)), "data CE bank")
      assert(d.io.correctedData(1)(d.io.dataFault(1).length - 1).peek().litValue == (BigInt(1) << 8 | (d.io.dataFault(1).length - 1)), "data CE corrected value")
      assert(d.io.tagUncorrectable(2).peek().litValue == (BigInt(1) << (d.io.tagFaultLo(2).length - 1)), "tag UEC way")
      assert(d.io.tagCorrectable(2).peek().litValue == 0, "tag UEC is not CE")
      assert(d.io.tagCorrectable(3).peek().litValue == (BigInt(1) << 1), "mixed tag CE")
      assert(d.io.dataUncorrectable(3).peek().litValue == 1, "mixed data UEC")
    }
  }

  behavior of "DCache ECC random concurrency"

  it should "correct CE and classify UEC under randomized concurrent traffic" in {
    val defaultConfig = new DefaultConfig
    implicit val p = defaultConfig.alterPartial({ case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy() })
    simulate(new DCacheEccRandomConcurrencyHarness(RequestWidth)) { d =>
      val tagWidth = d.io.tagWidth.peek().litValue.toInt
      val tagFaultWidth = d.io.tagFaultWidth.peek().litValue.toInt
      val tagMask = (BigInt(1) << tagWidth) - 1
      val seeds = Seq.fill(5)(Random.nextLong())
      var coveredNone = false
      var coveredTagCe = false
      var coveredDataCe = false
      var coveredTagUec = false
      var coveredDataUec = false
      var coveredMixed = false

      for (seed <- seeds) {
        val random = new Random(seed)
        println(s"DCache ECC concurrency seed=$seed")
        for (cycle <- 0 until 800) {
          val atomicMode = random.nextInt(100) < 20
          val operations = if (atomicMode) Seq(if (random.nextBoolean()) Amo else Lrsc, -1, -1, -1) else Seq.fill(RequestWidth)(random.nextInt(3))
          assert(operations.count(op => op == Amo || op == Lrsc) <= 1, s"seed=$seed cycle=$cycle AMO/LRSC overlap")

          clearFaults(d)
          val rawTags = Array.tabulate(RequestWidth)(req => Array.fill(d.io.rawTagLo(req).length)(BigInt(0)))
          val rawData = Array.tabulate(RequestWidth)(req => Array.fill(d.io.rawData(req).length)(BigInt(0)))
          val tagCeWay = Array.fill(RequestWidth)(-1)
          val dataCeBank = Array.fill(RequestWidth)(-1)
          val tagUecWay = Array.fill(RequestWidth)(-1)
          val dataUecBank = Array.fill(RequestWidth)(-1)

          for (req <- 0 until RequestWidth) {
            for (way <- 0 until d.io.rawTagLo(req).length) {
              val value = (BigInt(cycle + req + way + 1) * 0x13579bdfL) & tagMask
              rawTags(req)(way) = value
              pokeSplit(d.io.rawTagLo(req)(way), d.io.rawTagHi(req)(way), value)
            }
            for (bank <- 0 until d.io.rawData(req).length) {
              val value = (BigInt(seed).abs + cycle * 17 + req * 31 + bank) & ((BigInt(1) << d.io.rawData(req)(bank).getWidth) - 1)
              rawData(req)(bank) = value
              d.io.rawData(req)(bank).poke(value.U)
            }
            if (operations(req) >= 0) {
              random.nextInt(100) match {
                case x if x < 12 =>
                  val way = random.nextInt(d.io.tagFaultLo(req).length)
                  pokeSplit(d.io.tagFaultLo(req)(way), d.io.tagFaultHi(req)(way), randomMask(random, tagFaultWidth, 1))
                  tagCeWay(req) = way; coveredTagCe = true
                case x if x < 20 =>
                  val way = random.nextInt(d.io.tagFaultLo(req).length)
                  pokeSplit(d.io.tagFaultLo(req)(way), d.io.tagFaultHi(req)(way), randomMask(random, tagFaultWidth, 2))
                  tagUecWay(req) = way; coveredTagUec = true
                case x if x < 32 =>
                  val bank = random.nextInt(d.io.dataFault(req).length)
                  d.io.dataFault(req)(bank).poke(randomMask(random, d.io.dataFault(req)(bank).getWidth, 1).U)
                  dataCeBank(req) = bank; coveredDataCe = true
                case x if x < 40 =>
                  val bank = random.nextInt(d.io.dataFault(req).length)
                  d.io.dataFault(req)(bank).poke(randomMask(random, d.io.dataFault(req)(bank).getWidth, 2).U)
                  dataUecBank(req) = bank; coveredDataUec = true
                case x if x < 45 =>
                  val way = random.nextInt(d.io.tagFaultLo(req).length)
                  val bank = random.nextInt(d.io.dataFault(req).length)
                  pokeSplit(d.io.tagFaultLo(req)(way), d.io.tagFaultHi(req)(way), randomMask(random, tagFaultWidth, 1))
                  d.io.dataFault(req)(bank).poke(randomMask(random, d.io.dataFault(req)(bank).getWidth, 2).U)
                  tagCeWay(req) = way; dataUecBank(req) = bank; coveredMixed = true
                case _ => coveredNone = true
              }
            }
          }

          d.clock.step()
          for (req <- 0 until RequestWidth) {
            assert((d.io.tagCorrectable(req).peek().litValue != 0) == (tagCeWay(req) >= 0), s"seed=$seed cycle=$cycle req=$req tag CE")
            assert((d.io.tagUncorrectable(req).peek().litValue != 0) == (tagUecWay(req) >= 0), s"seed=$seed cycle=$cycle req=$req tag UEC")
            assert((d.io.dataCorrectable(req).peek().litValue != 0) == (dataCeBank(req) >= 0), s"seed=$seed cycle=$cycle req=$req data CE")
            assert((d.io.dataUncorrectable(req).peek().litValue != 0) == (dataUecBank(req) >= 0), s"seed=$seed cycle=$cycle req=$req data UEC")
            if (tagCeWay(req) >= 0) {
              assert(readSplit(d.io.correctedTagLo(req)(tagCeWay(req)), d.io.correctedTagHi(req)(tagCeWay(req))) == rawTags(req)(tagCeWay(req)),
                s"seed=$seed cycle=$cycle req=$req corrected tag")
            }
            if (dataCeBank(req) >= 0) {
              assert(d.io.correctedData(req)(dataCeBank(req)).peek().litValue == rawData(req)(dataCeBank(req)),
                s"seed=$seed cycle=$cycle req=$req corrected data")
            }
          }
        }
      }
      assert(coveredNone && coveredTagCe && coveredDataCe && coveredTagUec && coveredDataUec && coveredMixed, "random coverage incomplete")
    }
  }
}
