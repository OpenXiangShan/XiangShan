package xiangshan.cache

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.util.Random
import top.DefaultConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}

class DCacheEccSramReadModel(val ports: Int = 2)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val write = Input(Bool())
    val writeData = Input(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
    val read = Input(Vec(ports, Bool()))
    val fault = Input(Vec(ports, Vec(DCacheBanks, UInt(encDataBits.W))))
    val correctable = Output(Vec(ports, UInt(DCacheBanks.W)))
    val uncorrectable = Output(Vec(ports, UInt(DCacheBanks.W)))
    val corrected = Output(Vec(ports, Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W))))
  })

  val encodedSram = RegInit(VecInit(Seq.fill(DCacheBanks)(0.U(encDataBits.W))))
  when(io.write) {
    for (bank <- 0 until DCacheBanks) {
      encodedSram(bank) := cacheParams.dataCode.encode(io.writeData(bank))
    }
  }

  for (port <- 0 until ports) {
    val readEncoded = RegEnable(VecInit((0 until DCacheBanks).map(bank => encodedSram(bank) ^ io.fault(port)(bank))), io.read(port))
    val readValid = RegNext(io.read(port), false.B)
    val decoders = Seq.tabulate(DCacheBanks) { bank =>
      val decoder = Module(new DCacheEccDetect(DCacheSRAMRowBits, cacheParams.dataCode))
      decoder.io.encoded := readEncoded(bank)
      decoder.io.valid := readValid
      io.corrected(port)(bank) := decoder.io.corrected
      decoder
    }
    io.correctable(port) := VecInit(decoders.map(_.io.correctable)).asUInt
    io.uncorrectable(port) := VecInit(decoders.map(_.io.uncorrectable)).asUInt
  }
}

class DCacheEccSramReadModelTest extends AnyFlatSpec with ChiselSim {
  private val Ports = 2

  private def config = {
    val defaultConfig = new DefaultConfig
    defaultConfig.alterPartial({ case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy() })
  }

  private def mask(random: Random, width: Int, count: Int): BigInt = {
    val bits = mutable.LinkedHashSet.empty[Int]
    while (bits.size < count) bits += random.nextInt(width)
    bits.foldLeft(BigInt(0))((value, bit) => value | (BigInt(1) << bit))
  }

  private def clearRead(d: DCacheEccSramReadModel): Unit = {
    for (port <- 0 until Ports) {
      d.io.read(port).poke(false.B)
      for (bank <- 0 until d.io.fault(port).length) d.io.fault(port)(bank).poke(0.U)
    }
  }

  private def writeLine(d: DCacheEccSramReadModel, line: Seq[BigInt]): Unit = {
    for (bank <- line.indices) d.io.writeData(bank).poke(line(bank).U)
    d.io.write.poke(true.B)
    d.clock.step()
    d.io.write.poke(false.B)
  }

  behavior of "encoded SRAM read fault model"

  it should "preserve bank attribution and correction across concurrent reads" in {
    implicit val p = config
    simulate(new DCacheEccSramReadModel(Ports)) { d =>
      val line = (0 until d.io.writeData.length).map(bank => BigInt(0x100 + bank))
      clearRead(d)
      d.io.write.poke(false.B)
      writeLine(d, line)

      val last = d.io.fault(1).length - 1
      d.io.read(0).poke(true.B)
      d.io.fault(0)(0).poke(1.U)
      d.io.read(1).poke(true.B)
      d.io.fault(1)(last).poke(3.U)
      d.clock.step()
      clearRead(d)
      assert(d.io.correctable(0).peek().litValue == 1, "port0 CE bank attribution")
      assert(d.io.uncorrectable(0).peek().litValue == 0)
      assert(d.io.corrected(0)(0).peek().litValue == line.head, "port0 corrected data")
      assert(d.io.uncorrectable(1).peek().litValue == (BigInt(1) << last), "port1 UEC bank attribution")
      assert(d.io.correctable(1).peek().litValue == 0)

      val overwrite = line.updated(last, BigInt(0xbeef))
      writeLine(d, overwrite)
      d.io.read(0).poke(true.B)
      d.clock.step()
      clearRead(d)
      assert(d.io.correctable(0).peek().litValue == 0)
      assert(d.io.uncorrectable(0).peek().litValue == 0)
      assert(d.io.corrected(0)(last).peek().litValue == overwrite(last), "overwrite restores clean encoded word")
    }
  }

  it should "survive randomized concurrent encoded SRAM reads" in {
    implicit val p = config
    simulate(new DCacheEccSramReadModel(Ports)) { d =>
      for (seed <- Seq.fill(5)(Random.nextLong())) {
        val random = new Random(seed)
        println(s"DCache SRAM fault-model seed=$seed")
        clearRead(d)
        d.io.write.poke(false.B)
        for (cycle <- 0 until 500) {
          val line = (0 until d.io.writeData.length).map(bank => (BigInt(seed).abs + cycle * 37 + bank) & 0xffff)
          writeLine(d, line)
          val expectedCe = Array.fill(Ports)(BigInt(0))
          val expectedUec = Array.fill(Ports)(BigInt(0))
          for (port <- 0 until Ports) {
            d.io.read(port).poke(true.B)
            for (bank <- 0 until d.io.fault(port).length) d.io.fault(port)(bank).poke(0.U)
            random.nextInt(4) match {
              case 1 =>
                val bank = random.nextInt(d.io.fault(port).length)
                d.io.fault(port)(bank).poke(mask(random, d.io.fault(port)(bank).getWidth, 1).U)
                expectedCe(port) = BigInt(1) << bank
              case 2 =>
                val bank = random.nextInt(d.io.fault(port).length)
                d.io.fault(port)(bank).poke(mask(random, d.io.fault(port)(bank).getWidth, 2).U)
                expectedUec(port) = BigInt(1) << bank
              case _ =>
            }
          }
          d.clock.step()
          clearRead(d)
          for (port <- 0 until Ports) {
            assert(d.io.correctable(port).peek().litValue == expectedCe(port), s"seed=$seed cycle=$cycle port=$port CE")
            assert(d.io.uncorrectable(port).peek().litValue == expectedUec(port), s"seed=$seed cycle=$cycle port=$port UEC")
          }
        }
      }
    }
  }
}
