package device.RERI

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import top.DefaultConfig

class RERIStressTest extends AnyFlatSpec with ChiselSim {
  private val H = 0x40
  private val R = 0x40
  private val C = 0x00
  private val S = 0x08
  private val I = 0x18
  private val SI = 0x20
  private val T = 0x28
  private val statusReserved = (BigInt(0xffff) << 32) | (BigInt(1) << 22) | (BigInt(3) << 18)
  private val controlLegal = (BigInt(0xf) << 60) | (BigInt(0xffff) << 32) |
    (BigInt(3) << 6) | (BigInt(3) << 4) | (BigInt(3) << 2) | (BigInt(1) << 1) | 1

  private def addr(i: Int, o: Int): BigInt = H + i * R + o
  private def clearHw(d: ErrorRecordInterface): Unit = for (i <- 0 until d.io.hwWrite.length) {
    val e = d.io.hwWrite(i)
    e.valid.poke(false.B); e.ce.poke(false.B); e.ued.poke(false.B); e.uec.poke(false.B)
    e.priority.poke(0.U); e.containable.poke(false.B); e.transactionType.poke(0.U)
    e.addressInfoType.poke(0.U); e.address.poke(0.U); e.informationValid.poke(false.B)
    e.information.poke(0.U); e.supplementalInformationValid.poke(false.B)
    e.supplementalInformation.poke(0.U); e.scrubbed.poke(false.B)
  }
  private def resetD(d: ErrorRecordInterface): Unit = {
    clearHw(d); d.io.reg.addr.poke(0.U); d.io.reg.write.poke(false.B); d.io.reg.writeData.poke(0.U)
    d.reset.poke(true.B); d.clock.step(2); d.reset.poke(false.B); d.clock.step()
  }
  private def read(d: ErrorRecordInterface, x: BigInt): BigInt = {
    d.io.reg.addr.poke(x.U); d.io.reg.write.poke(false.B); d.io.reg.readData.peek().litValue
  }
  private def write(d: ErrorRecordInterface, x: BigInt, v: BigInt): Unit = {
    d.io.reg.addr.poke(x.U); d.io.reg.writeData.poke(v.U(64.W)); d.io.reg.write.poke(true.B)
    d.clock.step(); d.io.reg.write.poke(false.B)
  }
  private def events(d: ErrorRecordInterface, r: Random): Unit = {
    clearHw(d)
    for (i <- 0 until d.io.hwWrite.length if r.nextInt(100) < 65) {
      val e = d.io.hwWrite(i); val s = 1 + r.nextInt(3)
      e.valid.poke(true.B); e.ce.poke((s == 1).B); e.ued.poke((s == 2).B); e.uec.poke((s == 3).B)
      e.priority.poke(r.nextInt(4).U); e.containable.poke(r.nextBoolean().B)
      e.transactionType.poke(r.nextInt(8).U); e.addressInfoType.poke(r.nextInt(16).U)
      e.address.poke((r.nextLong() & Long.MaxValue).U); e.scrubbed.poke(r.nextBoolean().B)
    }
  }
  private def check(d: ErrorRecordInterface, ts: Array[BigInt], cycle: Int, seed: Long, checkTimestamp: Boolean = true): Unit = {
    var bits = BigInt(0)
    for (i <- 0 until d.io.hwWrite.length) {
      val c = read(d, addr(i, C)); val s = read(d, addr(i, S)); val t = read(d, addr(i, T))
      assert((c & ~controlLegal) == 0, "seed=" + seed + " cycle=" + cycle + " control")
      assert((s & statusReserved) == 0, "seed=" + seed + " cycle=" + cycle + " status")
      if (checkTimestamp) assert(t >= ts(i), "seed=" + seed + " cycle=" + cycle + " timestamp")
      ts(i) = t
      if ((s & 1) != 0) bits |= BigInt(1) << i
    }
    val summary = read(d, 0x10)
    assert(((summary >> 1) & ((BigInt(1) << d.io.hwWrite.length) - 1)) == bits,
      "seed=" + seed + " cycle=" + cycle + " summary")
  }

  behavior of "RERI long stress"

  it should "survive 2000 cycles of hardware traffic" in {
    implicit val p = new DefaultConfig
    simulate(new ErrorRecordInterface(ErrorRecordInterfaceParam())) { d =>
      val seed = Random.nextLong(); val r = new Random(seed); val ts = Array.fill[BigInt](6)(BigInt(-1))
      println("RERI stress seed=" + seed); resetD(d)
      for (cycle <- 0 until 2000) { events(d, r); d.clock.step(); clearHw(d); if (cycle % 8 == 0) check(d, ts, cycle, seed) }
      check(d, ts, 2000, seed)
    }
  }

  it should "survive 3000 cycles of mixed updates" in {
    implicit val p = new DefaultConfig
    simulate(new ErrorRecordInterface(ErrorRecordInterfaceParam())) { d =>
      val seed = Random.nextLong(); val r = new Random(seed); val ts = Array.fill[BigInt](6)(BigInt(-1))
      println("RERI stress seed=" + seed); resetD(d)
      for (cycle <- 0 until 3000) {
        if (r.nextInt(100) < 40) {
          val i = r.nextInt(6)
          r.nextInt(5) match {
            case 0 => write(d, addr(i, C), BigInt(1) | (BigInt(r.nextInt(4)) << 2) | (BigInt(r.nextInt(4)) << 4))
            case 1 => write(d, addr(i, S), (BigInt(r.nextInt(4)) << 4) | (BigInt(r.nextInt(8)) << 8))
            case 2 => write(d, addr(i, I), r.nextLong() & Long.MaxValue)
            case 3 => write(d, addr(i, SI), r.nextLong() & Long.MaxValue)
            case _ => write(d, addr(i, T), r.nextLong() & Long.MaxValue)
          }
        }
        events(d, r); d.clock.step(); clearHw(d); if (cycle % 10 == 0) check(d, ts, cycle, seed)
      }
      check(d, ts, 3000, seed)
    }
  }

  it should "survive 4096 cycles of boundary rotation" in {
    implicit val p = new DefaultConfig
    simulate(new ErrorRecordInterface(ErrorRecordInterfaceParam())) { d =>
      val seed = Random.nextLong(); val r = new Random(seed); val ts = Array.fill[BigInt](6)(BigInt(-1))
      println("RERI stress seed=" + seed); resetD(d)
      val offsets = Seq(C, S, I, SI, T, 0x30, 0x38)
      for (cycle <- 0 until 4096) {
        val i = cycle % 6; write(d, addr(i, offsets(cycle % offsets.length)), r.nextLong() & Long.MaxValue)
        if (cycle % 3 == 0) events(d, r) else clearHw(d)
        d.clock.step(); clearHw(d); if (cycle % 16 == 0) check(d, ts, cycle, seed, checkTimestamp = false)
      }
      check(d, ts, 4096, seed, checkTimestamp = false)
    }
  }
}
