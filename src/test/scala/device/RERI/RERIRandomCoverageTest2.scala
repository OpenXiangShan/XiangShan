package device.RERI

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import top.DefaultConfig

class RERIRandomCoverageTest2 extends AnyFlatSpec with ChiselSim {
  private val H = 0x40
  private val R = 0x40
  private val C = 0x00
  private val S = 0x08
  private val I = 0x18
  private val SI = 0x20
  private val T = 0x28

  private def a(i: Int, o: Int) = H + i * R + o
  private def clear(d: ErrorRecordInterface): Unit = for (i <- 0 until d.io.hwWrite.length) {
    val e = d.io.hwWrite(i)
    e.valid.poke(false.B); e.ce.poke(false.B); e.ued.poke(false.B); e.uec.poke(false.B)
    e.priority.poke(0.U); e.containable.poke(false.B); e.transactionType.poke(0.U)
    e.addressInfoType.poke(0.U); e.address.poke(0.U); e.informationValid.poke(false.B)
    e.information.poke(0.U); e.supplementalInformationValid.poke(false.B)
    e.supplementalInformation.poke(0.U); e.scrubbed.poke(false.B)
  }
  private def resetD(d: ErrorRecordInterface): Unit = {
    clear(d); d.io.reg.addr.poke(0.U); d.io.reg.write.poke(false.B); d.io.reg.writeData.poke(0.U)
    d.reset.poke(true.B); d.clock.step(2); d.reset.poke(false.B); d.clock.step()
  }
  private def rd(d: ErrorRecordInterface, x: BigInt): BigInt = {
    d.io.reg.addr.poke(x.U); d.io.reg.write.poke(false.B); d.io.reg.readData.peek().litValue
  }
  private def wr(d: ErrorRecordInterface, x: BigInt, v: BigInt): Unit = {
    d.io.reg.addr.poke(x.U); d.io.reg.writeData.poke(v.U(64.W)); d.io.reg.write.poke(true.B)
    d.clock.step(); d.io.reg.write.poke(false.B)
  }
  private def event(d: ErrorRecordInterface, i: Int, s: Int, p: Int): Unit = {
    clear(d); val e = d.io.hwWrite(i)
    e.valid.poke(true.B); e.ce.poke((s == 1).B); e.ued.poke((s == 2).B); e.uec.poke((s == 3).B)
    e.priority.poke(p.U); e.transactionType.poke(4.U); e.addressInfoType.poke(1.U)
    e.address.poke((0x1000 + i * 0x40).U); d.clock.step(); clear(d)
  }
  private val statusReserved = (BigInt(0xffff) << 32) | (BigInt(1) << 22) | (BigInt(3) << 18)
  private val controlLegal = (BigInt(0xf) << 60) | (BigInt(0xffff) << 32) |
    (BigInt(3) << 6) | (BigInt(3) << 4) | (BigInt(3) << 2) | (BigInt(1) << 1) | 1

  behavior of "RERI fresh-seed coverage"

  it should "cover header and wrong-address writes for five fresh seeds" in {
    implicit val p = new DefaultConfig
    simulate(new ErrorRecordInterface(ErrorRecordInterfaceParam())) { d =>
      resetD(d)
      val seeds = Seq.fill(5)(Random.nextLong())
      for (seed <- seeds) {
        val r = new Random(seed); println("RERI seed=" + seed)
        val before = Seq(0x00, 0x08, 0x10).map(rd(d, _))
        for (_ <- 0 until 10) wr(d, Seq(0x00, 0x08, 0x10, 0x38)(r.nextInt(4)), r.nextLong() & Long.MaxValue)
        assert(Seq(0x00, 0x08, 0x10).map(rd(d, _)) == before, "seed=" + seed)
        val record = r.nextInt(6); val wrong = (record + 1 + r.nextInt(5)) % 6
        wr(d, a(wrong, I), r.nextInt(0x10000)); assert(rd(d, a(wrong, I)) == 0, "seed=" + seed)
      }
    }
  }

  it should "cover simultaneous records and software hardware races for five fresh seeds" in {
    implicit val p = new DefaultConfig
    simulate(new ErrorRecordInterface(ErrorRecordInterfaceParam())) { d =>
      resetD(d)
      for (seed <- Seq.fill(5)(Random.nextLong())) {
        val r = new Random(seed); println("RERI seed=" + seed)
        resetD(d)
        for (i <- 1 until 6 if r.nextBoolean()) {
          val e = d.io.hwWrite(i); val s = 1 + r.nextInt(3)
          e.valid.poke(true.B); e.ce.poke((s == 1).B); e.ued.poke((s == 2).B); e.uec.poke((s == 3).B)
          e.priority.poke(r.nextInt(4).U)
        }
        d.clock.step(); clear(d)
        val rec = 0; d.io.hwWrite(rec).valid.poke(true.B); d.io.hwWrite(rec).uec.poke(true.B)
        d.io.reg.addr.poke(a(rec, S).U); d.io.reg.writeData.poke((1 << 1).U); d.io.reg.write.poke(true.B)
        d.clock.step(); d.io.reg.write.poke(false.B); clear(d)
        val st = rd(d, a(rec, S)); assert((st & 2) != 0 && (st & 8) == 0, "seed=" + seed)
      }
    }
  }

  it should "cover EID and software register updates for five fresh seeds" in {
    implicit val p = new DefaultConfig
    simulate(new ErrorRecordInterface(ErrorRecordInterfaceParam())) { d =>
      resetD(d)
      for (seed <- Seq.fill(5)(Random.nextLong())) {
        val r = new Random(seed); println("RERI seed=" + seed); val i = r.nextInt(6); val delay = 1 + r.nextInt(4)
        wr(d, a(i, S), 2); wr(d, a(i, C), (BigInt(delay) << 32) | 1)
        for (_ <- 0 until delay) event(d, i, 3, r.nextInt(4))
        assert((rd(d, a(i, S)) & 1) != 0, "seed=" + seed)
        wr(d, a(i, C), BigInt(1) << 49); assert((rd(d, a(i, S)) & (1 << 23)) != 0, "seed=" + seed)
      }
    }
  }

  it should "cover field legality and reserved bits for five fresh seeds" in {
    implicit val p = new DefaultConfig
    simulate(new ErrorRecordInterface(ErrorRecordInterfaceParam())) { d =>
      resetD(d)
      for (seed <- Seq.fill(5)(Random.nextLong())) {
        val r = new Random(seed); println("RERI seed=" + seed); val i = r.nextInt(6)
        for (_ <- 0 until 12) {
          wr(d, a(i, C), BigInt(1) | (BigInt(r.nextInt(4)) << 2) | (BigInt(r.nextInt(4)) << 4))
          wr(d, a(i, S), (BigInt(r.nextInt(4)) << 4) | (BigInt(r.nextInt(8)) << 8))
          assert((rd(d, a(i, C)) & ~controlLegal) == 0, "seed=" + seed)
          assert((rd(d, a(i, S)) & statusReserved) == 0, "seed=" + seed)
        }
      }
    }
  }

  it should "cover severity CEC and timestamp updates for five fresh seeds" in {
    implicit val p = new DefaultConfig
    simulate(new ErrorRecordInterface(ErrorRecordInterfaceParam())) { d =>
      resetD(d)
      for (seed <- Seq.fill(5)(Random.nextLong())) {
        val r = new Random(seed); println("RERI seed=" + seed); val last = Array.fill[BigInt](6)(BigInt(-1))
        for (_ <- 0 until 12) {
          val i = r.nextInt(6); event(d, i, 1 + r.nextInt(3), r.nextInt(4))
          val st = rd(d, a(i, S)); val ts = rd(d, a(i, T))
          assert((st & 1) != 0 && (st & statusReserved) == 0, "seed=" + seed)
          assert(ts >= last(i), "seed=" + seed); last(i) = ts
          assert((rd(d, 0x10) & 1) == 1, "seed=" + seed)
        }
      }
    }
  }

  it should "cover optional info disable and timestamp writes for five fresh seeds" in {
    implicit val p = new DefaultConfig
    simulate(new ErrorRecordInterface(ErrorRecordInterfaceParam())) { d =>
      resetD(d)
      for (seed <- Seq.fill(5)(Random.nextLong())) {
        val r = new Random(seed); println("RERI seed=" + seed); val i = r.nextInt(6)
        wr(d, a(i, I), r.nextInt(0x10000)); wr(d, a(i, SI), r.nextInt(0x10000))
        assert(rd(d, a(i, I)) == 0 && rd(d, a(i, SI)) == 0, "seed=" + seed)
        wr(d, a(i, T), r.nextLong() & Long.MaxValue)
        assert(rd(d, a(i, T)) != 0, "seed=" + seed)
      }
    }
  }
}
