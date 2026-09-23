package device.RERI

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import top.DefaultConfig

/** Seeded property-style tests for register updates and concurrent hardware writes. */
class RERIPropertyRandomTest extends AnyFlatSpec with ChiselSim {
  private val HeaderBytes = 0x40
  private val RecordBytes = 0x40
  private val Control = 0x00
  private val Status = 0x08
  private val Information = 0x18
  private val SupplementalInformation = 0x20
  private val Timestamp = 0x28

  private def addr(record: Int, offset: Int): BigInt = HeaderBytes + record * RecordBytes + offset

  private def clearHardware(dut: ErrorRecordInterface): Unit = {
    for (i <- 0 until dut.io.hwWrite.length) {
      val e = dut.io.hwWrite(i)
      e.valid.poke(false.B)
      e.ce.poke(false.B)
      e.ued.poke(false.B)
      e.uec.poke(false.B)
      e.priority.poke(0.U)
      e.containable.poke(false.B)
      e.transactionType.poke(0.U)
      e.addressInfoType.poke(0.U)
      e.address.poke(0.U)
      e.informationValid.poke(false.B)
      e.information.poke(0.U)
      e.supplementalInformationValid.poke(false.B)
      e.supplementalInformation.poke(0.U)
      e.scrubbed.poke(false.B)
    }
  }

  private def resetDut(dut: ErrorRecordInterface): Unit = {
    clearHardware(dut)
    dut.io.reg.addr.poke(0.U)
    dut.io.reg.write.poke(false.B)
    dut.io.reg.writeData.poke(0.U)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    dut.clock.step()
  }

  private def readReg(dut: ErrorRecordInterface, address: BigInt): BigInt = {
    dut.io.reg.addr.poke(address.U)
    dut.io.reg.write.poke(false.B)
    dut.io.reg.writeData.poke(0.U)
    dut.io.reg.readData.peek().litValue
  }

  private def writeReg(dut: ErrorRecordInterface, address: BigInt, data: BigInt): Unit = {
    dut.io.reg.addr.poke(address.U)
    dut.io.reg.writeData.poke(data.U(64.W))
    dut.io.reg.write.poke(true.B)
    dut.clock.step()
    dut.io.reg.write.poke(false.B)
  }

  private def checkInvariants(dut: ErrorRecordInterface): Unit = {
    val recordCount = dut.io.hwWrite.length
    val controlMask = (BigInt(0xf) << 60) | (BigInt(0xffff) << 32) |
      (BigInt(3) << 6) | (BigInt(3) << 4) | (BigInt(3) << 2) | (BigInt(1) << 1) | 1
    val statusReserved = (BigInt(0xffff) << 32) | (BigInt(1) << 22) | (BigInt(3) << 18)
    var validBits = BigInt(0)
    for (i <- 0 until recordCount) {
      val control = readReg(dut, addr(i, Control))
      val status = readReg(dut, addr(i, Status))
      assert((control & ~controlMask) == 0, s"control reserved bits changed at record $i")
      assert((status & statusReserved) == 0, s"status reserved bits changed at record $i")
      assert(readReg(dut, addr(i, Information)) == 0, s"disabled info read nonzero at record $i")
      assert(readReg(dut, addr(i, SupplementalInformation)) == 0, s"disabled suppl_info read nonzero at record $i")
      if ((status & 1) != 0) validBits |= BigInt(1) << i
    }
    val summary = readReg(dut, 0x10)
    assert(((summary >> 1) & ((BigInt(1) << recordCount) - 1)) == validBits,
      f"valid_summary mismatch: summary=0x$summary%x expected=0x$validBits%x")
    assert(readReg(dut, 0x00) == RERIVendorId.Reset)
  }

  private def randomHardware(dut: ErrorRecordInterface, random: Random): Unit = {
    clearHardware(dut)
    for (i <- 0 until dut.io.hwWrite.length if random.nextInt(100) < 45) {
      val e = dut.io.hwWrite(i)
      val severity = random.nextInt(4)
      e.valid.poke(true.B)
      e.ce.poke((severity == 1).B)
      e.ued.poke((severity == 2).B)
      e.uec.poke((severity == 3).B)
      e.priority.poke(random.nextInt(4).U)
      e.containable.poke(random.nextBoolean().B)
      e.transactionType.poke(random.nextInt(8).U)
      e.addressInfoType.poke(random.nextInt(16).U)
      e.address.poke((random.nextLong() & Long.MaxValue).U)
      e.informationValid.poke(random.nextBoolean().B)
      e.information.poke((random.nextLong() & Long.MaxValue).U)
      e.supplementalInformationValid.poke(random.nextBoolean().B)
      e.supplementalInformation.poke((random.nextLong() & Long.MaxValue).U)
      e.scrubbed.poke(random.nextBoolean().B)
    }
  }

  behavior of "RERI seeded random updates"
  it should "preserve register invariants across randomized hardware events" in {
    implicit val p = new DefaultConfig
    val random = new Random(0x5eed2026L)
    val param = ErrorRecordInterfaceParam()
    simulate(new ErrorRecordInterface(param)) { dut =>
      resetDut(dut)
      for (_ <- 0 until 120) {
        randomHardware(dut, random)
        dut.clock.step()
        clearHardware(dut)
        checkInvariants(dut)
      }
    }
  }

  it should "exercise randomized software register updates" in {
    implicit val p = new DefaultConfig
    val random = new Random(0x51a7e123L)
    val param = ErrorRecordInterfaceParam()
    simulate(new ErrorRecordInterface(param)) { dut =>
      resetDut(dut)
      for (_ <- 0 until 120) {
        val record = random.nextInt(param.numRecordInfo)
        random.nextInt(5) match {
          case 0 =>
            // Keep ELSE enabled while varying the configurable control fields.
            val control = BigInt(1) | (BigInt(random.nextInt(4)) << 2) | (BigInt(random.nextInt(4)) << 4) |
              (BigInt(random.nextInt(4)) << 6) | (BigInt(random.nextInt(16)) << 60) |
              (BigInt(random.nextInt(32)) << 32)
            writeReg(dut, addr(record, Control), control)
          case 1 =>
            val status = (BigInt(random.nextInt(4)) << 4) | (BigInt(random.nextInt(8)) << 8) |
              (BigInt(random.nextInt(16)) << 12) | (BigInt(random.nextInt(256)) << 24)
            writeReg(dut, addr(record, Status), status)
          case 2 =>
            writeReg(dut, addr(record, Information), random.nextLong() & Long.MaxValue)
          case 3 =>
            writeReg(dut, addr(record, SupplementalInformation), random.nextLong() & Long.MaxValue)
          case _ =>
            writeReg(dut, addr(record, Timestamp), random.nextLong() & Long.MaxValue)
        }
        randomHardware(dut, random)
        dut.clock.step()
        clearHardware(dut)
        checkInvariants(dut)
      }
    }
  }
}
