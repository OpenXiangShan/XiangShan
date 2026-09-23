package device.RERI

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig

/** RERI v1.0 behavior tests. These tests intentionally use the standalone MMIO port. */
class ErrorRecordInterfaceTest extends AnyFlatSpec with ChiselSim {
  private val HeaderBytes = 0x40
  private val RecordBytes = 0x40
  private val Control = 0x00
  private val Status = 0x08
  private val AddressInfo = 0x10
  private val Information = 0x18
  private val SupplementalInformation = 0x20
  private val Timestamp = 0x28

  private def addr(record: Int, offset: Int): BigInt = HeaderBytes + record * RecordBytes + offset

  private def clearHardware(dut: ErrorRecordInterface, count: Int): Unit = {
    for (i <- 0 until count) {
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

  private def resetDut(dut: ErrorRecordInterface, count: Int): Unit = {
    clearHardware(dut, count)
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

  private def hardwareError(
    dut: ErrorRecordInterface,
    record: Int,
    ce: Boolean = false,
    ued: Boolean = false,
    uec: Boolean = false,
    priority: Int = 0,
    addressType: Int = 1,
    address: BigInt = 0x1000
  ): Unit = {
    clearHardware(dut, dut.io.hwWrite.length)
    val e = dut.io.hwWrite(record)
    e.valid.poke(true.B)
    e.ce.poke(ce.B)
    e.ued.poke(ued.B)
    e.uec.poke(uec.B)
    e.priority.poke(priority.U)
    e.addressInfoType.poke(addressType.U)
    e.address.poke(address.U)
    e.transactionType.poke(4.U)
    dut.clock.step()
    clearHardware(dut, dut.io.hwWrite.length)
  }

  behavior of "RERI header"
  it should "hold reset values and ignore writes" in {
    implicit val p = new DefaultConfig
    val param = ErrorRecordInterfaceParam()
    simulate(new ErrorRecordInterface(param)) { dut =>
      resetDut(dut, param.numRecordInfo)
      assert(readReg(dut, 0x00) == RERIVendorId.Reset)
      assert(readReg(dut, 0x08) == ((1L << 56) | (param.numRecordInfo.toLong << 16)))
      assert(readReg(dut, 0x10) == 1)
      writeReg(dut, 0x00, BigInt("ffffffffffffffff", 16))
      writeReg(dut, 0x08, BigInt("ffffffffffffffff", 16))
      writeReg(dut, 0x10, BigInt("ffffffffffffffff", 16))
      assert(readReg(dut, 0x00) == RERIVendorId.Reset)
      assert(readReg(dut, 0x08) == ((1L << 56) | (param.numRecordInfo.toLong << 16)))
      assert(readReg(dut, 0x10) == 1)
      assert(readReg(dut, 0x38) == 0)
    }
  }

  behavior of "RERI record addressing"
  it should "keep records independent and accept simultaneous hardware writes" in {
    implicit val p = new DefaultConfig
    val param = ErrorRecordInterfaceParam()
    simulate(new ErrorRecordInterface(param)) { dut =>
      resetDut(dut, param.numRecordInfo)
      writeReg(dut, addr(1, Status), 1 << 1)
      assert(readReg(dut, addr(0, Status)) == 0)
      assert((readReg(dut, addr(1, Status)) & (1 << 1)) != 0)

      clearHardware(dut, param.numRecordInfo)
      dut.io.hwWrite(0).valid.poke(true.B)
      dut.io.hwWrite(0).ce.poke(true.B)
      dut.io.hwWrite(1).valid.poke(true.B)
      dut.io.hwWrite(1).ued.poke(true.B)
      dut.clock.step()
      clearHardware(dut, param.numRecordInfo)
      assert((readReg(dut, addr(0, Status)) & (1 << 1)) != 0)
      assert((readReg(dut, addr(1, Status)) & (1 << 2)) != 0)
    }
  }

  it should "give software writes priority over same-cycle hardware writes" in {
    implicit val p = new DefaultConfig
    val param = ErrorRecordInterfaceParam()
    simulate(new ErrorRecordInterface(param)) { dut =>
      resetDut(dut, param.numRecordInfo)
      clearHardware(dut, param.numRecordInfo)
      dut.io.hwWrite(0).valid.poke(true.B)
      dut.io.hwWrite(0).uec.poke(true.B)
      dut.io.reg.addr.poke(addr(0, Status).U)
      dut.io.reg.writeData.poke((1 << 1).U)
      dut.io.reg.write.poke(true.B)
      dut.clock.step()
      dut.io.reg.write.poke(false.B)
      clearHardware(dut, param.numRecordInfo)
      val status = readReg(dut, addr(0, Status))
      assert((status & (1 << 1)) != 0)
      assert((status & (1 << 3)) == 0)
    }
  }

  it should "make EID visible after its delay while hardware events continue" in {
    implicit val p = new DefaultConfig
    val param = ErrorRecordInterfaceParam()
    simulate(new ErrorRecordInterface(param)) { dut =>
      resetDut(dut, param.numRecordInfo)
      writeReg(dut, addr(0, Status), 1 << 1)
      writeReg(dut, addr(0, Control), (BigInt(2) << 32) | 1)
      dut.clock.step(2)
      assert((readReg(dut, addr(0, Status)) & 1) != 0)

      hardwareError(dut, 0, uec = true)
      assert((readReg(dut, addr(0, Status)) & (1 << 3)) != 0)
    }
  }

  behavior of "RERI field permissions"
  it should "return zero and ignore writes for disabled optional fields" in {
    implicit val p = new DefaultConfig
    val param = ErrorRecordInterfaceParam()
    simulate(new ErrorRecordInterface(param)) { dut =>
      resetDut(dut, param.numRecordInfo)
      writeReg(dut, addr(0, Information), 0x1234)
      writeReg(dut, addr(0, SupplementalInformation), 0x5678)
      assert(readReg(dut, addr(0, Information)) == 0)
      assert(readReg(dut, addr(0, SupplementalInformation)) == 0)
    }
  }

  it should "allow enabled optional fields while v is zero" in {
    implicit val p = new DefaultConfig
    val param = ErrorRecordInterfaceParam(
      recordParams = Seq(ErrorRecordInfoParam(
        "enabled", ErrorRecordType.CacheBlockDataError,
        infoEnable = true, supplInfoEnable = true)))
    simulate(new ErrorRecordInterface(param)) { dut =>
      resetDut(dut, 1)
      writeReg(dut, addr(0, Information), 0x1234)
      writeReg(dut, addr(0, SupplementalInformation), 0x5678)
      assert(readReg(dut, addr(0, Information)) == 0x1234)
      assert(readReg(dut, addr(0, SupplementalInformation)) == 0x5678)
    }
  }

  it should "ignore software status writes after v becomes one" in {
    implicit val p = new DefaultConfig
    val param = ErrorRecordInterfaceParam()
    simulate(new ErrorRecordInterface(param)) { dut =>
      resetDut(dut, param.numRecordInfo)
      hardwareError(dut, 0, ce = true)
      val oldStatus = readReg(dut, addr(0, Status))
      writeReg(dut, addr(0, Status), 1 << 2)
      assert(readReg(dut, addr(0, Status)) == oldStatus)
    }
  }

  behavior of "RERI severity and counters"
  it should "apply severity and priority overwrite rules" in {
    implicit val p = new DefaultConfig
    val param = ErrorRecordInterfaceParam()
    simulate(new ErrorRecordInterface(param)) { dut =>
      resetDut(dut, param.numRecordInfo)
      hardwareError(dut, 0, ce = true, priority = 2)
      hardwareError(dut, 0, ce = true, priority = 1)
      assert((readReg(dut, addr(0, Status)) & (1 << 6)) != 0)

      hardwareError(dut, 0, uec = true, priority = 0)
      val status = readReg(dut, addr(0, Status))
      assert((status & (1 << 3)) != 0)
      assert((status & (1 << 1)) != 0)
      assert((status & 1) != 0)
    }
  }

  it should "count CE and report corrected-error overflow" in {
    implicit val p = new DefaultConfig
    val param = ErrorRecordInterfaceParam()
    simulate(new ErrorRecordInterface(param)) { dut =>
      resetDut(dut, param.numRecordInfo)
      writeReg(dut, addr(0, Control), 1 | (1 << 1))
      writeReg(dut, addr(0, Status), (BigInt(0xffff) << 48))
      hardwareError(dut, 0, ce = true)
      val status = readReg(dut, addr(0, Status))
      assert(((status >> 48) & 0xffff) == 0)
      assert((status & (1 << 21)) != 0)
    }
  }

  it should "implement srdp and sinv commands" in {
    implicit val p = new DefaultConfig
    val param = ErrorRecordInterfaceParam()
    simulate(new ErrorRecordInterface(param)) { dut =>
      resetDut(dut, param.numRecordInfo)
      hardwareError(dut, 0, ce = true)
      writeReg(dut, addr(0, Control), BigInt(1) << 48)
      assert((readReg(dut, addr(0, Status)) & 1) == 0)
      writeReg(dut, addr(0, Control), BigInt(1) << 49)
      assert((readReg(dut, addr(0, Status)) & (1 << 23)) != 0)
    }
  }
}
