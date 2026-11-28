package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.Decoupled
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import top.DefaultConfig
import xiangshan.{CSROpType, XSCoreParamsKey, XSTileKey}
import xiangshan.backend.regfile.IntPregParams

class MinstretTestHarness(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new NewCSRInput))
    val out = Decoupled(new NewCSROutput)
    val commitInstNumValid = Input(Bool())
    val commitInstNumBits = Input(UInt(7.W))
    val rdataOut = Output(UInt(64.W))
  })

  val dut = Module(new NewCSR)

  dut.io.fromTop.hartId := 0.U
  dut.io.fromTop.clintTime.valid := false.B
  dut.io.fromTop.clintTime.bits := 0.U
  dut.io.fromTop.l2FlushDone := false.B
  dut.io.fromTop.criticalErrorState := false.B
  dut.io.fromTop.reset_mtvec.foreach(_ := 0.U)
  dut.platformIRP := 0.U.asTypeOf(dut.platformIRP)
  dut.nonMaskableIRP := 0.U.asTypeOf(dut.nonMaskableIRP)
  dut.fromAIA := 0.U.asTypeOf(dut.fromAIA)
  dut.io.trapInst.valid := false.B
  dut.io.trapInst.bits := 0.U
  dut.io.fromMem := 0.U.asTypeOf(dut.io.fromMem)
  dut.io.fromRob := 0.U.asTypeOf(dut.io.fromRob)
  dut.io.fromRob.commit.instNum.valid := io.commitInstNumValid
  dut.io.fromRob.commit.instNum.bits := io.commitInstNumBits
  dut.io.fromVecExcpMod.busy := false.B
  dut.io.perf := 0.U.asTypeOf(dut.io.perf)
  dut.io.fetchMalTval := 0.U
  dut.io.fromRename.diffVl.foreach(_ := 0.U)
  dut.io.oldSatpMode := 0.U.asTypeOf(dut.io.oldSatpMode)
  dut.io.oldVsatpMode := 0.U.asTypeOf(dut.io.oldVsatpMode)
  dut.io.oldPrivState := 0.U.asTypeOf(dut.io.oldPrivState)

  dut.io.in <> io.in
  io.out <> dut.io.out
  io.rdataOut := dut.io.out.bits.rData
}

class MinstretRetireCountTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  private val defaultConfig = new DefaultConfig
  implicit val config: Parameters = defaultConfig.alterPartial({
    case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy(
      intPreg = IntPregParams(
        numEntries = 64,
        numBank = 4,
        numRead = Some(14),
        numWrite = Some(8),
      ),
    )
  })

  behavior of "minstret post-write retirement counting"

  private def stepUntil(dut: MinstretTestHarness, hint: String, maxCycles: Int = 20)(cond: => Boolean): Unit = {
    var cycles = 0
    while (!cond && cycles < maxCycles) {
      dut.clock.step()
      cycles += 1
    }
    withClue(hint) {
      cond shouldBe true
    }
  }

  private def driveNoTrapDefaults(dut: MinstretTestHarness): Unit = {
    dut.io.out.ready.poke(true.B)
    dut.io.in.valid.poke(false.B)
    dut.io.in.bits.wen.poke(false.B)
    dut.io.in.bits.ren.poke(false.B)
    dut.io.in.bits.op.poke(CSROpType.getCSROp(CSROpType.wrt))
    dut.io.in.bits.addr.poke(0.U)
    dut.io.in.bits.src.poke(0.U)
    dut.io.in.bits.wdata.poke(0.U)
    dut.io.in.bits.mnret.poke(false.B)
    dut.io.in.bits.mret.poke(false.B)
    dut.io.in.bits.sret.poke(false.B)
    dut.io.in.bits.dret.poke(false.B)
    dut.io.in.bits.redirectFlush.poke(false.B)
    dut.io.commitInstNumValid.poke(false.B)
    dut.io.commitInstNumBits.poke(0.U)
  }

  private def writeCSR(dut: MinstretTestHarness, addr: BigInt, value: BigInt): Unit = {
    dut.io.in.valid.poke(true.B)
    dut.io.in.bits.wen.poke(true.B)
    dut.io.in.bits.addr.poke(addr.U)
    dut.io.in.bits.wdata.poke(value.U)
    stepUntil(dut, "write request was not accepted") {
      dut.io.in.ready.peek().litToBoolean
    }
    dut.clock.step()
    dut.clock.step()
    dut.io.in.valid.poke(false.B)
    dut.io.in.bits.wen.poke(false.B)
  }

  private def readCSR(dut: MinstretTestHarness, addr: BigInt): BigInt = {
    dut.io.out.ready.poke(false.B)
    dut.io.in.valid.poke(true.B)
    dut.io.in.bits.ren.poke(true.B)
    dut.io.in.bits.addr.poke(addr.U)
    stepUntil(dut, "read request was not accepted") {
      dut.io.in.ready.peek().litToBoolean
    }
    dut.clock.step()
    dut.io.in.valid.poke(false.B)
    dut.io.in.bits.ren.poke(false.B)
    stepUntil(dut, "read response did not become valid") {
      dut.io.out.valid.peek().litToBoolean
    }
    val data = dut.io.out.bits.rData.peek().litValue
    dut.io.out.ready.poke(true.B)
    dut.clock.step()
    data
  }

  it should "not count the writing instruction itself on csrw minstret" in {
    test(new MinstretTestHarness()(config)) { dut =>
      driveNoTrapDefaults(dut)
      dut.clock.step(2)

      val minstretAddr = BigInt("b02", 16) // CSR 0xb02 = minstret

      // 1. Explicitly write 0x1000 to minstret
      writeCSR(dut, minstretAddr, BigInt("1000", 16))
      dut.clock.step(4)

      // 2. The csrw instruction itself commits at ROB (instNum = 1)
      dut.io.commitInstNumValid.poke(true.B)
      dut.io.commitInstNumBits.poke(1.U)
      dut.clock.step()
      dut.io.commitInstNumValid.poke(false.B)
      dut.io.commitInstNumBits.poke(0.U)
      dut.clock.step(2)

      // 3. Post-write read of minstret must observe 0x1000 (not 0x1001)
      readCSR(dut, minstretAddr) shouldBe BigInt("1000", 16)

      // 4. Subsequent instruction commits (instNum = 1)
      dut.io.commitInstNumValid.poke(true.B)
      dut.io.commitInstNumBits.poke(1.U)
      dut.clock.step()
      dut.io.commitInstNumValid.poke(false.B)
      dut.io.commitInstNumBits.poke(0.U)
      dut.clock.step()

      // 5. Subsequent read must observe 0x1001
      readCSR(dut, minstretAddr) shouldBe BigInt("1001", 16)
    }
  }
}
