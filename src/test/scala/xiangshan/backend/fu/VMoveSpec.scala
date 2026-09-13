package xiangshan.backend.fu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.backend.fu.wrapper.VMove
import xiangshan.backend.vector.fu.VecFuConfig
import yunsuan.encoding.Opcode.Opcodes.VMoveOpcode

class VMoveTestTop(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val opcode = Input(UInt(VMoveOpcode.getWidth.W))
    val vs2 = Input(UInt(128.W))
    val fpWen = Input(Bool())
    val rfWen = Input(Bool())
    val vecWen = Input(Bool())
    val validOut = Output(Bool())
    val fpWenOut = Output(Bool())
    val rfWenOut = Output(Bool())
    val fp = Output(UInt(64.W))
    val int = Output(UInt(64.W))
    val vec = Output(UInt(128.W))
  })

  private val dut = Module(new VMove(VecFuConfig.VmoveCfg))
  dut.in := 0.U.asTypeOf(dut.in)
  dut.in.ex(0).valid := io.valid
  dut.in.ex(0).bits.ctrl.opcode := io.opcode
  dut.in.ex(0).bits.ctrl.fpWen.get := io.fpWen
  dut.in.ex(0).bits.ctrl.rfWen.get := io.rfWen
  dut.in.ex(0).bits.ctrl.vecWen.get := io.vecWen
  dut.in.ex(0).bits.ctrl.vm.get := true.B
  dut.in.ex(0).bits.data.src(1) := io.vs2
  io.validOut := dut.out.ex(0).valid
  io.fpWenOut := dut.out.ex(0).bits.ctrl.fpWen.get
  io.rfWenOut := dut.out.ex(0).bits.ctrl.rfWen.get
  io.fp := dut.out.ex(0).bits.data.fp.get
  io.int := dut.out.ex(0).bits.data.int.get
  io.vec := dut.out.ex(0).bits.data.vec.get.normal
}

class VMoveSpec extends AnyFlatSpec with ChiselSim {
  private val base = new DefaultConfig
  private val config = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
  }

  behavior of "VMove"

  it should "NaN-box floating scalar results without changing integer moves" in {
    simulate(new VMoveTestTop()(config)) { dut =>
      val mask64 = (BigInt(1) << 64) - 1
      val poison = BigInt("a5a55a5a123456789abcdef0", 16)
      val ops = Seq(
        8 -> VMoveOpcode.vmv_vs2x_e8,
        16 -> VMoveOpcode.vmv_vs2x_e16,
        32 -> VMoveOpcode.vmv_vs2x_e32,
        64 -> VMoveOpcode.vmv_vs2x_e64
      )
      val fpPatterns = Map(
        16 -> Seq("3c00", "7c00", "fc00", "7e01", "7c01"),
        32 -> Seq("3f800000", "7f800000", "ff800000", "7fc00001", "7f800001"),
        64 -> Seq("3ff0000000000000", "7ff0000000000000", "fff0000000000000", "7ff8000000000001")
      )
      dut.io.valid.poke(true.B)
      dut.io.vecWen.poke(false.B)
      for ((sew, op) <- ops) {
        val mask = (BigInt(1) << sew) - 1
        val sign = BigInt(1) << (sew - 1)
        val values = Seq(BigInt(0), BigInt(1), sign - 1, sign, sign + 1, mask) ++
          fpPatterns.getOrElse(sew, Seq.empty).map(BigInt(_, 16))
        dut.io.opcode.poke(op.encode.value)
        for (value <- values; upper <- Seq(BigInt(0), poison)) {
          val input = ((upper << sew) | value) & ((BigInt(1) << 128) - 1)
          dut.io.vs2.poke(input.U(128.W))
          dut.io.rfWen.poke(true.B)
          dut.io.fpWen.poke(false.B)
          dut.io.validOut.expect(true.B)
          dut.io.rfWenOut.expect(true.B)
          dut.io.fpWenOut.expect(false.B)
          val signed = if ((value & sign) != 0) value | (mask64 ^ mask) else value
          dut.io.int.expect(signed.U(64.W))
          if (sew >= 16) {
            dut.io.rfWen.poke(false.B)
            dut.io.fpWen.poke(true.B)
            dut.io.fpWenOut.expect(true.B)
            dut.io.rfWenOut.expect(false.B)
            dut.io.fp.expect((value | (mask64 ^ mask)).U(64.W))
          }
        }
      }
      dut.io.opcode.poke(VMoveOpcode.vmvnr.encode.value)
      dut.io.vs2.poke(poison.U(128.W))
      dut.io.rfWen.poke(false.B)
      dut.io.fpWen.poke(false.B)
      dut.io.vecWen.poke(true.B)
      dut.io.vec.expect(poison.U(128.W))
      dut.io.valid.poke(false.B)
      dut.io.validOut.expect(false.B)
    }
  }
}
