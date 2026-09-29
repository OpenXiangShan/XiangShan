package xiangshan.backend.fu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import xiangshan.{XSCoreParameters, XSCoreParamsKey}
import xiangshan.backend.decode.VTypeGen
import xiangshan.backend.fu.vector.Bundles.VType

class VTypeGenSpec extends AnyFlatSpec with ChiselSim {
  private val config = (new DefaultConfig).alterPartial {
    case XSCoreParamsKey => XSCoreParameters()
  }

  private def vsetivli(rd: Int, avl: Int, vtype: Int): BigInt =
    BigInt("c0007057", 16) | (BigInt(vtype) << 20) | (BigInt(avl) << 15) | (BigInt(rd) << 7)

  private def vsetvli(rd: Int, rs1: Int, vtype: Int): BigInt =
    BigInt("00007057", 16) | (BigInt(vtype) << 20) | (BigInt(rs1) << 15) | (BigInt(rd) << 7)

  behavior of "VTypeGen"

  it should "distinguish immediate AVL from keep-VL across lanes and packets" in {
    simulate(new VTypeGen()(config)) { dut =>
      val width = dut.in.insts.length

      def pokeType(target: VType, value: Int, illegal: Boolean = false): Unit = {
        target.illegal.poke(illegal.B)
        target.vma.poke(((value & 0x80) != 0).B)
        target.vta.poke(((value & 0x40) != 0).B)
        target.vsew.poke(((value >> 3) & 7).U)
        target.vlmul.poke((value & 7).U)
      }

      def expectType(target: VType, value: Int, illegal: Boolean = false): Unit = {
        target.illegal.expect(illegal.B)
        target.vma.expect(((value & 0x80) != 0).B)
        target.vta.expect(((value & 0x40) != 0).B)
        target.vsew.expect(((value >> 3) & 7).U)
        target.vlmul.expect((value & 7).U)
      }

      def packet(insts: (Int, BigInt)*): Unit = {
        for (lane <- 0 until width) {
          dut.in.insts(lane).valid.poke(false.B)
          dut.in.insts(lane).bits.poke(0x13.U)
        }
        for ((lane, inst) <- insts) {
          dut.in.insts(lane).valid.poke(true.B)
          dut.in.insts(lane).bits.poke(inst.U(32.W))
        }
      }

      def seed(vtype: Int): Unit = {
        dut.in.canUpdateVType.poke(true.B)
        packet(0 -> vsetivli(5, 1, vtype))
        expectType(dut.out.vtype(width - 1), vtype)
        dut.clock.step()
        packet()
      }

      dut.in.walkToArchVType.poke(false.B)
      dut.in.walkVType.valid.poke(false.B)
      pokeType(dut.in.walkVType.bits, 0)
      pokeType(dut.in.vsetvlVType, 0)
      dut.in.commitVType.hasVsetvl.poke(false.B)
      dut.in.commitVType.vtype.valid.poke(false.B)
      pokeType(dut.in.commitVType.vtype.bits, 0)
      dut.in.canUpdateVType.poke(true.B)
      packet()

      val legalTypes = Seq(0x00, 0x08, 0x10, 0x18, 0x09, 0x05, 0xc8)
      for {
        oldType <- legalTypes
        newType <- legalTypes
        rd <- Seq(0, 5)
        avl <- Seq(0, 1, 31)
        lane <- 0 until width
      } {
        withClue(s"old=$oldType new=$newType rd=$rd avl=$avl lane=$lane: ") {
          seed(oldType)
          packet(lane -> vsetivli(rd, avl, newType))
          for (i <- 0 until width) {
            expectType(dut.out.specvtype(i), if (i <= lane) oldType else newType)
            expectType(dut.out.vtype(i), if (i < lane) oldType else newType)
          }
          dut.clock.step()
          packet()
          expectType(dut.out.specvtype(0), newType)
        }
      }

      for (lane <- 1 until width) {
        seed(0x18)
        packet(0 -> vsetivli(5, 1, 0x00), lane -> vsetivli(0, 0, 0x08))
        expectType(dut.out.specvtype(lane), 0x00)
        expectType(dut.out.vtype(lane), 0x08)
        expectType(dut.out.vtype(width - 1), 0x08)
        dut.clock.step()
      }

      seed(0x00)
      packet(0 -> vsetivli(0, 0, 0x08))
      dut.in.canUpdateVType.poke(false.B)
      expectType(dut.out.vtype(0), 0x08)
      dut.clock.step()
      expectType(dut.out.specvtype(0), 0x00)
      dut.in.canUpdateVType.poke(true.B)
      dut.clock.step()
      packet()
      expectType(dut.out.specvtype(0), 0x08)

      seed(0x00)
      packet()
      dut.in.insts(0).bits.poke(vsetivli(0, 0, 0x08).U(32.W))
      expectType(dut.out.vtype(width - 1), 0x00)
      dut.clock.step()
      expectType(dut.out.specvtype(0), 0x00)

      packet(0 -> vsetvli(0, 0, 0x09))
      expectType(dut.out.vtype(0), 0x09)
      dut.clock.step()
      packet()
      expectType(dut.out.specvtype(0), 0x09)

      seed(0x18)
      packet(0 -> vsetivli(5, 1, 0x00), 1 -> vsetvli(0, 0, 0x09))
      expectType(dut.out.vtype(1), 0x09)
      dut.clock.step()

      seed(0x00)
      packet(0 -> vsetvli(0, 0, 0x08))
      dut.out.vtype(0).illegal.expect(true.B)

      seed(0x00)
      packet(0 -> vsetivli(0, 0, 0x100))
      expectType(dut.out.vtype(0), 0x00, illegal = true)
    }
  }
}
