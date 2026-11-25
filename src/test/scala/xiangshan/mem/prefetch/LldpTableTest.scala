package xiangshan.mem.prefetch

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class LldpTableTest extends AnyFlatSpec with ChiselScalatestTester {
  private def init(dut: LldpTable): Unit = {
    dut.io.train.valid.poke(false.B)
    dut.io.lookup.valid.poke(false.B)
    dut.io.returned.valid.poke(false.B)
    dut.io.role.ready.poke(true.B)
    dut.io.candidate.ready.poke(false.B)
  }

  private def train(dut: LldpTable, producer: Int, consumer: Int, length: Int = 2): Unit = {
    val sample = dut.io.train
    sample.bits.context.poke(1.U)
    sample.bits.dependence.producer.poke(producer.U)
    sample.bits.dependence.consumer.poke(consumer.U)
    sample.bits.dependence.descriptor.length.poke(length.U)
    sample.bits.dependence.descriptor.displacement.poke((-16).S)
    for (step <- sample.bits.dependence.descriptor.steps) {
      step.op.poke(LldpReplayOp.Add)
      step.immediate.poke((-16).S)
      step.word.poke(false.B)
    }
    sample.bits.dependence.consumerType.size.poke(3.U)
    sample.bits.dependence.consumerType.sext.poke(false.B)
    sample.bits.dependence.consumerType.floating.poke(false.B)
    sample.valid.poke(true.B)
    sample.ready.expect(true.B)
    dut.clock.step()
    sample.valid.poke(false.B)
    dut.clock.step()
  }

  private def lookup(dut: LldpTable, producer: Int, source: LldpValueSource.Type, depth: Boolean): Unit = {
    dut.io.lookup.valid.poke(true.B)
    dut.io.lookup.bits.pc.poke(producer.U)
    dut.io.lookup.bits.context.poke(1.U)
    dut.io.lookup.bits.offset.poke(16.U)
    dut.io.lookup.bits.dataType.size.poke(3.U)
    dut.io.lookup.bits.dataType.sext.poke(false.B)
    dut.io.lookup.bits.dataType.floating.poke(false.B)
    dut.io.lookup.bits.source.poke(source)
    dut.io.lookup.bits.depth.poke(depth.B)
  }

  private def saveHint(dut: LldpTable, source: LldpValueSource.Type): Unit = {
    val hint = dut.io.returned.bits.hint
    dut.io.hint.valid.expect(true.B)
    hint.row.poke(dut.io.hint.bits.row.peek())
    hint.generation.poke(dut.io.hint.bits.generation.peek())
    hint.offset.poke(dut.io.hint.bits.offset.peek())
    hint.dataType.size.poke(dut.io.hint.bits.dataType.size.peek())
    hint.dataType.sext.poke(false.B)
    hint.dataType.floating.poke(false.B)
    hint.source.poke(source)
    dut.clock.step()
    dut.io.lookup.valid.poke(false.B)
  }

  private def returnBytes(dut: LldpTable): Unit = {
    dut.io.returned.bits.bytes.poke(0x1080.U)
    dut.io.returned.valid.poke(true.B)
    dut.io.returned.ready.expect(true.B)
    dut.clock.step()
    dut.io.returned.valid.poke(false.B)
    dut.clock.step(2)
  }

  it should "send separate roles and replay each eligible source only after three matching samples" in {
    test(new LldpTable(20, 4, rows = 2)) { dut =>
      init(dut)
      train(dut, 16, 32)
      dut.io.role.valid.expect(true.B)
      dut.io.role.bits.pc.expect(16.U)
      dut.io.role.bits.roles.expect(1.U)
      dut.clock.step()
      dut.io.role.bits.pc.expect(32.U)
      dut.io.role.bits.roles.expect(2.U)
      dut.clock.step()
      lookup(dut, 16, LldpValueSource.Demand, depth = false)
      saveHint(dut, LldpValueSource.Demand)
      returnBytes(dut)
      dut.io.candidate.valid.expect(false.B)
      for (_ <- 0 until 2) {
        train(dut, 16, 32)
        dut.clock.step(2)
      }
      for (source <- Seq(LldpValueSource.Demand, LldpValueSource.Spatial, LldpValueSource.Chain)) {
        lookup(dut, 16, source, depth = false)
        saveHint(dut, source)
        returnBytes(dut)
        dut.io.candidate.valid.expect(true.B)
        dut.io.candidate.bits.vaddr.expect(0x1060.U)
        dut.io.candidate.bits.source.expect(source)
        dut.io.candidate.ready.poke(true.B)
        dut.clock.step()
        dut.io.candidate.ready.poke(false.B)
      }
      lookup(dut, 16, LldpValueSource.Chain, depth = true)
      dut.io.hint.valid.expect(false.B)
    }
  }

  it should "reject stale returned hints and keep dependency-only role updates" in {
    test(new LldpTable(20, 4, rows = 2)) { dut =>
      init(dut)
      for (_ <- 0 until 3) {
        train(dut, 16, 32)
        dut.clock.step(2)
      }
      lookup(dut, 16, LldpValueSource.Demand, depth = false)
      saveHint(dut, LldpValueSource.Demand)
      train(dut, 48, 48, length = 0)
      dut.io.role.bits.roles.expect(3.U)
      dut.clock.step(2)
      train(dut, 64, 80)
      dut.clock.step(2)
      returnBytes(dut)
      dut.io.candidate.valid.expect(false.B)
    }
  }
}
