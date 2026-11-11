// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE for details.
package cache

import chisel3._
import chisel3.util._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.cache.MissQueuePC

class MissQueuePCHarness extends Module {
  val io = IO(new Bundle {
    val valid = Input(Vec(4, Bool()))
    val pc = Input(Vec(4, UInt(50.W)))
    val selected = Output(Valid(UInt(50.W)))
    val canonical = Output(UInt(64.W))
  })
  val (valid, pc) = MissQueuePC.select(io.valid.toSeq, io.pc.toSeq)
  io.selected.valid := valid
  io.selected.bits := pc
  io.canonical := MissQueuePC.canonical(pc, 64)
}

class SDBPMissQueuePCTest extends AnyFlatSpec {
  behavior of "MissQueue demand PC selection"
  it should "retain the first valid PC across compressed and same-cycle merges, including PC zero" in {
    simulate(new MissQueuePCHarness) { dut =>
      for (mask <- 0 until 16) {
        for (i <- 0 until 4) {
          dut.io.valid(i).poke(((mask & (1 << i)) != 0).B)
          dut.io.pc(i).poke((if (i == 0) 0 else 0x80000000L + i).U)
        }
        dut.io.selected.valid.expect((mask != 0).B)
        if (mask != 0) {
          val first = (0 until 4).find(i => (mask & (1 << i)) != 0).get
          dut.io.selected.bits.expect((if (first == 0) 0 else 0x80000000L + first).U)
        }
      }
      dut.io.valid.foreach(_.poke(false.B))
      dut.io.valid(0).poke(true.B)
      dut.io.pc(0).poke(((BigInt(1) << 50) - 1).U)
      dut.io.canonical.expect(((BigInt(1) << 64) - 1).U)
    }
  }
}
