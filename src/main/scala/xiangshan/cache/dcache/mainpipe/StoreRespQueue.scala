/***************************************************************************************
 * Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND.
 ***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class StoreResult(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(PBCreditBits.W)
  val miss = Bool()
  val replay = Bool()
}

class StoreRespQueue(implicit p: Parameters) extends DCacheModule {
  val n = PBEntries + 3
  val io = IO(new Bundle {
    val alloc = Flipped(Decoupled(UInt(reqIdWidth.W)))
    val idx = Output(UInt(PBCreditBits.W))
    val put = Flipped(Vec(2, Valid(new StoreResult)))
    val resp = Decoupled(new DCacheLineResp)
  })
  val valid = RegInit(VecInit(Seq.fill(n)(false.B)))
  val ready = RegInit(VecInit(Seq.fill(n)(false.B)))
  val ids = Reg(Vec(n, UInt(reqIdWidth.W)))
  val result = Reg(Vec(n, new StoreResult))
  val arb = Module(new RRArbiter(new DCacheLineResp, n))
  io.alloc.ready := !valid.asUInt.andR
  io.idx := PriorityEncoder(~valid.asUInt)
  for (i <- 0 until n) {
    val alloc = io.alloc.fire && io.idx === i.U
    val puts = io.put.map(p => p.valid && p.bits.idx === i.U)
    arb.io.in(i).valid := ready(i)
    arb.io.in(i).bits.data := 0.U
    arb.io.in(i).bits.id := ids(i)
    arb.io.in(i).bits.miss := result(i).miss
    arb.io.in(i).bits.replay := result(i).replay
    when (alloc) {
      valid(i) := true.B
      ids(i) := io.alloc.bits
    }
    when (puts.reduce(_ || _)) {
      assert(valid(i) && !ready(i), "Store response has no outstanding credit")
      ready(i) := true.B
      result(i) := Mux1H(puts, io.put.map(_.bits))
    }
    when (arb.io.in(i).fire) {
      valid(i) := false.B
      ready(i) := false.B
    }
    assert(PopCount(VecInit(puts)) <= 1.U)
    assert(!ready(i) || valid(i))
  }
  io.put.foreach(p => assert(!p.valid || p.bits.idx < n.U))
  io.resp <> arb.io.out
}
