/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

// See LICENSE.SiFive for license details.

package bus.tilelink

import chisel3._
import chisel3.util._
import utils.Or

object TLArbiter
{
  // (valids, select) => readys
  type Policy = (Int, UInt, Bool) => UInt

  val lowestIndexFirst: Policy = (width, valids, select) => ~(Or.leftOR(valids) << 1)(width-1, 0)

  val roundRobin: Policy = (width, valids, select) => if (width == 1) 1.U(1.W) else {
    val valid = valids(width-1, 0)
    assert (valid === valids)
    val mask = RegInit(((BigInt(1) << width)-1).U(width.W))
    val filter = Cat(valid & ~mask, valid)
    val unready = (Or.rightOR(filter, width*2, width) >> 1) | (mask << width)
    val readys = ~((unready >> width) & unready(width-1, 0))
    when (select && valid.orR) {
      mask := Or.leftOR(readys & valid, width)
    }
    readys(width-1, 0)
  }

  def lowestFromSeq[T <: TLChannel](sink: DecoupledIO[T], sources: Seq[DecoupledIO[T]]) {
    apply(lowestIndexFirst)(sink, sources.map(s => (TLUtilities.numBeats1(s.bits), s)):_*)
  }

  def lowest[T <: TLChannel](sink: DecoupledIO[T], sources: DecoupledIO[T]*) {
    apply(lowestIndexFirst)(sink, sources.toList.map(s => (TLUtilities.numBeats1(s.bits), s)):_*)
  }

  def robin[T <: TLChannel](sink: DecoupledIO[T], sources: DecoupledIO[T]*) {
    apply(roundRobin)(sink, sources.toList.map(s => (TLUtilities.numBeats1(s.bits), s)):_*)
  }

  def apply[T <: Data](policy: Policy)(sink: DecoupledIO[T], sources: (UInt, DecoupledIO[T])*) {
    if (sources.isEmpty) {
      sink.valid := false.B
    } else if (sources.size == 1) {
      sink <> sources.head._2
    } else {
      val pairs = sources.toList
      val beatsIn = pairs.map(_._1)
      val sourcesIn = pairs.map(_._2)

      // The number of beats which remain to be sent
      val beatsLeft = RegInit(0.U)
      val idle = beatsLeft === 0.U
      val latch = idle && sink.ready // winner (if any) claims sink

      // Who wants access to the sink?
      val valids = sourcesIn.map(_.valid)
      // Arbitrate amongst the requests
      val readys = VecInit(policy(valids.size, Cat(valids.reverse), latch).asBools)
      // Which request wins arbitration?
      val winner = VecInit((readys zip valids) map { case (r,v) => r&&v })

      // Confirm the policy works properly
      require (readys.size == valids.size)
      // Never two winners
      val prefixOR = winner.scanLeft(false.B)(_||_).init
      assert((prefixOR zip winner) map { case (p,w) => !p || !w } reduce {_ && _})
      // If there was any request, there is a winner
      assert (!valids.reduce(_||_) || winner.reduce(_||_))

      // Track remaining beats
      val maskedBeats = (winner zip beatsIn) map { case (w,b) => Mux(w, b, 0.U) }
      val initBeats = maskedBeats.reduce(_ | _) // no winner => 0 beats
      beatsLeft := Mux(latch, initBeats, beatsLeft - sink.fire())

      // The one-hot source granted access in the previous cycle
      val state = RegInit(VecInit(Seq.fill(sources.size)(false.B)))
      val muxState = Mux(idle, winner, state)
      state := muxState

      val allowed = Mux(idle, readys, state)
      (sourcesIn zip allowed) foreach { case (s, r) =>
        s.ready := sink.ready && r
      }
      sink.valid := Mux(idle, valids.reduce(_||_), Mux1H(state, valids))
      sink.bits := Mux1H(muxState, sourcesIn.map(_.bits))
    }
  }
}

/** Synthesizeable unit tests */
/*
import freechips.rocketchip.unittest._

class TestRobin(txns: Int = 128, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val sources = Wire(Vec(6, DecoupledIO(UInt(width=3))))
  val sink = Wire(DecoupledIO(UInt(width=3)))
  val count = RegInit(0.U(8.W))

  val lfsr = LFSR(16, Bool(true))
  val valid = lfsr(0)
  val ready = lfsr(15)

  sources.zipWithIndex.map { case (z, i) => z.bits := i.U }
  sources(0).valid := valid
  sources(1).valid := false.B
  sources(2).valid := valid
  sources(3).valid := valid
  sources(4).valid := false.B
  sources(5).valid := valid
  sink.ready := ready

  TLArbiter(TLArbiter.roundRobin)(sink, sources.zipWithIndex.map { case (z, i) => (i.U, z) }:_*)
  when (sink.fire()) { printf("TestRobin: %d\n", sink.bits) }
  when (!sink.fire()) { printf("TestRobin: idle (%d %d)\n", valid, ready) }

  count := count + 1.U
  io.finished := count >= txns.U
}
*/
