/***************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
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

package xiangshan.backend.rob

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import utils._
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.{DynInst, ExceptionInfo, ExuOutput}
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.frontend.FtqPtr
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import xiangshan.backend.Bundles.{DynInst, ExceptionInfo, ExuOutput}
import xiangshan.backend.ctrlblock.{DebugLSIO, DebugLsInfo, LsTopdownInfo}
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.rename.SnapshotGenerator

class ExceptionGen(params: BackendParams)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))
    val flush = Input(Bool())
    val enq = Vec(RenameWidth, Flipped(ValidIO(new RobExceptionInfo)))
    // csr + load + store + varith + vload + vstore
    val wb = Vec(params.numException, Flipped(ValidIO(new RobExceptionInfo)))
    val out = ValidIO(new RobExceptionInfo)
    val state = ValidIO(new RobExceptionInfo)
  })

  val wbExuParams = params.allExuParams.filter(_.exceptionOut.nonEmpty)

  def getOldest(valid: Seq[Bool], bits: Seq[RobExceptionInfo]): RobExceptionInfo = {
    def getOldest_recursion(valid: Seq[Bool], bits: Seq[RobExceptionInfo]): (Seq[Bool], Seq[RobExceptionInfo]) = {
      assert(valid.length == bits.length)
      if (valid.length == 1) {
        (valid, bits)
      } else if (valid.length == 2) {
        val res = Seq.fill(2)(Wire(ValidIO(chiselTypeOf(bits(0)))))
        for (i <- res.indices) {
          res(i).valid := valid(i)
          res(i).bits := bits(i)
        }
        val oldest = Mux(!valid(1) || valid(0) && isAfter(bits(1).robIdx, bits(0).robIdx), res(0), res(1))
        (Seq(oldest.valid), Seq(oldest.bits))
      } else {
        val left = getOldest_recursion(valid.take(valid.length / 2), bits.take(valid.length / 2))
        val right = getOldest_recursion(valid.drop(valid.length / 2), bits.drop(valid.length / 2))
        getOldest_recursion(left._1 ++ right._1, left._2 ++ right._2)
      }
    }
    getOldest_recursion(valid, bits)._2.head
  }


  val currentValid = RegInit(false.B)
  val current = Reg(new RobExceptionInfo)

  // orR the exceptionVec
  val lastCycleFlush = RegNext(io.flush)
  val in_enq_valid = VecInit(io.enq.map(e => e.valid && e.bits.has_exception && !lastCycleFlush))

  // s0: compare wb in 6 groups
  val csr_wb = io.wb.zip(wbExuParams).filter(_._2.fuConfigs.filter(t => t.isCsr).nonEmpty).map(_._1)
  val load_wb = io.wb.zip(wbExuParams).filter(_._2.fuConfigs.filter(_.fuType == FuType.ldu).nonEmpty).map(_._1)
  val store_wb = io.wb.zip(wbExuParams).filter(_._2.fuConfigs.filter(t => t.isSta || t.fuType == FuType.mou).nonEmpty).map(_._1)
  val varith_wb = io.wb.zip(wbExuParams).filter(_._2.fuConfigs.filter(_.isVecArith).nonEmpty).map(_._1)
  val vload_wb = io.wb.zip(wbExuParams).filter(_._2.fuConfigs.filter(_.fuType == FuType.vldu).nonEmpty).map(_._1)
  val vstore_wb = io.wb.zip(wbExuParams).filter(_._2.fuConfigs.filter(_.fuType == FuType.vstu).nonEmpty).map(_._1)

  val writebacks = Seq(csr_wb, load_wb, store_wb, varith_wb, vload_wb, vstore_wb)
  val in_wb_valids = writebacks.map(_.map(w => w.valid && w.bits.has_exception && !lastCycleFlush))
  val wb_valid = in_wb_valids.zip(writebacks).map { case (valid, wb) =>
    valid.zip(wb.map(_.bits)).map { case (v, bits) => v && !(bits.robIdx.needFlush(io.redirect) || io.flush) }.reduce(_ || _)
  }
  val wb_bits = in_wb_valids.zip(writebacks).map { case (valid, wb) => getOldest(valid, wb.map(_.bits))}

  val s0_out_valid = wb_valid.map(x => RegNext(x))
  val s0_out_bits = wb_bits.zip(wb_valid).map{ case(b, v) => RegEnable(b, v)}

  // s1: compare last six and current flush
  val s1_valid = VecInit(s0_out_valid.zip(s0_out_bits).map{ case (v, b) => v && !(b.robIdx.needFlush(io.redirect) || io.flush) })
  val s1_out_bits = RegEnable(getOldest(s0_out_valid, s0_out_bits), s1_valid.asUInt.orR)
  val s1_out_valid = RegNext(s1_valid.asUInt.orR)

  val enq_valid = RegNext(in_enq_valid.asUInt.orR && !io.redirect.valid && !io.flush)
  val enq_bits = RegEnable(ParallelPriorityMux(in_enq_valid, io.enq.map(_.bits)), in_enq_valid.asUInt.orR && !io.redirect.valid && !io.flush)

  // s2: compare the input exception with the current one
  // priorities:
  // (1) system reset
  // (2) current is valid: flush, remain, merge, update
  // (3) current is not valid: s1 or enq
  val current_flush = current.robIdx.needFlush(io.redirect) || io.flush
  val s1_flush = s1_out_bits.robIdx.needFlush(io.redirect) || io.flush
  when (currentValid) {
    when (current_flush) {
      currentValid := Mux(s1_flush, false.B, s1_out_valid)
    }
    when (s1_out_valid && !s1_flush) {
      when (isAfter(current.robIdx, s1_out_bits.robIdx)) {
        current := s1_out_bits
      }.elsewhen (current.robIdx === s1_out_bits.robIdx) {
        current.exceptionVec := (s1_out_bits.exceptionVec.asUInt | current.exceptionVec.asUInt).asTypeOf(ExceptionVec())
        current.flushPipe := s1_out_bits.flushPipe || current.flushPipe
        current.replayInst := s1_out_bits.replayInst || current.replayInst
        current.singleStep := s1_out_bits.singleStep || current.singleStep
        current.trigger := (s1_out_bits.trigger.asUInt | current.trigger.asUInt).asTypeOf(new TriggerCf)
      }
    }
  }.elsewhen (s1_out_valid && !s1_flush) {
    currentValid := true.B
    current := s1_out_bits
  }.elsewhen (enq_valid && !(io.redirect.valid || io.flush)) {
    currentValid := true.B
    current := enq_bits
  }

  io.out.valid   := s1_out_valid || enq_valid && enq_bits.can_writeback
  io.out.bits    := Mux(s1_out_valid, s1_out_bits, enq_bits)
  io.state.valid := currentValid
  io.state.bits  := current

}