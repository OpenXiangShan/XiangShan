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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.mem.ReplayCauseNO._
import xiangshan.mem.Bundles._
import xiangshan.cache._
import xiangshan.cache.mmu._

class ReplayCauseGenIO(implicit p: Parameters) extends XSBundle {
  val s1_in = Flipped(ValidIO(new LsPipelineBundle))
  val s2_in = Flipped(ValidIO(new LsPipelineBundle))
  val s3_in = Flipped(ValidIO(new LsPipelineBundle))

  // from
  val fromTlb = Flipped(ValidIO(new TlbResp(2)))
  val fromDCache = Input(new Bundle() {
    val mqNack = Bool()
    val miss = Bool()
    val bankConflict = Bool()
    val wayPredictFail = Bool()
  })
  val fromSta = Vec(StorePipelineWidth, Flipped(ValidIO(new StoreNukeQueryBundle)))
  val fromLsq = Input(new Bundle() {
    val addrInvalid = Bool()
    val dataInvalid = Bool()
    val rarNack = Bool()
    val rawNack = Bool()
    val misalignBufNack = Bool()
  })

  //
  val replayCauseOut = Output(ReplayCauseNO())

  // common
  val commonOut = Output(new Bundle() {
    val s2_needReplay = Bool()
    val s2_canFastReplay = Bool()
  })
}

class ReplayCauseGen(implicit p: Parameters, params: MemUnitParams) extends XSModule
  with HasCircularQueuePtrHelper {

  private def causeCatched(replayCauseVec: Vec[Bool], replayCauseNO: Int) = {
    if (params.replayCauseOut.contains(replayCauseNO)) {
      replayCauseVec(replayCauseNO)
    } else {
      false.B
    }
  }

  private def catchCause(replayCauseVec: Vec[Bool], replayCauseNO: Int, set: Bool) = {
    if (params.replayCauseOut.contains(replayCauseNO)) {
      replayCauseVec(replayCauseNO) := set
    }
  }

  private def clearAllCauses(replayCauseVec: Vec[Bool]) = {
    for (i <- replayCauseVec.indices) {
      replayCauseVec(i) := false.B
    }
  }

  val io = IO(new ReplayCauseGenIO).suggestName("io")

  // Pipeline
  // -------------------------------------------------------------------
  // stage 1
  // -------------------------------------------------------------------
  val s1_replayCauses = WireInit(0.U.asTypeOf(ReplayCauseNO()))

  val s1_nukeVec = Wire(Vec(io.fromSta.length, Bool()))
  io.fromSta.zip(s1_nukeVec).foreach {
    case (query, nuke) =>
      /**
        * Determine whether the store access is 128-bits aligned
        * either the line matches or the access is 128-bit vector
        */
      val match128Bit = query.bits.matchLine ||
        ((io.s1_in.bits.isVector || io.s1_in.bits.misalignWith16Bytes) && io.s1_in.bits.is128bit)

      /**
        * Determine whether the physical address matches (with different bit
        * ranges based on 128-bit alignment)
        */
      val nukePAddrMatch = Mux(
        match128Bit,
        io.s1_in.bits.paddr(PAddrBits - 1, 4) === query.bits.paddr(PAddrBits - 1, 4),
        io.s1_in.bits.paddr(PAddrBits - 1, 3) === query.bits.paddr(PAddrBits - 1, 3)
      )

      /**
        * Determine whether the data mask matches (i.e., the relevant bits are
        * set in both the store and the query)
        */
      val nukeDataMaskMatch = (io.s1_in.bits.mask & query.bits.mask).orR

      /**
        * Determine whether the store operation is nuked by an older entry
        * (based on the ROB index)
        */
      val nukedByOlder = isAfter(io.s1_in.bits.uop.robIdx, query.bits.robIdx)
      nuke := query.valid && nukePAddrMatch && nukeDataMaskMatch && nukedByOlder
  }

  catchCause(s1_replayCauses, ReplayCauseNO.C_NK, io.s1_in.bits.isLoad && s1_nukeVec.asUInt.orR && !io.s1_in.bits.tlbMiss)

  // Pipeline
  // -------------------------------------------------------------------
  // stage 2
  // -------------------------------------------------------------------
  val s2_replayCauseIn = RegEnable(s1_replayCauses, io.s1_in.valid)
  val s2_replayCauseOut = WireInit(s2_replayCauseIn)

  catchCause(s2_replayCauseOut, ReplayCauseNO.C_MA, io.fromLsq.addrInvalid)
  catchCause(s2_replayCauseOut, ReplayCauseNO.C_TM, io.s2_in.bits.tlbMiss)
  catchCause(s2_replayCauseOut, ReplayCauseNO.C_FF, io.fromLsq.dataInvalid)
  catchCause(s2_replayCauseOut, ReplayCauseNO.C_DR, io.fromDCache.mqNack)
  catchCause(s2_replayCauseOut, ReplayCauseNO.C_DM, io.fromDCache.miss)
  catchCause(s2_replayCauseOut, ReplayCauseNO.C_BC, io.fromDCache.bankConflict)
  catchCause(s2_replayCauseOut, ReplayCauseNO.C_WPF, io.fromDCache.wayPredictFail)
  catchCause(s2_replayCauseOut, ReplayCauseNO.C_RARF, io.fromLsq.rarNack)
  catchCause(s2_replayCauseOut, ReplayCauseNO.C_RAWF, io.fromLsq.rawNack)

  val s2_nukeVec = Wire(Vec(io.fromSta.length, Bool()))
  io.fromSta.zip(s2_nukeVec).foreach {
    case (query, nuke) =>
      /**
        * Determine whether the store access is 128-bits aligned
        * either the line matches or the access is 128-bit vector
        */
      val match128Bit = query.bits.matchLine || (io.s2_in.bits.isVector && io.s2_in.bits.is128bit)
      /**
        * Determine whether the physical address matches (with different bit
        * ranges based on 128-bit alignment)
        */
      val nukePAddrMatch = Mux(
        match128Bit,
        io.s2_in.bits.paddr(PAddrBits - 1, 4) === query.bits.paddr(PAddrBits - 1, 4),
        io.s2_in.bits.paddr(PAddrBits - 1, 3) === query.bits.paddr(PAddrBits - 1, 3)
      )
      /**
        * Determine whether the data mask matches (i.e., the relevant bits are
        * set in both the store and the query)
        */
      val nukeDataMaskMatch = (io.s2_in.bits.mask & query.bits.mask).orR
      /**
        * Determine whether the store operation is nuked by an older entry
        * (based on the ROB index)
        */
      val nukedByOlder = isAfter(io.s2_in.bits.uop.robIdx, query.bits.robIdx)
      nuke := query.valid && nukePAddrMatch && nukeDataMaskMatch && nukedByOlder
  }
  val s2_nuke = s2_nukeVec.asUInt.orR && !io.s2_in.bits.tlbMiss || causeCatched(io.s2_in.bits.cause, ReplayCauseNO.C_NK)
  catchCause(s2_replayCauseOut, ReplayCauseNO.C_NK, s2_nuke)

  /**
    * Determine whether a fast replay is possible due to DCache-related causes for s2:
    * Conditions:
    * - A fast replay is allowed if:
    *   1. The replay cause matches `C_DR` (data cache-related cause), OR
    *   2. The replay cause does NOT match `C_DM` (data miss-related cause) AND
    *      matches either `C_BC` (bank conflict) or `C_WPF` (way prediction failure).
    */
  val s2_dcacheFastReplay = causeCatched(s2_replayCauseOut, ReplayCauseNO.C_DR) ||
    !causeCatched(s2_replayCauseOut, ReplayCauseNO.C_DM) &&
    (causeCatched(s2_replayCauseOut, ReplayCauseNO.C_BC) || causeCatched(s2_replayCauseOut, ReplayCauseNO.C_WPF))

  /**
    * Determine whether a fast replay is possible due to a nuke cause for s2:
    * Conditions:
    * - A fast replay is allowed if:
    *   1. The replay cause matches `C_NK` (nuke cause), AND
    *   2. None of the replay causes in the range from `C_DR` to `C_NK` are active
    *      (ensures no conflicting causes exist in this range).
    */
  val s2_nukeFastReplay = causeCatched(s2_replayCauseOut, ReplayCauseNO.C_NK) &&
    !ReplayCauseNO.slice(s2_replayCauseOut, ReplayCauseNO.C_DR, ReplayCauseNO.C_NK).reduce(_||_)

  /**
    * Determine whether fast replay is possible for s2:
    * Conditions:
    * - A fast replay is allowed if:
    *   1. None of the replay causes in the range from `C_MA` (memory access) to `C_DR` (data cache-related) are active
    *   2. Either `s2_dcacheFastReplay` or `s2_nukeFastReplay` is true.
    *   3. Misalign no need wakeup
    */
  val s2_canFastReplay = !ReplayCauseNO.slice(s2_replayCauseOut, ReplayCauseNO.C_MA, ReplayCauseNO.C_DR).reduce(_||_) &&
    (s2_dcacheFastReplay || s2_nukeFastReplay) && !io.s2_in.bits.misalignNeedWakeUp

  io.commonOut.s2_needReplay := Cat(s2_replayCauseOut).orR
  io.commonOut.s2_canFastReplay := s2_canFastReplay

  // Pipeline
  // -------------------------------------------------------------------
  // stage 3
  // -------------------------------------------------------------------
  val s3_replayCauseIn = RegEnable(s2_replayCauseOut, io.s2_in.valid)
  val s3_replayCauseOut = WireInit(s3_replayCauseIn)

  val s3_misalignBufferValid = io.s3_in.valid && io.s3_in.bits.misalign && !io.s3_in.bits.isMisalignBuf
  catchCause(s3_replayCauseOut, ReplayCauseNO.C_MF, s3_misalignBufferValid && io.fromLsq.misalignBufNack)

  io.replayCauseOut := s3_replayCauseOut
}
