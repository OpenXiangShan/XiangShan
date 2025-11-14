// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import utility.mbist.MbistPipeline
import xiangshan.frontend.bpu.RotateHelper // TODO: move this to utility

class ICacheMetaArray(implicit p: Parameters) extends ICacheModule
    with ICacheEccHelper
    with ICacheAddrHelper
    with RotateHelper {
  class ICacheMetaArrayIO(implicit p: Parameters) extends ICacheBundle {
    val write:    MetaWriteBundle = Flipped(new MetaWriteBundle)
    val read:     MetaReadBundle  = Flipped(new MetaReadBundle)
    val flush:    MetaFlushBundle = Flipped(new MetaFlushBundle)
    val flushAll: Bool            = Input(Bool())
  }

  val io: ICacheMetaArrayIO = IO(new ICacheMetaArrayIO)

  private object ICacheMetaEntry {
    def apply(meta: ICacheMetadata, poison: Bool)(implicit p: Parameters): ICacheMetaEntry = {
      val entry = Wire(new ICacheMetaEntry)
      entry.meta := meta
      entry.code := encodeMetaEccByPort(meta, poison)
      entry
    }
  }

  // sanity check
  require(MetaEntryBits == (new ICacheMetaEntry).getWidth)

  private val banks   = Seq.tabulate(PortNumber)(i => Module(new ICacheMetaInterleavedBank))
  private val mbistPl = MbistPipeline.PlaceMbistPipeline(1, "MbistPipeIcacheTag", hasMbist)

  /* *** read *** */
  // vSetIdx(1) must be vSetIdx(0) + 1 if isDoubleLine, it's pre-computed in Ftq for better timing (maybe)
  assert(
    !(
      io.read.req.valid && io.read.req.bits.isDoubleLine &&
        io.read.req.bits.vSetIdx(0) + 1.U =/= io.read.req.bits.vSetIdx(1)
    ),
    "2 read setIdx must be adjacent!"
  )

  // rotate setIdxVec to match interleaved banking
  // e.g. 2-interleaved, if vSetIdx(0) is even (getInterleavedBankIdx == 0), we don't need to rotate
  //      i.e. vSetIdx(0) goes to bank0, vSetIdx(1) goes to bank1
  //      if vSetIdx(0) is odd (getInterleavedBankIdx == 1), we need to rotate right once
  //      i.e. vSetIdx(0) goes to bank1, vSetIdx(1) goes to bank0
  private val r0_rotateNum = getInterleavedBankIdx(io.read.req.bits.vSetIdx(0))
  private val r0_validVec = vecRotateRight(
    VecInit(Seq(io.read.req.valid, io.read.req.valid && io.read.req.bits.isDoubleLine)),
    r0_rotateNum
  )
  private val r0_setIdxVec = vecRotateRight(
    VecInit(io.read.req.bits.vSetIdx.map(getInterleavedSetIdx)),
    r0_rotateNum
  )

  io.read.req.ready := banks.map(_.io.read.req.ready).reduce(_ && _)
  banks.zipWithIndex.foreach { case (b, i) =>
    b.io.read.req.valid       := r0_validVec(i)
    b.io.read.req.bits.setIdx := r0_setIdxVec(i)
  }

  private val readRotateNumReg = RegEnable(r0_rotateNum, 0.U(InterleavedBankIdxBits.W), io.read.req.fire)
  // rotate back to original order
  private val readResult = vecRotateLeft(
    VecInit(banks.map(_.io.read.resp)),
    readRotateNumReg
  )

  io.read.resp.entryValid := VecInit(readResult.map(port => VecInit(port.entries.map(_.valid))))
  io.read.resp.metas      := VecInit(readResult.map(port => VecInit(port.entries.map(_.bits.meta))))
  io.read.resp.codes      := VecInit(readResult.map(port => VecInit(port.entries.map(_.bits.code))))

  // TEST: force ECC to fail by setting readCodes to 0
  if (ForceMetaEccFail) {
    io.read.resp.codes := 0.U.asTypeOf(io.read.resp.codes)
  }

  /* *** read *** */
  private val w0_bankIdx = getInterleavedBankIdx(io.write.req.bits.bankIdx)
  private val w0_setIdx  = getInterleavedSetIdx(io.write.req.bits.vSetIdx)

  private val w0_entry = ICacheMetaEntry(
    meta = io.write.req.bits.meta,
    poison = io.write.req.bits.poison
  )

  io.write.req.ready := banks.map(_.io.write.req.ready).reduce(_ && _)
  banks.zipWithIndex.foreach { case (b, i) =>
    b.io.write.req.valid       := io.write.req.valid && (i.U === w0_bankIdx)
    b.io.write.req.bits.setIdx := w0_setIdx
    b.io.write.req.bits.entry  := w0_entry
  }

  /* *** flush *** */
  // similar to read
  assert(
    !(
      io.flush.req(0).valid && io.flush.req(1).valid &&
        io.flush.req(0).bits.vSetIdx + 1.U =/= io.flush.req(1).bits.vSetIdx
    ),
    "2 flush setIdx must be adjacent!"
  )

  private val f0_rotateNum = getInterleavedBankIdx(io.flush.req(0).bits.vSetIdx)
  private val f0_reqVec = vecRotateRight(
    io.flush.req,
    f0_rotateNum
  )

  banks.zipWithIndex.foreach { case (b, i) =>
    b.io.flush.req.valid        := f0_reqVec(i).valid
    b.io.flush.req.bits.setIdx  := getInterleavedSetIdx(f0_reqVec(i).bits.vSetIdx)
    b.io.flush.req.bits.waymask := f0_reqVec(i).bits.waymask

    b.io.flushAll := io.flushAll
  }

  /* *** perf *** */
  // refill
  XSPerfAccumulate("refill", io.write.req.valid)
  // flush
  XSPerfAccumulate("flush", io.flush.req.map(_.valid).reduce(_ || _))
  XSPerfAccumulate("flushAll", io.flushAll)
}
