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
import utils.VecRotate

class ICacheMetaArray(implicit p: Parameters) extends ICacheModule with ICacheAddrHelper {
  class ICacheMetaArrayIO(implicit p: Parameters) extends ICacheBundle {
    val write:    MetaWriteBundle = Flipped(new MetaWriteBundle)
    val read:     MetaReadBundle  = Flipped(new MetaReadBundle)
    val flush:    MetaFlushBundle = Flipped(new MetaFlushBundle)
    val flushAll: Bool            = Input(Bool())
  }

  val io: ICacheMetaArrayIO = IO(new ICacheMetaArrayIO)

  // sanity check
  require(MetaEntryBits == (new ICacheMetaEntry).getWidth)

  private val banks = Seq.tabulate(PortNumber)(i => Module(new ICacheMetaInterleavedBank(i)))

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
  private val r0_rotator = VecRotate(getInterleavedBankIdx(io.read.req.bits.vSetIdx(0)), storeOneHot = true)
  private val r0_validVec = r0_rotator.rotate(
    VecInit(Seq(io.read.req.valid, io.read.req.valid && io.read.req.bits.isDoubleLine))
  )
  private val r0_setIdxVec = r0_rotator.rotate(
    VecInit(io.read.req.bits.vSetIdx.map(getInterleavedSetIdx))
  )

  io.read.req.ready := banks.map(_.io.read.req.ready).reduce(_ && _)
  banks.zipWithIndex.foreach { case (b, i) =>
    b.io.read.req.valid       := r0_validVec(i)
    b.io.read.req.bits.setIdx := r0_setIdxVec(i)
  }

  private val r1_rotator = RegEnable(r0_rotator, io.read.req.fire)
  // rotate back to original order
  io.read.resp.entries := r1_rotator.revert(VecInit(banks.map(_.io.read.resp.entries)))

  // TEST: force ECC to fail by setting parity codes to 0
  if (ForceMetaEccFail) {
    io.read.resp.entries.foreach(_.foreach(_.bits.code := 0.U(MetaEccBits.W)))
  }

  /* *** write *** */
  private val w0_bankIdx = getInterleavedBankIdx(io.write.req.bits.vSetIdx)
  private val w0_valid   = io.write.req.valid
  private val w0_setIdx  = getInterleavedSetIdx(io.write.req.bits.vSetIdx)
  private val w0_waymask = io.write.req.bits.waymask
  private val w0_entry   = io.write.req.bits.entry

  io.write.req.ready := banks.map(_.io.write.req.ready).reduce(_ && _)
  banks.zipWithIndex.foreach { case (b, i) =>
    b.io.write.req.valid        := w0_valid && (i.U === w0_bankIdx)
    b.io.write.req.bits.setIdx  := w0_setIdx
    b.io.write.req.bits.waymask := w0_waymask
    b.io.write.req.bits.entry   := w0_entry
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

  private val f0_rotator = VecRotate(getInterleavedBankIdx(io.flush.req(0).bits.vSetIdx))
  private val f0_reqVec  = f0_rotator.rotate(io.flush.req)

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
