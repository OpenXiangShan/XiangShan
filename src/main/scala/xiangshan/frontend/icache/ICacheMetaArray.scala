// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import utility.sram.SplittedSRAMTemplate

class ICacheMetaArray(implicit p: Parameters) extends ICacheModule with ICacheEccHelper {
  class ICacheMetaArrayIO(implicit p: Parameters) extends ICacheBundle {
    val write:    MetaWriteBundle = Flipped(new MetaWriteBundle)
    val read:     MetaReadBundle  = Flipped(new MetaReadBundle)
    val flush:    MetaFlushBundle = Flipped(new MetaFlushBundle)
    val flushAll: Bool            = Input(Bool())
  }

  val io: ICacheMetaArrayIO = IO(new ICacheMetaArrayIO)

  class ICacheMetaEntry(implicit p: Parameters) extends ICacheBundle {
    val meta: ICacheMetadata = new ICacheMetadata
    val code: UInt           = UInt(MetaEccBits.W)
  }

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

  private val port0Read0 = io.read.req.valid && !io.read.req.bits.vSetIdx(0)(0)
  private val port0Read1 = io.read.req.valid && io.read.req.bits.vSetIdx(0)(0)
  private val port1Read1 = io.read.req.valid && io.read.req.bits.vSetIdx(1)(0) && io.read.req.bits.isDoubleLine
  private val port1Read0 = io.read.req.valid && !io.read.req.bits.vSetIdx(1)(0) && io.read.req.bits.isDoubleLine

  private val port0Read0Reg = RegEnable(port0Read0, 0.U.asTypeOf(port0Read0), io.read.req.fire)
  private val port0Read1Reg = RegEnable(port0Read1, 0.U.asTypeOf(port0Read1), io.read.req.fire)
  private val port1Read1Reg = RegEnable(port1Read1, 0.U.asTypeOf(port1Read1), io.read.req.fire)
  private val port1Read0Reg = RegEnable(port1Read0, 0.U.asTypeOf(port1Read0), io.read.req.fire)

  private val bank0Idx = Mux(port0Read0, io.read.req.bits.vSetIdx(0), io.read.req.bits.vSetIdx(1))
  private val bank1Idx = Mux(port0Read1, io.read.req.bits.vSetIdx(0), io.read.req.bits.vSetIdx(1))

  private val writeBank0 = io.write.req.valid && !io.write.req.bits.bankIdx
  private val writeBank1 = io.write.req.valid && io.write.req.bits.bankIdx

  private val writeMetaBits = ICacheMetaEntry(
    meta = io.write.req.bits.meta,
    poison = io.write.req.bits.poison
  )

  private val tagArrays = (0 until PortNumber) map { bank =>
    val tagArray = Module(new SplittedSRAMTemplate(
      new ICacheMetaEntry(),
      set = nSets / PortNumber,
      way = nWays,
      waySplit = 2,
      dataSplit = 1,
      shouldReset = true,
      holdRead = true,
      singlePort = true,
      withClockGate = true,
      hasMbist = hasMbist,
      hasSramCtl = hasSramCtl
    ))

    // meta connection
    if (bank == 0) {
      tagArray.io.r.req.valid := port0Read0 || port1Read0
      tagArray.io.r.req.bits.apply(setIdx = bank0Idx(idxBits - 1, 1))
      tagArray.io.w.req.valid := writeBank0
      tagArray.io.w.req.bits.apply(
        data = writeMetaBits,
        setIdx = io.write.req.bits.vSetIdx(idxBits - 1, 1),
        waymask = io.write.req.bits.waymask
      )
    } else {
      tagArray.io.r.req.valid := port0Read1 || port1Read1
      tagArray.io.r.req.bits.apply(setIdx = bank1Idx(idxBits - 1, 1))
      tagArray.io.w.req.valid := writeBank1
      tagArray.io.w.req.bits.apply(
        data = writeMetaBits,
        setIdx = io.write.req.bits.vSetIdx(idxBits - 1, 1),
        waymask = io.write.req.bits.waymask
      )
    }

    tagArray
  }
  private val mbistPl = MbistPipeline.PlaceMbistPipeline(1, "MbistPipeIcacheTag", hasMbist)

  private val readSetIdxNext =
    RegEnable(io.read.req.bits.vSetIdx, 0.U.asTypeOf(io.read.req.bits.vSetIdx), io.read.req.fire)
  private val validArray = RegInit(VecInit(Seq.fill(nWays)(0.U(nSets.W))))
  private val validMetas = Wire(Vec(PortNumber, Vec(nWays, Bool())))
  // valid read
  (0 until PortNumber).foreach(i =>
    (0 until nWays).foreach(way =>
      validMetas(i)(way) := validArray(way)(readSetIdxNext(i))
    )
  )
  io.read.resp.entryValid := validMetas

  io.read.req.ready := !io.write.req.valid && !io.flush.req.map(_.valid).reduce(_ || _) && !io.flushAll &&
    tagArrays.map(_.io.r.req.ready).reduce(_ && _)

  // valid write
  private val writeWayNum = OHToUInt(io.write.req.bits.waymask)
  when(io.write.req.valid) {
    validArray(writeWayNum) := validArray(writeWayNum).bitSet(io.write.req.bits.vSetIdx, true.B)
  }

  io.read.resp.metas <> DontCare
  io.read.resp.codes <> DontCare
  private val readMetaEntries = tagArrays.map(port => port.io.r.resp.asTypeOf(Vec(nWays, new ICacheMetaEntry())))
  private val readMetas       = readMetaEntries.map(_.map(_.meta))
  private val readCodes       = readMetaEntries.map(_.map(_.code))

  // TEST: force ECC to fail by setting readCodes to 0
  if (ForceMetaEccFail) {
    readCodes.foreach(_.foreach(_ := 0.U))
  }

  when(port0Read0Reg) {
    io.read.resp.metas(0) := readMetas(0)
    io.read.resp.codes(0) := readCodes(0)
  }.elsewhen(port0Read1Reg) {
    io.read.resp.metas(0) := readMetas(1)
    io.read.resp.codes(0) := readCodes(1)
  }

  when(port1Read0Reg) {
    io.read.resp.metas(1) := readMetas(0)
    io.read.resp.codes(1) := readCodes(0)
  }.elsewhen(port1Read1Reg) {
    io.read.resp.metas(1) := readMetas(1)
    io.read.resp.codes(1) := readCodes(1)
  }

  io.write.req.ready := true.B

  /*
   * flush logic
   */
  // flush standalone set (e.g. flushed by mainPipe before doing re-fetch)
  when(io.flush.req.map(_.valid).reduce(_ || _)) {
    (0 until nWays).foreach { w =>
      validArray(w) := (0 until PortNumber).map { i =>
        Mux(
          // check if set `vSetIdx` in way `w` is requested to be flushed by port `i`
          io.flush.req(i).valid && io.flush.req(i).bits.waymask(w),
          validArray(w).bitSet(io.flush.req(i).bits.vSetIdx, false.B),
          validArray(w)
        )
      }.reduce(_ & _)
    }
  }

  // flush all (e.g. fence.i)
  when(io.flushAll) {
    (0 until nWays).foreach(w => validArray(w) := 0.U)
  }

  /* *** perf *** */
  // refill
  XSPerfAccumulate("refill", io.write.req.valid)
  // flush
  XSPerfAccumulate("flush", io.flush.req.map(_.valid).reduce(_ || _))
  XSPerfAccumulate("flushAll", io.flushAll)
}
