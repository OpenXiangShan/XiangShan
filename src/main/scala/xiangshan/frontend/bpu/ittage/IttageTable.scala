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

package xiangshan.frontend.bpu.ittage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import scala.math.min
import utility.ParallelXOR
import utility.XSDebug
import utility.XSPerfAccumulate
import utility.mbist.MbistPipeline
import utility.sram.FoldedSRAMTemplate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.PhrHelper
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.WriteBuffer
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

class IttageTable(
    val nRows:    Int,
    val histLen:  Int,
    val tagLen:   Int,
    val tableIdx: Int
)(implicit p: Parameters) extends IttageModule with PhrHelper {
  class IttageTableIO extends IttageBundle {
    class Req extends Bundle {
      val pc:         PrunedAddr            = PrunedAddr(VAddrBits)
      val foldedHist: PhrAllFoldedHistories = new PhrAllFoldedHistories(AllFoldedHistoryInfo)
    }

    class Resp extends Bundle {
      val cnt:          SaturateCounter = new SaturateCounter(ConfidenceCntWidth)
      val usefulCnt:    SaturateCounter = new SaturateCounter(UsefulCntWidth)
      val targetOffset: IttageOffset    = new IttageOffset()
    }

    class Update extends Bundle {
      val pc:         PrunedAddr            = PrunedAddr(VAddrBits)
      val foldedHist: PhrAllFoldedHistories = new PhrAllFoldedHistories(AllFoldedHistoryInfo)
      // update tag and ctr
      val valid:   Bool            = Bool()
      val correct: Bool            = Bool()
      val alloc:   Bool            = Bool()
      val oldCnt:  SaturateCounter = new SaturateCounter(ConfidenceCntWidth)
      // update useful
      val usefulCntValid: Bool            = Bool()
      val usefulCnt:      SaturateCounter = new SaturateCounter(UsefulCntWidth)
      val resetUsefulCnt: Bool            = Bool()
      // target
      val targetOffset:    IttageOffset = new IttageOffset
      val oldTargetOffset: IttageOffset = new IttageOffset
    }

    val req:    DecoupledIO[Req] = Flipped(DecoupledIO(new Req))
    val resp:   Valid[Resp]      = Output(Valid(new Resp))
    val update: Update           = Input(new Update)
  }

  val io: IttageTableIO = IO(new IttageTableIO)

  private val foldedWidth = if (nRows >= TableSramSize) nRows / TableSramSize else 1
  private val dataSplit   = if (nRows <= 2 * TableSramSize) 1 else 2

  if (nRows < TableSramSize) {
    println(f"warning: ittage table $tableIdx has small sram depth of $nRows")
  }

  require(histLen == 0 && tagLen == 0 || histLen != 0 && tagLen != 0)
  private val idxFhInfo    = new FoldedHistoryInfo(histLen, min(histLen, log2Ceil(nRows)))
  private val tagFhInfo    = new FoldedHistoryInfo(histLen, min(histLen, tagLen))
  private val altTagFhInfo = new FoldedHistoryInfo(histLen, min(histLen, tagLen - 1))

  def computeTagAndHash(unhashedIdx: UInt, allFh: PhrAllFoldedHistories): (UInt, UInt) =
    if (histLen > 0) {
      val idxFh    = allFh.getHistWithInfo(idxFhInfo).foldedHist
      val tagFh    = allFh.getHistWithInfo(tagFhInfo).foldedHist
      val altTagFh = allFh.getHistWithInfo(altTagFhInfo).foldedHist
      // require(idxFh.getWidth == log2Ceil(nRows))
      val idx = (unhashedIdx ^ idxFh)(log2Ceil(nRows) - 1, 0)
      val tag = ((unhashedIdx >> log2Ceil(nRows)).asUInt ^ tagFh ^ (altTagFh << 1).asUInt)(tagLen - 1, 0)
      (idx, tag)
    } else {
      require(tagLen == 0)
      (unhashedIdx(log2Ceil(nRows) - 1, 0), 0.U)
    }

  // sanity check, FIXME: is this really needed?
  // The least significant bit of offset is pruned
  def ittageEntrySz: Int =
    1 + tagLen + ConfidenceCntWidth + UsefulCntWidth + TargetOffsetWidth + log2Ceil(RegionNums) + 1
  require(ittageEntrySz == (new IttageEntry(tagLen)).getWidth)

  // pc is start address of basic block, most 2 branch inst in block
  def getUnhashedIdx(pc: PrunedAddr): UInt = (pc >> instOffsetBits).asUInt

  private val s0_valid       = io.req.valid
  private val s0_pc          = io.req.bits.pc
  private val s0_unhashedIdx = getUnhashedIdx(io.req.bits.pc)

  private val (s0_idx, s0_tag) = computeTagAndHash(s0_unhashedIdx, io.req.bits.foldedHist)
  private val (s1_idx, s1_tag) = (RegEnable(s0_idx, io.req.fire), RegEnable(s0_tag, io.req.fire))
  private val s1_valid         = RegNext(s0_valid)

  private val table = Module(new FoldedSRAMTemplate(
    new IttageEntry(tagLen),
    setSplit = 1,
    waySplit = 1,
    dataSplit = dataSplit,
    set = nRows,
    width = foldedWidth,
    shouldReset = true,
    holdRead = true,
    singlePort = true,
    useBitmask = true,
    withClockGate = true,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl
  ))
  private val mbistPl = MbistPipeline.PlaceMbistPipeline(1, "MbistPipeIttage", hasMbist)
  table.io.r.req.valid       := io.req.fire
  table.io.r.req.bits.setIdx := s0_idx

  private val tableReadData = table.io.r.resp.data(0)

  private val s1_reqReadHit = tableReadData.valid && tableReadData.tag === s1_tag

  private val readWriteConflict    = io.update.valid && io.req.valid
  private val s1_readWriteConflict = RegEnable(readWriteConflict, io.req.valid)

  io.resp.valid    := (if (tagLen != 0) s1_reqReadHit && !s1_readWriteConflict else true.B) && s1_valid // && s1_mask(b)
  io.resp.bits.cnt := tableReadData.confidenceCnt
  io.resp.bits.usefulCnt    := tableReadData.usefulCnt
  io.resp.bits.targetOffset := tableReadData.targetOffset

  // Use fetchpc to compute hash
  private val updateFoldedHist       = io.update.foldedHist
  private val (updateIdx, updateTag) = computeTagAndHash(getUnhashedIdx(io.update.pc), updateFoldedHist)
  private val updateWdata            = Wire(new IttageEntry(tagLen))

  private val updateAllBitmask = VecInit.fill(ittageEntrySz)(1.U).asUInt // update all entry
  private val updateNoBitmask  = VecInit.fill(ittageEntrySz)(0.U).asUInt // update no
  private val updateNoUsBitmask =
    VecInit.tabulate(ittageEntrySz)(_.U >= UsefulCntWidth.U).asUInt // update others besides useful bit
  private val updateUsBitmask = VecInit.tabulate(ittageEntrySz)(_.U < UsefulCntWidth.U).asUInt // update useful bit

  private val needReset               = RegInit(false.B)
  private val usefulCanReset          = !(io.req.fire || io.update.valid) && needReset
  private val (resetSet, resetFinish) = Counter(usefulCanReset, nRows)
  when(io.update.resetUsefulCnt) {
    needReset := true.B
  }.elsewhen(resetFinish) {
    needReset := false.B
  }
  private val updateBitmask = Mux(
    io.update.usefulCntValid && io.update.valid,
    updateAllBitmask,
    Mux(io.update.valid, updateNoUsBitmask, Mux(usefulCanReset, updateUsBitmask, updateNoBitmask))
  )

  /** 
    Bypass write data from WriteBuffer to sram when read port is empty.
  */
  private val writeBuffer = Module(new WriteBuffer(
    gen = new IttageWriteReq(tagLen, nRows, ittageEntrySz),
    numEntries = TableWriteBufferSize,
    numPorts = 1
  ))

  // read/write port of write buffer
  private val writeBufferWPort = writeBuffer.io.write.head
  private val writeBufferRPort = writeBuffer.io.read.head
  // read the stored write req from write buffer
  private val writeValid   = writeBufferRPort.valid && !table.io.r.req.valid
  private val writeEntry   = writeBufferRPort.bits.entry
  private val writeSetIdx  = writeBufferRPort.bits.setIdx
  private val writeWayMask = true.B
  private val writeBitmask = writeBufferRPort.bits.bitmask
  writeBufferRPort.ready := table.io.w.req.ready && !table.io.r.req.valid

  // write to writeBuffer
  writeBufferWPort.valid        := io.update.valid || usefulCanReset
  writeBufferWPort.bits.entry   := updateWdata
  writeBufferWPort.bits.setIdx  := Mux(usefulCanReset, resetSet, updateIdx)
  writeBufferWPort.bits.bitmask := updateBitmask

  // write from writeBuffer to sram
  table.io.w.apply(writeValid, writeEntry, writeSetIdx, writeWayMask, writeBitmask)

  // Power-on reset
  private val powerOnResetState = RegInit(true.B)
  when(table.io.r.req.ready) {
    // When all the SRAM first reach ready state, we consider power-on reset is done
    powerOnResetState := false.B
  }
  // Do not use table banks io.r.req.ready directly
  // All table_banks are single port SRAM, ready := !wen
  // We do not want write request block the whole BPU pipeline
  // Once read priority is higher than write, table_banks(*).io.r.req.ready can be used
  io.req.ready := !powerOnResetState

  private val oldCtr = io.update.oldCnt
  updateWdata.valid := true.B
  updateWdata.confidenceCnt.value := Mux(
    io.update.alloc,
    oldCtr.getNeutral, // reset to neutral when allocate
    oldCtr.getUpdate(io.update.correct)
  )
  updateWdata.tag := updateTag
  updateWdata.usefulCnt := Mux(
    usefulCanReset,
    false.B.asTypeOf(new SaturateCounter(UsefulCntWidth)),
    io.update.usefulCnt
  )
  // only when ctr is null
  def ctrNull(ctr: SaturateCounter): Bool = ctr.isSaturateNegative
  updateWdata.targetOffset := Mux(
    io.update.alloc || ctrNull(oldCtr),
    io.update.targetOffset,
    io.update.oldTargetOffset
  )
  updateWdata.paddingBit := DontCare

  XSPerfAccumulate("ittage_table_updates", io.update.valid)
  XSPerfAccumulate("ittage_table_hits", io.resp.valid)
  XSPerfAccumulate("ittage_us_tick_reset", io.update.resetUsefulCnt)
  XSPerfAccumulate("ittage_table_read_write_conflict", readWriteConflict)

  if (debug) {
    val u   = io.update
    val idx = s0_idx
    val tag = s0_tag
    XSDebug(
      io.req.fire,
      p"ITTageTableReq: pc=0x${Hexadecimal(io.req.bits.pc.toUInt)}, " +
        p"idx=$idx, tag=$tag\n"
    )
    XSDebug(
      RegNext(io.req.fire) && s1_reqReadHit,
      p"ITTageTableResp: idx=$s1_idx, hit:${s1_reqReadHit}, " +
        p"ctr:${io.resp.bits.cnt}, u:${io.resp.bits.usefulCnt}, tar:${Hexadecimal(io.resp.bits.targetOffset.offset.toUInt)}\n"
    )
    XSDebug(
      io.update.valid,
      p"update ITTAGE Table: pc:${Hexadecimal(u.pc.toUInt)}}, " +
        p"correct:${u.correct}, alloc:${u.alloc}, oldCtr:${u.oldCnt}, " +
        p"target:${Hexadecimal(u.targetOffset.offset.toUInt)}, old_target:${Hexadecimal(u.oldTargetOffset.offset.toUInt)}\n"
    )
    XSDebug(
      io.update.valid,
      p"update ITTAGE Table: writing tag:${updateTag}, " +
        p"ctr: ${updateWdata.confidenceCnt}, target:${Hexadecimal(updateWdata.targetOffset.offset.toUInt)}" +
        p" in idx $updateIdx\n"
    )
    XSDebug(RegNext(io.req.fire) && !s1_reqReadHit, "TageTableResp: no hits!\n")

    // ------------------------------Debug-------------------------------------
    val valids = RegInit(0.U.asTypeOf(Vec(nRows, Bool())))
    when(io.update.valid)(valids(updateIdx) := true.B)
    XSDebug("ITTAGE Table usage:------------------------\n")
    XSDebug("%d out of %d rows are valid\n", PopCount(valids), nRows.U)
  }
}
