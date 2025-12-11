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
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.WriteBuffer
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

class IttageTable(
    val nRows:    Int,
    val histLen:  Int,
    val tagLen:   Int,
    val tableIdx: Int
)(implicit p: Parameters) extends IttageModule {
  class IttageTableIO extends IttageBundle {
    class Req extends Bundle {
      val startPc:    PrunedAddr            = PrunedAddr(VAddrBits)
      val foldedHist: PhrAllFoldedHistories = new PhrAllFoldedHistories(AllFoldedHistoryInfo)
    }

    class Resp extends Bundle {
      val cnt:          SaturateCounter = new SaturateCounter(ConfidenceCntWidth)
      val usefulCnt:    SaturateCounter = new SaturateCounter(UsefulCntWidth)
      val targetOffset: IttageOffset    = new IttageOffset()
    }

    class Update extends Bundle {
      val startPc:    PrunedAddr            = PrunedAddr(VAddrBits)
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

  // Banked organization: split rows evenly across banks to allow predict/read and update on different banks.
  private val bankIdxWidth = IttageBankIdxWidth
  private val numBanks     = IttageNumBanks
  require(nRows % numBanks == 0, "ITTAGE table rows must be divisible by number of banks")
  private val setsPerBank  = nRows / numBanks
  private val setIdxWidth  = log2Ceil(setsPerBank)
  private val idxFullWidth = setIdxWidth + bankIdxWidth
  private val foldedWidth  = if (setsPerBank >= TableSramSize) setsPerBank / TableSramSize else 1
  private val dataSplit    = if (setsPerBank <= 2 * TableSramSize) 1 else 2

  if (nRows < TableSramSize) {
    println(f"warning: ittage table $tableIdx has small sram depth of $nRows")
  }

  require(histLen == 0 && tagLen == 0 || histLen != 0 && tagLen != 0)
  private val idxFhInfo    = new FoldedHistoryInfo(histLen, min(histLen, setIdxWidth))
  private val tagFhInfo    = new FoldedHistoryInfo(histLen, min(histLen, tagLen))
  private val altTagFhInfo = new FoldedHistoryInfo(histLen, min(histLen, tagLen - 1))

  def computeTagAndHash(unhashedIdx: UInt, allFh: PhrAllFoldedHistories): (UInt, UInt, UInt) =
    if (histLen > 0) {
      val idxFh      = allFh.getHistWithInfo(idxFhInfo).foldedHist
      val tagFh      = allFh.getHistWithInfo(tagFhInfo).foldedHist
      val altTagFh   = allFh.getHistWithInfo(altTagFhInfo).foldedHist
      val bankIdx    = if (bankIdxWidth == 0) 0.U else unhashedIdx(bankIdxWidth - 1, 0)
      val setIdxBase = unhashedIdx(bankIdxWidth + setIdxWidth - 1, bankIdxWidth)
      val setIdx     = (setIdxBase ^ idxFh)(setIdxWidth - 1, 0)
      val tagBase    = (unhashedIdx >> idxFullWidth).asUInt
      val tag        = (tagBase ^ tagFh ^ (altTagFh << 1).asUInt)(tagLen - 1, 0)
      (bankIdx, setIdx, tag)
    } else {
      require(tagLen == 0)
      val bankIdx = if (bankIdxWidth == 0) 0.U else unhashedIdx(bankIdxWidth - 1, 0)
      val setIdx  = unhashedIdx(bankIdxWidth + setIdxWidth - 1, bankIdxWidth)
      (bankIdx, setIdx, 0.U)
    }

  // sanity check, FIXME: is this really needed?
  // The least significant bit of offset is pruned
  def ittageEntrySz: Int =
    1 + tagLen + ConfidenceCntWidth + UsefulCntWidth + TargetOffsetWidth + log2Ceil(RegionNums) + 1
  require(ittageEntrySz == (new IttageEntry(tagLen)).getWidth)

  // pc is start address of basic block, most 2 branch inst in block
  def getUnhashedIdx(pc: PrunedAddr): UInt = (pc >> instOffsetBits).asUInt

  private val s0_valid       = io.req.valid
  private val s0_startPc     = io.req.bits.startPc
  private val s0_unhashedIdx = getUnhashedIdx(io.req.bits.startPc)
  private val (s0_bankIdx, s0_setIdx, s0_tag) =
    computeTagAndHash(s0_unhashedIdx, io.req.bits.foldedHist)

  private val s0_bankMask = UIntToOH(s0_bankIdx, numBanks)

  private val (s1_setIdx, s1_tag) = (RegEnable(s0_setIdx, io.req.fire), RegEnable(s0_tag, io.req.fire))
  private val s1_bankMask         = RegEnable(s0_bankMask, io.req.fire)
  private val s1_valid            = RegNext(s0_valid)

  // Each bank is a single-port SRAM slice; banking lets predict/read and commit/update touch different banks concurrently.
  private val tables = Seq.tabulate(numBanks) { bankIdx =>
    Module(new FoldedSRAMTemplate(
      new IttageEntry(tagLen),
      setSplit = 1,
      waySplit = 1,
      dataSplit = dataSplit,
      set = setsPerBank,
      width = foldedWidth,
      shouldReset = true,
      holdRead = true,
      singlePort = true,
      useBitmask = true,
      withClockGate = true,
      hasMbist = hasMbist,
      hasSramCtl = hasSramCtl,
      suffix = Option(s"bpu_ittage_bank$bankIdx")
    )).suggestName(s"ittage_table_bank$bankIdx")
  }
  private val mbistPl = MbistPipeline.PlaceMbistPipeline(1, "MbistPipeIttage", hasMbist)
  tables.zipWithIndex.foreach { case (bank, idx) =>
    bank.io.r.req.valid       := io.req.fire && s0_bankMask(idx)
    bank.io.r.req.bits.setIdx := s0_setIdx
  }

  private val tableReadData =
    Mux1H(s1_bankMask, tables.map(_.io.r.resp.data.head))

  private val s1_reqReadHit = tableReadData.valid && (if (tagLen != 0) tableReadData.tag === s1_tag else true.B)

  io.resp.valid             := s1_reqReadHit && s1_valid // && s1_mask(b)
  io.resp.bits.cnt          := tableReadData.confidenceCnt
  io.resp.bits.usefulCnt    := tableReadData.usefulCnt
  io.resp.bits.targetOffset := tableReadData.targetOffset

  // Use fetchpc to compute hash
  private val updateFoldedHist                      = io.update.foldedHist
  private val updateUnhashedIdx                     = getUnhashedIdx(io.update.startPc)
  private val (updateBankIdx, updateIdx, updateTag) = computeTagAndHash(updateUnhashedIdx, updateFoldedHist)
  private val updateBankMask                        = UIntToOH(updateBankIdx, numBanks)
  private val updateWdata                           = Wire(new IttageEntry(tagLen))

  private val updateAllBitmask = VecInit.fill(ittageEntrySz)(1.U).asUInt // update all entry
  private val updateNoBitmask  = VecInit.fill(ittageEntrySz)(0.U).asUInt // update no
  private val updateNoUsBitmask =
    VecInit.tabulate(ittageEntrySz)(_.U >= UsefulCntWidth.U).asUInt // update others besides useful bit
  private val updateUsBitmask = VecInit.tabulate(ittageEntrySz)(_.U < UsefulCntWidth.U).asUInt // update useful bit

  private val needReset      = RegInit(false.B)
  private val usefulCanReset = !(io.req.fire || io.update.valid) && needReset
  // Sweep one set index per cycle; all banks reuse resetSet so the full table resets in setsPerBank cycles.
  private val (resetSet, resetFinish) = Counter(usefulCanReset, setsPerBank)
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
    Bypass write data from per-bank WriteBuffer to SRAM when that bank's read port is idle.
    Each bank owns its own buffer so an update to bank A can proceed while bank B is serving a read.
  */
  private val writeBuffers = Seq.tabulate(numBanks) { bankIdx =>
    Module(new WriteBuffer(
      gen = new IttageWriteReq(tagLen, setsPerBank, ittageEntrySz),
      numEntries = TableWriteBufferSize,
      numPorts = 1,
      nameSuffix = s"ittageTable${tableIdx}_bank$bankIdx"
    )).suggestName(s"ittage_write_buffer_bank$bankIdx")
  }

  // write to per-bank write buffers
  writeBuffers.zipWithIndex.foreach { case (writeBuffer, bankIdx) =>
    val writePort = writeBuffer.io.write.head
    writePort.valid        := (io.update.valid && updateBankMask(bankIdx)) || usefulCanReset
    writePort.bits.entry   := updateWdata
    writePort.bits.setIdx  := Mux(usefulCanReset, resetSet, updateIdx)
    writePort.bits.bitmask := updateBitmask
  }

  // read the stored write req from write buffer and push into the matching bank SRAM
  tables.zip(writeBuffers).zipWithIndex.foreach { case ((bank, writeBuffer), bankIdx) =>
    val readPort     = writeBuffer.io.read.head
    val writeValid   = readPort.valid && !bank.io.r.req.valid
    val writeEntry   = readPort.bits.entry
    val writeSetIdx  = readPort.bits.setIdx
    val writeBitMask = readPort.bits.bitmask
    bank.io.w.apply(writeValid, writeEntry, writeSetIdx, true.B, writeBitMask)
    readPort.ready := bank.io.w.req.ready && !bank.io.r.req.valid
  }

  // Power-on reset
  private val powerOnResetState = RegInit(true.B)
  when(tables.map(_.io.r.req.ready).reduce(_ && _)) {
    // When all the SRAM banks first reach ready state, we consider power-on reset is done
    powerOnResetState := false.B
  }
  // Do not use table banks io.r.req.ready directly
  // All table_banks are single port SRAM, ready := !wen
  // We do not want write request block the whole BPU pipeline
  // Once read priority is higher than write, table_banks(*).io.r.req.ready can be used
  io.req.ready := !powerOnResetState

  private val oldCtr = io.update.oldCnt
  updateWdata.valid := true.B
  updateWdata.confidenceCnt := Mux(
    io.update.alloc,
    SaturateCounter.WeakPositive(ConfidenceCntWidth), // reset to neutral (weak positive) when allocate
    oldCtr.getUpdate(io.update.correct)
  )
  updateWdata.tag := updateTag
  updateWdata.usefulCnt := Mux(
    usefulCanReset,
    SaturateCounter.SaturateNegative(UsefulCntWidth),
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
  // read-write conflict: can provide data to sram write port, but sram read port is also requesting
  XSPerfAccumulate(
    "ittage_table_read_write_conflict",
    VecInit(tables.zip(writeBuffers).map { case (bank, buffer) =>
      bank.io.r.req.valid && buffer.io.read.head.valid
    }).asUInt.orR
  )
  private val targetBankReady = writeBuffers
    .zip(updateBankMask.asBools)
    .map { case (buffer, sel) => buffer.io.write.head.ready && sel }
    .reduce(_ || _)
  XSPerfAccumulate("ittage_table_update_drop", io.update.valid && !targetBankReady)

  if (debug) {
    val u    = io.update
    val bank = s0_bankIdx
    val idx  = s0_setIdx
    val tag  = s0_tag
    XSDebug(
      io.req.fire,
      p"ITTageTableReq: pc=0x${Hexadecimal(io.req.bits.startPc.toUInt)}, bank=$bank, idx=$idx, tag=$tag\n"
    )
    XSDebug(
      RegNext(io.req.fire) && s1_reqReadHit,
      p"ITTageTableResp: bankMask=${Binary(s1_bankMask)}, idx=$s1_setIdx, hit:${s1_reqReadHit}, " +
        p"ctr:${io.resp.bits.cnt}, u:${io.resp.bits.usefulCnt}, tar:${Hexadecimal(io.resp.bits.targetOffset.offset.toUInt)}\n"
    )
    XSDebug(
      io.update.valid,
      p"update ITTAGE Table: pc:${Hexadecimal(u.startPc.toUInt)}}, bank=${updateBankIdx}, " +
        p"correct:${u.correct}, alloc:${u.alloc}, oldCtr:${u.oldCnt}, " +
        p"target:${Hexadecimal(u.targetOffset.offset.toUInt)}, old_target:${Hexadecimal(u.oldTargetOffset.offset.toUInt)}\n"
    )
    XSDebug(
      io.update.valid,
      p"update ITTAGE Table: writing bank=${updateBankIdx}, tag:${updateTag}, " +
        p"ctr: ${updateWdata.confidenceCnt}, target:${Hexadecimal(updateWdata.targetOffset.offset.toUInt)}" +
        p" in idx $updateIdx\n"
    )
    XSDebug(RegNext(io.req.fire) && !s1_reqReadHit, "TageTableResp: no hits!\n")

    // ------------------------------Debug-------------------------------------
    val valids = RegInit(VecInit.fill(numBanks)(VecInit.fill(setsPerBank)(false.B)))
    when(io.update.valid)(valids(updateBankIdx)(updateIdx) := true.B)
    XSDebug("ITTAGE Table usage:------------------------\n")
    val totalValid = valids.map(bankVec => PopCount(bankVec)).reduce(_ +& _)
    XSDebug("%d out of %d rows are valid\n", totalValid, nRows.U)
  }
}
