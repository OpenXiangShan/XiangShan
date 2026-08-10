/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] Fernando Latorre, Grigorios Magklis, Jose González, Pedro Chaparro, and Antonio González. "[Crob: implementing a
* large instruction window through compression.](https://doi.org/10.1007/978-3-642-19448-1_7)" Transactions on
* High-Performance Embedded Architectures and Compilers III: 115-134. Berlin, Heidelberg: Springer Berlin Heidelberg.
* 2011.
***************************************************************************************/

package xiangshan.backend.rename

import org.chipsalliance.cde.config.Parameters
import chisel3.Bundle
import xiangshan.backend.Bundles.DecodeOutUop
import xiangshan.XSModule
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.FuType

object CompressType {
  def NORMAL = "b00".U // Complex/Simple/Simplesss
  def SC     = "b01".U // Simplesss + Complex
  def CS     = "b10".U // Complex + Simplesss
  def CC     = "b11".U // Complex + Complex

  def apply() = UInt(2.W)

  def isNORMAL(entryPairType: UInt): Bool = entryPairType === NORMAL
  def isCC(entryPairType: UInt): Bool = entryPairType === CC
  def isCS(entryPairType: UInt): Bool = entryPairType === CS
  def isSC(entryPairType: UInt): Bool = entryPairType === SC
  def isNotNORMAL(entryPairType: UInt): Bool = entryPairType =/= NORMAL

  def isLoadStore(commitType: UInt): Bool = commitType(1)
  def lsInstIsStore(commitType: UInt): Bool = commitType(0)
  def isStore(commitType: UInt): Bool = isLoadStore(commitType) && lsInstIsStore(commitType)
  def isBranch(commitType: UInt): Bool = commitType(0) && !commitType(1)
}

object NoCompressReason {
  def compressed        = "b00".U
  def noEnoughInstr     = "b01".U
  def flushedHalf       = "b10".U
  def cannotCompress    = "b11".U

  def apply() = UInt(2.W)
}

class CompressUnit(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val in = Vec(RenameWidth, Flipped(Valid(new DecodeOutUop)))
    // `in.valid` keeps architectural members that decode fusion later removes.
    // `actualValid` identifies the uops that are really emitted by Rename.
    val actualValid = Vec(RenameWidth, Input(Bool()))
    val forceNoCompress = Input(Bool())
    val out = new Bundle {
      val isEntryTailLane = Vec(RenameWidth, Output(Bool()))
      val isEntryHeadLane = Vec(RenameWidth, Output(Bool()))
      val formerSlotMask = Vec(RenameWidth, Output(UInt(RenameWidth.W)))
      val latterSlotMask = Vec(RenameWidth, Output(UInt(RenameWidth.W)))
      val entryPairType = Vec(RenameWidth, CompressType())
      val slotNeedFlushMask = Vec(RenameWidth, Output(UInt(2.W)))
      val interruptSafe = Vec(RenameWidth, Output(Bool()))
      val slotHeadRvcMask = Vec(RenameWidth, Output(UInt(2.W)))
      val complexSlotHasDest = Vec(RenameWidth, Output(UInt(1.W)))
      val entryHasStore = Vec(RenameWidth, Output(Bool()))
      val noCompressReason = Vec(RenameWidth, NoCompressReason())
    }
  })

  val slotNeedsFlushVec = io.in.map { x =>
    x.valid && (x.bits.exceptionVec.orR || TriggerAction.isDmode(x.bits.trigger) || x.bits.flushPipe)
  }

  val allowInterruptsVec = io.in.map{ x =>
    !CommitType.isLoadStore(x.bits.commitType) && !FuType.isFence(x.bits.fuType) && !FuType.isCsr(x.bits.fuType) && !FuType.isVset(x.bits.fuType) && !FuType.isAMO(x.bits.fuType)
  }

  val cannotCompressVec = VecInit(io.in.map{ x =>
    // The current VTypeBuffer tracks one commit token per vector-state uop,
    // while a compressed ROB entry exposes only one entry-level needVTB bit.
    // Keep vset and vector-memory instructions in independent entries so no
    // VTypeBuffer token can be hidden in the latter slot. A split instruction
    // must also keep the target ROB's existing firstUop/lastUop allocation
    // semantics instead of treating each emitted uop as an architectural slot.
    x.valid && (x.bits.waitForward || x.bits.blockBackward ||
      FuType.isVset(x.bits.fuType) || FuType.isVArithMem(x.bits.fuType) ||
      !x.bits.firstUop || !x.bits.lastUop)
  })

  for (i <- 0 until RenameWidth) {
    io.out.isEntryTailLane(i)      := false.B
    io.out.isEntryHeadLane(i)     := false.B
    io.out.formerSlotMask(i)       := 0.U
    io.out.latterSlotMask(i)       := 0.U
    io.out.entryPairType(i)        := CompressType.NORMAL
    io.out.slotNeedFlushMask(i)    := 0.U
    io.out.interruptSafe(i)        := true.B
    io.out.slotHeadRvcMask(i)      := 0.U
    io.out.complexSlotHasDest(i)   := 0.U
    io.out.entryHasStore(i)        := false.B
    io.out.noCompressReason(i)     := NoCompressReason.compressed
  }

  val validVec = VecInit(io.in.map(_.valid))
  val actualValidVec = VecInit(io.actualValid.zip(validVec).map { case (actual, arch) => actual && arch })
  // TODO: move it to decode
  val isCboVec = VecInit(io.in.map(x => x.valid && FuType.isStore(x.bits.fuType) && LSUOpType.isCboAll(x.bits.fuOpType)))
  val rawNoCompressTypeVec = VecInit(io.in.zip(isCboVec).zip(cannotCompressVec).zip(slotNeedsFlushVec).map { case (((x, isCbo), cannotCompress), slotNeedsFlush) =>
    x.valid && (io.forceNoCompress ||
      FuType.isVArithMem(x.bits.fuType) ||
      FuType.isVset(x.bits.fuType) ||
      FuType.isCsr(x.bits.fuType) ||
      FuType.isFence(x.bits.fuType) ||
      FuType.isAMO(x.bits.fuType) ||
      isCbo ||
      cannotCompress ||
      slotNeedsFlush
    )
  })
  val noCompressTypeVec = rawNoCompressTypeVec
  val simpleVec = VecInit((0 until RenameWidth).map { i =>
    io.in(i).valid && io.in(i).bits.simple && !noCompressTypeVec(i)
  })
  val complexVec = VecInit((0 until RenameWidth).map(i =>
    validVec(i) && !noCompressTypeVec(i) && !simpleVec(i)
  ))

  val simpleStart = Wire(Vec(RenameWidth, Bool()))
  val tokenStart = Wire(Vec(RenameWidth, Bool()))
  val tokenIdOfInstr = Wire(Vec(RenameWidth, UInt(log2Ceil(RenameWidth + 1).W)))
  val tokenMaskFromStart = Wire(Vec(RenameWidth, UInt(RenameWidth.W)))
  val hasDestFromStart = VecInit(io.in.map(x => x.valid && (x.bits.rfWen || x.bits.fpWen)))
  val tokenCount = PopCount(tokenStart)

  for (i <- 0 until RenameWidth) {
    val prevSimple = if (i == 0) false.B else simpleVec(i - 1)
    simpleStart(i) := simpleVec(i) && !prevSimple
    tokenStart(i) := validVec(i) && (simpleStart(i) || complexVec(i) || noCompressTypeVec(i))
    tokenIdOfInstr(i) := Mux(validVec(i), PopCount(tokenStart.take(i + 1)) - 1.U, 0.U)

    val simpleCont = Wire(Vec(RenameWidth, Bool()))
    for (j <- 0 until RenameWidth) {
      if (j < i) {
        simpleCont(j) := false.B
      } else if (j == i) {
        simpleCont(j) := simpleVec(i)
      } else {
        simpleCont(j) := simpleCont(j - 1) && simpleVec(j)
      }
    }
    val simpleRunMask = Cat(simpleCont.reverse)
    tokenMaskFromStart(i) := Mux(simpleStart(i), simpleRunMask, 1.U(RenameWidth.W) << i)
  }

  val tokenValid = Wire(Vec(RenameWidth, Bool()))
  val tokenMask = Wire(Vec(RenameWidth, UInt(RenameWidth.W)))
  val tokenNoCompress = Wire(Vec(RenameWidth, Bool()))
  val tokenSimple = Wire(Vec(RenameWidth, Bool()))
  val tokenComplex = Wire(Vec(RenameWidth, Bool()))
  val tokenComplexHasDest = Wire(Vec(RenameWidth, Bool()))
  for (t <- 0 until RenameWidth) {
    tokenValid(t) := t.U < tokenCount
    val tokenSelAtStart = (0 until RenameWidth).map(i => tokenStart(i) && tokenIdOfInstr(i) === t.U)
    tokenMask(t) := tokenSelAtStart.zip(tokenMaskFromStart).map { case (sel, mask) =>
      Mux(sel, mask, 0.U(RenameWidth.W))
    }.reduce(_ | _)
    tokenNoCompress(t) := tokenSelAtStart.zip(noCompressTypeVec).map { case (sel, noComp) =>
      sel && noComp
    }.reduce(_ || _)
    tokenSimple(t) := tokenSelAtStart.zip(simpleStart).map { case (sel, s) =>
      sel && s
    }.reduce(_ || _)
    tokenComplex(t) := tokenSelAtStart.zip(complexVec).map { case (sel, c) =>
      sel && c
    }.reduce(_ || _)
    tokenComplexHasDest(t) := tokenSelAtStart.zip(hasDestFromStart).map { case (sel, d) =>
      sel && d
    }.reduce(_ || _)
  }

  val tokenEntryId = Wire(Vec(RenameWidth, UInt(log2Ceil(RenameWidth + 1).W)))
  val tokenIsSecond = Wire(Vec(RenameWidth, Bool()))
  val entryCount = Wire(Vec(RenameWidth + 1, UInt(log2Ceil(RenameWidth + 1).W)))
  val openPair = Wire(Vec(RenameWidth + 1, Bool()))
  entryCount(0) := 0.U
  openPair(0) := false.B
  for (t <- 0 until RenameWidth) {
    val tValid = tokenValid(t)
    val tNoCompress = tokenNoCompress(t)
    val tPairable = tValid && !tNoCompress
    tokenIsSecond(t) := tPairable && openPair(t)
    tokenEntryId(t) := Mux(
      tokenIsSecond(t),
      Mux(entryCount(t).orR, entryCount(t) - 1.U, 0.U),
      entryCount(t)
    )
    entryCount(t + 1) := Mux(
      !tValid,
      entryCount(t),
      Mux(
        tNoCompress,
        entryCount(t) + 1.U,
        Mux(openPair(t), entryCount(t), entryCount(t) + 1.U)
      )
    )
    openPair(t + 1) := Mux(
      !tValid,
      openPair(t),
      Mux(tNoCompress, false.B, !openPair(t))
    )
  }

  val unsafeMaskBits = Cat(io.in.zip(allowInterruptsVec).map { case (in, allow) => in.valid && !allow }.reverse)
  val actualMaskBits = Cat(actualValidVec.reverse)
  val storeMaskBits = Cat(io.in.map(in => in.valid && FuType.isStore(in.bits.fuType)).reverse)
  val flushMaskBits = Cat(io.in.zip(slotNeedsFlushVec).map { case (in, flush) => in.valid && flush }.reverse)
  val rvcMaskBits = Cat(io.in.map(in => in.valid && in.bits.isRVC).reverse)

  val entryMask = Wire(Vec(RenameWidth, UInt(RenameWidth.W)))
  val entryFormerMask = Wire(Vec(RenameWidth, UInt(RenameWidth.W)))
  val entryLatterMask = Wire(Vec(RenameWidth, UInt(RenameWidth.W)))
  val entryActualMask = Wire(Vec(RenameWidth, UInt(RenameWidth.W)))
  val entryPairType = Wire(Vec(RenameWidth, CompressType()))
  val entrySlotNeedFlushMask = Wire(Vec(RenameWidth, UInt(2.W)))
  val entryInterruptSafe = Wire(Vec(RenameWidth, Bool()))
  val entrySlotHeadRvcMask = Wire(Vec(RenameWidth, UInt(2.W)))
  val entryHasStore = Wire(Vec(RenameWidth, Bool()))
  val entryComplexSlotHasDest = Wire(Vec(RenameWidth, UInt(1.W)))
  val entryNoCompressReason = Wire(Vec(RenameWidth, NoCompressReason()))

  for (e <- 0 until RenameWidth) {
    val tokenInEntry = (0 until RenameWidth).map(t => tokenValid(t) && tokenEntryId(t) === e.U)
    val tokenIsFormerInEntry = (0 until RenameWidth).map(t => tokenInEntry(t) && !tokenIsSecond(t))
    val tokenIsLatterInEntry = (0 until RenameWidth).map(t => tokenInEntry(t) && tokenIsSecond(t))

    entryMask(e) := tokenInEntry.zip(tokenMask).map { case (sel, mask) =>
      Mux(sel, mask, 0.U(RenameWidth.W))
    }.reduce(_ | _)
    entryFormerMask(e) := tokenIsFormerInEntry.zip(tokenMask).map { case (sel, mask) =>
      Mux(sel, mask, 0.U(RenameWidth.W))
    }.reduce(_ | _)
    entryLatterMask(e) := tokenIsLatterInEntry.zip(tokenMask).map { case (sel, mask) =>
      Mux(sel, mask, 0.U(RenameWidth.W))
    }.reduce(_ | _)
    entryActualMask(e) := entryMask(e) & actualMaskBits
    val hasLatter = entryLatterMask(e).orR
    val formerIsComplex = tokenIsFormerInEntry.zip(tokenComplex).map { case (sel, c) => sel && c }.reduce(_ || _)
    val formerIsSimple = tokenIsFormerInEntry.zip(tokenSimple).map { case (sel, s) => sel && s }.reduce(_ || _)
    val latterIsComplex = tokenIsLatterInEntry.zip(tokenComplex).map { case (sel, c) => sel && c }.reduce(_ || _)
    val latterIsSimple = tokenIsLatterInEntry.zip(tokenSimple).map { case (sel, s) => sel && s }.reduce(_ || _)
    val formerNoCompress = tokenIsFormerInEntry.zip(tokenNoCompress).map { case (sel, n) => sel && n }.reduce(_ || _)
    val formerComplexHasDest = tokenIsFormerInEntry.zip(tokenComplexHasDest).map { case (sel, d) => sel && d }.reduce(_ || _)
    val latterComplexHasDest = tokenIsLatterInEntry.zip(tokenComplexHasDest).map { case (sel, d) => sel && d }.reduce(_ || _)

    entryPairType(e) := Mux(
      !hasLatter,
      CompressType.NORMAL,
      Mux(
        formerIsComplex && latterIsComplex,
        CompressType.CC,
        Mux(
          formerIsComplex && latterIsSimple,
          CompressType.CS,
          Mux(
            formerIsSimple && latterIsComplex,
            CompressType.SC,
            CompressType.CC
          )
        )
      )
    )
    entrySlotNeedFlushMask(e) := Cat((entryLatterMask(e) & flushMaskBits).orR, (entryFormerMask(e) & flushMaskBits).orR)
    entryInterruptSafe(e) := !(entryMask(e) & unsafeMaskBits).orR
    val formerFirstMask = PriorityEncoderOH(entryFormerMask(e))
    val latterFirstMask = PriorityEncoderOH(entryLatterMask(e))
    entrySlotHeadRvcMask(e) := Cat((latterFirstMask & rvcMaskBits).orR, (formerFirstMask & rvcMaskBits).orR)
    entryHasStore(e) := (entryMask(e) & storeMaskBits).orR
    entryComplexSlotHasDest(e) := Mux(
      formerIsComplex,
      formerComplexHasDest,
      Mux(latterIsComplex, latterComplexHasDest, false.B)
    ).asUInt
    entryNoCompressReason(e) := Mux(
      hasLatter || PopCount(entryFormerMask(e)) > 1.U,
      NoCompressReason.compressed,
      Mux(formerNoCompress, NoCompressReason.cannotCompress, NoCompressReason.noEnoughInstr)
    )
  }

  for (i <- 0 until RenameWidth) {
    when(actualValidVec(i)) {
      val tokenId = tokenIdOfInstr(i)
      val entryId = tokenEntryId(tokenId)
      val eActualMask = entryActualMask(entryId)
      val hasLaterActualInEntry = if (i == RenameWidth - 1) {
        false.B
      } else {
        eActualMask(RenameWidth - 1, i + 1).orR
      }
      val hasEarlierActualInEntry = if (i == 0) {
        false.B
      } else {
        eActualMask(i - 1, 0).orR
      }
      io.out.isEntryTailLane(i)      := !hasLaterActualInEntry
      io.out.isEntryHeadLane(i)     := !hasEarlierActualInEntry
      io.out.formerSlotMask(i)       := entryFormerMask(entryId)
      io.out.latterSlotMask(i)       := entryLatterMask(entryId)
      io.out.entryPairType(i)        := entryPairType(entryId)
      io.out.slotNeedFlushMask(i)    := entrySlotNeedFlushMask(entryId)
      io.out.interruptSafe(i)        := entryInterruptSafe(entryId)
      io.out.slotHeadRvcMask(i)      := entrySlotHeadRvcMask(entryId)
      io.out.complexSlotHasDest(i)   := entryComplexSlotHasDest(entryId)
      io.out.entryHasStore(i)        := entryHasStore(entryId)
      io.out.noCompressReason(i)     := entryNoCompressReason(entryId)
    }
  }
}
