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
  val tokenIdOfInstr = Wire(Vec(RenameWidth, UInt(log2Ceil(RenameWidth).W)))
  val tokenMaskFromStart = Wire(Vec(RenameWidth, UInt(RenameWidth.W)))
  val hasDestFromStart = VecInit(io.in.map(x => x.valid && (x.bits.rfWen || x.bits.fpWen)))

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

  // Each lane contributes a unary Boolean transform to the open-pair state:
  // identity for non-starts, reset for no-compress starts, and toggle otherwise.
  // Function composition is associative, so offsets 1/2/4 form a logarithmic prefix.
  var pairStatePrefix = (0 until RenameWidth).map { i =>
    val isPairableStart = tokenStart(i) && !noCompressTypeVec(i)
    (!noCompressTypeVec(i), isPairableStart)
  }
  for (distance <- Iterator.iterate(1)(_ << 1).takeWhile(_ < RenameWidth)) {
    val previousStage = pairStatePrefix
    pairStatePrefix = (0 until RenameWidth).map { i =>
      if (i < distance) {
        previousStage(i)
      } else {
        val earlier = previousStage(i - distance)
        val later = previousStage(i)
        (
          later._1 && earlier._1,
          (later._1 && earlier._2) ^ later._2
        )
      }
    }
  }
  val openPairBeforeLane = VecInit((0 until RenameWidth).map { i =>
    if (i == 0) false.B else pairStatePrefix(i - 1)._2
  })
  val tokenIsSecondAtStart = VecInit((0 until RenameWidth).map { i =>
    tokenStart(i) && !noCompressTypeVec(i) && openPairBeforeLane(i)
  })

  val tokenMask = Wire(Vec(RenameWidth, UInt(RenameWidth.W)))
  val tokenNoCompress = Wire(Vec(RenameWidth, Bool()))
  val tokenSimple = Wire(Vec(RenameWidth, Bool()))
  val tokenComplex = Wire(Vec(RenameWidth, Bool()))
  val tokenComplexHasDest = Wire(Vec(RenameWidth, Bool()))
  val tokenIsSecond = Wire(Vec(RenameWidth, Bool()))
  for (t <- 0 until RenameWidth) {
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
    tokenIsSecond(t) := tokenSelAtStart.zip(tokenIsSecondAtStart).map { case (sel, second) =>
      sel && second
    }.reduce(_ || _)
  }

  val unsafeMaskBits = Cat(io.in.zip(allowInterruptsVec).map { case (in, allow) => in.valid && !allow }.reverse)
  val actualMaskBits = Cat(actualValidVec.reverse)
  val storeMaskBits = Cat(io.in.map(in => in.valid && FuType.isStore(in.bits.fuType)).reverse)
  val flushMaskBits = Cat(io.in.zip(slotNeedsFlushVec).map { case (in, flush) => in.valid && flush }.reverse)
  val rvcMaskBits = Cat(io.in.map(in => in.valid && in.bits.isRVC).reverse)

  val entryMaskByToken = Wire(Vec(RenameWidth, UInt(RenameWidth.W)))
  val entryFormerMaskByToken = Wire(Vec(RenameWidth, UInt(RenameWidth.W)))
  val entryLatterMaskByToken = Wire(Vec(RenameWidth, UInt(RenameWidth.W)))
  val entryActualMaskByToken = Wire(Vec(RenameWidth, UInt(RenameWidth.W)))
  val entryPairTypeByToken = Wire(Vec(RenameWidth, CompressType()))
  val entrySlotNeedFlushMaskByToken = Wire(Vec(RenameWidth, UInt(2.W)))
  val entryInterruptSafeByToken = Wire(Vec(RenameWidth, Bool()))
  val entrySlotHeadRvcMaskByToken = Wire(Vec(RenameWidth, UInt(2.W)))
  val entryHasStoreByToken = Wire(Vec(RenameWidth, Bool()))
  val entryComplexSlotHasDestByToken = Wire(Vec(RenameWidth, UInt(1.W)))
  val entryNoCompressReasonByToken = Wire(Vec(RenameWidth, NoCompressReason()))

  for (t <- 0 until RenameWidth) {
    val isSecond = tokenIsSecond(t)
    val previousMask = if (t == 0) 0.U(RenameWidth.W) else tokenMask(t - 1)
    val nextMask = if (t == RenameWidth - 1) 0.U(RenameWidth.W) else tokenMask(t + 1)
    val nextIsSecond = if (t == RenameWidth - 1) false.B else tokenIsSecond(t + 1)

    entryFormerMaskByToken(t) := Mux(isSecond, previousMask, tokenMask(t))
    entryLatterMaskByToken(t) := Mux(isSecond, tokenMask(t), Mux(nextIsSecond, nextMask, 0.U))
    entryMaskByToken(t) := entryFormerMaskByToken(t) | entryLatterMaskByToken(t)
    entryActualMaskByToken(t) := entryMaskByToken(t) & actualMaskBits

    val previousIsComplex = if (t == 0) false.B else tokenComplex(t - 1)
    val previousIsSimple = if (t == 0) false.B else tokenSimple(t - 1)
    val previousNoCompress = if (t == 0) false.B else tokenNoCompress(t - 1)
    val previousComplexHasDest = if (t == 0) false.B else tokenComplexHasDest(t - 1)
    val nextIsComplex = if (t == RenameWidth - 1) false.B else tokenComplex(t + 1)
    val nextIsSimple = if (t == RenameWidth - 1) false.B else tokenSimple(t + 1)
    val nextComplexHasDest = if (t == RenameWidth - 1) false.B else tokenComplexHasDest(t + 1)

    val formerIsComplex = Mux(isSecond, previousIsComplex, tokenComplex(t))
    val formerIsSimple = Mux(isSecond, previousIsSimple, tokenSimple(t))
    val latterIsComplex = Mux(isSecond, tokenComplex(t), nextIsSecond && nextIsComplex)
    val latterIsSimple = Mux(isSecond, tokenSimple(t), nextIsSecond && nextIsSimple)
    val formerNoCompress = Mux(isSecond, previousNoCompress, tokenNoCompress(t))
    val formerComplexHasDest = Mux(isSecond, previousComplexHasDest, tokenComplexHasDest(t))
    val latterComplexHasDest = Mux(isSecond, tokenComplexHasDest(t), nextIsSecond && nextComplexHasDest)
    val hasLatter = entryLatterMaskByToken(t).orR

    entryPairTypeByToken(t) := Mux(
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
    entrySlotNeedFlushMaskByToken(t) := Cat(
      (entryLatterMaskByToken(t) & flushMaskBits).orR,
      (entryFormerMaskByToken(t) & flushMaskBits).orR
    )
    entryInterruptSafeByToken(t) := !(entryMaskByToken(t) & unsafeMaskBits).orR
    val formerFirstMask = PriorityEncoderOH(entryFormerMaskByToken(t))
    val latterFirstMask = PriorityEncoderOH(entryLatterMaskByToken(t))
    entrySlotHeadRvcMaskByToken(t) := Cat(
      (latterFirstMask & rvcMaskBits).orR,
      (formerFirstMask & rvcMaskBits).orR
    )
    entryHasStoreByToken(t) := (entryMaskByToken(t) & storeMaskBits).orR
    entryComplexSlotHasDestByToken(t) := Mux(
      formerIsComplex,
      formerComplexHasDest,
      Mux(latterIsComplex, latterComplexHasDest, false.B)
    ).asUInt
    entryNoCompressReasonByToken(t) := Mux(
      hasLatter || PopCount(entryFormerMaskByToken(t)) > 1.U,
      NoCompressReason.compressed,
      Mux(formerNoCompress, NoCompressReason.cannotCompress, NoCompressReason.noEnoughInstr)
    )
  }

  for (i <- 0 until RenameWidth) {
    when(actualValidVec(i)) {
      val tokenId = tokenIdOfInstr(i)
      val eActualMask = entryActualMaskByToken(tokenId)
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
      io.out.formerSlotMask(i)       := entryFormerMaskByToken(tokenId)
      io.out.latterSlotMask(i)       := entryLatterMaskByToken(tokenId)
      io.out.entryPairType(i)        := entryPairTypeByToken(tokenId)
      io.out.slotNeedFlushMask(i)    := entrySlotNeedFlushMaskByToken(tokenId)
      io.out.interruptSafe(i)        := entryInterruptSafeByToken(tokenId)
      io.out.slotHeadRvcMask(i)      := entrySlotHeadRvcMaskByToken(tokenId)
      io.out.complexSlotHasDest(i)   := entryComplexSlotHasDestByToken(tokenId)
      io.out.entryHasStore(i)        := entryHasStoreByToken(tokenId)
      io.out.noCompressReason(i)     := entryNoCompressReasonByToken(tokenId)
    }
  }
}
