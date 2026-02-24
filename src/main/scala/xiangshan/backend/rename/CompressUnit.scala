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
import chisel3.util.experimental.decode.EspressoMinimizer
import freechips.rocketchip.rocket.DecodeLogic
import xiangshan.ExceptionNO.{illegalInstr, selectFrontend, virtualInstr}
import xiangshan._
import xiangshan.backend.fu.FuType
import xiangshan.backend.rename

object CompressType {
  def NORMAL = "b00".U // Complex/Simple/Simplesss
  def SC     = "b01".U // Simplesss + Complex
  def CS     = "b10".U // Complex + Simplesss
  def CC     = "b11".U // Complex + Complex

  def apply() = UInt(2.W)

  def isNORMAL(compressType: UInt): Bool = compressType === NORMAL
  def isCC(compressType: UInt): Bool = compressType === CC
  def isCS(compressType: UInt): Bool = compressType === CS
  def isSC(compressType: UInt): Bool = compressType === SC
  def isNotNORMAL(compressType: UInt): Bool = compressType =/= NORMAL

  def isLoadStore(commitType: UInt): Bool = commitType(1)
  def lsInstIsStore(commitType: UInt): Bool = commitType(0)
  def isStore(commitType: UInt): Bool = isLoadStore(commitType) && lsInstIsStore(commitType)
  def isBranch(commitType: UInt): Bool = commitType(0) && !commitType(1)
}

object NoCompressSource {
  def compressed        = "b00".U
  def noEnoughInstr     = "b01".U
  def flushedHalf       = "b10".U
  def cannotCompress    = "b11".U

  def apply() = UInt(2.W)
}

class NewCompressUnit(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val in = Vec(RenameWidth, Flipped(Valid(new DecodeOutUop)))
    val out = new Bundle {
      val needRobFlags = Vec(RenameWidth, Output(Bool()))
      val instrSizes = Vec(RenameWidth, Output(UInt(log2Ceil(RenameWidth + 1).W)))
      val masks = Vec(RenameWidth, Output(UInt(RenameWidth.W)))
      val hasLastInFtqEntry = Vec(RenameWidth, Output(UInt(2.W)))
      val compressType = Vec(RenameWidth, CompressType())
      val isFormer = Vec(RenameWidth, Output(Bool()))
      val needFlush = Vec(RenameWidth, Output(UInt(2.W)))
      val interrupt_safe = Vec(RenameWidth, Output(Bool()))
      val RVC = Vec(RenameWidth, Output(UInt(2.W)))
      val complexHasDest = Vec(RenameWidth, Output(UInt(1.W)))
      val hasStore = Vec(RenameWidth, Output(Bool()))
      val noCompressSource = Vec(RenameWidth, NoCompressSource())
    }
  })

  val needFlushVec = io.in.map{ x =>
    x.valid && (Cat(selectFrontend(x.bits.exceptionVec) :+ x.bits.exceptionVec(illegalInstr) :+ x.bits.exceptionVec(virtualInstr)).orR || TriggerAction.isDmode(x.bits.trigger) || x.bits.flushPipe)
  }

  val allowInterruptsVec = io.in.map{ x =>
    !CommitType.isLoadStore(x.bits.commitType) && !FuType.isFence(x.bits.fuType) && !FuType.isCsr(x.bits.fuType) && !FuType.isVset(x.bits.fuType) && !FuType.isAMO(x.bits.fuType)
  }

  val cannotCompressVec = VecInit(io.in.map{ x =>
    x.valid && (x.bits.waitForward || x.bits.blockBackward)
  })

  for (i <- 0 until RenameWidth) {
    io.out.needRobFlags(i)      := false.B
    io.out.instrSizes(i)        := 0.U
    io.out.masks(i)             := 0.U
    io.out.hasLastInFtqEntry(i) := 0.U
    io.out.compressType(i)      := CompressType.NORMAL
    io.out.isFormer(i)          := false.B
    io.out.needFlush(i)         := 0.U
    io.out.interrupt_safe(i)    := true.B
    io.out.RVC(i)               := 0.U
    io.out.complexHasDest(i)    := 0.U
    io.out.hasStore(i)          := false.B
    io.out.noCompressSource(i)  := NoCompressSource.compressed
  }

  val validVec = VecInit(io.in.map(_.valid))
  // TODO: move it to decode
  val isCboVec = VecInit(io.in.map(x => x.valid && FuType.isStore(x.bits.fuType) && LSUOpType.isCboAll(x.bits.fuOpType)))
  val noCompressTypeVec = VecInit(io.in.zip(isCboVec).zip(cannotCompressVec).zip(needFlushVec).map { case (((x, isCbo), cannotCompress), needFlush) =>
    x.valid && (
      FuType.isVArithMem(x.bits.fuType) ||
      FuType.isVset(x.bits.fuType) ||
      FuType.isCsr(x.bits.fuType) ||
      FuType.isFence(x.bits.fuType) ||
      FuType.isAMO(x.bits.fuType) ||
      isCbo ||
      cannotCompress ||
      needFlush
    )
  })
  val simpleVec = VecInit(io.in.zip(noCompressTypeVec).map { case (x, noCompressType) =>
    x.valid && x.bits.simple && !noCompressType
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
  val storeMaskBits = Cat(io.in.map(in => in.valid && FuType.isStore(in.bits.fuType)).reverse)
  val flushMaskBits = Cat(io.in.zip(needFlushVec).map { case (in, flush) => in.valid && flush }.reverse)
  val lastFtqMaskBits = Cat(io.in.map(in => in.valid && in.bits.isLastInFtqEntry).reverse)
  val rvcMaskBits = Cat(io.in.map(in => in.valid && in.bits.isRVC).reverse)

  val entryMask = Wire(Vec(RenameWidth, UInt(RenameWidth.W)))
  val entryFormerMask = Wire(Vec(RenameWidth, UInt(RenameWidth.W)))
  val entryLatterMask = Wire(Vec(RenameWidth, UInt(RenameWidth.W)))
  val entryInstrCnt = Wire(Vec(RenameWidth, UInt(log2Ceil(RenameWidth + 1).W)))
  val entryCompressType = Wire(Vec(RenameWidth, CompressType()))
  val entryNeedFlush = Wire(Vec(RenameWidth, UInt(2.W)))
  val entryInterruptSafe = Wire(Vec(RenameWidth, Bool()))
  val entryRVC = Wire(Vec(RenameWidth, UInt(2.W)))
  val entryHasLastFtq = Wire(Vec(RenameWidth, UInt(2.W)))
  val entryHasStore = Wire(Vec(RenameWidth, Bool()))
  val entryComplexHasDest = Wire(Vec(RenameWidth, UInt(1.W)))
  val entryNoCompressSource = Wire(Vec(RenameWidth, NoCompressSource()))

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
    entryInstrCnt(e) := PopCount(entryMask(e))

    val hasLatter = entryLatterMask(e).orR
    val formerIsComplex = tokenIsFormerInEntry.zip(tokenComplex).map { case (sel, c) => sel && c }.reduce(_ || _)
    val formerIsSimple = tokenIsFormerInEntry.zip(tokenSimple).map { case (sel, s) => sel && s }.reduce(_ || _)
    val latterIsComplex = tokenIsLatterInEntry.zip(tokenComplex).map { case (sel, c) => sel && c }.reduce(_ || _)
    val latterIsSimple = tokenIsLatterInEntry.zip(tokenSimple).map { case (sel, s) => sel && s }.reduce(_ || _)
    val formerNoCompress = tokenIsFormerInEntry.zip(tokenNoCompress).map { case (sel, n) => sel && n }.reduce(_ || _)
    val formerComplexHasDest = tokenIsFormerInEntry.zip(tokenComplexHasDest).map { case (sel, d) => sel && d }.reduce(_ || _)
    val latterComplexHasDest = tokenIsLatterInEntry.zip(tokenComplexHasDest).map { case (sel, d) => sel && d }.reduce(_ || _)

    entryCompressType(e) := Mux(
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
    entryNeedFlush(e) := Cat((entryLatterMask(e) & flushMaskBits).orR, (entryFormerMask(e) & flushMaskBits).orR)
    entryInterruptSafe(e) := !(entryMask(e) & unsafeMaskBits).orR
    val formerFirstMask = PriorityEncoderOH(entryFormerMask(e))
    val latterFirstMask = PriorityEncoderOH(entryLatterMask(e))
    entryRVC(e) := Cat((latterFirstMask & rvcMaskBits).orR, (formerFirstMask & rvcMaskBits).orR)
    entryHasLastFtq(e) := Cat((entryLatterMask(e) & lastFtqMaskBits).orR, (entryFormerMask(e) & lastFtqMaskBits).orR)
    entryHasStore(e) := (entryMask(e) & storeMaskBits).orR
    entryComplexHasDest(e) := Mux(
      formerIsComplex,
      formerComplexHasDest,
      Mux(latterIsComplex, latterComplexHasDest, false.B)
    ).asUInt
    entryNoCompressSource(e) := Mux(
      hasLatter,
      NoCompressSource.compressed,
      Mux(formerNoCompress, NoCompressSource.cannotCompress, NoCompressSource.noEnoughInstr)
    )
  }

  for (i <- 0 until RenameWidth) {
    when(io.in(i).valid) {
      val tokenId = tokenIdOfInstr(i)
      val entryId = tokenEntryId(tokenId)
      val eMask = entryMask(entryId)
      val hasLaterInEntry = if (i == RenameWidth - 1) {
        false.B
      } else {
        eMask(RenameWidth - 1, i + 1).orR
      }
      io.out.needRobFlags(i)      := !hasLaterInEntry // needRobFlags is true for the lastUop
      io.out.instrSizes(i)        := entryInstrCnt(entryId)
      io.out.masks(i)             := eMask
      io.out.hasLastInFtqEntry(i) := entryHasLastFtq(entryId)
      io.out.compressType(i)      := entryCompressType(entryId)
      io.out.isFormer(i)          := !tokenIsSecond(tokenId)
      io.out.needFlush(i)         := entryNeedFlush(entryId)
      io.out.interrupt_safe(i)    := entryInterruptSafe(entryId)
      io.out.RVC(i)               := entryRVC(entryId)
      io.out.complexHasDest(i)    := entryComplexHasDest(entryId)
      io.out.hasStore(i)          := entryHasStore(entryId)
      io.out.noCompressSource(i)  := entryNoCompressSource(entryId)
    }
  }
}

class CompressUnit(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val in = Vec(RenameWidth, Flipped(Valid(new DecodeOutUop)))
    val oddFtqVec = Vec(RenameWidth, Input(Bool()))
    val out = new Bundle {
      val needRobFlags = Vec(RenameWidth, Output(Bool()))
      val instrSizes = Vec(RenameWidth, Output(UInt(log2Ceil(RenameWidth + 1).W)))
      val masks = Vec(RenameWidth, Output(UInt(RenameWidth.W)))
      val canCompressVec = Vec(RenameWidth, Output(Bool()))
    }
  })

  val noExc = io.in.map(in => !in.bits.exceptionVec.asUInt.orR && !TriggerAction.isDmode(in.bits.trigger))
  val uopCanCompress = io.in.map(_.bits.canRobCompress)
  val canCompress = io.in.zip(noExc).zip(uopCanCompress).map { case ((in, noExc), canComp) =>
    in.valid && in.bits.lastUop && noExc && canComp
  }
  val extendedCanCompress = canCompress.zip(io.in).zip(io.oddFtqVec).flatMap { case ((canComp, in), oddFtq) =>
    Seq((FuType.isBlockBackCompress(in.bits.fuType) && in.valid && backendParams.robCompressEn.B) || canComp ,canComp && !oddFtq)
  }

  val compressTable = (0 until 1 << (2 * RenameWidth)).filter { baseCandidate =>
    // check exist 01 pair
    !(0 until RenameWidth).exists { i =>
      val bitPair = (baseCandidate >> (2 * i)) & 0x3
      bitPair == 0x2
    }
  }.zipWithIndex.map{ case (keyCandidate, index) =>
    // padding 0s at each side for convenience
    val key = 0 +: (0 until RenameWidth * 2).map(idx => (keyCandidate >> idx) & 1) :+ 0
    // count 1s on the left side of key (including itself)
    def cntL(idx: Int): Int = (if (key(idx - 1) == 1) cntL(idx - 1) else 0) + key(idx)
    // count 1s on the right side of key (including itself)
    def cntR(idx: Int): Int = (if (key(idx + 1) == 1) cntR(idx + 1) else 0) + key(idx)
    // the last instruction among consecutive rob-compressed instructions is marked
    val needRobsExpand = (0 until RenameWidth * 2).map( idx => ~(key.tail(idx) & key.tail(idx + 1)) & 1)
    val needRobs = needRobsExpand.grouped(2).map(group => group.reduce(_ | _)).toIndexedSeq
    // how many instructions are rob-compressed with this instruction (including itself)
    val uopSizes = (1 to RenameWidth).map{ idx =>
      val i = idx * 2 - 1
      if (key(i) == 0) 1 else (cntL(i) + cntR(i)) / 2
    }
    // which instructions are rob-compressed with this instruction
    val masks = uopSizes.zip(1 to RenameWidth).map { case (size, idx) => // compress masks
      val i = idx * 2 - 1
      if (key(i) == 0) Seq.fill(RenameWidth)(0).updated(idx - 1, 1)
      else Seq.fill(RenameWidth)(0).patch(idx - (cntL(i) + 1)/2, Seq.fill(size)(1), size)
    }

    // for debug, don't delete
    /*
    println("[Rename.Compress]" +
      " index: "    + index +
      " i: "        + keyCandidate +
      " key: "      + key.tail.dropRight(1) +
      " needRobs: " + needRobs +
      " uopSizes: " + uopSizes +
      " masks: "    + masks.map(_.map(_.toBinaryString).reduce(_ + _))
    )
    */

    val keyBitPat = BitPat(keyCandidate.U)
    val needRobBitPats = needRobs.map(x => BitPat(x.U))
    val uopSizeBitPats = uopSizes.map(x => BitPat(x.U))
    val maskBitPats = masks.map(m => BitPat(m.foldRight(0)(_ | _ << 1).U))

    (keyBitPat -> (needRobBitPats ++ uopSizeBitPats ++ maskBitPats))
  }

  val default = Seq.fill(3 * RenameWidth)(BitPat.N())
  val decoder = DecodeLogic(VecInit(extendedCanCompress).asUInt, default, compressTable, minimizer = EspressoMinimizer)
  (io.out.needRobFlags ++ io.out.instrSizes ++ io.out.masks).zip(decoder).foreach {
    case (sink, source) => sink := source
  }
  io.out.canCompressVec := VecInit(canCompress)
}
