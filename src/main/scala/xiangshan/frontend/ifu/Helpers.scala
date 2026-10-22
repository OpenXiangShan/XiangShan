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

package xiangshan.frontend.ifu

import chisel3._
import chisel3.util._
import utility.SignExt
import utility.UIntToMask
import utility.XORFold
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.GuardedPcInit
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.icache.MainPipeToIfuReq

trait PreDecodeHelper extends HasIfuParameters {
  def isRVC(inst: UInt): Bool = inst(1, 0) =/= 3.U

  def getJalOffset(inst: UInt, isRvc: Bool): PrunedAddr = {
    val rvcOffset = Cat(inst(12), inst(8), inst(10, 9), inst(6), inst(7), inst(2), inst(11), inst(5, 3), 0.U(1.W))
    val rviOffset = Cat(inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W))
    val maxWidth  = rviOffset.getWidth
    PrunedAddrInit(SignExt(Mux(isRvc, SignExt(rvcOffset, maxWidth), SignExt(rviOffset, maxWidth)), GuardedVAddrBits))
  }

  def getBrOffset(inst: UInt, isRvc: Bool): PrunedAddr = {
    val rvcOffset = Cat(inst(12), inst(6, 5), inst(2), inst(11, 10), inst(4, 3), 0.U(1.W))
    val rviOffset = Cat(inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W))
    val maxWidth  = rviOffset.getWidth
    PrunedAddrInit(SignExt(Mux(isRvc, SignExt(rvcOffset, maxWidth), SignExt(rviOffset, maxWidth)), GuardedVAddrBits))
  }
}

trait IfuHelper extends HasIfuParameters with PreDecodeHelper {
  private object ShiftType {
    val NoShift     = 0.U(2.W)
    val ShiftRight1 = 1.U(2.W)
    val ShiftRight2 = 2.U(2.W)
    val ShiftRight3 = 3.U(2.W)
  }

  def mergeInstrRange(needMerge: Bool, firstRange: UInt, secondRange: UInt, firstSize: UInt): UInt =
    Mux(needMerge, (secondRange << firstSize) | firstRange, firstRange)

  def getTotalEndPos(fetchBlock: Vec[FetchBlock]): UInt = {
    val firstEndPos = fetchBlock(0).takenCfiOffset.bits
    Mux(fetchBlock(1).valid, fetchBlock(0).size + fetchBlock(1).takenCfiOffset.bits, firstEndPos)
  }

  def genPredMask(
      firstFlag:  Bool,
      firstIdx:   UInt,
      secondFlag: Bool,
      secondIdx:  UInt,
      select:     Vec[Bool]
  ): Vec[Bool] =
    VecInit.tabulate(FetchBlockInstNum) { i =>
      ((firstIdx === i.U) && !select(i) && firstFlag) || ((secondIdx === i.U) && select(i) && secondFlag)
    }

  def bitMask(index: UInt, blockSize: Int, numBlocks: Int): UInt = {
    val selectOH = UIntToOH(index)
    val blocks   = VecInit((0 until numBlocks).map(i => Mux(selectOH(i), Fill(blockSize, 1.U(1.W)), 0.U(blockSize.W))))
    Cat(blocks.reverse)
  }
  def catPC(low: UInt, high: UInt, high1: UInt): GuardedPc =
    GuardedPcInit(Mux(
      low(PcCutPoint),
      Cat(high1, low(PcCutPoint - 1, 0)),
      Cat(high, low(PcCutPoint - 1, 0))
    ))

  def getInstrPcLowerBits(instr: Instruction, fetchBlock: Vec[FetchBlock]): UInt =
    Mux(
      instr.blockSel,
      Cat(0.U(1.W), fetchBlock(1).startVAddr(PcCutPoint - 1, 0)),
      Cat(0.U(1.W), fetchBlock(0).startVAddr(PcCutPoint - 1, 0))
    ) + (instr.startOffset << 1)

  def getInstrPc(instr: Instruction, fetchBlock: Vec[FetchBlock]): GuardedPc = {
    val pcLower = getInstrPcLowerBits(instr, fetchBlock)
    Mux(
      instr.isCrossBlockInstr,
      GuardedPcInit(fetchBlock(1).startVAddr.toUInt - 2.U),
      catPC(
        pcLower,
        Mux(instr.blockSel, fetchBlock(1).pcUpperBits, fetchBlock(0).pcUpperBits),
        Mux(instr.blockSel, fetchBlock(1).pcUpperBitsPlus1, fetchBlock(0).pcUpperBitsPlus1)
      )
    )
  }

  def catPC(lowVec: Vec[UInt], high: UInt, high1: UInt): Vec[GuardedPc] =
    VecInit(lowVec.map(catPC(_, high, high1)))

  def cutICacheData(cacheline: UInt): Vec[UInt] = {
    // FIXME: !HasCExtension
    require(HasCExtension)
    val result  = Wire(Vec(ICacheLineBytes / 2, UInt(32.W)))
    val dataVec = cacheline.asTypeOf(Vec(ICacheLineBytes / 2, UInt(16.W)))
    (0 until ICacheLineBytes / 2 - 1).foreach(i =>
      result(i) := Cat(dataVec(i + 1), dataVec(i))
    )
    result(ICacheLineBytes / 2 - 1) := Cat(dataVec(0), dataVec(ICacheLineBytes / 2 - 1))
    result
  }

  def selectInstrData(
      instr:              Instruction,
      firstData:          Vec[UInt],
      secondData:         Vec[UInt],
      secondStartRviData: UInt
  ): UInt =
    Mux(
      !instr.blockSel,
      Mux(instr.isCrossBlockInstr, Cat(secondStartRviData, firstData(instr.index)(15, 0)), firstData(instr.index)),
      secondData(instr.index)
    )

  def genBaseInstrData(
      instrVec:           Vec[Instruction],
      firstData:          Vec[UInt],
      secondData:         Vec[UInt],
      secondStartRviData: UInt
  ): Vec[UInt] =
    VecInit((0 until IBufferEnqueueWidth).map { i =>
      selectInstrData(instrVec(i), firstData, secondData, secondStartRviData)
    })

  def alignData[T <: Data](indataVec: Vec[T], shiftNum: UInt, default: T): Vec[T] = {
    require(shiftNum.getWidth == 2)
    val dataVec = VecInit((0 until IBufferEnqueueWidth).map(i =>
      if (i < indataVec.length) indataVec(i) else 0.U.asTypeOf(default)
    ))
    VecInit((0 until IBufferEnqueueWidth).map { i =>
      MuxLookup(shiftNum, 0.U.asTypeOf(default))(Seq(
        ShiftType.NoShift     -> dataVec(i),
        ShiftType.ShiftRight1 -> (if (i == 0) 0.U.asTypeOf(default) else dataVec(i - 1)),
        ShiftType.ShiftRight2 -> (if (i < 2) 0.U.asTypeOf(default) else dataVec(i - 2)),
        ShiftType.ShiftRight3 -> (if (i < 3) 0.U.asTypeOf(default) else dataVec(i - 3))
      ))
    })
  }

  def alignInstrCompact(indata: InstrCompactBundle, shiftNum: UInt): InstrCompactBundle = {
    val out = Wire(new InstrCompactBundle(IBufferEnqueueWidth))
    out.instrIndex     := alignData(indata.instrIndex, shiftNum, 0.U.asTypeOf(new InstrIndexEntry))
    out.instrIsRvc     := alignData(indata.instrIsRvc, shiftNum, false.B)
    out.selectBlock    := alignData(indata.selectBlock, shiftNum, false.B)
    out.instrPcLower   := alignData(indata.instrPcLower, shiftNum, 0.U((PcCutPoint + 1).W))
    out.instrEndOffset := alignData(indata.instrEndOffset, shiftNum, 0.U(log2Ceil(FetchBlockInstNum).W))
    out
  }

  def compact(in: Vec[Instruction], fire: Bool): Vec[Instruction] = {
    val n = in.length
    require(n > 0)

    // 1. Valid flag vector
    val validVec = VecInit(in.map(_.valid))

    // rank(i) = number of valid elements in [0, i)
    val rank = VecInit((0 until n).map { i =>
      if (i == 0) 0.U
      else PopCount(validVec.take(i))
    })

    val inReg = RegEnable(in, fire)

    // 3. Output vector, default all zeros
    val out = WireDefault(0.U.asTypeOf(in))

    for (idx <- 0 until n) {
      val instrRange = idx until Math.min(2 * idx + 2, FetchBlockInstNum)
      val validOH = instrRange.map {
        i => validVec(i) && rank(i) === idx.U
      }
      val validOHReg   = RegEnable(VecInit(validOH), fire)
      val candidateReg = instrRange.map(inReg(_))
      out(idx)       := Mux1H(validOHReg, candidateReg)
      out(idx).valid := validOHReg.reduce(_ || _)
    }
    out
  }

  def align[T <: Data](in: Vec[T], shamt: UInt): Vec[T] = {
    require(IBufferEnqueueWidth - FetchBlockInstNum == 4)
    require(shamt.getWidth == 2)

    val zero = 0.U.asTypeOf(in(0))

    val s0 = VecInit(in ++ Seq.fill(4)(zero))
    val s1 = VecInit(Seq.fill(1)(zero) ++ in ++ Seq.fill(3)(zero))
    val s2 = VecInit(Seq.fill(2)(zero) ++ in ++ Seq.fill(2)(zero))
    val s3 = VecInit(Seq.fill(3)(zero) ++ in ++ Seq.fill(1)(zero))

    Mux1H(UIntToOH(shamt), Seq(s0, s1, s2, s3))
  }

  /**
    * Select the raw window used by fast fetch.
    *
    * The cache line is split into 8-byte chunks. `startIndex` is the byte offset
    * inside the line, and `startIndex(5, 3)` selects the 8-byte-aligned chunk
    * that contains the first instruction. Two adjacent chunks are concatenated
    * to form the 16-byte window consumed by [[cutFastData]]. The `b111` entry
    * indexes the chunks line-relative, so the last chunk wraps to the line start.
    */
  def genFastFetchData(cacheline: UInt, startIndex: UInt): UInt = {
    val fastFetchBitsWidth = FastFetchBytes * 8
    val windowWidth = fastFetchBitsWidth * 2
    val dataVec     = cacheline.asTypeOf(Vec(ICacheLineBytes / FastFetchBytes, UInt(fastFetchBitsWidth.W)))
    val result      = MuxLookup(startIndex(5, 3), 0.U(windowWidth.W))(Seq(
      "b000".U -> Cat(dataVec(1), dataVec(0)),
      "b001".U -> Cat(dataVec(2), dataVec(1)),
      "b010".U -> Cat(dataVec(3), dataVec(2)),
      "b011".U -> Cat(dataVec(4), dataVec(3)),
      "b100".U -> Cat(dataVec(5), dataVec(4)),
      "b101".U -> Cat(dataVec(6), dataVec(5)),
      "b110".U -> Cat(dataVec(7), dataVec(6)),
      "b111".U -> Cat(dataVec(0), dataVec(7))
    ))
    result
  }

  /**
    * Align the fast-fetch window to the first instruction and build candidate
    * 32-bit instructions.
    *
    * `startIndex(2, 1)` is the 2-byte offset inside the first chunk, so only
    * four shift amounts are possible. Keep the explicit shift table instead of
    * a variable shifter: EDA tools may infer the same logic, but this makes the
    * intended structure explicit. The shift amounts are in bits: 2/4/6 bytes
    * become 16/32/48 bits. The window is then viewed as `FastFetchWidth`
    * 16-bit slots; adjacent slots are concatenated into instructions. The last
    * slot is zero-extended because its upper half is not guaranteed to be
    * inside the window.
    */
  def cutFastData(fastFetch: UInt, startIndex: UInt): Vec[UInt] = {
    val width = FastFetchBytes * 8
    val useData = MuxLookup(startIndex(2, 1), 0.U(width.W))(Seq(
      "b00".U -> fastFetch(width - 1, 0),
      "b01".U -> (fastFetch >> 16)(width - 1, 0),
      "b10".U -> (fastFetch >> 32)(width - 1, 0),
      "b11".U -> (fastFetch >> 48)(width - 1, 0)
    ))
    val dataVec = useData.asTypeOf(Vec(FastFetchWidth, UInt(16.W)))
    val result  = Wire(Vec(FastFetchWidth, UInt(32.W)))
    for (i <- 0 until FastFetchWidth - 1) {
      result(i) := Cat(dataVec(i + 1), dataVec(i))
    }
    result(FastFetchWidth - 1) := Cat(0.U(16.W), dataVec(FastFetchWidth - 1))
    result
  }

  /**
    * Collect the fast-fetch metadata directly from the ICache request.
    *
    * The fast path bypasses the normal s2 predecode latency, so everything that
    * IBuffer and decode need must be derived here: 2-byte slot boundaries, RVC
    * information, PCs, predicted-taken CFI placement, and instruction ordering
    * inside the FTQ entry.
    */
  def genFastInfo(req: MainPipeToIfuReq): FastFetchInfo = {
    val maybeRvcMap = req.maybeRvcMap
    val firstRange  = req.firstRange
    val startVAddr  = req.info(0).startVAddr.toUInt
    val predTaken   = req.info(0).takenCfiOffset.valid
    val takenCfiIdx = req.info(0).takenCfiOffset.bits
    val altTakenCfiIdx = req.info(0).takenCfiOffset.bits - 1.U
    val ftqPtr      = req.info(0).ftqIdx

    val validMaskOH         = Wire(Vec(FastFetchMaxNum, UInt(FastFetchWidth.W)))
    val rawIsRvC            = Wire(Vec(FastFetchWidth, Bool()))
    val rawPredTaken        = Wire(Vec(FastFetchWidth, Bool()))
    val rawPc               = Wire(Vec(FastFetchWidth, new GuardedPc))
    val rawEndMask          = Wire(Vec(FastFetchWidth, Bool()))
    val rawInstrValid       = Wire(Vec(FastFetchWidth + 1, Bool()))
    val rawIsRvI            = Wire(Vec(FastFetchWidth + 1, Bool()))
    val fixedMask           = Wire(Vec(FastFetchWidth + 1, Bool()))

    // Walk the 2-byte slots from left to right. A slot starts an instruction
    // iff the previous slot did not start one, or the previous instruction was
    // RVC and therefore occupied only one slot.
    for (i <- 0 until FastFetchWidth) {
      rawInstrValid(i) := (if (i == 0) true.B else !rawInstrValid(i - 1) || maybeRvcMap(i - 1))
      rawIsRvI(i)      := rawInstrValid(i) && !maybeRvcMap(i)
      rawIsRvC(i)      := rawInstrValid(i) && maybeRvcMap(i)
      rawEndMask(i)    := !rawInstrValid(i) || rawIsRvC(i)
      fixedMask(i)     := (takenCfiIdx === i.U) && rawIsRvI(i)
      rawPredTaken(i)  := predTaken &&
        (((takenCfiIdx === i.U) && rawIsRvC(i)) || ((altTakenCfiIdx === i.U) && rawIsRvI(i)))
      rawPc(i)         := GuardedPcInit(startVAddr + (2 * i).U)
    }
    // Sentinel slot describing the instruction that ends at the window edge.
    rawInstrValid(FastFetchWidth) := !rawInstrValid(FastFetchWidth - 1) || maybeRvcMap(FastFetchWidth - 1)
    rawIsRvI(FastFetchWidth)      := rawInstrValid(FastFetchWidth) && !maybeRvcMap(FastFetchWidth)
    fixedMask(FastFetchWidth)     := (takenCfiIdx === FastFetchWidth.U) && rawIsRvI(FastFetchWidth)

    // The fast path must not resolve a predicted-taken 4-byte instruction on
    // its own: mask out the slot it would consume and leave the final boundary
    // and target to predChecker. This also covers a block that ends in the
    // middle of a 4-byte instruction, while keeping the fast-fetch width equal
    // to the final fetch width.
    val fixedRangeLow        = firstRange(FastFetchWidth - 1, 0) & ~fixedMask.asUInt(FastFetchWidth - 1, 0)
    val fixedRangeEnd        = firstRange(FastFetchWidth) & ~fixedMask(FastFetchWidth)
    val fixedRange           = Cat(fixedRangeEnd, fixedRangeLow)
    val blockEndsInFastFetch = !fixedRange(FastFetchWidth)
    // One-hot of the last valid slot in `fixedRange`.
    val lastSlotOH           = fixedRange & ~(fixedRange >> 1)

    // Start slot of each fast-fetch instruction. Instruction 0 starts at slot 0;
    // each following instruction starts after the previous RVC/RVI instruction.
    // Fast fetch returns at most FastFetchMaxNum instructions.
    validMaskOH(0) := "b0001".U
    validMaskOH(1) := Mux(rawIsRvI(0), "b0100".U, "b0010".U)
    validMaskOH(2) := Mux(
      rawIsRvI(0),
      Mux(rawIsRvI(2), "b0000".U, "b1000".U),
      Mux(rawIsRvI(1), "b1000".U, "b0100".U)
    )

    val info = Wire(new FastFetchInfo)
    // Number of instructions that end inside the fast window. Each end slot is
    // counted once, so this is the number of complete instructions to enqueue.
    val instrCount = PopCount(VecInit((0 until FastFetchWidth).map(i => rawEndMask(i) && fixedRange(i))))
    // Mark the low `instrCount` entries valid; the mask naturally clamps to
    // FastFetchMaxNum because only FastFetchMaxNum entries exist.
    info.realInstrValid := UIntToMask(instrCount, FastFetchMaxNum)
    info.rawIsPredTaken := rawPredTaken
    info.rawIsRvC       := rawIsRvC
    info.rawPc          := rawPc
    info.rawFoldedPC    := rawPc.map(pc => XORFold(pc(VAddrBits - 1, 1), MemPredPCWidth))
    info.instrNum       := Mux(instrCount > FastFetchMaxNum.U, FastFetchMaxNum.U, instrCount)
    info.validMaskOH    := validMaskOH
    info.ftqPtr         := ftqPtr
    info.startIndex     := startVAddr(log2Ceil(ICacheLineBytes) - 1, 0)
    // An instruction is the last one in its FTQ entry iff the FTQ entry ends
    // inside the fast window and this is its final slot. For RVC the final slot
    // is the instruction slot itself; for RVI it is the next (upper) slot.
    info.rawIsLastInFtqEntry := VecInit((0 until FastFetchWidth).map { i =>
      blockEndsInFastFetch && (
        (rawIsRvC(i) && lastSlotOH(i)) ||
          (if (i < FastFetchWidth - 1) rawIsRvI(i) && lastSlotOH(i + 1) else false.B)
      )
    })
    info
  }
}
