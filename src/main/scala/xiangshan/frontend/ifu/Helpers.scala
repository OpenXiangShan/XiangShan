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
import utility.XORFold
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.GuardedPcInit
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.BranchAttribute

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

  def genFastInfo(req: MainPipeToIfuReq): FastFetchInfo = {
    val maybeRvcMap = req.maybeRvcMap
    val firstRange  = req.firstRange
    val startVAddr  = req.info(0).startVAddr
    val predTakenMask = UIntToOH(req.info(0).takenCfiOffset.bits, FetchBlockInstNum)
    val predTaken     = req.info(0).takenCfiOffset.valid

    val info = Wire(new FastFetchInfo)
    val instrValid = Wire(Vec(5, Bool()))
    val isRvc      = Wire(Vec(5, Bool()))
    val isRvI      = Wire(Vec(5, Bool()))
    val endMask    = Wire(Vec(5, Bool()))
    val pc         = Wire(Vec(5, new GuardedPc))
    val fixedRange = Wire(Vec(5, Bool()))
    fixedRange    := ~(isRvI & predTakenMask) & firstRange
    instrValid(0) := true.B
    isRvc(0)      := maybeRvcMap(0)
    isRvI(0)      := !maybeRvcMap(0)
    endMask(0)    := instrValid(0) && maybeRvcMap(0)
    for (i <- 1 until 5) {
      instrValid(i) := (instrValid(i - 1) && maybeRvcMap(i - 1)) || !instrValid(i - 1)
      isRvc(i)      := instrValid(i) && maybeRvcMap(i)
      isRvI(i)      := instrValid(i) && !maybeRvcMap(i)
      endMask(i)    := (instrValid(i) && maybeRvcMap(i)) || (isRvI(i - 1))
    }
    for(i <- 0 until 4) {
      pc(i) := GuardedPcInit(startVAddr + i.U)
    }
    endMaskOH(0) := MuxLookup(isRvI, "b0000".U, Seq(
      "b0001".U -> "b0010".U,
      "bxxx0".U -> "b0001".U
    ))
    endMaskOH(1) := MuxLookup(isRvI, "b0000".U, Seq(
      "b0001".U -> "b0100".U,
      "b0101".U -> "b1000".U,
      "b0010".U -> "b0100".U,
      "bxx00".U -> "b0010".U
    ))
    endMaskOH(2) := MuxLookup(isRvI, "b0000".U, Seq(
      "b0001".U -> "b1000".U,
      "b0010".U -> "b1000".U,
      "b0100".U -> "b1000".U,
      "bx000".U -> "b0100".U
    ))
    endMaskOH(3) := MuxLookup(isRvI, "b0000".U, Seq(
      "b0000".U -> "b1000".U
    ))

    validMaskOH(0) := "b0001".U
    validMaskOH(1) := MuxLookup(isRvI, "b0000".U, Seq(
      "b0001".U -> "b0100".U,
      "bxxx0".U -> "b0010".U
    ))
    validMaskOH(2) := MuxLookup(isRvI, "b0000".U, Seq(
      "b0001".U -> "b1000".U,
      "b0010".U -> "b1000".U,
      "bxx00".U -> "b0100".U,
    ))
    validMaskOH(3) := MuxLookup(isRvI, "b0000".U, Seq(
      "b0000".U -> "b1000".U
    ))

    // 最多允许3条指令
    info.realInstrValid := UIntToMask(PopCount(instrValid & fixedRange)).toUInt & "b0111".U
    info.rawIsRvc    := isRvc
    info.rawIsRvI    := isRvI
    info.rawPc       := pc
    info.rawFoldedPC := pc.map{pc => XORFold(pc(VAddrBits - 1, 1), MemPredPCWidth)}
    info.endMaskOH   := endMaskOH
    info.validMaskOH := validMaskOH
    info.rawIsLastInFtqEntry := (0 until 4).map { i =>
      fixedRange(i) ^ fixedRange(i + 1)
    }
    info.ftqPtr      := req.info(0).ftqIdx
  }
}

