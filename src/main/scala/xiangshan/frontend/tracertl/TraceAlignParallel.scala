package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSError

class TraceAlignParallel(implicit p: Parameters) extends TraceModule {
  val io = IO(new TraceAlignToIFUCutIO())

  private val width       = trtl.TracePredictWidth
  private val offsetWidth = log2Ceil(width)
  private def isRVC(inst: UInt): Bool = inst(1, 0) =/= 3.U

  require(FetchPorts == 2, "TraceRTL two-fetch alignment requires two IFU fetch ports")
  require(width == io.predictInfo.block.head.instRange.getWidth && width <= IBufferEnqueueWidth)

  private val rawInsts  = io.traceInsts.bits
  private val block0    = io.predictInfo.block(0)
  private val block1    = io.predictInfo.block(1)
  private val totalSize = block0.size +& Mux(block1.valid, block1.size, 0.U)
  private val combinedRange = block0.instRange |
    ((block1.instRange << block0.size)(width - 1, 0) & Fill(width, block1.valid))

  XSError(
    io.debug_valid && totalSize > width.U,
    "TraceAlignParallel received a two-fetch window wider than 32 halfwords"
  )
  XSError(io.debug_valid && block1.valid && !block0.valid, "TraceAlignParallel received block 1 without block 0")

  private val expectedPc        = Wire(Vec(width, UInt(VAddrBits.W)))
  private val blockState        = Wire(Vec(width, Bool()))
  private val stillConsecutive  = Wire(Vec(width, Bool()))
  private val localStartOffset  = Wire(Vec(width, UInt(offsetWidth.W)))
  private val logicalStartWide  = Wire(Vec(width, UInt(VAddrBits.W)))
  private val logicalStart      = Wire(Vec(width, UInt(offsetWidth.W)))
  private val localEndOffset    = Wire(Vec(width, UInt(offsetWidth.W)))
  private val effectiveBlockSel = Wire(Vec(width, Bool()))
  private val inheritedHalfRvi  = Wire(Vec(width, Bool()))
  private val internalCrossRvi  = Wire(Vec(width, Bool()))
  private val incompleteRvi     = Wire(Vec(width, Bool()))
  private val addrInRange       = Wire(Vec(width, Bool()))
  private val positionHotVec    = Wire(Vec(width, UInt(width.W)))
  private val traceRangeVec     = Wire(Vec(width, UInt(width.W)))
  private val taken2BVec        = Wire(Vec(width, Bool()))

  for (i <- 0 until width) {
    val rawIsRvc = isRVC(rawInsts(i).inst)
    if (i == 0) {
      expectedPc(i)       := Mux(io.lastHalfValid, block0.startAddr - 2.U, block0.startAddr)
      blockState(i)       := false.B
      stillConsecutive(i) := io.traceInsts.valid && rawInsts(i).pcVA === expectedPc(i)
    } else {
      val previousSequentialPc = rawInsts(i - 1).pcVA + Mux(isRVC(rawInsts(i - 1).inst), 2.U, 4.U)
      val previousPredictedEnd = !effectiveBlockSel(i - 1) &&
        block0.ftqOffset.valid && localEndOffset(i - 1) === block0.ftqOffset.bits
      // An RVI inherited from the previous fetch block contributes only its
      // trailing halfword to block 0.  Counting the complete four-byte
      // instruction here would transition to block 1 one slot too early and
      // drop the instruction immediately following the inherited RVI.
      val previousOccupiedHalfwords = Mux(
        inheritedHalfRvi(i - 1) || isRVC(rawInsts(i - 1).inst),
        1.U,
        2.U
      )
      val previousReachesEnd = !effectiveBlockSel(i - 1) &&
        (logicalStartWide(i - 1) + previousOccupiedHalfwords >= block0.size)
      val transitionToBlock1 = block1.valid && !blockState(i - 1) &&
        (internalCrossRvi(i - 1) || previousPredictedEnd || previousReachesEnd)

      blockState(i) := blockState(i - 1) || transitionToBlock1
      expectedPc(i) := Mux(
        transitionToBlock1 && !internalCrossRvi(i - 1),
        block1.startAddr,
        previousSequentialPc
      )
      stillConsecutive(i) := stillConsecutive(i - 1) && rawInsts(i).pcVA === expectedPc(i)
    }

    inheritedHalfRvi(i) := (if (i == 0) io.lastHalfValid else false.B)

    // Keep the address-derived offsets wide until range checks are complete. Truncating
    // them to log2(width) here would make instructions beyond the window wrap around
    // and alias an earlier logical slot.
    val block0OffsetWide = (rawInsts(i).pcVA - block0.startAddr) >> 1
    val block1OffsetWide = (rawInsts(i).pcVA - block1.startAddr) >> 1
    val block0Offset     = block0OffsetWide(offsetWidth - 1, 0)
    val block1Offset     = block1OffsetWide(offsetWidth - 1, 0)
    val crossesIntoBlock1 = !blockState(i) && block1.valid && !block0.ftqOffset.valid && !rawIsRvc &&
      rawInsts(i).pcVA + 2.U === block1.startAddr &&
      (block0OffsetWide +& 1.U === block0.size)

    internalCrossRvi(i)  := crossesIntoBlock1
    effectiveBlockSel(i) := blockState(i) || crossesIntoBlock1
    localStartOffset(i) := Mux(
      effectiveBlockSel(i),
      Mux(crossesIntoBlock1, ~0.U(offsetWidth.W), block1Offset),
      block0Offset
    )
    localEndOffset(i) := Mux(crossesIntoBlock1 || inheritedHalfRvi(i), 0.U, localStartOffset(i) + !rawIsRvc)
    logicalStartWide(i) := Mux(
      inheritedHalfRvi(i),
      0.U,
      Mux(
        crossesIntoBlock1,
        block0.size - 1.U,
        Mux(effectiveBlockSel(i), block0.size + block1OffsetWide, block0OffsetWide)
      )
    )
    logicalStart(i) := logicalStartWide(i)(offsetWidth - 1, 0)

    val startsInSelectedBlock = Mux(
      effectiveBlockSel(i),
      block1.valid && (crossesIntoBlock1 || block1OffsetWide < block1.size),
      block0.valid && block0OffsetWide < block0.size
    )
    addrInRange(i) := inheritedHalfRvi(i) ||
      (startsInSelectedBlock && logicalStartWide(i) < totalSize)

    incompleteRvi(i) := !rawIsRvc && !inheritedHalfRvi(i) && !crossesIntoBlock1 && Mux(
      blockState(i),
      block1OffsetWide +& 1.U >= block1.size,
      block0OffsetWide +& 1.U >= block0.size
    )

    positionHotVec(i) := Mux(
      stillConsecutive(i) && addrInRange(i),
      UIntToOH(logicalStart(i), width),
      0.U(width.W)
    )
    traceRangeVec(i) := Mux(
      rawIsRvc || inheritedHalfRvi(i) || incompleteRvi(i),
      positionHotVec(i),
      positionHotVec(i) | (positionHotVec(i) << 1)(width - 1, 0)
    )
  }

  private val positionHotMerge = positionHotVec.reduce(_ | _)
  private val traceRangeMerge  = traceRangeVec.reduce(_ | _)
  private val zeroTraceInst    = 0.U.asTypeOf(new TraceInstrBundle())
  private val cutInsts         = Wire(Vec(width, Valid(new TraceInstrBundle)))
  private val slotEndOffset    = Wire(Vec(width, UInt(offsetWidth.W)))
  private val slotBlockSel     = Wire(Vec(width, Bool()))
  private val slotInheritedRvi = Wire(Vec(width, Bool()))
  private val slotCrossBlock   = Wire(Vec(width, Bool()))

  for (slot <- 0 until width) {
    val hitVec = positionHotVec.map(_(slot))
    XSError(
      io.debug_valid && PopCount(hitVec) > 1.U,
      s"TraceAlignParallel has multiple trace instructions at logical slot $slot"
    )

    cutInsts(slot).valid := positionHotMerge(slot) && combinedRange(slot)
    cutInsts(slot).bits  := Mux1H(hitVec, rawInsts)
    // hitVec maps a logical halfword slot back to the raw trace-instruction
    // index. Keep all per-instruction metadata in that same slot domain before
    // the subsequent slot-to-compact selection.
    slotEndOffset(slot)    := Mux1H(hitVec, localEndOffset)
    slotBlockSel(slot)     := Mux1H(hitVec, effectiveBlockSel)
    slotInheritedRvi(slot) := Mux1H(hitVec, inheritedHalfRvi)
    slotCrossBlock(slot)   := Mux1H(hitVec, internalCrossRvi)
    when(!cutInsts(slot).valid) {
      cutInsts(slot).bits := zeroTraceInst
    }

    taken2BVec(slot) := cutInsts(slot).valid && Mux1H(hitVec, incompleteRvi)
  }

  private val traceForceJump = io.traceInsts.valid && rawInsts.head.isForceJump
  when(traceForceJump) {
    cutInsts.head.valid := true.B
    cutInsts.head.bits  := rawInsts.head
  }

  private val compactSourceValid = VecInit.tabulate(width)(i =>
    cutInsts(i).valid && !taken2BVec(i)
  )
  private val compactSourceRank = VecInit.tabulate(width)(i =>
    if (i == 0) 0.U(log2Ceil(width + 1).W) else PopCount(compactSourceValid.take(i))
  )

  io.result.position.insts      := cutInsts
  io.result.position.traceRange := traceRangeMerge & combinedRange
  io.result.candidateCount      := PopCount(compactSourceValid)
  io.result.concede2Bytes       := taken2BVec.asUInt.orR
  io.result.traceForceJump      := traceForceJump

  io.result.compact.zipWithIndex.foreach { case (entry, compactIdx) =>
    val sourceOH = VecInit.tabulate(width)(i =>
      compactSourceValid(i) && compactSourceRank(i) === compactIdx.U
    )
    entry                   := 0.U.asTypeOf(new TraceCompactEntry)
    entry.valid             := sourceOH.asUInt.orR
    entry.traceInfo         := Mux1H(sourceOH, cutInsts.map(_.bits))
    entry.instrEndOffset    := Mux1H(sourceOH, slotEndOffset)
    entry.selectBlock       := Mux1H(sourceOH, slotBlockSel)
    entry.isPrevEndHalfRvi  := Mux1H(sourceOH, slotInheritedRvi)
    entry.isCrossBlockInstr := Mux1H(sourceOH, slotCrossBlock)
  }
}
