package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XORFold
import xiangshan.Redirect
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.ifu.Instruction

class TraceFromIFU(implicit p: Parameters) extends TraceBundle {
  val ifuS2Flush      = Bool()
  val ifuS1Flush      = Bool()
  val ifuS1Fire       = Bool()
  val ifuS2Fire       = Bool()
  val ifuS2Valid      = Bool()
  val ifuWbEnable     = Bool()
  val ifuS1Valid      = Bool()
  val ifuS1AlignShift = UInt(2.W)
  val ifuS1PredInfo   = Input(new TracePredictInfo())
}

/** The S2 IFU packet rebuilt from the trace; replaces the IFU's aligned fetch data. */
class TraceAlignedFetch(implicit p: Parameters) extends TraceBundle {
  val instrVec      = Vec(IBufferEnqueueWidth, new Instruction)
  val instrPcVec    = Vec(IBufferEnqueueWidth, GuardedPc())
  val instrValid    = UInt(IBufferEnqueueWidth.W)
  val instrCount    = UInt(log2Ceil(IBufferEnqueueWidth + 1).W)
  val foldPc        = Vec(IBufferEnqueueWidth, UInt(MemPredPCWidth.W))
  val pdInfoVec     = Vec(IBufferEnqueueWidth, new PreDecodeInfo)
  val jumpOffsetVec = Vec(IBufferEnqueueWidth, GuardedPc())
  val traceInfo     = Vec(IBufferEnqueueWidth, new TraceInstrBundle)
}

class TraceRTLIO(implicit p: Parameters) extends TraceBundle {
  val fromIFU = Input(new TraceFromIFU())
  val redirect = Input(new Bundle {
    val fromBackend = Valid(new Redirect())
    val fromIFUBPU  = Bool()
  })

  val checker          = Output(new TracePredCheckerFlatResp)
  val s2Block          = Output(Bool())
  val block            = Output(Bool())
  val s2CandidateCount = Output(UInt(log2Ceil(IBufferEnqueueWidth + 1).W))
  val aligned          = Output(new TraceAlignedFetch)
}

class TraceRTL(implicit p: Parameters) extends TraceModule {
  val io = IO(new TraceRTLIO)

  // Move the packet to the IBuffer slots the IFU aligned the fetch block to.
  private def shiftToIBuffer[T <: Data](data: Vec[T], shiftNum: UInt): Vec[T] = {
    require(data.length == IBufferEnqueueWidth && shiftNum.getWidth == 2)
    val zero = 0.U.asTypeOf(data.head)
    VecInit.tabulate(IBufferEnqueueWidth) { i =>
      MuxLookup(shiftNum, zero)((0 until 4).map(k => k.U -> (if (i < k) zero else data(i - k))))
    }
  }
  private def shiftToIBuffer(mask: UInt, shiftNum: UInt): UInt =
    (mask << shiftNum)(IBufferEnqueueWidth - 1, 0)

  val traceReader  = Module(new TraceReader)
  val traceDriver  = Module(new TraceDriver)
  val traceAligner = Module(new TraceAlignParallel)
  val preDecoder   = Module(new TracePreDecoder)
  val predChecker  = Module(new TracePredictChecker)

  traceReader.io.recv     := traceDriver.io.out.recv
  traceReader.io.redirect := io.redirect.fromBackend

  val pendingConcede2Bytes = RegInit(false.B)
  val nextConcede2Bytes    = Wire(Bool())

  // TraceReader exposes the post-consumption packet combinationally when S3
  // fires. Keep the concede state in that same transaction domain: the next
  // S2 packet must see the state produced by the S3 packet being consumed,
  // not the value that will remain in the register until the rising edge.
  val visibleConcede2Bytes = Mux(io.fromIFU.ifuS2Fire, nextConcede2Bytes, pendingConcede2Bytes)
  val effectiveConcede2Bytes = visibleConcede2Bytes &&
    traceReader.io.traceInsts.valid &&
    traceReader.io.traceInsts.bits.head.pcVA === io.fromIFU.ifuS1PredInfo.block(0).startAddr - 2.U &&
    !traceReader.io.traceInsts.bits.head.isRVC

  traceAligner.io.debug_valid   := io.fromIFU.ifuS1Valid
  traceAligner.io.traceInsts    := traceReader.io.traceInsts
  traceAligner.io.predictInfo   := io.fromIFU.ifuS1PredInfo
  traceAligner.io.lastHalfValid := effectiveConcede2Bytes

  preDecoder.io.compact     := traceAligner.io.result.compact
  preDecoder.io.predictInfo := io.fromIFU.ifuS1PredInfo

  val s2Block = !traceReader.io.traceInsts.valid ||
    (traceAligner.io.result.candidateCount === 0.U && !traceAligner.io.result.concede2Bytes)

  // Capture the same S2 transaction that sets IFU s3_valid at the rising edge.
  val s2ToS3 = io.fromIFU.ifuS1Fire && !io.fromIFU.ifuS1Flush

  val s3Decoded                = RegEnable(preDecoder.io.out, s2ToS3)
  val s3ShiftNum               = RegEnable(io.fromIFU.ifuS1AlignShift, 0.U(2.W), s2ToS3)
  val s3PredInfo               = RegEnable(io.fromIFU.ifuS1PredInfo, s2ToS3)
  val s3TraceRange             = RegEnable(traceAligner.io.result.traceRange, s2ToS3)
  val s3TraceForceJump         = RegEnable(traceAligner.io.result.traceForceJump, s2ToS3)
  val s3DetectedConcede2Bytes  = RegEnable(traceAligner.io.result.concede2Bytes, s2ToS3)
  val s3InheritedConcede2Bytes = RegEnable(effectiveConcede2Bytes, s2ToS3)

  // s2_fire && !s2_flush |=> wbEnable
  predChecker.io.wbEnable      := io.fromIFU.ifuWbEnable
  predChecker.io.decoded       := s3Decoded
  predChecker.io.predictInfo   := s3PredInfo
  predChecker.io.traceRange    := s3TraceRange
  predChecker.io.concede2Bytes := s3InheritedConcede2Bytes

  traceDriver.io.fire           := io.fromIFU.ifuS2Fire
  traceDriver.io.decoded        := s3Decoded
  traceDriver.io.fixedValid     := predChecker.io.out.stage1Out.fixedTwoFetchRange
  traceDriver.io.traceForceJump := s3TraceForceJump
  traceDriver.io.concede2Bytes  := s3DetectedConcede2Bytes

  // In a two-fetch transaction, block 0 may end with a taken CFI while block 1
  // contains only the first halfword of the target RVI.  That incomplete RVI
  // still has to be inherited by the next transaction even though the last
  // consumed instruction (in block 0) is a CFI.
  val concedeIsInSecondBlock = s3PredInfo.block(1).valid
  nextConcede2Bytes := s3DetectedConcede2Bytes &&
    (!traceDriver.io.out.endWithCFI || concedeIsInSecondBlock) &&
    !io.fromIFU.ifuS2Flush &&
    !io.fromIFU.ifuS1Flush

  val preserveConcedeOnInvalidTaken = pendingConcede2Bytes &&
    io.redirect.fromIFUBPU &&
    predChecker.io.out.stage2Out.checkerRedirect.valid &&
    predChecker.io.out.stage2Out.checkerRedirect.bits.invalidTaken
  val clearPendingConcede = io.fromIFU.ifuS2Flush || io.redirect.fromIFUBPU ||
    (io.fromIFU.ifuS1Flush && !io.fromIFU.ifuS2Valid)

  when(clearPendingConcede && !preserveConcedeOnInvalidTaken) {
    pendingConcede2Bytes := false.B
  }.elsewhen(io.fromIFU.ifuS2Fire) {
    pendingConcede2Bytes := nextConcede2Bytes
  }

  val s3DecodedForIFU = shiftToIBuffer(s3Decoded, s3ShiftNum)

  io.checker                              := predChecker.io.out
  io.checker.stage1Out.fixedTwoFetchRange := shiftToIBuffer(predChecker.io.out.stage1Out.fixedTwoFetchRange, s3ShiftNum)
  io.checker.stage1Out.fixedTwoFetchTaken := shiftToIBuffer(predChecker.io.out.stage1Out.fixedTwoFetchTaken, s3ShiftNum)
  io.s2Block                              := s2Block
  io.block                                := traceDriver.io.out.block
  io.s2CandidateCount                     := traceAligner.io.result.candidateCount

  private val aligned = io.aligned
  aligned.instrVec.zip(s3DecodedForIFU).foreach { case (instr, decoded) =>
    val entry = decoded.compact
    val isRvc = entry.traceInfo.isRVC
    instr.valid             := entry.valid
    instr.data              := entry.traceInfo.inst
    instr.isRvc             := isRvc
    instr.isPredTaken       := decoded.isPredTaken
    instr.invalidTaken      := false.B
    instr.blockSel          := entry.selectBlock
    instr.startOffset       := entry.instrEndOffset - Mux(isRvc, 0.U, 1.U)
    instr.index             := instr.startOffset
    instr.endOffset         := entry.instrEndOffset
    instr.isPrevEndHalfRvi  := entry.isPrevEndHalfRvi
    instr.isCrossBlockInstr := entry.isCrossBlockInstr
  }
  aligned.instrPcVec := VecInit(s3DecodedForIFU.map(d => traceGuardedPc(d.compact.traceInfo.pcVA)))
  aligned.instrValid := VecInit(s3DecodedForIFU.map(_.compact.valid)).asUInt
  aligned.instrCount := PopCount(s3DecodedForIFU.map(_.compact.valid))
  aligned.foldPc     := VecInit(aligned.instrPcVec.map(pc => XORFold(pc(VAddrBits - 1, 1), MemPredPCWidth)))
  aligned.pdInfoVec  := VecInit(s3DecodedForIFU.map(_.pd))
  aligned.jumpOffsetVec := VecInit(s3DecodedForIFU.map(_.jumpOffset))
  aligned.traceInfo     := VecInit(s3DecodedForIFU.map(_.compact.traceInfo))
}
