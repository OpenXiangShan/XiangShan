package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ParallelOR
import utility.ParallelPriorityEncoder
import xiangshan.ValidUndirectioned
import xiangshan.frontend.PcInit
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.ifu.PredCheckRedirect
import xiangshan.frontend.ifu.PreDecodeFaultType

class TracePredCheckerFlatResp(implicit p: Parameters) extends TraceBundle {
  class S1Out(implicit p: Parameters) extends TraceBundle {
    val fixedTwoFetchRange = UInt(IBufferEnqueueWidth.W)
    val fixedTwoFetchTaken = UInt(IBufferEnqueueWidth.W)
  }
  class S2Out(implicit p: Parameters) extends TraceBundle {
    val checkerRedirect: Valid[PredCheckRedirect] = Valid(new PredCheckRedirect)
    val perfFaultType:   Vec[UInt]                = Vec(FetchPorts, PreDecodeFaultType())
  }
  val stage1Out: S1Out = new S1Out
  val stage2Out: S2Out = new S2Out
}

class TracePredictCheckerIO(implicit p: Parameters) extends TraceBundle {
  val wbEnable      = Input(Bool())
  val decoded       = Input(Vec(IBufferEnqueueWidth, new TraceDecodedEntry))
  val predictInfo   = Input(new TracePredictInfo())
  val traceRange    = Input(UInt(trtl.TracePredictWidth.W))
  val concede2Bytes = Input(Bool())
  val out           = Output(new TracePredCheckerFlatResp)
}

class TracePredictChecker(implicit p: Parameters) extends TraceModule {
  val io = IO(new TracePredictCheckerIO())

  io.out := 0.U.asTypeOf(io.out)

  private val width       = IBufferEnqueueWidth
  private val pds         = VecInit(io.decoded.map(_.pd))
  private val pcs         = VecInit(io.decoded.map(_.compact.traceInfo.pcVA))
  private val instrValid  = VecInit(io.decoded.map(_.compact.valid))
  private val endOffsets  = VecInit(io.decoded.map(_.compact.instrEndOffset))
  private val selectBlock = VecInit(io.decoded.map(_.compact.selectBlock))

  private def traceAddrToPruned(addr: UInt): PrunedAddr =
    PrunedAddrInit(addr(VAddrBits - 1, 0))

  private def traceAddrToGuarded(addr: UInt) =
    PcInit(addr(VAddrBits - 1, 0)).signGuard

  private val isPredTaken = VecInit.tabulate(width) { i =>
    val selected = Mux(selectBlock(i), io.predictInfo.block(1), io.predictInfo.block(0))
    instrValid(i) && selected.valid && selected.ftqOffset.valid &&
    selected.ftqOffset.bits === endOffsets(i)
  }

  private val predictedEndIsValid = VecInit.tabulate(FetchPorts) { block =>
    VecInit.tabulate(width)(i =>
      instrValid(i) && selectBlock(i) === block.B &&
        io.predictInfo.block(block).ftqOffset.valid &&
        endOffsets(i) === io.predictInfo.block(block).ftqOffset.bits
    ).asUInt.orR
  }
  private val invalidTakenLane = VecInit.tabulate(FetchPorts) { block =>
    PopCount(VecInit.tabulate(width) { i =>
      if (block == 0) {
        instrValid(i) && !selectBlock(i) &&
        endOffsets(i) < io.predictInfo.block(block).ftqOffset.bits
      } else {
        instrValid(i) && (
          !selectBlock(i) || endOffsets(i) < io.predictInfo.block(block).ftqOffset.bits
        )
      }
    })
  }
  private val invalidTaken = VecInit.tabulate(FetchPorts) { block =>
    val info         = io.predictInfo.block(block)
    val logicalTaken = Mux(block.U === 1.U, io.predictInfo.block(0).size + info.ftqOffset.bits, info.ftqOffset.bits)
    info.valid && info.ftqOffset.valid && !predictedEndIsValid(block) &&
    info.instRange(info.ftqOffset.bits) &&
    (io.traceRange(logicalTaken) || io.concede2Bytes)
  }
  private val invalidTakenVec = VecInit.tabulate(width) { i =>
    VecInit.tabulate(FetchPorts)(block => invalidTaken(block) && invalidTakenLane(block) === i.U).asUInt.orR
  }

  private val jalFaultVec = VecInit.tabulate(width)(i =>
    pds(i).isJal && instrValid(i) && !isPredTaken(i)
  )
  private val jalrFaultVec = VecInit.tabulate(width)(i =>
    pds(i).isJalr && !pds(i).brAttribute.hasPop && instrValid(i) && !isPredTaken(i)
  )
  private val retFaultVec = VecInit.tabulate(width)(i =>
    pds(i).brAttribute.hasPop && instrValid(i) && !isPredTaken(i)
  )
  private val notCfiTaken = VecInit.tabulate(width)(i =>
    instrValid(i) && isPredTaken(i) && pds(i).notCFI
  )
  private val remaskFault = VecInit.tabulate(width)(i =>
    jalFaultVec(i) || jalrFaultVec(i) || retFaultVec(i) || notCfiTaken(i) || invalidTakenVec(i)
  )
  private val needRemask = ParallelOR(remaskFault)
  private val remaskIdx  = ParallelPriorityEncoder(remaskFault)
  private val fixedRange = VecInit.tabulate(width)(i =>
    instrValid(i) && (!needRemask || i.U <= remaskIdx)
  )

  private val fixedTwoFetchTaken = VecInit.tabulate(width)(i =>
    instrValid(i) && (
      pds(i).brAttribute.hasPop || pds(i).isJal || pds(i).isJalr ||
        (isPredTaken(i) && !pds(i).notCFI)
    )
  )
  io.out.stage1Out.fixedTwoFetchRange := fixedRange.asUInt
  io.out.stage1Out.fixedTwoFetchTaken := fixedTwoFetchTaken.asUInt

  private val jumpTargets = VecInit.tabulate(width)(i =>
    if (trtl.TraceOverrideTarget) traceAddrToGuarded(io.decoded(i).compact.traceInfo.target)
    else traceAddrToGuarded(pcs(i) + io.decoded(i).jumpOffset.toUInt)
  )
  private val seqTargets = VecInit.tabulate(width)(i =>
    traceAddrToGuarded(pcs(i) + Mux(pds(i).isRVC || !pds(i).valid, 2.U, 4.U))
  )
  private val predictedTargets = VecInit.tabulate(width)(i =>
    PcInit(Mux(
      selectBlock(i),
      io.predictInfo.block(1).nextStartAddr,
      io.predictInfo.block(0).nextStartAddr
    )).signGuard
  )
  private val targetFaultVec = VecInit.tabulate(width)(i =>
    fixedRange(i) && (pds(i).isJal || pds(i).isBr) && isPredTaken(i) &&
      jumpTargets(i) =/= predictedTargets(i)
  )
  private val stage1Fault = VecInit.tabulate(width)(i =>
    remaskFault(i) || targetFaultVec(i)
  )

  private val mispredIdx = WireDefault(
    0.U.asTypeOf(ValidUndirectioned(UInt(log2Ceil(IBufferEnqueueWidth).W)))
  )
  mispredIdx.valid := ParallelOR(stage1Fault)
  mispredIdx.bits  := ParallelPriorityEncoder(stage1Fault)

  private val mispredCompactIdx    = mispredIdx.bits
  private val selectedInvalidBlock = WireDefault(false.B)
  private val selectedInvalidTaken = WireDefault(false.B)
  for (block <- 0 until FetchPorts) {
    when(invalidTaken(block) && invalidTakenLane(block) === mispredCompactIdx) {
      selectedInvalidTaken := true.B
      selectedInvalidBlock := block.B
    }
  }
  private val selectedInvalidInfo = Mux(
    selectedInvalidBlock,
    io.predictInfo.block(1),
    io.predictInfo.block(0)
  )
  private val invalidTakenPc = selectedInvalidInfo.startAddr + (selectedInvalidInfo.ftqOffset.bits << 1)

  private val fixedIsJump = instrValid(mispredCompactIdx) && mispredIdx.valid && !selectedInvalidTaken &&
    (pds(mispredCompactIdx).isJal || pds(mispredCompactIdx).isBr ||
      pds(mispredCompactIdx).isJalr || pds(mispredCompactIdx).brAttribute.hasPop)
  private val fixedTarget = Mux(
    selectedInvalidTaken,
    traceAddrToGuarded(invalidTakenPc + 2.U),
    Mux(fixedIsJump, jumpTargets(mispredCompactIdx), seqTargets(mispredCompactIdx))
  )
  private val fixedTaken        = !selectedInvalidTaken && fixedTwoFetchTaken(mispredCompactIdx)
  private val selectedIsRVC     = Mux(selectedInvalidTaken, true.B, pds(mispredCompactIdx).isRVC)
  private val selectedAttribute = Mux(selectedInvalidTaken, BranchAttribute.None, pds(mispredCompactIdx).brAttribute)
  private val selectedMispredPc = Mux(
    selectedInvalidTaken,
    traceAddrToPruned(invalidTakenPc),
    traceAddrToPruned(pcs(mispredCompactIdx))
  )
  private val selectedEndOffset = Mux(
    selectedInvalidTaken,
    selectedInvalidInfo.ftqOffset.bits,
    endOffsets(mispredCompactIdx)
  )
  private val selectedBlock = Mux(
    selectedInvalidTaken,
    selectedInvalidBlock,
    selectBlock(mispredCompactIdx)
  )

  private val s2MispredIdx   = RegEnable(mispredIdx, io.wbEnable)
  private val s2FixedTarget  = RegEnable(fixedTarget, io.wbEnable)
  private val s2FixedTaken   = RegEnable(fixedTaken, io.wbEnable)
  private val s2IsRVC        = RegEnable(selectedIsRVC, io.wbEnable)
  private val s2InvalidTaken = RegEnable(selectedInvalidTaken, io.wbEnable)
  private val s2Attribute    = RegEnable(selectedAttribute, io.wbEnable)
  private val s2MispredPc    = RegEnable(selectedMispredPc, io.wbEnable)
  private val s2EndOffset    = RegEnable(selectedEndOffset, io.wbEnable)
  private val s2SelectBlock  = RegEnable(selectedBlock, io.wbEnable)
  private val s2FaultType = RegEnable(
    MuxCase(
      PreDecodeFaultType.NoFault,
      Seq(
        selectedInvalidTaken              -> PreDecodeFaultType.InvalidTaken,
        jalFaultVec(mispredCompactIdx)    -> PreDecodeFaultType.JalFault,
        jalrFaultVec(mispredCompactIdx)   -> PreDecodeFaultType.JalrFault,
        retFaultVec(mispredCompactIdx)    -> PreDecodeFaultType.RetFault,
        targetFaultVec(mispredCompactIdx) -> PreDecodeFaultType.TargetFault,
        notCfiTaken(mispredCompactIdx)    -> PreDecodeFaultType.NotCfiFault
      )
    ),
    io.wbEnable
  )
  private val s2WbValid = RegNext(io.wbEnable, init = false.B)

  io.out.stage2Out.checkerRedirect.valid             := s2MispredIdx.valid && s2WbValid
  io.out.stage2Out.checkerRedirect.bits.target       := s2FixedTarget
  io.out.stage2Out.checkerRedirect.bits.misIdx       := s2MispredIdx
  io.out.stage2Out.checkerRedirect.bits.taken        := s2FixedTaken
  io.out.stage2Out.checkerRedirect.bits.invalidTaken := s2InvalidTaken
  io.out.stage2Out.checkerRedirect.bits.isRVC        := s2IsRVC
  io.out.stage2Out.checkerRedirect.bits.blockSel     := s2SelectBlock
  io.out.stage2Out.checkerRedirect.bits.attribute    := s2Attribute
  io.out.stage2Out.checkerRedirect.bits.mispredPc    := s2MispredPc
  io.out.stage2Out.checkerRedirect.bits.endOffset    := s2EndOffset
  io.out.stage2Out.perfFaultType.zipWithIndex.foreach { case (faultType, block) =>
    faultType := Mux(
      s2WbValid && s2MispredIdx.valid && s2SelectBlock === block.B,
      s2FaultType,
      PreDecodeFaultType.NoFault
    )
  }
}
