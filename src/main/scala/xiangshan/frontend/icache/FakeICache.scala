/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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
 * ************************************************************************************* */

package xiangshan.frontend

import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper, ParallelPosteriorityEncoder, ParallelPriorityEncoder}
import utils.XSError
import xiangshan.frontend.ChiselRecordForField._
import xiangshan.{DebugOptionsKey, XSBundle, XSModule}

// From HX's NewCSR utility
object ChiselRecordForField {
  implicit class AddRecordSpecifyFields[T <: Record](val x: T) {
    def specifyField(elemFns: (T => Unit)*): Unit = {
      elemFns.foreach(_.apply(x))
    }
  }
}

object TraceRTLChoose {
  def apply[T <: Data](f: T, t: T)(implicit p: Parameters): T = {
    val env = p(DebugOptionsKey)
    if (env.TraceRTLMode) {
      t
    } else {
      f
    }
  }
}

object TraceRTLDontCare {
  def apply[T <: Data](f: T)(implicit p: Parameters): T = {
    val env = p(DebugOptionsKey)
    if (env.TraceRTLMode) {
      f
    } else {
      0.U.asInstanceOf[T]
    }
  }
}

class TraceICacheHelper extends ExtModule
  with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val enable = IO(Input(Bool()))
  val addr = IO(Input(UInt(64.W)))
  val data0 = IO(Output(UInt(64.W)))
  val data1 = IO(Output(UInt(64.W)))
  val data2 = IO(Output(UInt(64.W)))
  val data3 = IO(Output(UInt(64.W)))
  val data4 = IO(Output(UInt(64.W)))
  val data5 = IO(Output(UInt(64.W)))
  val data6 = IO(Output(UInt(64.W)))
  val data7 = IO(Output(UInt(64.W)))
  val legal_addr = IO(Output(Bool()))
}

// Replace ICache's data
// IFU1 --fire--> IFU2
class FakeICache()(implicit p: Parameters) extends TraceModule {
  val io = IO(new Bundle {
    val req = Flipped(ValidIO(new Bundle {
      val addr = UInt(VAddrBits.W)
    }))
    val resp = Valid(new Bundle {
      val data0 = UInt(256.W)
      val data1 = UInt(256.W)
      val addr = UInt(VAddrBits.W)
    })
  })

  val helper = Module(new TraceICacheHelper)
  helper.clock := clock
  helper.enable := io.req.valid
  helper.addr := io.req.bits.addr
  io.resp.valid := helper.legal_addr(0) && RegNext(io.req.valid)
  io.resp.bits.data0 := Cat(helper.data3, helper.data2, helper.data1, helper.data0)
  io.resp.bits.data1 := Cat(helper.data7, helper.data6, helper.data5, helper.data4)
  io.resp.bits.addr := RegEnable(helper.addr, io.req.valid)
}


trait TraceParams {
  val TracePCWidth = 64
  val TraceInstrWidth = 32
  val TraceFetchWidth = 16
  val TraceBufferSize = TraceFetchWidth * 3 //
}

class TraceBundle(implicit p: Parameters) extends XSBundle with TraceParams

class TraceModule(implicit p: Parameters) extends XSModule with TraceParams

class TraceInstrBundle(implicit p: Parameters) extends TraceBundle {
  val pc = UInt(TracePCWidth.W)
  val inst = UInt(TraceInstrWidth.W)
}

object TraceInstrBundle {
  def apply(pc: UInt, inst: UInt)(implicit p: Parameters): TraceInstrBundle = {
    val bundle = Wire(new TraceInstrBundle())
    bundle.pc := pc
    bundle.inst := inst
    bundle
  }
}

class TraceRecvInfo(implicit p: Parameters) extends TraceBundle {
  val instNum = UInt(log2Ceil(PredictWidth).W)
}

class TraceReaderIO(implicit p: Parameters) extends TraceBundle {
  // traceInst should always be valid
  val traceInsts = Output(Vec(PredictWidth, new TraceInstrBundle()))
  // recv.valid from f3_fire, bits.instNum from range
  val recv = Flipped(Valid(new TraceRecvInfo()))
}

class TraceBufferPtr(Size: Int)(implicit p: Parameters) extends CircularQueuePtr[TraceBufferPtr](Size)

class TraceReader(implicit p: Parameters) extends TraceModule
  with TraceParams
  with HasCircularQueuePtrHelper {
  val io = IO(new TraceReaderIO())
  dontTouch(io)

  val traceBuffer = Reg(Vec(TraceBufferSize, new TraceInstrBundle()))
  val traceReaderHelper = Module(new TraceReaderHelper)
  val deqPtr = RegInit(0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize)))
  val enqPtr = RegInit(0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize)))

  XSError(!isFull(enqPtr, deqPtr) && (enqPtr < deqPtr), "enqPtr should always be larger than deqPtr")
  XSError(io.recv.valid && ((deqPtr.value + io.recv.bits.instNum) >= enqPtr.value),
    "Reader should not read more than what is in the buffer. Error in ReaderHelper or Ptr logic.")

  when(io.recv.valid) {
    deqPtr := deqPtr + io.recv.bits.instNum
  }

  val readTraceEnable = !isFull(enqPtr, deqPtr) && (hasFreeEntries(enqPtr, deqPtr) >= TraceFetchWidth.U)
  when(readTraceEnable) {
    enqPtr := enqPtr + TraceFetchWidth.U
    (0 until TraceFetchWidth).foreach { i => {
      bufferInsert(enqPtr + i.U, TraceInstrBundle(traceReaderHelper.pc(i), traceReaderHelper.instr(i)))
    }
    }
  }

  def bufferInsert(ptr: TraceBufferPtr, data: TraceInstrBundle) = {
    traceBuffer(ptr.value) := data
    // traceBuffer(ptr.value) := 0.U
  }

  traceReaderHelper.clock := clock
  traceReaderHelper.reset := reset
  traceReaderHelper.enable := readTraceEnable
  io.traceInsts.zipWithIndex.foreach { case (inst, i) =>
    inst := traceBuffer(deqPtr.value + i.U)
  }
}

class TraceReaderHelper extends ExtModule with TraceParams {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val enable = IO(Input(Bool()))

  val pc = IO(Output(Vec(TraceFetchWidth, UInt(64.W))))
  val instr = IO(Output(Vec(TraceFetchWidth, UInt(32.W))))
}

/**
 * TraceRange
 *
 * BPU prediction: address Range & Jump Range & PredCheckerRange
 * - Address Range:only from bpu, from startAddr to nextStartAddr
 * - Jump Range:   only from bpu, ftqOffset.valid & ftqOffset.bits
 * - PredCheckerRange: from icache, precode the Cacheline's instr,
 * to get simple prediction result from instruction codes.
 *
 * Trace Checker:
 * - Address Range: from bpu, calculate if the traceInst are in range
 * - Jump Range:   from bpu, calculate if the traceInst are in range
 * - PredCheckerRange: from trace, precode the Cacheline's instr,
 * to get simple prediction result from instruction codes.
 *
 * As a result:
 * - Replace Trace Checker with BPU prediction Checker/predecode, the range is the most important.
 * - Checker's output should be the ssame as the PredChecker but as the type of instruction.
 */

/**
 * TraceInstAlignToCut
 * Input: traceInst: pc + instr
 * Output: same with cut result in IFU
 */
class TraceAlignToIFUCutIO(implicit p: Parameters) extends TraceBundle {
  val debug_valid = Input(Bool())

  val traceInsts = Input(Vec(PredictWidth, new TraceInstrBundle()))
  // preDInfo's startAddr
  val predStartAddr = Input(UInt(VAddrBits.W))
  // instRange come from BPU's prediction: 'startAddr to nextStartAddr' & 'ftqOffset'
  val instRange = Input(UInt(PredictWidth.W))
  // When lastHalfValid is true, then the first 2bytes is used by last fetch
  // So set the first TraceInstrBundle to invalid
  val lastHalfValid = Input(Bool())

  val cutInsts = Output(Vec(PredictWidth, Valid(new TraceInstrBundle)))
  val traceRange = Output(UInt(PredictWidth.W))
  val traceRangeTaken2B = Output(Bool())
  val instRangeTaken2B = Output(Bool())
}

class TraceAlignToIFUCut(implicit p: Parameters) extends TraceModule {
  val io = IO(new TraceAlignToIFUCutIO())

  val width = io.traceInsts.size
  require(width == io.instRange.getWidth, f"Width of traceInsts ${width} and instRange ${io.instRange.getWidth} should be the same")
  val traceRangeVec = Wire(Vec(width, Bool()))
  val lastInstEndVec = Wire(Vec(PredictWidth + 1, Bool()))
  val curInstIdxVec = Wire(Vec(PredictWidth + 1, UInt(log2Ceil(width).W)))
  io.traceRange := traceRangeVec.asUInt
  io.traceRangeTaken2B := isTaken2B(io.traceRange)
  io.instRangeTaken2B := isTaken2B(io.instRange)
  val startPC = io.predStartAddr

  def isTaken2B(range: UInt): Bool = {
    val lastIdx = ParallelPosteriorityEncoder(range)
    !isRVC(io.cutInsts(lastIdx).bits.inst) && io.cutInsts(lastIdx).valid
  }

  def isRVC(inst: UInt): Bool = (inst(1, 0) =/= 3.U)

  traceRangeVec.foreach(_ := false.B)
  lastInstEndVec.map(_ := false.B)
  curInstIdxVec.map(_ := 0.U)
  (0 until width).foreach { i =>
    val curPC = startPC + (i * 2).U
    val curTrace = io.traceInsts(curInstIdxVec(i))
    val stillConsecutive = traceRangeVec.take(i).foldRight(true.B)(_ && _)

    val inst = io.cutInsts(i)
    when(!io.instRange(i) || !stillConsecutive) {
      inst.valid := false.B
      inst.bits := (-1.S).asTypeOf(new TraceInstrBundle)
    }.elsewhen(lastInstEndVec(i)) {
      traceRangeVec(i) := curPC === curTrace.pc
      inst.valid := traceRangeVec(i)
      inst.bits := curTrace

      when(inst.valid) {
        lastInstEndVec(i + 1) := isRVC(inst.bits.inst)
        curInstIdxVec(i + 1) := curInstIdxVec(i) + 1.U
      }
    }.elsewhen(stillConsecutive) {
      inst.valid := false.B
      inst.bits := (-1.S).asTypeOf(new TraceInstrBundle)
      inst.bits.pc := curPC

      if (i == 0)
        traceRangeVec(i) := (curPC + 2.U) === curTrace.pc
      else {
        traceRangeVec(i) := true.B
        XSError(io.debug_valid && (curPC =/= (io.traceInsts(curInstIdxVec(i) - 1.U).pc + 2.U)),
          "traceRange should not be true.B at stillConsecutive path?")
      }

      lastInstEndVec(i + 1) := true.B
      curInstIdxVec(i + 1) := curInstIdxVec(i)
    }.otherwise {
      inst.valid := false.B
      inst.bits := (-1.S).asTypeOf(new TraceInstrBundle)

      XSError(true.B, "Should not reach here")
    }
  }
}

class TracePredictInfo(implicit p: Parameters) extends TraceBundle {
  val startAddr = UInt(VAddrBits.W)
  val nextStartAddr = UInt(VAddrBits.W)

  val instRange = UInt(PredictWidth.W)
  val ftqOffset = Valid(UInt(log2Ceil(PredictWidth).W))
}

class TraceFromIFU(implicit p: Parameters) extends TraceBundle {
  val redirect = Bool()
  val fire = Bool()
  val valid = Bool()
}

class TracePreDecodeAndCheckerIO(implicit p: Parameters) extends TraceBundle {
  // IFU info
  val fromIFU = Input(new TraceFromIFU())
  // From TraceReader
  val traceInsts = Input(Vec(PredictWidth, new TraceInstrBundle()))
  // From BPU and IFU
  val predInfo = Input(new TracePredictInfo())

  // Pre-decoder: normal predecoder
  val predecoder = Output(new PreDecodeResp())
  // Predict checker
  val checker = Output(new PredCheckerResp())
  // trace checker
  val traceChecker = Output(new TraceCheckerResp())
  // trace Inst: one-to-one with preDecoder but contains the traceInfo
  val traceAlignInsts = Output(Vec(PredictWidth, Valid(new TraceInstrBundle())))
  val traceExpandInsts = Output(Vec(PredictWidth, UInt(32.W)))
}

class TracePreDecodeAndChecker(implicit p: Parameters) extends TraceModule
  with TraceParams {
  val io = IO(new TracePreDecodeAndCheckerIO)

  val preDecoder = Module(new TracePreDecoder)
  val predChecker = Module(new TracePredictChecker)
  val traceChecker = Module(new TraceChecker)
  val traceAligner = Module(new TraceAlignToIFUCut)

  val lastFetchRedirect = RegEnable(io.fromIFU.redirect, false.B, io.fromIFU.fire || io.fromIFU.redirect)
  val lastFetchTakenMore2B = RegEnable((traceAligner.io.instRangeTaken2B || traceAligner.io.traceRangeTaken2B), false.B, io.fromIFU.fire)

  val concede2Bytes = !lastFetchRedirect && lastFetchTakenMore2B
  val traceInstIFUCut = traceAligner.io.cutInsts

  traceAligner.io.specifyField(
    _.debug_valid := io.fromIFU.valid,
    _.traceInsts := io.traceInsts,
    _.predStartAddr := io.predInfo.startAddr,
    _.instRange := io.predInfo.instRange,
    _.lastHalfValid := concede2Bytes
  )

  preDecoder.io.specifyField(
    _.traceInsts := traceInstIFUCut,
  )
  predChecker.io.specifyField(
    _.fire_in := io.fromIFU.fire,
    _.traceInsts := traceInstIFUCut,
    _.predictInfo := io.predInfo,
    _.preDecode := preDecoder.io.out,
  )
  traceChecker.io.specifyField(
    _.debug_valid := io.fromIFU.valid,
    _.traceInsts := traceInstIFUCut,
    _.predictInfo := io.predInfo,
    _.preDecode := preDecoder.io.out,
    _.predChecker := predChecker.io.out,
    _.traceRange := traceAligner.io.traceRange,
  )
  io.specifyField(
    _.predecoder := preDecoder.io.out,
    _.checker := predChecker.io.out,
    _.traceChecker := traceChecker.io.out,
    _.traceAlignInsts := traceAligner.io.cutInsts,
    _.traceExpandInsts.zip(traceAligner.io.cutInsts).foreach {
      case (expand, cut) => expand := expandInst(cut.bits.inst)
    }
  )

  def expandInst(inst: UInt): UInt = {
    require(inst.getWidth == 32)
    val expander = Module(new RVCExpander)
    expander.io.in := inst
    expander.io.out.bits
  }
}

/**
 * TracePreDecoder
 *
 * Input: traceInst
 * Output: Precoded simple prediction result, keep same with the original PreDecoder of IFU
 * NOTE:
 * For IFU's PreDecoder: the startAddr and nextStartAddr will control inst valid
 * For Trace's PreDecoder: all the inst is at the arch path, but only the ones inside the startAddr and nextStartAddr will be "recv"(accept/handle)-ed now.
 */

class TracePreDecoder(implicit p: Parameters) extends TraceModule
  with TraceParams
  with HasPdConst {
  val io = IO(new Bundle {
    val traceInsts = Input(Vec(PredictWidth, Valid(new TraceInstrBundle())))
    val out = Output(new PreDecodeResp)
  })

  // Set it to false to ignore
  io.out.hasHalfValid.map(_ := false.B)
  io.out.triggered := 0.U.asTypeOf(io.out.triggered)

  for (i <- 0 until PredictWidth) {
    val pd = io.out.pd(i)
    val trace = io.traceInsts(i).bits

    val curIsRVC = isRVC(trace.inst)
    val brType :: isCall :: isRet :: Nil = brInfo(trace.inst)
    val jalOffset = jal_offset(trace.inst, curIsRVC)
    val brOffset = br_offset(trace.inst, curIsRVC)

    pd.valid := io.traceInsts(i).valid
    pd.isRVC := curIsRVC
    pd.brType := brType
    pd.isCall := isCall
    pd.isRet := isRet

    io.out.instr(i) := trace.inst
    io.out.jumpOffset(i) := Mux(pd.isBr, brOffset, jalOffset)
  }
}

// TracePredictChecker is just the same with the IFU's PredChecker
// with no regnext for stage2Out and different input
class TracePredictChecker(implicit p: Parameters) extends TraceModule
  with TraceParams {
  val io = IO(new Bundle {
    val fire_in = Input(Bool())
    val traceInsts = Input(Vec(PredictWidth, Valid(new TraceInstrBundle())))
    val predictInfo = Input(new TracePredictInfo)
    val preDecode = Input(new PreDecodeResp)
    val out = Output(new PredCheckerResp)
  })

  val pds = io.preDecode.pd
  val pcs = io.traceInsts.map(_.bits.pc)

  val predRange = io.predictInfo.instRange
  val takenIdx = io.predictInfo.ftqOffset.bits
  val predTaken = io.predictInfo.ftqOffset.valid
  val predTarget = io.predictInfo.nextStartAddr
  val instValid = io.preDecode.pd.map(_.valid)

  // Fault
  val jalFaultVec = (0 until PredictWidth).map(i => {
    pds(i).isJal &&
      pds(i).valid && predRange(i) &&
      (takenIdx > i.U && predTaken || !predTaken)
  })
  val retFaultVec = (0 until PredictWidth).map(i => {
    pds(i).isRet &&
      pds(i).valid && predRange(i) &&
      (takenIdx > i.U && predTaken || !predTaken)
  })
  val remaskFault = jalFaultVec.zip(retFaultVec).map {
    case (jal, ret) => jal || ret
  }
  val remaskIdx = ParallelPriorityEncoder(remaskFault)
  val needRemask = Cat(remaskFault).orR
  val AllTrueMask = -1.S(PredictWidth.W).asUInt
  val fixedRange = Mux(needRemask, AllTrueMask >> (PredictWidth.U - (remaskIdx + 1.U)), predRange)

  io.out.stage1Out.fixedRange := fixedRange.asTypeOf(Vec(PredictWidth, Bool()))
  io.out.stage1Out.fixedTaken := VecInit((0 until PredictWidth).map(i => {
    pds(i).valid && predRange(i) &&
      (pds(i).isRet || pds(i).isJal ||
        (takenIdx === i.U && predTaken && !pds(i).notCFI))
  }))

  val notCFITaken = (0 until PredictWidth).map(i => {
    fixedRange(i) && pds(i).valid &&
      (i.U === takenIdx) && predTaken && pds(i).notCFI
  })
  val invalidTaken = (0 until PredictWidth).map(i => {
    fixedRange(i) && !pds(i).valid &&
      (i.U === takenIdx) && predTaken
  })

  val jumpTargets = (0 until PredictWidth).map(i => {
    (pcs(i) + io.preDecode.jumpOffset(i)).asTypeOf(UInt(VAddrBits.W))
  })
  val seqTargets = (0 until PredictWidth).map(i => {
    pcs(i) + (Mux(pds(i).isRVC || !pds(i).valid, 2.U, 4.U))
  })

  val targetFault = (0 until PredictWidth).map(i => {
    fixedRange(i) && pds(i).valid &&
      (pds(i).isJal || pds(i).isBr) &&
      (takenIdx === i.U) && predTaken &&
      (jumpTargets(i) =/= predTarget)
  })

  val stage2Out = Wire(chiselTypeOf(io.out.stage2Out))
  (0 until PredictWidth).foreach(i => {
    stage2Out.faultType(i).value := Mux(jalFaultVec(i), FaultType.jalFault,
      Mux(retFaultVec(i), FaultType.retFault,
        Mux(targetFault(i), FaultType.targetFault,
          Mux(notCFITaken(i), FaultType.notCFIFault,
            Mux(invalidTaken(i), FaultType.invalidTaken, FaultType.noFault)))))
    stage2Out.fixedMissPred(i) := jalFaultVec(i) || retFaultVec(i) ||
      targetFault(i) || notCFITaken(i) || invalidTaken(i)
    stage2Out.fixedTarget(i) := Mux(jalFaultVec(i) || targetFault(i), jumpTargets(i), seqTargets(i))
    stage2Out.jalTarget(i) := jumpTargets(i)
  })
  io.out.stage2Out := RegEnable(stage2Out, io.fire_in)

  // debug
  instValid.zip(io.traceInsts).foreach { case (valid, trace) =>
    XSError(valid =/= trace.valid, "instValid should be the same with trace.valid")
  }
}

/**
 * Normal Prediction Checker
 * Input: instRange, jumpRange, predInfo
 * Output: fixedRange, fixedTaken, fixedTarget, jalTarget, fixedMissPred, faultType
 */
// Can we just re-use the same module PredChecker from IFU?

/** Path Checker
 * Three path types:
 * - ArchPath: the right path, should be the same with the rob commit trace
 * - SpecPath: the wrong path, but can be corrected by the ifu's PredChecker
 * - WrongPath: the wrong path, cannot be corrected by the ifu's PredChecker
 */
/** What the IFU-PredChecker can do
 * - Fault Type:
 *   - 1: jal fault    : the jal inst not predicted to jump
 *   - 2: return fault : the return inst not predicted to jump
 *   - 3: target fault : the inst that predicted to jump,
 *     but the target is wrong with predecoded result
 *   - 4: notCFI fault : the inst that predicted to jump is not a CFI
 *   - 5: invalidTaken fault : the ftqOffset's inst is not a valid inst start
 *   - 6: no fault
 *     - When encounter a fault:
 *   - 1. for each '2bytes' bit, set the missPred bit
 *   - 2. for each '2bytes' bit, set the target result(jumptarget or seqtarget)
 *   - 3. for each '2bytes' bit, set the jalTarget result
 *   - 4. set the fixedRange to the range of the fault inst
 *   - 5. set the fixedTaken to the fault inst's taken
 *     - BranchFault
 *   - The fault that can not be corrected by the ifu's PredChecker
 *   - traceRange is shorted than predRange(or checkerRange)
 *   - The acutal checkRange should not be longer than traceRange
 *   - The acutal traceRange should not be longer than checkRange
 *   - So if there are no exception or "implement-wrong", traceRange should be the same with checkRange
 *     - Checker can do:
 *   - Normally, when checker find a fault, it may inside, equal, outside the traceRange. or checker find no fault.
 *   - when inside, the checker can correct the fault, the Trace maybe wrong?
 *     Just do the same as the IFU's PredChecker: redirect and wait for next fetch.
 *     When the next fetch is not equal to trace.pc, block and wait for backend redirect.
 *   - when equal, the checker can correct the fault, the Trace is right.
 *     Just do the same as the IFU's PredChecker: redirect and wait for next fetch.
 *   - when outside, the checker can not correct the fault, it looks like not found a fault.
 *     no IFU redirect, just block and wait for backend redirect.
 */

/** Range-Relationship
 * - traceRange <= predRange: when the raw traceRange > predRange,
 * just take the predRange num, and ignore the rest. We will fetch
 * the rest in the next fetch.
 * - checkRange <= predRange: check whether the inst, that is inside
 * the predRange, has simple miss-prediction: branch type, jump type,
 * target addr.
 * - checkRange <= traceRange: only inst inside the traceRange is valid, this
 * constraint comes from the trace.
 * when checkRange < traceRange: there are some inst that should jump/taken but not jump/taken
 * check what the checker can do
 * (? so checkRange == min(traceRange, predRange))
 *
 */

class TraceCheckerResp(implicit p: Parameters) extends TraceBundle {
  val traceRange = UInt(PredictWidth.W)
  val traceValid = UInt(PredictWidth.W)
}

class TraceCheckerIO(implicit p: Parameters) extends TraceBundle {
  val debug_valid = Input(Bool())
  val traceInsts = Input(Vec(PredictWidth, Valid(new TraceInstrBundle())))
  val predictInfo = Input(new TracePredictInfo)
  val preDecode = Input(new PreDecodeResp)
  val predChecker = Input(new PredCheckerResp)
  // TODO: this traceRange is alone, make it better
  val traceRange = Input(UInt(PredictWidth.W))

  val out = Output(new TraceCheckerResp)
}

class TraceChecker(implicit p: Parameters) extends TraceModule {
  val io = IO(new TraceCheckerIO)

  val predRange = io.predictInfo.instRange
  val checkRange = io.predChecker.stage1Out.fixedRange.asUInt
  io.out.traceRange := io.traceRange & checkRange
  io.out.traceValid := VecInit(io.traceInsts.map(_.valid)).asUInt

  /**
   * TraceRange
   * - 1. checkRange < traceRange && traceRange <= predRange
   * This may not happen or the jump is to seqInst
   * Just take normal ifu-redirect
   * - 2. checkRange = traceRange && traceRange <= predRange
   * predChecker finds the fault.
   * Just take normal ifu-redirect
   * - 3.
   */
  // Acutually, we don't  care about the target, we care about range
  // when (predCheckNoRemask) {
  // two case:
  // 1. checker find not fault
  // 2. checker find target fault, or notCFI fault, invalidTaken fault
  // Just do the same as the IFU: [redirect and] wait for next fetch.

  // There are may be two case:
  // 1. predRange same with traceRange: pred is right
  // Then nothing specially to do
  // val
  // 2. predRange is longer than traceRange: pred is wrong/partially wrong
  // Do we need IFU-redirect? NO, just w
  // }


  // debug
  io.traceInsts.zip(io.preDecode.pd).foreach { case (trace, pd) =>
    XSError(trace.valid =/= pd.valid,
      "traceInst should be the same with preDecode.valid")
  }
  when(io.debug_valid) {
    XSError((checkRange.asUInt & predRange.asUInt) === checkRange.asUInt,
      "checkRange should be shorter than predRange")
  }
}

class TraceDriverOutput(implicit p: Parameters) extends TraceBundle {
  // when block true, the fetch is at the wrong path, should block ifu-go, ibuffer-recv
  val block = Output(Bool())
  val recv = ValidIO(new TraceRecvInfo())
}

class TraceDriverIO(implicit p: Parameters) extends TraceBundle {
  val fire = Input(Bool())
  val traceInsts = Input(Vec(PredictWidth, Valid(new TraceInstrBundle())))
  val traceRange = Input(UInt(PredictWidth.W))

  val out = new TraceDriverOutput()
}

class TraceDriver(implicit p: Parameters) extends TraceModule {
  val io = IO(new TraceDriverIO())

  val traceValid = VecInit(io.traceInsts.map(_.valid)).asUInt
  io.out.block := io.out.recv.bits.instNum === 0.U // may be we need more precise control signal
  io.out.recv.bits.instNum := PopCount(io.traceRange & traceValid)
  io.out.recv.valid := io.fire
}
