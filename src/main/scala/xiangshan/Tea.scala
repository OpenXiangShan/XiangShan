package xiangshan

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.ctrlblock.DebugLsInfo

object TeaEvent {
  val DR_L1  = 0
  val DR_TLB = 1
  val DR_SQ  = 2
  val FL_MB  = 3
  val FL_EX  = 4
  val FL_MO  = 5
  val ST_L1  = 6
  val ST_TLB = 7
  val ST_LLC = 8

  val width = ST_LLC + 1

  def bit(idx: Int): UInt = {
    require(idx >= 0 && idx < width, s"TEA event index $idx out of range [0, $width)")
    (BigInt(1) << idx).U(TeaEvent.width.W)
  }
}

object TeaPsvOps {
  def empty: UInt = 0.U(TeaEvent.width.W)

  def setBit(psv: UInt, event: Int): UInt = {
    psv | TeaEvent.bit(event)
  }
}

object TeaBinders {
  def applyLoadDcacheFirstMiss(psv: UInt, isDcacheFirstMiss: Bool): UInt = {
    Mux(isDcacheFirstMiss, TeaPsvOps.setBit(psv, TeaEvent.ST_L1), psv)
  }

  def applyLoadTlbFirstMiss(psv: UInt, isTlbFirstMiss: Bool): UInt = {
    Mux(isTlbFirstMiss, TeaPsvOps.setBit(psv, TeaEvent.ST_TLB), psv)
  }

  def applyLoadDebug(psv: UInt, lsInfo: DebugLsInfo): UInt = {
    applyLoadTlbFirstMiss(applyLoadDcacheFirstMiss(psv, lsInfo.s2_isDcacheFirstMiss), lsInfo.s1_isTlbFirstMiss)
  }

  def applyControlRedirect(psv: UInt, isControlRedirect: Bool): UInt = {
    Mux(isControlRedirect, TeaPsvOps.setBit(psv, TeaEvent.FL_MB), psv)
  }

  def applyMemVioRedirect(psv: UInt, isMemVioRedirect: Bool): UInt = {
    Mux(isMemVioRedirect, TeaPsvOps.setBit(psv, TeaEvent.FL_MO), psv)
  }

  def applyExceptionFlush(psv: UInt, isExceptionFlush: Bool): UInt = {
    Mux(isExceptionFlush, TeaPsvOps.setBit(psv, TeaEvent.FL_EX), psv)
  }
}

object TeaFrontend {
  def selectEnqueued(
    valids: Seq[Bool],
    enqEnable: Seq[Bool],
    enqOffset: Seq[UInt],
    useBypass: Bool,
    numBypass: UInt
  ): Vec[Bool] = {
    VecInit(valids.indices.map { i =>
      valids(i) && enqEnable(i) && (!useBypass || enqOffset(i) >= numBypass)
    })
  }

  def bindPacketPsv(valids: Seq[Bool], packetPsv: UInt): Vec[UInt] = {
    val firstValidOH = PriorityEncoderOH(VecInit(valids))
    VecInit(valids.indices.map { i =>
      Mux(firstValidOH(i), packetPsv, TeaPsvOps.empty)
    })
  }
}

class TeaFlushCause(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val pc = UInt(VAddrBits.W)
  val psv = UInt(TeaEvent.width.W)
}

class TeaEntry(implicit p: Parameters) extends XSBundle {
  val cycle = UInt(64.W)
  val state = UInt(4.W)
  val validMask = UInt(CommitWidth.W)
  val pcVec = Vec(CommitWidth, UInt(VAddrBits.W))
  val psvVec = Vec(CommitWidth, UInt(TeaEvent.width.W))
  val oirValid = Bool()
  val overflow = Bool()
  val pendingDrain = Bool()
}

class TeaSampleSelector(implicit val p: Parameters) extends Module with HasXSParameter {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val samplePeriod = Input(UInt(64.W))
    val state = Input(UInt(4.W))
    val hasCommit = Input(Bool())
    val commitMask = Input(UInt(CommitWidth.W))
    val commitPc = Input(Vec(CommitWidth, UInt(VAddrBits.W)))
    val commitPsv = Input(Vec(CommitWidth, UInt(TeaEvent.width.W)))
    val flushCause = Input(new TeaFlushCause()(p))
    val sampleValid = Output(Bool())
    val sample = Output(new TeaEntry()(p))
  })

  val computingState = 0.U
  val stalledState = 1.U
  val flushedState = 2.U
  val drainedState = 3.U

  val cycle = RegInit(0.U(64.W))
  val countdown = RegInit(0.U(64.W))
  val pendingValid = RegInit(false.B)
  val pendingState = RegInit(0.U(4.W))
  val pendingCycle = RegInit(0.U(64.W))
  val overflowPending = RegInit(false.B)
  cycle := cycle + 1.U

  val sample = WireInit(0.U.asTypeOf(new TeaEntry()(p)))
  val sampleValid = WireDefault(false.B)
  val sampleDue = io.enable && countdown === 0.U
  val hasCurrentPayload =
    io.state === stalledState ||
    io.state === drainedState ||
    io.state === computingState && io.hasCommit ||
    io.state === flushedState && io.flushCause.valid
  val accept = sampleDue && !pendingValid && hasCurrentPayload
  val drop = sampleDue && !accept
  val resolvePending = pendingValid && io.hasCommit

  when(resolvePending) {
    sample.cycle := pendingCycle
    sample.state := pendingState
    sample.validMask := 1.U(CommitWidth.W)
    sample.pcVec(0) := io.commitPc(0)
    sample.psvVec(0) := io.commitPsv(0)
    sampleValid := true.B
  }.elsewhen(accept && io.state === computingState) {
    sample.cycle := cycle
    sample.state := computingState
    sample.validMask := io.commitMask
    sample.pcVec := io.commitPc
    sample.psvVec := io.commitPsv
    sampleValid := true.B
  }.elsewhen(accept && io.state === flushedState) {
    sample.cycle := cycle
    sample.state := flushedState
    sample.validMask := 1.U(CommitWidth.W)
    sample.pcVec(0) := io.flushCause.pc
    sample.psvVec(0) := io.flushCause.psv
    sampleValid := true.B
  }

  sample.oirValid := sample.state === flushedState
  sample.pendingDrain := sample.state === drainedState
  sample.overflow := overflowPending || drop

  when(!io.enable) {
    countdown := 0.U
    pendingValid := false.B
    overflowPending := false.B
  }.otherwise {
    when(countdown === 0.U) {
      countdown := Mux(io.samplePeriod === 0.U, 0.U, io.samplePeriod - 1.U)
    }.otherwise {
      countdown := countdown - 1.U
    }

    when(resolvePending) {
      pendingValid := false.B
    }.elsewhen(accept && (io.state === stalledState || io.state === drainedState)) {
      pendingValid := true.B
      pendingState := io.state
      pendingCycle := cycle
    }

    when(sampleValid) {
      overflowPending := false.B
    }.elsewhen(drop) {
      overflowPending := true.B
    }
  }

  io.sampleValid := sampleValid
  io.sample := sample
}
