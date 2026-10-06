package xiangshan

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

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

object TeaState {
  def computing: UInt = 0.U(4.W)
  def stalled: UInt = 1.U(4.W)
  def flushed: UInt = 2.U(4.W)
  def drained: UInt = 3.U(4.W)

  def classify(isEmpty: Bool, isWalking: Bool, hasCommit: Bool, flushCauseValid: Bool): UInt = {
    Mux(
      flushCauseValid,
      flushed,
      Mux(isEmpty, drained, Mux(isWalking, flushed, Mux(hasCommit, computing, stalled)))
    )
  }
}

object TeaFrontend {
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
  val resolvePending = pendingValid && io.hasCommit
  val delayedState = io.state === TeaState.stalled || io.state === TeaState.drained
  val acceptFlush = sampleDue && io.state === TeaState.flushed && io.flushCause.valid
  val acceptComputing = sampleDue && !pendingValid && io.state === TeaState.computing && io.hasCommit
  val queueBoundary = sampleDue && delayedState && (!pendingValid || resolvePending)
  val acceptCurrent = acceptFlush || acceptComputing
  val drop = sampleDue && !acceptCurrent && !queueBoundary

  when(acceptFlush) {
    sample.cycle := cycle
    sample.state := TeaState.flushed
    sample.validMask := 1.U(CommitWidth.W)
    sample.pcVec(0) := io.flushCause.pc
    sample.psvVec(0) := io.flushCause.psv
    sampleValid := true.B
  }.elsewhen(resolvePending) {
    sample.cycle := pendingCycle
    sample.state := pendingState
    sample.validMask := 1.U(CommitWidth.W)
    sample.pcVec(0) := io.commitPc(0)
    sample.psvVec(0) := io.commitPsv(0)
    sampleValid := true.B
  }.elsewhen(acceptComputing) {
    sample.cycle := cycle
    sample.state := TeaState.computing
    sample.validMask := io.commitMask
    sample.pcVec := io.commitPc
    sample.psvVec := io.commitPsv
    sampleValid := true.B
  }

  sample.oirValid := sample.state === TeaState.flushed
  sample.pendingDrain := sample.state === TeaState.drained
  sample.overflow := overflowPending || drop || (pendingValid && acceptFlush)

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

    when(acceptFlush) {
      pendingValid := false.B
    }.elsewhen(queueBoundary) {
      pendingValid := true.B
      pendingState := io.state
      pendingCycle := cycle
    }.elsewhen(resolvePending) {
      pendingValid := false.B
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
