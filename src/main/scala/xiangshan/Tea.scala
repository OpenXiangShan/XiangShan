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

class TeaOIR(implicit p: Parameters) extends XSBundle {
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
    val sampleFire = Input(Bool())
    val state = Input(UInt(4.W))
    val commitMask = Input(UInt(CommitWidth.W))
    val commitPc = Input(Vec(CommitWidth, UInt(VAddrBits.W)))
    val commitPsv = Input(Vec(CommitWidth, UInt(TeaEvent.width.W)))
    val headPc = Input(UInt(VAddrBits.W))
    val headPsv = Input(UInt(TeaEvent.width.W))
    val oir = Input(new TeaOIR()(p))
    val overflow = Input(Bool())
    val firstAllocValid = Input(Bool())
    val firstAllocPc = Input(UInt(VAddrBits.W))
    val firstAllocPsv = Input(UInt(TeaEvent.width.W))
    val sampleValid = Output(Bool())
    val sample = Output(new TeaEntry()(p))
    val pendingDrain = Output(Bool())
  })

  val drainCount = RegInit(0.U(64.W))
  val oldestDrainCycle = RegInit(0.U(64.W))
  val lastDrainCycle = RegInit(0.U(64.W))
  val drainStride = RegInit(0.U(64.W))
  val drainStrideValid = RegInit(false.B)
  val replayActive = RegInit(false.B)
  val replayPc = RegInit(0.U(VAddrBits.W))
  val replayPsv = RegInit(0.U(TeaEvent.width.W))
  val cycle = RegInit(0.U(64.W))
  cycle := cycle + 1.U

  val sample = WireInit(0.U.asTypeOf(new TeaEntry()(p)))
  val sampleValid = WireDefault(false.B)
  val effectiveState = Mux(io.oir.valid, 2.U(4.W), io.state)
  val oirSampleFire = io.sampleFire && io.oir.valid
  val drainSampleFire = io.sampleFire && io.state === 3.U
  val replayValid = replayActive && drainCount =/= 0.U
  val doDrainDeq = replayValid && !oirSampleFire
  val drainEmpty = drainCount === 0.U
  val nextDrainCount = WireDefault(drainCount)
  val observedDrainStride = cycle - lastDrainCycle

  when(drainSampleFire && !doDrainDeq) {
    nextDrainCount := drainCount + 1.U
  }.elsewhen(!drainSampleFire && doDrainDeq) {
    nextDrainCount := drainCount - 1.U
  }

  sample.cycle := cycle
  sample.state := effectiveState
  sample.overflow := io.overflow

  when(io.sampleFire) {
    switch(effectiveState) {
      is(0.U) {
        sample.validMask := io.commitMask
        sample.pcVec := io.commitPc
        sample.psvVec := io.commitPsv
        sampleValid := io.commitMask.orR
      }
      is(1.U) {
        sample.validMask := 1.U(CommitWidth.W)
        sample.pcVec(0) := io.headPc
        sample.psvVec(0) := io.headPsv
        sampleValid := true.B
      }
      is(2.U) {
        sample.validMask := 1.U(CommitWidth.W)
        sample.pcVec(0) := io.oir.pc
        sample.psvVec(0) := io.oir.psv
        sample.oirValid := io.oir.valid
        sampleValid := io.oir.valid
      }
    }
  }

  when(drainSampleFire) {
    when(drainEmpty) {
      oldestDrainCycle := cycle
      lastDrainCycle := cycle
      drainStride := 0.U
      drainStrideValid := false.B
    }.otherwise {
      when(drainStrideValid) {
        assert(observedDrainStride === drainStride, "TeaSampleSelector expects periodic drained sample fires")
      }.otherwise {
        drainStride := observedDrainStride
        drainStrideValid := true.B
      }
      lastDrainCycle := cycle
    }
  }

  when(!replayActive && io.firstAllocValid && (!drainEmpty || drainSampleFire)) {
    replayActive := true.B
    replayPc := io.firstAllocPc
    replayPsv := io.firstAllocPsv
  }

  when(doDrainDeq && !sample.oirValid) {
    sample.cycle := oldestDrainCycle
    sample.state := 3.U
    sample.validMask := 1.U(CommitWidth.W)
    sample.pcVec(0) := replayPc
    sample.psvVec(0) := replayPsv
    sample.pendingDrain := true.B
    sampleValid := true.B
  }

  when(drainSampleFire && !doDrainDeq) {
    drainCount := drainCount + 1.U
  }.elsewhen(!drainSampleFire && doDrainDeq) {
    drainCount := drainCount - 1.U
  }

  when(doDrainDeq && drainCount > 1.U) {
    oldestDrainCycle := oldestDrainCycle + Mux(drainStrideValid, drainStride, 0.U)
  }.elsewhen(doDrainDeq && drainSampleFire) {
    oldestDrainCycle := cycle
  }

  when(replayActive && doDrainDeq && drainCount === 1.U && !drainSampleFire) {
    replayActive := false.B
  }

  io.sampleValid := sampleValid
  io.sample := sample
  io.pendingDrain := nextDrainCount =/= 0.U
}
