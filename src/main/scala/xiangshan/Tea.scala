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

  val pendingDrainCount = RegInit(0.U(64.W))
  val replayActive = RegInit(false.B)
  val replayPc = RegInit(0.U(VAddrBits.W))
  val replayPsv = RegInit(0.U(TeaEvent.width.W))
  val cycle = RegInit(0.U(64.W))
  cycle := cycle + 1.U

  val sample = WireInit(0.U.asTypeOf(new TeaEntry()(p)))
  val sampleValid = WireDefault(false.B)
  val oirSample = io.sampleFire && io.oir.valid
  val regularSample = io.sampleFire && !io.oir.valid && io.state =/= 3.U
  val drainCapture = io.sampleFire && !io.oir.valid && io.state === 3.U

  val pendingAfterCapture = pendingDrainCount + drainCapture.asUInt
  val replayFromNewAlloc = !replayActive && io.firstAllocValid && pendingAfterCapture =/= 0.U
  val replayNow = replayActive && !io.oir.valid
  val replayDeq = (replayActive || replayFromNewAlloc) && pendingAfterCapture =/= 0.U && !io.oir.valid
  val pendingAfterReplay = pendingAfterCapture - replayDeq.asUInt

  sample.cycle := cycle
  sample.overflow := io.overflow

  when(oirSample) {
    sample.state := 2.U
    sample.validMask := 1.U(CommitWidth.W)
    sample.pcVec(0) := io.oir.pc
    sample.psvVec(0) := io.oir.psv
    sample.oirValid := true.B
    sampleValid := true.B
  }.elsewhen(replayNow) {
    sample.state := 3.U
    sample.validMask := 1.U(CommitWidth.W)
    sample.pcVec(0) := replayPc
    sample.psvVec(0) := replayPsv
    sample.pendingDrain := true.B
    sampleValid := true.B
  }.elsewhen(regularSample) {
    switch(io.state) {
      is(0.U) {
        sample.state := 0.U
        sample.validMask := io.commitMask
        sample.pcVec := io.commitPc
        sample.psvVec := io.commitPsv
        sampleValid := io.commitMask.orR
      }
      is(1.U) {
        sample.state := 1.U
        sample.validMask := 1.U(CommitWidth.W)
        sample.pcVec(0) := io.headPc
        sample.psvVec(0) := io.headPsv
        sampleValid := true.B
      }
    }
  }

  pendingDrainCount := pendingAfterReplay

  when(replayFromNewAlloc) {
    replayPc := io.firstAllocPc
    replayPsv := io.firstAllocPsv
    replayActive := true.B
  }.elsewhen(replayActive && pendingDrainCount === 0.U) {
    replayActive := false.B
  }

  io.sampleValid := sampleValid
  io.sample := sample
  io.pendingDrain := pendingAfterReplay =/= 0.U
}
