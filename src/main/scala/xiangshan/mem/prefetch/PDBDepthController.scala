package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import utility._
import xiangshan.{XSCoreParamsKey, XSModule}

case class PDBDepthParameters(
  enabled: Boolean = false,
  lateLow: Int = 20, lateHigh: Int = 40,
  unusedLow: Int = 40, unusedHigh: Int = 80,
  creditHalfUnits: Int = 2, settleWindows: Int = 1,
  usedOnHits: Int = 20, usedOffHits: Int = 5,
  usedOnWindows: Int = 3, usedOffWindows: Int = 2,
  unusedOffWindows: Int = 8
) {
  require(0 <= lateLow && lateLow < lateHigh && lateHigh < 65536)
  require(0 <= unusedLow && unusedLow < unusedHigh && unusedHigh < 65536)
  require(creditHalfUnits > 0 && creditHalfUnits <= 8)
  require(settleWindows >= 0 && settleWindows <= 15)
  require(0 <= usedOffHits && usedOffHits < usedOnHits && usedOnHits < 65536)
  require(Seq(usedOnWindows, usedOffWindows, unusedOffWindows).forall(n => n > 0 && n <= 15))
}
case object PDBDepthKey extends Field[PDBDepthParameters](PDBDepthParameters())

object PDBDepthPolicy {
  val levels = Seq(4, 8, 16, 24, 32, 48, 64)
  val windowRefills = 500
}

class PDBDepthEvents extends Bundle {
  val refill = Bool()
  val late = UInt(4.W)
  val used = UInt(2.W)
  val unused = UInt(2.W)
}

/** Reuse hits already passed the observer's two capture edges. Delay the raw
  * PB refill and MQ late events equally so a window has one physical boundary.
  */
class PDBDepthEventAligner extends Module {
  val io = IO(new Bundle {
    val raw = Input(new PDBDepthEvents)
    val completed = Output(new PDBDepthEvents)
  })
  io.completed := io.raw
  io.completed.refill := ShiftRegister(io.raw.refill, 2, false.B, true.B)
  io.completed.late := ShiftRegister(io.raw.late, 2, 0.U(4.W), true.B)
}

class PDBDepthWindow extends Bundle {
  val enabled = Bool()
  val depthBefore, depthAfter = UInt(12.W)
  val late, used, unused = UInt(16.W)
  // Integer half units: 0, 1, 2 represent pressures 0, 0.5, 1.
  val latePressure, unusedPressure = UInt(2.W)
  val upCredit, downCredit, settle = UInt(4.W)
  val usedMoveShadow, unusedMoveShadow = Bool()
}

/** Only sideband evidence enters this module. No cache request is backpressured. */
class PDBDepthController(params: PDBDepthParameters, initialDepth: Int = 16) extends Module {
  import PDBDepthPolicy._
  require(levels.contains(initialDepth))
  val io = IO(new Bundle {
    val enabled = Input(Bool())
    val fixedDepth = Input(UInt(12.W))
    val events = Input(new PDBDepthEvents)
    val depth = Output(UInt(12.W))
    val refills = Output(UInt(9.W))
    val usedMoveShadow, unusedMoveShadow = Output(Bool())
    val window = Output(Valid(new PDBDepthWindow))
  })

  val index = RegInit(levels.indexOf(initialDepth).U(3.W))
  val refills = RegInit(0.U(9.W))
  val late = RegInit(0.U(16.W))
  val used = RegInit(0.U(16.W))
  val unused = RegInit(0.U(16.W))
  val upCredit = RegInit(0.U(4.W))
  val downCredit = RegInit(0.U(4.W))
  val settle = RegInit(0.U(4.W))
  val usedMove = RegInit(false.B)
  val unusedMove = RegInit(false.B)
  val usedOnStreak = RegInit(0.U(4.W))
  val usedOffStreak = RegInit(0.U(4.W))
  val unusedOffStreak = RegInit(0.U(4.W))
  val wholeWindowAtMin = RegInit(true.B)

  def saturatedAdd(count: UInt, increment: UInt): UInt = {
    val sum = count +& increment
    Mux(sum > 65535.U, 65535.U(16.W), sum(15, 0))
  }
  def pressure(count: UInt, low: Int, high: Int): UInt =
    Mux(count <= low.U, 0.U(2.W), Mux(count < high.U, 1.U(2.W), 2.U(2.W)))

  val totalLate = saturatedAdd(late, io.events.late)
  val totalUsed = saturatedAdd(used, io.events.used)
  val totalUnused = saturatedAdd(unused, io.events.unused)
  val latePressure = pressure(totalLate, params.lateLow, params.lateHigh)
  val unusedPressure = pressure(totalUnused, params.unusedLow, params.unusedHigh)
  val windowEnd = io.events.refill && refills === (windowRefills - 1).U
  val depths = VecInit(levels.map(_.U(12.W)))
  io.depth := Mux(io.enabled, depths(index), io.fixedDepth)
  io.refills := refills
  io.usedMoveShadow := usedMove
  io.unusedMoveShadow := unusedMove

  val nextIndex = WireDefault(index)
  val nextUp = WireDefault(0.U(4.W))
  val nextDown = WireDefault(0.U(4.W))
  val nextSettle = WireDefault(0.U(4.W))
  when (io.enabled) {
    when (settle =/= 0.U) {
      nextSettle := settle - 1.U
    }.elsewhen (latePressure > unusedPressure) {
      val credit = upCredit +& (latePressure - unusedPressure)
      when (credit >= params.creditHalfUnits.U) {
        when (index < (levels.size - 1).U) {
          nextIndex := index + 1.U
          nextSettle := params.settleWindows.U
        }
      }.otherwise { nextUp := credit }
    }.elsewhen (unusedPressure > latePressure) {
      val credit = downCredit +& (unusedPressure - latePressure)
      when (credit >= params.creditHalfUnits.U) {
        when (index =/= 0.U) {
          nextIndex := index - 1.U
          nextSettle := params.settleWindows.U
        }
      }.otherwise { nextDown := credit }
    }
  }
  val nextDepth = Mux(io.enabled, depths(nextIndex), io.fixedDepth)

  // Shadow state remains active with both physical move policies absent. In
  // particular it never consumes unused pressure or changes the credit limit.
  val nextUsedMove = WireDefault(usedMove)
  val nextUsedOn = WireDefault(0.U(4.W))
  val nextUsedOff = WireDefault(0.U(4.W))
  when (!usedMove) {
    when (totalUsed >= params.usedOnHits.U) {
      when (usedOnStreak === (params.usedOnWindows - 1).U) {
        nextUsedMove := true.B
      }.otherwise { nextUsedOn := usedOnStreak + 1.U }
    }
  }.otherwise {
    when (totalUsed <= params.usedOffHits.U) {
      when (usedOffStreak === (params.usedOffWindows - 1).U) {
        nextUsedMove := false.B
      }.otherwise { nextUsedOff := usedOffStreak + 1.U }
    }
  }
  val nextUnusedMove = WireDefault(unusedMove)
  val nextUnusedOff = WireDefault(0.U(4.W))
  val unusedEligible = wholeWindowAtMin && io.depth === 4.U && nextDepth === 4.U &&
    unusedPressure =/= 0.U && unusedPressure >= latePressure
  when (!unusedMove) {
    when (unusedEligible) { nextUnusedMove := true.B }
  }.otherwise {
    when (unusedPressure === 0.U) {
      when (unusedOffStreak === (params.unusedOffWindows - 1).U) {
        nextUnusedMove := false.B
      }.otherwise { nextUnusedOff := unusedOffStreak + 1.U }
    }
  }

  // The closing edge includes all events on that edge. No extra cycle is
  // inserted between windows, and no FIFO entry is cleared at a boundary.
  late := Mux(windowEnd, 0.U, totalLate)
  used := Mux(windowEnd, 0.U, totalUsed)
  unused := Mux(windowEnd, 0.U, totalUnused)
  when (io.events.refill) { refills := Mux(windowEnd, 0.U, refills + 1.U) }
  wholeWindowAtMin := Mux(windowEnd, true.B, wholeWindowAtMin && io.depth === 4.U)
  when (windowEnd) {
    index := nextIndex
    upCredit := nextUp
    downCredit := nextDown
    settle := nextSettle
    usedMove := nextUsedMove
    usedOnStreak := nextUsedOn
    usedOffStreak := nextUsedOff
    unusedMove := nextUnusedMove
    unusedOffStreak := nextUnusedOff
  }

  io.window.valid := windowEnd
  io.window.bits.enabled := io.enabled
  io.window.bits.depthBefore := io.depth
  io.window.bits.depthAfter := nextDepth
  io.window.bits.late := totalLate
  io.window.bits.used := totalUsed
  io.window.bits.unused := totalUnused
  io.window.bits.latePressure := latePressure
  io.window.bits.unusedPressure := unusedPressure
  io.window.bits.upCredit := nextUp
  io.window.bits.downCredit := nextDown
  io.window.bits.settle := nextSettle
  io.window.bits.usedMoveShadow := nextUsedMove
  io.window.bits.unusedMoveShadow := nextUnusedMove
  assert(index < levels.size.U)
  assert(refills < windowRefills.U)
  assert(upCredit < params.creditHalfUnits.U && downCredit < params.creditHalfUnits.U)
  val previousIndex = RegNext(index, levels.indexOf(initialDepth).U)
  assert(index === previousIndex || RegNext(windowEnd, false.B),
    "Depth may change only after a complete 500-refill window")
}

class PDBDepthMonitor(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val events = Input(new PDBDepthEvents)
    val depth = Output(UInt(12.W))
  })
  private val params = p(PDBDepthKey)
  private val stream = p(StreamDepthKey)
  private val hart = p(XSCoreParamsKey).HartId
  val enabled = Constantin.createRecord(s"enablePDBAutoDepth$hart", initValue = params.enabled)
  val fixed = Constantin.createRecord(s"pdbFixedDepth$hart", initValue = stream.fixedL1)
  val trace = Constantin.createRecord(s"tracePDBDepth$hart", initValue = true)
  val controller = Module(new PDBDepthController(params, stream.initial))
  controller.io.enabled := enabled
  controller.io.fixedDepth := fixed
  assert(enabled || VecInit(PDBDepthPolicy.levels.map(d => fixed === d.U)).asUInt.orR,
    "The fixed-depth control must select an existing depth level")
  controller.io.events := io.events
  io.depth := controller.io.depth
  val window = controller.io.window
  val report = window.bits
  XSPerfAccumulate("refills", io.events.refill)
  XSPerfAccumulate("late", io.events.late)
  XSPerfAccumulate("usedVictimHits", io.events.used)
  XSPerfAccumulate("unusedVictimHits", io.events.unused)
  XSPerfAccumulate("windows", window.valid)
  XSPerfAccumulate("depth_increases", window.valid && report.depthAfter > report.depthBefore)
  XSPerfAccumulate("depth_decreases", window.valid && report.depthAfter < report.depthBefore)
  XSPerfAccumulate("used_move_shadow_windows", window.valid && report.usedMoveShadow)
  XSPerfAccumulate("unused_move_shadow_windows", window.valid && report.unusedMoveShadow)
  for (depth <- PDBDepthPolicy.levels) {
    XSPerfAccumulate(s"cycles_at_depth_$depth", io.depth === depth.U)
    XSPerfAccumulate(s"windows_at_depth_$depth", window.valid && report.depthBefore === depth.U)
  }
  val table = ChiselDB.createTable(s"PDBDepthWindow$hart", chiselTypeOf(report), basicDB = true)
  table.log(report, window.valid && trace, s"PDBDepthMonitor$hart", clock, reset)
  println(s"PDB depth: enabled=${params.enabled}, initial=${stream.initial}, fixed=${stream.fixedL1}, " +
    s"window=500, levels=${PDBDepthPolicy.levels.mkString(",")}, parameters=$params, physicalMove=false")
}
