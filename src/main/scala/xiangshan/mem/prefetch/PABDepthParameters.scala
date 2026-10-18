package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Field

case class PABDepthParameters(
  enabled: Boolean = false,
  autoDepth: Boolean = true,
  entries: Int = 64,
  victimEntries: Int = 256,
  initialDepth: Int = 16,
  fixedDepth: Int = 64,
  windowInserts: Int = 500,
  lateLow: Int = 20, lateHigh: Int = 40,
  unusedLow: Int = 40, unusedHigh: Int = 80,
  creditHalfUnits: Int = 2,
  settleWindows: Int = 1
) {
  require(entries > 0 && victimEntries > 0)
  require(PABDepthPolicy.levels.contains(initialDepth) && PABDepthPolicy.levels.contains(fixedDepth))
  require(windowInserts > 0 && windowInserts <= 65535)
  require(0 <= lateLow && lateLow < lateHigh && lateHigh < 65536)
  require(0 <= unusedLow && unusedLow < unusedHigh && unusedHigh < 65536)
  require(creditHalfUnits > 0 && creditHalfUnits <= 8)
  require(settleWindows >= 0 && settleWindows <= 15)
}
case object PABDepthKey extends Field[PABDepthParameters](PABDepthParameters())

object PABDepthPolicy {
  val levels = Seq(4, 8, 16, 24, 32, 48, 64, 128)
}

class PABDepthEvents extends Bundle {
  val insert = Bool()
  val late = UInt(4.W)
  val unused = UInt(2.W)
}

class PABDepthWindow extends Bundle {
  val enabled = Bool()
  val depthBefore, depthAfter = UInt(12.W)
  val late, unused = UInt(16.W)
  // Pressures and credits are integer half units: 1 means 0.5.
  val latePressure, unusedPressure = UInt(2.W)
  val upCredit, downCredit, settle = UInt(4.W)
}

/** The release-only competitive policy from feat-pdb-all, driven by PAB events. */
class PABDepthController(params: PABDepthParameters) extends Module {
  val io = IO(new Bundle {
    val enabled = Input(Bool())
    val fixedDepth = Input(UInt(12.W))
    val events = Input(new PABDepthEvents)
    val depth = Output(UInt(12.W))
    val inserts = Output(UInt(log2Ceil(params.windowInserts max 2).W))
    val window = Output(Valid(new PABDepthWindow))
  })
  import PABDepthPolicy._
  val index = RegInit(levels.indexOf(params.initialDepth).U(3.W))
  val inserts = RegInit(0.U(log2Ceil(params.windowInserts max 2).W))
  val late, unused = RegInit(0.U(16.W))
  val upCredit, downCredit, settle = RegInit(0.U(4.W))
  def saturatedAdd(count: UInt, increment: UInt): UInt = {
    val sum = count +& increment
    Mux(sum > 65535.U, 65535.U(16.W), sum(15, 0))
  }
  def pressure(count: UInt, low: Int, high: Int): UInt =
    Mux(count <= low.U, 0.U(2.W), Mux(count < high.U, 1.U(2.W), 2.U(2.W)))
  val totalLate = saturatedAdd(late, io.events.late)
  val totalUnused = saturatedAdd(unused, io.events.unused)
  val latePressure = pressure(totalLate, params.lateLow, params.lateHigh)
  val unusedPressure = pressure(totalUnused, params.unusedLow, params.unusedHigh)
  val windowEnd = io.events.insert && inserts === (params.windowInserts - 1).U
  val depths = VecInit(levels.map(_.U(12.W)))
  io.depth := Mux(io.enabled, depths(index), io.fixedDepth)
  io.inserts := inserts
  val nextIndex = WireDefault(index)
  val nextUp, nextDown, nextSettle = WireDefault(0.U(4.W))
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
  late := Mux(windowEnd, 0.U, totalLate)
  unused := Mux(windowEnd, 0.U, totalUnused)
  when (io.events.insert) { inserts := Mux(windowEnd, 0.U, inserts + 1.U) }
  when (windowEnd) {
    index := nextIndex
    upCredit := nextUp
    downCredit := nextDown
    settle := nextSettle
  }
  io.window.valid := windowEnd
  io.window.bits.enabled := io.enabled
  io.window.bits.depthBefore := io.depth
  io.window.bits.depthAfter := Mux(io.enabled, depths(nextIndex), io.fixedDepth)
  io.window.bits.late := totalLate
  io.window.bits.unused := totalUnused
  io.window.bits.latePressure := latePressure
  io.window.bits.unusedPressure := unusedPressure
  io.window.bits.upCredit := nextUp
  io.window.bits.downCredit := nextDown
  io.window.bits.settle := nextSettle
  assert(io.enabled || VecInit(levels.map(d => io.fixedDepth === d.U)).asUInt.orR)
  assert(index < levels.size.U && inserts < params.windowInserts.U)
  assert(upCredit < params.creditHalfUnits.U && downCredit < params.creditHalfUnits.U)
  val previousIndex = RegNext(index, levels.indexOf(params.initialDepth).U)
  assert(index === previousIndex || RegNext(windowEnd, false.B))
}
