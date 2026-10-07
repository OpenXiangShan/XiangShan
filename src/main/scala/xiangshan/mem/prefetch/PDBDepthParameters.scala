package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Field

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

class PDBDepthWindow extends Bundle {
  val enabled = Bool()
  val depthBefore, depthAfter = UInt(12.W)
  val late, used, unused = UInt(16.W)
  // Integer half units: 0, 1, 2 represent pressures 0, 0.5, 1.
  val latePressure, unusedPressure = UInt(2.W)
  val upCredit, downCredit, settle = UInt(4.W)
  val usedMoveShadow, unusedMoveShadow = Bool()
}

class PDBDepthState extends Bundle {
  val depth = UInt(12.W)
  val refills = UInt(9.W)
  val usedMoveShadow, unusedMoveShadow = Bool()
  val window = Valid(new PDBDepthWindow)
}
