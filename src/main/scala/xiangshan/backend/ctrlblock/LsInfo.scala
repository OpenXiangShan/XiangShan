package xiangshan.backend.ctrlblock

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.XSBundle

class DebugMdpInfo(implicit p: Parameters) extends XSBundle{
  val ssid = UInt(SSIDWidth.W)
  val waitAllStore = Bool()
}

class DebugLsInfo(implicit p: Parameters) extends XSBundle{
  val s1 = new Bundle{
    val isTlbFirstMiss = Bool() // in s1
    val isBankConflict = Bool() // in s1
    val isLoadToLoadForward = Bool()
    val isReplayFast = Bool()
  }
  val s2 = new Bundle{
    val isDcacheFirstMiss = Bool() // in s2 (predicted result is in s1 when using WPU, real result is in s2)
    val isForwardFail = Bool() // in s2
    val isReplaySlow = Bool()
    val isLoadReplayTLBMiss = Bool()
    val isLoadReplayCacheMiss = Bool()
  }
  val replayCnt = UInt(XLEN.W)

  def s1SignalEnable(ena: DebugLsInfo) = {
    when(ena.s1.isTlbFirstMiss) { s1.isTlbFirstMiss := true.B }
    when(ena.s1.isBankConflict) { s1.isBankConflict := true.B }
    when(ena.s1.isLoadToLoadForward) { s1.isLoadToLoadForward := true.B }
    when(ena.s1.isReplayFast) {
      s1.isReplayFast := true.B
      replayCnt := replayCnt + 1.U
    }
  }

  def s2SignalEnable(ena: DebugLsInfo) = {
    when(ena.s2.isDcacheFirstMiss) { s2.isDcacheFirstMiss := true.B }
    when(ena.s2.isForwardFail) { s2.isForwardFail := true.B }
    when(ena.s2.isLoadReplayTLBMiss) { s2.isLoadReplayTLBMiss := true.B }
    when(ena.s2.isLoadReplayCacheMiss) { s2.isLoadReplayCacheMiss := true.B }
    when(ena.s2.isReplaySlow) {
      s2.isReplaySlow := true.B
      replayCnt := replayCnt + 1.U
    }
  }
}

object DebugLsInfo{
  def init(implicit p: Parameters): DebugLsInfo = {
    val lsInfo = Wire(new DebugLsInfo)
    lsInfo.s1.isTlbFirstMiss := false.B
    lsInfo.s1.isBankConflict := false.B
    lsInfo.s1.isLoadToLoadForward := false.B
    lsInfo.s1.isReplayFast := false.B
    lsInfo.s2.isDcacheFirstMiss := false.B
    lsInfo.s2.isForwardFail := false.B
    lsInfo.s2.isReplaySlow := false.B
    lsInfo.s2.isLoadReplayTLBMiss := false.B
    lsInfo.s2.isLoadReplayCacheMiss := false.B
    lsInfo.replayCnt := 0.U
    lsInfo
  }
}

class DebugLsInfoBundle(implicit p: Parameters) extends DebugLsInfo {
  // unified processing at the end stage of load/store  ==> s2  ==> bug that will write error robIdx data
  val s1_robIdx = UInt(log2Ceil(RobSize).W)
  val s2_robIdx = UInt(log2Ceil(RobSize).W)
}

class DebugLSIO(implicit p: Parameters) extends XSBundle {
  val debugLsInfo = Vec(backendParams.LduCnt + backendParams.StaCnt, Output(new DebugLsInfoBundle))
}
