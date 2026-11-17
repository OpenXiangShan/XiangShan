package xiangshan.backend.ctrlblock

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.XSBundle
import xiangshan.mem.LoadReplayCauses

class DebugMdpInfo(implicit p: Parameters) extends XSBundle{
  val ssid = UInt(SSIDWidth.W)
  val waitAllStore = Bool()
}

class DebugLsInfo(implicit p: Parameters) extends XSBundle{
  val s1_isTlbFirstMiss = Bool() // in s1
  val s1_isLoadToLoadForward = Bool()
  val s2_isBankConflict = Bool()
  val s2_isDcacheFirstMiss = Bool() // in s2 (predicted result is in s1 when using WPU, real result is in s2)
  val s2_isForwardFail = Bool() // in s2
  val s3_isReplayFast = Bool()
  val s3_isReplaySlow = Bool()
  val s3_isReplayRS = Bool()
  val s3_isReplay = Bool()
  val replayCause = Vec(LoadReplayCauses.allCauses, Bool())
  val replayCnt = UInt(XLEN.W)

  def s1SignalEnable(ena: DebugLsInfo) = {
    when(ena.s1_isTlbFirstMiss) { s1_isTlbFirstMiss := true.B }
    when(ena.s1_isLoadToLoadForward) { s1_isLoadToLoadForward := true.B }
  }

  def s2SignalEnable(ena: DebugLsInfo) = {
    when(ena.s2_isBankConflict) { s2_isBankConflict := true.B }
    when(ena.s2_isDcacheFirstMiss) { s2_isDcacheFirstMiss := true.B }
    when(ena.s2_isForwardFail) { s2_isForwardFail := true.B }
  }
  def s3SignalEnable(ena: DebugLsInfo) = {
    when(ena.s3_isReplayFast) { s3_isReplayFast := true.B }
    when(ena.s3_isReplaySlow) { s3_isReplaySlow := true.B }
    when(ena.s3_isReplayRS) { s3_isReplayRS := true.B }
    when(ena.s3_isReplay) {
      s3_isReplay := true.B
      replayCnt := replayCnt + 1.U
      when((ena.replayCause.asUInt ^ replayCause.asUInt).orR) {
        replayCause := ena.replayCause.zipWithIndex.map{ case (x, i) => x | replayCause(i) }
      }
    }
  }
}

object DebugLsInfo {
  def init(implicit p: Parameters): DebugLsInfo = {
    val lsInfo = Wire(new DebugLsInfo)
    lsInfo.s1_isTlbFirstMiss := false.B
    lsInfo.s1_isLoadToLoadForward := false.B
    lsInfo.s2_isBankConflict := false.B
    lsInfo.s2_isDcacheFirstMiss := false.B
    lsInfo.s2_isForwardFail := false.B
    lsInfo.s3_isReplayFast := false.B
    lsInfo.s3_isReplaySlow := false.B
    lsInfo.s3_isReplayRS := false.B
    lsInfo.s3_isReplay := false.B
    lsInfo.replayCnt := 0.U
    lsInfo.replayCause := Seq.fill(LoadReplayCauses.allCauses)(false.B)
    lsInfo
  }
}

class DebugLsInfoBundle(implicit p: Parameters) extends DebugLsInfo {
  // unified processing at the end stage of load/store  ==> s2  ==> bug that will write error robIdx data
  val s1_robIdx = UInt(log2Ceil(RobSize).W)
  val s2_robIdx = UInt(log2Ceil(RobSize).W)
  val s3_robIdx = UInt(log2Ceil(RobSize).W)
}

class DebugLSIO(implicit p: Parameters) extends XSBundle {
  val debugLsInfo = Vec(backendParams.LduCnt + backendParams.HyuCnt + backendParams.StaCnt + backendParams.HyuCnt, Output(new DebugLsInfoBundle))
}
class LsTopdownInfo(implicit p: Parameters) extends XSBundle {
  val s1 = new Bundle {
    val robIdx = UInt(log2Ceil(RobSize).W)
    val vaddr_valid = Bool()
    val vaddr_bits = UInt(VAddrBits.W)
  }
  val s2 = new Bundle {
    val robIdx = UInt(log2Ceil(RobSize).W)
    val paddr_valid = Bool()
    val paddr_bits = UInt(PAddrBits.W)
    val cache_miss_en = Bool()
    val first_real_miss = Bool()
  }

  def s1SignalEnable(ena: LsTopdownInfo) = {
    when(ena.s1.vaddr_valid) {
      s1.vaddr_valid := true.B
      s1.vaddr_bits := ena.s1.vaddr_bits
    }
  }

  def s2SignalEnable(ena: LsTopdownInfo) = {
    when(ena.s2.paddr_valid) {
      s2.paddr_valid := true.B
      s2.paddr_bits := ena.s2.paddr_bits
    }
    when(ena.s2.cache_miss_en) {
      s2.first_real_miss := ena.s2.first_real_miss
    }
  }
}

object LsTopdownInfo {
  def init(implicit p: Parameters): LsTopdownInfo = 0.U.asTypeOf(new LsTopdownInfo)
}