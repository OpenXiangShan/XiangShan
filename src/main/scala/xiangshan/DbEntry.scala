package xiangshan

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util.log2Ceil
import xiangshan.backend.ctrlblock.{DebugLsInfo, DebugMdpInfo}
import xiangshan.cache.{DCacheBundle, HasDCacheParameters}
import xiangshan.backend.fu.FuType
import utility.MemReqSource
import xiangshan.mem.prefetch.HasL1PrefetchHelper

/** Mem */
class LoadMissEntry(implicit p: Parameters) extends DCacheBundle {
  val timeCnt = UInt(XLEN.W)
  val robIdx = UInt(log2Ceil(RobSize).W)
  val paddr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  // 1:first hit, 2:first miss, 3:second miss
  val missState = UInt(3.W)
}

class LoadAccessEntry(implicit p: Parameters) extends LoadMissEntry{
  val pred_way_num = UInt(XLEN.W)
  val dm_way_num = UInt(XLEN.W)
  val real_way_num = UInt(XLEN.W)
}

class InstInfoEntry(implicit p: Parameters) extends XSBundle{
  /*
   * The annotated signals are discarded in New Backend.
   * But it can be used as a signal reference for instinfo
   */
  val robIdx = UInt(log2Ceil(RobSize).W)
  // val globalID = UInt(XLEN.W)
  // val instType = FuType()
  // val mdpInfo = new DebugMdpInfo
  // val ivaddr = UInt(VAddrBits.W)
  val dvaddr = UInt(VAddrBits.W) // the l/s access address
  val dpaddr = UInt(VAddrBits.W) // need the physical address when the TLB is valid
  val issueTime = UInt(XLEN.W)
  val writebackTime = UInt(XLEN.W)
  val dispatchLatency = UInt(XLEN.W)
  val enqRsLatency = UInt(XLEN.W)
  val selectLatency = UInt(XLEN.W)
  val issueLatency = UInt(XLEN.W)
  val executeLatency = UInt(XLEN.W)
  val rsFuLatency = UInt(XLEN.W)
  // val commitLatency = UInt(XLEN.W) // can not record when writing back
  val tlbLatency = UInt(XLEN.W)  // original requirements is L1toL2TlbLatency
  val lsInfo = new DebugLsInfo
  val exceptType = UInt(ExceptionVec.ExceptionVecSize.W)
}

class LoadInfoEntry(implicit p: Parameters) extends XSBundle{
  val pc = UInt(VAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
  val cacheMiss = Bool()
  val tlbQueryLatency = UInt(64.W)
  val exeLatency = UInt(64.W)
}

class StreamPFTraceInEntry(implicit p: Parameters) extends XSBundle with HasL1PrefetchHelper{
  val TriggerPC = UInt(VAddrBits.W)
  val TriggerVaddr = UInt(VAddrBits.W)
  val PFVaddr = UInt(VAddrBits.W)
  val PFSink = UInt(SINK_BITS.W)
}

class StreamTrainTraceEntry(implicit p: Parameters) extends XSBundle with HasDCacheParameters{
  val Type = UInt(MemReqSource.reqSourceBits.W)
  val OldAddr = UInt(VAddrBits.W)
  val CurAddr = UInt(VAddrBits.W)
  val Offset = UInt(32.W)
  val Score = UInt(32.W)
  val Miss = Bool()
}

class StreamPFTraceOutEntry(implicit p: Parameters) extends XSBundle with HasL1PrefetchHelper{
  val PFVaddr = UInt(VAddrBits.W)
  val PFSink = UInt(SINK_BITS.W)
}