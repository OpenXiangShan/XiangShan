package xiangshan

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util.log2Ceil
import xiangshan.backend.ctrlblock.{DebugLsInfo, DebugMdpInfo}
import xiangshan.cache.DCacheBundle
import xiangshan.backend.fu.FuType

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
  val globalID = UInt(XLEN.W)
  val robIdx = UInt(log2Ceil(RobSize).W)
  val instType = FuType()
  val exceptType = UInt(ExceptionVec.ExceptionVecSize.W)
  val ivaddr = UInt(VAddrBits.W)
  val dvaddr = UInt(VAddrBits.W) // the l/s access address
  val dpaddr = UInt(VAddrBits.W) // need the physical address when the TLB is valid
  val tlbLatency = UInt(XLEN.W)  // original requirements is L1toL2TlbLatency
  val accessLatency = UInt(XLEN.W)  // RS out time --> write back time
  val executeLatency = UInt(XLEN.W)
  val issueLatency = UInt(XLEN.W)
  val lsInfo = new DebugLsInfo
  val mdpInfo = new DebugMdpInfo
  val issueTime = UInt(XLEN.W)
  val writebackTime = UInt(XLEN.W)
}

class LoadInfoEntry(implicit p: Parameters) extends XSBundle{
  val pc = UInt(VAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
  val cacheMiss = Bool()
  val tlbQueryLatency = UInt(64.W)
  val exeLatency = UInt(64.W)
}