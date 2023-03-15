package xiangshan

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import xiangshan.cache.DCacheBundle

/** Mem */
class LoadMissEntry(implicit p: Parameters) extends DCacheBundle {
  val timeCnt = UInt(XLEN.W)
  val paddr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  // 1:first hit, 2:first miss, 3:second miss
  val missState = UInt(3.W)
}