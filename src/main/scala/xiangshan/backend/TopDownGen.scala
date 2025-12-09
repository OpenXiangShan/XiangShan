package xiangshan.backend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utility._

class TopDownGen(implicit p: Parameters) extends XSModule
  with HasPerfEvents {
  val io = IO(new TopDownGenIO)

  val uopsIssued = io.intTopDown.uopsIssued || io.fpTopDown.uopsIssued || io.vecTopDown.uopsIssued
  val uopsIssuedCnt = io.intTopDown.uopsIssuedCnt + io.vecTopDown.uopsIssuedCnt
  val noStoreIssued = io.intTopDown.noStoreIssued

  val fewUopsIssued = (0 until p(XSCoreParamsKey).fewUops).map(_.U === uopsIssuedCnt).reduce(_ || _)

  val stallLoad = !uopsIssued
  val stallStore = uopsIssued && noStoreIssued

  val stallLoadDly2 = DelayN(stallLoad, 2)
  val stallStoreDly2 = DelayN(stallStore, 2)

  val lqEmpty = io.topDownInfo.lqEmpty
  val sqEmpty = io.topDownInfo.sqEmpty
  val l1Miss = io.topDownInfo.l1Miss
  val l2Miss = io.topDownInfo.l2TopMiss.l2Miss
  val l3Miss = io.topDownInfo.l2TopMiss.l3Miss

  val memStallAnyLoad = stallLoadDly2 && !lqEmpty
  val memStallStore = stallStoreDly2 && !sqEmpty
  val memStallL1Miss = memStallAnyLoad && l1Miss
  val memStallL2Miss = memStallL1Miss && l2Miss
  val memStallL3Miss = memStallL2Miss && l3Miss

  io.topDownInfo.noUopsIssued := stallLoad
  
  XSPerfAccumulate("exec_stall_cycle",   fewUopsIssued)
  XSPerfAccumulate("mem_stall_store",    memStallStore)
  XSPerfAccumulate("mem_stall_l1miss",   memStallL1Miss)
  XSPerfAccumulate("mem_stall_l2miss",   memStallL2Miss)
  XSPerfAccumulate("mem_stall_l3miss",   memStallL3Miss)

  val perfEvents = Seq(
    ("EXEC_STALL_CYCLE",  fewUopsIssued),
    ("MEMSTALL_STORE",    memStallStore),
    ("MEMSTALL_L1MISS",   memStallL1Miss),
    ("MEMSTALL_L2MISS",   memStallL2Miss),
    ("MEMSTALL_L3MISS",   memStallL3Miss),
  )
  generatePerfEvent()
}

class TopDownGenIO(implicit p: Parameters) extends XSBundle {
  val intTopDown = Flipped(new UopTopDown)
  val fpTopDown  = Flipped(new UopTopDown)
  val vecTopDown = Flipped(new UopTopDown)
  val topDownInfo = new TopDownInfo
}
