package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import xiangshan._
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.mem.{L1PrefetchReq, LsPipelineBundle}

class PrefetcherIO()(implicit p: Parameters) extends XSBundle {
  val ld_in = Flipped(Vec(exuParameters.LduCnt, ValidIO(new LsPipelineBundle())))
  val tlb_req = new TlbRequestIO(nRespDups = 2)
  val pf_addr = ValidIO(UInt(PAddrBits.W))
  val l1_req = DecoupledIO(new L1PrefetchReq())
  val enable = Input(Bool())
}

trait PrefetcherParams

abstract class BasePrefecher()(implicit p: Parameters) extends XSModule {
  val io = IO(new PrefetcherIO())
}