package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import xiangshan._
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.mem.{LdPrefetchTrainBundle, StPrefetchTrainBundle, L1PrefetchReq}

class PrefetcherIO()(implicit p: Parameters) extends XSBundle {
  val ld_in = Flipped(Vec(exuParameters.LduCnt, ValidIO(new LdPrefetchTrainBundle())))
  val st_in = Flipped(Vec(exuParameters.StuCnt, ValidIO(new StPrefetchTrainBundle())))
  val tlb_req = new TlbRequestIO(nRespDups = 2)
  val pf_addr = ValidIO(UInt(PAddrBits.W))
  val l1_req = DecoupledIO(new L1PrefetchReq())
  val enable = Input(Bool())
}

class PrefetchReqBundle()(implicit p: Parameters) extends XSBundle {
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
  val pc    = UInt(VAddrBits.W)
}

trait PrefetcherParams

abstract class BasePrefecher()(implicit p: Parameters) extends XSModule {
  val io = IO(new PrefetcherIO())
}