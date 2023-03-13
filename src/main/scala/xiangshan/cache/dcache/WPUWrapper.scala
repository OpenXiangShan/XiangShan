package xiangshan.cache.dcache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSPerfAccumulate
import xiangshan._
import xiangshan.cache.HasDCacheParameters

trait HasWPUParameters extends HasDCacheParameters{
  //auxiliary 1 bit is used to judge whether cache miss
  val auxWayBits = wayBits + 1
  val nTagIdx = nWays
  val TagIdxBits = log2Up(nTagIdx)
  val AlgorithmList = List("MRU", "MMRU", "UTAG")
}

abstract class WPUBuddle(implicit P: Parameters) extends XSBundle with HasWPUParameters
abstract class WPUModule(implicit P: Parameters) extends XSModule with HasWPUParameters

class ReplayCarry(implicit p: Parameters) extends XSBundle with HasDCacheParameters {
  val real_way_en = UInt(nWays.W)
  val valid = Bool()
}

object ReplayCarry{
  def apply(rwe: UInt, v: Bool)(implicit p: Parameters): ReplayCarry = {
    val rcry = Wire(new ReplayCarry)
    rcry.real_way_en := rwe
    rcry.valid := v
    rcry
  }

  def init(implicit p: Parameters): ReplayCarry = {
    val rcry = Wire(new ReplayCarry)
    rcry.real_way_en := 0.U
    rcry.valid := false.B
    rcry
  }
}

class WPUReq(implicit p: Parameters) extends WPUBuddle {
  val vaddr = UInt(VAddrBits.W)
  val replayCarry = new ReplayCarry
}

class WPUResp(implicit p:Parameters) extends WPUBuddle{
  val s0_pred_way_en = UInt(nWays.W)
  val s1_pred_fail = Bool()
}

class WPUUpdate(implicit p: Parameters) extends WPUBuddle{
  val vaddr = UInt(VAddrBits.W)
  val s1_real_way_en = UInt(nWays.W)
}

class WPUIO(implicit p:Parameters) extends WPUBuddle{
  val req = Flipped(Decoupled(new WPUReq))
  val resp = ValidIO(new WPUResp)
  val update = Flipped(ValidIO(new WPUUpdate))
}

class DCacheWPU (implicit p:Parameters) extends WPUModule{
  val io = IO(new WPUIO)

  val wpu = Module(new MmruWPU)

  // predict and response in s0
  io.req.ready := true.B
  if(EnableDCacheWPU){
    io.resp.valid := io.req.valid
  }else{
    io.resp.valid := false.B
  }
  wpu.io.pred_en := io.req.valid
  wpu.io.pred_vaddr := io.req.bits.vaddr
  val s0_pred_way_en = Wire(UInt(nWays.W))
  // when (io.req.valid){
  when (io.req.bits.replayCarry.valid){
    s0_pred_way_en := io.req.bits.replayCarry.real_way_en
  }.otherwise{
    s0_pred_way_en := wpu.io.pred_way_en
  }
  // }.otherwise{
  //   s0_pred_way_en := 0.U(nWays.W)
  // }
  io.resp.bits.s0_pred_way_en := s0_pred_way_en
  assert(PopCount(io.resp.bits.s0_pred_way_en) <= 1.U, "tag should not match with more than 1 way")

  // check and update in s1
  wpu.io.update_en := io.update.valid
  wpu.io.update_vaddr := io.update.bits.vaddr

  when(io.update.bits.s1_real_way_en.orR){ // not cache miss
    wpu.io.update_way_en := io.update.bits.s1_real_way_en
  }.otherwise{
    wpu.io.update_way_en := 0.U(nWays.W)
  }
  // FIXME lyq: if cache misses, it can be updated by way number replaced

  val s1_pred_fail = RegNext(s0_pred_way_en) =/= io.update.bits.s1_real_way_en && RegNext(io.resp.valid)
  io.resp.bits.s1_pred_fail := s1_pred_fail

  //in s1
  XSPerfAccumulate("wpu_pred_total", RegNext(io.resp.valid))
  XSPerfAccumulate("wpu_pred_succ", !s1_pred_fail && RegNext(io.resp.valid))
  XSPerfAccumulate("wpu_pred_fail", s1_pred_fail && RegNext(io.resp.valid))
  XSPerfAccumulate("wpu_pred_miss", RegNext(s0_pred_way_en).orR)
  XSPerfAccumulate("wpu_real_miss", io.update.bits.s1_real_way_en.orR)
}

/** IdealWPU:
  * req in s1 and resp in s1
  */
class IdealWPUIO(implicit p:Parameters) extends WPUBuddle{
  val req = Flipped(ValidIO(new WPUReq))
  val resp = ValidIO(new WPUResp)
  val update = Flipped(ValidIO(new WPUUpdate))
}

class IdealWPU(implicit p:Parameters) extends WPUModule{
  val io = IO(new WPUIO)

  val s1_pred_way_en = io.update.bits.s1_real_way_en

  if(EnableDCacheWPU){
    io.resp.valid := io.req.valid
  }else{
    io.resp.valid := false.B
  }
  io.resp.bits.s0_pred_way_en := s1_pred_way_en
}
