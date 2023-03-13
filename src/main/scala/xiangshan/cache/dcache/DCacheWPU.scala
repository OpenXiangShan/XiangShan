package xiangshan.cache.dcache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.cache.{HasDCacheParameters, HasL1CacheParameters, L1CacheParameters, Meta}

trait HasWPUParameters extends HasDCacheParameters
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
  val vaddr = UInt(PAddrBits.W)
  val replayCarry = new ReplayCarry
}

class WPUResp(implicit p:Parameters) extends WPUBuddle{
  val s0_pred_way_en = UInt(nWays.W)
  val s2_pred_fail = Bool()
}

class WPUCheckIO(implicit p: Parameters) extends WPUBuddle{
  val s1_tag_resp = Input(Vec(nWays, UInt(encTagBits.W)))
  val s1_meta_resp = Input(Vec(nWays, new Meta))
  val s1_real_tag = Input(UInt(encTagBits.W))
}

class WPUUpdate(implicit p: Parameters) extends WPUBuddle{
  val s1_real_way_en = UInt(nWays.W)
}

class WPUIO(implicit p:Parameters) extends WPUBuddle{
  val req = Flipped(Decoupled(new WPUReq))
  val resp = ValidIO(new WPUResp)
  val update = Flipped(ValidIO(new WPUUpdate))
}

class DCacheWPU (implicit p:Parameters) extends WPUModule{
  val io = IO(new WPUIO)

  val predict_regs = RegInit(VecInit(Seq.fill(nSets)(0.U(wayBits.W))))

  // predict and response in s0
  io.req.ready := true.B
  if(EnableDCacheWPU){
    io.resp.valid := true.B
  }else{
    io.resp.valid := io.req.valid
  }
  val idx = addr_to_dcache_set(io.req.bits.vaddr)
  val s0_pred_way_en = Wire(UInt(nWays.W))
  // when (io.req.valid){
    when (io.req.bits.replayCarry.valid){
      s0_pred_way_en := io.req.bits.replayCarry.real_way_en
    }.otherwise{
      s0_pred_way_en := UIntToOH(predict_regs(idx))
    }
  // }.otherwise{
  //   s0_pred_way_en := 0.U(nWays.W)
  // }
  io.resp.bits.s0_pred_way_en := s0_pred_way_en
  assert(PopCount(io.resp.bits.s0_pred_way_en) <= 1.U, "tag should not match with more than 1 way")

  // check and update in s1
  when(io.update.valid) {
    when(io.update.bits.s1_real_way_en.orR){ // not cache miss
      predict_regs(RegNext(idx)) := OHToUInt(io.update.bits.s1_real_way_en)
    }
    // FIXME lyq: if cache misses, it can be updated by way number replaced
  }

  // correct in s2 (not in s1 due to meeting the timing requirements)
  // val s2_pred_hit = RegNext(s1_pred_way_en =/= real_way_en && real_way_en.orR)
  val s2_pred_fail = RegNext(RegNext(s0_pred_way_en) =/= io.update.bits.s1_real_way_en && RegNext(io.resp.valid))
  io.resp.bits.s2_pred_fail := s2_pred_fail
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
  val io = IO(new IdealWPUIO)

  val s1_pred_way_en = io.update.bits.s1_real_way_en

  if(EnableDCacheWPU){
    io.resp.valid := true.B
  }else{
    io.resp.valid := false.B
  }
  io.resp.bits.s0_pred_way_en := s1_pred_way_en
}
