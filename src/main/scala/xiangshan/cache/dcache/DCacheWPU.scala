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
}

class WPUReq(implicit p: Parameters) extends WPUBuddle {
  val vaddr = UInt(PAddrBits.W)
  val replayCarry = new ReplayCarry
}

class WPUResp(implicit p:Parameters) extends WPUBuddle{
  val predict_way = UInt(wayBits.W)
  val predict_way_en = UInt(nWays.W)
}

class WPUCheck(implicit p: Parameters) extends WPUBuddle{
  val s1_tag_resp = Input(Vec(nWays, UInt(encTagBits.W)))
  val s1_meta_resp = Input(Vec(nWays, new Meta))
  val s1_real_tag = Input(UInt(encTagBits.W))
}

class WPUIO(implicit p:Parameters) extends WPUBuddle{
  val req = Flipped(Decoupled(new WPUReq))
  val resp = ValidIO(new WPUResp)
  val check = Flipped(ValidIO(new WPUCheck))
  val s2_pred_fail = Output(Bool())
  val s2_real_way_en = Output(UInt(nWays.W))
}

class DCacheWPU (implicit p:Parameters) extends WPUModule{
  val io = IO(new WPUIO)
  
  // deploy
  val predict_regs = RegInit(VecInit(Seq.fill(nSets)(0.U(wayBits.W))))

  // predict in s0
  io.req.ready := true.B
  val idx = addr_to_dcache_set(io.req.bits.vaddr)
  val pred_way_en = Wire(UInt(nWays.W))
  // when (io.req.valid){
    when (io.req.bits.replayCarry.valid){
      pred_way_en := io.req.bits.replayCarry.real_way_en
    }.otherwise{
      pred_way_en := predict_regs(idx)
    }
  // }.otherwise{
  //   pred_way_en := 0.U(nWays.W)
  // }

  // resp in s1
  val s1_pred_way_en = Reg(chiselTypeOf(pred_way_en))
  s1_pred_way_en := pred_way_en
  io.resp.bits.predict_way := OHToUInt(s1_pred_way_en)
  io.resp.bits.predict_way_en := s1_pred_way_en
  io.resp.valid := true.B
  assert(RegNext(PopCount(io.resp.bits.predict_way_en) <= 1.U), "tag should not match with more than 1 way")

  // check and update in s1
  val real_way_en = VecInit((0 until nWays).map(x =>
    io.check.bits.s1_tag_resp(x.U) === io.check.bits.s1_real_tag && io.check.bits.s1_meta_resp(x.U).coh.isValid()
  )).asUInt
  when(io.check.valid) {
    when(real_way_en.orR){ // not cache miss
      predict_regs(RegNext(idx)) := real_way_en
    }
  }

  // correct in s2 (not in s1 due to meeting the timing requirements)
  // val s2_pred_hit = RegNext(s1_pred_way_en =/= real_way_en && real_way_en.orR)
  val s2_pred_fail = RegNext(s1_pred_way_en =/= real_way_en && io.resp.valid)
  val s2_real_way_en = Reg(chiselTypeOf(real_way_en))
  s2_real_way_en := real_way_en
  
  // wire out
  io.s2_pred_fail := s2_pred_fail
  io.s2_real_way_en := s2_real_way_en
}



/** IdealWPU:
  * req in s1 and resp in s1
  */
class IdealWPUIO(implicit p:Parameters) extends WPUBuddle{
  val req = Flipped(ValidIO(new WPUReq))
  val resp = ValidIO(new WPUResp)
  val idealIf = new WPUCheck()
}
class IdealWPU(implicit p:Parameters) extends WPUModule{
  val io = IO(new IdealWPUIO)

  val pred_way = MuxCase(
    nWays.U,
    io.idealIf.s1_tag_resp.zipWithIndex.map {
      case (x, i) => (x === io.idealIf.s1_real_tag && io.idealIf.s1_meta_resp(i.U).coh.isValid(), i.U)
    }
  )

  io.resp.valid := true.B
  io.resp.bits.predict_way := pred_way
  io.resp.bits.predict_way_en := VecInit((0 until nWays).map(x => x.U === pred_way)).asUInt
  
  /* 
  val predict_way_en = VecInit((0 until nWays).map(x =>
    io.idealIf.s1_real_tag(x.U) === io.idealIf.s1_real_tag && io.idealIf.s1_meta_resp(x.U).coh.isValid()
  )).asUInt
  io.resp.valid := true.B
  io.resp.bits.predict_way_en := predict_way_oh
  io.resp.bits.predict_way := OHToUInt(predict_way_oh)
  */
}
