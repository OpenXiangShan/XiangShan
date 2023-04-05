package xiangshan.cache.dcache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSPerfAccumulate
import xiangshan._
import xiangshan.cache.HasDCacheParameters

case class WPUParameters
(
  enWPU: Boolean = true,
  algoName: String = "mru",
  enCfPred: Boolean = false
)

trait HasWPUParameters extends HasDCacheParameters{
  //auxiliary 1 bit is used to judge whether cache miss
  val auxWayBits = wayBits + 1
  val nTagIdx = nWays
  val TagIdxBits = log2Up(nTagIdx)

/*
  val AlgoWPUMap = Map(
    "MRU" -> Module(new MruWPU()),
    "MMRU" -> Module(new MmruWPU()),
    "UTAG" -> Module(new UtagWPU())
  )
*/
  def AlgoWPUMap(algo: String): BaseWPU = algo.toLowerCase match {
    case "mru" => Module(new MruWPU())
    case "mmru" => Module(new MmruWPU())
    case "utag" => Module(new UtagWPU())
    case t => throw new IllegalArgumentException(s"unknown WPU Algorithm $t")
  }
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

class ConflictPredictIO(implicit p:Parameters) extends WPUBuddle{
  // pred
  val s0_pc = Input(UInt(VAddrBits.W))
  // update
  // FIXME lyq: Updating in s1 may result in insufficient timing
  val s1_pc = Input(UInt(VAddrBits.W))
  val s1_dm_hit = Input(Bool())
}

class WPUIO(implicit p:Parameters) extends WPUBuddle{
  val req = Flipped(Decoupled(new WPUReq))
  val resp = ValidIO(new WPUResp)
  val update = Flipped(ValidIO(new WPUUpdate))
  val cfpred = new ConflictPredictIO
}

class DCacheWPU (implicit p:Parameters) extends WPUModule{
  val io = IO(new WPUIO)

  val wpu = AlgoWPUMap(wpuParam.algoName)

  val wayConflictPredictor = Module(new WayConflictPredictor)
  val s0_dmSel = Wire(Bool())

  /** pred */
  val s0_pred_way_conflict = Wire(Bool())
  val s0_pred_way_en = Wire(UInt(nWays.W))

  wayConflictPredictor.io.pred_en := io.req.valid
  wayConflictPredictor.io.pred_pc := io.cfpred.s0_pc
  s0_pred_way_conflict := wayConflictPredictor.io.pred_way_conflict

  s0_dmSel := false.B
  wpu.io.pred_en := io.req.valid
  wpu.io.pred_vaddr := io.req.bits.vaddr
  when (io.req.bits.replayCarry.valid){
    // replay carry
    s0_pred_way_en := io.req.bits.replayCarry.real_way_en
  }.otherwise {
    // way prediction
    s0_pred_way_en := wpu.io.pred_way_en

    if (wpuParam.enCfPred) {
      // selective direct mapping
      when(!s0_pred_way_conflict) {
        s0_pred_way_en := UIntToOH(get_direct_map_way(io.req.bits.vaddr))
        s0_dmSel := true.B
      }
    }

  }

  /** check and update in s1 */
  val s1_dmSel = RegNext(s0_dmSel)
  val s1_pred_way_en = RegNext(s0_pred_way_en)
  val s1_pred_fail = RegNext(io.resp.valid) && s1_pred_way_en =/= io.update.bits.s1_real_way_en
  val s1_pred_hit = RegNext(io.resp.valid) && s1_pred_way_en.orR && s1_pred_way_en === io.update.bits.s1_real_way_en

  wayConflictPredictor.io.update_en := io.update.valid
  wayConflictPredictor.io.update_pc := io.cfpred.s1_pc
  wayConflictPredictor.io.update_dm_hit := s1_dmSel && io.cfpred.s1_dm_hit
  wayConflictPredictor.io.update_sa_hit := !s1_dmSel && s1_pred_hit

  wpu.io.update_en := io.update.valid
  wpu.io.update_vaddr := io.update.bits.vaddr
  when(io.update.bits.s1_real_way_en.orR){ // not cache miss
    wpu.io.update_way_en := io.update.bits.s1_real_way_en
  }.otherwise{
    wpu.io.update_way_en := 0.U(nWays.W)
  }
  // FIXME lyq: if cache misses, it can be updated by way number replaced

  /** predict and response in s0 */
  io.req.ready := true.B
  if (wpuParam.enWPU) {
    io.resp.valid := io.req.valid
  } else {
    io.resp.valid := false.B
  }
  io.resp.bits.s0_pred_way_en := s0_pred_way_en
  assert(PopCount(io.resp.bits.s0_pred_way_en) <= 1.U, "tag should not match with more than 1 way")

  io.resp.bits.s1_pred_fail := s1_pred_fail

  // PerfLog
  // pred situation
  XSPerfAccumulate("wpu_pred_total", RegNext(io.resp.valid))
  XSPerfAccumulate("wpu_pred_succ", RegNext(io.resp.valid) && !s1_pred_fail)
  XSPerfAccumulate("wpu_pred_fail", RegNext(io.resp.valid) && s1_pred_fail)
  XSPerfAccumulate("wpu_pred_miss", RegNext(io.resp.valid) && !RegNext(s0_pred_way_en).orR)
  XSPerfAccumulate("wpu_real_miss", RegNext(io.resp.valid) && !io.update.bits.s1_real_way_en.orR)
  // pred component
  XSPerfAccumulate("wpu_pred_replayCarry", io.req.valid && io.req.bits.replayCarry.valid)
  if(!wpuParam.enCfPred){
    XSPerfAccumulate("wpu_pred_wayPrediction", io.req.valid && !io.req.bits.replayCarry.valid)
  }else{
    XSPerfAccumulate("wpu_pred_wayPrediction", io.req.valid && !io.req.bits.replayCarry.valid && s0_pred_way_conflict)
    XSPerfAccumulate("wpu_pred_directMap", io.req.valid && !io.req.bits.replayCarry.valid && !s0_pred_way_conflict)
    // dm situation
    XSPerfAccumulate("direct_map_all", io.update.valid)
    XSPerfAccumulate("direct_map_ok", io.update.valid && io.cfpred.s1_dm_hit)
  }
}

/** IdealWPU:
  * req in s1 and resp in s1
  */
class IdealWPU(implicit p:Parameters) extends WPUModule{
  val io = IO(new Bundle{
    val req = Output(new Bundle {
      val valid = Bool()
      val s1_real_way_en = UInt(nWays.W)
    })
    val resp = Output(new Bundle{
      val valid = Bool()
      val s1_pred_way_en = UInt(nWays.W)
    })
  })

  val s1_pred_way_en = io.req.s1_real_way_en

  if(wpuParam.enWPU){
    io.resp.valid := io.req.valid
  }else{
    io.resp.valid := false.B
  }
  io.resp.s1_pred_way_en := s1_pred_way_en
}
