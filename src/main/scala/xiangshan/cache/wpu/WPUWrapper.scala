package xiangshan.cache.wpu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSPerfAccumulate
import xiangshan._
import xiangshan.cache.{DCacheModule, HasDCacheParameters}
import xiangshan.frontend.icache.HasICacheParameters

class ReplayCarry(nWays: Int)(implicit p: Parameters) extends XSBundle {
  val real_way_en = UInt(nWays.W)
  val valid = Bool()
}

object ReplayCarry{
  def apply(nWays: Int, rwe: UInt = 0.U, v: Bool = false.B)(implicit p: Parameters): ReplayCarry = {
    val rcry = Wire(new ReplayCarry(nWays))
    rcry.real_way_en := rwe
    rcry.valid := v
    rcry
  }

  def init(nWays: Int)(implicit p: Parameters): ReplayCarry = {
    val rcry = Wire(new ReplayCarry(nWays))
    rcry.real_way_en := 0.U
    rcry.valid := false.B
    rcry
  }
}

class WPUBaseReq(implicit p: Parameters) extends BaseWPUBundle{
  val vaddr = UInt(VAddrBits.W)
}

class WPUReplayedReq(nWays: Int)(implicit p: Parameters) extends WPUBaseReq {
  val replayCarry = new ReplayCarry(nWays)
}

class WPUResp(nWays:Int)(implicit p:Parameters) extends BaseWPUBundle{
  val s0_pred_way_en = UInt(nWays.W)
}

class WPUUpdate(nWays:Int)(implicit p:Parameters) extends BaseWPUBundle{
  val vaddr = UInt(VAddrBits.W)
  val s1_real_way_en = UInt(nWays.W)
}

class WPUUpdateLookup(nWays:Int)(implicit p:Parameters) extends WPUUpdate(nWays){
  val s1_pred_way_en = UInt(nWays.W)
}

class ConflictPredictIO(nWays:Int)(implicit p:Parameters) extends BaseWPUBundle{
  // pred
  val s0_pc = Input(UInt(VAddrBits.W))
  // update
  // FIXME lyq: Updating in s1 may result in insufficient timing
  val s1_pc = Input(UInt(VAddrBits.W))
  val s1_dm_hit = Input(Bool())
}

class IwpuBaseIO(nWays:Int, nPorts: Int)(implicit p:Parameters) extends BaseWPUBundle{
  val req = Vec(nPorts, Flipped(Decoupled(new WPUBaseReq)))
  val resp = Vec(nPorts, ValidIO(new WPUResp(nWays)))
  val lookup_upd = Vec(nPorts, Flipped(ValidIO(new WPUUpdateLookup(nWays))))
}

class IwpuIO(nWays:Int, nPorts: Int)(implicit p:Parameters) extends IwpuBaseIO(nWays, nPorts){
  val tagwrite_upd = Vec(nPorts, Flipped(ValidIO(new WPUUpdate(nWays))))
}

class DwpuIO(nWays:Int)(implicit p:Parameters) extends BaseWPUBundle{
  val req = Flipped(Decoupled(new WPUReplayedReq(nWays)))
  val resp = ValidIO(new WPUResp(nWays))
  val lookup_upd = Flipped(ValidIO(new WPUUpdateLookup(nWays)))
  val tagwrite_upd = Flipped(ValidIO(new WPUUpdate(nWays)))
  val cfpred = new ConflictPredictIO(nWays)
}

class DCacheWpuWrapper (nPorts: Int = 1) (implicit p:Parameters) extends DCacheModule with HasWPUParameters  {
  val wpu = AlgoWPUMap(dwpuParam, nPorts)
  val wayConflictPredictor = Module(new WayConflictPredictor)
  val io = IO(new DwpuIO(nWays))

  /** pred */
  val s0_dmSel = Wire(Bool())
  val s0_pred_way_conflict = Wire(Bool())
  val s0_pred_way_en = Wire(UInt(nWays.W))

  wayConflictPredictor.io.pred_en := io.req.valid
  wayConflictPredictor.io.pred_pc := io.cfpred.s0_pc
  s0_pred_way_conflict := wayConflictPredictor.io.pred_way_conflict

  s0_dmSel := false.B
  wpu.io.predVec(0).en := io.req.valid
  wpu.io.predVec(0).vaddr := io.req.bits.vaddr
  when (io.req.bits.replayCarry.valid){
    // replay carry
    s0_pred_way_en := io.req.bits.replayCarry.real_way_en
  }.otherwise {
    // way prediction
    s0_pred_way_en := wpu.io.predVec(0).way_en

    if (dwpuParam.enCfPred) {
      // selective direct mapping
      when(!s0_pred_way_conflict) {
        s0_pred_way_en := UIntToOH(get_direct_map_way(io.req.bits.vaddr))
        s0_dmSel := true.B
      }
    }

  }

  /** check and update in s1 */
  val lookup_valid = io.lookup_upd.valid
  val s1_dmSel = RegNext(s0_dmSel)
  val s1_pred_way_en = io.lookup_upd.bits.s1_pred_way_en
  val s1_pred_fail = io.lookup_upd.valid && s1_pred_way_en =/= io.lookup_upd.bits.s1_real_way_en
  val s1_hit = !s1_pred_fail && s1_pred_way_en.orR

  val s0_replay_upd = Wire(new BaseWpuUpdateBundle(nWays))
  s0_replay_upd.en := io.req.valid && io.req.bits.replayCarry.valid
  s0_replay_upd.vaddr := io.req.bits.vaddr
  s0_replay_upd.way_en := io.req.bits.replayCarry.real_way_en
  val s1_replay_upd = RegNext(s0_replay_upd)

  wayConflictPredictor.io.update_en := io.lookup_upd.valid
  wayConflictPredictor.io.update_pc := io.cfpred.s1_pc
  wayConflictPredictor.io.update_dm_hit := s1_dmSel && io.cfpred.s1_dm_hit
  wayConflictPredictor.io.update_sa_hit := !s1_dmSel && s1_hit

  // look up res
  wpu.io.updLookup(0).en := io.lookup_upd.valid
  wpu.io.updLookup(0).vaddr := io.lookup_upd.bits.vaddr
  wpu.io.updLookup(0).way_en := io.lookup_upd.bits.s1_real_way_en
  wpu.io.updLookup(0).pred_way_en := io.lookup_upd.bits.s1_pred_way_en

  // which will update in look up pred fail
  wpu.io.updReplaycarry(0) := s1_replay_upd

  // replace / tag write
  wpu.io.updTagwrite(0).en := io.tagwrite_upd.valid
  wpu.io.updTagwrite(0).vaddr := io.tagwrite_upd.bits.vaddr
  wpu.io.updTagwrite(0).way_en := io.tagwrite_upd.bits.s1_real_way_en

  /** predict and response in s0 */
  io.req.ready := true.B
  if (dwpuParam.enWPU) {
    io.resp.valid := io.req.valid
  } else {
    io.resp.valid := false.B
  }
  io.resp.bits.s0_pred_way_en := s0_pred_way_en
  assert(PopCount(io.resp.bits.s0_pred_way_en) <= 1.U, "tag should not match with more than 1 way")

  // PerfLog
  // pred situation
  XSPerfAccumulate("wpu_pred_total", lookup_valid)
  XSPerfAccumulate("wpu_pred_succ", lookup_valid && !s1_pred_fail)
  XSPerfAccumulate("wpu_pred_fail", lookup_valid && s1_pred_fail)
  XSPerfAccumulate("wpu_pred_miss", lookup_valid && !s1_pred_way_en.orR)
  XSPerfAccumulate("wpu_real_miss", lookup_valid && !io.lookup_upd.bits.s1_real_way_en.orR)
  // pred component
  XSPerfAccumulate("wpu_pred_replayCarry", io.req.valid && io.req.bits.replayCarry.valid)
  if(!dwpuParam.enCfPred){
    XSPerfAccumulate("wpu_pred_wayPrediction", io.req.valid && !io.req.bits.replayCarry.valid)
  }else{
    XSPerfAccumulate("wpu_pred_wayPrediction", io.req.valid && !io.req.bits.replayCarry.valid && s0_pred_way_conflict)
    XSPerfAccumulate("wpu_pred_directMap", io.req.valid && !io.req.bits.replayCarry.valid && !s0_pred_way_conflict)
    // dm situation
    XSPerfAccumulate("direct_map_all", io.lookup_upd.valid)
    XSPerfAccumulate("direct_map_ok", io.lookup_upd.valid && io.cfpred.s1_dm_hit)
  }
}


class ICacheWpuWrapper (nPorts: Int) (implicit p:Parameters) extends WPUModule with HasICacheParameters {
  val wpu = AlgoWPUMap(iwpuParam, nPorts)
  val io = IO(new IwpuIO(nWays, nPorts))

  val s1_pred_fail = Wire(Vec(nPorts, Bool()))
  val s0_pred_way_en = Wire(Vec(nPorts, UInt(nWays.W)))
  val s1_pred_way_en = Wire(Vec(nPorts, UInt(nWays.W)))
  val s1_real_way_en = Wire(Vec(nPorts, UInt(nWays.W)))
  /** pred in s0*/
  for (i <- 0 until nPorts){
    wpu.io.predVec(i).en := io.req(i).valid
    wpu.io.predVec(i).vaddr := io.req(i).bits.vaddr
    s0_pred_way_en(i) := wpu.io.predVec(i).way_en
    // io
    io.req(i).ready := true.B
    if (iwpuParam.enWPU) {
      io.resp(i).valid := io.req(i).valid
    } else {
      io.resp(i).valid := false.B
    }
    io.resp(i).bits.s0_pred_way_en := s0_pred_way_en(i)
    assert(PopCount(io.resp(i).bits.s0_pred_way_en) <= 1.U, "tag should not match with more than 1 way")

    /** update in s1 */
    s1_pred_way_en(i) := io.lookup_upd(i).bits.s1_pred_way_en
    s1_real_way_en(i) := io.lookup_upd(i).bits.s1_real_way_en
    s1_pred_fail(i) := io.lookup_upd(i).valid && s1_pred_way_en(i) =/= s1_real_way_en(i)
    // look up res
    wpu.io.updLookup(i).en := io.lookup_upd(i).valid
    wpu.io.updLookup(i).vaddr := io.lookup_upd(i).bits.vaddr
    wpu.io.updLookup(i).way_en := io.lookup_upd(i).bits.s1_real_way_en
    wpu.io.updLookup(i).pred_way_en := io.lookup_upd(i).bits.s1_pred_way_en
    // which will update in look up pred fail
    wpu.io.updReplaycarry := DontCare
    // replace / tag write
    wpu.io.updTagwrite := DontCare
  }
  wpu.io.updTagwrite.head.en := io.tagwrite_upd.head.valid
  wpu.io.updTagwrite.head.vaddr := io.tagwrite_upd.head.bits.vaddr
  wpu.io.updTagwrite.head.way_en := io.tagwrite_upd.head.bits.s1_real_way_en

  XSPerfAccumulate("wpu_pred_total", PopCount(io.lookup_upd.map(x => RegNext(x.valid))))
  XSPerfAccumulate("wpu_pred_succ",  PopCount(io.lookup_upd.zipWithIndex.map{case(x, i) => RegNext(x.valid) && !s1_pred_fail(i)}))
  XSPerfAccumulate("wpu_pred_fail",  PopCount(io.lookup_upd.zipWithIndex.map{case(x, i) => RegNext(x.valid) && s1_pred_fail(i)}))
  XSPerfAccumulate("wpu_pred_miss",  PopCount(io.lookup_upd.zipWithIndex.map{case(x, i) => RegNext(x.valid) && !RegNext(s0_pred_way_en(i)).orR}))
  XSPerfAccumulate("wpu_real_miss",  PopCount(io.lookup_upd.zipWithIndex.map{case(x, i) => RegNext(x.valid) && !RegNext(s1_real_way_en(i)).orR}))
}

/** IdealWPU:
  * req in s1 and resp in s1
  */
class IdealWPU(implicit p:Parameters) extends WPUModule with HasDCacheParameters {
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

  if(dwpuParam.enWPU){
    io.resp.valid := io.req.valid
  }else{
    io.resp.valid := false.B
  }
  io.resp.s1_pred_way_en := s1_pred_way_en
}