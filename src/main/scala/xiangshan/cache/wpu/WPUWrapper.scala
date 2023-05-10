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
  val tagwrite_upd = Flipped(ValidIO(new WPUUpdate(nWays)))
}

class DwpuBaseIO(nWays:Int, nPorts: Int)(implicit p:Parameters) extends BaseWPUBundle{
  val req = Vec(nPorts, Flipped(Decoupled(new WPUReplayedReq(nWays))))
  val resp = Vec(nPorts, ValidIO(new WPUResp(nWays)))
  val lookup_upd = Vec(nPorts, Flipped(ValidIO(new WPUUpdateLookup(nWays))))
  val cfpred = Vec(nPorts, new ConflictPredictIO(nWays))
}

class DwpuIO(nWays:Int, nPorts:Int)(implicit p:Parameters) extends DwpuBaseIO(nWays, nPorts){
  val tagwrite_upd = Flipped(ValidIO(new WPUUpdate(nWays)))
}

class DCacheWpuWrapper (nPorts: Int = 1) (implicit p:Parameters) extends DCacheModule with HasWPUParameters  {
  val wpu = AlgoWPUMap(dwpuParam, nPorts)
  val wayConflictPredictor = Module(new WayConflictPredictor(nPorts))
  val io = IO(new DwpuIO(nWays, nPorts))

  /** pred */
  val s0_dmSel = Wire(Vec(nPorts, Bool()))
  val s0_pred_way_conflict = Wire(Vec(nPorts, Bool()))
  val s0_pred_way_en = Wire(Vec(nPorts, UInt(nWays.W)))
  val s1_lookup_valid = Wire(Vec(nPorts, Bool()))
  val s1_dmSel = Wire(Vec(nPorts, Bool()))
  val s1_pred_way_en = Wire(Vec(nPorts, UInt(nWays.W)))
  val s1_pred_fail = Wire(Vec(nPorts, Bool()))
  val s1_hit = Wire(Vec(nPorts, Bool()))

  for(i <- 0 until nPorts){
    wayConflictPredictor.io.pred(i).en := io.req(i).valid
    wayConflictPredictor.io.pred(i).pc := io.cfpred(i).s0_pc
    s0_pred_way_conflict(i) := wayConflictPredictor.io.pred(i).way_conflict

    s0_dmSel(i) := false.B
    wpu.io.predVec(i).en := io.req(i).valid
    wpu.io.predVec(i).vaddr := io.req(i).bits.vaddr
    when(io.req(i).bits.replayCarry.valid) {
      // replay carry
      s0_pred_way_en(i) := io.req(i).bits.replayCarry.real_way_en
    }.otherwise {
      // way prediction
      s0_pred_way_en(i) := wpu.io.predVec(i).way_en

      if (dwpuParam.enCfPred) {
        // selective direct mapping
        when(!s0_pred_way_conflict(i)) {
          s0_pred_way_en(i) := UIntToOH(get_direct_map_way(io.req(i).bits.vaddr))
          s0_dmSel(i) := true.B
        }
      }
    }

    /** check and update in s1 */
    s1_lookup_valid(i) := io.lookup_upd(i).valid
    s1_dmSel(i) := RegNext(s0_dmSel(i))
    s1_pred_way_en(i) := io.lookup_upd(i).bits.s1_pred_way_en
    s1_pred_fail(i) := io.lookup_upd(i).valid && s1_pred_way_en(i) =/= io.lookup_upd(i).bits.s1_real_way_en
    s1_hit(i) := !s1_pred_fail(i) && s1_pred_way_en(i).orR

    val s0_replay_upd = Wire(new BaseWpuUpdateBundle(nWays))
    s0_replay_upd.en := io.req(i).valid && io.req(i).bits.replayCarry.valid
    s0_replay_upd.vaddr := io.req(i).bits.vaddr
    s0_replay_upd.way_en := io.req(i).bits.replayCarry.real_way_en
    val s1_replay_upd = RegNext(s0_replay_upd)

    wayConflictPredictor.io.update(i).en := io.lookup_upd(i).valid
    wayConflictPredictor.io.update(i).pc := io.cfpred(i).s1_pc
    wayConflictPredictor.io.update(i).dm_hit := s1_dmSel(i) && io.cfpred(i).s1_dm_hit
    wayConflictPredictor.io.update(i).sa_hit := !s1_dmSel(i) && s1_hit(i)

    // look up res
    wpu.io.updLookup(i).en := io.lookup_upd(i).valid
    wpu.io.updLookup(i).vaddr := io.lookup_upd(i).bits.vaddr
    wpu.io.updLookup(i).way_en := io.lookup_upd(i).bits.s1_real_way_en
    wpu.io.updLookup(i).pred_way_en := io.lookup_upd(i).bits.s1_pred_way_en

    // which will update in look up pred fail
    wpu.io.updReplaycarry(i) := s1_replay_upd

    // replace / tag write
    wpu.io.updTagwrite(i) := DontCare

    /** predict and response in s0 */
    io.req(i).ready := true.B
    if (dwpuParam.enWPU) {
      io.resp(i).valid := io.req(i).valid
    } else {
      io.resp(i).valid := false.B
    }
    io.resp(i).bits.s0_pred_way_en := s0_pred_way_en(i)
    assert(PopCount(io.resp(i).bits.s0_pred_way_en) <= 1.U, "tag should not match with more than 1 way")
  }
  wpu.io.updTagwrite(0).en := io.tagwrite_upd.valid
  wpu.io.updTagwrite(0).vaddr := io.tagwrite_upd.bits.vaddr
  wpu.io.updTagwrite(0).way_en := io.tagwrite_upd.bits.s1_real_way_en

  // PerfLog
  // pred situation
  XSPerfAccumulate("wpu_pred_total", PopCount((0 until nPorts).map(i => RegNext(io.req(i).valid) && s1_lookup_valid(i))))
  XSPerfAccumulate("wpu_pred_succ", PopCount((0 until nPorts).map(i => RegNext(io.req(i).valid) && s1_lookup_valid(i) && !s1_pred_fail(i))))
  XSPerfAccumulate("wpu_pred_fail", PopCount((0 until nPorts).map(i => RegNext(io.req(i).valid) && s1_lookup_valid(i) && s1_pred_fail(i))))
  XSPerfAccumulate("wpu_pred_miss", PopCount((0 until nPorts).map(i => RegNext(io.req(i).valid) && s1_lookup_valid(i) && !s1_pred_way_en(i).orR)))
  XSPerfAccumulate("wpu_real_miss", PopCount((0 until nPorts).map(i => RegNext(io.req(i).valid) && s1_lookup_valid(i) && !io.lookup_upd(i).bits.s1_real_way_en.orR)))
  // pred component
  XSPerfAccumulate("wpu_pred_replayCarry", PopCount((0 until nPorts).map(i => io.req(i).valid && io.req(i).bits.replayCarry.valid)))
  if(!dwpuParam.enCfPred){
    XSPerfAccumulate("wpu_pred_wayPrediction", PopCount((0 until nPorts).map(i => io.req(i).valid && !io.req(i).bits.replayCarry.valid)))
  }else{
    XSPerfAccumulate("wpu_pred_wayPrediction", PopCount((0 until nPorts).map(i => io.req(i).valid && !io.req(i).bits.replayCarry.valid && s0_pred_way_conflict(i))))
    XSPerfAccumulate("wpu_pred_directMap", PopCount((0 until nPorts).map(i => io.req(i).valid && !io.req(i).bits.replayCarry.valid && !s0_pred_way_conflict(i))))
    // dm situation
    XSPerfAccumulate("direct_map_all", PopCount((0 until nPorts).map(i => io.lookup_upd(i).valid)))
    XSPerfAccumulate("direct_map_ok", PopCount((0 until nPorts).map(i => io.lookup_upd(i).valid && io.cfpred(i).s1_dm_hit)))
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
  // wpu.io.updTagwrite.head.en := io.tagwrite_upd.valid
  // wpu.io.updTagwrite.head.vaddr := io.tagwrite_upd.bits.vaddr
  // wpu.io.updTagwrite.head.way_en := io.tagwrite_upd.bits.s1_real_way_en

  XSPerfAccumulate("wpu_pred_total", PopCount((0 until nPorts).map{i => RegNext(io.req(i).valid) && io.lookup_upd(i).valid}))
  XSPerfAccumulate("wpu_pred_succ",  PopCount((0 until nPorts).map{i => RegNext(io.req(i).valid) && io.lookup_upd(i).valid && !s1_pred_fail(i)}))
  XSPerfAccumulate("wpu_pred_fail",  PopCount((0 until nPorts).map{i => RegNext(io.req(i).valid) && io.lookup_upd(i).valid && s1_pred_fail(i)}))
  XSPerfAccumulate("wpu_pred_miss",  PopCount((0 until nPorts).map{i => RegNext(io.req(i).valid) && io.lookup_upd(i).valid && !RegNext(s0_pred_way_en(i)).orR}))
  XSPerfAccumulate("wpu_real_miss",  PopCount((0 until nPorts).map{i => RegNext(io.req(i).valid) && io.lookup_upd(i).valid && !RegNext(s1_real_way_en(i)).orR}))
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