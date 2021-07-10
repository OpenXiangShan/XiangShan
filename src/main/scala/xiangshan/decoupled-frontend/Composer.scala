package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class Composer(implicit p: Parameters) extends BasePredictor with HasBPUConst {
  // TODO: Add into parameter system
  // val loop = Module(new LoopPredictor)
  // val tage = (if(EnableBPD) { if (EnableSC) Module(new Tage_SC)
  //                             else          Module(new Tage) }
  //             else          { Module(new FakeTage) })
  val ftb = Module(new FTB()(p))
  val ubtb = Module(new MicroBTB)
  val bim = Module(new BIM()(p))
  // val fake = Module(new FakePredictor())

  // val preds = Seq(loop, tage, btb, ubtb, bim)
  val preds = Seq(ftb, ubtb, bim)
  preds.map(_.io := DontCare)

  // ubtb.io.resp_in(0)  := resp_in
  // bim.io.resp_in(0)   := ubtb.io.resp
  // btb.io.resp_in(0)   := bim.io.resp
  // tage.io.resp_in(0)  := btb.io.resp
  // loop.io.resp_in(0)  := tage.io.resp
  ubtb.io.resp_in(0)     := io.resp_in(0)
  bim.io.resp_in(0)      := ubtb.io.resp.bits
  ftb.io.resp_in(0)      := bim.io.resp.bits

  val (components, resp) = (preds, ftb.io.resp)
  io.resp := resp

  var metas = 0.U(1.W)
  var meta_sz = 0
  var spec_metas = 0.U(1.W)
  var spec_meta_sz = 0
  for (c <- components) {
    // c.io.f0_valid := io.f0_valid
    // c.io.f0_pc := io.f0_pc
    c.io.ghist  := io.ghist
    c.io.toFtq_fire   := io.toFtq_fire
    if (c.meta_size > 0) {
      metas = (metas << c.meta_size) | c.io.meta(c.meta_size-1,0)
    }
    meta_sz = meta_sz + c.meta_size

    if (c.spec_meta_size > 0) {
      spec_metas = (spec_metas << c.spec_meta_size) | c.io.spec_meta(c.spec_meta_size-1,0)
    }
    spec_meta_sz = spec_meta_sz + c.spec_meta_size
  }

  io.in_ready := components.map(_.io.in_ready).reduce(_&&_) // TODO: Add valid and ready logic

  ubtb.io.flush := ftb.io.flush_out.valid

  ubtb.io.f0_pc.valid  := io.f0_pc.valid && ubtb.io.resp.valid
  ubtb.io.f0_pc     := Mux(ftb.io.flush_out.valid, ubtb.io.resp.bits.f1.preds.pred_target, ftb.io.flush_out.bits)

  bim.io.f0_pc.valid := io.f0_pc.valid && ftb.io.resp.valid
  bim.io.f0_pc := ftb.io.resp.bits.f2.preds.pred_target

  ftb.io.f0_pc.valid := io.f0_pc.valid && ftb.io.resp.valid
  ftb.io.f0_pc := ftb.io.resp.bits.f2.preds.pred_target

  val f1_resp_valid = bim.io.resp.valid
  val f1_resp = bim.io.resp.bits.f1

  val f2_resp_valid = ftb.io.resp.valid
  val f2_resp = ftb.io.resp.bits.f2

  when(f2_resp_valid) {
    when(f2_resp.preds.taken =/= ftb.io.resp.bits.f1.preds.taken ||
      f2_resp.preds.pred_target =/= ftb.io.resp.bits.f1.preds.pred_target) {
      ubtb.io.flush := true.B
      ubtb.io.f0_pc := f2_resp.preds.pred_target

      bim.io.flush := true.B
      bim.io.f0_pc := f2_resp.preds.pred_target
    }
  }

  // ftb.io.f0_pc     := ubtb.io.resp.bits.f2.preds.pred_target


  require(meta_sz < MaxMetaLength && spec_meta_sz < MaxMetaLength)
  io.meta := metas
  io.spec_meta := spec_metas


  var update_meta = io.update.bits.meta
  var update_spec_meta = io.update.bits.spec_meta
  for (c <- components.reverse) {
    c.io.update := io.update
    c.io.update.bits.meta := update_meta
    c.io.update.bits.spec_meta := update_spec_meta
    update_meta = update_meta >> c.meta_size
    update_spec_meta = update_spec_meta >> c.spec_meta_size
  }

  components.map(_.io.flush := io.flush)
}