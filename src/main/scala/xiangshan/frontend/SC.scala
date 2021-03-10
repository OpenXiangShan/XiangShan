package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.experimental.chiselName

import scala.math.min

trait HasSCParameter extends HasTageParameter {
  val SCHistLens = 0 :: TableInfo.map{ case (_,h,_) => h}.toList
  val SCNTables = 6
  val SCCtrBits = 6
  val SCNRows = 1024
  val SCTableInfo = Seq.fill(SCNTables)((SCNRows, SCCtrBits)) zip SCHistLens map {case ((n, cb), h) => (n, cb, h)}
}

class SCReq extends TageReq

abstract class SCBundle extends TageBundle with HasSCParameter {}
abstract class SCModule extends TageModule with HasSCParameter {}

class SCResp(val ctrBits: Int = 6) extends SCBundle {
  val ctr = Vec(2, SInt(ctrBits.W))
}

class SCUpdate(val ctrBits: Int = 6) extends SCBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
  val mask = Vec(TageBanks, Bool())
  val oldCtrs = Vec(TageBanks, SInt(ctrBits.W))
  val tagePreds = Vec(TageBanks, Bool())
  val takens = Vec(TageBanks, Bool())
}

class SCTableIO(val ctrBits: Int = 6) extends SCBundle {
  val req = Input(Valid(new SCReq))
  val resp = Output(Vec(TageBanks, new SCResp(ctrBits)))
  val update = Input(new SCUpdate(ctrBits))
}

@chiselName
class SCTable(val nRows: Int, val ctrBits: Int, val histLen: Int)
  extends SCModule with HasFoldedHistory {
  val io = IO(new SCTableIO(ctrBits))

  val table = Module(new SRAMTemplate(SInt(ctrBits.W), set=nRows, way=2*TageBanks, shouldReset=true, holdRead=true, singlePort=false))

  def getIdx(hist: UInt, pc: UInt) = {
    (compute_folded_hist(hist, log2Ceil(nRows)) ^ (pc >> (instOffsetBits+log2Ceil(TageBanks))))(log2Ceil(nRows)-1,0)
  }

  def ctrUpdate(ctr: SInt, cond: Bool): SInt = signedSatUpdate(ctr, ctrBits, cond)

  val if2_idx = getIdx(io.req.bits.hist, io.req.bits.pc)
  val if3_idx = RegEnable(if2_idx, enable=io.req.valid)

  val table_r = 
    VecInit((0 until TageBanks).map(b => VecInit((0 to 1).map(i => table.io.r.resp.data(b*2+i)))))


  val if2_mask = io.req.bits.mask
  val if3_mask = RegEnable(if2_mask, enable=io.req.valid)

  val update_idx = getIdx(io.update.hist, io.update.pc)
  val update_wdatas =
    VecInit((0 until TageBanks).map(w =>
      ctrUpdate(io.update.oldCtrs(w), io.update.takens(w))))

  table.io.r.req.valid := io.req.valid
  table.io.r.req.bits.setIdx := if2_idx
                        
  val updateWayMask = 
    VecInit((0 until TageBanks).map(b =>
      VecInit((0 to 1).map(i =>
        (io.update.mask(b) && i.U === io.update.tagePreds(b).asUInt))))).asUInt

  table.io.w.apply(
    valid = io.update.mask.asUInt.orR,
    data = VecInit((0 until TageBanks*2).map(i => update_wdatas(i/2))),
    setIdx = update_idx,
    waymask = updateWayMask
  )

  (0 until TageBanks).map(b => {
    io.resp(b).ctr := table_r(b)
  })

  val wrBypassEntries = 4
  
  val wrbypass_idxs = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(log2Ceil(nRows).W))))
  val wrbypass_ctrs = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, Vec(2*TageBanks, SInt(ctrBits.W)))))
  val wrbypass_ctr_valids = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, Vec(2*TageBanks, Bool()))))
  val wrbypass_enq_idx = RegInit(0.U(log2Ceil(wrBypassEntries).W))

  when (reset.asBool) {
    wrbypass_ctr_valids := 0.U.asTypeOf(Vec(wrBypassEntries, Vec(2*TageBanks, Bool())))
  }

  val wrbypass_hits = VecInit((0 until wrBypassEntries) map (i => wrbypass_idxs(i) === update_idx))
  val wrbypass_hit = wrbypass_hits.asUInt.orR
  val wrbypass_hit_idx = ParallelPriorityEncoder(wrbypass_hits)

  for (w <- 0 until TageBanks) {
    val ctrPos = (w << 1).U | io.update.tagePreds(w).asUInt
    val altPos = (w << 1).U | ~io.update.tagePreds(w).asUInt
    val bypass_ctr = wrbypass_ctrs(wrbypass_hit_idx)(ctrPos)
    val hit_and_valid = wrbypass_hit && wrbypass_ctr_valids(wrbypass_hit_idx)(ctrPos)
    val oldCtr = Mux(hit_and_valid, wrbypass_ctrs(wrbypass_hit_idx)(ctrPos), io.update.oldCtrs(w))
    update_wdatas(w) := ctrUpdate(oldCtr, io.update.takens(w))

    when (io.update.mask.reduce(_||_)) {
      when (wrbypass_hit) {
        when (io.update.mask(w)) {
          wrbypass_ctrs(wrbypass_hit_idx)(ctrPos) := update_wdatas(w)
          wrbypass_ctr_valids(wrbypass_hit_idx)(ctrPos) := true.B
        }
      }.otherwise {
        // reset valid bit first
        wrbypass_ctr_valids(wrbypass_enq_idx)(ctrPos) := false.B
        wrbypass_ctr_valids(wrbypass_enq_idx)(altPos) := false.B
        when (io.update.mask(w)) {
          wrbypass_ctr_valids(wrbypass_enq_idx)(ctrPos) := true.B
          wrbypass_ctrs(wrbypass_enq_idx)(w) := update_wdatas(w)
        }
      }
    }
  }
  
  when (io.update.mask.reduce(_||_) && !wrbypass_hit) {
    wrbypass_idxs(wrbypass_enq_idx) := update_idx
    wrbypass_enq_idx := (wrbypass_enq_idx + 1.U)(log2Ceil(wrBypassEntries)-1,0)
  }


  if (BPUDebug && debug) {
    val u = io.update
    XSDebug(io.req.valid,
      p"scTableReq: pc=0x${Hexadecimal(io.req.bits.pc)}, " +
      p"if2_idx=${if2_idx}, hist=${Hexadecimal(io.req.bits.hist)}, " +
      p"if2_mask=${Binary(if2_mask)}\n")
    for (i <- 0 until TageBanks) {
      XSDebug(RegNext(io.req.valid), 
        p"scTableResp[${i.U}]: if3_idx=${if3_idx}," + 
        p"ctr:${io.resp(i).ctr}, if3_mask=${Binary(if3_mask)}\n")
      XSDebug(io.update.mask(i),
        p"update Table: pc:${Hexadecimal(u.pc)}, hist:${Hexadecimal(u.hist)}, " +
        p"bank:${i}, tageTaken:${u.tagePreds(i)}, taken:${u.takens(i)}, oldCtr:${u.oldCtrs(i)}\n")
      val ctrPos = (i << 1).U | io.update.tagePreds(i).asUInt
      val hitCtr = wrbypass_ctrs(wrbypass_hit_idx)(ctrPos)
      XSDebug(wrbypass_hit && wrbypass_ctr_valids(wrbypass_hit_idx)(ctrPos) && io.update.mask(i),
        p"bank $i wrbypass hit wridx:$wrbypass_hit_idx, idx:$update_idx, ctr:$hitCtr" +
        p"taken:${io.update.takens(i)} newCtr:${update_wdatas(i)}\n")
    }
  }

}

class SCThreshold(val ctrBits: Int = 6) extends SCBundle {
  val ctr = UInt(ctrBits.W)
  def satPos(ctr: UInt = this.ctr) = ctr === ((1.U << ctrBits) - 1.U)
  def satNeg(ctr: UInt = this.ctr) = ctr === 0.U
  def neutralVal = (1.U << (ctrBits - 1))
  val thres = UInt(8.W)
  def initVal = 6.U
  def minThres = 6.U
  def maxThres = 31.U
  def update(cause: Bool): SCThreshold = {
    val res = Wire(new SCThreshold(this.ctrBits))
    val newCtr = satUpdate(this.ctr, this.ctrBits, cause)
    val newThres = Mux(res.satPos(newCtr) && this.thres <= maxThres, this.thres + 2.U,
                      Mux(res.satNeg(newCtr) && this.thres >= minThres, this.thres - 2.U,
                      this.thres))
    res.thres := newThres
    res.ctr := Mux(res.satPos(newCtr) || res.satNeg(newCtr), res.neutralVal, newCtr)
    // XSDebug(true.B, p"scThres Update: cause${cause} newCtr ${newCtr} newThres ${newThres}\n")
    res
  }
}

object SCThreshold {
  def apply(bits: Int) = {
    val t = Wire(new SCThreshold(ctrBits=bits))
    t.ctr := t.neutralVal
    t.thres := t.initVal
    t
  }
}


trait HasSC extends HasSCParameter { this: Tage =>
  val scTables = SCTableInfo.map {
    case (nRows, ctrBits, histLen) => {
      val t = Module(new SCTable(nRows/TageBanks, ctrBits, histLen))
      val req = t.io.req
      req.valid := io.pc.valid
      req.bits.pc := io.pc.bits
      req.bits.hist := io.hist
      req.bits.mask := io.inMask
      if (!EnableSC) {t.io.update := DontCare}
      t
    }
  }
  
  val scThresholds = List.fill(TageBanks)(RegInit(SCThreshold(5)))
  val useThresholds = VecInit(scThresholds map (_.thres))
  val updateThresholds = VecInit(useThresholds map (t => (t << 3) +& 21.U))
  
  val if3_scResps = VecInit(scTables.map(t => t.io.resp))

  val scUpdateMask = WireInit(0.U.asTypeOf(Vec(SCNTables, Vec(TageBanks, Bool()))))
  val scUpdateTagePreds = Wire(Vec(TageBanks, Bool()))
  val scUpdateTakens = Wire(Vec(TageBanks, Bool()))
  val scUpdateOldCtrs = Wire(Vec(TageBanks, Vec(SCNTables, SInt(SCCtrBits.W))))
  scUpdateTagePreds := DontCare
  scUpdateTakens := DontCare
  scUpdateOldCtrs := DontCare

  val updateSCMetas = VecInit(u.metas.map(_.tageMeta.scMeta))
  
  val if4_sc_used, if4_conf, if4_unconf, if4_agree, if4_disagree =
    0.U.asTypeOf(Vec(TageBanks, Bool()))
  val update_sc_used, update_conf, update_unconf, update_agree, update_disagree =
    0.U.asTypeOf(Vec(TageBanks, Bool()))
  val update_on_mispred, update_on_unconf, sc_misp_tage_corr, sc_corr_tage_misp =
    0.U.asTypeOf(Vec(TageBanks, Bool()))
  
  // for sc ctrs
  def getCentered(ctr: SInt): SInt = (ctr << 1).asSInt + 1.S
  // for tage ctrs
  def getPvdrCentered(ctr: UInt): SInt = ((((ctr.zext -& 4.S) << 1).asSInt + 1.S) << 3).asSInt

  for (w <- 0 until TageBanks) {
    val scMeta = io.meta(w).scMeta
    scMeta := DontCare
    // do summation in if3
    val if3_scTableSums = VecInit(
      (0 to 1) map { i => 
        ParallelSingedExpandingAdd(if3_scResps map (r => getCentered(r(w).ctr(i)))) // TODO: rewrite with wallace tree
      }
    )

    val providerCtr = if3_providerCtrs(w)
    val if3_pvdrCtrCentered = getPvdrCentered(providerCtr)
    val if3_totalSums = VecInit(if3_scTableSums.map(_  +& if3_pvdrCtrCentered))
    val if3_sumAbs = VecInit(if3_totalSums.map(_.abs.asUInt))
    val if3_sumBelowThresholds = VecInit(if3_sumAbs map (_ <= useThresholds(w)))
    val if3_scPreds = VecInit(if3_totalSums.map (_ >= 0.S))

    val if4_sumBelowThresholds = RegEnable(if3_sumBelowThresholds, s3_fire)
    val if4_scPreds = RegEnable(if3_scPreds, s3_fire)
    val if4_sumAbs = RegEnable(if3_sumAbs, s3_fire)

    val if4_scCtrs = RegEnable(VecInit(if3_scResps.map(r => r(w).ctr(if3_tageTakens(w).asUInt))), s3_fire)
    val if4_chooseBit = if4_tageTakens(w)
    scMeta.tageTaken := if4_tageTakens(w)
    scMeta.scUsed := if4_provideds(w)
    scMeta.scPred := if4_scPreds(if4_chooseBit)
    scMeta.ctrs   := if4_scCtrs

    when (if4_provideds(w)) {
      if4_sc_used(w) := true.B
      if4_unconf(w) := if4_sumBelowThresholds(if4_chooseBit)
      if4_conf(w) := !if4_sumBelowThresholds(if4_chooseBit)
      // Use prediction from Statistical Corrector
      XSDebug(p"---------tage${w} provided so that sc used---------\n")
      XSDebug(p"scCtrs:$if4_scCtrs, prdrCtr:${if4_providerCtrs(w)}, sumAbs:$if4_sumAbs, tageTaken:${if4_chooseBit}\n")
      when (!if4_sumBelowThresholds(if4_chooseBit)) {
        when (ctrl.sc_enable) {
          val pred = if4_scPreds(if4_chooseBit)
          val debug_pc = Cat(packetIdx(debug_pc_s3), w.U, 0.U(instOffsetBits.W))
          XSDebug(p"pc(${Hexadecimal(debug_pc)}) SC(${w.U}) overriden pred to ${pred}\n")
          if4_agree(w) := if4_tageTakens(w) === pred
          if4_disagree(w) := if4_tageTakens(w) =/= pred
          io.resp.takens(w) := pred
        }
      }
    }

    val updateSCMeta = updateSCMetas(w)
    val updateTageMeta = updateMetas(w)
    when (updateValids(w) && updateSCMeta.scUsed.asBool) {
      val scPred = updateSCMeta.scPred
      val tagePred = updateSCMeta.tageTaken
      val taken = u.takens(w)
      val scOldCtrs = updateSCMeta.ctrs
      val pvdrCtr = updateTageMeta.providerCtr
      val sum = ParallelSingedExpandingAdd(scOldCtrs.map(getCentered)) +& getPvdrCentered(pvdrCtr)
      val sumAbs = sum.abs.asUInt
      scUpdateTagePreds(w) := tagePred
      scUpdateTakens(w) := taken
      (scUpdateOldCtrs(w) zip scOldCtrs).foreach{case (t, c) => t := c}

      update_sc_used(w) := true.B
      update_unconf(w) := sumAbs < useThresholds(w)
      update_conf(w) := sumAbs >= useThresholds(w)
      update_agree(w) := scPred === tagePred
      update_disagree(w) := scPred =/= tagePred
      sc_corr_tage_misp(w) := scPred === taken && tagePred =/= taken && update_conf(w)
      sc_misp_tage_corr(w) := scPred =/= taken && tagePred === taken && update_conf(w)

      val thres = useThresholds(w)
      when (scPred =/= tagePred && sumAbs >= thres - 4.U && sumAbs <= thres - 2.U) {
        val newThres = scThresholds(w).update(scPred =/= taken)
        scThresholds(w) := newThres
        XSDebug(p"scThres $w update: old ${useThresholds(w)} --> new ${newThres.thres}\n")
      }

      val updateThres = updateThresholds(w)
      when (scPred =/= taken || sumAbs < updateThres) {
        scUpdateMask.foreach(t => t(w) := true.B)
        XSDebug(sum < 0.S,
        p"scUpdate: bank(${w}), scPred(${scPred}), tagePred(${tagePred}), " +
        p"scSum(-$sumAbs), mispred: sc(${scPred =/= taken}), tage(${updateTageMisPreds(w)})\n"
        )
        XSDebug(sum >= 0.S,
        p"scUpdate: bank(${w}), scPred(${scPred}), tagePred(${tagePred}), " +
        p"scSum(+$sumAbs), mispred: sc(${scPred =/= taken}), tage(${updateTageMisPreds(w)})\n"
        )
        XSDebug(p"bank(${w}), update: sc: ${updateSCMeta}\n")
        update_on_mispred(w) := scPred =/= taken
        update_on_unconf(w) := scPred === taken
      }
    }
  }

  tage_perf("sc_conf", PopCount(if4_conf), PopCount(update_conf))
  tage_perf("sc_unconf", PopCount(if4_unconf), PopCount(update_unconf))
  tage_perf("sc_agree", PopCount(if4_agree), PopCount(update_agree))
  tage_perf("sc_disagree", PopCount(if4_disagree), PopCount(update_disagree))
  tage_perf("sc_used", PopCount(if4_sc_used), PopCount(update_sc_used))
  XSPerf("sc_update_on_mispred", PopCount(update_on_mispred))
  XSPerf("sc_update_on_unconf", PopCount(update_on_unconf))
  XSPerf("sc_mispred_but_tage_correct", PopCount(sc_misp_tage_corr))
  XSPerf("sc_correct_and_tage_wrong", PopCount(sc_corr_tage_misp))

  for (i <- 0 until SCNTables) {
    scTables(i).io.update.mask := RegNext(scUpdateMask(i))
    scTables(i).io.update.tagePreds := RegNext(scUpdateTagePreds)
    scTables(i).io.update.takens    := RegNext(scUpdateTakens)
    scTables(i).io.update.oldCtrs   := RegNext(VecInit(scUpdateOldCtrs.map(_(i))))
    scTables(i).io.update.pc := RegNext(u.ftqPC)
    scTables(i).io.update.hist := RegNext(updateHist)
  }
}