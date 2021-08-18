/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.experimental.chiselName

import scala.math.min

trait HasSCParameter extends TageParams {
  val SCHistLens = 0 :: TableInfo.map{ case (_,h,_) => h}.toList
  val SCNTables = 6
  val SCCtrBits = 6
  val SCNRows = 1024
  val SCTableInfo = Seq.fill(SCNTables)((SCNRows, SCCtrBits)) zip SCHistLens map {case ((n, cb), h) => (n, cb, h)}
}

class SCReq(implicit p: Parameters) extends TageReq

abstract class SCBundle(implicit p: Parameters) extends TageBundle with HasSCParameter {}
abstract class SCModule(implicit p: Parameters) extends TageModule with HasSCParameter {}

class SCResp(val ctrBits: Int = 6)(implicit p: Parameters) extends SCBundle {
  val ctr = Vec(2, SInt(ctrBits.W))
}

class SCUpdate(val ctrBits: Int = 6)(implicit p: Parameters) extends SCBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
  val mask = Vec(TageBanks, Bool())
  val oldCtrs = Vec(TageBanks, SInt(ctrBits.W))
  val tagePreds = Vec(TageBanks, Bool())
  val takens = Vec(TageBanks, Bool())
}

class SCTableIO(val ctrBits: Int = 6)(implicit p: Parameters) extends SCBundle {
  val req = Input(Valid(new SCReq))
  val resp = Output(Vec(TageBanks, new SCResp(ctrBits)))
  val update = Input(new SCUpdate(ctrBits))
}

@chiselName
class SCTable(val nRows: Int, val ctrBits: Int, val histLen: Int)(implicit p: Parameters)
  extends SCModule with HasFoldedHistory {
  val io = IO(new SCTableIO(ctrBits))
  
  // val table = Module(new SRAMTemplate(SInt(ctrBits.W), set=nRows, way=2*TageBanks, shouldReset=true, holdRead=true, singlePort=false))
  val table = Seq.fill(TageBanks)(Module(new SRAMTemplate(SInt(ctrBits.W), set=nRows, way=2, shouldReset=true, holdRead=true, singlePort=false)))

  val phistLen = PathHistoryLength
  def getIdx(hist: UInt, pc: UInt) = {
    (compute_folded_ghist(hist, log2Ceil(nRows)) ^ (pc >> instOffsetBits))(log2Ceil(nRows)-1,0)
  }

  def ctrUpdate(ctr: SInt, cond: Bool): SInt = signedSatUpdate(ctr, ctrBits, cond)

  val s1_idxes, s2_idxes  = Wire(Vec(TageBanks, UInt(log2Ceil(nRows).W)))

  val s1_idx = getIdx(io.req.bits.hist, io.req.bits.pc)
  val s2_idx = RegEnable(s1_idx, enable=io.req.valid)
  
  for (b <- 0 until TageBanks) {
    val idx = getIdx(io.req.bits.hist << b, io.req.bits.pc)
    s1_idxes(b) := idx

    table(b).io.r.req.valid := io.req.valid
    table(b).io.r.req.bits.setIdx := s1_idxes(b)
  }

  s2_idxes := RegEnable(s1_idxes, io.req.valid)

  val table_r =
    VecInit((0 until TageBanks).map(b => VecInit((0 until 2).map(i => table(b).io.r.resp.data(i)))))

  // val s1_mask = io.req.bits.mask // TODO: Delete it
  // val s2_mask = RegEnable(s1_mask, enable=io.req.valid)

  val update_idxes  = Wire(Vec(TageBanks, UInt(log2Ceil(nRows).W)))
  // val update_idx = getIdx(io.update.hist, io.update.pc)
  

  val update_wdatas =
    VecInit((0 until TageBanks).map(w =>
      ctrUpdate(io.update.oldCtrs(w), io.update.takens(w))))


  val updateWayMask =
    VecInit((0 until TageBanks).map(b =>
      VecInit((0 to 1).map(i =>
        (io.update.mask(b) && i.U === io.update.tagePreds(b).asUInt))))).asUInt

  for (b <- 0 until TageBanks) {
    val idx = getIdx(io.update.hist << b, io.update.pc)
    update_idxes(b) := idx
    
    table(b).io.w.apply(
      valid = io.update.mask(b),
      data = VecInit(update_wdatas(b), update_wdatas(b)),
      setIdx = update_idxes(b),
      waymask = updateWayMask(b)
    )
  }

  // table.io.w.apply(
  //   valid = io.update.mask.asUInt.orR,
  //   data = VecInit((0 until TageBanks*2).map(i => update_wdatas(i/2))),
  //   setIdx = update_idx,
  //   waymask = updateWayMask
  // )

  (0 until TageBanks).map(b => {
    io.resp(b).ctr := table_r(b)
  })

  val wrBypassEntries = 4

  class SCWrBypass extends XSModule {
    val io = IO(new Bundle {
      val wen = Input(Bool())
      val update_idx  = Input(UInt(log2Ceil(nRows).W))
      val update_ctrs  = Flipped(ValidIO(SInt(ctrBits.W)))
      val update_ctrPos = Input(UInt(log2Ceil(2).W))
      val update_altPos = Input(UInt(log2Ceil(2).W))
      
      val hit   = Output(Bool())
      val ctrs  = Vec(2, ValidIO(SInt(ctrBits.W)))
    })
    
    val idxes       = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(log2Ceil(nRows).W))))
    val ctrs        = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, Vec(2, SInt(ctrBits.W)))))
    val ctr_valids  = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, Vec(2, Bool()))))
    val enq_idx     = RegInit(0.U(log2Ceil(wrBypassEntries).W))
    
    val hits = VecInit((0 until wrBypassEntries).map { i => idxes(i) === io.update_idx })
    
    val hit = hits.reduce(_||_)
    val hit_idx = ParallelPriorityEncoder(hits)
    
    io.hit := hit
    
    for (i <- 0 until 2) {
      io.ctrs(i).valid := ctr_valids(hit_idx)(i)
      io.ctrs(i).bits := ctrs(hit_idx)(i)
    }
    
    when (io.wen) {
      when (hit) {
        ctrs(hit_idx)(io.update_ctrPos) := io.update_ctrs.bits
        ctr_valids(hit_idx)(io.update_ctrPos) := io.update_ctrs.valid
      }.otherwise {
        ctr_valids(enq_idx)(io.update_altPos) := false.B
        ctr_valids(enq_idx)(io.update_ctrPos) := io.update_ctrs.valid
        ctrs(enq_idx)(io.update_ctrPos) := io.update_ctrs.bits
      }
    }
    
    when(io.wen && !hit) {
      idxes(enq_idx) := io.update_idx
      enq_idx := (enq_idx + 1.U)(log2Ceil(wrBypassEntries)-1, 0)
    }
  }

  val wrbypass = Seq.fill(TageBanks)(Module(new SCWrBypass))

  // val wrbypass_idxs = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(log2Ceil(nRows).W))))
  // val wrbypass_ctrs = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, Vec(2*TageBanks, SInt(ctrBits.W)))))
  // val wrbypass_ctr_valids = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, Vec(2*TageBanks, Bool()))))
  // val wrbypass_enq_idx = RegInit(0.U(log2Ceil(wrBypassEntries).W))

  // val wrbypass_hits = VecInit((0 until wrBypassEntries) map (i => wrbypass_idxs(i) === update_idx))
  // val wrbypass_hit = wrbypass_hits.asUInt.orR
  // val wrbypass_hit_idx = ParallelPriorityEncoder(wrbypass_hits)

  // for (w <- 0 until TageBanks) {
  //   val ctrPos = (w << 1).U | io.update.tagePreds(w).asUInt
  //   val altPos = (w << 1).U | ~io.update.tagePreds(w).asUInt
  //   val bypass_ctr = wrbypass_ctrs(wrbypass_hit_idx)(ctrPos)
  //   val hit_and_valid = wrbypass_hit && wrbypass_ctr_valids(wrbypass_hit_idx)(ctrPos)
  //   val oldCtr = Mux(hit_and_valid, wrbypass_ctrs(wrbypass_hit_idx)(ctrPos), io.update.oldCtrs(w))
  //   update_wdatas(w) := ctrUpdate(oldCtr, io.update.takens(w))

  //   when (io.update.mask.reduce(_||_)) {
  //     when (wrbypass_hit) {
  //       when (io.update.mask(w)) {
  //         wrbypass_ctrs(wrbypass_hit_idx)(ctrPos) := update_wdatas(w)
  //         wrbypass_ctr_valids(wrbypass_hit_idx)(ctrPos) := true.B
  //       }
  //     }.otherwise {
  //       // reset valid bit first
  //       wrbypass_ctr_valids(wrbypass_enq_idx)(ctrPos) := false.B
  //       wrbypass_ctr_valids(wrbypass_enq_idx)(altPos) := false.B
  //       when (io.update.mask(w)) {
  //         wrbypass_ctr_valids(wrbypass_enq_idx)(ctrPos) := true.B
  //         wrbypass_ctrs(wrbypass_enq_idx)(w) := update_wdatas(w)
  //       }
  //     }
  //   }
  // }

  // when (io.update.mask.reduce(_||_) && !wrbypass_hit) {
  //   wrbypass_idxs(wrbypass_enq_idx) := update_idx
  //   wrbypass_enq_idx := (wrbypass_enq_idx + 1.U)(log2Ceil(wrBypassEntries)-1,0)
  // }
  
  for (b <- 0 until TageBanks) {
    val ctrPos = io.update.tagePreds(b)
    val altPos = !io.update.tagePreds(b)
    val bypass_ctr = wrbypass(b).io.ctrs(ctrPos)
    val hit_and_valid = wrbypass(b).io.hit && wrbypass(b).io.ctrs(ctrPos).valid
    val oldCtr = Mux(hit_and_valid, wrbypass(b).io.ctrs(ctrPos).bits, io.update.oldCtrs(b))
    update_wdatas(b) := ctrUpdate(oldCtr, io.update.takens(b))

    wrbypass(b).io.wen := io.update.mask(b)
    wrbypass(b).io.update_ctrs.valid := io.update.mask(b)
    wrbypass(b).io.update_ctrs.bits := update_wdatas(b)
    wrbypass(b).io.update_idx := update_idxes(b)
    wrbypass(b).io.update_ctrPos := ctrPos
    wrbypass(b).io.update_altPos := altPos

  }


  if (BPUDebug && debug) {
    val u = io.update
    XSDebug(io.req.valid,
      p"scTableReq: pc=0x${Hexadecimal(io.req.bits.pc)}, " +
      p"if2_idx=${s1_idx}, hist=${Hexadecimal(io.req.bits.hist)}\n")
    for (i <- 0 until TageBanks) {
      XSDebug(RegNext(io.req.valid),
        p"scTableResp[${i.U}]: s2_idx=${s2_idx}," +
        p"ctr:${io.resp(i).ctr}\n")
      XSDebug(io.update.mask(i),
        p"update Table: pc:${Hexadecimal(u.pc)}, hist:${Hexadecimal(u.hist << i)}, " +
        p"bank:${i}, tageTaken:${u.tagePreds(i)}, taken:${u.takens(i)}, oldCtr:${u.oldCtrs(i)}\n")
      val ctrPos = io.update.tagePreds(i)
      // val hitCtr = wrbypass_ctrs(wrbypass_hit_idx)(ctrPos)
      val hitCtr = wrbypass(i).io.ctrs(ctrPos).bits
      XSDebug(wrbypass(i).io.hit && wrbypass(i).io.ctrs(ctrPos).valid && io.update.mask(i),
        p"bank $i wrbypass hit idx:${update_idxes(i)}, ctr:$hitCtr, " +
        p"taken:${io.update.takens(i)} newCtr:${update_wdatas(i)}\n")
    }
  }

}

class SCThreshold(val ctrBits: Int = 6)(implicit p: Parameters) extends SCBundle {
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
  def apply(bits: Int)(implicit p: Parameters) = {
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
      req.valid := io.s1_fire
      req.bits.pc := s1_pc
      req.bits.hist := io.in.bits.ghist
      req.bits.phist := DontCare
      req.bits.mask := VecInit(Seq.fill(numBr)(1.U(1.W))).asUInt()
      if (!EnableSC) {t.io.update := DontCare}
      t
    }
  }

  val scThresholds = List.fill(TageBanks)(RegInit(SCThreshold(5)))
  val useThresholds = VecInit(scThresholds map (_.thres))
  val updateThresholds = VecInit(useThresholds map (t => (t << 3) +& 21.U))

  val s2_scResps = VecInit(scTables.map(t => t.io.resp))

  val scUpdateMask = WireInit(0.U.asTypeOf(Vec(SCNTables, Vec(TageBanks, Bool()))))
  val scUpdateTagePreds = Wire(Vec(TageBanks, Bool()))
  val scUpdateTakens = Wire(Vec(TageBanks, Bool()))
  val scUpdateOldCtrs = Wire(Vec(TageBanks, Vec(SCNTables, SInt(SCCtrBits.W))))
  scUpdateTagePreds := DontCare
  scUpdateTakens := DontCare
  scUpdateOldCtrs := DontCare

  val updateSCMetas = VecInit(updateMetas.map(_.scMeta))

  val s3_sc_used, s3_conf, s3_unconf, s3_agree, s3_disagree =
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
    val scMeta = resp_meta(w).scMeta
    scMeta := DontCare
    // do summation in s2
    val s2_scTableSums = VecInit(
      (0 to 1) map { i =>
        ParallelSingedExpandingAdd(s2_scResps map (r => getCentered(r(w).ctr(i)))) // TODO: rewrite with wallace tree
      }
    )

    val providerCtr = s2_providerCtrs(w)
    val s2_pvdrCtrCentered = getPvdrCentered(providerCtr)
    val s2_totalSums = VecInit(s2_scTableSums.map(_  +& s2_pvdrCtrCentered))
    val s2_sumAbs = VecInit(s2_totalSums.map(_.abs.asUInt))
    val s2_sumBelowThresholds = VecInit(s2_sumAbs map (_ <= useThresholds(w)))
    val s2_scPreds = VecInit(s2_totalSums.map (_ >= 0.S))

    val s3_sumBelowThresholds = RegEnable(s2_sumBelowThresholds, io.s2_fire)
    val s3_scPreds = RegEnable(s2_scPreds, io.s2_fire)
    val s3_sumAbs = RegEnable(s2_sumAbs, io.s2_fire)

    val s3_scCtrs = RegEnable(VecInit(s2_scResps.map(r => r(w).ctr(s2_tageTakens(w).asUInt))), io.s2_fire)
    val s3_chooseBit = s3_tageTakens(w)
    scMeta.tageTaken := s3_tageTakens(w)
    scMeta.scUsed := s3_provideds(w)
    scMeta.scPred := s3_scPreds(s3_chooseBit)
    scMeta.ctrs   := s3_scCtrs

    when (s3_provideds(w)) {
      s3_sc_used(w) := true.B
      s3_unconf(w) := s3_sumBelowThresholds(s3_chooseBit)
      s3_conf(w) := !s3_sumBelowThresholds(s3_chooseBit)
      // Use prediction from Statistical Corrector
      XSDebug(p"---------tage${w} provided so that sc used---------\n")
      XSDebug(p"scCtrs:$s3_scCtrs, prdrCtr:${s3_providerCtrs(w)}, sumAbs:$s3_sumAbs, tageTaken:${s3_chooseBit}\n")
      when (!s3_sumBelowThresholds(s3_chooseBit)) {
        // when (ctrl.sc_enable) {
        val pred = s3_scPreds(s3_chooseBit)
        val debug_pc = Cat(debug_pc_s3, w.U, 0.U(instOffsetBits.W))
        XSDebug(p"pc(${Hexadecimal(debug_pc)}) SC(${w.U}) overriden pred to ${pred}\n")
        s3_agree(w) := s3_tageTakens(w) === pred
        s3_disagree(w) := s3_tageTakens(w) =/= pred
        // io.resp.takens(w) := pred
        io.out.resp.s3.preds.taken_mask(w) := pred
      }
    }

    val updateSCMeta = updateSCMetas(w)
    val updateTageMeta = updateMetas(w)
    when (updateValids(w) && updateSCMeta.scUsed.asBool) {
      val scPred = updateSCMeta.scPred
      val tagePred = updateSCMeta.tageTaken
      val taken = update.preds.taken_mask(w)
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
        p"scSum(-$sumAbs), mispred: sc(${scPred =/= taken}), tage(${updateMisPreds(w)})\n"
        )
        XSDebug(sum >= 0.S,
        p"scUpdate: bank(${w}), scPred(${scPred}), tagePred(${tagePred}), " +
        p"scSum(+$sumAbs), mispred: sc(${scPred =/= taken}), tage(${updateMisPreds(w)})\n"
        )
        XSDebug(p"bank(${w}), update: sc: ${updateSCMeta}\n")
        update_on_mispred(w) := scPred =/= taken
        update_on_unconf(w) := scPred === taken
      }
    }
  }

  tage_perf("sc_conf", PopCount(s3_conf), PopCount(update_conf))
  tage_perf("sc_unconf", PopCount(s3_unconf), PopCount(update_unconf))
  tage_perf("sc_agree", PopCount(s3_agree), PopCount(update_agree))
  tage_perf("sc_disagree", PopCount(s3_disagree), PopCount(update_disagree))
  tage_perf("sc_used", PopCount(s3_sc_used), PopCount(update_sc_used))
  XSPerfAccumulate("sc_update_on_mispred", PopCount(update_on_mispred))
  XSPerfAccumulate("sc_update_on_unconf", PopCount(update_on_unconf))
  XSPerfAccumulate("sc_mispred_but_tage_correct", PopCount(sc_misp_tage_corr))
  XSPerfAccumulate("sc_correct_and_tage_wrong", PopCount(sc_corr_tage_misp))

  for (i <- 0 until SCNTables) {
    scTables(i).io.update.mask := RegNext(scUpdateMask(i))
    scTables(i).io.update.tagePreds := RegNext(scUpdateTagePreds)
    scTables(i).io.update.takens    := RegNext(scUpdateTakens)
    scTables(i).io.update.oldCtrs   := RegNext(VecInit(scUpdateOldCtrs.map(_(i))))
    scTables(i).io.update.pc := RegNext(update.pc)
    scTables(i).io.update.hist := RegNext(updateHist.predHist)
  }
}
