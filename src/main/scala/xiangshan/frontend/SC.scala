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
}

class SCReq(implicit p: Parameters) extends TageReq

abstract class SCBundle(implicit p: Parameters) extends TageBundle with HasSCParameter {}
abstract class SCModule(implicit p: Parameters) extends TageModule with HasSCParameter {}


class SCMeta(val useSC: Boolean, val ntables: Int)(implicit p: Parameters) extends XSBundle with HasSCParameter {
  val tageTaken = if (useSC) Bool() else UInt(0.W)
  val scUsed = if (useSC) Bool() else UInt(0.W)
  val scPred = if (useSC) Bool() else UInt(0.W)
  // Suppose ctrbits of all tables are identical
  val ctrs = if (useSC) Vec(ntables, SInt(SCCtrBits.W)) else Vec(ntables, SInt(0.W))
}


class SCResp(val ctrBits: Int = 6)(implicit p: Parameters) extends SCBundle {
  val ctr = Vec(2, SInt(ctrBits.W))
}

class SCUpdate(val ctrBits: Int = 6)(implicit p: Parameters) extends SCBundle {
  val pc = UInt(VAddrBits.W)
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
  val mask = Bool()
  val oldCtr = SInt(ctrBits.W)
  val tagePred = Bool()
  val taken = Bool()
}

class SCTableIO(val ctrBits: Int = 6)(implicit p: Parameters) extends SCBundle {
  val req = Input(Valid(new SCReq))
  val resp = Output(new SCResp(ctrBits))
  val update = Input(new SCUpdate(ctrBits))
}

@chiselName
class SCTable(val nRows: Int, val ctrBits: Int, val histLen: Int)(implicit p: Parameters)
  extends SCModule with HasFoldedHistory {
  val io = IO(new SCTableIO(ctrBits))

  // val table = Module(new SRAMTemplate(SInt(ctrBits.W), set=nRows, way=2*TageBanks, shouldReset=true, holdRead=true, singlePort=false))
  val table = Module(new SRAMTemplate(SInt(ctrBits.W), set=nRows, way=2, shouldReset=true, holdRead=true, singlePort=false))

  val phistLen = PathHistoryLength
  // def getIdx(hist: UInt, pc: UInt) = {
  //   (compute_folded_ghist(hist, log2Ceil(nRows)) ^ (pc >> instOffsetBits))(log2Ceil(nRows)-1,0)
  // }


  val idxFhInfo = (histLen, min(log2Ceil(nRows), histLen))

  def getFoldedHistoryInfo = Set(idxFhInfo).filter(_._1 > 0)

  def getIdx(pc: UInt, allFh: AllFoldedHistories) = {
    if (histLen > 0) {
      val idx_fh = allFh.getHistWithInfo(idxFhInfo).folded_hist
      // require(idx_fh.getWidth == log2Ceil(nRows))
      ((pc >> instOffsetBits) ^ idx_fh)(log2Ceil(nRows)-1,0)
    }
    else {
      pc(log2Ceil(nRows)-1,0)
    }
  }

  def ctrUpdate(ctr: SInt, cond: Bool): SInt = signedSatUpdate(ctr, ctrBits, cond)

  val s0_idx = getIdx(io.req.bits.pc, io.req.bits.folded_hist)
  val s1_idx = RegEnable(s0_idx, enable=io.req.valid)

  table.io.r.req.valid := io.req.valid
  table.io.r.req.bits.setIdx := s0_idx

  io.resp.ctr := table.io.r.resp.data

  val update_wdata = Wire(SInt(ctrBits.W))
  val updateWayMask =
      VecInit((0 to 1).map(io.update.mask && _.U === io.update.tagePred.asUInt)).asUInt

  val update_idx = getIdx(io.update.pc, io.update.folded_hist)

  table.io.w.apply(
    valid = io.update.mask,
    data = VecInit(Seq.fill(2)(update_wdata)),
    setIdx = update_idx,
    waymask = updateWayMask
  )

  val wrBypassEntries = 4

  val wrbypass = Module(new WrBypass(SInt(ctrBits.W), wrBypassEntries, log2Ceil(nRows), numWays=2))

  val ctrPos = io.update.tagePred
  val altPos = !io.update.tagePred
  val bypass_ctr = wrbypass.io.hit_data(ctrPos)
  val hit_and_valid = wrbypass.io.hit && bypass_ctr.valid
  val oldCtr = Mux(hit_and_valid, bypass_ctr.bits, io.update.oldCtr)
  update_wdata := ctrUpdate(oldCtr, io.update.taken)

  wrbypass.io.wen := io.update.mask
  wrbypass.io.write_data.map(_ := update_wdata) // only one of them are used
  wrbypass.io.write_idx := update_idx
  wrbypass.io.write_way_mask.map(_ := UIntToOH(ctrPos).asTypeOf(Vec(2, Bool())))

  val u = io.update
  XSDebug(io.req.valid,
    p"scTableReq: pc=0x${Hexadecimal(io.req.bits.pc)}, " +
    p"s0_idx=${s0_idx}\n")
  XSDebug(RegNext(io.req.valid),
    p"scTableResp: s1_idx=${s1_idx}," +
    p"ctr:${io.resp.ctr}\n")
  XSDebug(io.update.mask,
    p"update Table: pc:${Hexadecimal(u.pc)}, " +
    p"tageTaken:${u.tagePred}, taken:${u.taken}, oldCtr:${u.oldCtr}\n")
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


trait HasSC extends HasSCParameter with HasPerfEvents { this: Tage =>
  val update_on_mispred, update_on_unconf = WireInit(0.U.asTypeOf(Vec(TageBanks, Bool())))
  var sc_fh_info = Set[FoldedHistoryInfo]()
  if (EnableSC) {
    val bank_scTables = BankSCTableInfos.zipWithIndex.map {
      case (info, b) =>
        val tables = info.map {
          case (nRows, ctrBits, histLen) => {
            val t = Module(new SCTable(nRows/TageBanks, ctrBits, histLen))
            val req = t.io.req
            req.valid := io.s0_fire
            req.bits.pc := s0_pc
            req.bits.folded_hist := io.in.bits.folded_hist
            req.bits.phist := DontCare
            if (!EnableSC) {t.io.update := DontCare}
            t
          }
        }
        tables
    }
    sc_fh_info = bank_scTables.flatMap(_.map(_.getFoldedHistoryInfo).reduce(_++_)).toSet
  
    val scThresholds = List.fill(TageBanks)(RegInit(SCThreshold(5)))
    val useThresholds = VecInit(scThresholds map (_.thres))
    val updateThresholds = VecInit(useThresholds map (t => (t << 3) +& 21.U))
  
    val s1_scResps = MixedVecInit(bank_scTables.map(b => VecInit(b.map(t => t.io.resp))))
  
    val scUpdateMask = WireInit(0.U.asTypeOf(MixedVec(BankSCNTables.map(Vec(_, Bool())))))
    val scUpdateTagePreds = Wire(Vec(TageBanks, Bool()))
    val scUpdateTakens = Wire(Vec(TageBanks, Bool()))
    val scUpdateOldCtrs = Wire(MixedVec(BankSCNTables.map(Vec(_, SInt(SCCtrBits.W)))))
    scUpdateTagePreds := DontCare
    scUpdateTakens := DontCare
    scUpdateOldCtrs := DontCare
  
    val updateSCMetas = VecInit(updateMetas.map(_.scMeta))
  
    val s2_sc_used, s2_conf, s2_unconf, s2_agree, s2_disagree =
      0.U.asTypeOf(Vec(TageBanks, Bool()))
    val update_sc_used, update_conf, update_unconf, update_agree, update_disagree =
      0.U.asTypeOf(Vec(TageBanks, Bool()))
    val sc_misp_tage_corr, sc_corr_tage_misp =
      0.U.asTypeOf(Vec(TageBanks, Bool()))
  
    // for sc ctrs
    def getCentered(ctr: SInt): SInt = (ctr << 1).asSInt + 1.S
    // for tage ctrs
    def getPvdrCentered(ctr: UInt): SInt = ((((ctr.zext -& 4.S) << 1).asSInt + 1.S) << 3).asSInt
  
    for (w <- 0 until TageBanks) {
      val scMeta = resp_meta(w).scMeta
      scMeta := DontCare
      // do summation in s2
      val s1_scTableSums = VecInit(
        (0 to 1) map { i =>
          ParallelSingedExpandingAdd(s1_scResps(w) map (r => getCentered(r.ctr(i)))) // TODO: rewrite with wallace tree
        }
      )
  
      val providerCtr = s1_providerCtrs(w)
      val s1_pvdrCtrCentered = getPvdrCentered(providerCtr)
      val s1_totalSums = VecInit(s1_scTableSums.map(_  +& s1_pvdrCtrCentered))
      val s1_sumAbs = VecInit(s1_totalSums.map(_.abs.asUInt))
      val s1_sumBelowThresholds = VecInit(s1_sumAbs map (_ <= useThresholds(w)))
      val s1_scPreds = VecInit(s1_totalSums.map (_ >= 0.S))
  
      val s2_sumBelowThresholds = RegEnable(s1_sumBelowThresholds, io.s1_fire)
      val s2_scPreds = RegEnable(s1_scPreds, io.s1_fire)
      val s2_sumAbs = RegEnable(s1_sumAbs, io.s1_fire)
  
      val s2_scCtrs = RegEnable(VecInit(s1_scResps(w).map(r => r.ctr(s1_tageTakens(w).asUInt))), io.s1_fire)
      val s2_chooseBit = s2_tageTakens(w)
      scMeta.tageTaken := s2_tageTakens(w)
      scMeta.scUsed := s2_provideds(w)
      scMeta.scPred := s2_scPreds(s2_chooseBit)
      scMeta.ctrs   := s2_scCtrs
  
      when (s2_provideds(w)) {
        s2_sc_used(w) := true.B
        s2_unconf(w) := s2_sumBelowThresholds(s2_chooseBit)
        s2_conf(w) := !s2_sumBelowThresholds(s2_chooseBit)
        // Use prediction from Statistical Corrector
        XSDebug(p"---------tage_bank_${w} provided so that sc used---------\n")
        XSDebug(p"scCtrs:$s2_scCtrs, prdrCtr:${s2_providerCtrs(w)}, sumAbs:$s2_sumAbs, tageTaken:${s2_chooseBit}\n")
        when (!s2_sumBelowThresholds(s2_chooseBit)) {
          val pred = s2_scPreds(s2_chooseBit)
          val debug_pc = Cat(debug_pc_s2, w.U, 0.U(instOffsetBits.W))
          s2_agree(w) := s2_tageTakens(w) === pred
          s2_disagree(w) := s2_tageTakens(w) =/= pred
          // fit to always-taken condition
          io.out.resp.s2.preds.br_taken_mask(w) := pred
          XSDebug(p"pc(${Hexadecimal(debug_pc)}) SC(${w.U}) overriden pred to ${pred}\n")
        }
      }
  
      val updateSCMeta = updateSCMetas(w)
      val updateTageMeta = updateMetas(w)
      when (updateValids(w) && updateSCMeta.scUsed.asBool) {
        val scPred = updateSCMeta.scPred
        val tagePred = updateSCMeta.tageTaken
        val taken = update.preds.br_taken_mask(w)
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
          scUpdateMask(w).foreach(_ := true.B)
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
  
    
    for (b <- 0 until TageBanks) {
      for (i <- 0 until BankSCNTables(b)) {
        bank_scTables(b)(i).io.update.mask := RegNext(scUpdateMask(b)(i))
        bank_scTables(b)(i).io.update.tagePred := RegNext(scUpdateTagePreds(b))
        bank_scTables(b)(i).io.update.taken    := RegNext(scUpdateTakens(b))
        bank_scTables(b)(i).io.update.oldCtr   := RegNext(scUpdateOldCtrs(b)(i))
        bank_scTables(b)(i).io.update.pc := RegNext(update.pc)
        bank_scTables(b)(i).io.update.folded_hist := RegNext(updateFHist)
      }
    }
    
    tage_perf("sc_conf", PopCount(s2_conf), PopCount(update_conf))
    tage_perf("sc_unconf", PopCount(s2_unconf), PopCount(update_unconf))
    tage_perf("sc_agree", PopCount(s2_agree), PopCount(update_agree))
    tage_perf("sc_disagree", PopCount(s2_disagree), PopCount(update_disagree))
    tage_perf("sc_used", PopCount(s2_sc_used), PopCount(update_sc_used))
    XSPerfAccumulate("sc_update_on_mispred", PopCount(update_on_mispred))
    XSPerfAccumulate("sc_update_on_unconf", PopCount(update_on_unconf))
    XSPerfAccumulate("sc_mispred_but_tage_correct", PopCount(sc_misp_tage_corr))
    XSPerfAccumulate("sc_correct_and_tage_wrong", PopCount(sc_corr_tage_misp))
    
  }

  override def getFoldedHistoryInfo = Some(tage_fh_info ++ sc_fh_info)

  val perfEvents = Seq(
    ("tage_tht_hit                  ", updateMetas(1).provider.valid + updateMetas(0).provider.valid),
    ("sc_update_on_mispred          ", PopCount(update_on_mispred) ),
    ("sc_update_on_unconf           ", PopCount(update_on_unconf)  ),
  )
  generatePerfEvent()
}
