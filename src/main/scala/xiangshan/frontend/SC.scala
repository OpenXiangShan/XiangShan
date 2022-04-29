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


class SCMeta(val ntables: Int)(implicit p: Parameters) extends XSBundle with HasSCParameter {
  val tageTakens = Vec(numBr, Bool())
  val scUsed = Vec(numBr, Bool())
  val scPreds = Vec(numBr, Bool())
  // Suppose ctrbits of all tables are identical
  val ctrs = Vec(numBr, Vec(ntables, SInt(SCCtrBits.W)))
}


class SCResp(val ctrBits: Int = 6)(implicit p: Parameters) extends SCBundle {
  val ctrs = Vec(numBr, Vec(2, SInt(ctrBits.W)))
}

class SCUpdate(val ctrBits: Int = 6)(implicit p: Parameters) extends SCBundle {
  val pc = UInt(VAddrBits.W)
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
  val mask = Vec(numBr, Bool())
  val oldCtrs = Vec(numBr, SInt(ctrBits.W))
  val tagePreds = Vec(numBr, Bool())
  val takens = Vec(numBr, Bool())
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
  val table = Module(new SRAMTemplate(SInt(ctrBits.W), set=nRows, way=2*TageBanks, shouldReset=false, holdRead=true, singlePort=false))

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
      (pc >> instOffsetBits)(log2Ceil(nRows)-1,0)
    }
  }


  def ctrUpdate(ctr: SInt, cond: Bool): SInt = signedSatUpdate(ctr, ctrBits, cond)

  val s0_idx = getIdx(io.req.bits.pc, io.req.bits.folded_hist)
  val s1_idx = RegEnable(s0_idx, enable=io.req.valid)

  val s1_pc = RegEnable(io.req.bits.pc, io.req.fire())
  val s1_unhashed_idx = s1_pc >> instOffsetBits

  table.io.r.req.valid := io.req.valid
  table.io.r.req.bits.setIdx := s0_idx

  val per_br_ctrs_unshuffled = table.io.r.resp.data.sliding(2,2).toSeq.map(VecInit(_))
  val per_br_ctrs = VecInit((0 until numBr).map(i => Mux1H(
    UIntToOH(get_phy_br_idx(s1_unhashed_idx, i), numBr),
    per_br_ctrs_unshuffled
  )))

  io.resp.ctrs := per_br_ctrs

  val update_wdata = Wire(Vec(numBr, SInt(ctrBits.W))) // correspond to physical bridx
  val update_wdata_packed = VecInit(update_wdata.map(Seq.fill(2)(_)).reduce(_++_))
  val updateWayMask = Wire(Vec(2*numBr, Bool())) // correspond to physical bridx

  val update_unhashed_idx = io.update.pc >> instOffsetBits
  for (pi <- 0 until numBr) {
    updateWayMask(2*pi)   := Seq.tabulate(numBr)(li =>
      io.update.mask(li) && get_phy_br_idx(update_unhashed_idx, li) === pi.U && !io.update.tagePreds(li)
    ).reduce(_||_)
    updateWayMask(2*pi+1) := Seq.tabulate(numBr)(li =>
      io.update.mask(li) && get_phy_br_idx(update_unhashed_idx, li) === pi.U &&  io.update.tagePreds(li)
    ).reduce(_||_)
  }

  val update_idx = getIdx(io.update.pc, io.update.folded_hist)

  table.io.w.apply(
    valid = io.update.mask.reduce(_||_),
    data = update_wdata_packed,
    setIdx = update_idx,
    waymask = updateWayMask.asUInt
  )

  val wrBypassEntries = 16

  // let it corresponds to logical brIdx
  val wrbypasses = Seq.fill(numBr)(Module(new WrBypass(SInt(ctrBits.W), wrBypassEntries, log2Ceil(nRows), numWays=2)))

  for (pi <- 0 until numBr) {
    val br_lidx = get_lgc_br_idx(update_unhashed_idx, pi.U(log2Ceil(numBr).W))

    val wrbypass_io = Mux1H(UIntToOH(br_lidx, numBr), wrbypasses.map(_.io))

    val ctrPos = Mux1H(UIntToOH(br_lidx, numBr), io.update.tagePreds)
    val bypass_ctr = wrbypass_io.hit_data(ctrPos)
    val previous_ctr = Mux1H(UIntToOH(br_lidx, numBr), io.update.oldCtrs)
    val hit_and_valid = wrbypass_io.hit && bypass_ctr.valid
    val oldCtr = Mux(hit_and_valid, bypass_ctr.bits, previous_ctr)
    val taken = Mux1H(UIntToOH(br_lidx, numBr), io.update.takens)
    update_wdata(pi) := ctrUpdate(oldCtr, taken)
  }

  val per_br_update_wdata_packed = update_wdata_packed.sliding(2,2).map(VecInit(_)).toSeq
  val per_br_update_way_mask = updateWayMask.sliding(2,2).map(VecInit(_)).toSeq
  for (li <- 0 until numBr) {
    val wrbypass = wrbypasses(li)
    val br_pidx = get_phy_br_idx(update_unhashed_idx, li)
    wrbypass.io.wen := io.update.mask(li)
    wrbypass.io.write_idx := update_idx
    wrbypass.io.write_data := Mux1H(UIntToOH(br_pidx, numBr), per_br_update_wdata_packed)
    wrbypass.io.write_way_mask.map(_ := Mux1H(UIntToOH(br_pidx, numBr), per_br_update_way_mask))
  }


  val u = io.update
  XSDebug(io.req.valid,
    p"scTableReq: pc=0x${Hexadecimal(io.req.bits.pc)}, " +
    p"s0_idx=${s0_idx}\n")
  XSDebug(RegNext(io.req.valid),
    p"scTableResp: s1_idx=${s1_idx}," +
    p"ctr:${io.resp.ctrs}\n")
  XSDebug(io.update.mask.reduce(_||_),
    p"update Table: pc:${Hexadecimal(u.pc)}, " +
    p"tageTakens:${u.tagePreds}, taken:${u.takens}, oldCtr:${u.oldCtrs}\n")
}

class SCThreshold(val ctrBits: Int = 6)(implicit p: Parameters) extends SCBundle {
  val ctr = UInt(ctrBits.W)
  def satPos(ctr: UInt = this.ctr) = ctr === ((1.U << ctrBits) - 1.U)
  def satNeg(ctr: UInt = this.ctr) = ctr === 0.U
  def neutralVal = (1 << (ctrBits - 1)).U
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
    val scTables = SCTableInfos.map {
      case (nRows, ctrBits, histLen) => {
        val t = Module(new SCTable(nRows/TageBanks, ctrBits, histLen))
        val req = t.io.req
        req.valid := io.s0_fire
        req.bits.pc := s0_pc
        req.bits.folded_hist := io.in.bits.folded_hist
        req.bits.ghist := DontCare
        if (!EnableSC) {t.io.update := DontCare}
        t
      }
    }
    sc_fh_info = scTables.map(_.getFoldedHistoryInfo).reduce(_++_).toSet

    val scThresholds = List.fill(TageBanks)(RegInit(SCThreshold(5)))
    val useThresholds = VecInit(scThresholds map (_.thres))

    def sign(x: SInt) = x(x.getWidth-1)
    def pos(x: SInt) = !sign(x)
    def neg(x: SInt) = sign(x)

    def aboveThreshold(scSum: SInt, tagePvdr: SInt, threshold: UInt): Bool = {
      val signedThres = threshold.zext
      val totalSum = scSum +& tagePvdr
      (scSum >  signedThres - tagePvdr) && pos(totalSum) ||
      (scSum < -signedThres - tagePvdr) && neg(totalSum)
    }
    val updateThresholds = VecInit(useThresholds map (t => (t << 3) +& 21.U))

    val s1_scResps = VecInit(scTables.map(t => t.io.resp))

    val scUpdateMask = WireInit(0.U.asTypeOf(Vec(numBr, Vec(SCNTables, Bool()))))
    val scUpdateTagePreds = Wire(Vec(TageBanks, Bool()))
    val scUpdateTakens = Wire(Vec(TageBanks, Bool()))
    val scUpdateOldCtrs = Wire(Vec(numBr, Vec(SCNTables, SInt(SCCtrBits.W))))
    scUpdateTagePreds := DontCare
    scUpdateTakens := DontCare
    scUpdateOldCtrs := DontCare

    val updateSCMeta = updateMeta.scMeta.get

    val s2_sc_used, s2_conf, s2_unconf, s2_agree, s2_disagree =
      WireInit(0.U.asTypeOf(Vec(TageBanks, Bool())))
    val update_sc_used, update_conf, update_unconf, update_agree, update_disagree =
      WireInit(0.U.asTypeOf(Vec(TageBanks, Bool())))
    val sc_misp_tage_corr, sc_corr_tage_misp =
      WireInit(0.U.asTypeOf(Vec(TageBanks, Bool())))

    // for sc ctrs
    def getCentered(ctr: SInt): SInt = Cat(ctr, 1.U(1.W)).asSInt
    // for tage ctrs, (2*(ctr-4)+1)*8
    def getPvdrCentered(ctr: UInt): SInt = Cat(ctr ^ (1 << (TageCtrBits-1)).U, 1.U(1.W), 0.U(3.W)).asSInt

    val scMeta = resp_meta.scMeta.get
    scMeta := DontCare
    for (w <- 0 until TageBanks) {
      // do summation in s2
      val s1_scTableSums = VecInit(
        (0 to 1) map { i =>
          ParallelSingedExpandingAdd(s1_scResps map (r => getCentered(r.ctrs(w)(i)))) // TODO: rewrite with wallace tree
        }
      )
      val s2_scTableSums = RegEnable(s1_scTableSums, io.s1_fire)
      val s2_tagePrvdCtrCentered = getPvdrCentered(RegEnable(s1_providerResps(w).ctr, io.s1_fire))
      val s2_totalSums = s2_scTableSums.map(_ +& s2_tagePrvdCtrCentered)
      val s2_sumAboveThresholds = VecInit((0 to 1).map(i => aboveThreshold(s2_scTableSums(i), s2_tagePrvdCtrCentered, useThresholds(w))))
      val s2_scPreds = VecInit(s2_totalSums.map(_ >= 0.S))

      val s2_scResps = VecInit(RegEnable(s1_scResps, io.s1_fire).map(_.ctrs(w)))
      val s2_scCtrs = VecInit(s2_scResps.map(_(s2_tageTakens(w).asUInt)))
      val s2_chooseBit = s2_tageTakens(w)

      val s2_pred =
        Mux(s2_provideds(w) && s2_sumAboveThresholds(s2_chooseBit),
          s2_scPreds(s2_chooseBit),
          s2_tageTakens(w)
        )

      scMeta.tageTakens(w) := RegEnable(s2_tageTakens(w), io.s2_fire)
      scMeta.scUsed(w)     := RegEnable(s2_provideds(w), io.s2_fire)
      scMeta.scPreds(w)    := RegEnable(s2_scPreds(s2_chooseBit), io.s2_fire)
      scMeta.ctrs(w)       := RegEnable(s2_scCtrs, io.s2_fire)

      when (s2_provideds(w)) {
        s2_sc_used(w) := true.B
        s2_unconf(w) := !s2_sumAboveThresholds(s2_chooseBit)
        s2_conf(w) := s2_sumAboveThresholds(s2_chooseBit)
        // Use prediction from Statistical Corrector
        XSDebug(p"---------tage_bank_${w} provided so that sc used---------\n")
        when (s2_sumAboveThresholds(s2_chooseBit)) {
          val pred = s2_scPreds(s2_chooseBit)
          val debug_pc = Cat(debug_pc_s2, w.U, 0.U(instOffsetBits.W))
          s2_agree(w) := s2_tageTakens(w) === pred
          s2_disagree(w) := s2_tageTakens(w) =/= pred
          // fit to always-taken condition
          // io.out.resp.s2.full_pred.br_taken_mask(w) := pred
          XSDebug(p"pc(${Hexadecimal(debug_pc)}) SC(${w.U}) overriden pred to ${pred}\n")
        }
      }

      when (io.ctrl.sc_enable) {
        io.out.resp.s3.full_pred.br_taken_mask(w) := RegEnable(s2_pred, io.s2_fire)
      }

      val updateTageMeta = updateMeta
      when (updateValids(w) && updateSCMeta.scUsed(w)) {
        val scPred = updateSCMeta.scPreds(w)
        val tagePred = updateSCMeta.tageTakens(w)
        val taken = update.full_pred.br_taken_mask(w)
        val scOldCtrs = updateSCMeta.ctrs(w)
        val pvdrCtr = updateTageMeta.providerResps(w).ctr
        val sum = ParallelSingedExpandingAdd(scOldCtrs.map(getCentered)) +& getPvdrCentered(pvdrCtr)
        val sumAbs = sum.abs.asUInt
        val updateThres = updateThresholds(w)
        val sumAboveThreshold = aboveThreshold(sum, getPvdrCentered(pvdrCtr), updateThres)
        scUpdateTagePreds(w) := tagePred
        scUpdateTakens(w) := taken
        (scUpdateOldCtrs(w) zip scOldCtrs).foreach{case (t, c) => t := c}

        update_sc_used(w) := true.B
        update_unconf(w) := !sumAboveThreshold
        update_conf(w) := sumAboveThreshold
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

        when (scPred =/= taken || !sumAboveThreshold) {
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
      for (i <- 0 until SCNTables) {
        scTables(i).io.update.mask(b) := RegNext(scUpdateMask(b)(i))
        scTables(i).io.update.tagePreds(b) := RegNext(scUpdateTagePreds(b))
        scTables(i).io.update.takens(b)    := RegNext(scUpdateTakens(b))
        scTables(i).io.update.oldCtrs(b)   := RegNext(scUpdateOldCtrs(b)(i))
        scTables(i).io.update.pc := RegNext(update.pc)
        scTables(i).io.update.folded_hist := RegNext(updateFHist)
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

  override val perfEvents = Seq(
    ("tage_tht_hit                  ", PopCount(updateMeta.providers.map(_.valid))),
    ("sc_update_on_mispred          ", PopCount(update_on_mispred) ),
    ("sc_update_on_unconf           ", PopCount(update_on_unconf)  ),
  )
  generatePerfEvent()
}
