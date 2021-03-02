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

class SCTableIO extends SCBundle {
  val req = Input(Valid(new SCReq))
  val resp = Output(Vec(TageBanks, new SCResp))
  val update = Input(new SCUpdate)
}

@chiselName
class SCTable(val nRows: Int, val ctrBits: Int, val histLen: Int)
  extends SCModule with HasFoldedHistory {
  val io = IO(new SCTableIO)

  val table = Module(new SRAMTemplate(SInt(ctrBits.W), set=nRows, way=2*TageBanks, shouldReset=true, holdRead=true, singlePort=false))

  def getIdx(hist: UInt, pc: UInt) = {
    (compute_folded_hist(hist, log2Ceil(nRows)) ^ (pc >> instOffsetBits.U))(log2Ceil(nRows)-1,0)
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
          wrbypass_idxs(wrbypass_enq_idx) := update_idx
        }
      }
    }
  }

  when (io.update.mask.reduce(_||_) && !wrbypass_hit) {
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

class SCThreshold(val ctrBits: Int = 5) extends SCBundle {
  val ctr = UInt(ctrBits.W)
  def satPos(ctr: UInt = this.ctr) = ctr === ((1.U << ctrBits) - 1.U)
  def satNeg(ctr: UInt = this.ctr) = ctr === 0.U
  def neutralVal = (1.U << (ctrBits - 1))
  val thres = UInt(5.W)
  def minThres = 5.U
  def maxThres = 31.U
  def update(cause: Bool): SCThreshold = {
    val res = Wire(new SCThreshold(this.ctrBits))
    val newCtr = satUpdate(this.ctr, this.ctrBits, cause)
    val newThres = Mux(res.satPos(newCtr), this.thres + 1.U,
                      Mux(res.satNeg(newCtr), this.thres - 1.U,
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
    t.thres := t.minThres
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
  
  val scThreshold = RegInit(SCThreshold(5))
  val useThreshold = WireInit(scThreshold.thres)
  val updateThreshold = WireInit((useThreshold << 3) + 21.U)
  
  val if3_scResps = VecInit(scTables.map(t => t.io.resp))

  val scUpdateMask = WireInit(0.U.asTypeOf(Vec(SCNTables, Vec(TageBanks, Bool()))))
  val scUpdateTagePreds = Wire(Vec(TageBanks, Bool()))
  val scUpdateTakens = Wire(Vec(TageBanks, Bool()))
  val scUpdateOldCtrs = Wire(Vec(TageBanks, Vec(SCNTables, SInt(SCCtrBits.W))))
  scUpdateTagePreds := DontCare
  scUpdateTakens := DontCare
  scUpdateOldCtrs := DontCare

  val updateSCMetas = VecInit(u.metas.map(_.tageMeta.scMeta))

  // for sc ctrs
  def getCentered(ctr: SInt): SInt = (ctr << 1).asSInt + 1.S
  // for tage ctrs
  def getPvdrCentered(ctr: UInt): SInt = ((((ctr.zext - 4.S) << 1).asSInt + 1.S) << 3).asSInt

  for (w <- 0 until TageBanks) {
    val scMeta = io.meta(w).scMeta
    scMeta := DontCare
    // do summation in if3
    val if3_scTableSums = VecInit(
      (0 to 1) map { i => {
          (0 until SCNTables) map { j => 
            getCentered(if3_scResps(j)(w).ctr(i))
          } reduce (_+_) // TODO: rewrite with adder tree
        }
      }
    )

    val providerCtr = if3_providerCtrs(w)
    val if3_pvdrCtrCentered = getPvdrCentered(providerCtr)
    val if3_totalSums = VecInit(if3_scTableSums.map(_  + if3_pvdrCtrCentered))
    val if3_sumAbs = VecInit(if3_totalSums.map(_.abs.asUInt))
    val if3_sumBelowThresholds = VecInit(if3_sumAbs.map(_ < useThreshold))
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
      // Use prediction from Statistical Corrector
      XSDebug(p"---------tage${w} provided so that sc used---------\n")
      XSDebug(p"scCtrs:$if4_scCtrs, prdrCtr:${if4_providerCtrs(w)}, sumAbs:$if4_sumAbs, tageTaken:${if4_chooseBit}\n")
      when (!if4_sumBelowThresholds(if4_chooseBit)) {
        when (ctrl.sc_enable) {
          val pred = if4_scPreds(if4_chooseBit)
          val debug_pc = Cat(packetIdx(debug_pc_s3), w.U, 0.U(instOffsetBits.W))
          XSDebug(p"pc(${Hexadecimal(debug_pc)}) SC(${w.U}) overriden pred to ${pred}\n")
          io.resp.takens(w) := pred
        }
      }
    }

    val updateSCMeta = updateSCMetas(w)
    val updateTageMeta = updateMetas(w)
    when (updateValids(w) && updateSCMeta.scUsed.asBool && updateBrMask(w)) {
      val scPred = updateSCMeta.scPred
      val tagePred = updateSCMeta.tageTaken
      val taken = u.takens(w)
      val scOldCtrs = updateSCMeta.ctrs
      val pvdrCtr = updateTageMeta.providerCtr
      val sum = scOldCtrs.map(getCentered).reduce(_+_) + getPvdrCentered(pvdrCtr)
      val sumAbs = sum.abs.asUInt
      scUpdateTagePreds(w) := tagePred
      scUpdateTakens(w) := taken
      (scUpdateOldCtrs(w) zip scOldCtrs).foreach{case (t, c) => t := c}

      when (scPred =/= tagePred && sumAbs < useThreshold - 2.U) {
        val newThres = scThreshold.update(scPred =/= taken)
        scThreshold := newThres
        XSDebug(p"scThres update: old d${useThreshold} --> new ${newThres.thres}\n")
      }
      when (scPred =/= taken || sumAbs < updateThreshold) {
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
      }
    }

    for (i <- 0 until SCNTables) {
      scTables(i).io.update.mask := RegNext(scUpdateMask(i))
      scTables(i).io.update.tagePreds := RegNext(scUpdateTagePreds)
      scTables(i).io.update.takens    := RegNext(scUpdateTakens)
      scTables(i).io.update.oldCtrs   := RegNext(VecInit(scUpdateOldCtrs.map(_(i))))
      scTables(i).io.update.pc := RegNext(u.ftqPC)
      scTables(i).io.update.hist := RegNext(updateHist)
    }
  }
}