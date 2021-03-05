package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.experimental.chiselName
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.transforms.RenameModules
import freechips.rocketchip.transforms.naming.RenameDesiredNames

import scala.math.min
import scala.util.matching.Regex

trait HasTageParameter extends HasXSParameter with HasBPUParameter with HasIFUConst {
  //                   Sets  Hist   Tag
  val TableInfo = Seq(( 128,    2,    7),
                      ( 128,    4,    7),
                      ( 256,    8,    8),
                      ( 256,   16,    8),
                      ( 128,   32,    9),
                      ( 128,   64,    9))
                      // (  64,   64,   11),
                      // (  64,  101,   12),
                      // (  64,  160,   12),
                      // (  64,  254,   13),
                      // (  32,  403,   14),
                      // (  32,  640,   15))
  val TageNTables = TableInfo.size
  val UBitPeriod = 2048
  val TageBanks = PredictWidth // FetchWidth
  val TageCtrBits = 3

  val TotalBits = TableInfo.map {
    case (s, h, t) => {
      s * (1+t+TageCtrBits) * PredictWidth
    }
  }.reduce(_+_)
}

trait HasFoldedHistory {
  val histLen: Int
  def compute_folded_hist(hist: UInt, l: Int) = {
    if (histLen > 0) {
      val nChunks = (histLen + l - 1) / l
      val hist_chunks = (0 until nChunks) map {i =>
        hist(min((i+1)*l, histLen)-1, i*l)
      }
      ParallelXOR(hist_chunks)
    }
    else 0.U
  }
}

abstract class TageBundle extends XSBundle
  with HasIFUConst with HasTageParameter
  with PredictorUtils
abstract class TageModule extends XSModule
  with HasIFUConst with HasTageParameter
  with PredictorUtils
  { val debug = true }


class TageReq extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
  val mask = UInt(PredictWidth.W)
}

class TageResp extends TageBundle {
  val ctr = UInt(TageCtrBits.W)
  val u = UInt(2.W)
}

class TageUpdate extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
  // update tag and ctr
  val mask = Vec(TageBanks, Bool())
  val taken = Vec(TageBanks, Bool())
  val alloc = Vec(TageBanks, Bool())
  val oldCtr = Vec(TageBanks, UInt(TageCtrBits.W))
  // update u
  val uMask = Vec(TageBanks, Bool())
  val u = Vec(TageBanks, UInt(2.W))
}

class FakeTageTable() extends TageModule {
  val io = IO(new Bundle() {
    val req = Input(Valid(new TageReq))
    val resp = Output(Vec(TageBanks, Valid(new TageResp)))
    val update = Input(new TageUpdate)
  })
  io.resp := DontCare

}
@chiselName
class TageTable(val nRows: Int, val histLen: Int, val tagLen: Int, val uBitPeriod: Int)
  extends TageModule with HasFoldedHistory {
  val io = IO(new Bundle() {
    val req = Input(Valid(new TageReq))
    val resp = Output(Vec(TageBanks, Valid(new TageResp)))
    val update = Input(new TageUpdate)
  })
  // override val debug = true
  // bypass entries for tage update
  val wrBypassEntries = 4

  def compute_tag_and_hash(unhashed_idx: UInt, hist: UInt) = {
    val idx_history = compute_folded_hist(hist, log2Ceil(nRows))
    val idx = (unhashed_idx ^ idx_history)(log2Ceil(nRows)-1,0)
    val tag_history = compute_folded_hist(hist, tagLen)
    // Use another part of pc to make tags
    val tag = ((unhashed_idx >> log2Ceil(nRows)) ^ tag_history)(tagLen-1,0)
    (idx, tag)
  }

  def inc_ctr(ctr: UInt, taken: Bool): UInt = satUpdate(ctr, TageCtrBits, taken)

  class TageEntry() extends TageBundle {
    val valid = Bool()
    val tag = UInt(tagLen.W)
    val ctr = UInt(TageCtrBits.W)
  }

  val tageEntrySz = instOffsetBits + tagLen + TageCtrBits

  def getUnhashedIdx(pc: UInt) = pc >> (instOffsetBits+log2Ceil(TageBanks))

  val if2_packetAlignedPC = packetAligned(io.req.bits.pc)
  val if2_unhashed_idx = getUnhashedIdx(io.req.bits.pc)

  val (if2_idx, if2_tag) = compute_tag_and_hash(if2_unhashed_idx, io.req.bits.hist)
  val (if3_idx, if3_tag) = (RegEnable(if2_idx, io.req.valid), RegEnable(if2_tag, io.req.valid))

  val hi_us = Module(new SRAMTemplate(Bool(), set=nRows, way=TageBanks, shouldReset=true, holdRead=true, singlePort=false))
  val lo_us = Module(new SRAMTemplate(Bool(), set=nRows, way=TageBanks, shouldReset=true, holdRead=true, singlePort=false))
  val table = Module(new SRAMTemplate(new TageEntry, set=nRows, way=TageBanks, shouldReset=true, holdRead=true, singlePort=false))

  table.io.r.req.valid := io.req.valid
  hi_us.io.r.req.valid := io.req.valid
  lo_us.io.r.req.valid := io.req.valid
  table.io.r.req.bits.setIdx := if2_idx
  hi_us.io.r.req.bits.setIdx := if2_idx
  lo_us.io.r.req.bits.setIdx := if2_idx

  val if3_hi_us_r = hi_us.io.r.resp.data
  val if3_lo_us_r = lo_us.io.r.resp.data
  val if3_table_r = table.io.r.resp.data

  val if2_mask = io.req.bits.mask
  val if3_mask = RegEnable(if2_mask, enable=io.req.valid)

  val if3_req_rhits = VecInit((0 until TageBanks).map(b => {
    if3_table_r(b).valid && if3_table_r(b).tag === if3_tag
  }))
  
  (0 until TageBanks).map(b => {
    io.resp(b).valid := if3_req_rhits(b) && if3_mask(b)
    io.resp(b).bits.ctr := if3_table_r(b).ctr
    io.resp(b).bits.u := Cat(if3_hi_us_r(b),if3_lo_us_r(b))
  })


  val clear_u_ctr = RegInit(0.U((log2Ceil(uBitPeriod) + log2Ceil(nRows) + 1).W))
  clear_u_ctr := clear_u_ctr + 1.U

  val doing_clear_u = clear_u_ctr(log2Ceil(uBitPeriod)-1,0) === 0.U
  val doing_clear_u_hi = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 1.U
  val doing_clear_u_lo = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 0.U
  val clear_u_idx = clear_u_ctr >> log2Ceil(uBitPeriod)

  // Use fetchpc to compute hash
  val (update_idx, update_tag) = compute_tag_and_hash(getUnhashedIdx(io.update.pc), io.update.hist)

  val update_wdata = Wire(Vec(TageBanks, new TageEntry))

  table.io.w.apply(
    valid = io.update.mask.asUInt.orR,
    data = update_wdata,
    setIdx = update_idx,
    waymask = io.update.mask.asUInt
  )

  val update_hi_wdata = Wire(Vec(TageBanks, Bool()))
  hi_us.io.w.apply(
    valid = io.update.uMask.asUInt.orR || doing_clear_u_hi,
    data = Mux(doing_clear_u_hi, 0.U.asTypeOf(Vec(TageBanks, Bool())), update_hi_wdata),
    setIdx = Mux(doing_clear_u_hi, clear_u_idx, update_idx),
    waymask = Mux(doing_clear_u_hi, Fill(TageBanks, "b1".U), io.update.uMask.asUInt)
  )

  val update_lo_wdata = Wire(Vec(TageBanks, Bool()))
  lo_us.io.w.apply(
    valid = io.update.uMask.asUInt.orR || doing_clear_u_lo,
    data = Mux(doing_clear_u_lo, 0.U.asTypeOf(Vec(TageBanks, Bool())), update_lo_wdata),
    setIdx = Mux(doing_clear_u_lo, clear_u_idx, update_idx),
    waymask = Mux(doing_clear_u_lo, Fill(TageBanks, "b1".U), io.update.uMask.asUInt)
  )

  val wrbypass_tags    = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(tagLen.W))))
  val wrbypass_idxs    = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(log2Ceil(nRows).W))))
  val wrbypass_ctrs    = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, Vec(TageBanks, UInt(TageCtrBits.W)))))
  val wrbypass_ctr_valids = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, Vec(TageBanks, Bool()))))
  val wrbypass_enq_idx = RegInit(0.U(log2Ceil(wrBypassEntries).W))

  when (reset.asBool) { wrbypass_ctr_valids.foreach(_.foreach(_ := false.B))}

  val wrbypass_hits    = VecInit((0 until wrBypassEntries) map { i =>
    wrbypass_tags(i) === update_tag &&
    wrbypass_idxs(i) === update_idx
  })


  val wrbypass_hit      = wrbypass_hits.reduce(_||_)
  // val wrbypass_rhit     = wrbypass_rhits.reduce(_||_)
  val wrbypass_hit_idx  = ParallelPriorityEncoder(wrbypass_hits)
  // val wrbypass_rhit_idx = PriorityEncoder(wrbypass_rhits)

  // val wrbypass_rctr_hits = VecInit((0 until TageBanks).map( b => wrbypass_ctr_valids(wrbypass_rhit_idx)(b)))

  // val rhit_ctrs = RegEnable(wrbypass_ctrs(wrbypass_rhit_idx), wrbypass_rhit)

  // when (RegNext(wrbypass_rhit)) {
  //   for (b <- 0 until TageBanks) {
  //     when (RegNext(wrbypass_rctr_hits(b.U + baseBank))) {
  //       io.resp(b).bits.ctr := rhit_ctrs(if3_bankIdxInOrder(b))
  //     }
  //   }
  // }


  val updateBank = PriorityEncoder(io.update.mask)

  for (w <- 0 until TageBanks) {
    update_wdata(w).ctr   := Mux(io.update.alloc(w),
      Mux(io.update.taken(w), 4.U,
                              3.U
      ),
      Mux(wrbypass_hit && wrbypass_ctr_valids(wrbypass_hit_idx)(w),
            inc_ctr(wrbypass_ctrs(wrbypass_hit_idx)(w), io.update.taken(w)),
            inc_ctr(io.update.oldCtr(w), io.update.taken(w))
      )
    )
    update_wdata(w).valid := true.B
    update_wdata(w).tag   := update_tag

    update_hi_wdata(w)    := io.update.u(w)(1)
    update_lo_wdata(w)    := io.update.u(w)(0)

    when (io.update.mask.reduce(_||_)) {
      when (wrbypass_hit) {
        when (io.update.mask(w)) {
          wrbypass_ctrs(wrbypass_hit_idx)(w) := update_wdata(w).ctr
          wrbypass_ctr_valids(wrbypass_hit_idx)(w) := true.B
        }
      } .otherwise {
        // reset valid bit first
        wrbypass_ctr_valids(wrbypass_enq_idx)(w) := false.B
        when (io.update.mask(w)) {
          wrbypass_ctr_valids(wrbypass_enq_idx)(w) := true.B
          wrbypass_ctrs(wrbypass_enq_idx)(w) := update_wdata(w).ctr
          wrbypass_tags(wrbypass_enq_idx) := update_tag
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
    val b = PriorityEncoder(u.mask)
    val ub = PriorityEncoder(u.uMask)
    val idx = if2_idx
    val tag = if2_tag
    XSDebug(io.req.valid, 
      p"tableReq: pc=0x${Hexadecimal(io.req.bits.pc)}, " +
      p"hist=${Hexadecimal(io.req.bits.hist)}, idx=$idx, " +
      p"tag=$tag, mask=${Binary(if2_mask)}\n")
    for (i <- 0 until TageBanks) {
      XSDebug(RegNext(io.req.valid && io.req.bits.mask(i)) && if3_req_rhits(i),
        p"TageTableResp[$i]: idx=$if3_idx, hit:${if3_req_rhits(i)}, " +
        p"ctr:${io.resp(i).bits.ctr}, u:${io.resp(i).bits.u}\n")
      XSDebug(io.update.mask(i),
        p"update Table bank $i: pc:${Hexadecimal(u.pc)}, hist:${Hexadecimal(u.hist)}, " +
        p"taken:${u.taken(i)}, alloc:${u.alloc(i)}, oldCtr:${u.oldCtr(i)}\n")
      XSDebug(io.update.mask(i),
        p"update Table bank $i: writing tag:${update_tag}, " +
        p"ctr: ${update_wdata(b).ctr} in idx $update_idx\n")
      val hitCtr = wrbypass_ctrs(wrbypass_hit_idx)(i)
      XSDebug(wrbypass_hit && wrbypass_ctr_valids(wrbypass_hit_idx)(i) && io.update.mask(i),
        p"bank $i wrbypass hit wridx:$wrbypass_hit_idx, idx:$update_idx, tag: $update_tag, " +
        p"ctr:$hitCtr, newCtr:${update_wdata(i).ctr}")
    }

    XSDebug(RegNext(io.req.valid) && !if3_req_rhits.reduce(_||_), "TageTableResp: no hits!\n")

    // when (wrbypass_rhit && wrbypass_ctr_valids(wrbypass_rhit_idx).reduce(_||_)) {
    //   for (b <- 0 until TageBanks) {
    //     XSDebug(wrbypass_ctr_valids(wrbypass_rhit_idx)(b),
    //       "wrbypass rhits, wridx:%d, tag:%x, idx:%d, hitctr:%d, bank:%d\n",
    //       wrbypass_rhit_idx, tag, idx, wrbypass_ctrs(wrbypass_rhit_idx)(b), b.U)
    //   }
    // }

    // ------------------------------Debug-------------------------------------
    val valids = Reg(Vec(TageBanks, Vec(nRows, Bool())))
    when (reset.asBool) { valids.foreach(b => b.foreach(r => r := false.B)) }
    (0 until TageBanks).map( b => { when (io.update.mask(b)) { valids(b)(update_idx) := true.B }})
    XSDebug("Table usage:------------------------\n")
    (0 until TageBanks).map( b => { XSDebug("Bank(%d): %d out of %d rows are valid\n", b.U, PopCount(valids(b)), nRows.U)})
  }

}

abstract class BaseTage extends BasePredictor with HasTageParameter {
  class TAGEResp extends Resp {
    val takens = Vec(PredictWidth, Bool())
    val hits = Vec(PredictWidth, Bool())
  }
  class TAGEMeta extends Meta{
  }
  class FromBIM extends FromOthers {
    val ctrs = Vec(PredictWidth, UInt(2.W))
  }
  class TageIO extends DefaultBasePredictorIO {
    val resp = Output(new TAGEResp)
    val meta = Output(Vec(PredictWidth, new TageMeta))
    val bim = Input(new FromBIM)
    val s3Fire = Input(Bool())
  }

  override val io = IO(new TageIO)
}

class FakeTage extends BaseTage {
  io.resp <> DontCare
  io.meta <> DontCare
}

@chiselName
class Tage extends BaseTage {

  val tables = TableInfo.map {
    case (nRows, histLen, tagLen) =>
      val t = if(EnableBPD) Module(new TageTable(nRows, histLen, tagLen, UBitPeriod)) else Module(new FakeTageTable)
      t.io.req.valid := io.pc.valid
      t.io.req.bits.pc := io.pc.bits
      t.io.req.bits.hist := io.hist
      t.io.req.bits.mask := io.inMask
      t
  }

  override val debug = true

  // Keep the table responses to process in s3
  // val if4_resps = RegEnable(VecInit(tables.map(t => t.io.resp)), enable=s3_fire)
  // val if4_scResps = RegEnable(VecInit(scTables.map(t => t.io.resp)), enable=s3_fire)
  
  val if3_resps = VecInit(tables.map(t => t.io.resp))


  val if3_bim = RegEnable(io.bim, enable=io.pc.valid) // actually it is s2Fire
  val if4_bim = RegEnable(if3_bim, enable=s3_fire)

  val debug_pc_s2 = RegEnable(io.pc.bits, enable=io.pc.valid)
  val debug_pc_s3 = RegEnable(debug_pc_s2, enable=s3_fire)

  val debug_hist_s2 = RegEnable(io.hist, enable=io.pc.valid)
  val debug_hist_s3 = RegEnable(debug_hist_s2, enable=s3_fire)

  val u = io.update.bits
  val updateValids = u.valids.map(v => v && io.update.valid)
  val updateHist = u.predHist.asUInt

  val updateBrMask = u.br_mask
  val updateMetas = VecInit(u.metas.map(_.tageMeta))
  val updateMisPred = u.mispred

  val updateMask = WireInit(0.U.asTypeOf(Vec(TageNTables, Vec(TageBanks, Bool()))))
  val updateUMask = WireInit(0.U.asTypeOf(Vec(TageNTables, Vec(TageBanks, Bool()))))
  val updateTaken = Wire(Vec(TageNTables, Vec(TageBanks, Bool())))
  val updateAlloc = Wire(Vec(TageNTables, Vec(TageBanks, Bool())))
  val updateOldCtr = Wire(Vec(TageNTables, Vec(TageBanks, UInt(TageCtrBits.W))))
  val updateU = Wire(Vec(TageNTables, Vec(TageBanks, UInt(2.W))))
  updateTaken := DontCare
  updateAlloc := DontCare
  updateOldCtr := DontCare
  updateU := DontCare

  val if3_tageTakens = Wire(Vec(TageBanks, Bool()))
  val if3_provideds = Wire(Vec(TageBanks, Bool()))
  val if3_providers = Wire(Vec(TageBanks, UInt(log2Ceil(TageBanks).W)))
  val if3_finalAltPreds = Wire(Vec(TageBanks, Bool()))
  val if3_providerUs = Wire(Vec(TageBanks, UInt(2.W)))
  val if3_providerCtrs = Wire(Vec(TageBanks, UInt(3.W)))

  val if4_tageTakens = RegEnable(if3_tageTakens, s3_fire)
  val if4_provideds = RegEnable(if3_provideds, s3_fire)
  val if4_providers = RegEnable(if3_providers, s3_fire)
  val if4_finalAltPreds = RegEnable(if3_finalAltPreds, s3_fire)
  val if4_providerUs = RegEnable(if3_providerUs, s3_fire)
  val if4_providerCtrs = RegEnable(if3_providerCtrs, s3_fire)


  val updateTageMisPreds = VecInit((0 until PredictWidth).map(i => updateMetas(i).taken =/= u.takens(i) && updateBrMask(i)))

  // val updateBank = u.pc(log2Ceil(TageBanks)+instOffsetBits-1, instOffsetBits)

  // access tag tables and output meta info
  for (w <- 0 until TageBanks) {
    val if3_tageTaken = WireInit(if3_bim.ctrs(w)(1).asBool)
    var if3_altPred = if3_bim.ctrs(w)(1)
    val if3_finalAltPred = WireInit(if3_bim.ctrs(w)(1))
    var if3_provided = false.B
    var if3_provider = 0.U

    for (i <- 0 until TageNTables) {
      val hit = if3_resps(i)(w).valid
      val ctr = if3_resps(i)(w).bits.ctr
      when (hit) {
        if3_tageTaken := Mux(ctr === 3.U || ctr === 4.U, if3_altPred, ctr(2)) // Use altpred on weak taken
        if3_finalAltPred := if3_altPred
      }
      if3_provided = if3_provided || hit          // Once hit then provide
      if3_provider = Mux(hit, i.U, if3_provider)  // Use the last hit as provider
      if3_altPred = Mux(hit, ctr(2), if3_altPred) // Save current pred as potential altpred
    }
    if3_provideds(w) := if3_provided
    if3_providers(w) := if3_provider
    if3_finalAltPreds(w) := if3_finalAltPred
    if3_tageTakens(w) := if3_tageTaken
    if3_providerUs(w) := if3_resps(if3_provider)(w).bits.u
    if3_providerCtrs(w) := if3_resps(if3_provider)(w).bits.ctr

    io.resp.hits(w) := if4_provideds(w) && ctrl.tage_enable
    io.resp.takens(w) := if4_tageTakens(w) && ctrl.tage_enable
    io.meta(w) := DontCare
    io.meta(w).provider.valid := if4_provideds(w)
    io.meta(w).provider.bits := if4_providers(w)
    io.meta(w).altDiffers := if4_finalAltPreds(w) =/= io.resp.takens(w)
    io.meta(w).providerU := if4_providerUs(w)
    io.meta(w).providerCtr := if4_providerCtrs(w)
    io.meta(w).taken := if4_tageTakens(w)

    // Create a mask fo tables which did not hit our query, and also contain useless entries
    // and also uses a longer history than the provider
    val allocatableSlots = RegEnable(VecInit(if3_resps.map(r => !r(w).valid && r(w).bits.u === 0.U)).asUInt &
      ~(LowerMask(UIntToOH(if3_provider), TageNTables) & Fill(TageNTables, if3_provided.asUInt)), s3_fire
    )
    val allocLFSR = LFSR64()(TageNTables - 1, 0)
    val firstEntry = PriorityEncoder(allocatableSlots)
    val maskedEntry = PriorityEncoder(allocatableSlots & allocLFSR)
    val allocEntry = Mux(allocatableSlots(maskedEntry), maskedEntry, firstEntry)
    io.meta(w).allocate.valid := allocatableSlots =/= 0.U
    io.meta(w).allocate.bits := allocEntry

    val updateValid = updateValids(w)
    val updateMeta = updateMetas(w)
    val updateIsBr = updateBrMask(w)
    val isUpdateTaken = updateValid && u.takens(w) && updateIsBr
    val updateMisPred = updateTageMisPreds(w)
    when (updateValid && updateIsBr) {
      when (updateMeta.provider.valid) {
        val provider = updateMeta.provider.bits

        updateMask(provider)(w) := true.B
        updateUMask(provider)(w) := true.B

        updateU(provider)(w) := Mux(!updateMeta.altDiffers, updateMeta.providerU,
          Mux(updateMisPred, Mux(updateMeta.providerU === 0.U, 0.U, updateMeta.providerU - 1.U),
                              Mux(updateMeta.providerU === 3.U, 3.U, updateMeta.providerU + 1.U))
        )
        updateTaken(provider)(w) := isUpdateTaken
        updateOldCtr(provider)(w) := updateMeta.providerCtr
        updateAlloc(provider)(w) := false.B
      }
    }
    when (updateValid && updateMisPred) {
      val allocate = updateMeta.allocate
      when (allocate.valid) {
        updateMask(allocate.bits)(w) := true.B
        updateTaken(allocate.bits)(w) := isUpdateTaken
        updateAlloc(allocate.bits)(w) := true.B
        updateUMask(allocate.bits)(w) := true.B
        updateU(allocate.bits)(w) := 0.U
      }.otherwise {
        val provider = updateMeta.provider
        val decrMask = Mux(provider.valid, ~LowerMask(UIntToOH(provider.bits), TageNTables), 0.U(TageNTables.W))
        for (i <- 0 until TageNTables) {
          when (decrMask(i)) {
            updateUMask(i)(w) := true.B
            updateU(i)(w) := 0.U
          }
        }
      }
    }
  }

  for (i <- 0 until TageNTables) {
    for (w <- 0 until TageBanks) {
      tables(i).io.update.mask(w) := RegNext(updateMask(i)(w))
      tables(i).io.update.taken(w) := RegNext(updateTaken(i)(w))
      tables(i).io.update.alloc(w) := RegNext(updateAlloc(i)(w))
      tables(i).io.update.oldCtr(w) := RegNext(updateOldCtr(i)(w))

      tables(i).io.update.uMask(w) := RegNext(updateUMask(i)(w))
      tables(i).io.update.u(w) := RegNext(updateU(i)(w))
      tables(i).io.update.pc := RegNext(packetAligned(u.ftqPC) + (w << instOffsetBits).U)
    }
    // use fetch pc instead of instruction pc
    tables(i).io.update.hist := RegNext(updateHist)
  }


  if (BPUDebug && debug) {
    for (b <- 0 until TageBanks) {
      val m = updateMetas(b)
      val bri = u.metas(b)
      XSDebug(updateValids(b), "update(%d): pc=%x, fetchpc=%x, cycle=%d, hist=%x, taken:%d, misPred:%d, bimctr:%d, pvdr(%d):%d, altDiff:%d, pvdrU:%d, pvdrCtr:%d, alloc(%d):%d\n",
        b.U, u.ftqPC, packetAligned(u.ftqPC)+(b << instOffsetBits).U, bri.debug_tage_cycle, updateHist, u.takens(b), u.mispred(b),
        bri.bimCtr, m.provider.valid, m.provider.bits, m.altDiffers, m.providerU, m.providerCtr, m.allocate.valid, m.allocate.bits
      )
    }
    val if4_resps = RegEnable(if3_resps, s3_fire)
    XSDebug(io.pc.valid, "req: pc=0x%x, hist=%x\n", io.pc.bits, io.hist)
    XSDebug(s3_fire, "s3Fire:%d, resp: pc=%x, hist=%x\n", s3_fire, debug_pc_s2, debug_hist_s2)
    XSDebug(RegNext(s3_fire), "s3FireOnLastCycle: resp: pc=%x, hist=%x, hits=%b, takens=%b\n",
      debug_pc_s3, debug_hist_s3, io.resp.hits.asUInt, io.resp.takens.asUInt)
    for (i <- 0 until TageNTables) {
      XSDebug(RegNext(s3_fire), "TageTable(%d): valids:%b, resp_ctrs:%b, resp_us:%b\n", i.U, VecInit(if4_resps(i).map(_.valid)).asUInt, Cat(if4_resps(i).map(_.bits.ctr)), Cat(if4_resps(i).map(_.bits.u)))
    }
    // XSDebug(io.update.valid && updateIsBr, p"update: sc: ${updateSCMeta}\n")
    // XSDebug(true.B, p"scThres: use(${useThreshold}), update(${updateThreshold})\n")
  }
}


class Tage_SC extends Tage with HasSC {}

object TageTest extends App {
  override def main(args: Array[String]): Unit = {
    (new ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => new Tage),
      RunFirrtlTransformAnnotation(new RenameDesiredNames)
    ))
  }
}