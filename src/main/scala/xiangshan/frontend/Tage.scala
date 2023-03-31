/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * ************************************************************************************* */

package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import chisel3.experimental.chiselName

import scala.math.min

trait TageParams extends HasBPUConst with HasXSParameter {
  val TageNTables: Int = TageTableInfos.size
  val TageCtrBits      = 3
  // Controlled Allocation Throttling (CAT) is a mechanism to control TAGE entry allocation
  // Details see BATAGE paper
  val TageCATMAX : Int = 65536 * 16 - 1
  val BatageCATR1: Int = 1
  val BatageCATR2: Int = 6

  val TageMINAP: Int = 4 // Minimum allocation probability, MINAP = 4 mean at lease 1/4 probability

  val TageTotalBits: Int = TageTableInfos.map {
    case (sets, h, tag_width) => {
      sets * (tag_width + TageCtrBits * 2)
    }
  }.sum

  def posUnconf(ctr: UInt): Bool = ctr === (1 << (ctr.getWidth - 1)).U

  def negUnconf(ctr: UInt): Bool = ctr === ((1 << (ctr.getWidth - 1)) - 1).U

  def unconf(ctr: UInt): Bool = posUnconf(ctr) || negUnconf(ctr)

  val unshuffleBitWidth = log2Ceil(numBr)

  def get_unshuffle_bits(idx: UInt) = idx(unshuffleBitWidth - 1, 0)

  // xor hashes are reversable
  def get_phy_br_idx(unhashed_idx: UInt, br_lidx: Int) = get_unshuffle_bits(unhashed_idx) ^ br_lidx.U(log2Ceil(numBr).W)

  def get_lgc_br_idx(unhashed_idx: UInt, br_pidx: UInt) = get_unshuffle_bits(unhashed_idx) ^ br_pidx

}

trait HasFoldedHistory {
  val histLen: Int

  def compute_folded_hist(hist: UInt, l: Int)(histLen: Int) = {
    if (histLen > 0) {
      val nChunks     = (histLen + l - 1) / l
      val hist_chunks = (0 until nChunks) map { i =>
        hist(min((i + 1) * l, histLen) - 1, i * l)
      }
      ParallelXOR(hist_chunks)
    }
    else 0.U
  }

  val compute_folded_ghist = compute_folded_hist(_: UInt, _: Int)(histLen)
}

abstract class TageBundle(implicit p: Parameters)
  extends XSBundle with TageParams with BPUUtils

abstract class TageModule(implicit p: Parameters)
  extends XSModule with TageParams with BPUUtils {}

class TageReq(implicit p: Parameters) extends TageBundle {
  val pc          = UInt(VAddrBits.W)
  val ghist       = UInt(HistoryLength.W)
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
}

// TageResp is the entry info from each table (component)
class TageResp(implicit p: Parameters) extends TageBundle {
  val ctr_up   = UInt(TageCtrBits.W)
  val ctr_down = UInt(TageCtrBits.W)

  // Confidence level
  // 0: High, 1: Mid, 2: Low
  def conf(): UInt = {
    val medium: Bool = (ctr_up === Cat(ctr_down, 1.U)) || (Cat(ctr_up, 1.U) === ctr_down)
    val low   : Bool = (ctr_up < Cat(ctr_down, 1.U)) && (Cat(ctr_up, 1.U) > ctr_down)
    Cat(low, medium)
  }

  // Whether this is a Mid-High confidence
  def isMHC(): Bool = {
    // High conf but not that high
    // See BATAGE paper for detailed definition
    this.conf() === 0.U && (ctr_up < 4.U) && (ctr_down < 4.U)
  }

  def taken(): Bool = ctr_up >= ctr_down

  def unconf(): Bool = this.conf() =/= 0.U
}

class TageUpdate(implicit p: Parameters) extends TageBundle {
  val pc           = UInt(VAddrBits.W)
  val folded_hist  = new AllFoldedHistories(foldedGHistInfos)
  val ghist        = UInt(HistoryLength.W)
  // update tag and ctr
  val mask         = Vec(numBr, Bool())
  val takens       = Vec(numBr, Bool())
  val decay        = Vec(numBr, Bool()) // BATAGE decay mechanism
  val alloc        = Vec(numBr, Bool())
  val old_ctr_up   = Vec(numBr, UInt(TageCtrBits.W)) // BATAGE up down ctr
  val old_ctr_down = Vec(numBr, UInt(TageCtrBits.W)) // BATAGE up down ctr
}

class TageMeta(implicit p: Parameters)
  extends TageBundle with HasSCParameter {
  // Providers with longest history and highest confidence
  val providers     = Vec(numBr, ValidUndirectioned(UInt(log2Ceil(TageNTables).W)))
  // Next longest history that matches tag
  val nextProviders = Vec(numBr, ValidUndirectioned(UInt(log2Ceil(TageNTables).W)))
  val tagResps      = Vec(numBr, Vec(TageNTables, new TageResp))
  // Tag match hits
  val tagHits       = Vec(numBr, Vec(TageNTables, Bool()))
  val basecnts      = Vec(numBr, UInt(2.W))
  val takens        = Vec(numBr, Bool())
  val scMeta        = if (EnableSC) Some(new SCMeta(SCNTables)) else None
  val pred_cycle    = if (!env.FPGAPlatform) Some(UInt(64.W)) else None
}

trait TBTParams extends HasXSParameter with TageParams {
  val BtSize        = 2048
  val bypassEntries = 8
}

@chiselName
class TageBTable(implicit p: Parameters) extends XSModule with TBTParams {
  val io = IO(new Bundle {
    // Predict
    val s0_fire       = Input(Bool())
    val s0_pc         = Input(UInt(VAddrBits.W))
    val s1_cnt        = Output(Vec(numBr, UInt(2.W)))
    // Update
    val update_mask   = Input(Vec(TageBanks, Bool()))
    val update_pc     = Input(UInt(VAddrBits.W))
    val update_cnt    = Input(Vec(numBr, UInt(2.W)))
    val update_takens = Input(Vec(TageBanks, Bool()))
  })

  val bimAddr = new TableAddr(log2Up(BtSize), instOffsetBits)

  val bt = Module(new SRAMTemplate(UInt(2.W), set = BtSize, way = numBr, shouldReset = true, holdRead = true, bypassWrite = true))

  val doing_reset = RegInit(true.B)
  val resetRow    = RegInit(0.U(log2Ceil(BtSize).W))
  resetRow := resetRow + doing_reset
  when(resetRow === (BtSize - 1).U) {
    doing_reset := false.B
  }

  val s0_idx = bimAddr.getIdx(io.s0_pc)
  bt.io.r.req.valid := io.s0_fire
  bt.io.r.req.bits.setIdx := s0_idx

  val s1_read = bt.io.r.resp.data
  val s1_idx  = RegEnable(s0_idx, io.s0_fire)


  val per_br_ctr = VecInit((0 until numBr).map(i => Mux1H(UIntToOH(get_phy_br_idx(s1_idx, i), numBr), s1_read)))
  io.s1_cnt := per_br_ctr

  // Update logic

  val u_idx = bimAddr.getIdx(io.update_pc)

  val newCtrs = Wire(Vec(numBr, UInt(2.W))) // physical bridx

  val wrbypass = Module(new WrBypass(UInt(2.W), bypassEntries, log2Up(BtSize), numWays = numBr)) // logical bridx
  wrbypass.io.wen := io.update_mask.reduce(_ || _)
  wrbypass.io.write_idx := u_idx
  wrbypass.io.write_way_mask.map(_ := io.update_mask)
  for (li <- 0 until numBr) {
    val br_pidx = get_phy_br_idx(u_idx, li)
    wrbypass.io.write_data(li) := newCtrs(br_pidx)
  }


  val oldCtrs =
    VecInit((0 until numBr).map(pi => {
      val br_lidx = get_lgc_br_idx(u_idx, pi.U(log2Ceil(numBr).W))
      Mux(wrbypass.io.hit && wrbypass.io.hit_data(br_lidx).valid,
        wrbypass.io.hit_data(br_lidx).bits,
        io.update_cnt(br_lidx))
    }))

  def satUpdate(old: UInt, len: Int, taken: Bool): UInt = {
    val oldSatTaken    = old === ((1 << len) - 1).U
    val oldSatNotTaken = old === 0.U
    Mux(oldSatTaken && taken, ((1 << len) - 1).U,
      Mux(oldSatNotTaken && !taken, 0.U,
        Mux(taken, old + 1.U, old - 1.U)))
  }

  val newTakens = io.update_takens
  newCtrs := VecInit((0 until numBr).map(pi => {
    val br_lidx = get_lgc_br_idx(u_idx, pi.U(log2Ceil(numBr).W))
    satUpdate(oldCtrs(pi), 2, newTakens(br_lidx))
  }))

  val updateWayMask = VecInit((0 until numBr).map(pi =>
    (0 until numBr).map(li =>
      io.update_mask(li) && get_phy_br_idx(u_idx, li) === pi.U
    ).reduce(_ || _)
  )).asUInt

  bt.io.w.apply(
    valid = io.update_mask.reduce(_ || _) || doing_reset,
    data = Mux(doing_reset, VecInit(Seq.fill(numBr)(2.U(2.W))), newCtrs),
    setIdx = Mux(doing_reset, resetRow, u_idx),
    waymask = Mux(doing_reset, Fill(numBr, 1.U(1.W)).asUInt(), updateWayMask)
  )

}


@chiselName
class TageTable
(
  val nRows: Int, val histLen: Int, val tagLen: Int, val tableIdx: Int
)(implicit p: Parameters)
  extends TageModule with HasFoldedHistory {
  val io = IO(new Bundle() {
    val req    = Flipped(DecoupledIO(new TageReq))
    val resps  = Output(Vec(numBr, Valid(new TageResp)))
    val update = Input(new TageUpdate)
  })

  // Constants
  private val nRowsPerBr: Int = nRows / numBr

  // bypass entries for tage update
  val perBankWrbypassEntries = 8

  // Class definitions
  class TageTag() extends TageBundle {
    val value: UInt = UInt(tagLen.W)
  }

  class TageCtr() extends TageBundle {
    val up  : UInt = UInt(TageCtrBits.W)
    val down: UInt = UInt(TageCtrBits.W)
  }

  class TageEntry() extends TageBundle {
    val tag: TageTag = new TageTag()
    val ctr: TageCtr = new TageCtr()
  }

  // Internal functions
  def getFoldedHistoryInfo: Set[(Int, Int)] = allFhInfos.filter(_._1 > 0).toSet

  def computeTagAndHash(unhashedIdx: UInt, allFh: AllFoldedHistories): (UInt, UInt) = {
    val idxFoldedHistory  = allFh.getHistWithInfo(idxFhInfo).folded_hist
    val tagFoldedHistory  = allFh.getHistWithInfo(tagFhInfo).folded_hist
    val tag2FoldedHistory = allFh.getHistWithInfo(altTagFhInfo).folded_hist

    // Sanity check
    require(idxFoldedHistory.getWidth <= log2Ceil(nRowsPerBr)) // history length maybe shorter

    val idx = (unhashedIdx ^ idxFoldedHistory)(log2Ceil(nRowsPerBr) - 1, 0)
    val tag = (unhashedIdx ^ tagFoldedHistory ^ (tag2FoldedHistory << 1).asUInt)(tagLen - 1, 0)
    (idx, tag)
  }

  def ctrUpdate(ctr: UInt, taken: Bool): UInt = satUpdate(ctr, TageCtrBits, taken)

  def getUnhashedIdx(pc: UInt): UInt = (pc >> instOffsetBits).asUInt

  // silentUpdate
  // under this condition, the content after update is still the same
  // so can avoid update to lower SRAM power consumption
  def silentUpdate(ctr: UInt, taken: Bool): Bool = {
    ctr.andR && taken || !ctr.orR && !taken
  }

  // Physical SRAM size
  val SRAM_SIZE = 256
  // Sanity check
  require(nRows % SRAM_SIZE == 0)
  require(isPow2(numBr))
  val nBanks        = 8
  val bankSize      = nRowsPerBr / nBanks
  val bankFoldWidth = if (bankSize >= SRAM_SIZE) bankSize / SRAM_SIZE else 1
  if (bankSize < SRAM_SIZE) {
    println(f"warning: tage table $tableIdx has small sram depth of $bankSize")
  }
  val bankIdxWidth = log2Ceil(nBanks)

  def get_bank_mask(idx: UInt): Vec[Bool] = VecInit((0 until nBanks).map(idx(bankIdxWidth - 1, 0) === _.U))

  def get_bank_idx(idx: UInt) = idx >> bankIdxWidth


  // FhInfo is in (original length, compressed length) form
  val idxFhInfo   : (Int, Int) = (histLen, min(log2Ceil(nRowsPerBr), histLen))
  val tagFhInfo   : (Int, Int) = (histLen, min(histLen, tagLen))
  val altTagFhInfo: (Int, Int) = (histLen, min(histLen, tagLen - 1))
  val allFhInfos               = Seq(idxFhInfo, tagFhInfo, altTagFhInfo)

  val tagBanks = Seq.fill(nBanks)(
    Module(new FoldedSRAMTemplate(
      new TageTag,
      set = bankSize,
      width = bankFoldWidth,
      way = numBr,
      shouldReset = true,
      holdRead = true,
      singlePort = true)
    )
  )
  val ctrBanks = Seq.fill(nBanks)(
    Module(new FoldedSRAMTemplate(
      new TageCtr,
      set = bankSize,
      width = bankFoldWidth,
      way = numBr,
      shouldReset = true,
      holdRead = true,
      singlePort = true)
    )
  )

  val tableBanks_wrbypasses = Seq.fill(nBanks)(Seq.fill(numBr)(
    Module(new WrBypass(UInt((TageCtrBits * 2).W), perBankWrbypassEntries, 1, tagWidth = tagLen))
  )) // let it corresponds to logical brIdx


  // CSR base folded history debug
  // Diff with parallel XOR
  if (EnableGHistDiff) {
    val idx_history = compute_folded_ghist(io.req.bits.ghist, log2Ceil(nRowsPerBr))
    val idx_fh      = io.req.bits.folded_hist.getHistWithInfo(idxFhInfo)
    XSError(idx_history =/= idx_fh.folded_hist, p"tage table $tableIdx has different fh," +
      p" ghist: ${Binary(idx_history)}, fh: ${Binary(idx_fh.folded_hist)}\n")
  }

  // Stage 0
  val req_unhashedIdx  = getUnhashedIdx(io.req.bits.pc)
  val (s0_idx, s0_tag) = computeTagAndHash(req_unhashedIdx, io.req.bits.folded_hist)
  val s0_bank_req_1h   = get_bank_mask(s0_idx)

  // SRAM R port
  for (b <- 0 until nBanks) {
    tagBanks(b).io.r.req.valid := io.req.fire && s0_bank_req_1h(b)
    tagBanks(b).io.r.req.bits.setIdx := get_bank_idx(s0_idx)
    ctrBanks(b).io.r.req.valid := io.req.fire && s0_bank_req_1h(b)
    ctrBanks(b).io.r.req.bits.setIdx := get_bank_idx(s0_idx)
  }

  // Predict query
  // Stage 1
  val s1_unhashedIdx               : UInt      = RegEnable(req_unhashedIdx, io.req.fire)
  val s1_idx                       : UInt      = RegEnable(s0_idx, io.req.fire)
  val s1_tag                       : UInt      = RegEnable(s0_tag, io.req.fire)
  val s1_pc                        : UInt      = RegEnable(io.req.bits.pc, io.req.fire)
  val s1_bankReqOH                 : Vec[Bool] = RegEnable(s0_bank_req_1h, io.req.fire)
  val s1_bank_has_write_on_this_req: Vec[Bool] = RegEnable(VecInit(tagBanks.map(_.io.w.req.valid)), io.req.valid)

  val s1_resp_invalid_by_write: Bool = Mux1H(s1_bankReqOH, s1_bank_has_write_on_this_req)

  // s1_phy* signals are corresponded to physical branch Idx
  val s1_phyTablesRespRaw: Seq[Vec[TageEntry]] = tagBanks.zip(ctrBanks).map { case (tagEntry, ctrEntry) => {
    val entry = Wire(Vec(numBr, new TageEntry()))
    for (b <- 0 until numBr) {
      entry(b).tag := tagEntry.io.r.resp.data(b)
      entry(b).ctr := ctrEntry.io.r.resp.data(b)
    }
    entry
  }
  }
  // SRAM resp data
  val s1_phyHitRaw       : Seq[Vec[Bool]]      = s1_phyTablesRespRaw.map(r => VecInit(r.map(e => {
    e.tag.value === s1_tag && !s1_resp_invalid_by_write // Do tag compare in S1
  })))
  val s1_phyTablesResp   : Vec[TageEntry]      = Mux1H(s1_bankReqOH, s1_phyTablesRespRaw)
  val s1_phyHit          : Vec[Bool]           = Mux1H(s1_bankReqOH, s1_phyHitRaw)


  val s1_tablesResp: Vec[TageEntry] = VecInit((0 until numBr).map(
    i => Mux1H(UIntToOH(get_phy_br_idx(s1_unhashedIdx, i), numBr), s1_phyTablesResp))
  )
  val s1_hit       : Vec[Bool]      = VecInit((0 until numBr).map(
    i => Mux1H(UIntToOH(get_phy_br_idx(s1_unhashedIdx, i), numBr), s1_phyHit))
  )

  // Generate S1 Response
  for (i <- 0 until numBr) {
    io.resps(i).valid := s1_hit(i)
    io.resps(i).bits.ctr_up := s1_tablesResp(i).ctr.up
    io.resps(i).bits.ctr_down := s1_tablesResp(i).ctr.down
  }


  // Update logic
  if (EnableGHistDiff) {
    val update_idx_history = compute_folded_ghist(io.update.ghist, log2Ceil(nRowsPerBr))
    val update_idx_fh      = io.update.folded_hist.getHistWithInfo(idxFhInfo)
    XSError(update_idx_history =/= update_idx_fh.folded_hist && io.update.mask.reduce(_ || _),
      p"tage table $tableIdx has different fh when update," +
        p" ghist: ${Binary(update_idx_history)}, fh: ${Binary(update_idx_fh.folded_hist)}\n")
  }
  val update_unhashedIdx            = getUnhashedIdx(io.update.pc)
  val (update_idx, update_tag)      = computeTagAndHash(update_unhashedIdx, io.update.folded_hist) // Recompute Hash
  val update_reqBankOH  : Vec[Bool] = get_bank_mask(update_idx)
  val update_idx_in_bank: Bits      = get_bank_idx(update_idx)

  // No update on tag by default
  val update_phyWTagValid      : Vec[Vec[Bool]]    = WireInit(0.U.asTypeOf(Vec(nBanks, Vec(numBr, Bool())))) // [nBanks, numBr]
  val update_phyWTag           : Vec[Vec[TageTag]] = Wire(Vec(nBanks, Vec(numBr, new TageTag))) // [nBanks, numBr]
  val update_phyWCtr           : Vec[Vec[TageCtr]] = Wire(Vec(nBanks, Vec(numBr, new TageCtr))) // [nBanks, numBr]
  val update_phyNotSilentUpdate: Vec[Vec[Bool]]    = Wire(Vec(nBanks, Vec(numBr, Bool()))) // [nBanks, numBr]
  val update_phyWayMask        : Vec[UInt]         =
    VecInit((0 until nBanks).map(b =>
      VecInit((0 until numBr).map(pi => {
        // whether any of the logical branches updates on each slot
        Seq.tabulate(numBr)(li =>
          get_phy_br_idx(update_unhashedIdx, li) === pi.U &&
            io.update.mask(li)).reduce(_ || _) && update_phyNotSilentUpdate(b)(pi)
      })).asUInt
    ))

  // SRAM W port
  for (b <- 0 until nBanks) {
    tagBanks(b).io.w.apply(
      valid = update_phyWayMask(b).orR && update_reqBankOH(b) && update_phyWTagValid(b).asUInt.orR,
      data = update_phyWTag(b),
      setIdx = update_idx_in_bank.asUInt,
      waymask = update_phyWayMask(b)
    )
    ctrBanks(b).io.w.apply(
      valid = update_phyWayMask(b).orR && update_reqBankOH(b),
      data = update_phyWCtr(b),
      setIdx = update_idx_in_bank.asUInt,
      waymask = update_phyWayMask(b)
    )
  }

  // Always ready since WrBypass added in
  io.req.ready := true.B


  // Update signal gen
  for (b <- 0 until nBanks) {
    for (brPhyIdx <- 0 until numBr) { // physical brIdx
      val not_silent_update = update_phyNotSilentUpdate(b)(brPhyIdx)
      val update_wCtr       = update_phyWCtr(b)(brPhyIdx)
      val update_wTag       = update_phyWTag(b)(brPhyIdx)
      val update_wTagValid  = update_phyWTagValid(b)(brPhyIdx)
      val brLogicIdx        = get_lgc_br_idx(update_unhashedIdx, brPhyIdx.U(log2Ceil(numBr).W))
      val alloc             = io.update.alloc(brLogicIdx)
      val taken             = io.update.takens(brLogicIdx)
      val decay             = io.update.decay(brLogicIdx)

      // WrBypass signals
      val wrbypass_io         = Mux1H(UIntToOH(brLogicIdx, numBr), tableBanks_wrbypasses(b).map(_.io))
      val wrbypass_hit        = wrbypass_io.hit
      val wrbypass_ctr: UInt  = wrbypass_io.hit_data(0).bits
      val wrbypass_ctr_down   = wrbypass_ctr(TageCtrBits - 1, 0)
      val wrbypass_ctr_up     = wrbypass_ctr(TageCtrBits * 2 - 1, TageCtrBits)
      val wrbypass_data_valid = wrbypass_hit && wrbypass_io.hit_data(0).valid

      val latest_ctr_up   = Wire(UInt(TageCtrBits.W))
      val latest_ctr_down = Wire(UInt(TageCtrBits.W))
      when(wrbypass_data_valid) {
        latest_ctr_up := wrbypass_ctr_up
        latest_ctr_down := wrbypass_ctr_down
      } otherwise {
        latest_ctr_up := io.update.old_ctr_up(brLogicIdx)
        latest_ctr_down := io.update.old_ctr_down(brLogicIdx)
      }

      // Sanity check
      assert(!(alloc && decay)) // Alloc and decay should not be both true

      // Generate update_wdata
      update_wTag.value := update_tag
      when(alloc) {
        // Update tags
        update_wTagValid := true.B
        // Allocation need to update tag, so always update
        not_silent_update := true.B
        // Init counters according to taken
        when(taken) {
          update_wCtr.up := 1.U
          update_wCtr.down := 0.U
        }.otherwise {
          update_wCtr.up := 0.U
          update_wCtr.down := 1.U
        }
      }.otherwise {
        when(decay) {
          // Decay the larger counter
          when(latest_ctr_up > latest_ctr_down) {
            update_wCtr.up := latest_ctr_up - 1.U
            update_wCtr.down := latest_ctr_down
            not_silent_update := true.B
          }.elsewhen(latest_ctr_down < latest_ctr_up) {
            update_wCtr.up := latest_ctr_up
            update_wCtr.down := latest_ctr_down - 1.U
            not_silent_update := true.B
          }.otherwise {
            update_wCtr.up := latest_ctr_up
            update_wCtr.down := latest_ctr_down
            not_silent_update := false.B
          }
        }.otherwise {
          update_wCtr.up := ctrUpdate(latest_ctr_up, taken)
          update_wCtr.down := ctrUpdate(latest_ctr_down, !taken)
          not_silent_update := !silentUpdate(latest_ctr_up, taken) || !silentUpdate(latest_ctr_down, !taken)
        }
      }
    }

    // Maintain WrBypass
    for (brLogicIdx <- 0 until numBr) {
      val wrbypass = tableBanks_wrbypasses(b)(brLogicIdx)
      val brPhyIdx = get_phy_br_idx(update_unhashedIdx, brLogicIdx)
      wrbypass.io.wen := io.update.mask(brLogicIdx) && update_reqBankOH(b)
      wrbypass.io.write_idx := get_bank_idx(update_idx)
      wrbypass.io.write_tag.foreach(_ := update_tag)
      val entry = Mux1H(UIntToOH(brPhyIdx, numBr), update_phyWCtr(b))
      wrbypass.io.write_data(0) := Cat(entry.up, entry.down)
    }
  }


  // Perf counter
  val perf_bankConflict: Bool =
    (0 until nBanks).map(b => tagBanks(b).io.w.req.valid && s0_bank_req_1h(b)).reduce(_ || _)
  XSPerfAccumulate(f"tage_table_bank_conflict", perf_bankConflict)

  for (i <- 0 until numBr) {
    for (b <- 0 until nBanks) {
      val wrbypass = tableBanks_wrbypasses(b)(i)
      XSPerfAccumulate(f"tage_table_bank_${b}_wrbypass_enq_$i",
        io.update.mask(i) && update_reqBankOH(b) && !wrbypass.io.hit)
      XSPerfAccumulate(f"tage_table_bank_${b}_wrbypass_hit_$i",
        io.update.mask(i) && update_reqBankOH(b) && wrbypass.io.hit)
    }
  }

  for (b <- 0 until nBanks) {
    val not_silent_update = update_phyNotSilentUpdate(b)
    XSPerfAccumulate(f"tage_table_bank_${b}_real_updates",
      io.update.mask.reduce(_ || _) && update_reqBankOH(b) && not_silent_update.reduce(_ || _))
    XSPerfAccumulate(f"tage_table_bank_${b}_silent_updates_eliminated",
      io.update.mask.reduce(_ || _) && update_reqBankOH(b) && !not_silent_update.reduce(_ || _))
  }

  XSPerfAccumulate("tage_table_hits", PopCount(io.resps.map(_.valid)))

  for (b <- 0 until nBanks) {
    XSPerfAccumulate(f"tage_table_bank_${b}_update_req", io.update.mask.reduce(_ || _) && update_reqBankOH(b))
    for (i <- 0 until numBr) {
      val li   = i
      val pidx = get_phy_br_idx(update_unhashedIdx, li)
      XSPerfAccumulate(f"tage_table_bank_${b}_br_li_${li}_updated",
        tagBanks(b).io.w.req.valid && tagBanks(b).io.w.req.bits.waymask.get(pidx))
      val pi = i
      XSPerfAccumulate(f"tage_table_bank_${b}_br_pi_${pi}_updated",
        tagBanks(b).io.w.req.valid && tagBanks(b).io.w.req.bits.waymask.get(pi))
    }
  }

}

abstract class BaseTage(implicit p: Parameters) extends BasePredictor with TageParams with BPUUtils {
}

class FakeTage(implicit p: Parameters) extends BaseTage {
  io.out <> 0.U.asTypeOf(DecoupledIO(new BasePredictorOutput))

  // io.s0_ready := true.B
  io.s1_ready := true.B
  io.s2_ready := true.B
}

@chiselName
class Tage(implicit p: Parameters) extends BaseTage {

  println(s"TAGE Total Bits: ${TageTotalBits / 8}B")

  val CAT: Vec[UInt] = RegInit(VecInit(Seq.fill(numBr)(0.U(log2Ceil(TageCATMAX).W))))

  val resp_meta = Wire(new TageMeta)
  override val meta_size = resp_meta.getWidth

  // Util function definition
  // This PriorityMux select the lowest value conf (highest conf) and valid T
  object BATageParallelPriorityMux {
    // hit, conf, T
    def apply[T <: Data](in: Seq[(Bool, UInt, T)]): T = {
      ParallelOperation(in,
        (a: (Bool, UInt, T), b: (Bool, UInt, T)) => (
          a._1 || b._1,
          // Left first if same confidence
          Mux(a._2 <= b._2 && a._1, a._2, b._2),
          Mux(a._2 <= b._2 && a._1, a._3, b._3)
        )
      )._3
    }

    def apply[T <: Data](sel: Bits, conf: Seq[UInt], in: Seq[T]): T = apply(in.indices.map(sel(_)), conf, in)

    def apply[T <: Data](sel: Seq[Bool], conf: Seq[UInt], in: Seq[T]): T = apply((sel, conf, in).zipped.map((_, _, _)))
  }

  // Class definition
  // TageTableInfo is a resp info from tagged component
  class TageTableInfo(implicit p: Parameters) extends XSBundle {
    val resp     = new TageResp
    val tableIdx = UInt(log2Ceil(TageNTables).W)
    val conf     = UInt(2.W) // BATAGE confidence, use 2bits encoding
  }

  // TaggedTables
  val taggedTables: Seq[TageTable] = TageTableInfos.zipWithIndex.map {
    case ((nRows, histLen, tagLen), idx) => {
      val t = Module(new TageTable(nRows, histLen, tagLen, idx))
      t.io.req.valid := io.s0_fire
      t.io.req.bits.pc := s0_pc
      t.io.req.bits.folded_hist := io.in.bits.folded_hist
      t.io.req.bits.ghist := io.in.bits.ghist
      t
    }
  }

  // BaseTable
  val baseTable = Module(new TageBTable)
  baseTable.io.s0_fire := io.s0_fire
  baseTable.io.s0_pc := s0_pc


  val tage_fh_info = taggedTables.map(_.getFoldedHistoryInfo).reduce(_ ++ _).toSet

  override def getFoldedHistoryInfo = Some(tage_fh_info)

  val s1_resps: Vec[Vec[ValidIO[TageResp]]] = VecInit(taggedTables.map(_.io.resps))

  val s1_provideds     = Wire(Vec(numBr, Bool()))
  val s1_providers     = Wire(Vec(numBr, UInt(log2Ceil(TageNTables).W)))
  val s1_tableHits     = Wire(Vec(numBr, Vec(TageNTables, Bool())))
  val s1_taggedResps   = Wire(Vec(numBr, Vec(TageNTables, new TageResp)))
  val s1_providerResps = Wire(Vec(numBr, new TageResp))
  val s1_tageTakens    = Wire(Vec(numBr, Bool()))
  val s1_basecnts      = Wire(Vec(numBr, UInt(2.W)))

  val s2_resps              = RegEnable(s1_resps, io.s1_fire)
  val s2_provideds          = RegEnable(s1_provideds, io.s1_fire)
  val s2_providers          = RegEnable(s1_providers, io.s1_fire)
  val s2_tableHits          = RegEnable(s1_tableHits, io.s1_fire)
  val s2_taggedResps        = RegEnable(s1_taggedResps, io.s1_fire)
  val s2_tageTakens         = RegEnable(s1_tageTakens, io.s1_fire)
  val s2_basecnts           = RegEnable(s1_basecnts, io.s1_fire)
  val s2_nextProviders      = Wire(Vec(numBr, UInt(log2Ceil(TageNTables).W)))
  val s2_nextProvidersValid = Wire(Vec(numBr, Bool()))

  io.out := io.in.bits.resp_in(0)
  io.out.last_stage_meta := resp_meta.asUInt


  //---------------- Predict logics below ------------------//
  for (i <- 0 until numBr) {

    val resp: Vec[ValidIO[TageResp]] = VecInit(s1_resps.map(_(i)))
    val s1_structuredResp            = resp.zipWithIndex.map { case (r, idx) => {
      val tableInfo = Wire(new TageTableInfo)
      tableInfo.resp := r.bits
      tableInfo.conf := r.bits.conf()
      tableInfo.tableIdx := idx.U(log2Ceil(TageNTables).W)
      (r.valid, tableInfo.conf, tableInfo)
    }
    }

    // Stage 1
    // Select provider
    val providerInfo = BATageParallelPriorityMux(s1_structuredResp.reverse)
    val provided     = s1_structuredResp.map(_._1).reduce(_ || _)
    s1_provideds(i) := provided
    s1_providers(i) := providerInfo.tableIdx
    s1_providerResps(i) := providerInfo.resp
    s1_taggedResps(i) := resp.map(r => r.bits)
    s1_tableHits(i) := resp.map(r => r.valid)

    val s1_bimCtr = baseTable.io.s1_cnt(i)
    s1_tageTakens(i) :=
      Mux(!provided,
        s1_bimCtr(1),
        providerInfo.resp.taken()
      )
    s1_basecnts(i) := s1_bimCtr

    // Stage 2
    // Calculate next longest history that hits
    val s2_structuredResp = VecInit(s2_resps.map(_(i))).zipWithIndex.map {
      case (r, idx) => {
        (r.valid && idx.asUInt < s2_providers(i), idx.U(log2Ceil(TageNTables).W))
      }
    }
    s2_nextProviders(i) := ParallelPriorityMux(s2_structuredResp.reverse)
    s2_nextProvidersValid(i) := s2_tableHits(i)(s2_nextProviders(i))

    // Stage 3
    resp_meta.providers(i).valid := RegEnable(s2_provideds(i), io.s2_fire)
    resp_meta.providers(i).bits := RegEnable(s2_providers(i), io.s2_fire)
    resp_meta.tagResps(i) := RegEnable(s2_taggedResps(i), io.s2_fire)
    resp_meta.tagHits(i) := RegEnable(s2_tableHits(i), io.s2_fire)
    resp_meta.takens(i) := RegEnable(s2_tageTakens(i), io.s2_fire)
    resp_meta.basecnts(i) := RegEnable(s2_basecnts(i), io.s2_fire)
    resp_meta.nextProviders(i).valid := RegEnable(s2_nextProvidersValid(i), io.s2_fire)
    resp_meta.nextProviders(i).bits := RegEnable(s2_nextProviders(i), io.s2_fire)
    resp_meta.pred_cycle.foreach(_ := RegEnable(GTimer(), io.s2_fire))

    when(io.ctrl.tage_enable) {
      io.out.s2.full_pred.br_taken_mask(i) := s2_tageTakens(i)
    }
  }

  //---------------- Update logics below ------------------//
  // These signals will be directly connect to TageTables and Basetable
  val update_valid                             = io.update.valid
  val update_data                              = io.update.bits
  val update_misPred                           = update_data.mispred_mask
  val update_meta                              = update_data.meta.asTypeOf(new TageMeta)
  val update_tageResp                          = update_meta.tagResps
  val update_condition                         = VecInit((0 until numBr).map(w =>
    update_data.ftb_entry.brValids(w) && update_valid && !update_data.ftb_entry.always_taken(w) &&
      !(PriorityEncoder(update_data.br_taken_mask) < w.U)))
  val update_foldedHistory: AllFoldedHistories = update_data.spec_info.folded_hist
  // These signal is generated below
  val update_mask                              = WireInit(0.U.asTypeOf(Vec(numBr, Vec(TageNTables, Bool()))))
  val update_takens                            = Wire(Vec(numBr, Vec(TageNTables, Bool())))
  val update_allocMask                         = WireInit(0.U.asTypeOf(Vec(numBr, Vec(TageNTables, Bool()))))
  val update_decayMask                         = WireInit(0.U.asTypeOf(Vec(numBr, Vec(TageNTables, Bool()))))
  val update_baseCnt                           = Wire(Vec(numBr, UInt(2.W)))
  val update_baseUpdateValid                   = WireInit(0.U.asTypeOf(Vec(numBr, Bool())))
  val update_baseTakens                        = Wire(Vec(numBr, Bool()))

  // Connect to submodules
  for (i <- 0 until numBr) {
    for (compIdx <- 0 until TageNTables) {
      taggedTables(compIdx).io.update.mask(i) := RegNext(update_mask(i)(compIdx))
      taggedTables(compIdx).io.update.takens(i) := RegNext(update_takens(i)(compIdx))
      taggedTables(compIdx).io.update.decay(i) := RegNext(update_decayMask(i)(compIdx))
      taggedTables(compIdx).io.update.alloc(i) := RegNext(update_allocMask(i)(compIdx))
      taggedTables(compIdx).io.update.old_ctr_up(i) := RegNext(update_tageResp(i)(compIdx).ctr_up)
      taggedTables(compIdx).io.update.old_ctr_down(i) := RegNext(update_tageResp(i)(compIdx).ctr_down)

      // use fetch pc instead of instruction pc
      taggedTables(compIdx).io.update.pc := RegNext(update_data.pc)
      taggedTables(compIdx).io.update.folded_hist := RegNext(update_foldedHistory)
      taggedTables(compIdx).io.update.ghist := RegNext(io.update.bits.ghist)
    }
  }
  baseTable.io.update_mask := RegNext(update_baseUpdateValid)
  baseTable.io.update_cnt := RegNext(update_baseCnt)
  baseTable.io.update_pc := RegNext(update_data.pc)
  baseTable.io.update_takens := RegNext(update_baseTakens)

  // Generate the update signals
  // NOTICE: Currently all the allocation and decaying decisions are made based on OLD INFO
  for (i <- 0 until numBr) {
    // Use no update_ prefix to distinguish internal signals
    val hasUpdate         = update_condition(i)
    val misPred           = update_data.mispred_mask(i)
    val taken             = hasUpdate && update_data.br_taken_mask(i)
    val tableHitMask      = update_meta.tagHits(i)
    val providerValid     = update_meta.providers(i).valid
    val providerIdx       = update_meta.providers(i).bits
    val providerResp      = update_tageResp(i)(providerIdx)
    val nextProviderValid = update_meta.providers(i).valid
    val nextProviderIdx   = update_meta.providers(i).bits
    val nextProviderResp  = update_tageResp(i)(nextProviderIdx)

    // Some conditions
    val nextProviderUnconf = nextProviderValid && nextProviderResp.unconf()
    val nextProviderWrong  = nextProviderValid && taken =/= nextProviderResp.taken()
    val providerUnconf     = providerResp.unconf()


    // Update provider and longer history component
    update_takens(i) := DontCare
    when(hasUpdate) {
      for (idx <- 0 until TageNTables) {
        when(idx.asUInt > providerIdx && tableHitMask(idx)) {
          // Always update longer history hit component
          // They are not selected as provider most likely because is low conf
          update_mask(i)(idx) := true.B
          update_takens(i)(idx) := taken
        }.elsewhen(idx.asUInt === providerIdx && providerValid) {
          when(nextProviderUnconf || nextProviderWrong || providerUnconf) {
            // Update provider only under this condition
            update_mask(i)(idx) := true.B
            update_takens(i)(idx) := taken
          }.otherwise {
            // Next longer hit comp can predict well
            // Entry in this provider is probably useless
            // So decay
            update_mask(i)(idx) := true.B
            update_decayMask(i)(idx) := true.B
          }
        }.elsewhen(idx.asUInt === nextProviderIdx && nextProviderValid && providerUnconf) {
          // Update next longer hit component when current provider is not confidence
          update_mask(i)(idx) := true.B
          update_takens(i)(idx) := taken
        }
      }
    }

    // Update base table if used base table to predict
    update_baseUpdateValid(i) := hasUpdate && !providerValid
    update_baseCnt(i) := update_meta.basecnts(i)
    update_baseTakens(i) := taken


    // Allocation logic
    // CAT probabilty
    val catLFSR: UInt  = LFSR64()(log2Ceil(TageMINAP) - 1, 0)
    val catAllow       = catLFSR >= (CAT(i) >> (log2Ceil(TageCATMAX) - log2Ceil(TageMINAP))).asUInt
    // Allocate when mispredict and CAT allowed
    val needToAllocate = hasUpdate && misPred && catAllow

    // Maintain CAT
    val mhcSum: UInt = PopCount(update_tageResp(i).map(r => r.isMHC()))
    // Perform a saturating update
    when(needToAllocate) {
      when(CAT(i) > TageCATMAX.asUInt - BatageCATR1.asUInt) {
        // First check overflow
        when(mhcSum.orR) {
          CAT(i) := CAT(i) - BatageCATR2.asUInt * mhcSum + BatageCATR1.asUInt
        }.otherwise {
          CAT(i) := TageCATMAX.asUInt
        }
      }.elsewhen(CAT(i) + BatageCATR1.asUInt < mhcSum * BatageCATR2.asUInt) {
        // Underflow
        CAT(i) := 0.U
      }.otherwise {
        // Normal
        CAT(i) := CAT(i) - BatageCATR2.asUInt * mhcSum + BatageCATR1.asUInt
      }
    }

    val longerHistoryTableMask = ~(
      LowerMask(UIntToOH(providerIdx), TageNTables)
        & Fill(TageNTables, providerValid.asUInt)
      )
    // Not high confidence is allocatable
    val allocatableMask        = VecInit(update_tageResp(i).map(r => r.unconf())).asUInt & longerHistoryTableMask.asUInt
    val allocatable            = allocatableMask.asBools.reduce(_ | _)
    val allocateIdx: UInt      = Mux(allocatable, PriorityEncoder(allocatableMask), TageNTables.asUInt)


    when(needToAllocate) {
      for (idx <- 0 until TageNTables) {
        when(idx.asUInt < allocateIdx) {
          // Do decaying
          // With probability 1
          update_mask(i)(allocateIdx) := true.B
          update_decayMask(i)(allocateIdx) := true.B
        }.elsewhen(idx.asUInt === allocateIdx) {
          // Allocate it
          update_mask(i)(allocateIdx) := true.B
          update_takens(i)(allocateIdx) := taken
          update_allocMask(i)(allocateIdx) := true.B
        }
      }
    }
    XSPerfAccumulate(s"tage_bank_${i}_mispred", hasUpdate && misPred)
  }



  // all should be ready for req
  io.s1_ready := taggedTables.map(_.io.req.ready).reduce(_ && _)
  assert(io.s1_ready)

  // Debug and perf info
  def pred_perf(name: String, cnt: UInt) = XSPerfAccumulate(s"${name}_at_pred", cnt)

  def commit_perf(name: String, cnt: UInt) = XSPerfAccumulate(s"${name}_at_commit", cnt)

  def tage_perf(name: String, pred_cnt: UInt, commit_cnt: UInt) = {
    pred_perf(name, pred_cnt)
    commit_perf(name, commit_cnt)
  }

  for (b <- 0 until TageBanks) {
    val updateProvided = update_meta.providers(b).valid
    val updateProvider = update_meta.providers(b).bits
    for (i <- 0 until TageNTables) {
      val pred_i_provided   =
        s2_provideds(b) && s2_providers(b) === i.U
      val commit_i_provided =
        updateProvided && updateProvider === i.U && update_condition(b)
      tage_perf(
        s"bank_${b}_tage_table_${i}_provided",
        PopCount(pred_i_provided),
        PopCount(commit_i_provided)
      )
    }
    tage_perf(
      s"bank_${b}_tage_use_bim",
      PopCount(!s2_provideds(b)),
      PopCount(!updateProvided && update_condition(b))
    )

    tage_perf(
      s"bank_${b}_tage_provided",
      PopCount(s2_provideds(b)),
      PopCount(updateProvided && update_condition(b))
    )
  }

  // Debug signal
  val debug_s0_pc = dontTouch(s0_pc)
  val debug_s1_pc = dontTouch(RegEnable(s0_pc, io.s0_fire))
  val debug_s2_pc = dontTouch(RegEnable(debug_s1_pc, io.s1_fire))

}


class Tage_SC(implicit p: Parameters) extends Tage with HasSC {}
