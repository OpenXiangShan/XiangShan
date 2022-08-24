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
import utils._
import xiangshan._
import chisel3.experimental.chiselName
import xiangshan.cache.mmu.CAMTemplate

import scala.{Tuple2 => &}

trait MicroBTBParams extends HasXSParameter with HasBPUParameter {
  val numEntries = UbtbSize
  val ftPredBits = 1
  val ftPredSize = FtbSize
  val ftPredDecayPeriod = 2048 // each time decay an entire row
  def ubtbAddr = new TableAddr(log2Up(numEntries), 1)
}

class NewMicroBTBEntry(implicit p: Parameters) extends XSBundle with MicroBTBParams {
  // val valid = Bool()
  val nextAddr = UInt(VAddrBits.W) // could be target or fallThrough
  val cfiOffset = UInt(log2Ceil(PredictWidth).W)
  val taken = Bool()
  val takenOnBr = Bool()
  val brNumOH = UInt((numBr+1).W) // used to speculative update histPtr

  def fromBpuUpdateBundle(u: BranchPredictionUpdate) = {
    // this.valid := true.B
    this.nextAddr := u.full_target
    this.cfiOffset := u.cfi_idx.bits
    this.taken := u.cfi_idx.valid
    this.takenOnBr := u.br_taken_mask.reduce(_||_)
    this.brNumOH := 
      VecInit(!u.ftb_entry.brValids.reduce(_||_) +:
        (0 until numBr).map(i =>
          u.ftb_entry.brValids(i) &&
          !u.br_taken_mask.take(i).reduceOption(_||_).getOrElse(false.B) && // no brs taken in front it
          (u.br_taken_mask(i) || !u.ftb_entry.brValids.drop(i+1).reduceOption(_||_).getOrElse(false.B)) // no brs behind it
        )
      ).asUInt
  }
}

@chiselName
class MicroBTB(implicit p: Parameters) extends BasePredictor
  with MicroBTBParams with HasPerfEvents
{
  

  class MicroBTBOutMeta extends XSBundle {
    val ftPred = UInt(ftPredBits.W)
  }

  class FallThruPred extends XSModule with MicroBTBParams {
    val io = IO(new Bundle {
      val ren = Input(Bool())
      val ridx = Input(UInt(log2Ceil(ftPredSize).W))
      val rdata = Output(UInt(ftPredBits.W))

      val wen = Input(Bool())
      val widx = Input(UInt(log2Ceil(ftPredSize).W))
      val wdata = Input(UInt(ftPredBits.W))
    })
    val nRows = ftPredSize

    val doing_reset = RegInit(true.B)
    val reset_idx = RegInit(0.U(log2Ceil(nRows).W))
    val reset_wdata = WireInit(0.U(ftPredBits.W))
    reset_idx := reset_idx + doing_reset
    when (reset_idx === (nRows-1).U) { doing_reset := false.B }

    val decay_timer = RegInit(0.U(log2Ceil(ftPredDecayPeriod).W))
    decay_timer := decay_timer + 1.U
    val doing_decay = RegNext(decay_timer.andR())
    val decay_wdata = reset_wdata // TODO: gradually decay
    val decay_idx = RegInit(0.U(log2Ceil(nRows).W))
    decay_idx := decay_idx + doing_decay

    val data = Module(new SyncDataModuleTemplate(Bool(), nRows, 1, 1, "UbtbFallThruPred",
      concatData=false, perReadPortBypassEnable=Some(Seq(false))))

    data.io.raddr(0) := io.ridx
    io.rdata := data.io.rdata(0)

    
    val wdata = Mux1H(Seq(
      (doing_reset, reset_wdata),
      (!doing_reset && doing_decay, decay_wdata),
      (!(doing_reset || doing_decay) && io.wen, io.wdata)
    ))
    val widx = Mux1H(Seq(
      (doing_reset, reset_idx),
      (!doing_reset && doing_decay, decay_idx),
      (!(doing_reset || doing_decay) && io.wen, io.widx)
    ))
    val wen = io.wen || doing_decay || doing_reset

    data.io.wen(0) := wen
    data.io.waddr(0) := widx
    data.io.wdata(0) := wdata

    XSPerfAccumulate("num_decays", doing_decay)
    XSPerfAccumulate("num_writes", io.wen)

  }



  override val meta_size = WireInit(0.U.asTypeOf(new MicroBTBOutMeta)).getWidth // TODO: ReadResp shouldn't save useless members
  require(UbtbGHRLength <= log2Ceil(UbtbSize))
  
  def getIdx(pc: UInt) = pc(log2Ceil(numEntries)+instOffsetBits-1, instOffsetBits)
  def getFtPredIdx(pc: UInt) = {
    // require(pc.getWidth >= instOffsetBits + 2 * log2Ceil(ftPredSize))
    // hash twice as more bits into idx
    (0 until 2).map {i =>
      (pc >> (instOffsetBits + i * log2Ceil(ftPredSize)))(log2Ceil(ftPredSize)-1,0)
    }.reduce(_^_)
  }
  val fh_info = (UbtbGHRLength, log2Ceil(UbtbSize))
  println(s"ubtb fh info ${fh_info}")
  def get_ghist_from_fh(afh: AllFoldedHistories) = afh.getHistWithInfo(fh_info)

  val s0_data_ridx_dup = dup_wire(UInt(log2Ceil(UbtbSize).W))
  for (s0_data_ridx & s0_pc & fh <- s0_data_ridx_dup zip s0_pc_dup zip io.in.bits.folded_hist)
    s0_data_ridx := getIdx(s0_pc) ^ get_ghist_from_fh(fh).folded_hist
  val dataMem = Module(new SRAMTemplate(new NewMicroBTBEntry, set=numEntries, way=1, shouldReset=false, holdRead=true, singlePort=true))
  val fallThruPredRAM = Module(new FallThruPred)
  val validArray = RegInit(0.U.asTypeOf(Vec(numEntries, Bool())))


  dataMem.io.r.req.valid := io.s0_fire(dupForUbtb)
  dataMem.io.r.req.bits.setIdx := s0_data_ridx_dup(dupForUbtb)

  fallThruPredRAM.io.ren := io.s0_fire(dupForUbtb)
  fallThruPredRAM.io.ridx := getFtPredIdx(s0_pc_dup(dupForUbtb))
  val shouldNotFallThru = fallThruPredRAM.io.rdata.andR() // only when confident should we not fallThru

  val update_valid = Wire(Bool())
  val pred_may_invalid_by_update_dup = dup_seq(RegInit(false.B))
  when (update_valid) {
    pred_may_invalid_by_update_dup.map(_ := true.B)
  }.elsewhen (io.s1_fire(dupForUbtb)) {
    pred_may_invalid_by_update_dup.map(_ := false.B)
  }
  



  // io.out
  val s1_data_ridx_dup = s0_data_ridx_dup.zip(io.s0_fire).map {case (ridx, f) => RegEnable(ridx, f)}
  // only for timing purposes
  def s0_select_bits = 3
  def s1_select_bits = 5
  require(1 << (s0_select_bits + s1_select_bits) == numEntries)
  // val resp_valid = RegEnable(validArray(s0_data_ridx), io.s0_fire && !update_valid)
  // select using highest bits of s0_ridx
  val s0_selected_valid_bits_dup = s0_data_ridx_dup.map(ridx => VecInit((0 until (1 << s1_select_bits)).map {i =>
    validArray(Cat(ridx(log2Ceil(UbtbSize)-1, log2Ceil(UbtbSize)-s0_select_bits), i.U(s1_select_bits.W)))
  }))
  val s1_selected_valid_bits_dup = s0_selected_valid_bits_dup.zip(io.s0_fire).map {case (s0_vb, f) => RegEnable(s0_vb, f)}
  // select using the lower bits of s1_ridx
  val resp_valid_dup = s1_selected_valid_bits_dup.zip(s1_data_ridx_dup).map {case (s1_vb, ridx) => s1_vb(ridx(s1_select_bits-1,0))}


  val outMeta = Wire(new MicroBTBOutMeta)

  XSDebug(p"uBTB entry, read_pc=${Hexadecimal(s0_pc_dup(dupForUbtb))}\n")

  val ubtb_enable_dup = dup_seq(RegNext(io.ctrl.ubtb_enable))

  for (mp & invalid_by_upd & ubtb_enable & s1_pc & resp_valid <-
    io.out.s1.minimal_pred zip pred_may_invalid_by_update_dup zip ubtb_enable_dup zip s1_pc_dup zip resp_valid_dup) {
      mp.fromMicroBTBEntry(
        resp_valid && shouldNotFallThru && !invalid_by_upd && ubtb_enable,
        dataMem.io.r.resp.data(0), s1_pc
        ) // invalid when update
    }
  io.out.s1.is_minimal.map(_ := true.B)

  outMeta.ftPred := fallThruPredRAM.io.rdata
  io.out.last_stage_meta := RegEnable(RegEnable(outMeta.asUInt, io.s1_fire(dupForUbtb)), io.s2_fire(dupForUbtb))

  // Update logic
  val io_update = io.update(dupForUbtb)
  val update_mispred = io_update.bits.mispred_mask.reduce(_||_)
  val update_redirected = io_update.bits.from_stage === BP_S2
  val update = RegNext(io_update.bits)
  val u_valid = RegNext(io_update.valid && (update_mispred || update_redirected))
  update_valid := u_valid
  val u_pc = update.pc
  val u_br_taken_mask = update.br_taken_mask
  val u_meta = update.meta.asTypeOf(new MicroBTBOutMeta)
  val u_data = Wire(new NewMicroBTBEntry)
  u_data.fromBpuUpdateBundle(update)
  val u_idx = getIdx(update.pc) ^ get_ghist_from_fh(update.spec_info.folded_hist).folded_hist

  val u_ftPred = u_meta.ftPred.andR
  val u_ftMisPred = u_ftPred ^ update.pred_hit

  dataMem.io.w.apply(u_valid, u_data, u_idx, 1.U(1.W))
  when (u_valid) {
    validArray(u_idx) := true.B
  }

  fallThruPredRAM.io.wen := u_ftMisPred && RegNext(io_update.valid)
  fallThruPredRAM.io.widx := getFtPredIdx(u_pc)
  fallThruPredRAM.io.wdata := satUpdate(u_meta.ftPred, ftPredBits, true.B)


  // XSDebug("req_v=%b, req_pc=%x, hit=%b\n", io.s1_fire, s1_pc, bank.read_hit)
  XSDebug("target=%x\n", io.out.s1.target(dupForUbtb))

  XSDebug(u_valid, "[update]Update from ftq\n")
  XSDebug(u_valid, "[update]update_pc=%x, tag=%x\n", u_pc, ubtbAddr.getTag(u_pc))
  XSDebug(u_valid, "[update]taken_mask=%b, brValids=%b, jmpValid=%b\n",
    u_br_taken_mask.asUInt, update.ftb_entry.brValids.asUInt, update.ftb_entry.jmpValid)

  // XSPerfAccumulate("ubtb_read_hits", RegNext(io.s1_fire) && bank.read_hit)
  // XSPerfAccumulate("ubtb_read_misses", RegNext(io.s1_fire) && !bank.read_hit)

  // XSPerfAccumulate("ubtb_commit_hits", u_valid && u_meta.hit)
  // XSPerfAccumulate("ubtb_commit_misses", u_valid && !u_meta.hit)

  XSPerfAccumulate("ubtb_update_on_mispred", u_valid && RegNext(update_mispred))
  XSPerfAccumulate("ubtb_update_on_redirected_by_s2", u_valid && RegNext(update_redirected))
  XSPerfAccumulate("ubtb_update_eliminated", io_update.valid && !(update_mispred || update_redirected))

  XSPerfAccumulate("ubtb_resp_invalid_by_update", io.s1_fire(dupForUbtb) && pred_may_invalid_by_update_dup(dupForUbtb) && shouldNotFallThru)
  XSPerfAccumulate("ubtb_resp_invalid_by_ftpred", io.s1_fire(dupForUbtb) && !pred_may_invalid_by_update_dup(dupForUbtb) && !shouldNotFallThru)

  XSPerfAccumulate("ubtb_update_ft_mispred", RegNext(io_update.valid) && u_ftMisPred)
  XSPerfAccumulate("ubtb_update_ft_pred_correct", RegNext(io_update.valid) && !u_ftMisPred)

  override val perfEvents = Seq(
    // ("ubtb_commit_hit       ", u_valid &&  u_meta.hit),
    // ("ubtb_commit_miss      ", u_valid && !u_meta.hit),
  )
  generatePerfEvent()
}
