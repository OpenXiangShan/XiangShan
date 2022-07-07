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
import chisel3.experimental.chiselName
import chisel3.util._
import huancun.utils.SRAMTemplate
import utils._
import xiangshan._
import huancun.mbist.MBISTPipeline.placePipelines

trait MicroBTBParams extends HasXSParameter with HasBPUParameter {
  val numEntries = UbtbSize
  val ftPredBits = 1
  val ftPredSize = FtbSize
  val ftPredFoldWidth = 8
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
    assert(!u.is_minimal)
    this.nextAddr := u.getTarget
    this.cfiOffset := u.cfiIndex.bits
    this.taken := u.taken
    this.takenOnBr := (u.lastBrPosOH.tail zip u.full_pred.br_taken_mask).map{case (a, b) => a && b}.reduce(_||_)
    this.brNumOH := u.lastBrPosOH.asUInt()
  }
}

@chiselName
class MicroBTB(parentName:String = "Unknown")(implicit p: Parameters) extends BasePredictor(parentName)(p)
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
    val nRows = ftPredSize / ftPredFoldWidth

    val doing_reset = RegInit(true.B)
    val reset_idx = RegInit(0.U(log2Ceil(nRows).W))
    val reset_wdata = WireInit(0.U.asTypeOf(Vec(ftPredFoldWidth, UInt(ftPredBits.W))))
    reset_idx := reset_idx + doing_reset
    when (reset_idx === (nRows-1).U) { doing_reset := false.B }

    val decay_timer = RegInit(0.U(log2Ceil(ftPredDecayPeriod).W))
    decay_timer := decay_timer + 1.U
    val doing_decay = RegNext(decay_timer.andR())
    val decay_wdata = reset_wdata // TODO: gradually decay
    val decay_idx = RegInit(0.U(log2Ceil(nRows).W))
    decay_idx := decay_idx + doing_decay

    val ram = Module(new SRAMTemplate(UInt(ftPredBits.W), set=nRows, way=ftPredFoldWidth, shouldReset=false, singlePort=true, parentName = parentName + "ram_"))
    ram.io.r.req.valid := io.ren
    ram.io.r.req.bits.setIdx := io.ridx >> log2Ceil(ftPredFoldWidth)

    val ram_rdata = ram.io.r.resp.data
    val ridx_reg = RegEnable(io.ridx, io.ren)
    val r_way = ridx_reg(log2Ceil(ftPredFoldWidth)-1, 0)
    io.rdata := ram_rdata(r_way)

    val wdata = Mux1H(Seq(
      (doing_reset, reset_wdata),
      (!doing_reset && doing_decay, decay_wdata),
      (!(doing_reset || doing_decay) && io.wen, VecInit((0 until ftPredFoldWidth).map(_=>io.wdata)))
    ))
    val widx = Mux1H(Seq(
      (doing_reset, reset_idx),
      (!doing_reset && doing_decay, decay_idx),
      (!(doing_reset || doing_decay) && io.wen, io.widx >> log2Ceil(ftPredFoldWidth))
    ))
    val waymask = UIntToOH(io.widx(log2Ceil(ftPredFoldWidth)-1, 0)) | Fill(ftPredFoldWidth, (doing_reset || doing_decay).asTypeOf(UInt(1.W))).asUInt
    val ram_wen = io.wen || doing_decay || doing_reset

    ram.io.w.apply(ram_wen, wdata, widx, waymask)

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

  val s0_data_ridx = Wire(UInt(log2Ceil(UbtbSize).W))
  s0_data_ridx := getIdx(s0_pc) ^ get_ghist_from_fh(io.in.bits.folded_hist).folded_hist
  val dataMem = Module(new SRAMTemplate(new NewMicroBTBEntry, set=numEntries, way=1, shouldReset=false, singlePort=true, parentName = parentName + "dataMem_"))
  val fallThruPredRAM = Module(new FallThruPred)
  val validArray = RegInit(0.U.asTypeOf(Vec(numEntries, Bool())))


  dataMem.io.r.req.valid := io.s0_fire
  dataMem.io.r.req.bits.setIdx := s0_data_ridx

  fallThruPredRAM.io.ren := io.s0_fire
  fallThruPredRAM.io.ridx := getFtPredIdx(s0_pc)
  val shouldNotFallThru = fallThruPredRAM.io.rdata.andR() // only when confident should we not fallThru

  val update_valid = Wire(Bool())
  val pred_may_invalid_by_update = RegInit(false.B)
  when (update_valid) {
    pred_may_invalid_by_update := true.B
  }.elsewhen (io.s1_fire) {
    pred_may_invalid_by_update := false.B
  }




  // io.out.resp
  val s1_data_ridx = RegEnable(s0_data_ridx, io.s0_fire)
  // only for timing purposes
  def s0_select_bits = 3
  def s1_select_bits = 5
  require(1 << (s0_select_bits + s1_select_bits) == numEntries)
  // val resp_valid = RegEnable(validArray(s0_data_ridx), io.s0_fire && !update_valid)
  // select using highest bits of s0_ridx
  val s0_selected_valid_bits = VecInit((0 until (1 << s1_select_bits)).map {i =>
    validArray(Cat(s0_data_ridx(log2Ceil(UbtbSize)-1, log2Ceil(UbtbSize)-s0_select_bits), i.U(s1_select_bits.W)))
  })
  val s1_selected_valid_bits = RegEnable(s0_selected_valid_bits, io.s0_fire)
  // select using the lower bits of s1_ridx
  val resp_valid = s1_selected_valid_bits(s1_data_ridx(s1_select_bits-1,0))


  val outMeta = Wire(new MicroBTBOutMeta)

  XSDebug(p"uBTB entry, read_pc=${Hexadecimal(s0_pc)}\n")

  io.out.resp.s1.minimal_pred.fromMicroBTBEntry(
    resp_valid && shouldNotFallThru && !pred_may_invalid_by_update && io.ctrl.ubtb_enable,
    dataMem.io.r.resp.data(0), s1_pc
  ) // invalid when update
  io.out.resp.s1.is_minimal := true.B

  outMeta.ftPred := fallThruPredRAM.io.rdata
  io.out.last_stage_meta := RegEnable(RegEnable(outMeta.asUInt, io.s1_fire), io.s2_fire)

  // Update logic
  val update_mispred = io.update.bits.mispred_mask.reduce(_||_)
  val update_redirected = io.update.bits.from_stage === BP_S2
  val update = RegNext(io.update.bits)
  val u_valid = RegNext(io.update.valid && (update_mispred || update_redirected))
  update_valid := u_valid
  val u_pc = update.pc
  val u_br_taken_mask = update.full_pred.br_taken_mask
  val u_meta = update.meta.asTypeOf(new MicroBTBOutMeta)
  val u_data = Wire(new NewMicroBTBEntry)
  u_data.fromBpuUpdateBundle(update)
  val u_idx = getIdx(update.pc) ^ get_ghist_from_fh(update.folded_hist).folded_hist

  val u_ftPred = u_meta.ftPred.andR
  val u_ftMisPred = u_ftPred ^ update.pred_hit

  dataMem.io.w.apply(u_valid, u_data, u_idx, 1.U(1.W))
  when (u_valid) {
    validArray(u_idx) := true.B
  }

  fallThruPredRAM.io.wen := u_ftMisPred && RegNext(io.update.valid)
  fallThruPredRAM.io.widx := getFtPredIdx(u_pc)
  fallThruPredRAM.io.wdata := satUpdate(u_meta.ftPred, ftPredBits, true.B)

  val (uBTBMbistPipelineSram,uBTBMbistPipelineRf,uBTBMbistPipelineSramRepair,uBTBMbistPipelineRfRepair) = placePipelines(level = 1,infoName = s"MBISTPipeline_uBTB")
  // XSDebug("req_v=%b, req_pc=%x, hit=%b\n", io.s1_fire, s1_pc, bank.read_hit)
  XSDebug("target=%x\n", io.out.resp.s1.getTarget)

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
  XSPerfAccumulate("ubtb_update_eliminated", io.update.valid && !(update_mispred || update_redirected))

  XSPerfAccumulate("ubtb_resp_invalid_by_update", io.s1_fire && pred_may_invalid_by_update && shouldNotFallThru)
  XSPerfAccumulate("ubtb_resp_invalid_by_ftpred", io.s1_fire && !pred_may_invalid_by_update && !shouldNotFallThru)

  XSPerfAccumulate("ubtb_update_ft_mispred", RegNext(io.update.valid) && u_ftMisPred)
  XSPerfAccumulate("ubtb_update_ft_pred_correct", RegNext(io.update.valid) && !u_ftMisPred)

  override val perfEvents = Seq(
    // ("ubtb_commit_hit       ", u_valid &&  u_meta.hit),
    // ("ubtb_commit_miss      ", u_valid && !u_meta.hit),
  )
  generatePerfEvent()
}
