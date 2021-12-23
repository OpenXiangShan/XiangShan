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

trait MicroBTBParams extends HasXSParameter with HasBPUParameter {
  val numEntries = UbtbSize
  def ubtbAddr = new TableAddr(log2Up(numEntries), 1)
}

class NewMicroBTBEntry(implicit p: Parameters) extends XSBundle with MicroBTBParams {
  // val valid = Bool()
  val nextAddr = UInt(VAddrBits.W) // could be target or fallThrough
  val cfiOffset = UInt(log2Ceil(PredictWidth).W)
  val taken = Bool()
  val takenOnBr = Bool()
  val brNumOH = UInt((numBr+1).W) // used to speculative update histPtr
  val oversize = Bool()

  def fromBpuUpdateBundle(u: BranchPredictionUpdate) = {
    // this.valid := true.B
    assert(!u.is_minimal)
    this.nextAddr := u.getTarget
    this.cfiOffset := u.cfiIndex.bits
    this.taken := u.taken
    this.takenOnBr := (u.lastBrPosOH.tail zip u.full_pred.br_taken_mask).map{case (a, b) => a && b}.reduce(_||_)
    this.brNumOH := u.lastBrPosOH.asUInt()
    this.oversize := u.full_pred.oversize && (!u.taken || u.taken && u.cfiIndex.bits.andR)
  }
}

// class MicroBTBEntry(implicit p: Parameters) extends XSBundle with MicroBTBParams {
//   val valid = Bool()
//   val tag = UInt(tagSize.W)
//   val slot_valids = Vec(totalSlot, Bool())
//   val offsets = Vec(totalSlot, UInt(log2Ceil(PredictWidth).W))
//   val targets = Vec(totalSlot, UInt(VAddrBits.W))
//   val fallThroughAddr = UInt(VAddrBits.W)
//   val oversize = Bool()
//   val last_is_br = Bool()
//   def brValids = VecInit(slot_valids.init :+ (slot_valids.last && last_is_br))
//   def jmpValid = VecInit(slot_valids.last && !last_is_br)
//   def fromBpuUpdateBundle(u: BranchPredictionUpdate) = {
//     this.valid := true.B
//     this.tag := ubtbAddr.getTag(u.pc)
//     this.slot_valids := VecInit(u.ftb_entry.brSlots.map(_.valid) :+ u.ftb_entry.tailSlot.valid)
//     this.offsets := u.ftb_entry.getOffsetVec
//     this.targets := u.ftb_entry.getTargetVec(u.pc)
//     this.fallThroughAddr := u.ftb_entry.getFallThrough(u.pc)
//     this.oversize := u.ftb_entry.oversize
//     this.last_is_br := u.ftb_entry.tailSlot.sharing
//   }
// }

@chiselName
class MicroBTB(implicit p: Parameters) extends BasePredictor
  with MicroBTBParams with HasPerfEvents
{
  

  class MicroBTBOutMeta extends XSBundle {
    val hit = Bool()
  }



  override val meta_size = WireInit(0.U.asTypeOf(new MicroBTBOutMeta)).getWidth // TODO: ReadResp shouldn't save useless members
  
  def getIdx(pc: UInt) = pc(log2Ceil(numEntries)+instOffsetBits-1, instOffsetBits)

  val fh_info = (UbtbGHRLength, log2Ceil(UbtbSize))
  println(s"ubtb fh info ${fh_info}")
  def get_ghist_from_fh(afh: AllFoldedHistories) = afh.getHistWithInfo(fh_info)
  val s0_ridx = getIdx(s0_pc) ^ get_ghist_from_fh(io.in.bits.folded_hist).folded_hist
  val dataMem = Module(new SRAMTemplate(new NewMicroBTBEntry, set=numEntries, way=1, shouldReset=false, holdRead=true, singlePort=false, bypassWrite=true))
  dataMem.io.r.req.valid := io.s0_fire
  dataMem.io.r.req.bits.setIdx := s0_ridx
  val validArray = RegInit(0.U.asTypeOf(Vec(numEntries, Bool())))
  // io.out.resp
  val s1_ridx = RegEnable(s0_ridx, io.s0_fire)
  val resp_valid = RegEnable(validArray(s0_ridx), io.s0_fire)


  val outMeta = Wire(new MicroBTBOutMeta)

  XSDebug(p"uBTB entry, read_pc=${Hexadecimal(s0_pc)}\n")

  io.out.resp.s1.minimal_pred.fromMicroBTBEntry(resp_valid, dataMem.io.r.resp.data(0), s1_pc)
  io.out.resp.s1.is_minimal := true.B

  outMeta.hit := true.B
  io.out.last_stage_meta := RegEnable(outMeta.asUInt, io.s1_fire)

  // Update logic
  val update = RegNext(io.update.bits)
  val u_valid = RegNext(io.update.valid)
  val u_pc = update.pc
  val u_br_taken_mask = update.full_pred.br_taken_mask
  val u_meta = update.meta.asTypeOf(new MicroBTBOutMeta)
  val u_data = Wire(new NewMicroBTBEntry)
  u_data.fromBpuUpdateBundle(update)
  val u_idx = getIdx(update.pc) ^ get_ghist_from_fh(update.folded_hist).folded_hist

  dataMem.io.w.apply(u_valid, u_data, u_idx, 1.U(1.W))
  when (u_valid) {
    validArray(u_idx) := true.B
  }

  // XSDebug("req_v=%b, req_pc=%x, hit=%b\n", io.s1_fire, s1_pc, bank.read_hit)
  XSDebug("target=%x\n", io.out.resp.s1.getTarget)

  XSDebug(u_valid, "[update]Update from ftq\n")
  XSDebug(u_valid, "[update]update_pc=%x, tag=%x\n", u_pc, ubtbAddr.getTag(u_pc))
  XSDebug(u_valid, "[update]taken_mask=%b, brValids=%b, jmpValid=%b\n",
    u_br_taken_mask.asUInt, update.ftb_entry.brValids.asUInt, update.ftb_entry.jmpValid)

  // XSPerfAccumulate("ubtb_read_hits", RegNext(io.s1_fire) && bank.read_hit)
  // XSPerfAccumulate("ubtb_read_misses", RegNext(io.s1_fire) && !bank.read_hit)

  XSPerfAccumulate("ubtb_commit_hits", u_valid && u_meta.hit)
  XSPerfAccumulate("ubtb_commit_misses", u_valid && !u_meta.hit)

  val perfEvents = Seq(
    ("ubtb_commit_hit       ", u_valid &&  u_meta.hit),
    ("ubtb_commit_miss      ", u_valid && !u_meta.hit),
  )
  generatePerfEvent()
}
