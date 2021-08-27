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

trait MicroBTBParams extends HasXSParameter {
  val numWays = 32
  val tagSize = 20
  val lowerBitSize = 20
  val untaggedBits = instOffsetBits
}

@chiselName
class MicroBTB(implicit p: Parameters) extends BasePredictor
  with MicroBTBParams
{
  def getTag(pc: UInt)  = (pc >> untaggedBits)(tagSize-1, 0)

  class MicroBTBMeta extends XSBundle {
    val valid = Bool()
    val tag = UInt(tagSize.W)
  }

  class MicroBTBOutMeta extends XSBundle {
    val hit = Bool()
  }

  class MicroBTBEntry extends FTBEntry {}

  override val meta_size = WireInit(0.U.asTypeOf(new MicroBTBOutMeta)).getWidth // TODO: ReadResp shouldn't save useless members

  class UBTBBank(val nWays: Int) extends XSModule with BPUUtils {
    val io = IO(new Bundle {
      val read_pc = Flipped(Valid(UInt(VAddrBits.W)))
      // val read_taken_mask = Input(Vec(numBr, Bool()))
      val read_entry = Output(new FTBEntry)
      val read_hit = Output(Bool())

      val update_valid = Input(Bool())
      val update_write_tag = Input(UInt(tagSize.W))
      val update_write_entry = Input(new MicroBTBEntry)
    })

    val tagCam = Module(new CAMTemplate(UInt(tagSize.W), nWays, 2))
    val valids = RegInit(VecInit(Seq.fill(nWays)(false.B))) // valids
    val dataMem = Module(new AsyncDataModuleTemplate(new MicroBTBEntry, nWays, 1, 1))

    val read_pc = io.read_pc.bits
    val read_tag = getTag(read_pc)

    val hits = VecInit((0 until nWays).map(i => valids(i) && tagCam.io.r.resp(0)(i)))
    val hit = hits.reduce(_||_)
    val hitWay = OHToUInt(hits)

    dataMem.io.raddr(0) := hitWay

    val hit_entry = Mux(hit, dataMem.io.rdata(0), 0.U.asTypeOf(new MicroBTBEntry))

    io.read_entry := hit_entry
    io.read_hit := hit

    val update_tag = io.update_write_tag
    val update_hits = VecInit((0 until nWays).map(i => valids(i) && tagCam.io.r.resp(1)(i)))
    val update_hit = update_hits.reduce(_||_)
    val update_hitWay = OHToUInt(update_hits)
    
    require(tagSize % log2Ceil(nWays) == 0)

    val update_alloc_way = RegInit(0.U(log2Ceil(nWays).W))
    
    when (io.update_valid && !update_hit) {
      update_alloc_way := update_alloc_way ^ foldTag(update_tag, log2Ceil(nWays))
    }

    val update_emptys = valids.map(!_)
    val update_has_empty_way = update_emptys.reduce(_||_)
    val update_empty_way = ParallelPriorityEncoder(update_emptys)
    val update_way = Wire(UInt(log2Ceil(nWays).W))
    

    val update_valid_reg = RegNext(io.update_valid)
    val update_way_reg = RegNext(update_way)
    val update_tag_reg = RegNext(update_tag)
    val update_entry_reg = RegNext(io.update_write_entry)

    val update_bypass_valid = update_valid_reg && io.update_valid && update_tag === update_tag_reg
    update_way :=
      Mux(update_bypass_valid, update_way_reg,
        Mux(update_hit, update_hitWay,
          Mux(update_has_empty_way, update_empty_way,
            update_alloc_way)))

    tagCam.io.r.req := VecInit(Seq(read_tag, update_tag))
    
    tagCam.io.w.valid       := update_valid_reg
    tagCam.io.w.bits.index  := update_way_reg
    tagCam.io.w.bits.data   := update_tag_reg

    when (update_valid_reg) {
      valids(update_way_reg) := true.B
    }

    dataMem.io.wen(0)   := update_valid_reg
    dataMem.io.waddr(0) := update_way_reg
    dataMem.io.wdata(0) := update_entry_reg

  } // uBTBBank

  val ubtbBank = Module(new UBTBBank(numWays))
  val bank = ubtbBank.io
  val read_entry = bank.read_entry
  val outMeta = Wire(new MicroBTBOutMeta)

  XSDebug(p"uBTB entry, read_pc=${Hexadecimal(s1_pc)}\n")

  bank.read_pc.valid := io.s1_fire
  bank.read_pc.bits := s1_pc

  io.out.resp := io.in.bits.resp_in(0)
  io.out.resp.s1.pc := s1_pc
  io.out.resp.s1.preds.hit := bank.read_hit
  io.out.resp.s1.ftb_entry := read_entry
  io.out.resp.s1.preds.fromFtbEntry(read_entry, s1_pc)

  when(!bank.read_hit) {
    io.out.resp.s1.ftb_entry.pftAddr := s1_pc(instOffsetBits + log2Ceil(PredictWidth), instOffsetBits) ^ (1 << log2Ceil(PredictWidth)).U
    io.out.resp.s1.ftb_entry.carry := s1_pc(instOffsetBits + log2Ceil(PredictWidth)).asBool
    io.out.resp.s1.ftb_entry.oversize := false.B
  }

  outMeta.hit := bank.read_hit
  io.out.s3_meta := RegEnable(RegEnable(outMeta.asUInt, io.s1_fire), io.s2_fire)

  // Update logic
  val update = RegNext(io.update.bits)
  val u_valid = RegNext(io.update.valid)
  val u_pc = update.pc
  val u_taken = update.preds.taken
  val u_taken_mask = update.preds.taken_mask
  val u_meta = update.meta.asTypeOf(new MicroBTBOutMeta)

  val u_tag = getTag(u_pc)

  bank.update_valid := u_valid && u_taken
  bank.update_write_tag := u_tag
  bank.update_write_entry := update.ftb_entry

  if (debug && !env.FPGAPlatform && env.EnablePerfDebug) {
    XSDebug("req_v=%b, req_pc=%x, hit=%b\n", io.s1_fire, s1_pc, bank.read_hit)
    XSDebug("target=%x, real_taken_mask=%b, taken_mask=%b, brValids=%b, jmpValid=%b\n",
      io.out.resp.s1.target, io.out.resp.s1.real_taken_mask.asUInt, io.out.resp.s1.preds.taken_mask.asUInt, read_entry.brValids.asUInt, read_entry.jmpValid.asUInt)

    XSDebug(u_valid, "[update]Update from ftq\n")
    XSDebug(u_valid, "[update]update_pc=%x, tag=%x\n", u_pc, getTag(u_pc))
    XSDebug(u_valid, "[update]taken_mask=%b, brValids=%b, jmpValid=%b\n",
      u_taken_mask.asUInt, update.ftb_entry.brValids.asUInt, update.ftb_entry.jmpValid)

    XSPerfAccumulate("ubtb_read_hits", RegNext(io.s1_fire) && bank.read_hit)
    XSPerfAccumulate("ubtb_read_misses", RegNext(io.s1_fire) && !bank.read_hit)

    XSPerfAccumulate("ubtb_commit_hits", u_valid && u_meta.hit)
    XSPerfAccumulate("ubtb_commit_misses", u_valid && !u_meta.hit)
  }


}
