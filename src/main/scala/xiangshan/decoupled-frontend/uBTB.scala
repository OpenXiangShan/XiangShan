/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

import scala.math.min

trait MicroBTBParams extends HasXSParameter {
  val numWays = 16
  val tagSize = 20
  val lowerBitSize = 20
  val untaggedBits = instOffsetBits
}

@chiselName
class MicroBTB(implicit p: Parameters) extends BasePredictor
  with MicroBTBParams
{
  def getTag(pc: UInt)  = (pc >> untaggedBits)(tagSize-1, 0)

  class MicroBTBMeta extends XSBundle
  {
    val valid       = Bool()
    val tag         = UInt(tagSize.W)

    val brOffset    = Vec(numBr, UInt(log2Up(FetchWidth*2).W))
    val brValids    = Vec(numBr, Bool())

    val jmpValid    = Bool() // include jal and jalr

    val carry       = Bool()

    val isCall      = Bool()
    val isRet       = Bool()
    val isJalr      = Bool()

    val oversize    = Bool()

    val last_is_rvc = Bool()

    // Bimodal
    val pred        = Vec(numBr, UInt(2.W))

    def taken = pred.map(_(1)).reduce(_ || _)
    def taken_mask = VecInit(pred.map(_(1)))
    def real_taken_mask(): Vec[Bool] = {
      VecInit(taken_mask.zip(brValids).map{ case(m, b) => m && b } :+ jmpValid)
    }

    def real_br_taken_mask(): Vec[Bool] = {
      VecInit(taken_mask.zip(brValids).map{ case(m, b) => m && b })
    }
  }

  class MicroBTBData extends XSBundle
  {
    val brTargets = Vec(numBr, UInt(VAddrBits.W))
    val jmpTarget = UInt(VAddrBits.W)
    val pftAddr   = UInt((log2Up(PredictWidth)+1).W)
  }

  class ReadResp extends XSBundle
  {
    val valid = Bool()
    val taken_mask = Vec(numBr, Bool())
    val target = UInt(VAddrBits.W)
    val brValids = Vec(numBr, Bool())
    val jmpValid = Bool()
    // val isCall = Bool()
    // val isRet = Bool()
    // val isJalr = Bool()
    // val last_is_rvc = Bool()
    val pred        = Vec(numBr, UInt(2.W))

    val hit = Bool()

    def real_taken_mask(): Vec[Bool] = {
      VecInit(taken_mask.zip(brValids).map{ case(m, b) => m && b } :+ jmpValid)
    }

    def real_br_taken_mask(): Vec[Bool] = {
      VecInit(taken_mask.zip(brValids).map{ case(m, b) => m && b })
    }
  }

  override val meta_size = WireInit(0.U.asTypeOf(new ReadResp)).getWidth

  class UBTBBank(val nWays: Int) extends XSModule with BPUUtils {
    val io = IO(new Bundle {
      val read_pc = Flipped(Valid(UInt(VAddrBits.W)))
      val read_resp = Output(new ReadResp)
      val read_hit = Output(Bool())

      val update_write_meta = Flipped(Valid(new MicroBTBMeta))
      val update_write_data = Flipped(Valid(new MicroBTBData))
      val update_taken_mask = Input(Vec(numBr, Bool()))
      val update_mask = Input(UInt(numBr.W))
    })

    val meta = Module(new AsyncDataModuleTemplate(new MicroBTBMeta, nWays, nWays*2, 1))
    val data = Module(new AsyncDataModuleTemplate(new MicroBTBData, nWays,   nWays, 1))

    for (w <- 0 until nWays) {
      meta.io.raddr(w) := w.U
      meta.io.raddr(w+nWays) := w.U
      data.io.raddr(w) := w.U
    }

    val rmetas = meta.io.rdata.take(nWays)
    val rdatas = data.io.rdata

    val read_pc = io.read_pc.bits
    val read_tag = getTag(read_pc)

    val hits = VecInit(rmetas.map(m => m.valid && m.tag === read_tag))
    val taken_masks = VecInit(rmetas.map(m => m.taken_mask))
    val reaL_taken_masks = VecInit(rmetas.map(m => m.real_taken_mask))
    val hit_oh = hits.asUInt
    val hit_meta = ParallelMux(hits zip rmetas)
    val hit_data = ParallelMux(hits zip rdatas)
    val hit_and_taken_mask = ParallelMux(hits zip taken_masks)
    val hit_and_real_taken_mask = ParallelMux(hits zip reaL_taken_masks)

    val target = Wire(UInt(VAddrBits.W))
    target := read_pc + (FetchWidth*4).U

    val fallThruAddr = getFallThroughAddr(read_pc, hit_meta.carry, hit_data.pftAddr)

    when(hit_oh =/= 0.U) {
      target := Mux(hit_and_real_taken_mask.asUInt =/= 0.U,
        PriorityMux(hit_and_real_taken_mask, hit_data.brTargets :+ hit_data.jmpTarget),
        fallThruAddr)
    }

    val ren = io.read_pc.valid
    io.read_resp.valid := ren
    io.read_resp.taken_mask := Mux(ren, hit_and_taken_mask, VecInit(Seq.fill(numBr)(false.B)))
    io.read_resp.target := target
    io.read_resp.brValids := hit_meta.brValids
    io.read_resp.jmpValid := hit_meta.jmpValid
    io.read_resp.pred := hit_meta.pred
    io.read_resp.hit := hit_oh.orR
    io.read_hit := hit_oh.orR

    val do_reset = RegInit(true.B)
    val reset_way = RegInit(0.U(log2Ceil(nWays).W))
    when (RegNext(reset.asBool) && !reset.asBool) {
      do_reset := true.B
      reset_way := 0.U
    }
    when (do_reset) { reset_way := reset_way + 1.U }
    when (reset_way === (nWays-1).U) { do_reset := false.B }

    val update_rmetas = meta.io.rdata.drop(nWays)
    val update_tag = io.update_write_meta.bits.tag
    val update_hits = VecInit(update_rmetas.map(m => m.valid && m.tag === update_tag))
    val update_hit = update_hits.asUInt.orR
    val update_hit_way = OHToUInt(update_hits.asUInt)
    val update_hit_meta = ParallelMux(update_hits zip update_rmetas)
    val update_old_pred = update_hit_meta.pred
    val update_new_pred = VecInit(
      (0 until numBr).map { i =>
        Mux(update_hit && io.update_mask(i), satUpdate(update_old_pred(i), 2, io.update_taken_mask(i)),
          Mux(io.update_taken_mask(i), 3.U, 0.U))
      })

    val update_alloc_way = {
      val source = Cat(VecInit(update_rmetas.map(_.tag)).asUInt, update_tag)
      val l = log2Ceil(nWays)
      val nChunks = (source.getWidth + l - 1) / l
      val chunks = (0 until nChunks) map { i =>
        source(min((i+1)*l, source.getWidth)-1, i*l)
      }
      ParallelXOR(chunks)
    }
    val update_emptys = update_rmetas.map(m => !m.valid)
    val update_has_empty_way = update_emptys.reduce(_||_)
    val update_empty_way = ParallelPriorityEncoder(update_emptys)
    val update_way = Mux(update_hit, update_hit_way, Mux(update_has_empty_way, update_empty_way, update_alloc_way))

    meta.io.waddr(0) := Mux(do_reset, reset_way, RegNext(update_way))
    meta.io.wen(0)   := do_reset || RegNext(io.update_write_meta.valid)
    meta.io.wdata(0) := Mux(do_reset,
      0.U.asTypeOf(new MicroBTBMeta),
      RegNext(io.update_write_meta.bits))
    meta.io.wdata(0).pred := Mux(do_reset, VecInit(Seq.fill(numBr)(0.U(2.W))), RegNext(update_new_pred))
    data.io.waddr(0) := Mux(do_reset, reset_way, RegNext(update_way))
    data.io.wen(0)   := do_reset || RegNext(io.update_write_data.valid)
    data.io.wdata(0) := Mux(do_reset,
      0.U.asTypeOf(new MicroBTBData),
      RegNext(io.update_write_data.bits))

  } // uBTBBank

  val ubtbBanks = Module(new UBTBBank(numWays))
  val banks = ubtbBanks.io
  val read_resps = banks.read_resp

  XSDebug(p"uBTB entry, read_pc=${Hexadecimal(s1_pc)}\n")
  XSDebug(p"v=${read_resps.valid}, brValids=${Binary(read_resps.brValids.asUInt)}, jmpValid=${read_resps.jmpValid}, pred0=${Binary(read_resps.pred(0).asUInt)}, pred1=${Binary(read_resps.pred(1).asUInt)}, hit=${read_resps.hit}\n")

  // io.in.ready := !io.flush.valid

  banks.read_pc.valid := io.s1_fire
  banks.read_pc.bits := s1_pc

  // io.out.valid := io.s1_fire && !io.redirect.valid
  io.out.resp := io.in.bits.resp_in(0)
  // io.out.resp.valids(0) := io.out.valid
  io.out.resp.s1.pc := s1_pc
  io.out.resp.s1.preds.target := Mux(banks.read_hit, read_resps.target, s1_pc + (FetchWidth*4).U)
  io.out.resp.s1.preds.taken_mask := read_resps.taken_mask
  // io.out.resp.s1.preds.is_br := read_resps.brValids
  io.out.resp.s1.preds.hit := banks.read_hit
  // io.out.bits.resp.s1.preds.is_jal := read_resps.jmpValid && !(read_resps.isCall || read_resps.isRet || read_resps.isJalr)
  // io.out.bits.resp.s1.preds.is_jalr := read_resps.jmpValid && read_resps.isJalr
  // io.out.bits.resp.s1.preds.is_call := read_resps.jmpValid && read_resps.isCall
  // io.out.bits.resp.s1.preds.is_ret := read_resps.jmpValid && read_resps.isRet
  // io.out.bits.resp.s1.preds.call_is_rvc := read_resps.last_is_rvc
  io.out.resp.s1.ftb_entry := DontCare
  io.out.resp.s1.ftb_entry.brValids := read_resps.brValids
  io.out.s3_meta := RegEnable(RegEnable(read_resps.asUInt, io.s1_fire), io.s2_fire)

  // Update logic
  val update = RegNext(io.update.bits)
  val u_valid = RegNext(io.update.valid)
  val u_pc = update.pc
  val u_taken = update.preds.taken
  val u_taken_mask = update.preds.taken_mask
  val u_meta = update.meta.asTypeOf(new ReadResp)

  val u_tag = getTag(u_pc)
  // val u_target_lower = update.preds.target(lowerBitSize-1+instOffsetBits, instOffsetBits)

  val data_write_valid = u_valid && u_taken
  val meta_write_valid = u_valid && (u_taken || update.ftb_entry.brValids.reduce(_||_))

  val update_write_datas = Wire(new MicroBTBData)
  val update_write_metas = Wire(new MicroBTBMeta)

  update_write_metas := DontCare

  update_write_metas.valid := true.B
  update_write_metas.tag := u_tag
  // brOffset
  update_write_metas.brValids := update.ftb_entry.brValids
  update_write_metas.jmpValid := update.ftb_entry.jmpValid
  // isJalr
  // isCall
  // isRet
  update_write_metas.pred := DontCare
  update_write_metas.carry := update.ftb_entry.carry

  // update_write_datas.lower := u_target_lower
  update_write_datas.jmpTarget := update.ftb_entry.jmpTarget
  update_write_datas.brTargets := update.ftb_entry.brTargets
  update_write_datas.pftAddr := update.ftb_entry.pftAddr

  banks.update_write_meta.valid := meta_write_valid
  banks.update_write_meta.bits := update_write_metas
  banks.update_write_data.valid := data_write_valid
  banks.update_write_data.bits := update_write_datas
  banks.update_taken_mask := u_taken_mask
  banks.update_mask := LowerMaskFromLowest(u_taken_mask.asUInt)

  if (debug) {
    XSDebug("req_v=%b, req_pc=%x, hit=%b\n", io.s1_fire, s1_pc, banks.read_hit)
    XSDebug("target=%x, taken_mask=%b\n", io.out.resp.s1.preds.target, read_resps.taken_mask.asUInt)

    XSDebug(u_valid, "Update from ftq\n")
    XSDebug(u_valid, "update_pc=%x, tag=%x\n", u_pc, getTag(u_pc))
    XSDebug(u_valid, "taken_mask=%b, brValids=%b, jmpValid=%b\n",
      u_taken_mask.asUInt, update.ftb_entry.brValids.asUInt, update.ftb_entry.jmpValid)

    XSPerfAccumulate("ubtb_read_hits", RegNext(io.s1_fire) && banks.read_hit)
    XSPerfAccumulate("ubtb_read_misses", RegNext(io.s1_fire) && !banks.read_hit)

    XSPerfAccumulate("ubtb_commit_hits", u_valid && u_meta.hit)
    XSPerfAccumulate("ubtb_commit_misses", u_valid && !u_meta.hit)
  }


}
