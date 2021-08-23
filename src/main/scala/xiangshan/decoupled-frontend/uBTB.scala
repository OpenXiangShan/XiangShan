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

import scala.math.min

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

  class MicroBTBData extends FTBEntry {
    // val brTargets = Vec(numBr, UInt(VAddrBits.W))
    // val jmpTarget = UInt(VAddrBits.W)
    // val pftAddr   = UInt((log2Up(PredictWidth)+1).W)
  }

  class ReadResp extends FTBEntry {
    val hit = Bool()
  }
  
  // object ReadResp {
  //   def apply(entry: MicroBTBMeta, hit: Bool)(implicit p: Parameters): ReadResp = {
  //     val e = Wire(new ReadResp())
  //     
  //     e.valid := entry.valid
  //     e.brOffset := entry.brOffset
  //     e.brTargets := entry.brTargets
  //     e.brValids := entry.brValids

  //     e.jmpOffset := entry.jmpOffset
  //     e.jmpTarget := entry.jmpTarget
  //     e.jmpValid := entry.jmpValid
  //     
  //     e.pftAddr := entry.pftAddr
  //     e.carry := entry.carry
  //     e.oversize := entry.oversize
  //     e.last_is_rvc := entry.last_is_rvc
  //     
  //     e.hit := hit
  //     
  //     e
  //   }
  // }

  override val meta_size = WireInit(0.U.asTypeOf(new MicroBTBOutMeta)).getWidth // TODO: ReadResp shouldn't save useless members

  class UBTBBank(val nWays: Int) extends XSModule with BPUUtils {
    val io = IO(new Bundle {
      val read_pc = Flipped(Valid(UInt(VAddrBits.W)))
      // val read_taken_mask = Input(Vec(numBr, Bool()))
      val read_resp = Output(new ReadResp)
      // val read_hit = Output(Bool())

      val update_write_meta = Flipped(Valid(new MicroBTBMeta))
      val update_write_data = Flipped(Valid(new MicroBTBData))
      val update_taken_mask = Input(Vec(numBr, Bool()))
      val update_mask = Input(UInt(numBr.W))
    })

    val tagCam = Module(new CAMTemplate(UInt(tagSize.W), nWays, 2))
    val metaMem = Module(new AsyncDataModuleTemplate(new MicroBTBMeta, nWays, nWays, 1))
    val dataMem = Module(new AsyncDataModuleTemplate(new MicroBTBData, nWays, 1, 1))


    for (w <- 0 until nWays) {
      metaMem.io.raddr(w) := w.U
    }

    val meta = metaMem.io.rdata
    // val rbims = bims.io.rdata.take(nWays)
    // val rdatas = data.io.rdata
    

    val read_pc = io.read_pc.bits
    val read_tag = getTag(read_pc)

    // val hits = VecInit(rmetas.map(m => m.valid && m.tag === read_tag))
    val hits = VecInit((0 until nWays).map(i => meta(i).valid && tagCam.io.r.resp(0)(i)))
    val hit = hits.reduce(_||_)
    val hitWay = OHToUInt(hits)

    dataMem.io.raddr(0) := hitWay
    // val hit_meta = ParallelMux(hits zip rmetas)
    // val hit_data = ParallelMux(hits zip rdatas)

    val hit_data = Mux(hit, dataMem.io.rdata(0), 0.U.asTypeOf(new MicroBTBData))

    io.read_resp := DontCare
    io.read_resp.valid := io.read_pc.valid

    io.read_resp.tag          := read_tag
    io.read_resp.brOffset     := hit_data.brOffset
    io.read_resp.brTargets    := hit_data.brTargets
    io.read_resp.brValids     := hit_data.brValids

    io.read_resp.jmpOffset    := hit_data.jmpOffset
    io.read_resp.jmpTarget    := hit_data.jmpTarget
    io.read_resp.jmpValid     := hit_data.jmpValid

    io.read_resp.pftAddr      := hit_data.pftAddr
    io.read_resp.carry        := hit_data.carry

    io.read_resp.isCall       := hit_data.isCall
    io.read_resp.isRet        := hit_data.isRet
    io.read_resp.isJalr       := hit_data.isJalr

    io.read_resp.oversize     := hit_data.oversize
    io.read_resp.last_is_rvc  := hit_data.last_is_rvc

    io.read_resp.hit          := hit

    val do_reset = RegInit(true.B)
    val reset_way = RegInit(0.U(log2Ceil(nWays).W))
    when (RegNext(reset.asBool) && !reset.asBool) {
      do_reset := true.B
      reset_way := 0.U
    }
    when (do_reset) { reset_way := reset_way + 1.U }
    when (reset_way === (nWays-1).U) { do_reset := false.B }

    val update_tag = io.update_write_meta.bits.tag
    val update_hits = VecInit((0 until nWays).map(i => meta(i).valid && tagCam.io.r.resp(1)(i)))
    val update_hit = update_hits.reduce(_||_)
    val update_hitWay = OHToUInt(update_hits)
    

    val update_alloc_way = {
      val source = Cat(VecInit(meta.map(_.tag)).asUInt, update_tag)
      val l = log2Ceil(nWays)
      val nChunks = (source.getWidth + l - 1) / l
      val chunks = (0 until nChunks) map { i =>
        source(min((i+1)*l, source.getWidth)-1, i*l)
      }
      ParallelXOR(chunks)
    }
    val update_emptys = meta.map(m => !m.valid)
    val update_has_empty_way = update_emptys.reduce(_||_)
    val update_empty_way = ParallelPriorityEncoder(update_emptys)
    val update_way = Mux(update_hit, update_hitWay, Mux(update_has_empty_way, update_empty_way, update_alloc_way))

    tagCam.io.r.req := VecInit(Seq(read_tag, update_tag))
    
    tagCam.io.w.valid       := do_reset || RegNext(io.update_write_meta.valid)
    tagCam.io.w.bits.index  := Mux(do_reset, reset_way, RegNext(update_way))
    tagCam.io.w.bits.data   := Mux(do_reset,
      0.U(tagSize.W),
      RegNext(io.update_write_meta.bits.tag))

    metaMem.io.wen(0)   := do_reset || RegNext(io.update_write_data.valid)
    metaMem.io.waddr(0) := Mux(do_reset, reset_way, RegNext(update_way))
    metaMem.io.wdata(0) := Mux(do_reset,
      0.U.asTypeOf(new MicroBTBMeta),
      RegNext(io.update_write_meta.bits))

    
    dataMem.io.wen(0)   := do_reset || RegNext(io.update_write_data.valid)
    dataMem.io.waddr(0) := Mux(do_reset, reset_way, RegNext(update_way))
    dataMem.io.wdata(0) := Mux(do_reset,
      0.U.asTypeOf(new MicroBTBData),
      RegNext(io.update_write_data.bits))

  } // uBTBBank

  val ubtbBanks = Module(new UBTBBank(numWays))
  val banks = ubtbBanks.io
  val read_resps = banks.read_resp
  val outMeta = Wire(new MicroBTBOutMeta)

  XSDebug(p"uBTB entry, read_pc=${Hexadecimal(s1_pc)}\n")
  //XSDebug(p"v=${read_resps.valid}, brValids=${Binary(read_resps.brValids.asUInt)}, jmpValid=${read_resps.jmpValid}, pred0=${Binary(read_resps.pred(0).asUInt)}, pred1=${Binary(read_resps.pred(1).asUInt)}, hit=${read_resps.hit}\n")
  // XSDebug(p"v=${read_resps.valid}, brValids=${Binary(read_resps.brValids.asUInt)}, jmpValid=${read_resps.jmpValid}, pred0=${Binary(read_resps.pred(0).asUInt)}, pred1=${Binary(read_resps.pred(1).asUInt)}, hit=${read_resps.hit}\n")
  // XSDebug(p"v=${read_resps.valid}, brValids=${Binary(read_resps.brValids.asUInt)}, jmpValid=${read_resps.jmpValid}, pred0=${Binary(read_resps.pred(0).asUInt)}, hit=${read_resps.hit}\n")

  banks.read_pc.valid := io.s1_fire
  banks.read_pc.bits := s1_pc

  io.out.resp := io.in.bits.resp_in(0)
  io.out.resp.s1.pc := s1_pc
  io.out.resp.s1.preds.taken_mask := io.in.bits.resp_in(0).s1.preds.taken_mask
  io.out.resp.s1.preds.hit := read_resps.hit
  io.out.resp.s1.ftb_entry := DontCare
  io.out.resp.s1.ftb_entry.valid := read_resps.valid

  io.out.resp.s1.ftb_entry.brOffset     := read_resps.brOffset
  io.out.resp.s1.ftb_entry.brTargets    := read_resps.brTargets
  io.out.resp.s1.ftb_entry.brValids     := read_resps.brValids

  io.out.resp.s1.ftb_entry.jmpOffset    := read_resps.jmpOffset
  io.out.resp.s1.ftb_entry.jmpTarget    := read_resps.jmpTarget
  io.out.resp.s1.ftb_entry.jmpValid     := read_resps.jmpValid

  io.out.resp.s1.ftb_entry.pftAddr      := read_resps.pftAddr
  io.out.resp.s1.ftb_entry.carry        := read_resps.carry

  io.out.resp.s1.ftb_entry.oversize     := read_resps.oversize
  io.out.resp.s1.ftb_entry.last_is_rvc  := read_resps.last_is_rvc

  outMeta.hit := read_resps.hit
  io.out.s3_meta := RegEnable(RegEnable(outMeta.asUInt, io.s1_fire), io.s2_fire)

  // Update logic
  val update = RegNext(io.update.bits)
  val u_valid = RegNext(io.update.valid)
  val u_pc = update.pc
  val u_taken = update.preds.taken
  val u_taken_mask = update.preds.taken_mask
  val u_meta = update.meta.asTypeOf(new MicroBTBOutMeta)

  val u_tag = getTag(u_pc)
  // val u_target_lower = update.preds.target(lowerBitSize-1+instOffsetBits, instOffsetBits)

  val data_write_valid = u_valid && u_taken
  val meta_write_valid = u_valid && (u_taken || update.ftb_entry.brValids.reduce(_||_))

  val update_write_datas = Wire(new MicroBTBData)
  val update_write_metas = Wire(new MicroBTBMeta)

  update_write_metas := DontCare

  update_write_metas.valid        := true.B
  update_write_metas.tag          := u_tag

  update_write_datas              := DontCare
  update_write_datas.brOffset     := update.ftb_entry.brOffset
  update_write_datas.brValids     := update.ftb_entry.brValids
  update_write_datas.jmpOffset    := update.ftb_entry.jmpOffset
  update_write_datas.jmpValid     := update.ftb_entry.jmpValid
  update_write_datas.isCall       := update.ftb_entry.isCall
  update_write_datas.isRet        := update.ftb_entry.isRet
  update_write_datas.isJalr       := update.ftb_entry.isJalr
  update_write_datas.carry        := update.ftb_entry.carry
  update_write_datas.oversize     := update.ftb_entry.oversize
  update_write_datas.last_is_rvc  := update.ftb_entry.last_is_rvc
  update_write_datas.jmpTarget    := update.ftb_entry.jmpTarget
  update_write_datas.brTargets    := update.ftb_entry.brTargets
  update_write_datas.pftAddr      := update.ftb_entry.pftAddr

  banks.update_write_meta.valid := meta_write_valid
  banks.update_write_meta.bits := update_write_metas
  banks.update_write_data.valid := data_write_valid
  banks.update_write_data.bits := update_write_datas
  banks.update_taken_mask := u_taken_mask
  banks.update_mask := LowerMaskFromLowest(u_taken_mask.asUInt)

  if (debug && !env.FPGAPlatform && env.EnablePerfDebug) {
    XSDebug("req_v=%b, req_pc=%x, hit=%b\n", io.s1_fire, s1_pc, read_resps.hit)
    XSDebug("target=%x, real_taken_mask=%b, taken_mask=%b, brValids=%b, jmpValid=%b\n",
      io.out.resp.s1.target, io.out.resp.s1.real_taken_mask.asUInt, io.out.resp.s1.preds.taken_mask.asUInt, read_resps.brValids.asUInt, read_resps.jmpValid.asUInt)

    XSDebug(u_valid, "[update]Update from ftq\n")
    XSDebug(u_valid, "[update]update_pc=%x, tag=%x\n", u_pc, getTag(u_pc))
    XSDebug(u_valid, "[update]taken_mask=%b, brValids=%b, jmpValid=%b\n",
      u_taken_mask.asUInt, update.ftb_entry.brValids.asUInt, update.ftb_entry.jmpValid)

    XSPerfAccumulate("ubtb_read_hits", RegNext(io.s1_fire) && read_resps.hit)
    XSPerfAccumulate("ubtb_read_misses", RegNext(io.s1_fire) && !read_resps.hit)

    XSPerfAccumulate("ubtb_commit_hits", u_valid && u_meta.hit)
    XSPerfAccumulate("ubtb_commit_misses", u_valid && !u_meta.hit)
  }


}
