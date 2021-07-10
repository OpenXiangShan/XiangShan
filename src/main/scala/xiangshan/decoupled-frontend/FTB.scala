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
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import xiangshan._
import utils._
import chisel3.experimental.chiselName

import scala.math.min


trait FTBParams extends HasXSParameter with HasBPUConst {
  val num_entries = 2048
  val num_ways    = 4
  val num_sets    = num_entries/num_ways // 512
  val tag_size    = 20
}

class FTBEntry (implicit p: Parameters) extends XSBundle with FTBParams {
  val valid       = Bool()
  val tag         = UInt(tag_size.W)

  val br_offset   = Vec(num_br, UInt(log2Up(FetchWidth*2).W))
  val br_target   = UInt(VAddrBits.W)
  val br_valids   = Vec(num_br, Bool())

  val jmp_target  = UInt(VAddrBits.W)
  val jmp_valid   = Bool()

  // Partial Fall-Through Address
  val pft_addr    = UInt(VAddrBits.W) // TODO: Modify only use lowerbits
  val carry       = Bool()

  val is_call     = Bool()
  val is_ret      = Bool()
  val is_jalr     = Bool()

  val call_is_rvc = Bool()

  val oversize    = Bool()
}

class FTBMeta(implicit p: Parameters) extends XSBundle with FTBParams {
  val writeWay = UInt(log2Up(num_ways).W)
  val hit = Bool()
}

object FTBMeta {
  def apply(writeWay: UInt, hit: Bool)(implicit p: Parameters): FTBMeta = {
    val e = Wire(new FTBMeta)
    e.writeWay := writeWay
    e.hit := hit
    e
  }
}

class FTB(implicit p: Parameters) extends BasePredictor with FTBParams {
  val ftbAddr = new TableAddr(log2Up(num_sets), num_br)

  val f0_valid = io.f0_valid
  val f0_pc = io.f0_pc

  val f1_pc = RegEnable(f0_pc, f0_valid)

  val ftb = Module(new SRAMTemplate(new FTBEntry, set = num_sets, way = num_ways, shouldReset = true, holdRead = true, singlePort = true))

  val f0_idx = ftbAddr.getBankIdx(f0_pc)
  val f1_idx = RegEnable(f0_idx, f0_valid)

  val f1_tag = ftbAddr.getTag(f1_pc)

  ftb.io.r.req.valid := f0_valid
  ftb.io.r.req.bits.setIdx := f0_idx

  io.resp.valid := io.f0_valid && ftb.io.r.req.ready && io.flush

  val f1_read = VecInit((0 until num_ways).map(w =>
    ftb.io.r.resp.data(w)
  ))

  val f1_totalHits = VecInit((0 until num_ways).map(b => f1_read(b).tag === f1_tag && f1_read(b).valid))
  val f1_hit = f1_totalHits.reduce(_||_)
  val f1_hit_way = PriorityEncoder(f1_totalHits)

  def allocWay(valids: UInt, meta_tags: UInt, req_tag: UInt) = {
    val randomAlloc = true
    if (num_ways > 1) {
      val w = Wire(UInt(log2Up(num_ways).W))
      val valid = WireInit(valids.andR)
      val tags = Cat(meta_tags, req_tag)
      val l = log2Up(num_ways)
      val nChunks = (tags.getWidth + l - 1) / l
      val chunks = (0 until nChunks).map( i =>
        tags(min((i+1)*l, tags.getWidth)-1, i*l)
      )
      w := Mux(valid, if (randomAlloc) {LFSR64()(log2Up(num_ways)-1,0)} else {chunks.reduce(_^_)}, PriorityEncoder(~valids))
      w
    } else {
      val w = WireInit(0.U)
      w
    }
  }
  val allocWays = VecInit((0 until num_ways).map(b =>
    allocWay(VecInit(f1_read.map(w => w.valid)).asUInt,
      VecInit(f1_read.map(w => w.tag)).asUInt,
      f1_tag)))

  val writeWay = Mux(f1_hit, f1_hit_way, allocWays)

  val ftb_entry = f1_read(f1_hit_way)

  val br_target = ftb_entry.br_target
  val jal_target = ftb_entry.jmp_target

  io.resp.bits := io.resp_in(0)

  io.resp.bits.f1.preds.pred_target := Mux(f1_hit, Mux(ftb_entry.jmp_valid, jal_target, br_target), f0_pc + (FetchWidth*4).U)
  io.resp.bits.f1.hit               := f1_hit
  io.resp.bits.f1.preds.is_br       := ftb_entry.br_valids.reduce(_||_)
  io.resp.bits.f1.preds.is_jal      := ftb_entry.jmp_valid
  io.resp.bits.f1.preds.is_call     := ftb_entry.is_call
  io.resp.bits.f1.preds.is_ret      := ftb_entry.is_ret
  io.resp.bits.f1.preds.call_is_rvc := ftb_entry.call_is_rvc
  io.resp.bits.f1.meta              := FTBMeta(writeWay.asUInt(), f1_hit).asUInt()
  io.resp.bits.f1.ftb_entry         := ftb_entry

  when (RegNext(f1_hit)) {
    io.resp.bits.f2.preds.pred_target := RegNext(io.resp.bits.f1.preds.pred_target)
    io.resp.bits.f2.hit               := RegNext(io.resp.bits.f1.hit)
    io.resp.bits.f2.preds.is_br       := RegNext(io.resp.bits.f1.preds.is_br)
    io.resp.bits.f2.preds.is_jal      := RegNext(io.resp.bits.f1.preds.is_jal)
    io.resp.bits.f2.preds.is_call     := RegNext(io.resp.bits.f1.preds.is_call)
    io.resp.bits.f2.preds.is_ret      := RegNext(io.resp.bits.f1.preds.is_ret)
    io.resp.bits.f2.preds.call_is_rvc := RegNext(io.resp.bits.f1.preds.call_is_rvc)
    io.resp.bits.f2.meta              := RegNext(io.resp.bits.f1.meta)
    io.resp.bits.f2.ftb_entry         := RegNext(io.resp.bits.f1.ftb_entry)
  }

  io.flush_out.valid := io.resp_in(0).f1.preds.taken =/= io.resp_in(0).f2.preds.taken ||
                        io.resp_in(0).f1.preds.pred_target =/= io.resp_in(0).f2.preds.pred_target
  io.flush_out.bits := io.resp_in(0).f2.preds.pred_target

  // Update logic
  val update = io.update.bits

  val u_pc = update.pc

  val u_meta = update.meta.asTypeOf(new FTBMeta)
  val u_way = u_meta.writeWay
  val u_idx = ftbAddr.getIdx(u_pc)
  // val u_is_br = update.br_mask(update.cfi_idx.bits)
  val u_is_br = update.preds.is_br.reduce(_||_) && update.preds.taken
  // val u_taken = update.cfi_idx.valid && (update.jmp_valid || update.br_mask(update.cfi_idx.bits))
  val u_taken = update.preds.taken && (update.preds.is_jal || update.preds.is_br.reduce(_||_))

  val ftb_write = Wire(new FTBEntry)
  ftb_write.valid := true.B
  ftb_write.tag := ftbAddr.getTag(u_pc)

  val cfi_hit = update.meta.asTypeOf(new FTBMeta).hit
  val u_valid = RegNext(io.update.valid)
  val u_way_mask = UIntToOH(u_way)

  ftb.io.w.apply(u_valid, ftb_write, u_idx, u_way_mask)
}
