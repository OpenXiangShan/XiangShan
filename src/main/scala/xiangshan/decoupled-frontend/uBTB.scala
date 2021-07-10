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
  val num_ways = 16
  val tag_size = 20
  val lower_bit_size = 20
  val untaggedBits = log2Up(PredictWidth) + instOffsetBits
  val num_br = 1
}

@chiselName
class MicroBTB(implicit p: Parameters) extends BasePredictor
  with MicroBTBParams
{
  def getTag(pc: UInt)  = (pc >> untaggedBits)(tag_size-1, 0)
  def getBank(pc: UInt) = pc(log2Ceil(PredictWidth), instOffsetBits)

  class MicroBTBMeta extends XSBundle
  {
    val is_Br = Vec(num_br, Bool())
    val is_RVC = Bool()
    val valid = Bool()
    val pred = UInt(2.W)
    val tag = UInt(tag_size.W)
  }

  class MicroBTBData extends XSBundle
  {
    val lower = UInt(lower_bit_size.W)
  }

  class ReadResp extends XSBundle
  {
    val valid = Bool()
    val taken = Bool()
    val target = UInt(VAddrBits.W)
    val is_RVC = Bool()
    val is_Br = Vec(num_br, Bool())
  }

  class UBTBBank(val nWays: Int) extends XSModule with HasIFUConst with BPUUtils {
    val io = IO(new Bundle {
      val read_pc = Flipped(Valid(UInt(VAddrBits.W)))
      val read_resp = Output(new ReadResp)
      val read_hit = Output(Bool())

      val update_write_meta = Flipped(Valid(new MicroBTBMeta))
      val update_write_data = Flipped(Valid(new MicroBTBData))
      val update_taken = Input(Bool())
    })

    val debug_io = IO(new Bundle {
      val read_hit = Output(Bool())
      val read_hit_way = Output(UInt(log2Ceil(nWays).W))

      val update_hit = Output(Bool())
      val update_hit_way = Output(UInt(log2Ceil(nWays).W))
      val update_write_way = Output(UInt(log2Ceil(nWays).W))
      val update_old_pred = Output(UInt(2.W))
      val update_new_pred = Output(UInt(2.W))
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

    val packetAlignedPC = packetAligned(io.read_pc.bits)
    val read_tag = getTag(io.read_pc.bits)

    val hits = VecInit(rmetas.map(m => m.valid && m.tag === read_tag))
    val takens = VecInit(rmetas.map(m => m.pred(1)))
    val hit_oh = hits.asUInt
    val hit_and_taken = VecInit((hits zip takens) map {case (h, t) => h && t}).asUInt.orR
    val hit_meta = ParallelMux(hits zip rmetas)
    val hit_data = ParallelMux(hits zip rdatas)
    val target = Cat(io.read_pc.bits(VAddrBits-1, lower_bit_size+instOffsetBits), hit_data.lower, 0.U(instOffsetBits.W))

    val ren = io.read_pc.valid
    io.read_resp.valid := ren
    io.read_resp.is_RVC := ren && hit_meta.is_RVC
    when(ren) {
      io.read_resp.is_Br := hit_meta.is_Br
    }.otherwise {
      io.read_resp.is_Br := 0.U(num_br.W)
    }
    io.read_resp.taken := ren && hit_and_taken
    io.read_resp.target := target
    io.read_hit := hit_oh.orR

    debug_io.read_hit := hit_oh.orR
    debug_io.read_hit_way := OHToUInt(hit_oh)

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
    val update_new_pred =
      Mux(update_hit,
        satUpdate(update_old_pred, 2, io.update_taken),
        Mux(io.update_taken, 3.U, 0.U))
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
    meta.io.wdata(0).pred := Mux(do_reset, 0.U(2.W), RegNext(update_new_pred))
    data.io.waddr(0) := Mux(do_reset, reset_way, RegNext(update_way))
    data.io.wen(0)   := do_reset || RegNext(io.update_write_data.valid)
    data.io.wdata(0) := Mux(do_reset,
      0.U.asTypeOf(new MicroBTBData),
      RegNext(io.update_write_data.bits))

    debug_io.update_hit := update_hit
    debug_io.update_hit_way := update_hit_way
    debug_io.update_write_way := update_way
    debug_io.update_old_pred := update_old_pred
    debug_io.update_new_pred := update_new_pred
  }

  val ubtbBanks = Module(new UBTBBank(num_ways))
  val banks = ubtbBanks.io
  val read_resps = banks.read_resp

  banks.read_pc.valid := s1_valid
  banks.read_pc.bits := RegNext(io.f0_pc)

  io.resp.bits.f1.preds.pred_target := Mux(banks.read_hit, read_resps.target, io.f0_pc + (FetchWidth*4).U)
  io.resp.bits.f1.preds.taken := read_resps.taken
  io.resp.bits.f1.preds.is_br := read_resps.is_Br
  io.resp.bits.f1.hit := banks.read_hit

  // Update logic
  val update = RegNext(io.update.bits)
  val u_valid = RegNext(io.update.valid)
  val u_pc = update.pc
  val u_taken = update.preds.taken

  val u_tag = getTag(u_pc)
  val u_target_lower = update.preds.pred_target(lower_bit_size-1+instOffsetBits, instOffsetBits)

  val data_write_valid = u_valid && u_taken
  val meta_write_valid = u_valid && (u_taken || update.preds.is_br.reduce(_||_))

  val update_write_datas = new MicroBTBData
  val update_write_metas = new MicroBTBMeta

  update_write_metas.is_Br := update.preds.is_br.reduce(_||_)
  update_write_metas.valid := true.B
  update_write_metas.tag := u_tag
  update_write_metas.pred := DontCare

  update_write_datas.lower := u_target_lower

  banks.update_write_meta.valid := meta_write_valid
  banks.update_write_meta.bits := update_write_metas
  banks.update_write_data.valid := data_write_valid
  banks.update_write_data.bits := update_write_datas
  banks.update_taken := u_taken

}
