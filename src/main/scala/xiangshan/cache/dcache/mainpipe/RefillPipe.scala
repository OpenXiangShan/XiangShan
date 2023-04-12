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

package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

class RefillPipeReqCtrl(implicit p: Parameters) extends DCacheBundle {
  val source = UInt(sourceTypeWidth.W)
  val vaddr = UInt(VAddrBits.W)
  val addr = UInt(PAddrBits.W)
  val way_en = UInt(DCacheWays.W)
  val alias = UInt(2.W) // TODO: parameterize

  val miss_id = UInt(log2Up(cfg.nMissEntries).W)

  val id = UInt(reqIdWidth.W)
  val error = Bool()
  val prefetch = Bool()
  val access = Bool()

  def paddrWithVirtualAlias: UInt = {
    Cat(alias, addr(DCacheSameVPAddrLength - 1, 0))
  }
  def idx: UInt = get_idx(paddrWithVirtualAlias)
}

class RefillPipeReq(implicit p: Parameters) extends RefillPipeReqCtrl {
  val wmask = UInt(DCacheBanks.W)
  val data = Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W))
  val meta = new Meta

  def getCtrl = {
    val ctrl = Wire(new RefillPipeReqCtrl)
    ctrl.source := source
    ctrl.vaddr := vaddr
    ctrl.addr := addr
    ctrl.way_en := way_en
    ctrl.alias := alias
    ctrl.miss_id := miss_id
    ctrl.id := id
    ctrl.error := error
    ctrl.prefetch := prefetch
    ctrl.access := access
    ctrl
  }
}

class RefillPipe(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new RefillPipeReq))
    // val req_dup_for_data_w = Input(Valid(new RefillPipeReqCtrl))
    val req_dup_for_data_w = Vec(DCacheBanks, Input(Valid(new RefillPipeReqCtrl)))
    val req_dup_for_meta_w = Input(Valid(new RefillPipeReqCtrl))
    val req_dup_for_tag_w = Input(Valid(new RefillPipeReqCtrl))
    val req_dup_for_err_w = Input(Valid(new RefillPipeReqCtrl))
    val resp = ValidIO(UInt(log2Up(cfg.nMissEntries).W))

    val data_write = DecoupledIO(new L1BankedDataWriteReq)
    val data_write_dup = Vec(DCacheBanks, Valid(new L1BankedDataWriteReqCtrl))
    val meta_write = DecoupledIO(new CohMetaWriteReq)
    val error_flag_write = DecoupledIO(new FlagMetaWriteReq)
    val prefetch_flag_write = DecoupledIO(new FlagMetaWriteReq)
    val access_flag_write = DecoupledIO(new FlagMetaWriteReq)
    val tag_write = DecoupledIO(new TagWriteReq)
    val store_resp = ValidIO(new DCacheLineResp)
    val release_wakeup = ValidIO(UInt(log2Up(cfg.nMissEntries).W))
  })

  // Assume that write in refill pipe is always ready
  assert(RegNext(io.data_write.ready))
  assert(RegNext(io.meta_write.ready))
  assert(RegNext(io.tag_write.ready))

  val refill_w_valid = io.req.valid
  val refill_w_req = io.req.bits

  val req_dup_for_meta_w = io.req_dup_for_meta_w.bits
  val req_dup_for_err_w = io.req_dup_for_err_w.bits
  val req_dup_for_tag_w = io.req_dup_for_tag_w.bits

  io.req.ready := true.B
  io.resp.valid := io.req.fire()
  io.resp.bits := refill_w_req.miss_id

  val idx = refill_w_req.idx
  val tag = get_tag(refill_w_req.addr)

  io.data_write.valid := io.req_dup_for_data_w(0).valid
  io.data_write.bits.addr := io.req_dup_for_data_w(0).bits.paddrWithVirtualAlias
  io.data_write.bits.way_en := io.req_dup_for_data_w(0).bits.way_en
  io.data_write.bits.wmask := refill_w_req.wmask
  io.data_write.bits.data := refill_w_req.data

  io.data_write_dup.zipWithIndex.foreach { case (w, bank) =>
    w.valid := io.req_dup_for_data_w(bank).valid
    w.bits.addr := io.req_dup_for_data_w(bank).bits.paddrWithVirtualAlias
    w.bits.way_en := io.req_dup_for_data_w(bank).bits.way_en
  }

  io.meta_write.valid := io.req_dup_for_meta_w.valid
  io.meta_write.bits.idx := req_dup_for_meta_w.idx
  io.meta_write.bits.way_en := req_dup_for_meta_w.way_en
  io.meta_write.bits.meta := refill_w_req.meta

  io.error_flag_write.valid := io.req_dup_for_err_w.valid
  io.error_flag_write.bits.idx := req_dup_for_err_w.idx
  io.error_flag_write.bits.way_en := req_dup_for_err_w.way_en
  io.error_flag_write.bits.flag := refill_w_req.error

  io.prefetch_flag_write.valid := io.req_dup_for_err_w.valid
  io.prefetch_flag_write.bits.idx := req_dup_for_err_w.idx
  io.prefetch_flag_write.bits.way_en := req_dup_for_err_w.way_en
  io.prefetch_flag_write.bits.flag := refill_w_req.prefetch

  io.access_flag_write.valid := io.req_dup_for_err_w.valid
  io.access_flag_write.bits.idx := req_dup_for_err_w.idx
  io.access_flag_write.bits.way_en := req_dup_for_err_w.way_en
  io.access_flag_write.bits.flag := refill_w_req.access

  io.tag_write.valid := io.req_dup_for_tag_w.valid
  io.tag_write.bits.idx := req_dup_for_tag_w.idx
  io.tag_write.bits.way_en := req_dup_for_tag_w.way_en
  io.tag_write.bits.tag := tag
  io.tag_write.bits.vaddr := refill_w_req.vaddr

  io.store_resp.valid := refill_w_valid && refill_w_req.source === STORE_SOURCE.U
  io.store_resp.bits := DontCare
  io.store_resp.bits.miss := false.B
  io.store_resp.bits.replay := false.B
  io.store_resp.bits.id := refill_w_req.id

  io.release_wakeup.valid := refill_w_valid
  io.release_wakeup.bits := refill_w_req.miss_id
}
