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

class RefillPipeReq(implicit p: Parameters) extends DCacheBundle {
  val source = UInt(sourceTypeWidth.W)
  val addr = UInt(PAddrBits.W)
  val way_en = UInt(DCacheWays.W)
  val wmask = UInt(DCacheBanks.W)
  val data = Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W))
  val meta = new Meta
  val alias = UInt(2.W) // TODO: parameterize

  val miss_id = UInt(log2Up(cfg.nMissEntries).W)

  val id = UInt(reqIdWidth.W)
  val error = Bool()

  def paddrWithVirtualAlias: UInt = {
    Cat(alias, addr(DCacheSameVPAddrLength - 1, 0))
  }
  def idx: UInt = get_idx(paddrWithVirtualAlias)
}

class RefillPipe(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new RefillPipeReq))
    val req_dup_0 = Input(Valid(new RefillPipeReq))
    val req_dup_1 = Input(Valid(new RefillPipeReq))
    val req_dup_2 = Input(Valid(new RefillPipeReq))
    val req_dup_3 = Input(Valid(new RefillPipeReq))
    val resp = ValidIO(UInt(log2Up(cfg.nMissEntries).W))

    val data_write = DecoupledIO(new L1BankedDataWriteReq)
    val meta_write = DecoupledIO(new MetaWriteReq)
    val error_flag_write = DecoupledIO(new ErrorWriteReq)
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

  val req_dup_0 = io.req_dup_0.bits
  val req_dup_1 = io.req_dup_1.bits
  val req_dup_2 = io.req_dup_2.bits
  val req_dup_3 = io.req_dup_3.bits

  io.req.ready := true.B
  io.resp.valid := io.req.fire()
  io.resp.bits := refill_w_req.miss_id

  val idx = refill_w_req.idx
  val tag = get_tag(refill_w_req.addr)

  io.data_write.valid := io.req_dup_0.valid
  io.data_write.bits.addr := req_dup_0.paddrWithVirtualAlias
  io.data_write.bits.way_en := req_dup_0.way_en
  io.data_write.bits.wmask := refill_w_req.wmask
  io.data_write.bits.data := refill_w_req.data

  io.meta_write.valid := io.req_dup_1.valid
  io.meta_write.bits.idx := req_dup_1.idx
  io.meta_write.bits.way_en := req_dup_1.way_en
  io.meta_write.bits.meta := refill_w_req.meta

  io.error_flag_write.valid := io.req_dup_2.valid
  io.error_flag_write.bits.idx := req_dup_2.idx
  io.error_flag_write.bits.way_en := req_dup_2.way_en
  io.error_flag_write.bits.error := refill_w_req.error

  io.tag_write.valid := io.req_dup_3.valid
  io.tag_write.bits.idx := req_dup_3.idx
  io.tag_write.bits.way_en := req_dup_3.way_en
  io.tag_write.bits.tag := tag

  io.store_resp.valid := refill_w_valid && refill_w_req.source === STORE_SOURCE.U
  io.store_resp.bits := DontCare
  io.store_resp.bits.miss := false.B
  io.store_resp.bits.replay := false.B
  io.store_resp.bits.id := refill_w_req.id

  io.release_wakeup.valid := refill_w_valid
  io.release_wakeup.bits := refill_w_req.miss_id
}
