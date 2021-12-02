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

  def paddrWithVirtualAlias: UInt = {
    Cat(alias, addr(DCacheSameVPAddrLength - 1, 0))
  }
  def idx: UInt = get_idx(paddrWithVirtualAlias)
}

class RefillPipe(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new RefillPipeReq))
    val resp = ValidIO(UInt(log2Up(cfg.nMissEntries).W))

    val data_write = DecoupledIO(new L1BankedDataWriteReq)
    val meta_write = DecoupledIO(new MetaWriteReq)
    val tag_write = DecoupledIO(new TagWriteReq)
    val store_resp = ValidIO(new DCacheLineResp)
    val release_wakeup = ValidIO(UInt(log2Up(cfg.nMissEntries).W))
    val replace_access = ValidIO(new ReplacementAccessBundle)
  })

  // Assume that write in refill pipe is always ready
  assert(RegNext(io.data_write.ready))
  assert(RegNext(io.meta_write.ready))
  assert(RegNext(io.tag_write.ready))

  val s0_fire, s1_ready, s1_can_go, s1_fire = Wire(Bool())
  val s1_valid = RegInit(false.B)
  val s1_req = RegEnable(io.req.bits, s0_fire)
  when (s0_fire) {
    s1_valid := true.B
  }.elsewhen (s1_fire) {
    s1_valid := false.B
  }
  s0_fire := io.req.valid && s1_ready
  s1_ready := !s1_valid || s1_fire
  s1_fire := s1_valid && s1_can_go
  s1_can_go := io.data_write.ready && io.meta_write.ready && io.tag_write.ready
  assert(RegNext(s1_can_go))

  io.req.ready := s1_ready
  io.resp.valid := s1_fire
  io.resp.bits := s1_req.miss_id

  val idx = s1_req.idx
  val tag = get_tag(s1_req.addr)

  io.data_write.valid := s1_valid
  io.data_write.bits.addr := s1_req.paddrWithVirtualAlias
  io.data_write.bits.way_en := s1_req.way_en
  io.data_write.bits.wmask := s1_req.wmask
  io.data_write.bits.data := s1_req.data

  io.meta_write.valid := s1_valid
  io.meta_write.bits.idx := idx
  io.meta_write.bits.way_en := s1_req.way_en
  io.meta_write.bits.meta := s1_req.meta
  io.meta_write.bits.tag := tag

  io.tag_write.valid := s1_valid
  io.tag_write.bits.idx := idx
  io.tag_write.bits.way_en := s1_req.way_en
  io.tag_write.bits.tag := tag

  io.store_resp.valid := s1_fire && s1_req.source === STORE_SOURCE.U
  io.store_resp.bits := DontCare
  io.store_resp.bits.miss := false.B
  io.store_resp.bits.replay := false.B
  io.store_resp.bits.id := s1_req.id

  io.release_wakeup.valid := s1_fire
  io.release_wakeup.bits := s1_req.miss_id

  io.replace_access.valid := s1_fire
  io.replace_access.bits.set := idx
  io.replace_access.bits.way := OHToUInt(s1_req.way_en)
}
