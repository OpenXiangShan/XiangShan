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
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates}
import utils.{OneHot, ParallelMux}

class ReplacePipeReq(implicit p: Parameters) extends DCacheBundle {
  val miss_id = UInt(log2Up(cfg.nMissEntries).W)
  val way_en = UInt(DCacheWays.W)

  // if dcache size > 32KB, vaddr is also needed for store
  // vaddr is used to get extra index bits
  val vaddr  = UInt(VAddrBits.W)

  val tag = UInt(tagBits.W) // tag of the block to be replaced
}

class ReplacePipeResp(implicit p: Parameters) extends DCacheBundle {
  val miss_id = UInt(log2Up(cfg.nMissEntries).W)
}

class ReplacePipe(implicit p: Parameters) extends  DCacheModule {
  def metaAndTagOnReset = MetaAndTag(ClientMetadata.onReset, 0.U)
  // enc bits encode both tag and meta, but is saved in meta array
  val encMetaBits = cacheParams.tagCode.width(metaAndTagOnReset.getWidth) - tagBits
  val metaBits = (new Meta).getWidth

  val io = IO(new Bundle() {
    // miss queue
    val req = Flipped(DecoupledIO(new ReplacePipeReq))
    // s1 resp when the block is already probed
    // s2 resp when the block is sent to wbq
    val resp = Vec(numReplaceRespPorts, ValidIO(new ReplacePipeResp))

    // write-back queue
    val wb = DecoupledIO(new WritebackReq)

    // read data array, invalid meta array
    val data_read = DecoupledIO(new L1BankedDataReadLineReq)
    val data_resp = Input(Vec(DCacheBanks, new L1BankedDataReadResult))
    val meta_read = DecoupledIO(new MetaReadReq)
    val meta_resp = Input(Vec(nWays, UInt(encMetaBits.W)))
    val meta_write = DecoupledIO(new MetaWriteReq)

    val status = new Bundle() {
      val s1_set, s2_set = ValidIO(UInt(idxBits.W))
    }
  })

  require(numReplaceRespPorts >= 2)

  val s0_fire = Wire(Bool())
  val s1_ready, s2_ready = Wire(Bool())
  val s1_valid, s2_valid = RegInit(false.B)
  val s1_fire, s2_fire = Wire(Bool())

  val s1_resp = io.resp.init.last
  val s2_resp = io.resp.last

  // meta array is made of regs, so meta write or read should always be ready
  assert(RegNext(io.meta_write.ready))
  assert(RegNext(io.meta_read.ready))
  when (s0_fire) {
    assert(PopCount(io.req.bits.way_en) <= 1.U)
  }

  // s0: read meta to be replaced
  val s0_can_go = s1_ready && io.meta_read.ready
  s0_fire := io.req.valid && s0_can_go

  // s1: invalid meta that is going to be replaced
  val s1_req = RegEnable(io.req.bits, s0_fire)
  val s1_idx = addr_to_dcache_set(s1_req.vaddr)
  val s1_coh = Wire(new ClientMetadata)
  val s1_need_release = s1_coh.state =/= ClientStates.Nothing
  val s1_need_data = if (dcacheParameters.alwaysReleaseData){
    s1_coh.state =/= ClientStates.Nothing
  } else {
    s1_coh.state === ClientStates.Dirty
  }
  val s1_can_go_to_s2 = s1_need_release && s2_ready && (io.data_read.ready || !s1_need_data)
  val s1_can_go_to_mq = !s1_need_release
  val s1_can_go = s1_can_go_to_s2 || s1_can_go_to_mq
  val s1_fire_to_s2 = s1_valid && s1_can_go_to_s2
  val s1_fire_to_mq = s1_valid && s1_can_go_to_mq
  s1_fire := s1_valid && s1_can_go
  s1_ready := !s1_valid || s1_fire
  when (s0_fire) {
    s1_valid := true.B
  }.elsewhen(s1_fire) {
    s1_valid := false.B
  }

  val meta_resp_ecc = ParallelMux(s1_req.way_en.asBools zip io.meta_resp)
  val meta_resp_raw = meta_resp_ecc(metaBits - 1, 0).asTypeOf(new Meta)
  s1_coh := Mux(RegNext(s0_fire), meta_resp_raw.coh, RegNext(s1_coh))

  // s2: invalid meta that is going to be replaced
  val s2_req = RegEnable(s1_req, s1_fire_to_s2)
  val s2_idx = addr_to_dcache_set(s2_req.vaddr)
  val s2_coh = RegEnable(s1_coh, s1_fire_to_s2)
  val s2_need_data = RegEnable(s1_need_data, s1_fire_to_s2)
  val s2_can_go = /*io.meta_write.ready && */io.wb.ready
  s2_fire := s2_valid && s2_can_go
  s2_ready := !s2_valid || s2_fire
  when (s1_fire_to_s2) {
    s2_valid := true.B
  }.elsewhen (s2_fire) {
    s2_valid := false.B
  }

  val data_resp_raw = WireInit(VecInit(io.data_resp.map(_.raw_data)))
  val s2_data = Wire(Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W)))
  s2_data := Mux(RegNext(s1_fire_to_s2), data_resp_raw, RegNext(s2_data))

  io.req.ready := s0_can_go

  io.data_read.valid := s1_valid && s1_need_data && s2_ready
  io.data_read.bits.way_en := s1_req.way_en
  io.data_read.bits.addr := s1_req.vaddr
  io.data_read.bits.rmask := ~0.U(DCacheBanks.W)

  io.meta_read.valid := io.req.valid && s1_ready
  io.meta_read.bits.idx := addr_to_dcache_set(io.req.bits.vaddr)
  io.meta_read.bits.way_en := io.req.bits.way_en

  io.meta_write.valid := false.B // s2_valid && io.wb.ready
  io.meta_write.bits.idx := s2_idx
  io.meta_write.bits.way_en := s2_req.way_en
  io.meta_write.bits.meta.coh := ClientMetadata.onReset
  io.meta_write.bits.tag := s2_req.tag // only used to calculate ecc

  io.wb.valid := s2_valid// && io.meta_write.ready
  io.wb.bits.addr := get_block_addr(Cat(s2_req.tag, get_untag(s2_req.vaddr)))
  val (_, release_param, _) = s2_coh.onCacheControl(M_FLUSH)
  io.wb.bits.param := release_param
  io.wb.bits.voluntary := true.B
  io.wb.bits.hasData := s2_need_data
  io.wb.bits.dirty := s2_coh.state === ClientStates.Dirty
  io.wb.bits.data := s2_data.asUInt
  io.wb.bits.delay_release := true.B
  io.wb.bits.miss_id := s2_req.miss_id

  s1_resp.valid := s1_fire_to_mq
  s1_resp.bits.miss_id := s1_req.miss_id

  s2_resp.valid := s2_fire
  s2_resp.bits.miss_id := s2_req.miss_id

  io.status.s1_set.valid := s1_valid
  io.status.s1_set.bits := s1_idx
  io.status.s2_set.valid := s2_valid
  io.status.s2_set.bits := s2_idx

  // Theoretically s1 and s2 won't resp to the same miss entry
  assert(RegNext(!s1_resp.valid || !s2_resp.valid || s1_resp.bits.miss_id =/= s2_resp.bits.miss_id))
}
