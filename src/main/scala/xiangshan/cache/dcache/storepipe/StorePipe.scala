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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientMetadata
import utils.{HasPerfEvents, XSDebug, XSPerfAccumulate}
import xiangshan.L1CacheErrorInfo

class DcacheStoreRequestIO(implicit p: Parameters) extends DCacheBundle {
  val cmd = UInt(M_SZ.W)
  val vaddr = UInt(VAddrBits.W)
  val instrtype   = UInt(sourceTypeWidth.W)
}

class DCacheStoreIO(implicit p: Parameters) extends DCacheBundle {
  // Paddr in STA s1, used for hit check
  val s1_paddr = Output(UInt(PAddrBits.W))
  // TLB miss or Exception in STA s1, kill Dcache req
  val s1_kill = Output(Bool())
  // Access Fault or MMIO in STA s2, kill Dcache req
  val s2_kill = Output(Bool())
  // Debug PC
  val s2_pc = Output(UInt(VAddrBits.W))

  val req = DecoupledIO(new DcacheStoreRequestIO)
  val resp = Flipped(DecoupledIO(new DCacheBundle() {
    // this store misses (for now, not used)
    val miss = Bool()
    // this store needs replay (for now, not used)
    val replay = Bool()
    // tag error TODO: add logic
    val tag_error = Bool()
  }))
}
/** Non-Blocking Store Dcache Pipeline
  *
  *  Associated with STA Pipeline
  *  Issue a store write prefetch to dcache if miss (if EnableStorePrefetchAtIssue)
  *  Issue a prefetch train request to sms if miss (if EnableStorePrefetchSMS)
  *  Recieve prefetch request, Issue a store write prefetch to dcache if miss (if EnableStorePrefetchAtCommit or EnableStorePrefetchSPB)
  */
class StorePipe(id: Int)(implicit p: Parameters) extends DCacheModule{
  val io = IO(new DCacheBundle {
    // incoming requests
    val lsu = Flipped(new DCacheStoreIO)

    // meta and data array read port
    val meta_read = DecoupledIO(new MetaReadReq)
    val meta_resp = Input(Vec(nWays, new Meta))
    // TODO extra_meta_resp: error; prefetch; access (prefetch hit?)
    // val extra_meta_resp = Input(Vec(nWays, new DCacheExtraMeta))

    val tag_read = DecoupledIO(new TagReadReq)
    val tag_resp = Input(Vec(nWays, UInt(encTagBits.W)))

    // send miss request to dcache miss queue
    val miss_req = DecoupledIO(new MissReq)

    // update state vec in replacement algo, for now, set this as false
    val replace_access = ValidIO(new ReplacementAccessBundle)
    // find the way to be replaced
    val replace_way = new ReplacementWayReqIO

    // ecc error
    val error = Output(new L1CacheErrorInfo())
  })

  // TODO: error
  io.error := DontCare

/** S0:
  *   send tag and meta read req
  */
  val s0_valid = io.lsu.req.valid
  val s0_req = io.lsu.req.bits
  val s0_fire = io.lsu.req.fire

  io.meta_read.valid        := s0_valid
  io.meta_read.bits.idx     := get_idx(io.lsu.req.bits.vaddr)
  io.meta_read.bits.way_en  := ~0.U(nWays.W)

  io.tag_read.valid         := s0_valid
  io.tag_read.bits.idx      := get_idx(io.lsu.req.bits.vaddr)
  io.tag_read.bits.way_en   := ~0.U(nWays.W)

  io.lsu.req.ready := io.meta_read.ready && io.tag_read.ready

  XSPerfAccumulate("s0_valid", io.lsu.req.valid)
  XSPerfAccumulate("s0_valid_not_ready", io.lsu.req.valid && !io.lsu.req.ready)


/** S1:
  * get tag and meta read resp
  * judge hit or miss
  */
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))

  val s1_valid = RegNext(s0_fire)
  val s1_req = RegEnable(s0_req, s0_fire)

  val s1_meta_resp = io.meta_resp
  val s1_tag_resp  = io.tag_resp.map(tag => tag(tagBits - 1, 0))

  val s1_paddr = io.lsu.s1_paddr

  /**
    * get hit meta
    */
  val s1_tag_match = Wire(UInt(nWays.W))
  s1_tag_match := wayMap((wayid: Int) => {s1_tag_resp(wayid) === get_tag(s1_paddr) && s1_meta_resp(wayid).coh.isValid()}).asUInt
  val s1_fake_meta = Wire(new Meta)
  s1_fake_meta.coh := ClientMetadata.onReset
  val s1_hit_meta = Mux(s1_tag_match.orR, Mux1H(s1_tag_match, wayMap((wayid: Int) => {s1_meta_resp(wayid)})), s1_fake_meta)
  val s1_hit_coh = s1_hit_meta.coh

  val (s1_has_permission, _, s1_new_hit_coh) = s1_hit_coh.onAccess(s1_req.cmd)
  val s1_hit = s1_has_permission && s1_new_hit_coh === s1_hit_coh && s1_tag_match.orR

  /**
    * Don't choose a replace_way anymore
    */
  io.replace_way.set.valid := false.B
  io.replace_way.set.bits  := get_idx(s1_req.vaddr)
  io.replace_way.dmWay     := get_direct_map_way(s1_req.vaddr)

  val s1_need_replacement = !s1_tag_match.orR

/** S2:
  * miss: send a write hint to Dache
  * hit : update replace algrithom to make the hited line stay longer
  */
  val s2_valid = RegNext(s1_valid) && RegNext(!io.lsu.s1_kill)
  val s2_req = RegEnable(s1_req, s1_valid)

  val s2_hit = RegEnable(s1_hit, s1_valid)
  val s2_paddr = RegEnable(s1_paddr, s1_valid)
  val s2_hit_coh = RegEnable(s1_hit_coh, s1_valid)
  val s2_is_prefetch = RegEnable(s1_req.instrtype === DCACHE_PREFETCH_SOURCE.U, s1_valid)

  io.lsu.resp.valid := s2_valid
  io.lsu.resp.bits.miss := !s2_hit
  io.lsu.resp.bits.replay := false.B
  // TODO: consider tag error
  io.lsu.resp.bits.tag_error := false.B


  /**
    * send req to Dcache MissQueue
    */
  if(EnableStorePrefetchAtIssue) {
    // all miss stores, whether prefetched or normal, send requests directly to mshr
    io.miss_req.valid := s2_valid && !s2_hit
  }else {
    // only prefetched miss stores will send requests directly to mshr
    io.miss_req.valid := s2_valid && !s2_hit && s2_is_prefetch
  }
  io.miss_req.bits := DontCare
  // only send out a prefetch write to Dcache
  io.miss_req.bits.source := DCACHE_PREFETCH_SOURCE.U
  io.miss_req.bits.pf_source := L1_HW_PREFETCH_STORE
  io.miss_req.bits.cmd := MemoryOpConstants.M_PFW
  io.miss_req.bits.addr := get_block_addr(s2_paddr)
  io.miss_req.bits.vaddr := s2_req.vaddr
  io.miss_req.bits.req_coh := s2_hit_coh
  // TODO: consider tag error
  io.miss_req.bits.cancel := io.lsu.s2_kill
  io.miss_req.bits.pc := io.lsu.s2_pc

  /**
    * update replacer, for now, disable this
    */
  io.replace_access.valid := false.B
  io.replace_access.bits  := DontCare

  XSPerfAccumulate("store_fire", s2_valid && !io.lsu.s2_kill)
  XSPerfAccumulate("sta_hit",  s2_valid &&  s2_hit && !io.lsu.s2_kill)
  XSPerfAccumulate("sta_miss", s2_valid && !s2_hit && !io.lsu.s2_kill)
  XSPerfAccumulate("store_miss_prefetch_fire", io.miss_req.fire && !io.miss_req.bits.cancel)
  XSPerfAccumulate("store_miss_prefetch_not_fire", io.miss_req.valid && !io.miss_req.ready && !io.miss_req.bits.cancel)
}