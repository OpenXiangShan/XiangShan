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

package xiangshan.frontend.icache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates, TLPermissions}
import xiangshan._
import utils._

class ReplacePipeReq(implicit p: Parameters) extends ICacheBundle
{
  val paddr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val param  = UInt(TLPermissions.cWidth.W)
  val voluntary = Bool()
  val waymask = UInt(nWays.W)
  val id = UInt(ReplaceIdWid.W)

  def vidx = get_idx(vaddr)
  def ptag = get_phy_tag(paddr)
  def isProbe = !voluntary
  def isRelease = voluntary
}


class ReplacePipe(implicit p: Parameters) extends ICacheModule{
  val io = IO(new Bundle{
    val pipe_req = Flipped(DecoupledIO(new ReplacePipeReq))

    val meta_read = DecoupledIO(new ICacheReadBundle)
    val data_read = DecoupledIO(new ICacheReadBundle)

    val meta_response     = Input(new ICacheMetaRespBundle)
    val data_response     = Input(new ICacheDataRespBundle)

    val meta_write = DecoupledIO(new ICacheMetaWriteBundle)

    val release_req = DecoupledIO(new ReleaseReq)

    val pipe_resp = ValidIO(UInt(ReplaceIdWid.W))
    
    val status = new Bundle() {
      val r1_set, r2_set = ValidIO(UInt(idxBits.W))
    }
  })

  val (toMeta, metaResp) =  (io.meta_read, io.meta_response.metaData(0))
  val (toData, dataResp) =  (io.data_read, io.data_response.datas(0))

  val r0_ready, r1_ready, r2_ready = WireInit(false.B)
  val r0_fire,  r1_fire , r2_fire  = WireInit(false.B)

  val r0_valid       = io.pipe_req.valid

  val r0_req         = io.pipe_req.bits
  val r0_req_vidx    = r0_req.vidx

  r0_fire        := io.pipe_req.fire()

  val array_req = List(toMeta, toData)
  for(i <- 0 until 2) {
    array_req(i).valid             := r0_valid
    array_req(i).bits.isDoubleLine := false.B
    array_req(i).bits.vSetIdx(0)   := r0_req_vidx
    array_req(i).bits.vSetIdx(1)   := DontCare
  }

  io.pipe_req.ready := array_req(0).ready && array_req(1).ready && r1_ready

  //---------------------------------------------

  //---------------------------------------------

  val r1_valid = generatePipeControl(lastFire = r0_fire, thisFire = r1_fire, thisFlush = false.B, lastFlush = false.B)
  r1_ready := r2_ready  || !r1_valid
  r1_fire  := r1_valid && r2_ready


  val r1_req = RegEnable(next = r0_req, enable = r0_fire)

  val r1_meta_ptags              = ResultHoldBypass(data = VecInit(metaResp.map(way => way.tag)),valid = RegNext(r0_fire))
  val r1_meta_cohs               = ResultHoldBypass(data = VecInit(metaResp.map(way => way.coh)),valid = RegNext(r0_fire))
  val r1_data_cacheline          = ResultHoldBypass(VecInit(dataResp.map(way => way)),valid = RegNext(r0_fire))

  /*** for Probe hit check ***/
  val probe_phy_tag   = r1_req.ptag
  val probe_hit_vec   = VecInit(r1_meta_ptags.zip(r1_meta_cohs).map{case(way_tag,way_coh) => way_tag === probe_phy_tag && way_coh.isValid()})
  val probe_hit_coh   = Mux1H(probe_hit_vec, r1_meta_cohs)

  /*** for Release way select ***/
  val release_waymask  = r1_req.waymask
  val (release_tag, release_coh)   = (Mux1H(release_waymask, r1_meta_ptags), Mux1H(release_waymask, r1_meta_cohs))
  val release_addr     = get_block_addr(Cat(release_tag, get_untag(r1_req.vaddr)) )

  when(RegNext(io.meta_read.fire()) && r1_req.isProbe){
    assert(PopCount(probe_hit_vec) <= 1.U, "Probe Multi-Hit")
  }

  io.status.r1_set.valid := r1_valid
  io.status.r1_set.bits  := r1_req.vidx

  //---------------------------------------------

  //---------------------------------------------
  val r2_valid          = generatePipeControl(lastFire = r1_fire, thisFire = r2_fire, thisFlush = false.B, lastFlush = false.B)

  r2_ready      := r2_valid && io.release_req.ready  || !r2_valid
  r2_fire       := r2_valid && io.release_req.ready

  val r2_req = RegEnable(next = r1_req, enable = r1_fire)
  val r2_data_cacheline = RegEnable(next = r1_data_cacheline, enable = r1_fire)

  /*** for Probe hit mux ***/
  val r2_probe_hit_ptag =   RegEnable(next = probe_phy_tag, enable = r1_fire)
  val r2_probe_hit_vec = RegEnable(next = probe_hit_vec, enable = r1_fire)
  val r2_probe_hit_coh = RegEnable(next = probe_hit_coh, enable = r1_fire)
  val r2_probe_hit_data = Mux1H(r2_probe_hit_vec, r2_data_cacheline)

  val (probe_has_dirty_data, probe_shrink_param, probe_new_coh) = r2_probe_hit_coh.onProbe(r2_req.param)

  /*** for Release mux ***/
  val r2_release_ptag = RegEnable(next = release_tag, enable = r1_fire)
  val r2_release_coh  = RegEnable(next = release_coh, enable = r1_fire)
  val r2_release_data = Mux1H(r2_req.waymask, r2_data_cacheline)
  val r2_release_addr = RegEnable(next = release_addr, enable = r1_fire)

  val release_need_send = r2_valid &&  r2_req.isRelease && r2_release_coh.isValid()

  val (release_has_dirty_data, release_shrink_param, release_new_coh) = r2_release_coh.onCacheControl(M_FLUSH)

  /*** to Release Unit ***/
  val r2_paddr = Mux(r2_req.isProbe, r2_req.paddr , r2_release_addr)
  val r2_param = Mux(r2_req.isProbe, probe_shrink_param , release_shrink_param)
  val r2_hasData = true.B//r2_req.isProbe || release_has_dirty_data
  val r2_data  = Mux(r2_req.isProbe, r2_probe_hit_data , r2_release_data)

  val r2_write_tag = Mux(r2_req.isProbe, r2_probe_hit_ptag , r2_release_ptag)
  val r2_write_coh = Mux(r2_req.isProbe, probe_new_coh , release_new_coh)
  val r2_write_waymask = Mux(r2_req.isProbe, r2_probe_hit_vec.asUInt , r2_req.waymask)

  //release not write back
  io.meta_write.valid := r2_fire && r2_req.isProbe
  io.meta_write.bits.generate(tag = r2_probe_hit_ptag, coh = probe_new_coh, idx = r2_req.vidx, waymask = r2_probe_hit_vec.asUInt, bankIdx = r2_req.vidx(0))

  //NToN Release should not been send to slave
  io.release_req.valid          := r2_valid && (r2_req.isProbe || release_need_send)
  io.release_req.bits.addr      := r2_paddr
  io.release_req.bits.param     := r2_param
  io.release_req.bits.voluntary := r2_req.voluntary
  io.release_req.bits.hasData   := r2_hasData
  io.release_req.bits.data      := r2_data
  io.release_req.bits.dirty     := release_has_dirty_data || probe_has_dirty_data
  io.release_req.bits.waymask   := DontCare
  io.release_req.bits.vidx      := DontCare

  //response to MissQueue
  io.pipe_resp.valid := r2_fire && r2_req.isRelease
  io.pipe_resp.bits  := r2_req.id

  io.status.r2_set.valid := r2_valid
  io.status.r2_set.bits  := r2_req.vidx

}