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
import freechips.rocketchip.tilelink.TLPermissions
import utils._
import xiangshan._
import xiangshan.cache.wpu._

class ReplacePipeReq(implicit p: Parameters) extends ICacheBundle
{
  val paddr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val param  = UInt(TLPermissions.cWidth.W)
  val voluntary = Bool()
  val needData = Bool()
  val waymask = UInt(nWays.W)
  val id = UInt(ReplaceIdWid.W)

  def vidx = get_idx(vaddr)
  def ptag = get_phy_tag(paddr)
  def isProbe = !voluntary
  def isRelease = voluntary
}


class ICacheReplacePipe(implicit p: Parameters) extends ICacheModule{
  val io = IO(new Bundle{
    val pipe_req = Flipped(DecoupledIO(new ReplacePipeReq))
    val iwpu = Flipped(new IwpuBaseIO(nWays = nWays, nPorts = 1))
    val tagwriteUpd = Flipped(ValidIO(new WPUUpdate(nWays)))
    val meta_read = DecoupledIO(new ICacheReadBundle)
    val data_read = DecoupledIO(Vec(partWayNum, new ICacheReadBundle))
    val meta_response = Input(new ICacheMetaRespBundle)
    val data_response = Input(new ICacheDataRespBundle)

    val error      = Output(new L1CacheErrorInfo)

    val re_meta_read = DecoupledIO(new ICacheReadBundle)
    val re_data_read = DecoupledIO(Vec(partWayNum, new ICacheReadBundle))
    val re_meta_response = Input(new ICacheMetaRespBundle)
    val re_data_response = Input(new ICacheDataRespBundle)

    val meta_write = DecoupledIO(new ICacheMetaWriteBundle)

    val release_req = DecoupledIO(new ReleaseReq)
    val release_finish = Input(Bool())

    val pipe_resp = ValidIO(UInt(ReplaceIdWid.W))
    
    val status = new Bundle() {
      val r0_set, r1_set, r2_set, r3_set = ValidIO(UInt(idxBits.W))
    }

    val csr_parity_enable = Input(Bool())

  })

  val (toMeta, metaResp) =  (io.meta_read, io.meta_response.metaData(0))
  val (toData, dataResp) =  (io.data_read, io.data_response.datas(0))
  val (metaError, codeResp) = (io.meta_response.errors(0), io.data_response.codes(0))

  val (reToMeta, reMetaResp) =  (io.re_meta_read, io.re_meta_response)
  val (reToData, reDataResp) = (io.re_data_read, io.re_data_response)

  val r0_ready, r1_ready, r2_ready = WireInit(false.B)
  val r0_fire,  r1_fire , r2_fire, r3_fire  = WireInit(false.B)

  /**
    ******************************************************************************
    * ReplacePipe Stage 0
    ******************************************************************************
    */  

  val r0_valid = generatePipeControl(lastFire = io.pipe_req.fire(), thisFire = r0_fire, thisFlush = false.B, lastFlush = false.B)

  val r0_req         = RegEnable(io.pipe_req.bits, enable = io.pipe_req.fire())
  val r0_req_vidx    = r0_req.vidx
  val r0_way_mask = r0_req.waymask

  val array_req = List(toMeta, toData)

  r0_ready := array_req(0).ready && array_req(1).ready && r1_ready  || !r0_valid
  r0_fire  := r0_valid && r0_ready

  val p0_pred_way_en = Wire(UInt(nWays.W))
  // val iwpu = Module(new ICacheWpuWrapper(1))
  // iwpu.io.tagwrite_upd <> io.tagwriteUpd
  if (iwpuParam.enWPU) {
    io.iwpu.req(0).valid := r0_valid && r0_req.isProbe
    io.iwpu.req(0).bits.vaddr := r0_req.vaddr
    when(io.iwpu.resp(0).valid) {
      p0_pred_way_en := io.iwpu.resp(0).bits.s0_pred_way_en
    }.otherwise {
      p0_pred_way_en := 0.U(nWays.W)
    }
  } else {
    io.iwpu.req(0).valid := false.B
    io.iwpu.req(0).bits := DontCare
    p0_pred_way_en := ~0.U(nWays.W)
  }

  for(i <- 0 until partWayNum) {
    toData.valid                    :=  r0_valid
    toData.bits(i).isDoubleLine     :=  false.B
    toData.bits(i).vSetIdx(0)        :=  r0_req_vidx
    toData.bits(i).vSetIdx(1)        :=  DontCare
    // read one data
    toData.bits(i).way_en := VecInit(Seq(Mux(r0_req.isProbe, p0_pred_way_en, r0_way_mask), 0.U(nWays.W)))
  }

  toMeta.valid               := r0_valid
  toMeta.bits.isDoubleLine   :=false.B
  toMeta.bits.vSetIdx(0)        := r0_req_vidx
  toMeta.bits.vSetIdx(1)        := DontCare
  // read all meta
  toMeta.bits.way_en := DontCare

  io.pipe_req.ready := array_req(0).ready && array_req(1).ready && r1_ready
  
  io.status.r0_set.valid := r0_valid
  io.status.r0_set.bits  := r0_req.vidx

  /**
    ******************************************************************************
    * ReplacePipe Stage 1
    ******************************************************************************
    */

  val r1_resend_can_go = Wire(Bool())
  val replay_read_valid = Wire(Bool())
  val r1_valid = generatePipeControl(lastFire = r0_fire, thisFire = r1_fire, thisFlush = false.B, lastFlush = false.B)
  r1_ready := r1_resend_can_go && r2_ready  || !r1_valid
  r1_fire  := r1_valid && r1_ready


  val r1_req = RegEnable(r0_req, r0_fire)
  val r1_toDataBits = RegEnable(toData.bits, r0_fire)
  val r1_toMetaBits = RegEnable(toMeta.bits, r0_fire)
  val p1_pred_way_en = RegEnable(p0_pred_way_en, r0_fire)
  val r1_way_mask = RegEnable(r0_way_mask, r0_fire)

  val r1_meta_ptags              = ResultHoldBypass(data = VecInit(metaResp.map(way => way.tag)),valid = RegNext(r0_fire))
  val r1_meta_cohs               = ResultHoldBypass(data = VecInit(metaResp.map(way => way.coh)),valid = RegNext(r0_fire))
  val r1_meta_errors             = ResultHoldBypass(data = metaError, valid = RegNext(r0_fire))

  val r1_datas_line = ResultHoldBypass(data = dataResp, valid = RegNext(r0_fire))
  val r1_data_errorBits_line = ResultHoldBypass(data = codeResp, valid = RegNext(r0_fire))
  val r1_datas = Wire(UInt(blockBits.W))
  val r1_data_errorBits = Wire(UInt(dataCodeEntryBits.W))
  val p1_pred_fail_and_real_hit = Wire(Bool())

  /*** for Probe hit check ***/
  val probe_phy_tag   = r1_req.ptag
  val probe_hit_vec   = VecInit(r1_meta_ptags.zip(r1_meta_cohs).map{case(way_tag,way_coh) => way_tag === probe_phy_tag && way_coh.isValid()})
  val probe_hit_coh   = Mux1H(probe_hit_vec, r1_meta_cohs)

  /** wpu */
  io.iwpu.lookup_upd(0).valid := r1_valid && r1_req.isProbe
  io.iwpu.lookup_upd(0).bits.vaddr := r1_req.vaddr
  io.iwpu.lookup_upd(0).bits.s1_pred_way_en := p1_pred_way_en
  io.iwpu.lookup_upd(0).bits.s1_real_way_en := probe_hit_vec.asUInt

  replay_read_valid := r1_valid && p1_pred_fail_and_real_hit
  r1_resend_can_go := !replay_read_valid || reToData.ready && reToMeta.ready

  if(iwpuParam.enWPU){
    val r1_way_en = Mux(r1_req.isProbe, p1_pred_way_en, r1_way_mask)
    r1_datas := Mux1H(r1_way_en, r1_datas_line)
    r1_data_errorBits := Mux1H(r1_way_en, r1_data_errorBits_line)

    p1_pred_fail_and_real_hit := r1_valid && r1_req.isProbe && p1_pred_way_en =/= probe_hit_vec.asUInt && probe_hit_vec.asUInt.orR
    reToData.valid := replay_read_valid
    reToData.bits := r1_toDataBits
    for (i <- 0 until partWayNum) {
      reToData.bits(i).way_en := VecInit(Seq(probe_hit_vec.asUInt, 0.U(nWays.W)))
    }
    reToMeta.valid := replay_read_valid
    reToMeta.bits := r1_toMetaBits
  }else{
    val r1_way_en = Mux(r1_req.isProbe, probe_hit_vec.asUInt, r1_way_mask)
    r1_datas := Mux1H(r1_way_en, r1_datas_line)
    r1_data_errorBits := Mux1H(r1_way_en, r1_data_errorBits_line)

    p1_pred_fail_and_real_hit := false.B
    reToData.valid := false.B
    reToData.bits := DontCare
    reToMeta.valid := false.B
    reToMeta.bits := DontCare
  }
  XSPerfAccumulate("wpu_pred_total", RegNext(io.iwpu.req(0).valid) && io.iwpu.lookup_upd(0).valid)
  XSPerfAccumulate("count_first_send", r1_valid && r1_req.isProbe)
  XSPerfAccumulate("count_second_send", replay_read_valid)
  XSPerfAccumulate("resend_block", !r1_resend_can_go)

  /*** for Release way select ***/
  val release_waymask  = r1_req.waymask
  val (release_tag, release_coh)   = (Mux1H(release_waymask, r1_meta_ptags), Mux1H(release_waymask, r1_meta_cohs))
  val release_addr     = get_block_addr(Cat(release_tag, get_untag(r1_req.vaddr)) )

  when(RegNext(io.meta_read.fire()) && r1_req.isProbe){
    assert(PopCount(probe_hit_vec) <= 1.U, "Probe Multi-Hit")
  }

  io.status.r1_set.valid := r1_valid
  io.status.r1_set.bits  := r1_req.vidx

  /**
    ******************************************************************************
    * ReplacePipe Stage 2
    ******************************************************************************
    */
    
  val r2_valid          = generatePipeControl(lastFire = r1_fire, thisFire = r2_fire, thisFlush = false.B, lastFlush = false.B)

  r2_ready      := r2_valid && io.release_req.ready  || !r2_valid
  r2_fire       := r2_valid && io.release_req.ready

  val r2_req = RegEnable(r1_req, r1_fire)
  val p2_pred_fail_and_real_hit = RegEnable(p1_pred_fail_and_real_hit, r1_fire)
  val r2_datas = RegEnable(r1_datas, r1_fire)
  val r2_probe_hit_ptag = RegEnable(probe_phy_tag, r1_fire)

  /*** for Probe hit mux ***/
  // recheck resend-meta
  val r2_re_meta_ptags = ResultHoldBypass(data = VecInit(reMetaResp.metaData(0).map(way => way.tag)), valid = RegNext(r1_fire))
  val r2_re_meta_cohs = ResultHoldBypass(data = VecInit(reMetaResp.metaData(0).map(way => way.coh)), valid = RegNext(r1_fire))
  val r2_re_meta_errors = ResultHoldBypass(data = reMetaResp.errors(0), valid = RegNext(r1_fire))
  val r2_re_datas_line = ResultHoldBypass(data = reDataResp.datas(0), valid = RegNext(r1_fire))
  val r2_re_data_errorBits_line = ResultHoldBypass(data = reDataResp.codes(0), valid = RegNext(r1_fire))
  val re_probe_hit_vec = VecInit(r2_re_meta_ptags.zip(r2_re_meta_cohs).map { case (way_tag, way_coh) => way_tag === r2_probe_hit_ptag && way_coh.isValid() })
  val re_probe_hit_coh = Mux1H(re_probe_hit_vec, r2_re_meta_cohs)
  val r2_re_datas = Mux1H(re_probe_hit_vec.asUInt, r2_re_datas_line)
  val r2_re_data_errorBits = Mux1H(re_probe_hit_vec.asUInt, r2_re_data_errorBits_line)

  val r2_probe_hit_data = Mux(p2_pred_fail_and_real_hit, r2_re_datas, r2_datas)
  val r2_data_errorBits = Mux(p2_pred_fail_and_real_hit, r2_re_data_errorBits, RegEnable(r1_data_errorBits, r1_fire))
  val r2_meta_errors = Mux(p2_pred_fail_and_real_hit, r2_re_meta_errors, RegEnable(r1_meta_errors, r1_fire))
  val r2_probe_hit_coh = Mux(p2_pred_fail_and_real_hit, re_probe_hit_coh, RegEnable(probe_hit_coh, r1_fire))
  val r2_probe_hit_vec = Mux(p2_pred_fail_and_real_hit, re_probe_hit_vec, RegEnable(probe_hit_vec, r1_fire))

  val (probe_has_dirty_data, probe_shrink_param, probe_new_coh) = r2_probe_hit_coh.onProbe(r2_req.param)

  val r2_parity_meta_error = r2_meta_errors.reduce(_||_) && io.csr_parity_enable
  val r2_parity_data_error = get_data_errors(r2_probe_hit_data, r2_data_errorBits, r1_fire) && io.csr_parity_enable
  val r2_parity_error      = RegNext(r2_parity_meta_error) || r2_parity_data_error


  io.error.valid                := RegNext(r2_parity_error &&  RegNext(RegNext(r1_fire))) 
  io.error.report_to_beu        := RegNext(r2_parity_error &&  RegNext(RegNext(r1_fire)))
  io.error.paddr                := RegNext(RegNext(r2_req.paddr))
  io.error.source.tag           := RegNext(RegNext(r2_parity_meta_error))
  io.error.source.data          := RegNext(r2_parity_data_error)
  io.error.source.l2            := false.B
  io.error.opType               := DontCare
  io.error.opType.fetch         := true.B
  io.error.opType.release       := RegNext(RegNext(r2_req.isRelease))
  io.error.opType.probe         := RegNext(RegNext(r2_req.isProbe))

  XSError(r2_parity_error && RegNext(RegNext(r1_fire)), "ICache has parity error in ReplacePipe!")


  /*** for Release mux ***/
  val r2_release_ptag = RegEnable(release_tag, r1_fire)
  val r2_release_coh  = RegEnable(release_coh, r1_fire)
  val r2_release_data = r2_datas
  val r2_release_addr = RegEnable(release_addr, r1_fire)

  val release_need_send = r2_valid &&  r2_req.isRelease && r2_release_coh.isValid()

  val (release_has_dirty_data, release_shrink_param, release_new_coh) = r2_release_coh.onCacheControl(M_FLUSH)

  /*** to Release Unit ***/
  val r2_paddr = Mux(r2_req.isProbe, r2_req.paddr , r2_release_addr)
  val r2_param = Mux(r2_req.isProbe, probe_shrink_param , release_shrink_param)
  val r2_hasData = r2_req.isProbe && r2_probe_hit_coh.isValid() && (r2_req.needData || probe_has_dirty_data) || r2_req.isRelease
  val r2_data  = Mux(r2_req.isProbe, r2_probe_hit_data , r2_release_data)

  val r2_write_tag = Mux(r2_req.isProbe, r2_probe_hit_ptag , r2_release_ptag)
  val r2_write_coh = Mux(r2_req.isProbe, probe_new_coh , release_new_coh)
  val r2_write_waymask = Mux(r2_req.isProbe, r2_probe_hit_vec.asUInt , r2_req.waymask)

  //release not write back
  io.meta_write.valid := r2_fire && r2_req.isProbe
  io.meta_write.bits.generate(vaddr = r2_req.vaddr, tag = r2_probe_hit_ptag, coh = probe_new_coh, idx = r2_req.vidx, waymask = r2_probe_hit_vec.asUInt, bankIdx = r2_req.vidx(0))

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
  // io.pipe_resp.valid := r2_fire && r2_req.isRelease
  // io.pipe_resp.bits  := r2_req.id

  io.status.r2_set.valid := r2_valid
  io.status.r2_set.bits  := r2_req.vidx

  /**
    ******************************************************************************
    * ReplacePipe Stage 3
    ******************************************************************************
    */

  val r3_valid          = generatePipeControl(lastFire = r2_fire && r2_req.isRelease, thisFire = r3_fire, thisFlush = false.B, lastFlush = false.B)
  val r3_release_need_send = RegEnable(release_need_send, r2_fire && r2_req.isRelease)

  r3_fire       := (r3_valid && RegNext(io.release_finish) && r3_release_need_send) || (r3_valid && !r3_release_need_send) 

  val r3_req = RegEnable(r2_req, r2_fire && r2_req.isRelease)

  io.pipe_resp.valid := r3_fire 
  io.pipe_resp.bits  := r3_req.id

  io.status.r3_set.valid := r3_valid
  io.status.r3_set.bits  := r3_req.vidx

}
