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

    val meta_read = DecoupledIO(new ICacheReadBundle)
    val data_read = DecoupledIO(new ICacheReadBundle)

    val error      = Output(new L1CacheErrorInfo)

    val meta_response     = Input(new ICacheMetaRespBundle)
    val data_response     = Input(new ICacheDataRespBundle)

    val meta_write = DecoupledIO(new ICacheMetaWriteBundle)

    val release_req = DecoupledIO(new ReleaseReq)
    val release_finish = Input(Bool())

    val pipe_resp = ValidIO(UInt(ReplaceIdWid.W))
    
    val status = new Bundle() {
      val r1_set, r2_set, r3_set = ValidIO(UInt(idxBits.W))
    }

    val csr_parity_enable = Input(Bool())

  })

  val (toMeta, metaResp) =  (io.meta_read, io.meta_response.metaData(0))
  val (toData, dataResp) =  (io.data_read, io.data_response.datas(0))
  val (metaError, codeResp) = (io.meta_response.errors(0), io.data_response.codes(0))

  val r0_ready, r1_ready, r2_ready = WireInit(false.B)
  val r0_fire,  r1_fire , r2_fire, r3_fire  = WireInit(false.B)

  /**
    ******************************************************************************
    * ReplacePipe Stage 0
    ******************************************************************************
    */  

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

  /**
    ******************************************************************************
    * ReplacePipe Stage 1
    ******************************************************************************
    */  
  
  val r1_valid = generatePipeControl(lastFire = r0_fire, thisFire = r1_fire, thisFlush = false.B, lastFlush = false.B)
  r1_ready := r2_ready  || !r1_valid
  r1_fire  := r1_valid && r2_ready


  val r1_req = RegEnable(r0_req, r0_fire)

  val r1_meta_ptags              = ResultHoldBypass(data = VecInit(metaResp.map(way => way.tag)),valid = RegNext(r0_fire))
  val r1_meta_cohs               = ResultHoldBypass(data = VecInit(metaResp.map(way => way.coh)),valid = RegNext(r0_fire))
  val r1_meta_errors             = ResultHoldBypass(data = metaError, valid = RegNext(r0_fire))

  val r1_data_cacheline          = ResultHoldBypass(data = VecInit(dataResp.map(way => way)),valid = RegNext(r0_fire))
  val r1_data_errorBits          = ResultHoldBypass(data = VecInit(codeResp.map(way => way)), valid = RegNext(r0_fire))


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

  /**
    ******************************************************************************
    * ReplacePipe Stage 2
    ******************************************************************************
    */
    
  val r2_valid          = generatePipeControl(lastFire = r1_fire, thisFire = r2_fire, thisFlush = false.B, lastFlush = false.B)

  r2_ready      := r2_valid && io.release_req.ready  || !r2_valid
  r2_fire       := r2_valid && io.release_req.ready

  val r2_req = RegEnable(r1_req, r1_fire)
  val r2_data_cacheline = RegEnable(r1_data_cacheline, r1_fire)

  /*** for Probe hit mux ***/
  val r2_probe_hit_ptag =   RegEnable(probe_phy_tag, r1_fire)
  val r2_probe_hit_vec = RegEnable(probe_hit_vec, r1_fire)
  val r2_probe_hit_coh = RegEnable(probe_hit_coh, r1_fire)
  val r2_probe_hit_data = Mux1H(r2_probe_hit_vec, r2_data_cacheline)

  val (probe_has_dirty_data, probe_shrink_param, probe_new_coh) = r2_probe_hit_coh.onProbe(r2_req.param)



  val r2_meta_errors    = RegEnable(r1_meta_errors,    r1_fire)
  val r2_data_errorBits = RegEnable(r1_data_errorBits, r1_fire)

  val r2_data_errors    = Wire(Vec(nWays, Bool()))

  val read_datas = r2_data_cacheline.asTypeOf(Vec(nWays,Vec(dataCodeUnitNum, UInt(dataCodeUnit.W))))
  val read_codes = r2_data_errorBits.asTypeOf(Vec(nWays,Vec(dataCodeUnitNum, UInt(dataCodeBits.W))))
  val data_full_wayBits = VecInit((0 until nWays).map( w => 
                                VecInit((0 until dataCodeUnitNum).map(u => 
                                      Cat(read_codes(w)(u), read_datas(w)(u))))))
  val data_error_wayBits = VecInit((0 until nWays).map( w => 
                                VecInit((0 until dataCodeUnitNum).map(u => 
                                      cacheParams.dataCode.decode(data_full_wayBits(w)(u)).error ))))
  (0 until nWays).map{ w => r2_data_errors(w) := RegNext(RegNext(r1_fire)) && RegNext(data_error_wayBits(w)).reduce(_||_) } 

  val r2_parity_meta_error = r2_meta_errors.reduce(_||_) && io.csr_parity_enable
  val r2_parity_data_error = r2_data_errors.reduce(_||_) && io.csr_parity_enable
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
  val r2_release_data = Mux1H(r2_req.waymask, r2_data_cacheline)
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
