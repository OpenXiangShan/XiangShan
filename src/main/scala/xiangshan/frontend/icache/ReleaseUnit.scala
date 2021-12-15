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
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates, TLArbiter, TLBundleC, TLBundleD, TLEdgeOut, TLPermissions}
import xiangshan._
import utils._
import huancun.{DirtyField, DirtyKey}

class ReleaseReq(implicit p: Parameters) extends ICacheBundle{
  val addr = UInt(PAddrBits.W)
  val vidx  = UInt(idxBits.W)
  val param  = UInt(TLPermissions.cWidth.W)
  val voluntary = Bool()
  val hasData = Bool()
  val dirty = Bool()
  val data = UInt((blockBytes * 8).W)
  val waymask = UInt(nWays.W)
}

class ICacheReleaseBundle(implicit p: Parameters) extends  ICacheBundle{
  val req = Vec(2, Flipped(DecoupledIO(new ReleaseReq)))
}

class RealeaseEntry(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheModule
{
  val io = IO(new Bundle {
    val id = Input(UInt())
    val req = Flipped(DecoupledIO(new ReleaseReq))

    val mem_release = DecoupledIO(new TLBundleC(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
  })

  val s_invalid :: s_release_req :: s_release_resp :: s_meta_write :: Nil = Enum(4)
  val state = RegInit(s_invalid)

  val req  = Reg(new ReleaseReq)
  val req_ptag = get_phy_tag(req.addr)
  val req_idx  = req.vidx

  // internal regs
  // remaining beats
  val remain = RegInit(0.U(refillCycles.W))
  val remain_set = WireInit(0.U(refillCycles.W))
  val remain_clr = WireInit(0.U(refillCycles.W))
  remain := (remain | remain_set) & ~remain_clr

  val busy = remain.orR

  io.req.ready := state === s_invalid
  io.mem_grant.ready := false.B

  when (io.req.fire()) {
    req        := io.req.bits
    remain_set := ~0.U(refillCycles.W)
    state      := s_release_req
  }

  val beat = PriorityEncoder(remain)
  val beat_data = Wire(Vec(refillCycles, UInt(beatBits.W)))
  for (i <- 0 until refillCycles) {
    beat_data(i) := req.data((i + 1) * beatBits - 1, i * beatBits)
  }

  val probeResponse = edge.ProbeAck(
    fromSource = io.id,
    toAddress = addrAlign(req.addr, blockBytes, PAddrBits),
    lgSize = log2Ceil(cacheParams.blockBytes).U,
    reportPermissions = req.param
  )

  val probeResponseData = edge.ProbeAck(
    fromSource = io.id,
    toAddress = addrAlign(req.addr, blockBytes, PAddrBits),
    lgSize = log2Ceil(cacheParams.blockBytes).U,
    reportPermissions = req.param,
    data = beat_data(beat)
  )

    val voluntaryReleaseData = edge.Release(
    fromSource = io.id,
    toAddress = addrAlign(req.addr, blockBytes, PAddrBits),
    lgSize = log2Ceil(blockBytes).U,
    shrinkPermissions = req.param,
    data = beat_data(beat)
  )._2

  voluntaryReleaseData.echo.lift(DirtyKey).foreach(_ := req.dirty)

  io.mem_release.valid := Mux(!req.voluntary && req.hasData, busy,  state === s_release_req )
  io.mem_release.bits  := Mux(req.voluntary, voluntaryReleaseData, 
                            Mux(req.hasData,probeResponseData,probeResponse))

  when (io.mem_release.fire()) { remain_clr := PriorityEncoderOH(remain) }

  val (_, _, release_done, _) = edge.count(io.mem_release)

  when (state === s_release_req && release_done) {
    state := Mux(req.voluntary, s_release_resp, s_invalid)
  }

  // --------------------------------------------------------------------------------
  // receive ReleaseAck for Releases
  when (state === s_release_resp) {
    io.mem_grant.ready := true.B
    when (io.mem_grant.fire()) {
      state := Mux(req.voluntary,s_meta_write,s_invalid)
    }
  }

  when(state === s_meta_write) {
      state := s_invalid
  }

}

class ReleaseUnit(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheModule
{
  val io = IO(new Bundle {
    val req = Vec(2, Flipped(DecoupledIO(new ReleaseReq)))
    val mem_release = DecoupledIO(new TLBundleC(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
  })

  val req = io.req
  io.mem_release.valid := false.B
  io.mem_release.bits  := DontCare
  io.mem_grant.ready   := false.B

  val entries = (0 until cacheParams.nReleaseEntries) map { i =>
    val entry = Module(new RealeaseEntry(edge))

    entry.io.id := i.U

    // entry req
    entry.io.req.valid := io.req(i).valid
    entry.io.req.bits  := io.req(i).bits
    io.req(i).ready    := entry.io.req.ready

    entry.io.mem_grant.valid := (i.U === io.mem_grant.bits.source) && io.mem_grant.valid
    entry.io.mem_grant.bits  := io.mem_grant.bits
    when (i.U === io.mem_grant.bits.source) {
      io.mem_grant.ready := entry.io.mem_grant.ready
    }


    entry
  }
  TLArbiter.robin(edge, io.mem_release, entries.map(_.io.mem_release):_*)

}