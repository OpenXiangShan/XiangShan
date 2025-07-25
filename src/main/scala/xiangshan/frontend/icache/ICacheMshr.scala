// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLEdgeOut
import org.chipsalliance.cde.config.Parameters
import utility.BoolStopWatch
import utility.MemReqSource
import utility.ReqSourceKey
import utility.XSPerfHistogram
import xiangshan.WfiReqBundle

class ICacheMshr(edge: TLEdgeOut, isFetch: Boolean, ID: Int)(implicit p: Parameters) extends ICacheModule {
  class ICacheMshrIO(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheBundle {
    val fencei: Bool         = Input(Bool())
    val flush:  Bool         = Input(Bool())
    val wfi:    WfiReqBundle = Flipped(new WfiReqBundle)

    // request from mainPipe / prefetchPipe
    val req: DecoupledIO[MissReqBundle] = Flipped(DecoupledIO(new MissReqBundle))
    // look up if the request is already in MSHR
    // NOTE: lookUps Vec(2) is not Vec(PortNumber), it's mainPipe + prefetchPipe
    val lookUps: Vec[MshrLookupBundle] = Flipped(Vec(2, new MshrLookupBundle))

    // send request to L2 (tilelink bus)
    val acquire: DecoupledIO[MshrAcquireBundle] = DecoupledIO(new MshrAcquireBundle(edge))
    // select the victim way when acquire fire
    val victimWay: UInt = Input(UInt(wayBits.W))

    // offer the information needed by responding to requester
    val info: Valid[MshrInfoBundle] = ValidIO(new MshrInfoBundle)
    // after respond to requester, invalid the MSHR
    val invalid: Bool = Input(Bool())
  }

  val io: ICacheMshrIO = IO(new ICacheMshrIO(edge))

  private val valid = RegInit(Bool(), false.B)
  // this MSHR doesn't respond to fetch and sram
  private val flush  = RegInit(Bool(), false.B)
  private val fencei = RegInit(Bool(), false.B)
  // this MSHR has been issued
  private val issue = RegInit(Bool(), false.B)

  private val blkPAddr = RegInit(UInt((PAddrBits - blockOffBits).W), 0.U)
  private val vSetIdx  = RegInit(UInt(idxBits.W), 0.U)
  private val way      = RegInit(UInt(wayBits.W), 0.U)

  // look up and return result at the same cycle
  private val hits = io.lookUps.map { lookup =>
    valid && !fencei && !flush && (lookup.req.bits.vSetIdx === vSetIdx) &&
    (lookup.req.bits.blkPAddr === blkPAddr)
  }
  // Decoupling valid and bits
  (0 until 2).foreach(i => io.lookUps(i).resp.hit := hits(i))

  // disable wake up when hit MSHR (fencei is low)
  // when(hit) {
  //   flush := false.B
  // }

  // invalid when the req hasn't been issued
  when(io.fencei || io.flush) {
    fencei := true.B
    flush  := true.B
    when(!issue) {
      valid := false.B
    }
  }

  // receive request and register
  io.req.ready := !valid && !io.flush && !io.fencei
  when(io.req.fire) {
    valid    := true.B
    flush    := false.B
    issue    := false.B
    fencei   := false.B
    blkPAddr := io.req.bits.blkPAddr
    vSetIdx  := io.req.bits.vSetIdx
  }

  // send request to L2
  io.acquire.valid := valid && !issue && !io.flush && !io.fencei && !io.wfi.wfiReq
  private val getBlock = edge.Get(
    fromSource = ID.U,
    toAddress = Cat(blkPAddr, 0.U(blockOffBits.W)),
    lgSize = log2Up(cacheParams.blockBytes).U
  )._2
  io.acquire.bits.acquire := getBlock
  io.acquire.bits.acquire.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPUInst.id.U)
  io.acquire.bits.vSetIdx := vSetIdx

  // get victim way when acquire fire
  when(io.acquire.fire) {
    issue := true.B
    way   := io.victimWay
  }

  // invalid request when grant finish
  when(io.invalid) {
    valid := false.B
  }

  // offer the information other than data for write sram and response fetch
  io.info.valid         := valid && (!flush && !fencei)
  io.info.bits.blkPAddr := blkPAddr
  io.info.bits.vSetIdx  := vSetIdx
  io.info.bits.way      := way

  // we are safe to enter wfi if we have no pending response from L2
  io.wfi.wfiSafe := !(valid && issue)

  XSPerfHistogram(
    "responseLatency",
    BoolStopWatch(io.acquire.fire, io.invalid),
    io.invalid,
    0,
    200,
    10
  )
}
