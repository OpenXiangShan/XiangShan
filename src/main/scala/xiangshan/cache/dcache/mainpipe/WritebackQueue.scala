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

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleC, TLBundleD, TLEdgeOut}
import org.chipsalliance.cde.config.Parameters
import utils.HasTLDump
import utility.{XSDebug, XSPerfAccumulate, HasPerfEvents}


class WritebackReqCtrl(implicit p: Parameters) extends DCacheBundle {
  val param  = UInt(cWidth.W)
  val voluntary = Bool()
  val hasData = Bool()
  val corrupt = Bool()
  val dirty = Bool()

  val delayRelease = Bool()
  val missId = UInt(log2Up(cfg.nMissEntries).W)
}

class WritebackReqWodata(implicit p: Parameters) extends WritebackReqCtrl {
  val addr = UInt(PAddrBits.W)

  def dump(cond: Bool) = {
    XSDebug(cond, "WritebackReq addr: %x param: %d voluntary: %b hasData: %b\n",
      addr, param, voluntary, hasData)
  }
}

class WritebackReqData(implicit p: Parameters) extends DCacheBundle {
  val data = UInt((cfg.blockBytes * 8).W)
}

class WritebackReq(implicit p: Parameters) extends WritebackReqWodata {
  val data = UInt((cfg.blockBytes * 8).W)

  override def dump(cond: Bool) = {
    XSDebug(cond, "WritebackReq addr: %x param: %d voluntary: %b hasData: %b data: %x\n",
      addr, param, voluntary, hasData, data)
  }

  def toWritebackReqWodata(): WritebackReqWodata = {
    val out = Wire(new WritebackReqWodata)
    out.addr := addr
    out.param := param
    out.voluntary := voluntary
    out.hasData := hasData
    out.corrupt := corrupt
    out.dirty := dirty
    out.delayRelease := delayRelease
    out.missId := missId
    out
  }

  def toWritebackReqCtrl(): WritebackReqCtrl = {
    val out = Wire(new WritebackReqCtrl)
    out.param := param
    out.voluntary := voluntary
    out.hasData := hasData
    out.corrupt := corrupt
    out.dirty := dirty
    out.delayRelease := delayRelease
    out.missId := missId
    out
  }

  def toWritebackReqData(): WritebackReqData = {
    val out = Wire(new WritebackReqData)
    out.data := data
    out
  }
}

// While a Release sleeps and waits for a refill to wake it up,
// main pipe might update meta & data during this time.
// So the meta & data to be released need to be updated too.
class ReleaseUpdate(implicit p: Parameters) extends DCacheBundle {
  // only consider store here
  val addr = UInt(PAddrBits.W)
  val mask = UInt(DCacheBanks.W)
  val data = UInt((cfg.blockBytes * 8).W)
}

// To reduce fanout, writeback queue entry data is updated 1 cycle
// after ReleaseUpdate.fire
class WBQEntryReleaseUpdate(implicit p: Parameters) extends DCacheBundle {
  // only consider store here
  val addr = UInt(PAddrBits.W)
  val maskDelayed = UInt(DCacheBanks.W)
  val dataDelayed = UInt((cfg.blockBytes * 8).W)
  val maskOrr = Bool()
}

// When a probe TtoB req enter dcache main pipe, check if that cacheline
// is waiting for release. If it is so, change TtoB to TtoN, set dcache
// coh to N.
class ProbeToBCheckReq(implicit p: Parameters) extends DCacheBundle {
  val addr = UInt(PAddrBits.W) // paddr from mainpipe s1
}

class ProbeToBCheckResp(implicit p: Parameters) extends DCacheBundle {
  val toN = Bool() // need to set dcache coh to N
}

class WritebackEntry(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule with HasTLDump
{
  val io = IO(new Bundle {
    val id = Input(UInt())

    val req = Flipped(DecoupledIO(new WritebackReqWodata))
    val reqData = Input(new WritebackReqData)

    val memRelease = DecoupledIO(new TLBundleC(edge.bundle))
    val memGrant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val primaryValid = Input(Bool())
    val primaryReady = Output(Bool())
    val primaryReadyDup = Vec(nDupWbReady, Output(Bool()))

    val blockAddr  = Output(Valid(UInt()))
  })

  val sInvalid :: s_release_req :: s_release_resp ::Nil = Enum(3)
  // ProbeAck:               s_invalid ->            s_release_req
  // ProbeAck merge Release: s_invalid ->            s_release_req
  // Release:                s_invalid -> s_sleep -> s_release_req -> s_release_resp
  // Release merge ProbeAck: s_invalid -> s_sleep -> s_release_req
  //                        (change Release into ProbeAck when Release is not fired)
  //                     or: s_invalid -> s_sleep -> s_release_req -> s_release_resp -> s_release_req
  //                        (send a ProbeAck after Release transaction is over)

  val state = RegInit(sInvalid)
  val stateDup0 = RegInit(sInvalid)
  val stateDup1 = RegInit(sInvalid)
  val stateDupForMp = RegInit(VecInit(Seq.fill(nDupWbReady)(sInvalid))) //TODO: clock gate

  val remain = RegInit(0.U(refillCycles.W))
  val remainDup0 = RegInit(0.U(refillCycles.W))
  val remainDup1 = RegInit(0.U(refillCycles.W))
  val remainSet = WireInit(0.U(refillCycles.W))
  val remainClr = WireInit(0.U(refillCycles.W))
  remain := (remain | remainSet) & ~remainClr
  remainDup0 := (remainDup0 | remainSet) & ~remainClr
  remainDup1 := (remainDup1 | remainSet) & ~remainClr

  // writeback queue data
  val data = Reg(UInt((cfg.blockBytes * 8).W))

  // writeback queue paddr
  val paddrDup0 = Reg(UInt(PAddrBits.W))
  val paddrDup1 = Reg(UInt(PAddrBits.W))
  val paddrDup2 = Reg(UInt(PAddrBits.W))

  // pending data write
  // !s_data_override means there is an in-progress data write
  val sDataOverride = RegInit(true.B)
  // !s_data_merge means there is an in-progress data merge
  //val s_data_merge = RegInit(true.B)

  // there are valid request that can be sent to release bus
  //val busy = remain.orR && s_data_override && s_data_merge // have remain beats and data write finished
  val busy = remain.orR && sDataOverride  // have remain beats and data write finished
  val req = Reg(new WritebackReqWodata)

  // assign default signals to output signals
  io.req.ready := false.B
  io.memRelease.valid := false.B
  io.memRelease.bits  := DontCare
  io.memGrant.ready   := false.B
  io.blockAddr.valid  := state =/= sInvalid
  io.blockAddr.bits   := req.addr

  sDataOverride := true.B // data_override takes only 1 cycle
  //s_data_merge := true.B // data_merge takes only 1 cycle

  XSDebug(state =/= sInvalid, "WritebackEntry: %d state: %d block_addr: %x\n", io.id, state, io.blockAddr.bits)

  // --------------------------------------------------------------------------------
  // s_invalid: receive requests
  // new req entering
  io.req.ready := state === sInvalid
  val alloc = io.req.valid && io.primaryValid && io.primaryReady
  when (alloc) {
    assert (remain === 0.U)
    req := io.req.bits
    sDataOverride := false.B
    // only update paddr when allocate a new missqueue entry
    paddrDup0 := io.req.bits.addr
    paddrDup1 := io.req.bits.addr
    paddrDup2 := io.req.bits.addr

    remainSet := Mux(io.req.bits.hasData, ~0.U(refillCycles.W), 1.U(refillCycles.W))
    state      := s_release_req
    stateDup0 := s_release_req
    stateDup1 := s_release_req
    stateDupForMp.foreach(_ := s_release_req)
  }

  // --------------------------------------------------------------------------------
  // while there beats remaining to be sent, we keep sending
  // which beat to send in this cycle?
  val beat = PriorityEncoder(remainDup0)

  val beatData = Wire(Vec(refillCycles, UInt(beatBits.W)))
  for (i <- 0 until refillCycles) {
    beatData(i) := data((i + 1) * beatBits - 1, i * beatBits)
  }

  val probeResponse = edge.ProbeAck(
    fromSource = io.id,
    toAddress = paddrDup1,
    lgSize = log2Ceil(cfg.blockBytes).U,
    reportPermissions = req.param
  )
  probeResponse.corrupt := req.corrupt

  val probeResponseData = edge.ProbeAck(
    fromSource = io.id,
    toAddress = paddrDup1,
    lgSize = log2Ceil(cfg.blockBytes).U,
    reportPermissions = req.param,
    data = beatData(beat),
    corrupt = req.corrupt
  )

  val voluntaryRelease = edge.Release(
    fromSource = io.id,
    toAddress = paddrDup2,
    lgSize = log2Ceil(cfg.blockBytes).U,
    shrinkPermissions = req.param
  )._2
  voluntaryRelease.corrupt := req.corrupt

  val voluntaryReleaseData = edge.Release(
    fromSource = io.id,
    toAddress = paddrDup2,
    lgSize = log2Ceil(cfg.blockBytes).U,
    shrinkPermissions = req.param,
    data = beatData(beat),
    corrupt = req.corrupt
  )._2

  // voluntaryReleaseData.echo.lift(DirtyKey).foreach(_ := req.dirty)
  when(busy) {
    assert(!req.dirty || req.hasData)
  }

  val (_, _, release_done, release_count) = edge.count(io.memRelease)

  io.memRelease.valid := busy
  io.memRelease.bits  := Mux(req.voluntary,
    Mux(req.hasData, voluntaryReleaseData, voluntaryRelease),
    Mux(req.hasData, probeResponseData, probeResponse))


  when (io.memRelease.fire) {remainClr := PriorityEncoderOH(remainDup1)}

  when(state === s_release_req && release_done){
    state := Mux(req.voluntary, s_release_resp, sInvalid)
    when(req.voluntary){
      stateDupForMp.foreach(_ := s_release_resp)
    } .otherwise{
      stateDupForMp.foreach(_ := sInvalid)
    }
  }

  io.primaryReady := state === sInvalid
  io.primaryReadyDup.zip(stateDupForMp).foreach { case (rdy, st) => rdy := st === sInvalid }
  // --------------------------------------------------------------------------------
  // receive ReleaseAck for Releases
  when (state === s_release_resp) {
    io.memGrant.ready := true.B
    when (io.memGrant.fire) {
      state := sInvalid
      stateDupForMp.foreach(_ := sInvalid)
    }
  }

  // data update logic
  when(!sDataOverride && (req.hasData || RegNext(alloc))) {
    data := io.reqData.data
  }

  // assert(!RegNext(!s_data_merge && !s_data_override))

  // performance counters
  XSPerfAccumulate("wb_req", io.req.fire)
  XSPerfAccumulate("wb_release", state === s_release_req && release_done && req.voluntary)
  XSPerfAccumulate("wb_probe_resp", state === s_release_req && release_done && !req.voluntary)
  XSPerfAccumulate("penalty_blocked_by_channel_C", io.memRelease.valid && !io.memRelease.ready)
  XSPerfAccumulate("penalty_waiting_for_channel_D", io.memGrant.ready && !io.memGrant.valid && state === s_release_resp)
}

class WritebackQueue(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule with HasTLDump with HasPerfEvents
{
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new WritebackReq))
    val reqReadyDup = Vec(nDupWbReady, Output(Bool()))
    val memRelease = DecoupledIO(new TLBundleC(edge.bundle))
    val memGrant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    //val probe_ttob_check_req = Flipped(ValidIO(new ProbeToBCheckReq))
    //val probe_ttob_check_resp = ValidIO(new ProbeToBCheckResp)

    // align with DCacheWrapper MissReqPortCount:
    // main pipe * 1 + load pipe * LduCnt + store pipe * StaCnt (optional) + hybrid * HyuCnt
    val missReqConflictCheck = Vec(MissReqPortCount, Flipped(Valid(UInt())))
    val blockMissReq = Vec(MissReqPortCount, Output(Bool()))
  })

  require(cfg.nReleaseEntries > cfg.nMissEntries)

  val primaryReadyVec = Wire(Vec(cfg.nReleaseEntries, Bool()))
  val alloc = Cat(primaryReadyVec).orR

  val req = io.req
  val blockConflict = Wire(Bool())

  req.ready := alloc && !blockConflict

  // assign default values to output signals
  io.memRelease.valid := false.B
  io.memRelease.bits  := DontCare
  io.memGrant.ready   := false.B

  // delay data write in writeback req for 1 cycle
  val reqData = RegEnable(io.req.bits.toWritebackReqData(), io.req.valid)

  require(isPow2(cfg.nMissEntries))
  val grantSource = io.memGrant.bits.source
  val entries = Seq.fill(cfg.nReleaseEntries)(Module(new WritebackEntry(edge)))
  entries.zipWithIndex.foreach {
    case (entry, i) =>
      val formerPrimaryReady = if(i == 0)
        false.B
      else
        Cat((0 until i).map(j => entries(j).io.primaryReady)).orR
      val entryId = (i + releaseIdBase).U

      entry.io.id := entryId

      // entry req
      entry.io.req.valid := req.valid && !blockConflict
      primaryReadyVec(i)   := entry.io.primaryReady
      entry.io.req.bits  := req.bits
      entry.io.reqData  := reqData

      entry.io.primaryValid := alloc &&
        !formerPrimaryReady &&
        entry.io.primaryReady

      entry.io.memGrant.valid := (entryId === grantSource) && io.memGrant.valid
      entry.io.memGrant.bits  := io.memGrant.bits
      //when (i.U === io.mem_grant.bits.source) {
      //  io.mem_grant.ready := entry.io.mem_grant.ready
      //}
  }

  io.reqReadyDup.zipWithIndex.foreach { case (rdy, i) =>
    rdy := Cat(entries.map(_.io.primaryReadyDup(i))).orR && !blockConflict
  }

  io.memGrant.ready := true.B
  blockConflict := VecInit(entries.map(e => e.io.blockAddr.valid && e.io.blockAddr.bits === io.req.bits.addr)).asUInt.orR
  val missReqConflict = io.missReqConflictCheck.map{ r =>
    VecInit(entries.map(e => e.io.blockAddr.valid && e.io.blockAddr.bits === r.bits)).asUInt.orR
  }
  io.blockMissReq.zipWithIndex.foreach{ case(blk, i) =>
    blk := io.missReqConflictCheck(i).valid && missReqConflict(i)
  }

  TLArbiter.lowest(edge, io.memRelease, entries.map(_.io.memRelease):_*)

  // sanity check
  // print all input/output requests for debug purpose
  // print req
  io.req.bits.dump(io.req.fire)

  io.memGrant.bits.dump(io.memGrant.fire)

  // XSDebug(io.miss_req.valid, "miss_req: addr: %x\n", io.miss_req.bits)
  // XSDebug(io.block_miss_req, "block_miss_req\n")

  // performance counters
  XSPerfAccumulate("wb_req", io.req.fire)
  for(i <- 0 until MissReqPortCount) {
    XSPerfAccumulate(s"block_miss_req_$i", io.blockMissReq(i))
  }

  val perfValidCount = RegNext(PopCount(entries.map(e => e.io.blockAddr.valid)))
  val perfEvents = Seq(
    ("dcache_wbq_req      ", io.req.fire),
    ("dcache_wbq_1_4_valid", (perfValidCount < (cfg.nReleaseEntries.U/4.U))),
    ("dcache_wbq_2_4_valid", (perfValidCount > (cfg.nReleaseEntries.U/4.U)) & (perfValidCount <= (cfg.nReleaseEntries.U/2.U))),
    ("dcache_wbq_3_4_valid", (perfValidCount > (cfg.nReleaseEntries.U/2.U)) & (perfValidCount <= (cfg.nReleaseEntries.U*3.U/4.U))),
    ("dcache_wbq_4_4_valid", (perfValidCount > (cfg.nReleaseEntries.U*3.U/4.U))),
  )
  generatePerfEvent()

}

