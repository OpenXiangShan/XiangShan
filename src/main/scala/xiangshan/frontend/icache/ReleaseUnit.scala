package xiangshan.frontend.icache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates, TLArbiter, TLBundleC, TLBundleD, TLEdgeOut, TLPermissions}
import xiangshan._
import utils._

class ReleaseReq(implicit p: Parameters) extends ICacheBundle{
  val addr = UInt(PAddrBits.W)
  val param  = UInt(TLPermissions.cWidth.W)
  val voluntary = Bool()
  val hasData = Bool()
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
    remain_set := Mux(io.req.bits.hasData, ~0.U(refillCycles.W), 1.U(refillCycles.W))
    state      := s_release_req
  }

  val beat = PriorityEncoder(remain)
  val beat_data = Wire(Vec(refillCycles, UInt(beatBits.W)))
  for (i <- 0 until refillCycles) {
    beat_data(i) := req.data((i + 1) * beatBits - 1, i * beatBits)
  }

  val probeResponseData = edge.ProbeAck(
    fromSource = io.id,
    toAddress = req.addr,
    lgSize = log2Ceil(cacheParams.blockBytes).U,
    reportPermissions = req.param,
    data = beat_data(beat)
  )

  val voluntaryRelease = edge.Release(
    fromSource = io.id,
    toAddress = addrAlign(req.addr, blockBytes, PAddrBits),
    lgSize = log2Ceil(blockBytes).U,
    shrinkPermissions = req.param
  )._2

  io.mem_release.valid := busy
  io.mem_release.bits  := Mux(!req.voluntary, probeResponseData,voluntaryRelease)

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
      state := s_invalid
    }
  }

  //assert((io.mem_release.fire() && io.mem_release.bits.data =/= 0.U && !req.voluntary) || !io.mem_release.fire() || (io.mem_release.fire() && req.voluntary))

}

class ReleaseUnit(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheModule
{
  val io = IO(new Bundle {
    val req = Vec(2, Flipped(DecoupledIO(new ReleaseReq)))
    val mem_release = DecoupledIO(new TLBundleC(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
  })

  val req = io.req
  // assign default values to output signals
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