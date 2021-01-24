package xiangshan.cache

import chisel3._
import chisel3.util._
import utils.XSDebug
import freechips.rocketchip.tilelink.{TLBundleC, TLBundleD, TLEdgeOut, TLPermissions}

class WritebackReq extends DCacheBundle {
  val addr = UInt(PAddrBits.W)
  val param  = UInt(TLPermissions.cWidth.W)
  val voluntary = Bool()
  val hasData = Bool()
  val data = UInt((cfg.blockBytes * 8).W)

  def dump() = {
    XSDebug("WritebackReq addr: %x param: %d voluntary: %b hasData: %b data: %x\n",
      addr, param, voluntary, hasData, data)
  }
}

class WritebackUnit(edge: TLEdgeOut) extends DCacheModule {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new WritebackReq))
    val mem_release = DecoupledIO(new TLBundleC(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
  })

  // remaining beats
  val remain = RegInit(0.U(refillCycles.W))
  val remain_set = WireInit(0.U(refillCycles.W))
  val remain_clr = WireInit(0.U(refillCycles.W))
  remain := (remain | remain_set) & ~remain_clr

  // used source id
  // source id 0 is reserved for ProbeAck[Data]
  val used = RegInit(0.U((cfg.nReleaseEntries - 1).W))
  val used_set = WireInit(0.U((cfg.nReleaseEntries - 1).W))
  val used_clr = WireInit(0.U((cfg.nReleaseEntries - 1).W))
  used := (used | used_set) & ~used_clr

  val busy = remain.orR
  val all_used = used.andR

  val req_reg  = Reg(new WritebackReq)
  val req = Mux(busy, req_reg, io.req.bits)

  // --------------------------------------------------------------------------------
  // new req entering
  // source to use for this transaction
  val source = Reg(UInt())
  io.req.ready := !busy && (!io.req.bits.voluntary || !all_used)
  when (io.req.fire()) {
    remain_set := Mux(io.req.bits.hasData, ~0.U(refillCycles.W), 1.U(refillCycles.W))
    used_set   := Mux(io.req.bits.voluntary, PriorityEncoderOH(~used), 0.U)
    // source 0 is reserved for ProbeAck[Data]
    source     := Mux(io.req.bits.voluntary, PriorityEncoder(~used) + 1.U, 0.U)
  }

  // --------------------------------------------------------------------------------
  // while there beats remaining to be sent, we keep sending
  // which beat to send in this cycle?
  val beat = PriorityEncoder(remain)

  val beat_data = Wire(Vec(refillCycles, UInt(beatBits.W)))
  for (i <- 0 until refillCycles) {
    beat_data(i) := req.data((i + 1) * beatBits - 1, i * beatBits)
  }

  val probeResponse = edge.ProbeAck(
    fromSource = source,
    toAddress = req.addr,
    lgSize = log2Ceil(cfg.blockBytes).U,
    reportPermissions = req.param
  )

  val probeResponseData = edge.ProbeAck(
    fromSource = source,
    toAddress = req.addr,
    lgSize = log2Ceil(cfg.blockBytes).U,
    reportPermissions = req.param,
    data = beat_data(beat)
  )

  val voluntaryRelease = edge.Release(
    fromSource = source,
    toAddress = req.addr,
    lgSize = log2Ceil(cfg.blockBytes).U,
    shrinkPermissions = req.param
  )._2

  val voluntaryReleaseData = edge.Release(
    fromSource = source,
    toAddress = req.addr,
    lgSize = log2Ceil(cfg.blockBytes).U,
    shrinkPermissions = req.param,
    data = beat_data(beat)
  )._2

  io.mem_release.valid := busy
  io.mem_release.bits  := Mux(req.voluntary,
    Mux(req.hasData, voluntaryReleaseData, voluntaryRelease),
    Mux(req.hasData, probeResponseData, probeResponse))

  when (io.mem_release.fire()) { remain_clr := PriorityEncoderOH(remain) }

  // --------------------------------------------------------------------------------
  // receive ReleaseAck for Releases
  // we are alway ready
  // remember to assert any invalid grant
  io.mem_grant.ready := used(io.mem_grant.bits.source - 1.U)
  when (io.mem_grant.fire()) {
    used_clr := UIntToOH(io.mem_grant.bits.source - 1.U)
  }
  
  // print all input/output requests for debug purpose
  // print req
  when (io.req.fire()) {
    io.req.bits.dump()
  }
}
