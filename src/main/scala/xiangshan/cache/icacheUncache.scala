package xiangshan.cache

import chisel3._
import chisel3.util._
import utils.{HasTLDump, PriorityMuxWithFlag, XSDebug}
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleA, TLBundleD, TLClientNode, TLEdgeOut, TLMasterParameters, TLMasterPortParameters}
import xiangshan._
import xiangshan.{HasXSLog, MicroOp, Redirect}

class unCacheReq extends ICacheBundle
{
    val addr = UInt(VAddrBits.W)
    val id = UInt(3.W)
}

class unCacheResp extends ICacheBundle
{
  val data = Vec(MMIOBeats,UInt(MMIOWordBits.W))
  val id   = UInt(3.W)
}

// One miss entry deals with one mmio request
class icacheMMIOEntry(edge: TLEdgeOut) extends XSModule with HasICacheParameters
{
  val io = IO(new Bundle {
    val id = Input(UInt(log2Up(cacheParams.nMMIOs).W))
    // client requests
    val req = Flipped(DecoupledIO(new unCacheReq ))
    val resp = DecoupledIO(new unCacheResp)

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    
    val flush = Input(Bool())
  })


  val s_invalid :: s_refill_req :: s_refill_resp :: s_send_resp :: Nil = Enum(4)

  val state = RegInit(s_invalid)

  val req       = Reg(new unCacheReq )
  val respDataReg = Reg(Vec(MMIOBeats,UInt(MMIOWordBits.W)))
  val refillCounter = Counter(MMIOBeats)


  // assign default values to output signals
  io.req.ready           := false.B
  io.resp.valid          := false.B
  io.resp.bits           := DontCare

  io.mem_acquire.valid   := false.B
  io.mem_acquire.bits    := DontCare

  io.mem_grant.ready     := false.B


  XSDebug("entry: %d state: %d\n", io.id, state)

      //flush register
  val needFlush = RegInit(false.B)
  when(io.flush && (state =/= s_invalid) && (state =/= s_send_resp)){ needFlush := true.B }
  .elsewhen((state=== s_send_resp) && needFlush){ needFlush := false.B }

  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    io.req.ready := true.B
    refillCounter.value := 0.U

    when (io.req.fire()) {
      req   := io.req.bits
      state := s_refill_req
    }
  }

  // --------------------------------------------
  // refill
  // TODO: determine 'lgSize' in memend
  val out = edge.Get(
    fromSource      = io.id,
    toAddress       = req.addr,
    lgSize          = log2Up(8).U
  )._2

  when (state === s_refill_req) {
    io.mem_acquire.valid := true.B
    io.mem_acquire.bits  := out

    when (io.mem_acquire.fire()) {
      state := s_refill_resp
    }
  }

  val (_, _, refill_done, _) = edge.addr_inc(io.mem_grant)

  when (state === s_refill_resp) {
    io.mem_grant.ready := true.B

    when (io.mem_grant.fire()) {
      respDataReg(refillCounter.value) := io.mem_grant.bits.data
      assert(refill_done, "MMIO response should be one beat only!")
      state := Mux(needFlush || io.flush,s_invalid,Mux(refillCounter.value === (MMIOBeats - 1).U,s_send_resp,s_refill_resp))
      refillCounter.inc()
    }
  }

  // --------------------------------------------
  when (state === s_send_resp) {
    io.resp.valid := true.B
    io.resp.bits.data := respDataReg
    io.resp.bits.id := req.id
    // meta data should go with the response
    when (io.resp.fire() || needFlush) {
      state := s_invalid
    }
  }
}

class icacheUncacheIO extends DCacheBundle {
    val req = Flipped(DecoupledIO(new unCacheReq ))
    val resp = DecoupledIO(new unCacheResp)
    val flush = Input(Bool())

}

// convert DCacheIO to TileLink
// for Now, we only deal with TL-UL

class icacheUncache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {

  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "icacheUncache",
      sourceId = IdRange(0, cacheParams.nMMIOs)
    ))
  )
  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new icacheUncacheImp(this)

}

class icacheUncacheImp(outer: icacheUncache)
  extends LazyModuleImp(outer)
    with HasICacheParameters
    with HasXSLog
    with HasTLDump
{
  val io = IO(new icacheUncacheIO)

  val (bus, edge) = outer.clientNode.out.head
  require(bus.d.bits.data.getWidth == wordBits, "Uncache: tilelink width does not match")

  val resp_arb = Module(new Arbiter(new unCacheResp, cacheParams.nMMIOs))

  val req  = io.req
  val resp = io.resp
  val mem_acquire = bus.a
  val mem_grant   = bus.d

  val entry_alloc_idx = Wire(UInt())
  val req_ready = WireInit(false.B)

  // assign default values to output signals
  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.d.ready := false.B
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  val entries = (0 until cacheParams.nMMIOs) map { i =>
    val entry = Module(new icacheMMIOEntry(edge))

    entry.io.id := i.U(log2Up(cacheParams.nMMIOs).W)
    entry.io.flush := io.flush

    // entry req
    entry.io.req.valid := (i.U === entry_alloc_idx) && req.valid
    entry.io.req.bits  := req.bits
    when (i.U === entry_alloc_idx) {
      req_ready := entry.io.req.ready
    }

    // entry resp
    resp_arb.io.in(i) <> entry.io.resp

    entry.io.mem_grant.valid := false.B
    entry.io.mem_grant.bits  := DontCare
    when (mem_grant.bits.source === i.U) {
      entry.io.mem_grant <> mem_grant
    }
    entry
  }

  entry_alloc_idx    := PriorityEncoder(entries.map(m=>m.io.req.ready))

  req.ready  := req_ready
  resp          <> resp_arb.io.out
  TLArbiter.lowestFromSeq(edge, mem_acquire, entries.map(_.io.mem_acquire))


  // print all input/output requests for debug purpose

  // print req/resp
}
