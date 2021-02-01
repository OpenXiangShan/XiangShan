package xiangshan.cache

import chisel3._
import chisel3.util._
import utils.{HasTLDump, PriorityMuxWithFlag, XSDebug}
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleA, TLBundleD, TLClientNode, TLEdgeOut, TLMasterParameters, TLMasterPortParameters}
import xiangshan.{HasXSLog, MicroOp, Redirect}

// One miss entry deals with one mmio request
class MMIOEntry(edge: TLEdgeOut) extends DCacheModule
{
  val io = IO(new Bundle {
    // MSHR ID
    val id = Input(UInt())

    // client requests
    val req = Flipped(DecoupledIO(new DCacheWordReq ))
    val resp = DecoupledIO(new DCacheWordResp)

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
  })


  val s_invalid :: s_refill_req :: s_refill_resp :: s_send_resp :: Nil = Enum(4)

  val state = RegInit(s_invalid)

  val req       = Reg(new DCacheWordReq )
  val resp_data = Reg(UInt(DataBits.W))


  // assign default values to output signals
  io.req.ready           := false.B
  io.resp.valid          := false.B
  io.resp.bits           := DontCare

  io.mem_acquire.valid   := false.B
  io.mem_acquire.bits    := DontCare

  io.mem_grant.ready     := false.B


  XSDebug("entry: %d state: %d\n", io.id, state)
  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    io.req.ready := true.B

    when (io.req.fire()) {
      req   := io.req.bits
      req.addr := io.req.bits.addr
      state := s_refill_req
    }
  }

  // --------------------------------------------
  // refill
  // TODO: determine 'lgSize' in memend
  val size = PopCount(req.mask)
  val (lgSize, legal) = PriorityMuxWithFlag(Seq(
    1.U -> 0.U,
    2.U -> 1.U,
    4.U -> 2.U,
    8.U -> 3.U
  ).map(m => (size===m._1) -> m._2))
  assert(!(io.mem_acquire.valid && !legal))

  val load = edge.Get(
    fromSource      = io.id,
    toAddress       = req.addr,
    lgSize          = lgSize
  )._2

  val store = edge.Put(
    fromSource      = io.id,
    toAddress = req.addr,
    lgSize  = lgSize,
    data  = req.data,
    mask = req.mask
  )._2

  when (state === s_refill_req) {
    io.mem_acquire.valid := true.B
    io.mem_acquire.bits  := Mux(req.cmd === MemoryOpConstants.M_XWR, store, load)

    when (io.mem_acquire.fire()) {
      state := s_refill_resp
    }
  }

  val (_, _, refill_done, _) = edge.addr_inc(io.mem_grant)

  when (state === s_refill_resp) {
    io.mem_grant.ready := true.B

    when (io.mem_grant.fire()) {
      resp_data := io.mem_grant.bits.data
      assert(refill_done, "MMIO response should be one beat only!")
      state := s_send_resp
    }
  }

  // --------------------------------------------
  when (state === s_send_resp) {
    io.resp.valid := true.B
    io.resp.bits.data   := resp_data
    // meta data should go with the response
    io.resp.bits.id     := req.id
    io.resp.bits.miss   := false.B
    io.resp.bits.replay := false.B

    when (io.resp.fire()) {
      state := s_invalid
    }
  }
}

class UncacheIO extends DCacheBundle {
  val lsq = Flipped(new DCacheWordIO)
}

// convert DCacheIO to TileLink
// for Now, we only deal with TL-UL

class Uncache()(implicit p: Parameters) extends LazyModule with HasDCacheParameters {

  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "uncache",
      sourceId = IdRange(0, cfg.nMMIOEntries)
    ))
  )
  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new UncacheImp(this)

}

class UncacheImp(outer: Uncache)
  extends LazyModuleImp(outer)
    with HasDCacheParameters
    with HasXSLog
    with HasTLDump
{
  val io = IO(new UncacheIO)

  val (bus, edge) = outer.clientNode.out.head
  require(bus.d.bits.data.getWidth == wordBits, "Uncache: tilelink width does not match")

  val resp_arb = Module(new Arbiter(new DCacheWordResp, cfg.nMMIOEntries))

  val req  = io.lsq.req
  val resp = io.lsq.resp
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

  val entries = (0 until cfg.nMMIOEntries) map { i =>
    val entry = Module(new MMIOEntry(edge))

    entry.io.id := i.U(log2Up(cfg.nMMIOEntries).W)

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
  XSDebug(req.fire(), "req cmd: %x addr: %x data: %x mask: %x\n",
    req.bits.cmd, req.bits.addr, req.bits.data, req.bits.mask)
  XSDebug(resp.fire(), "data: %x\n", req.bits.data)

  // print tilelink messages
  when(mem_acquire.valid){
    XSDebug("mem_acquire valid, ready=%d ", mem_acquire.ready)
    mem_acquire.bits.dump
  }
  when (mem_grant.fire()) {
    XSDebug("mem_grant fire ")
    mem_grant.bits.dump
  }
}
