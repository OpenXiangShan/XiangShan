package xiangshan.cache

import chisel3._
import chisel3.util._
import utils._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleA, TLBundleD, TLClientNode, TLEdgeOut, TLMasterParameters, TLMasterPortParameters}
import xiangshan._
import xiangshan.frontend._

class InsUncacheReq extends ICacheBundle
{
    val addr = UInt(PAddrBits.W)
    val id = UInt(3.W)
}

class InsUncacheResp extends ICacheBundle
{
  val data = UInt((mmioBeats * mmioBusWidth).W)
  val id   = UInt(3.W)
}

// One miss entry deals with one mmio request
class InstrMMIOEntry(edge: TLEdgeOut) extends XSModule with HasICacheParameters with HasIFUConst
{
  val io = IO(new Bundle {
    val id = Input(UInt(log2Up(cacheParams.nMMIOs).W))
    // client requests
    val req = Flipped(DecoupledIO(new InsUncacheReq ))
    val resp = DecoupledIO(new InsUncacheResp)

    val mmio_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mmio_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    
    val flush = Input(Bool())
  })


  val s_invalid :: s_refill_req :: s_refill_resp :: s_send_resp :: Nil = Enum(4)

  val state = RegInit(s_invalid)

  val req       = Reg(new InsUncacheReq )
  val respDataReg = Reg(Vec(mmioBeats,UInt(mmioBusWidth.W)))
  val beatCounter = Counter(mmioBeats)


  // assign default values to output signals
  io.req.ready           := false.B
  io.resp.valid          := false.B
  io.resp.bits           := DontCare

  io.mmio_acquire.valid   := false.B
  io.mmio_acquire.bits    := DontCare

  io.mmio_grant.ready     := false.B

  val needFlush = RegInit(false.B)

  XSDebug("[ICache MMIO]entry: %d state: %d needFlush%d  flush:%d\n", io.id, state, needFlush,io.flush)
  XSDebug("[ICache MMIO]req.addr: %x req.id \n", req.addr)
  XSDebug("[ICache MMIO]mmio_acquire:(v:%d  r:%d)  mmio_grant:(v:%d r:%d)\n", io.mmio_acquire.valid, io.mmio_acquire.ready, io.mmio_grant.valid, io.mmio_grant.ready) 
  XSDebug("[ICache MMIO]mmio_acquire:(v:%d  r:%d)  mmio_grant:(v:%d r:%d)\n", io.mmio_acquire.valid, io.mmio_acquire.ready, io.mmio_grant.valid, io.mmio_grant.ready) 

  XSDebug("[ICache MMIO]respReg:  %x\n",respDataReg.asUInt)


  when(io.flush && (state =/= s_invalid) && (state =/= s_send_resp)){ needFlush := true.B }
  .elsewhen((state=== s_send_resp) && needFlush){ needFlush := false.B }

  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    io.req.ready := true.B
    beatCounter.value := 0.U

    when (io.req.fire()) {
      req   := io.req.bits
      state := s_refill_req
    }
  }


  when (state === s_refill_req) {
    io.mmio_acquire.valid := true.B
    io.mmio_acquire.bits  :=  edge.Get(
          fromSource      = io.id,
          toAddress       = packetAligned(req.addr) + (beatCounter.value << log2Ceil(mmioBusBytes).U),
          lgSize          = log2Ceil(mmioBusBytes).U
        )._2

    when (io.mmio_acquire.fire()) {
      state := s_refill_resp
    }
  }

  val (_, _, refill_done, _) = edge.addr_inc(io.mmio_grant)

  when (state === s_refill_resp) {
    io.mmio_grant.ready := true.B

    when (io.mmio_grant.fire()) {
      // val realAddr = packetAligned(req.addr) + (beatCounter.value << log2Ceil(mmioBusBytes).U)
      // val start = realAddr(5,3)
      respDataReg(beatCounter.value) := io.mmio_grant.bits.data
      state :=Mux((beatCounter.value === (mmioBeats - 1).U) || needFlush || io.flush ,s_send_resp,s_refill_req)
      beatCounter.inc()
    }
  }

  // --------------------------------------------
  when (state === s_send_resp) {
    io.resp.valid := !needFlush
    io.resp.bits.data := respDataReg.asUInt
    io.resp.bits.id := req.id
    // meta data should go with the response
    when (io.resp.fire() || needFlush) {
      state := s_invalid
      beatCounter.value := 0.U
    }
  }
}

class icacheUncacheIO extends DCacheBundle {
    val req = Flipped(DecoupledIO(new InsUncacheReq ))
    val resp = DecoupledIO(new InsUncacheResp)
    val flush = Input(Bool())

}

// convert DCacheIO to TileLink
// for Now, we only deal with TL-UL

class InstrUncache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {

  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "InstrUncache",
      sourceId = IdRange(0, cacheParams.nMMIOs)
    ))
  )
  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new icacheUncacheImp(this)

}

class icacheUncacheImp(outer: InstrUncache)
  extends LazyModuleImp(outer)
    with HasICacheParameters
    with HasXSLog
    with HasTLDump
{
  val io = IO(new icacheUncacheIO)

  val (bus, edge) = outer.clientNode.out.head
  //require(bus.d.bits.data.getWidth == wordBits, "Uncache: tilelink width does not match")

  val resp_arb = Module(new Arbiter(new InsUncacheResp, cacheParams.nMMIOs))

  val req  = io.req
  val resp = io.resp
  val mmio_acquire = bus.a
  val mmio_grant   = bus.d

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
    val entry = Module(new InstrMMIOEntry(edge))

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

    entry.io.mmio_grant.valid := false.B
    entry.io.mmio_grant.bits  := DontCare
    when (mmio_grant.bits.source === i.U) {
      entry.io.mmio_grant <> mmio_grant
    }
    entry
  }

  entry_alloc_idx    := PriorityEncoder(entries.map(m=>m.io.req.ready))

  req.ready  := req_ready
  resp          <> resp_arb.io.out
  TLArbiter.lowestFromSeq(edge, mmio_acquire, entries.map(_.io.mmio_acquire))

}
