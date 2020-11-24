package xiangshan.cache

import chisel3._
import chisel3.util._
import utils.{HasTLDump, XSDebug}
import xiangshan.{HasXSParameter, XSBundle, XSModule, HasXSLog}

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, IdRange}
import freechips.rocketchip.tilelink.{TLClientNode, TLMasterParameters, TLMasterPortParameters, TLHints}
import sifive.blocks.inclusivecache.{PrefetcherIO}

import scala.math.max

// DCachePrefetcher specific parameters
case class DCachePrefetcherParameters
(
    nMaxPrefetchRequests: Int = 8
)

trait HasDCachePrefetcherParameters extends HasXSParameter {
  val cfg = dcachePrefetcherParameters
}

abstract class DCachePrefetcherModule extends XSModule
  with HasDCachePrefetcherParameters

abstract class DCachePrefetcherBundle extends XSBundle
  with HasDCachePrefetcherParameters

// prefetch into L2
class L2PrefetchReq extends DCachePrefetcherBundle {
  val address = UInt(PAddrBits.W)
  val write   = Bool()
  val id      = UInt(log2Up(cfg.nMaxPrefetchRequests).W)
}

class L2PrefetchResp extends DCachePrefetcherBundle {
  val id      = UInt(log2Up(cfg.nMaxPrefetchRequests).W)
}

class L2PrefetchIO extends DCachePrefetcherBundle {
  val req = Decoupled(new L2PrefetchReq)
  val resp = Flipped(Decoupled(new L2PrefetchResp))
}

class DCachePrefetcher()(implicit p: Parameters) extends LazyModule with HasDCachePrefetcherParameters {

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "dcachePrefetcher",
      sourceId = IdRange(0, cfg.nMaxPrefetchRequests)
    ))
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new DCachePrefetcherImp(this)
}


class DCachePrefetcherImp(outer: DCachePrefetcher) extends LazyModuleImp(outer) with HasDCachePrefetcherParameters with HasXSLog {

  val io = IO(Flipped(new PrefetcherIO(PAddrBits)))

  // use this to send prefetch req to L2
  val l2Prefetch = Wire(new L2PrefetchIO())
  val l2PrefetchReq = l2Prefetch.req
  val l2PrefetchResp = l2Prefetch.resp

  // all the tilelink stuff
  val (bus, edge) = outer.clientNode.out.head
  require(bus.d.bits.data.getWidth == l1BusDataWidth, "DCachePrefetcher: tilelink width does not match")

  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  // convert between L2Prefetch and tilelink messages
  bus.a.valid := l2PrefetchReq.valid
  l2PrefetchReq.ready := bus.a.ready
  bus.a.bits  := edge.Hint(
    fromSource = l2PrefetchReq.bits.id,
    toAddress  = l2PrefetchReq.bits.address,
    lgSize     = (log2Up(L2BlockSize)).U,
    param      = Mux(l2PrefetchReq.bits.write, TLHints.PREFETCH_WRITE, TLHints.PREFETCH_READ))._2

  l2PrefetchResp.valid := bus.d.valid
  bus.d.ready := l2PrefetchResp.ready
  l2PrefetchResp.bits.id := bus.d.bits.source

  // TODO: send prefetch reqs and handle responses
  l2PrefetchReq.valid  := false.B
  l2PrefetchReq.bits   := DontCare
  l2PrefetchResp.ready := false.B
}
