package xiangshan.cache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import oceanus.compactchi._

/*
 * 1 upstream Type3 requester (Uncache) -> N downstream ports by address decode.
 * TXREQ/TXDAT: demux by Addr (txdatAddr from Uncache entry, same as txreq Addr).
 * RXRSP/RXDAT: RR arbiter when both downstream respond.
 */
class Type3Router(implicit val p: Parameters) extends Module with HasDCacheParameters {
  val io = IO(new Bundle {
    val up = Flipped(new CCHIType3Port)
    val txdatAddr = Input(UInt(48.W))
    val downL2 = new CCHIType3Port
    val downCtrl = new CCHIType3Port
  })

  private def isCtrlAddr(addr: UInt): Bool =
    dcacheParameters.cacheCtrlAddressOpt.map(_.contains(addr)).getOrElse(false.B)

  // TXREQ: route by address
  val txreqToCtrl = io.up.txreq.valid && isCtrlAddr(io.up.txreq.bits.Addr)

  io.downCtrl.txreq.valid := txreqToCtrl
  io.downCtrl.txreq.bits := io.up.txreq.bits
  io.downL2.txreq.valid := io.up.txreq.valid && !txreqToCtrl
  io.downL2.txreq.bits := io.up.txreq.bits
  io.up.txreq.ready := Mux(txreqToCtrl, io.downCtrl.txreq.ready, io.downL2.txreq.ready)

  // TXDAT: route by address
  val txdatToCtrl = io.up.txdat.valid && isCtrlAddr(io.txdatAddr)

  io.downCtrl.txdat.valid := txdatToCtrl
  io.downCtrl.txdat.bits := io.up.txdat.bits
  io.downL2.txdat.valid := io.up.txdat.valid && !txdatToCtrl
  io.downL2.txdat.bits := io.up.txdat.bits
  io.up.txdat.ready := Mux(txdatToCtrl, io.downCtrl.txdat.ready, io.downL2.txdat.ready)

  // RXRSP: RR arbiter (downL2 = in(0), downCtrl = in(1))
  val rspArb = Module(new RRArbiter(new FlitDnRSP, 2))
  rspArb.io.in(0).valid := io.downL2.rxrsp.valid
  rspArb.io.in(0).bits := io.downL2.rxrsp.bits
  rspArb.io.in(1).valid := io.downCtrl.rxrsp.valid
  rspArb.io.in(1).bits := io.downCtrl.rxrsp.bits
  io.up.rxrsp <> rspArb.io.out
  io.downL2.rxrsp.ready := rspArb.io.in(0).ready
  io.downCtrl.rxrsp.ready := rspArb.io.in(1).ready

  // RXDAT: RR arbiter (downL2 = in(0), downCtrl = in(1))
  val datArb = Module(new RRArbiter(new FlitDnDAT64, 2))
  datArb.io.in(0).valid := io.downL2.rxdat.valid
  datArb.io.in(0).bits := io.downL2.rxdat.bits
  datArb.io.in(1).valid := io.downCtrl.rxdat.valid
  datArb.io.in(1).bits := io.downCtrl.rxdat.bits
  io.up.rxdat <> datArb.io.out
  io.downL2.rxdat.ready := datArb.io.in(0).ready
  io.downCtrl.rxdat.ready := datArb.io.in(1).ready
}
