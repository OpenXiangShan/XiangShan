package xiangshan.cache

import chisel3._
import chisel3.util._

import utils.XSDebug
import bus.tilelink._
import xiangshan.{MicroOp, Redirect, NeedImpl}

class UncacheIO extends DCacheBundle {
  val lsroq = Flipped(new DCacheLoadIO)
  val bus = new TLCached(l1BusParams)
}

// convert DCacheIO to TileLink
class Uncache extends DCacheModule {
  val io = IO(new UncacheIO)

  val (legal, load) = TLMasterUtilities.Get(io.bus.params, 0.U, io.lsroq.req.bits.addr, 3.U)
  val (_, store) = TLMasterUtilities.Put(
    io.bus.params, 0.U, io.lsroq.req.bits.addr, 3.U, io.lsroq.req.bits.data, io.lsroq.req.bits.mask
  )

  io.bus <> DontCare

  io.bus.a.valid := io.lsroq.req.valid
  io.bus.a.bits := Mux(
    io.lsroq.req.bits.cmd === MemoryOpConstants.M_XWR,
    store,
    load
  )

  when(io.bus.a.fire()){
    io.bus.a.bits.dump()
  }

  when(io.bus.d.fire()){
    io.bus.d.bits.dump()
  }

  io.lsroq.resp.valid := io.bus.d.valid
  io.lsroq.resp.bits.data := io.bus.d.bits.data
  io.bus.d.ready := io.lsroq.resp.ready

}
