package system

import noop.{NOOP, NOOPConfig, HasNOOPParameter, Cache, CacheConfig}
import bus.axi4.{AXI4, AXI4Lite}
import bus.simplebus._
import utils._

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

trait HasPrefetcherParameter extends HasNOOPParameter {
  val supportPrefetch = HasDcache
}

class Prefetcher extends Module with HasPrefetcherParameter {
	val io = IO(new Bundle {
		val in = Flipped(Decoupled(new SimpleBusReqBundle))
		val out = Decoupled(new SimpleBusReqBundle)
	})
  val getNewReq = RegInit(false.B)
  val prefetchReq = RegNext(io.in.bits)
  prefetchReq.cmd := SimpleBusCmd.prefetch
  prefetchReq.addr := io.in.bits.addr + XLEN.U

  val lastReqAddr = (RegEnable(io.in.bits.addr, io.in.fire()))
  val thisReqAddr = io.in.bits.addr
  val lineMask = Cat(Fill(AddrBits - 6, 1.U(1.W)), 0.U(6.W))
  val neqAddr = (thisReqAddr & lineMask) =/= (lastReqAddr & lineMask)

  when (!getNewReq) {
    io.out.bits <> io.in.bits
    io.out.valid := io.in.valid
    io.in.ready := !io.in.valid || io.out.fire()
    getNewReq := io.in.fire() && io.in.bits.isBurst() && neqAddr
  }.otherwise {
    io.out.bits <> prefetchReq
    io.out.valid := true.B
    io.in.ready := false.B
    getNewReq := !io.out.fire()
  }
  
  Debug() {
    printf("%d: [Prefetcher]: in(%d,%d), out(%d,%d), in.bits.addr = %x\n",
      GTimer(), io.in.valid, io.in.ready, io.out.valid, io.out.ready, io.in.bits.addr)
  }
}
