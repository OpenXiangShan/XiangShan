package system

import noop.{NOOP, NOOPConfig, HasNOOPParameter, Cache, CacheConfig}
import bus.axi4.{AXI4, AXI4Lite}
import bus.simplebus._

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
	/*
	io.in.ready := !io.in.valid || io.out.fire()
	val lastReq = RegEnable(io.in.bits, io.in.fire())
	val lastAddr = lastReq.addr
	io.out.bits := lastReq
	io.out.bits.cmd := SimpleBusCmd.prefetch
	io.out.bits.addr := lastAddr + Cat(Cat(0.U((TagBits + IndexBits - 1).W), 1.U(1.W)), 0.U(OffsetBits.W))
	io.out.valid := io.in.valid
	*/
	io.out.bits := io.in.bits
	io.out.bits.cmd := SimpleBusCmd.prefetch
	// io.out.bits.addr := io.in.bits.addr + Cat(Cat(0.U((TagBits + IndexBits - 1).W), 1.U(1.W)), 0.U(OffsetBits.W))
	io.out.bits.addr := io.in.bits.addr + 64.U(32.W)
	io.out.valid := io.in.valid
	io.in.ready := !io.in.valid || io.out.fire()
}
