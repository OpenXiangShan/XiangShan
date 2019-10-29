package system

import noop.{NOOP, NOOPConfig, Cache, L2Cache, CacheConfig}
import bus.axi4.{AXI4, AXI4Lite}
import bus.simplebus._

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

class NOOPSoC(implicit val p: NOOPConfig) extends Module {
  val io = IO(new Bundle{
    val mem = new AXI4
    val mmio = (if (p.FPGAPlatform) { new AXI4Lite } else { new SimpleBusUC })
    val mtip = Input(Bool())
    val meip = Input(Bool())
  })

  val noop = Module(new NOOP)
  val cohMg = Module(new CoherenceInterconnect)
  cohMg.io.in(0) <> noop.io.imem
  cohMg.io.in(1) <> noop.io.dmem
	
	/*
	// add L2 Cache and Dcache Prefetcher
	val prefetcher = Module(new Prefetcher)
	prefetcher.io.in <> noop.io.prefetchReq

	val l2cacheIn = Wire(new SimpleBusUC)
	val l2cacheInReqArb = Module(new Arbiter(noop.io.prefetchReq, 2))
	l2cacheInReqArb.io.in(0) <> cohMg.io.out.req
	l2cacheInReqArb.io.in(1) <> prefetcher.io.out
	l2cacheIn.req <> l2cacheInReqArb.io.out
	cohMg.io.out.resp <> l2cacheIn.resp

	val mmioXbar = Module(new SimpleBusCrossbarNto1(2))
	
	val l2cacheOut = Wire(new SimpleBusUC)
	l2cacheOut <> Cache(in = l2cacheIn, mmio = mmioXbar.io.in(0), flush = "b00".U, enable = true)(CacheConfig(ro = false, name = "l2cache", cacheLevel = 2))
	io.mem <> l2cacheOut.toAXI4()

	mmioXbar.io.in(1) <> noop.io.mmio
	if (p.FPGAPlatform) io.mmio <> mmioXbar.io.out.toAXI4Lite()
  else io.mmio <> mmioXbar.io.out
	*/
	
	// add L2 Cache
	val mmioXbar = Module(new SimpleBusCrossbarNto1(2))
	
	val l2cacheOut = Wire(new SimpleBusUC)
	l2cacheOut <> Cache(in = cohMg.io.out, mmio = mmioXbar.io.in(0), flush = "b00".U, enable = true)(CacheConfig(ro = false, name = "l2cache", cacheLevel = 2))
	io.mem <> l2cacheOut.toAXI4()

	mmioXbar.io.in(1) <> noop.io.mmio
	if (p.FPGAPlatform) io.mmio <> mmioXbar.io.out.toAXI4Lite()
  else io.mmio <> mmioXbar.io.out
	
	/*
	// no L2 Cache
	io.mem <> cohMg.io.out.toAXI4()

  if (p.FPGAPlatform) io.mmio <> noop.io.mmio.toAXI4Lite()
  else io.mmio <> noop.io.mmio
	*/
  val mtipSync = RegNext(RegNext(io.mtip))
  val meipSync = RegNext(RegNext(io.meip))
  BoringUtils.addSource(mtipSync, "mtip")
  BoringUtils.addSource(meipSync, "meip")
}
