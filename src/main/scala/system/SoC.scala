package system

import noop._
import bus.axi4.{AXI4, AXI4Lite}
import bus.simplebus._

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

trait HasSoCParameter {
  val EnableILA = false
  val HasL2cache = false
  val HasPrefetch = false
}

class ILABundle extends Bundle {
  val WBUpc = UInt(32.W)
  val WBUvalid = UInt(1.W)
  val WBUrfWen = UInt(1.W)
  val WBUrfDest = UInt(5.W)
  val WBUrfData = UInt(64.W)
  val InstrCnt = UInt(64.W)
}

class NOOPSoC(implicit val p: NOOPConfig) extends Module with HasSoCParameter {
  val io = IO(new Bundle{
    val mem = new AXI4
    val mmio = (if (p.FPGAPlatform) { new AXI4Lite } else { new SimpleBusUC })
    val mtip = Input(Bool())
    val meip = Input(Bool())
    val ila = if (p.FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
  })

  val noop = Module(new NOOP)

	val cohMg = Module(new CoherenceManager)
  val xbar = Module(new SimpleBusCrossbarNto1(2))
  cohMg.io.in <> noop.io.imem.mem
  noop.io.dmem.coh <> cohMg.io.out.coh
  xbar.io.in(0) <> cohMg.io.out.mem
  xbar.io.in(1) <> noop.io.dmem.mem

	if (HasL2cache) {
    val l2cacheOut = Wire(new SimpleBusC)
    if (HasPrefetch) {
			val prefetcher = Module(new Prefetcher)
			prefetcher.io.in <> noop.io.prefetchReq
			val l2cacheIn = Wire(new SimpleBusUC)
			val l2cacheInReqArb = Module(new Arbiter(chiselTypeOf(noop.io.prefetchReq.bits), 2))
			l2cacheInReqArb.io.in(0) <> xbar.io.out.req
			l2cacheInReqArb.io.in(1) <> prefetcher.io.out
			l2cacheIn.req <> l2cacheInReqArb.io.out
			xbar.io.out.resp <> l2cacheIn.resp
			l2cacheOut <> Cache(in = l2cacheIn, mmio = 0.U.asTypeOf(new SimpleBusUC), flush = "b00".U, enable = true)(CacheConfig(ro = false, name = "l2cache", cacheLevel = 2))
		} else {
			l2cacheOut <> Cache(in = xbar.io.out, mmio = 0.U.asTypeOf(new SimpleBusUC), flush = "b00".U, enable = true)(CacheConfig(ro = false, name = "l2cache", cacheLevel = 2))
		}
    io.mem <> l2cacheOut.mem.toAXI4()
		l2cacheOut.coh.resp.ready := true.B
		l2cacheOut.coh.req.valid := false.B
		l2cacheOut.coh.req.bits := DontCare
  } else {
    io.mem <> xbar.io.out.toAXI4()
  }

	if (!HasPrefetch) {
		noop.io.prefetchReq.ready := true.B
	}

  noop.io.imem.coh.resp.ready := true.B
  noop.io.imem.coh.req.valid := false.B
  noop.io.imem.coh.req.bits := DontCare

  if (p.FPGAPlatform) io.mmio <> noop.io.mmio.toAXI4Lite()
  else io.mmio <> noop.io.mmio

  val mtipSync = RegNext(RegNext(io.mtip))
  val meipSync = RegNext(RegNext(io.meip))
  BoringUtils.addSource(mtipSync, "mtip")
  BoringUtils.addSource(meipSync, "meip")

  // ILA
  if (p.FPGAPlatform) {
    def BoringUtilsConnect(sink: UInt, id: String) {
      val temp = WireInit(0.U(64.W))
      BoringUtils.addSink(temp, id)
      sink := temp
    }

    val dummy = WireInit(0.U.asTypeOf(new ILABundle))
    val ila = io.ila.getOrElse(dummy)
    BoringUtilsConnect(ila.WBUpc      ,"ilaWBUpc")
    BoringUtilsConnect(ila.WBUvalid   ,"ilaWBUvalid")
    BoringUtilsConnect(ila.WBUrfWen   ,"ilaWBUrfWen")
    BoringUtilsConnect(ila.WBUrfDest  ,"ilaWBUrfDest")
    BoringUtilsConnect(ila.WBUrfData  ,"ilaWBUrfData")
    BoringUtilsConnect(ila.InstrCnt   ,"ilaInstrCnt")
  }
}
