package system

import noop._
import bus.axi4.{AXI4, AXI4Lite}
import bus.simplebus._
import device.AXI4Timer

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

trait HasSoCParameter {
  val EnableILA = false
  val HasL2cache = true
  val HasPrefetch = true
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
    val frontend = Flipped(new AXI4)
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

  val axi2sb = Module(new AXI42SimpleBusConverter())
  axi2sb.io.in <> io.frontend
  noop.io.frontend <> axi2sb.io.out

  if (HasL2cache) {
    val l2cacheOut = Wire(new SimpleBusC)
    val l2cacheIn = if (HasPrefetch) {
      val prefetcher = Module(new Prefetcher)
      val l2cacheIn = Wire(new SimpleBusUC)
      prefetcher.io.in <> xbar.io.out.req
      l2cacheIn.req <> prefetcher.io.out
      xbar.io.out.resp <> l2cacheIn.resp
      l2cacheIn
    } else xbar.io.out
    val l2Empty = Wire(Bool())
    l2cacheOut <> Cache(in = l2cacheIn, mmio = 0.U.asTypeOf(new SimpleBusUC) :: Nil, flush = "b00".U, empty = l2Empty, enable = true)(
      CacheConfig(name = "l2cache", totalSize = 128, cacheLevel = 2))
    io.mem <> l2cacheOut.mem.toAXI4()
    l2cacheOut.coh.resp.ready := true.B
    l2cacheOut.coh.req.valid := false.B
    l2cacheOut.coh.req.bits := DontCare
  } else {
    io.mem <> xbar.io.out.toAXI4()
  }
  
  noop.io.imem.coh.resp.ready := true.B
  noop.io.imem.coh.req.valid := false.B
  noop.io.imem.coh.req.bits := DontCare

  val addrSpace = List(
    (0x40000000L, 0x08000000L), // external devices
    (0x48000000L, 0x00010000L)  // CLINT
  )
  val mmioXbar = Module(new SimpleBusCrossbar1toN(addrSpace))
  mmioXbar.io.in <> noop.io.mmio

  val extDev = mmioXbar.io.out(0)
  val clint = Module(new AXI4Timer(sim = !p.FPGAPlatform))
  clint.io.in <> mmioXbar.io.out(1).toAXI4Lite()
  if (p.FPGAPlatform) io.mmio <> extDev.toAXI4Lite()
  else io.mmio <> extDev

  //val mtipSync = RegNext(RegNext(io.mtip))
  val mtipSync = clint.io.extra.get.mtip
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
