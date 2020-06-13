package xiangshan

import chisel3._
import chisel3.util._
import bus.simplebus._
import noop.{Cache, CacheConfig, HasExceptionNO, TLB, TLBConfig}
import utils.PipelineConnect
import xiangshan.backend._
import xiangshan.ifu.FakeIFU

trait HasXSParameter {
  val XLEN = 64
  val HasMExtension = true
  val HasCExtension = true
  val HasDiv = true
  val HasIcache = true
  val HasDcache = true
  val EnableStoreQueue = false
  val AddrBits = 64 // AddrBits is used in some cases
  val VAddrBits = 39 // VAddrBits is Virtual Memory addr bits
  val PAddrBits = 32 // PAddrBits is Phyical Memory addr bits
  val AddrBytes = AddrBits / 8 // unused
  val DataBits = XLEN
  val DataBytes = DataBits / 8
  val HasFPU = true
  val FetchWidth = 8
  val CommitWidth = 6
}

abstract class XSModule extends Module
  with HasXSParameter
  with HasExceptionNO

abstract class XSBundle extends Bundle
  with HasXSParameter

case class XSConfig
(
  FPGAPlatform: Boolean = true,
  EnableDebug: Boolean = false
)

class XSCore(implicit val p: XSConfig) extends XSModule {
  val io = IO(new Bundle {
    val imem = new SimpleBusC
    val dmem = new SimpleBusC
    val mmio = new SimpleBusUC
    val frontend = Flipped(new SimpleBusUC())
  })

  io.imem <> DontCare

  val dmemXbar = Module(new SimpleBusCrossbarNto1(3))

  val ifu = Module(new FakeIFU)
  val backend = Module(new Backend)

  ifu.io.redirect := backend.io.redirect
  PipelineConnect(ifu.io.fetchPacket, backend.io.fetchPacket, true.B, false.B)

  backend.io.memMMU.imem <> DontCare

  val dtlb = TLB(
    in = backend.io.dmem,
    mem = dmemXbar.io.in(1),
    flush = false.B,
    csrMMU = backend.io.memMMU.dmem
  )(TLBConfig(name = "dtlb", totalEntry = 64))
  dmemXbar.io.in(0) <> dtlb.io.out
  dmemXbar.io.in(2) <> io.frontend

  io.dmem <> Cache(
    in = dmemXbar.io.out,
    mmio = Seq(io.mmio),
    flush = "b00".U,
    empty = dtlb.io.cacheEmpty,
    enable = HasDcache
  )(CacheConfig(name = "dcache"))
}
