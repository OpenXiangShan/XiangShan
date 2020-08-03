package xiangshan.mem.cache

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap
import xiangshan.mem._
import xiangshan.mem.pipeline._
import bus.simplebus._

trait HasPtwConst extends HasTLBConst{
  val PtwWidth = 2
}

abstract class PtwBundle extends XSBundle with HasPtwConst
abstract class PtwModule extends XSModule with HasPtwConst

class PTWReq extends PtwBundle {
  val vpn = UInt(vpnLen.W)
  val cmd = SimpleBusCmd()
}

class PTWResp extends PtwBundle {
  val pte = UInt(XLEN.W)
  val level = UInt(log2Up(Level).W)
}

class PTWIO extends PtwBundle {
  val req = Vec(PtwWidth, Flipped(Decoupled(new PTWReq)))
  val resp = Vec(PtwWidth, Decoupled(new PTWResp))
  val sfence = Flipped(ValidIO(new SfenceBundle))
  val csr = Flipped(new TlbCsrIO)
  val mem = new DCacheLoadIO // Use Dcache temp
}

class PTW extends PtwModule {
  val io = IO(new PTWIO)

  io <> DontCare
}