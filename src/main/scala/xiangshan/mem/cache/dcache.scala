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

object CacheOp {
  def load   = "b00".U
  def store  = "b01".U
  def refill = "b11".U
  def empty  = "b10".U

  def width  = load.getWidth
}

class DcacheUserBundle extends XSBundle {
  val uop = Output(new MicroOp) //FIXME: opt data width
  val mmio = Output(Bool())
  val mask  = Output(UInt((XLEN/8).W))
  // val tlbmiss = Output(Bool())
  // for pipeline test
  val id  = Output(UInt(1.W)) // 0: load  1: store
  val paddr = Output(UInt(PAddrBits.W))
}

class DCacheLoadReq extends XSBundle
{
  val paddr  = UInt(PAddrBits.W)
  val vaddr  = UInt(PAddrBits.W)
  val miss = Bool()
  val user = new DcacheUserBundle
}

class DCacheStoreReq extends XSBundle
{
  val paddr  = UInt(PAddrBits.W)
  val data  = UInt(CacheLineSize.W)
  val mask  = UInt((CacheLineSize/8).W)
  val miss = Bool()
  val user = new DcacheUserBundle
}

class DCacheResp extends XSBundle {
  val paddr = UInt(PAddrBits.W)
  val data = UInt(XLEN.W)
  val user = new DcacheUserBundle
}

class DCacheLoadIO extends XSBundle
{
  val req = Flipped(DecoupledIO(new DCacheLoadReq))
  val resp = DecoupledIO(new DCacheResp)
}

class DCacheStoreIO extends XSBundle
{
  val req = Flipped(DecoupledIO(new DCacheStoreReq))
  val resp = DecoupledIO(new DCacheResp)
}

class MissReqIO extends XSBundle
{
  val paddr = UInt(PAddrBits.W)
}

class DcacheToLsuIO extends XSBundle with HasMEMConst {
  val load = Vec(LoadPipelineWidth, new DCacheLoadIO)
  val store = new DCacheStoreIO
  val refill = Flipped(Valid(new DCacheStoreReq))
  val redirect = Flipped(ValidIO(new Redirect))
}

class DcacheIO extends XSBundle with HasMEMConst {
  val lsu = new DcacheToLsuIO
  // val l2 = TODO

  // NutShell cache for pipeline test
  val dmem = new SimpleBusUC(userBits = DcacheUserBundleWidth)
}

class Dcache extends XSModule {
  val io = IO(new DcacheIO)
  
  // Arbiter for 2 dcache ports in built in decache
  // store/refill only use port0, port1 is always assigned to load request

  // priority:
  // load
  // store
  // refill

  // NutShell cache
  assert(!io.lsu.load(1).req.valid)
  io.lsu.refill <> DontCare
  io.lsu.load(1).resp := DontCare
  io.lsu.load(1).resp.valid := false.B
  io.lsu.load(1).req.ready := false.B

  val dmem = io.dmem
  val ldReq = io.lsu.load(0).req
  val stReq = io.lsu.store.req
  val ldResp = io.lsu.load(0).resp
  val stResp = io.lsu.store.resp
  val haveLoadReq = io.lsu.load(0).req.valid
  val ldUser = Wire(new DcacheUserBundle)
  val stUser = Wire(new DcacheUserBundle)

  ldUser.uop := ldReq.bits.user.uop
  ldUser.mmio := ldReq.bits.user.mmio
  ldUser.mask := DontCare
  ldUser.id := 0.U
  ldUser.paddr := ldReq.bits.paddr
  stUser.uop := stReq.bits.user.uop
  stUser.mmio := stReq.bits.user.mmio
  stUser.mask := stReq.bits.mask
  stUser.id := 1.U
  stUser.paddr := stReq.bits.paddr

  dmem.req.bits.apply(
    addr = Mux(haveLoadReq, ldReq.bits.paddr, stReq.bits.paddr), // VM is ignored
    size = Mux(haveLoadReq, ldReq.bits.user.uop.ctrl.fuOpType(1,0), stReq.bits.user.uop.ctrl.fuOpType(1,0)), 
    wdata = stReq.bits.data(63, 0), // just for test
    wmask = stReq.bits.mask(7,0),  // just for test
    cmd = Mux(haveLoadReq, SimpleBusCmd.write, SimpleBusCmd.read)
  )
  dmem.req.valid := Mux(haveLoadReq, ldReq.valid, stReq.valid)
  dmem.resp.ready := true.B

  ldReq.ready := dmem.req.ready && haveLoadReq
  stReq.ready := dmem.req.ready && !haveLoadReq

  ldResp.valid := dmem.resp.fire() && dmem.resp.bits.user.get.asTypeOf(new DcacheUserBundle).id === 0.U
  ldResp.bits.paddr := dmem.resp.bits.user.get.asTypeOf(new DcacheUserBundle).paddr
  ldResp.bits.data := dmem.resp.bits.rdata
  ldResp.bits.user := dmem.resp.bits.user.get.asTypeOf(new DcacheUserBundle)

  stResp.valid := dmem.resp.fire() && dmem.resp.bits.user.get.asTypeOf(new DcacheUserBundle).id === 1.U
  stResp.bits.paddr := dmem.resp.bits.user.get.asTypeOf(new DcacheUserBundle).paddr
  stResp.bits.data := dmem.resp.bits.rdata
  stResp.bits.user := dmem.resp.bits.user.get.asTypeOf(new DcacheUserBundle)

  XSInfo(io.dmem.req.fire() && io.dmem.req.bits.cmd =/= SimpleBusCmd.write, "[DMEM LOAD  REQ] addr 0x%x wdata 0x%x size %d\n", dmem.req.bits.addr, dmem.req.bits.wdata, dmem.req.bits.size)
  XSInfo(io.dmem.req.fire() && io.dmem.req.bits.cmd === SimpleBusCmd.write, "[DMEM STORE REQ] addr 0x%x wdata 0x%x size %d mask %b\n", dmem.req.bits.addr, dmem.req.bits.wdata, dmem.req.bits.size, dmem.req.bits.wmask(7,0))
  XSInfo(io.dmem.resp.fire() && io.dmem.resp.bits.user.get.asTypeOf(new DcacheUserBundle).id === 0.U, "[DMEM LOAD  RESP] data %x\n", io.dmem.resp.bits.rdata)
  XSInfo(io.dmem.resp.fire() && io.dmem.resp.bits.user.get.asTypeOf(new DcacheUserBundle).id === 1.U, "[DMEM STORE RESP] data %x\n", io.dmem.resp.bits.rdata)
}