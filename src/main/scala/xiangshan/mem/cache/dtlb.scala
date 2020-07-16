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

class DtlbReq extends XSBundle with HasMEMConst {
  val vaddr = UInt(VAddrBits.W)
}

class DtlbResp extends XSBundle with HasMEMConst {
  val paddr = UInt(PAddrBits.W)
  val miss = Bool()
}

class DtlbToLsuIO extends XSBundle with HasMEMConst {
  val req = Vec(LoadPipelineWidth + StorePipelineWidth, Flipped(Valid(new DtlbReq)))
  val resp = Vec(LoadPipelineWidth + StorePipelineWidth, Valid(new DtlbResp))
}

class DtlbIO extends XSBundle with HasMEMConst {
  val lsu = new DtlbToLsuIO
  // val l2 = TODO
}

class Dtlb extends XSModule with HasMEMConst with NeedImpl{
  val io = IO(new DtlbIO)
  // Dtlb has 4 ports: 2 for load, 2 fore store 
  (0 until LoadPipelineWidth + StorePipelineWidth).map(i => {
    io.lsu.resp(i).valid := io.lsu.req(i).valid
    io.lsu.resp(i).bits.miss := DontCare
  })
}