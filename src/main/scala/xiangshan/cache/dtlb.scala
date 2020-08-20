package xiangshan.cache

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class DtlbReq extends XSBundle {
  val vaddr = UInt(VAddrBits.W)
}

class DtlbResp extends XSBundle {
  val paddr = UInt(PAddrBits.W)
  val miss = Bool()
}

class DtlbToLsuIO extends XSBundle {
  val req = Flipped(ValidIO(new DtlbReq))
  val resp = ValidIO(new DtlbResp)
}

class DtlbIO extends XSBundle {
  val lsu = Vec(LoadPipelineWidth + StorePipelineWidth, new DtlbToLsuIO)
  // val l2 = TODO
}

class Dtlb extends XSModule {
  val io = IO(new DtlbIO)
  // Dtlb has 4 ports: 2 for load, 2 fore store 

  // fake dtlb
  (0 until LoadPipelineWidth + StorePipelineWidth).map(i => {
    io.lsu(i).resp.valid := io.lsu(i).req.valid
    io.lsu(i).resp.bits.paddr := io.lsu(i).req.bits.vaddr
    io.lsu(i).resp.bits.miss := LFSR64()(5, 0) === 0.U
    when(io.lsu(i).req.valid){
      XSDebug("vaddr %x paddr %x miss %b\n",
        io.lsu(i).req.bits.vaddr, io.lsu(i).resp.bits.paddr, io.lsu(i).resp.bits.miss)
    }
  })
}
