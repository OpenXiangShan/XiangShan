package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._

class SbufferUserBundle extends XSBundle {
  val pc = UInt(VAddrBits.W) //for debug
  val lsroqId = UInt(log2Up(LsroqSize).W)
}

// Store buffer for XiangShan Out of Order LSU
class Sbuffer extends XSModule with NeedImpl{
  val io = IO(new Bundle() {
    val in = Vec(StorePipelineWidth, Flipped(Decoupled(new DCacheStoreReq)))
    val dcache = new DCacheStoreIO
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
  })

  assert(!(io.in(1).ready && !io.in(0).ready))

  // Get retired store from lsroq
  (0 until StorePipelineWidth).map(i => {
    io.in(i).ready := DontCare
    when(io.in(i).fire()){
      // Add to sbuffer
      // io.in(i).paddr
      // io.in(i).data
      // io.in(i).mask // 8 bit -> 1 bit mask
    }
  })

  // Write back to dcache
  io.dcache.req.valid := DontCare //needWriteToCache
  io.dcache.req.bits.addr := DontCare
  io.dcache.req.bits.data := DontCare
  io.dcache.req.bits.mask := DontCare

  when(io.dcache.req.fire()){
    // TODO
  }

  // loadForwardQuery
  (0 until LoadPipelineWidth).map(i => {
    io.forward(i).forwardMask := VecInit(List.fill(XLEN / 8)(false.B))
    io.forward(i).forwardData := DontCare
    // (0 until SbufferSize).map(i => {
    //   when(io.loadForwardQuery.paddr match sbuffer(i).paddr){
    //     (0 until XLEN / 8).map(j => {
    //       when(io.loadForwardQuery.mask match sbuffer(i)(j).mask){
    //         io.loadForwardQuery.forwardMask(j) := true.B
    //         io.loadForwardQuery.forwardData(j) := sbuffer(i)(j).data
    //       }
    //     })
    //   }
    // })
  })
}
  
// Fake Store buffer for XiangShan Out of Order LSU
// NutShell DCache Interface
class FakeSbuffer extends XSModule {
  val io = IO(new Bundle() {
    val in = Vec(StorePipelineWidth, Flipped(Decoupled(new DCacheStoreReq)))
    val dcache = new DCacheStoreIO
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
  })

  io.in(1) := DontCare
  io.in(1).ready := false.B
  assert(!(io.in(1).ready && !io.in(0).ready))
  // To make lsroq logic simpler, we assume io.in(0).ready == io.in(1).ready ?

  // store req will go to DCache directly, forward is not needed here
  (0 until 2).map(i => {
    io.forward(i) := DontCare
    io.forward(i).forwardMask := 0.U(8.W).asBools
  })

  io.dcache.req <> io.in(0)
  io.dcache.resp.ready := true.B
  XSInfo(io.in(0).fire(), "ensbuffer addr 0x%x wdata 0x%x\n", io.in(0).bits.addr, io.in(0).bits.data)
  XSInfo(io.in(1).fire(), "ensbuffer addr 0x%x wdata 0x%x\n", io.in(1).bits.addr, io.in(1).bits.data)
  XSInfo(io.dcache.req.fire(), "desbuffer addr 0x%x wdata 0x%x\n", io.dcache.req.bits.addr, io.dcache.req.bits.data)
}
