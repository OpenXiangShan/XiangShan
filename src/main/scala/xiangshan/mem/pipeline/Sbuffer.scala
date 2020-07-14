package xiangshan.mem.pipeline

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap
import xiangshan.mem._
import xiangshan.mem.cache._
import bus.simplebus._

class SbufferUserBundle extends XSBundle with HasMEMConst {
  val pc = UInt(VAddrBits.W) //for debug
  val lsroqId = UInt(log2Up(LSRoqSize).W)
}

// Store buffer for XiangShan Out of Order LSU
class Sbuffer(implicit val p: XSConfig) extends XSModule with HasMEMConst with NeedImpl{
  val io = IO(new Bundle() {
    val in = Vec(StorePipelineWidth, Flipped(Decoupled(new DCacheStoreReq)))
    val dcache = Flipped(new DCacheStoreIO)
    val loadForwardQuery = Flipped(new LoadForwardQueryIO)
  })

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
  io.dcache.req.bits.paddr := DontCare
  io.dcache.req.bits.data := DontCare
  io.dcache.req.bits.mask := DontCare

  when(io.dcache.req.fire()){
    // TODO
  }

  // loadForwardQuery
  io.loadForwardQuery.forwardMask := VecInit(List.fill(XLEN / 8)(false.B))
  io.loadForwardQuery.forwardData := DontCare
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
}
  