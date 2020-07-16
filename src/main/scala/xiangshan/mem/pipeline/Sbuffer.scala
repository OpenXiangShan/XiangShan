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

trait HasSBufferConst extends HasXSParameter {
  val tagWidth = PAddrBits - log2Up(CacheLineSize / 8)
  val lruCounterWidth = 32
}

class SBufferCacheLine extends XSBundle with HasSBufferConst {
  val valid = Bool()
  val tag = UInt(tagWidth.W)
  val data = UInt(CacheLineSize.W)
  val mask = UInt((CacheLineSize / 8).W)
  val lruCnt = UInt(lruCounterWidth.W)
}

// Store buffer for XiangShan Out of Order LSU
class Sbuffer(implicit val p: XSConfig) extends XSModule with HasMEMConst with NeedImpl{
  val io = IO(new Bundle() {
    // DCacheStoreReq:
    // * receive pa and data from lsroq
    // * mask for cache line
    // * miss?
    // * user: uop + ismmio + mask for double word
    val in = Vec(StorePipelineWidth, Flipped(Decoupled(new DCacheStoreReq)))

    // DCacheStoreIO
    // * req: output DCacheStoreReq
    // * resp: input DCacheResp
    val dcache = Flipped(new DCacheStoreIO)
    // LoadForward
    // input:
    //   * paddr and mask(8-bit)
    //   * moqIdx: index in lsroq
    //   * pc
    // output:
    //   * mask and data vec (8 for 8 bytes)
    //     - mask: bool
    //     - data: represent 1 byte
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
  })

  val cache = RegInit(VecInit(Seq.fill(SbufferSize)(0.U.asTypeOf(new SBufferCacheLine))))

  // Get retired store from lsroq
  (0 until StorePipelineWidth).map(i => {
    io.in(i).ready := DontCare // when there is empty line for this query
    when(io.in(i).fire()){
      // Add to sbuffer
      // io.in(i).paddr
      // io.in(i).data
      // io.in(i).mask // 8 bit -> 1 bit mask

      // 1. search if paddr falls in current lines
      //    * update current 'data' and 'mask' field, then set lruCnt to 0
      // 2. otherwise
      //                 if there is empty line: modify valid + tag + data + mask + lruCnt
      //                 else: write back certain line first
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
  