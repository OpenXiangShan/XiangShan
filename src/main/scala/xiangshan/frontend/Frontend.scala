package xiangshan.frontend
import utils.XSInfo
import chisel3._
import chisel3.util._
import utils.PipelineConnect
import xiangshan._
import xiangshan.cache._


class Frontend extends XSModule {
  val io = IO(new Bundle() {
    val backend = new FrontendToBackendIO
  })

  val ifu = Module(new IFU)
  val icache = ICache(enableICache = true)
  val ibuffer =  if(EnableLB) Module(new LoopBuffer) else Module(new Ibuffer)

  val needFlush = io.backend.redirect.valid

  //backend
  ifu.io.redirect <> io.backend.redirect
  ifu.io.inOrderBrInfo <> io.backend.inOrderBrInfo
  ifu.io.outOfOrderBrInfo <> io.backend.outOfOrderBrInfo

  //cache
  icache.io.req <> ifu.io.icacheReq
  ifu.io.icacheResp <> icache.io.resp
  icache.io.flush := ifu.io.icacheFlush

  //ibuffer
  ibuffer.io.in <> ifu.io.fetchPacket
  ibuffer.io.flush := needFlush

  io.backend.cfVec <> ibuffer.io.out

  for(out <- ibuffer.io.out){
    XSInfo(out.fire(),
      p"inst:${Hexadecimal(out.bits.instr)} pc:${Hexadecimal(out.bits.pc)}\n"
    )
  }


}
