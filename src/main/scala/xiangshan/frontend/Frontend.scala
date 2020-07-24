package xiangshan.frontend
import utils.XSInfo
import chisel3._
import chisel3.util._
import utils.PipelineConnect
import xiangshan._


class Frontend extends XSModule {
  val io = IO(new Bundle() {
    val backend = new FrontendToBackendIO
  })

  val ifu = Module(new IFU)
  val fakeicache = Module(new FakeCache)
  val lbuffer =  Module(new LoopBuffer)

  val needFlush = io.backend.redirectInfo.flush()

  ifu.io.redirectInfo <> io.backend.redirectInfo
  fakeicache.io.in <> ifu.io.icacheReq
  ifu.io.icacheResp <> fakeicache.io.out

  lbuffer.io.in <> ifu.io.fetchPacket
  lbuffer.io.flush := needFlush

  io.backend.cfVec <> lbuffer.io.out

  for(out <- lbuffer.io.out){
    XSInfo(out.fire(),
      p"inst:${Hexadecimal(out.bits.instr)} pc:${Hexadecimal(out.bits.pc)}\n"
    )
  }


}
