package xiangshan.frontend
import utils.XSInfo
import chisel3._
import chisel3.util._
import utils.PipelineConnect
import xiangshan._
import xiangshan.cache._


class Frontend extends XSModule {
  val io = IO(new Bundle() {
    val icacheReq = DecoupledIO(new ICacheReq)
    val icacheResp = Flipped(DecoupledIO(new ICacheResp))
    val icacheFlush = Output(UInt(2.W))
    val icacheToTlb = Flipped(new BlockTlbRequestIO)
    val ptw = new TlbPtwIO
    val backend = new FrontendToBackendIO
    val sfence = Input(new SfenceBundle)
    val tlbCsr = Input(new TlbCsrBundle)
  })

  val ifu = Module(new IFU)
  val ibuffer =  Module(new Ibuffer)

  val needFlush = io.backend.redirect.valid

  //backend
  ifu.io.redirect <> io.backend.redirect
  ifu.io.inOrderBrInfo <> io.backend.inOrderBrInfo
  ifu.io.outOfOrderBrInfo <> io.backend.outOfOrderBrInfo
  //icache
  io.icacheReq <> ifu.io.icacheReq
  io.icacheFlush <> ifu.io.icacheFlush
  ifu.io.icacheResp <> io.icacheResp
  //itlb to ptw
  io.ptw <> TLB(
    in = Seq(io.icacheToTlb),
    sfence = io.sfence,
    csr = io.tlbCsr,
    width = 1,
    isDtlb = false,
    shouldBlock = true
  )
  //ibuffer
  ibuffer.io.in <> ifu.io.fetchPacket
  ibuffer.io.flush := needFlush

  io.backend.cfVec <> ibuffer.io.out

  // for(out <- ibuffer.io.out){
  //   XSInfo(out.fire(),
  //     p"inst:${Hexadecimal(out.bits.instr)} pc:${Hexadecimal(out.bits.pc)}\n"
  //   )
  // }


}