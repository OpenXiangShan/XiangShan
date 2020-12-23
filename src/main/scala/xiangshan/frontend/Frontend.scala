package xiangshan.frontend
import utils.XSInfo
import chisel3._
import chisel3.util._
import utils.PipelineConnect
import xiangshan._
import xiangshan.cache._


class Frontend extends XSModule {
  val io = IO(new Bundle() {
    val icacheMemAcq = DecoupledIO(new L1plusCacheReq)
    val icacheMemGrant = Flipped(DecoupledIO(new L1plusCacheResp))
    val l1plusFlush = Output(Bool())
    val fencei = Input(Bool())
    val ptw = new TlbPtwIO
    val backend = new FrontendToBackendIO
    val sfence = Input(new SfenceBundle)
    val tlbCsr = Input(new TlbCsrBundle)
  })

  val ifu = Module(new IFU)
  val ibuffer =  Module(new Ibuffer)
  

  val needFlush = io.backend.redirect.valid

  // from backend
  ifu.io.redirect <> io.backend.redirect
  ifu.io.cfiUpdateInfo <> io.backend.cfiUpdateInfo
  // to icache
  ifu.io.icacheMemGrant <> io.icacheMemGrant
  ifu.io.fencei := io.fencei
  // from icache
  io.l1plusFlush := ifu.io.l1plusFlush
  io.icacheMemAcq <> ifu.io.icacheMemAcq
  // itlb to ptw
  io.ptw <> ifu.io.ptw
  // ifu to ibuffer
  ibuffer.io.in <> ifu.io.fetchPacket
  // backend to ibuffer
  ibuffer.io.flush := needFlush
  // ibuffer to backend
  io.backend.cfVec <> ibuffer.io.out

  // for(out <- ibuffer.io.out){
  //   XSInfo(out.fire(),
  //     p"inst:${Hexadecimal(out.bits.instr)} pc:${Hexadecimal(out.bits.pc)}\n"
  //   )
  // }


}