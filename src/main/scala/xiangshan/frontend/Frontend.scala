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
  val icache = Module(new ICache)

  val needFlush = io.backend.redirect.valid

  //backend
  ifu.io.redirect <> io.backend.redirect
  ifu.io.inOrderBrInfo <> io.backend.inOrderBrInfo
  ifu.io.outOfOrderBrInfo <> io.backend.outOfOrderBrInfo
  //icache
  ifu.io.icacheResp <> icache.io.resp
  icache.io.req <> ifu.io.icacheReq
  icache.io.flush <> ifu.io.icacheFlush
  icache.io.fencei := io.fencei
  io.l1plusFlush := icache.io.l1plusflush
  io.icacheMemAcq <> icache.io.mem_acquire
  icache.io.mem_grant <> io.icacheMemGrant
  //itlb to ptw
  io.ptw <> TLB(
    in = Seq(icache.io.tlb),
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