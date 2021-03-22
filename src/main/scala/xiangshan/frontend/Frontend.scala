package xiangshan.frontend
import utils._
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.prefetch.L1plusPrefetcher
import xiangshan.backend.fu.HasExceptionNO
import system.L1CacheErrorInfo


class Frontend()(implicit p: Parameters) extends LazyModule with HasXSParameter{

  val instrUncache = LazyModule(new InstrUncache())

  lazy val module = new FrontendImp(this)
}


class FrontendImp (outer: Frontend) extends LazyModuleImp(outer)
  with HasL1plusCacheParameters 
  with HasXSParameter
  with HasExceptionNO
  with HasXSLog
{
  val io = IO(new Bundle() {
    val icacheMemAcq = DecoupledIO(new L1plusCacheReq)
    val icacheMemGrant = Flipped(DecoupledIO(new L1plusCacheResp))
    val l1plusFlush = Output(Bool())
    val fencei = Input(Bool())
    val ptw = new TlbPtwIO
    val backend = new FrontendToBackendIO
    val sfence = Input(new SfenceBundle)
    val tlbCsr = Input(new TlbCsrBundle)
    val csrCtrl = Input(new CustomCSRCtrlIO)
    val error  = new L1CacheErrorInfo
  })

  val ifu = Module(new IFU)
  val ibuffer =  Module(new Ibuffer)
  val l1plusPrefetcher = Module(new L1plusPrefetcher)
  val instrUncache = outer.instrUncache.module

  val needFlush = io.backend.redirect_cfiUpdate.valid

  // from backend
  ifu.io.redirect <> io.backend.redirect_cfiUpdate
  ifu.io.bp_ctrl <> io.csrCtrl.bp_ctrl
  ifu.io.commitUpdate <> io.backend.commit_cfiUpdate
  ifu.io.ftqEnqPtr <> io.backend.ftqEnqPtr
  ifu.io.ftqLeftOne <> io.backend.ftqLeftOne
  // to icache
  val grantClientId = clientId(io.icacheMemGrant.bits.id)
  val grantEntryId = entryId(io.icacheMemGrant.bits.id)
  ifu.io.icacheMemGrant.valid := io.icacheMemGrant.valid && grantClientId === icacheMissQueueId.U
  ifu.io.icacheMemGrant.bits := io.icacheMemGrant.bits
  ifu.io.icacheMemGrant.bits.id := Cat(0.U(clientIdWidth.W), grantEntryId)
  l1plusPrefetcher.io.mem_grant.valid := io.icacheMemGrant.valid && grantClientId === l1plusPrefetcherId.U
  l1plusPrefetcher.io.mem_grant.bits := io.icacheMemGrant.bits
  l1plusPrefetcher.io.mem_grant.bits.id := Cat(0.U(clientIdWidth.W), grantEntryId)
  io.icacheMemGrant.ready := Mux(grantClientId === icacheMissQueueId.U,
    ifu.io.icacheMemGrant.ready,
    l1plusPrefetcher.io.mem_grant.ready)
  ifu.io.fencei := io.fencei


  instrUncache.io.req <> ifu.io.mmio_acquire
  instrUncache.io.resp <> ifu.io.mmio_grant
  instrUncache.io.flush <> ifu.io.mmio_flush
  // to tlb
  ifu.io.sfence := io.sfence
  ifu.io.tlbCsr := io.tlbCsr
  // from icache and l1plus prefetcher
  io.l1plusFlush := ifu.io.l1plusFlush
  l1plusPrefetcher.io.in.valid := ifu.io.prefetchTrainReq.valid
  l1plusPrefetcher.io.in.bits := ifu.io.prefetchTrainReq.bits
  l1plusPrefetcher.io.enable := RegNext(io.csrCtrl.l1plus_pf_enable)
  val memAcquireArb = Module(new Arbiter(new L1plusCacheReq, nClients))
  memAcquireArb.io.in(icacheMissQueueId) <> ifu.io.icacheMemAcq
  memAcquireArb.io.in(icacheMissQueueId).bits.id := Cat(icacheMissQueueId.U(clientIdWidth.W),
    entryId(ifu.io.icacheMemAcq.bits.id))
  memAcquireArb.io.in(l1plusPrefetcherId) <> l1plusPrefetcher.io.mem_acquire
  memAcquireArb.io.in(l1plusPrefetcherId).bits.id := Cat(l1plusPrefetcherId.U(clientIdWidth.W),
    entryId(l1plusPrefetcher.io.mem_acquire.bits.id))
  io.icacheMemAcq <> memAcquireArb.io.out
  // itlb to ptw
  io.ptw <> ifu.io.ptw
  // ifu to ibuffer
  ibuffer.io.in <> ifu.io.fetchPacket
  // backend to ibuffer
  ibuffer.io.flush := needFlush
  // ibuffer to backend
  io.backend.cfVec <> ibuffer.io.out
  // ifu to backend
  io.backend.fetchInfo <> ifu.io.toFtq

  io.error <> RegNext(ifu.io.error)

  // for(out <- ibuffer.io.out){
  //   XSInfo(out.fire(),
  //     p"inst:${Hexadecimal(out.bits.instr)} pc:${Hexadecimal(out.bits.pc)}\n"
  //   )
  // }

  val frontendBubble = PopCount((0 until DecodeWidth).map(i => io.backend.cfVec(i).ready && !ibuffer.io.out(i).valid))
  XSPerf("FrontendBubble", frontendBubble)
}
