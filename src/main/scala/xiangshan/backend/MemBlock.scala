package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.roq.RoqPtr
import xiangshan.backend.exu._
import xiangshan.cache._
import xiangshan.mem._
import xiangshan.backend.fu.FenceToSbuffer
import xiangshan.backend.issue.ReservationStationNew
import xiangshan.backend.fu.FunctionUnit.{lduCfg, mouCfg, stuCfg}

class LsBlockToCtrlIO extends XSBundle {
  val stOut = Vec(exuParameters.StuCnt, ValidIO(new ExuOutput)) // write to roq
  val numExist = Vec(exuParameters.LsExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
  val lsqIdxResp = Vec(RenameWidth, Output(new LSIdx))
  val oldestStore = Output(Valid(new RoqPtr))
  val replay = ValidIO(new Redirect)
}

class MemBlockToDcacheIO extends XSBundle {
  val loadUnitToDcacheVec = Vec(exuParameters.LduCnt, new DCacheLoadIO)
  val loadMiss = new DCacheLineIO
  val atomics  = new DCacheWordIO
  val sbufferToDcache = new DCacheLineIO
  val uncache = new DCacheWordIO
}

class MemBlockCSRIO extends XSBundle {
  val exceptionAddr = new ExceptionAddrIO
  val fenceToSbuffer = Flipped(new FenceToSbuffer)
  val sfence = Input(new SfenceBundle)
  val tlbInfo = Input(new TlbCsrBundle)
}

class MemBlock
(
  fastWakeUpIn: Seq[ExuConfig],
  slowWakeUpIn: Seq[ExuConfig],
  fastFpOut: Seq[ExuConfig],
  slowFpOut: Seq[ExuConfig],
  fastIntOut: Seq[ExuConfig],
  slowIntOut: Seq[ExuConfig]
) extends XSModule with NeedImpl {

  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToLsBlockIO)
    val toCtrlBlock = new LsBlockToCtrlIO

    val wakeUpIn = new WakeUpBundle(fastWakeUpIn.size, slowWakeUpIn.size)
    val wakeUpFpOut = Flipped(new WakeUpBundle(fastFpOut.size, slowFpOut.size))
    val wakeUpIntOut = Flipped(new WakeUpBundle(fastIntOut.size, slowIntOut.size))

    val ptw = new TlbPtwIO
    // TODO: dcache should be inside MemBlock
    val dcache = new MemBlockToDcacheIO
    val csr = new MemBlockCSRIO
  })

//  val loadUnits = Array.tabulate(exuParameters.LduCnt)(_ => Module(new LoadUnit))
//  val storeUnits = Array.tabulate(exuParameters.StuCnt)(_ => Module(new StoreUnit))
//  val exeUnits = loadUnits ++ storeUnits
//  val ldExeUnitCfg = ExuConfig("LoadExu", Seq(lduCfg), wbIntPriority = 0, wbFpPriority = 0)
//  val stExeUnitCfg = ExuConfig("StoreExu", Seq(stuCfg, mouCfg), wbIntPriority = Int.MaxValue, wbFpPriority = Int.MaxValue)
//  val exuConfigs = Seq.fill(exuParameters.LduCnt)(ldExeUnitCfg) ++ Seq.fill(exuParameters.StuCnt)(stExeUnitCfg)
//
//  val atomicsUnit = Module(new AtomicsUnit)
//
//  val loadWritebackOverride = Mux(atomicsUnit.io.out.valid, atomicsUnit.io.out, loadUnits.head.io.ldout)
//  val exeWbReqs = loadWritebackOverride +: loadUnits.tail.map(_.io.ldout)
//  val writebackData = exuConfigs.zip(exeWbReqs).filter(x => x._1.hasCertainLatency && x._1.writeIntRf).map(_._2.bits.data)
//  val extraListenPorts = exuConfigs.zip(exeWbReqs).filter(x => x._1.hasUncertainlatency && x._1.writeIntRf).map(_._2)
//
//  val rsConfigs = Seq(5, 5, 9, 9)
//  val reservationStations = exuConfigs.zipWithIndex.map({ case (cfg, i) =>
//    val rs = Module(new ReservationStationNew(cfg, rsConfigs(i), 6, fixedDelay = -1, feedback = true))
//
//    rs.io.redirect <> io.fromCtrlBlock.redirect
//    rs.io.numExist <> io.toCtrlBlock.numExist(i)
//    rs.io.enqCtrl <> io.fromCtrlBlock.enqIqCtrl(i)
//    rs.io.enqData <> io.fromCtrlBlock.enqIqData(i)
//
//    rs.io.writeBackedData <> writebackData ++ io.writebackData
//    for((x, y) <- rs.io.extraListenPorts.zip(extraListenPorts ++ io.extraListenPorts)){
//      x.valid := y.fire()
//      x.bits := y.bits
//    }
//
//    rs.suggestName(s"rs_${cfg.name}")
//    rs
//  })
//
//
//  val dtlb = Module(new TLB(Width = DTLBWidth, isDtlb = true))
//  val lsroq = Module(new LsqWrappper)
//  val sbuffer = Module(new NewSbuffer)
//  // if you wants to stress test dcache store, use FakeSbuffer
//  // val sbuffer = Module(new FakeSbuffer)
//
//  // dtlb
//  io.ptw <> dtlb.io.ptw
//  dtlb.io.sfence <> io.csr.sfence
//  dtlb.io.csr <> io.csr.tlbInfo
//
//  // LoadUnit
//  for (i <- 0 until exuParameters.LduCnt) {
//    loadUnits(i).io.redirect <> io.fromCtrlBlock.redirect
//    loadUnits(i).io.tlbFeedback <> reservationStations(i).io.tlbFeedback
//    loadUnits(i).io.dtlb <> dtlb.io.requestor(i)
//    // get input form dispatch
//    loadUnits(i).io.ldin          <> reservationStations(i).io.deq
//    // dcache access
//    loadUnits(i).io.dcache        <> io.dcache.loadUnitToDcacheVec(i)
//    // forward
//    loadUnits(i).io.lsroq.forward <> lsroq.io.forward(i)
//    loadUnits(i).io.sbuffer       <> sbuffer.io.forward(i)
//
//    // passdown to lsroq
//    lsroq.io.loadIn(i)            <> loadUnits(i).io.lsroq.loadIn
//    lsroq.io.ldout(i)             <> loadUnits(i).io.lsroq.ldout
//  }
//
//  // StoreUnit
//  for (i <- 0 until exuParameters.StuCnt) {
//    storeUnits(i).io.redirect <> io.fromCtrlBlock.redirect
//    storeUnits(i).io.tlbFeedback <> reservationStations(exuParameters.LduCnt + i).io.tlbFeedback
//    storeUnits(i).io.dtlb <> dtlb.io.requestor(exuParameters.LduCnt + i)
//    // get input form dispatch
//    storeUnits(i).io.stin        <> reservationStations(exuParameters.LduCnt + i).io.deq
//    // passdown to lsroq
//    storeUnits(i).io.lsroq       <> lsroq.io.storeIn(i)
//  }
//
//  // Lsroq
//  lsroq.io.commits     <> io.fromCtrlBlock.commits
//  lsroq.io.dp1Req      <> io.fromCtrlBlock.lsqIdxReq
//  lsroq.io.oldestStore <> io.toCtrlBlock.oldestStore
//  lsroq.io.lsIdxs   <> io.toCtrlBlock.lsqIdxResp
//  lsroq.io.brqRedirect := io.fromCtrlBlock.redirect
//  lsroq.io.roqDeqPtr := io.fromCtrlBlock.roqDeqPtr
//
//  io.toCtrlBlock.replay <> lsroq.io.rollback
//
//  lsroq.io.dcache      <> io.dcache.loadMiss
//  lsroq.io.uncache     <> io.dcache.uncache
//
//  // LSROQ to store buffer
//  lsroq.io.sbuffer     <> sbuffer.io.in
//
//  // Sbuffer
//  sbuffer.io.dcache <> io.dcache.sbufferToDcache
//
//  // flush sbuffer
//  val fenceFlush = io.csr.fenceToSbuffer.flushSb
//  val atomicsFlush = atomicsUnit.io.flush_sbuffer.valid
//  io.csr.fenceToSbuffer.sbIsEmpty := sbuffer.io.flush.empty
//  // if both of them tries to flush sbuffer at the same time
//  // something must have gone wrong
//  assert(!(fenceFlush && atomicsFlush))
//  sbuffer.io.flush.valid := fenceFlush || atomicsFlush
//
//  // TODO: make 0/1 configurable
//  // AtomicsUnit
//  // AtomicsUnit will override other control signials,
//  // as atomics insts (LR/SC/AMO) will block the pipeline
//  val st0_atomics = reservationStations(2).io.deq.valid && reservationStations(2).io.deq.bits.uop.ctrl.fuType === FuType.mou
//  val st1_atomics = reservationStations(3).io.deq.valid && reservationStations(3).io.deq.bits.uop.ctrl.fuType === FuType.mou
//  // amo should always go through store issue queue 0
//  assert(!st1_atomics)
//
//  atomicsUnit.io.dtlb.resp.valid := false.B
//  atomicsUnit.io.dtlb.resp.bits  := DontCare
//  atomicsUnit.io.out.ready       := false.B
//
//  // dispatch 0 takes priority
//  atomicsUnit.io.in.valid := st0_atomics
//  atomicsUnit.io.in.bits  := reservationStations(2).io.deq.bits
//  when (st0_atomics) {
//    reservationStations(0).io.deq.ready := atomicsUnit.io.in.ready
//    storeUnits(0).io.stin.valid := false.B
//  }
//
//  when(atomicsUnit.io.dtlb.req.valid) {
//    dtlb.io.requestor(0) <> atomicsUnit.io.dtlb // TODO: check it later
//    // take load unit 0's tlb port
//    // make sure not to disturb loadUnit
//    assert(!loadUnits(0).io.dtlb.req.valid)
//    loadUnits(0).io.dtlb.resp.valid := false.B
//  }
//
//  when(atomicsUnit.io.tlbFeedback.valid) {
//    assert(!storeUnits(0).io.tlbFeedback.valid)
//    atomicsUnit.io.tlbFeedback <> reservationStations(exuParameters.LduCnt + 0).io.tlbFeedback
//  }
//
//  atomicsUnit.io.dcache        <> io.dcache.atomics
//  atomicsUnit.io.flush_sbuffer.empty := sbuffer.io.flush.empty
//
//  atomicsUnit.io.redirect <> io.fromCtrlBlock.redirect
//
//  when(atomicsUnit.io.out.valid){
//    // take load unit 0's write back port
//    assert(!loadUnits(0).io.ldout.valid)
//    loadUnits(0).io.ldout.ready := false.B
//  }
//
//  lsroq.io.exceptionAddr.lsIdx := io.csr.exceptionAddr.lsIdx
//  lsroq.io.exceptionAddr.isStore := io.csr.exceptionAddr.isStore
//  io.csr.exceptionAddr.vaddr := Mux(atomicsUnit.io.exceptionAddr.valid, atomicsUnit.io.exceptionAddr.bits, lsroq.io.exceptionAddr.vaddr)

}
