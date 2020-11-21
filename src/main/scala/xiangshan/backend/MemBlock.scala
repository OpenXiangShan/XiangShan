package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.exu.Exu.{loadExuConfigs, storeExuConfigs}
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
  val replay = ValidIO(new Redirect)
}

class MemBlockToDcacheIO extends XSBundle {
  val loadUnitToDcacheVec = Vec(exuParameters.LduCnt, new DCacheLoadIO)
  val loadMiss = new DCacheLineIO
  val atomics  = new DCacheWordIO
  val sbufferToDcache = new DCacheLineIO
  val uncache = new DCacheWordIO
}

class MemBlock
(
  fastWakeUpIn: Seq[ExuConfig],
  slowWakeUpIn: Seq[ExuConfig],
  fastFpOut: Seq[ExuConfig],
  slowFpOut: Seq[ExuConfig],
  fastIntOut: Seq[ExuConfig],
  slowIntOut: Seq[ExuConfig]
) extends XSModule with HasExeBlockHelper {

  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToLsBlockIO)
    val toCtrlBlock = new LsBlockToCtrlIO

    val wakeUpIn = new WakeUpBundle(fastWakeUpIn.size, slowWakeUpIn.size)
    val wakeUpFpOut = Flipped(new WakeUpBundle(fastFpOut.size, slowFpOut.size))
    val wakeUpIntOut = Flipped(new WakeUpBundle(fastIntOut.size, slowIntOut.size))

    val ptw = new TlbPtwIO
    // TODO: dcache should be inside MemBlock
    val dcache = new MemBlockToDcacheIO
    val sfence = Input(new SfenceBundle)
    val tlbCsr = Input(new TlbCsrBundle)
    val fenceToSbuffer = Flipped(new FenceToSbuffer)

    val lsqio = new Bundle {
      val exceptionAddr = new ExceptionAddrIO // to csr
      val commits = Flipped(Vec(CommitWidth, Valid(new RoqCommit))) // to lsq
      val roqDeqPtr = Input(new RoqPtr) // to lsq
      val oldestStore = Output(Valid(new RoqPtr)) // to dispatch
    }
  })

  val redirect = io.fromCtrlBlock.redirect

  val loadUnits = Seq.fill(exuParameters.LduCnt)(Module(new LoadUnit))
  val storeUnits = Seq.fill(exuParameters.StuCnt)(Module(new StoreUnit))
  val exeUnits = loadUnits ++ storeUnits

  val atomicsUnit = Module(new AtomicsUnit)

  val loadWritebackOverride  = Mux(atomicsUnit.io.out.valid, atomicsUnit.io.out.bits, loadUnits.head.io.ldout.bits)
  val ldOut0 = Wire(Decoupled(new ExuOutput))
  ldOut0.valid := atomicsUnit.io.out.valid || loadUnits.head.io.ldout.valid
  ldOut0.bits  := loadWritebackOverride
  atomicsUnit.io.out.ready := ldOut0.ready
  loadUnits.head.io.ldout.ready := ldOut0.ready

  val exeWbReqs = ldOut0 +: loadUnits.tail.map(_.io.ldout)

  val reservationStations = (loadExuConfigs ++ storeExuConfigs).zipWithIndex.map({ case (cfg, i) =>
    var certainLatency = -1
    if (cfg.hasCertainLatency) {
      certainLatency = cfg.latency.latencyVal.get
    }

    val readIntRf = cfg.readIntRf
    val readFpRf = cfg.readFpRf

    // load has uncertain latency, so only use external wake up data
    val writeBackData = fastWakeUpIn.zip(io.wakeUpIn.fast)
      .filter(x => (x._1.writeIntRf && readIntRf) || (x._1.writeFpRf && readFpRf))
      .map(_._2.bits.data)
    val wakeupCnt = writeBackData.length

    val inBlockListenPorts = exeWbReqs
    val extraListenPorts = inBlockListenPorts ++
      slowWakeUpIn.zip(io.wakeUpIn.slow)
        .filter(x => (x._1.writeIntRf && readIntRf) || (x._1.writeFpRf && readFpRf))
        .map(_._2)

    val extraListenPortsCnt = extraListenPorts.length

    // if tlb miss, replay
    val feedback = true

    println(s"${i}: exu:${cfg.name} wakeupCnt: ${wakeupCnt} extraListenPorts: ${extraListenPortsCnt} delay:${certainLatency} feedback:${feedback}")

    val rs = Module(new ReservationStationNew(
      cfg, wakeupCnt, extraListenPortsCnt, fixedDelay = certainLatency, feedback = feedback
    ))

    rs.io.redirect <> redirect
    rs.io.numExist <> io.toCtrlBlock.numExist(i)
    rs.io.enqCtrl <> io.fromCtrlBlock.enqIqCtrl(i)
    rs.io.enqData <> io.fromCtrlBlock.enqIqData(i)

    rs.io.writeBackedData <> writeBackData
    for ((x, y) <- rs.io.extraListenPorts.zip(extraListenPorts)) {
      x.valid := y.fire()
      x.bits := y.bits
    }

    rs.io.tlbFeedback := DontCare

    rs.suggestName(s"rs_${cfg.name}")

    rs
  })

  for(rs <- reservationStations){
    rs.io.broadcastedUops <> fastWakeUpIn.zip(io.wakeUpIn.fastUops)
      .filter(x => (x._1.writeIntRf && rs.exuCfg.readIntRf) || (x._1.writeFpRf && rs.exuCfg.readFpRf))
      .map(_._2)
  }

  // TODO: make this better
  io.wakeUpIn.fast.foreach(_.ready := true.B)
  io.wakeUpIn.slow.foreach(_.ready := true.B)

  io.wakeUpFpOut.slow <> exeWbReqs.map(x => {
    val raw = WireInit(x)
    raw.valid := x.valid && x.bits.uop.ctrl.fpWen
    raw
  })

  io.wakeUpIntOut.slow <> exeWbReqs.map(x => {
    val raw = WireInit(x)
    raw.valid := x.valid && x.bits.uop.ctrl.rfWen
    raw
  })

  // load always ready
  exeWbReqs.foreach(_.ready := true.B)

  val dtlb = Module(new TLB(Width = DTLBWidth, isDtlb = true))
  val lsq = Module(new LsqWrappper)
  val sbuffer = Module(new NewSbuffer)
  // if you wants to stress test dcache store, use FakeSbuffer
  // val sbuffer = Module(new FakeSbuffer)

  // dtlb
  io.ptw <> dtlb.io.ptw
  dtlb.io.sfence <> io.sfence
  dtlb.io.csr <> io.tlbCsr

  // LoadUnit
  for (i <- 0 until exuParameters.LduCnt) {
    loadUnits(i).io.redirect      <> io.fromCtrlBlock.redirect
    loadUnits(i).io.tlbFeedback   <> reservationStations(i).io.tlbFeedback
    loadUnits(i).io.dtlb          <> dtlb.io.requestor(i)
    // get input form dispatch
    loadUnits(i).io.ldin          <> reservationStations(i).io.deq
    // dcache access
    loadUnits(i).io.dcache        <> io.dcache.loadUnitToDcacheVec(i)
    // forward
    loadUnits(i).io.lsq.forward   <> lsq.io.forward(i)
    loadUnits(i).io.sbuffer       <> sbuffer.io.forward(i)

    // passdown to lsq
    lsq.io.loadIn(i)              <> loadUnits(i).io.lsq.loadIn
    lsq.io.ldout(i)               <> loadUnits(i).io.lsq.ldout
  }

  // StoreUnit
  for (i <- 0 until exuParameters.StuCnt) {
    storeUnits(i).io.redirect     <> io.fromCtrlBlock.redirect
    storeUnits(i).io.tlbFeedback  <> reservationStations(exuParameters.LduCnt + i).io.tlbFeedback
    storeUnits(i).io.dtlb         <> dtlb.io.requestor(exuParameters.LduCnt + i)
    // get input form dispatch
    storeUnits(i).io.stin         <> reservationStations(exuParameters.LduCnt + i).io.deq
    // passdown to lsq
    storeUnits(i).io.lsq          <> lsq.io.storeIn(i)
    io.toCtrlBlock.stOut(i).valid := lsq.io.stout(i).valid
    io.toCtrlBlock.stOut(i).bits  := lsq.io.stout(i).bits
	lsq.io.stout(i).ready := true.B
  }

  // Lsq
  lsq.io.commits     <> io.lsqio.commits
  lsq.io.dp1Req      <> io.fromCtrlBlock.lsqIdxReq
  lsq.io.oldestStore <> io.lsqio.oldestStore
  lsq.io.lsIdxs      <> io.toCtrlBlock.lsqIdxResp
  lsq.io.brqRedirect := io.fromCtrlBlock.redirect
  lsq.io.roqDeqPtr   := io.lsqio.roqDeqPtr
  io.toCtrlBlock.replay <> lsq.io.rollback
  lsq.io.dcache      <> io.dcache.loadMiss
  lsq.io.uncache     <> io.dcache.uncache

  // LSQ to store buffer
  lsq.io.sbuffer     <> sbuffer.io.in

  // Sbuffer
  sbuffer.io.dcache    <> io.dcache.sbufferToDcache

  // flush sbuffer
  val fenceFlush = io.fenceToSbuffer.flushSb
  val atomicsFlush = atomicsUnit.io.flush_sbuffer.valid
  io.fenceToSbuffer.sbIsEmpty := sbuffer.io.flush.empty
  // if both of them tries to flush sbuffer at the same time
  // something must have gone wrong
  assert(!(fenceFlush && atomicsFlush))
  sbuffer.io.flush.valid := fenceFlush || atomicsFlush

  // TODO: make 0/1 configurable
  // AtomicsUnit
  // AtomicsUnit will override other control signials,
  // as atomics insts (LR/SC/AMO) will block the pipeline
  val st0_atomics = reservationStations(2).io.deq.valid && reservationStations(2).io.deq.bits.uop.ctrl.fuType === FuType.mou
  val st1_atomics = reservationStations(3).io.deq.valid && reservationStations(3).io.deq.bits.uop.ctrl.fuType === FuType.mou
  // amo should always go through store issue queue 0
  assert(!st1_atomics)

  atomicsUnit.io.dtlb.resp.valid := false.B
  atomicsUnit.io.dtlb.resp.bits  := DontCare

  // dispatch 0 takes priority
  atomicsUnit.io.in.valid := st0_atomics
  atomicsUnit.io.in.bits  := reservationStations(2).io.deq.bits
  when (st0_atomics) {
    reservationStations(0).io.deq.ready := atomicsUnit.io.in.ready
    storeUnits(0).io.stin.valid := false.B
  }

  when(atomicsUnit.io.dtlb.req.valid) {
    dtlb.io.requestor(0) <> atomicsUnit.io.dtlb
    // take load unit 0's tlb port
    // make sure not to disturb loadUnit
    assert(!loadUnits(0).io.dtlb.req.valid)
    loadUnits(0).io.dtlb.resp.valid := false.B
  }

  when(atomicsUnit.io.tlbFeedback.valid) {
    assert(!storeUnits(0).io.tlbFeedback.valid)
    atomicsUnit.io.tlbFeedback <> reservationStations(exuParameters.LduCnt + 0).io.tlbFeedback
  }

  atomicsUnit.io.dcache        <> io.dcache.atomics
  atomicsUnit.io.flush_sbuffer.empty := sbuffer.io.flush.empty

  atomicsUnit.io.redirect <> io.fromCtrlBlock.redirect

  when(atomicsUnit.io.out.valid){
    // take load unit 0's write back port
    assert(!loadUnits(0).io.ldout.valid)
    loadUnits(0).io.ldout.ready := false.B
  }

  lsq.io.exceptionAddr.lsIdx := io.lsqio.exceptionAddr.lsIdx
  lsq.io.exceptionAddr.isStore := io.lsqio.exceptionAddr.isStore
  io.lsqio.exceptionAddr.vaddr := Mux(atomicsUnit.io.exceptionAddr.valid, atomicsUnit.io.exceptionAddr.bits, lsq.io.exceptionAddr.vaddr)

}
