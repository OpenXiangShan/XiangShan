package xiangshan.backend

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.HasFPUParameters
import xiangshan._
import xiangshan.backend.exu.Exu.{loadExuConfigs, storeExuConfigs}
import xiangshan.backend.roq.RoqPtr
import xiangshan.backend.exu._
import xiangshan.cache._
import xiangshan.mem._
import xiangshan.backend.fu.{HasExceptionNO, FenceToSbuffer}
import xiangshan.backend.issue.{ReservationStationCtrl, ReservationStationData}
import xiangshan.backend.fu.FunctionUnit.{lduCfg, mouCfg, stuCfg}

class LsBlockToCtrlIO extends XSBundle {
  val stOut = Vec(exuParameters.StuCnt, ValidIO(new ExuOutput)) // write to roq
  val numExist = Vec(exuParameters.LsExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
  val replay = ValidIO(new Redirect)
}

class MemBlockToCacheIO extends XSBundle {
  val loadUnitToDcacheVec = Vec(exuParameters.LduCnt, new DCacheLoadIO)
  val loadMiss = new DCacheLineIO
  val atomics  = new DCacheWordIO
  val sbufferToDcache = new DCacheLineIO
  val uncache = new DCacheWordIO
}

class MemBlock(
  fastWakeUpIn: Seq[ExuConfig],
  slowWakeUpIn: Seq[ExuConfig],
  fastFpOut: Seq[ExuConfig],
  slowFpOut: Seq[ExuConfig],
  fastIntOut: Seq[ExuConfig],
  slowIntOut: Seq[ExuConfig]
)(implicit p: Parameters) extends LazyModule {

  val dcache = LazyModule(new DCache())
  val uncache = LazyModule(new Uncache())

  lazy val module = new MemBlockImp(fastWakeUpIn, slowWakeUpIn, fastFpOut, slowFpOut, fastIntOut, slowIntOut)(this)
}



class MemBlockImp
(
  fastWakeUpIn: Seq[ExuConfig],
  slowWakeUpIn: Seq[ExuConfig],
  fastFpOut: Seq[ExuConfig],
  slowFpOut: Seq[ExuConfig],
  fastIntOut: Seq[ExuConfig],
  slowIntOut: Seq[ExuConfig]
) (outer: MemBlock) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasExceptionNO
  with HasXSLog
  with HasFPUParameters
  with HasExeBlockHelper
{

  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToLsBlockIO)
    val toCtrlBlock = new LsBlockToCtrlIO

    val wakeUpIn = new WakeUpBundle(fastWakeUpIn.size, slowWakeUpIn.size)
    val wakeUpFpOut = Flipped(new WakeUpBundle(fastFpOut.size, slowFpOut.size))
    val wakeUpIntOut = Flipped(new WakeUpBundle(fastIntOut.size, slowIntOut.size))

    val ptw = new TlbPtwIO
    val sfence = Input(new SfenceBundle)
    val tlbCsr = Input(new TlbCsrBundle)
    val fenceToSbuffer = Flipped(new FenceToSbuffer)

    val lsqio = new Bundle {
      val exceptionAddr = new ExceptionAddrIO // to csr
      val commits = Flipped(new RoqCommitIO) // to lsq
      val roqDeqPtr = Input(new RoqPtr) // to lsq
    }
  })

  val cache = Wire(new MemBlockToCacheIO)

  val dcache = outer.dcache.module
  val uncache = outer.uncache.module

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

  val intExeWbReqs = ldOut0 +: loadUnits.tail.map(_.io.ldout)
  val fpExeWbReqs = loadUnits.map(_.io.fpout)

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

    val inBlockListenPorts = intExeWbReqs ++ fpExeWbReqs
    val extraListenPorts = inBlockListenPorts ++
      slowWakeUpIn.zip(io.wakeUpIn.slow)
        .filter(x => (x._1.writeIntRf && readIntRf) || (x._1.writeFpRf && readFpRf))
        .map(_._2)

    val extraListenPortsCnt = extraListenPorts.length

    // if tlb miss, replay
    val feedback = true

    println(s"${i}: exu:${cfg.name} wakeupCnt: ${wakeupCnt} extraListenPorts: ${extraListenPortsCnt} delay:${certainLatency} feedback:${feedback}")

    val rsCtrl = Module(new ReservationStationCtrl(cfg, wakeupCnt, extraListenPortsCnt, fixedDelay = certainLatency, feedback = feedback))
    val rsData = Module(new ReservationStationData(cfg, wakeupCnt, extraListenPortsCnt, fixedDelay = certainLatency, feedback = feedback))

    rsCtrl.io.data <> rsData.io.ctrl
    rsCtrl.io.redirect <> redirect // TODO: remove it
    rsCtrl.io.numExist <> io.toCtrlBlock.numExist(i)
    rsCtrl.io.enqCtrl <> io.fromCtrlBlock.enqIqCtrl(i)
    rsData.io.enqData <> io.fromCtrlBlock.enqIqData(i)
    rsData.io.redirect <> redirect

    rsData.io.writeBackedData <> writeBackData
    for ((x, y) <- rsData.io.extraListenPorts.zip(extraListenPorts)) {
      x.valid := y.fire()
      x.bits := y.bits
    }

    // exeUnits(i).io.redirect <> redirect
    // exeUnits(i).io.fromInt <> rsData.io.deq
    rsData.io.feedback := DontCare

    rsCtrl.suggestName(s"rsc_${cfg.name}")
    rsData.suggestName(s"rsd_${cfg.name}")

    rsData
  })

  for(rs <- reservationStations){
    rs.io.broadcastedUops <> fastWakeUpIn.zip(io.wakeUpIn.fastUops)
      .filter(x => (x._1.writeIntRf && rs.exuCfg.readIntRf) || (x._1.writeFpRf && rs.exuCfg.readFpRf))
      .map(_._2)
  }

  // TODO: make this better
  io.wakeUpIn.fast.foreach(_.ready := true.B)
  io.wakeUpIn.slow.foreach(_.ready := true.B)

  io.wakeUpFpOut.slow <> fpExeWbReqs
  io.wakeUpIntOut.slow <> intExeWbReqs

  // load always ready
  fpExeWbReqs.foreach(_.ready := true.B)
  intExeWbReqs.foreach(_.ready := true.B)

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
    loadUnits(i).io.tlbFeedback   <> reservationStations(i).io.feedback
    loadUnits(i).io.dtlb          <> dtlb.io.requestor(i)
    // get input form dispatch
    loadUnits(i).io.ldin          <> reservationStations(i).io.deq
    // dcache access
    loadUnits(i).io.dcache        <> cache.loadUnitToDcacheVec(i)
    // forward
    loadUnits(i).io.lsq.forward   <> lsq.io.forward(i)
    loadUnits(i).io.sbuffer       <> sbuffer.io.forward(i)

    // passdown to lsq
    lsq.io.loadIn(i)              <> loadUnits(i).io.lsq.loadIn
    lsq.io.ldout(i)               <> loadUnits(i).io.lsq.ldout
  }

  // StoreUnit
  for (i <- 0 until exuParameters.StuCnt) {
	val stu = storeUnits(i)
	val rs = reservationStations(exuParameters.LduCnt + i)
	val dtlbReq = dtlb.io.requestor(exuParameters.LduCnt + i)

	stu.io.redirect <> io.fromCtrlBlock.redirect
	stu.io.tlbFeedback <> rs.io.feedback
	stu.io.dtlb <> dtlbReq

	// get input from dispatch
	stu.io.stin <> rs.io.deq

	// passdown to lsq
	stu.io.lsq <> lsq.io.storeIn(i)
	io.toCtrlBlock.stOut(i).valid := stu.io.stout.valid
	io.toCtrlBlock.stOut(i).bits := stu.io.stout.bits
	stu.io.stout.ready := true.B
  }

  // mmio store writeback will use store writeback port 0
  lsq.io.mmioStout.ready := false.B
  when(lsq.io.mmioStout.valid && !storeUnits(0).io.stout.valid) {
    io.toCtrlBlock.stOut(0).valid := true.B
    lsq.io.mmioStout.ready := true.B
    io.toCtrlBlock.stOut(0).bits  := lsq.io.mmioStout.bits
  }

  // Lsq
  lsq.io.commits     <> io.lsqio.commits
  lsq.io.enq         <> io.fromCtrlBlock.enqLsq
  lsq.io.brqRedirect := io.fromCtrlBlock.redirect
  lsq.io.roqDeqPtr   := io.lsqio.roqDeqPtr
  io.toCtrlBlock.replay <> lsq.io.rollback
  lsq.io.dcache      <> cache.loadMiss
  lsq.io.uncache     <> cache.uncache

  // LSQ to store buffer
  lsq.io.sbuffer     <> sbuffer.io.in

  // Sbuffer
  sbuffer.io.dcache    <> cache.sbufferToDcache

  // flush sbuffer
  val fenceFlush = io.fenceToSbuffer.flushSb
  val atomicsFlush = atomicsUnit.io.flush_sbuffer.valid
  io.fenceToSbuffer.sbIsEmpty := sbuffer.io.flush.empty
  // if both of them tries to flush sbuffer at the same time
  // something must have gone wrong
  assert(!(fenceFlush && atomicsFlush))
  sbuffer.io.flush.valid := fenceFlush || atomicsFlush

  // AtomicsUnit: AtomicsUnit will override other control signials,
  // as atomics insts (LR/SC/AMO) will block the pipeline
  val s_normal :: s_atomics_0 :: s_atomics_1 :: Nil = Enum(3)
  val state = RegInit(s_normal)

  val atomic_rs0 = exuParameters.LduCnt + 0
  val atomic_rs1 = exuParameters.LduCnt + 1
  val st0_atomics = reservationStations(atomic_rs0).io.deq.valid && reservationStations(atomic_rs0).io.deq.bits.uop.ctrl.fuType === FuType.mou
  val st1_atomics = reservationStations(atomic_rs1).io.deq.valid && reservationStations(atomic_rs1).io.deq.bits.uop.ctrl.fuType === FuType.mou

  when (st0_atomics) {
    reservationStations(atomic_rs0).io.deq.ready := atomicsUnit.io.in.ready
    storeUnits(0).io.stin.valid := false.B

    state := s_atomics_0
    assert(!st1_atomics)
  }
  when (st1_atomics) {
    reservationStations(atomic_rs1).io.deq.ready := atomicsUnit.io.in.ready
    storeUnits(1).io.stin.valid := false.B

    state := s_atomics_1
    assert(!st0_atomics)
  }
  when (atomicsUnit.io.out.valid) {
    assert(state === s_atomics_0 || state === s_atomics_1)
    state := s_normal
  }

  atomicsUnit.io.in.valid := st0_atomics || st1_atomics
  atomicsUnit.io.in.bits  := Mux(st0_atomics, reservationStations(atomic_rs0).io.deq.bits, reservationStations(atomic_rs1).io.deq.bits)
  atomicsUnit.io.redirect <> io.fromCtrlBlock.redirect

  atomicsUnit.io.dtlb.resp.valid := false.B
  atomicsUnit.io.dtlb.resp.bits  := DontCare
  atomicsUnit.io.dtlb.req.ready := dtlb.io.requestor(0).req.ready

  atomicsUnit.io.dcache        <> cache.atomics
  atomicsUnit.io.flush_sbuffer.empty := sbuffer.io.flush.empty

  // for atomicsUnit, it uses loadUnit(0)'s TLB port
  when (state === s_atomics_0 || state === s_atomics_1) {
    atomicsUnit.io.dtlb <> dtlb.io.requestor(0)

    loadUnits(0).io.dtlb.resp.valid := false.B
    loadUnits(0).io.ldout.ready := false.B

    // make sure there's no in-flight uops in load unit
    assert(!loadUnits(0).io.dtlb.req.valid)
    assert(!loadUnits(0).io.ldout.valid)
  }

  when (state === s_atomics_0) {
    atomicsUnit.io.tlbFeedback <> reservationStations(atomic_rs0).io.feedback

    assert(!storeUnits(0).io.tlbFeedback.valid)
  }
  when (state === s_atomics_1) {
    atomicsUnit.io.tlbFeedback <> reservationStations(atomic_rs1).io.feedback

    assert(!storeUnits(1).io.tlbFeedback.valid)
  }

  lsq.io.exceptionAddr.lsIdx := io.lsqio.exceptionAddr.lsIdx
  lsq.io.exceptionAddr.isStore := io.lsqio.exceptionAddr.isStore
  io.lsqio.exceptionAddr.vaddr := Mux(atomicsUnit.io.exceptionAddr.valid, atomicsUnit.io.exceptionAddr.bits, lsq.io.exceptionAddr.vaddr)

  // connect to cache
  dcache.io.lsu.load    <> cache.loadUnitToDcacheVec
  dcache.io.lsu.lsq     <> cache.loadMiss
  dcache.io.lsu.atomics <> cache.atomics
  dcache.io.lsu.store   <> cache.sbufferToDcache
  uncache.io.lsq        <> cache.uncache
}
