package xiangshan.backend

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.HasFPUParameters
import xiangshan._
import xiangshan.backend.exu.Exu.{loadExuConfigs, storeExuConfigs}
import xiangshan.backend.roq.{RoqPtr, RoqLsqIO}
import xiangshan.backend.exu._
import xiangshan.cache._
import xiangshan.mem._
import xiangshan.backend.fu.{HasExceptionNO, FenceToSbuffer}
import xiangshan.backend.issue.{ReservationStationCtrl, ReservationStationData}
import xiangshan.backend.regfile.RfReadPort

class LsBlockToCtrlIO extends XSBundle {
  val stOut = Vec(exuParameters.StuCnt, ValidIO(new ExuOutput)) // write to roq
  val numExist = Vec(exuParameters.LsExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
  val replay = ValidIO(new Redirect)
}

class IntBlockToMemBlockIO extends XSBundle {
  val readIntRf = Vec(NRMemReadPorts, new RfReadPort(XLEN))
}

class FpBlockToMemBlockIO extends XSBundle {
  val readFpRf = Vec(exuParameters.StuCnt, new RfReadPort(XLEN + 1))
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
    val fromIntBlock = Flipped(new IntBlockToMemBlockIO)
    val fromFpBlock = Flipped(new FpBlockToMemBlockIO)
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
      val roq = Flipped(new RoqLsqIO) // roq to lsq
    }

    val toDCachePrefetch = DecoupledIO(new MissReq)
  })
  val difftestIO = IO(new Bundle() {
    val fromSbuffer = new Bundle() {
      val sbufferResp = Output(Bool())
      val sbufferAddr = Output(UInt(64.W))
      val sbufferData = Output(Vec(64, UInt(8.W)))
      val sbufferMask = Output(UInt(64.W))
    }
    val fromSQ = new Bundle() {
      val storeCommit = Output(UInt(2.W))
      val storeAddr   = Output(Vec(2, UInt(64.W)))
      val storeData   = Output(Vec(2, UInt(64.W)))
      val storeMask   = Output(Vec(2, UInt(8.W)))
    }
  })
  difftestIO <> DontCare

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

  val readPortIndex = Seq(0, 1, 2, 4)
  io.fromIntBlock.readIntRf.foreach(_.addr := DontCare)
  io.fromFpBlock.readFpRf.foreach(_.addr := DontCare)
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

    rsCtrl.io.data     <> rsData.io.ctrl
    rsCtrl.io.redirect <> redirect // TODO: remove it
    rsCtrl.io.numExist <> io.toCtrlBlock.numExist(i)
    rsCtrl.io.enqCtrl  <> io.fromCtrlBlock.enqIqCtrl(i)

    val src2IsFp = RegNext(io.fromCtrlBlock.enqIqCtrl(i).bits.ctrl.src2Type === SrcType.fp)
    rsData.io.srcRegValue := DontCare
    rsData.io.srcRegValue(0) := io.fromIntBlock.readIntRf(readPortIndex(i)).data
    if (i >= exuParameters.LduCnt) {
      rsData.io.srcRegValue(1) := Mux(src2IsFp, io.fromFpBlock.readFpRf(i - exuParameters.LduCnt).data, io.fromIntBlock.readIntRf(readPortIndex(i) + 1).data)
    }
    rsData.io.redirect <> redirect

    rsData.io.writeBackedData <> writeBackData
    for ((x, y) <- rsData.io.extraListenPorts.zip(extraListenPorts)) {
      x.valid := y.fire()
      x.bits  := y.bits
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

  io.wakeUpFpOut.slow  <> fpExeWbReqs
  io.wakeUpIntOut.slow <> intExeWbReqs

  // load always ready
  fpExeWbReqs.foreach(_.ready := true.B)
  intExeWbReqs.foreach(_.ready := true.B)

  val dtlb    = Module(new TLB(Width = DTLBWidth, isDtlb = true))
  val lsq     = Module(new LsqWrappper)
  val sbuffer = Module(new NewSbuffer)
  // if you wants to stress test dcache store, use FakeSbuffer
  // val sbuffer = Module(new FakeSbuffer)

  // dtlb
  io.ptw         <> dtlb.io.ptw
  dtlb.io.sfence <> io.sfence
  dtlb.io.csr    <> io.tlbCsr
  if (env.DualCoreDifftest) {
    difftestIO.fromSbuffer <> sbuffer.difftestIO
    difftestIO.fromSQ <> lsq.difftestIO.fromSQ
  }

  // LoadUnit
  for (i <- 0 until exuParameters.LduCnt) {
    loadUnits(i).io.redirect      <> io.fromCtrlBlock.redirect
    loadUnits(i).io.tlbFeedback   <> reservationStations(i).io.feedback
    loadUnits(i).io.dtlb          <> dtlb.io.requestor(i)
    // get input form dispatch
    loadUnits(i).io.ldin          <> reservationStations(i).io.deq
    // dcache access
    loadUnits(i).io.dcache        <> dcache.io.lsu.load(i)
    // forward
    loadUnits(i).io.lsq.forward   <> lsq.io.forward(i)
    loadUnits(i).io.sbuffer       <> sbuffer.io.forward(i)

    // passdown to lsq
    lsq.io.loadIn(i)              <> loadUnits(i).io.lsq.loadIn
    lsq.io.ldout(i)               <> loadUnits(i).io.lsq.ldout
    lsq.io.loadDataForwarded(i)   <> loadUnits(i).io.lsq.loadDataForwarded
  }

  // StoreUnit
  for (i <- 0 until exuParameters.StuCnt) {
    val stu = storeUnits(i)
    val rs = reservationStations(exuParameters.LduCnt + i)
    val dtlbReq = dtlb.io.requestor(exuParameters.LduCnt + i)

    stu.io.redirect    <> io.fromCtrlBlock.redirect
    stu.io.tlbFeedback <> rs.io.feedback
    stu.io.dtlb        <> dtlbReq
    stu.io.stin        <> rs.io.deq
    stu.io.lsq         <> lsq.io.storeIn(i)

    io.toCtrlBlock.stOut(i).valid := stu.io.stout.valid
    io.toCtrlBlock.stOut(i).bits  := stu.io.stout.bits
    stu.io.stout.ready := true.B
  }

  // mmio store writeback will use store writeback port 0
  lsq.io.mmioStout.ready := false.B
  when (lsq.io.mmioStout.valid && !storeUnits(0).io.stout.valid) {
    io.toCtrlBlock.stOut(0).valid := true.B
    lsq.io.mmioStout.ready := true.B
    io.toCtrlBlock.stOut(0).bits  := lsq.io.mmioStout.bits
  }

  // Lsq
  lsq.io.roq            <> io.lsqio.roq
  lsq.io.enq            <> io.fromCtrlBlock.enqLsq
  lsq.io.brqRedirect    <> io.fromCtrlBlock.redirect
  io.toCtrlBlock.replay <> lsq.io.rollback
  lsq.io.dcache         <> dcache.io.lsu.lsq
  lsq.io.uncache        <> uncache.io.lsq

  // LSQ to store buffer
  lsq.io.sbuffer        <> sbuffer.io.in
  lsq.io.sqempty        <> sbuffer.io.sqempty

  // Sbuffer
  sbuffer.io.dcache     <> dcache.io.lsu.store

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

  val atomic_rs0  = exuParameters.LduCnt + 0
  val atomic_rs1  = exuParameters.LduCnt + 1
  val st0_atomics = reservationStations(atomic_rs0).io.deq.valid && FuType.storeIsAMO(reservationStations(atomic_rs0).io.deq.bits.uop.ctrl.fuType)
  val st1_atomics = reservationStations(atomic_rs1).io.deq.valid && FuType.storeIsAMO(reservationStations(atomic_rs1).io.deq.bits.uop.ctrl.fuType)

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
  atomicsUnit.io.dtlb.req.ready  := dtlb.io.requestor(0).req.ready

  atomicsUnit.io.dcache <> dcache.io.lsu.atomics
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

  lsq.io.exceptionAddr.lsIdx  := io.lsqio.exceptionAddr.lsIdx
  lsq.io.exceptionAddr.isStore := io.lsqio.exceptionAddr.isStore
  io.lsqio.exceptionAddr.vaddr := Mux(atomicsUnit.io.exceptionAddr.valid, atomicsUnit.io.exceptionAddr.bits, lsq.io.exceptionAddr.vaddr)

  io.toDCachePrefetch <> dcache.io.prefetch
}
