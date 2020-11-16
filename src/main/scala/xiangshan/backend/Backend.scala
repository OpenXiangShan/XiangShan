package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.decode.{DecodeBuffer, DecodeStage}
import xiangshan.backend.rename.Rename
import xiangshan.backend.brq.Brq
import xiangshan.backend.dispatch.Dispatch
import xiangshan.backend.exu._
import xiangshan.backend.issue.ReservationStationNew
import xiangshan.backend.regfile.{Regfile, RfWritePort}
import xiangshan.backend.roq.Roq
import xiangshan.mem._
import utils.ParallelOR
import xiangshan.backend.fu.FunctionUnit.{lduCfg, mouCfg, stuCfg}

/** Backend Pipeline:
  * Decode -> Rename -> Dispatch-1 -> Dispatch-2 -> Issue -> Exe
  */
class Backend extends XSModule
  with NeedImpl {
  val io = IO(new Bundle {
    val frontend = Flipped(new FrontendToBackendIO)
    val mem = Flipped(new MemToBackendIO)
    val externalInterrupt = new ExternalInterruptIO
    val sfence = Output(new SfenceBundle)
    val fencei = Output(Bool())
    val tlbCsrIO = Output(new TlbCsrBundle)
  })


  val aluExeUnits = Array.tabulate(exuParameters.AluCnt)(_ => Module(new AluExeUnit))
  val jmpExeUnit = Module(new JumpExeUnit)
  val mduExeUnits = Array.tabulate(exuParameters.MduCnt)(_ => Module(new MulDivExeUnit))
  val fmacExeUnits = Array.tabulate(exuParameters.FmacCnt)(_ => Module(new FmacExeUnit))
  val fmiscExeUnits = Array.tabulate(exuParameters.FmiscCnt)(_ => Module(new FmiscExeUnit))
  val exeUnits = jmpExeUnit +: (aluExeUnits ++ mduExeUnits ++ fmacExeUnits ++ fmiscExeUnits)

  fmacExeUnits.foreach(_.frm := jmpExeUnit.frm)
  fmiscExeUnits.foreach(_.frm := jmpExeUnit.frm)

  val wbIntExus = exeUnits.filter(e => e.config.writeIntRf)
  val wbFpExus = exeUnits.filter(e => e.config.writeFpRf)
  // wb int exu + wb fp exu + ldu / stu + brq
  val wbSize = wbIntExus.length + wbFpExus.length + exuParameters.LduCnt + exuParameters.StuCnt + 1

  val ldExeUnitCfg = ExuConfig("LoadExu", Seq(lduCfg), wbIntPriority = 0, wbFpPriority = 0)
  val stExeUnitCfg = ExuConfig("StoreExu", Seq(stuCfg, mouCfg), wbIntPriority = Int.MaxValue, wbFpPriority = Int.MaxValue)

  val ldIntOut = io.mem.ldout.map(x => {
    val raw = WireInit(x)
    raw.valid := x.valid && x.bits.uop.ctrl.rfWen
    raw
  })

  val ldFpOut = io.mem.ldout.map(x => {
    val raw = WireInit(x)
    raw.valid := x.valid && x.bits.uop.ctrl.fpWen
    raw
  })


  val decode = Module(new DecodeStage)
  val brq = Module(new Brq)
  val decBuf = Module(new DecodeBuffer)
  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch(
    jmpExeUnit.config, aluExeUnits(0).config, mduExeUnits(0).config,
    fmacExeUnits(0).config, fmiscExeUnits(0).config,
    ldExeUnitCfg, stExeUnitCfg
  ))
  val roq = Module(new Roq(wbSize))
  val intRf = Module(new Regfile(
    numReadPorts = NRIntReadPorts,
    numWirtePorts = NRIntWritePorts,
    hasZero = true,
    len = XLEN
  ))
  val fpRf = Module(new Regfile(
    numReadPorts = NRFpReadPorts,
    numWirtePorts = NRFpWritePorts,
    hasZero = false,
    len = XLEN + 1
  ))

  // backend redirect, flush pipeline
  val redirect = Mux(
    roq.io.redirect.valid,
    roq.io.redirect,
    Mux(
      brq.io.redirect.valid,
      brq.io.redirect,
      io.mem.replayAll
    )
  )

  io.frontend.redirect := redirect
  io.frontend.redirect.valid := redirect.valid && !redirect.bits.isReplay


  val ldConfigs = Seq.fill(exuParameters.LduCnt)(ldExeUnitCfg)
  val stConfigs = Seq.fill(exuParameters.StuCnt)(stExeUnitCfg)
  val memConfigs = ldConfigs ++ stConfigs

  val exuConfigs = exeUnits.map(_.config) ++ memConfigs


  def needWakeup(cfg: ExuConfig): Boolean =
    (cfg.readIntRf && cfg.writeIntRf) || (cfg.readFpRf && cfg.writeFpRf)

  def needData(a: ExuConfig, b: ExuConfig): Boolean =
    (a.readIntRf && b.writeIntRf) || (a.readFpRf && b.writeFpRf)

  val reservedStations = exuConfigs.zipWithIndex.map({ case (cfg, i) =>

    // NOTE: exu could have certern and uncertaion latency
    // but could not have multiple certern latency
    var certainLatency = -1
    if (cfg.hasCertainLatency) {
      certainLatency = cfg.latency.latencyVal.get
    }

    val readIntRf = cfg.readIntRf
    val readFpRf = cfg.readFpRf

    val writeBackIntData = wbIntExus.filter(e => e.config.hasCertainLatency && readIntRf).map(_.io.toInt.bits.data)
    val writeBackFpData = wbFpExus.filter(e => e.config.hasCertainLatency && readFpRf).map(_.io.toFp.bits.data)
    val writeBackedData = writeBackIntData ++ writeBackFpData
    val wakeupCnt = writeBackedData.length

    val extraListenInt = wbIntExus.filter(e => e.config.hasUncertainlatency && readIntRf).map(_.io.toInt)
    val extraListenFp = wbFpExus.filter(e => e.config.hasUncertainlatency && readFpRf).map(_.io.toFp)

    val extraListenPorts = extraListenInt ++ extraListenFp ++ io.mem.ldout
    val extraListenPortsCnt = extraListenPorts.length

    val feedback = (cfg == ldExeUnitCfg) || (cfg == stExeUnitCfg)

    println(s"${i}: exu:${cfg.name} wakeupCnt: ${wakeupCnt} extraListenPorts: ${extraListenPortsCnt} delay:${certainLatency} feedback:${feedback}")

    val rs = Module(new ReservationStationNew(
      cfg, wakeupCnt, extraListenPortsCnt, fixedDelay = certainLatency, feedback = feedback
    ))

    rs.io.redirect <> redirect
    rs.io.numExist <> dispatch.io.numExist(i)
    rs.io.enqCtrl <> dispatch.io.enqIQCtrl(i)
    rs.io.enqData <> dispatch.io.enqIQData(i)

    rs.io.writeBackedData <> writeBackedData
    for ((x, y) <- rs.io.extraListenPorts.zip(extraListenPorts)) {
      x.valid := y.fire()
      x.bits := y.bits
    }

    cfg match {
      case `ldExeUnitCfg` =>
      case `stExeUnitCfg` =>
      case otherCfg =>
        if (cfg.readIntRf) {
          exeUnits(i).io.fromInt <> rs.io.deq
        } else {
          exeUnits(i).io.fromFp <> rs.io.deq
        }
        exeUnits(i).io.redirect <> redirect
        rs.io.tlbFeedback := DontCare
    }

    rs.suggestName(s"rs_${cfg.name}")

    rs
  })

  for (rs <- reservedStations) {
    val wbIntUops = reservedStations.filter(x =>
      x.exuCfg.hasCertainLatency && x.exuCfg.writeIntRf && rs.exuCfg.readIntRf
    ).map(x => {
      val raw = WireInit(x.io.selectedUop)
      raw.valid := x.io.selectedUop.valid && raw.bits.ctrl.rfWen
      raw
    })
    val wbFpUops = reservedStations.filter(x =>
      x.exuCfg.hasCertainLatency && x.exuCfg.writeFpRf && rs.exuCfg.readFpRf
    ).map(x => {
      val raw = WireInit(x.io.selectedUop)
      raw.valid := x.io.selectedUop.valid && raw.bits.ctrl.fpWen
      raw
    })

    rs.io.broadcastedUops <> wbIntUops ++ wbFpUops
  }

  io.mem.commits <> roq.io.commits
  io.mem.roqDeqPtr := roq.io.roqDeqPtr

  io.mem.ldin <> reservedStations.filter(_.exuCfg == ldExeUnitCfg).map(_.io.deq)
  io.mem.stin <> reservedStations.filter(_.exuCfg == stExeUnitCfg).map(_.io.deq)
  jmpExeUnit.csrOnly.exception.valid := roq.io.redirect.valid && roq.io.redirect.bits.isException
  jmpExeUnit.csrOnly.exception.bits := roq.io.exception
  jmpExeUnit.fflags := roq.io.csr.fflags
  jmpExeUnit.dirty_fs := roq.io.csr.dirty_fs
  jmpExeUnit.csrOnly.externalInterrupt := io.externalInterrupt
  jmpExeUnit.csrOnly.memExceptionVAddr := io.mem.exceptionAddr.vaddr
  jmpExeUnit.csrOnly.isInterrupt := DontCare // TODO: fix this
  jmpExeUnit.fenceToSbuffer <> io.mem.fenceToSbuffer
  io.mem.sfence <> jmpExeUnit.sfence
  io.mem.csr <> jmpExeUnit.tlbCsrIO

  io.mem.exceptionAddr.lsIdx.lsroqIdx := roq.io.exception.lsroqIdx
  io.mem.exceptionAddr.lsIdx.lqIdx := roq.io.exception.lqIdx
  io.mem.exceptionAddr.lsIdx.sqIdx := roq.io.exception.sqIdx
  io.mem.exceptionAddr.isStore := CommitType.lsInstIsStore(roq.io.exception.ctrl.commitType)

  io.mem.tlbFeedback <> reservedStations.filter(
    x => x.exuCfg == ldExeUnitCfg || x.exuCfg == stExeUnitCfg
  ).map(_.io.tlbFeedback)

  io.frontend.outOfOrderBrInfo <> brq.io.outOfOrderBrInfo
  io.frontend.inOrderBrInfo <> brq.io.inOrderBrInfo
  io.frontend.sfence <> jmpExeUnit.sfence
  io.frontend.tlbCsrIO <> jmpExeUnit.tlbCsrIO

  io.fencei := jmpExeUnit.fencei
  io.tlbCsrIO := jmpExeUnit.tlbCsrIO

  decode.io.in <> io.frontend.cfVec
  brq.io.roqRedirect <> roq.io.redirect
  brq.io.memRedirect <> io.mem.replayAll
  brq.io.bcommit := roq.io.bcommit
  brq.io.enqReqs <> decode.io.toBrq
  for ((x, y) <- brq.io.exuRedirect.zip(exeUnits.filter(_.config.hasRedirect))) {
    x.bits := y.io.toInt.bits
    x.valid := y.io.toInt.fire() && y.io.toInt.bits.redirectValid
  }
  decode.io.brTags <> brq.io.brTags
  decBuf.io.isWalking := ParallelOR(roq.io.commits.map(c => c.valid && c.bits.isWalk)) // TODO: opt this
  decBuf.io.redirect <> redirect
  decBuf.io.in <> decode.io.out

  rename.io.redirect <> redirect
  rename.io.roqCommits <> roq.io.commits
  rename.io.in <> decBuf.io.out
  rename.io.intRfReadAddr <> dispatch.io.readIntRf.map(_.addr)
  rename.io.intPregRdy <> dispatch.io.intPregRdy
  rename.io.fpRfReadAddr <> dispatch.io.readFpRf.map(_.addr)
  rename.io.fpPregRdy <> dispatch.io.fpPregRdy
  rename.io.replayPregReq <> dispatch.io.replayPregReq
  dispatch.io.redirect <> redirect
  dispatch.io.fromRename <> rename.io.out

  roq.io.memRedirect <> io.mem.replayAll
  roq.io.brqRedirect <> brq.io.redirect
  roq.io.dp1Req <> dispatch.io.toRoq
  roq.io.csr.intrBitSet := jmpExeUnit.csrOnly.interrupt
  roq.io.csr.trapTarget := jmpExeUnit.csrOnly.trapTarget
  dispatch.io.roqIdxs <> roq.io.roqIdxs
  io.mem.dp1Req <> dispatch.io.toLsroq
  dispatch.io.lsIdxs <> io.mem.lsIdxs
  dispatch.io.dequeueRoqIndex.valid := roq.io.commitRoqIndex.valid || io.mem.oldestStore.valid
  // store writeback must be after commit roqIdx
  dispatch.io.dequeueRoqIndex.bits := Mux(io.mem.oldestStore.valid, io.mem.oldestStore.bits, roq.io.commitRoqIndex.bits)


  intRf.io.readPorts <> dispatch.io.readIntRf
  fpRf.io.readPorts <> dispatch.io.readFpRf

  io.mem.redirect <> redirect


  val wbIntArbiter = Module(new Wb(
    wbIntExus.map(_.config.wbIntPriority) ++ ldConfigs.map(_.wbIntPriority),
    NRIntWritePorts,
    wen = (e: ExuOutput)=>e.uop.ctrl.rfWen
  ))

  val wbFpArbiter = Module(new Wb(
    wbFpExus.map(_.config.wbFpPriority) ++ ldConfigs.map(_.wbFpPriority),
    NRFpWritePorts,
    wen = (e: ExuOutput) => e.uop.ctrl.fpWen
  ))

  wbIntArbiter.io.in <> wbIntExus.map(_.io.toInt) ++ ldIntOut
  wbFpArbiter.io.in <> wbFpExus.map(_.io.toFp) ++ ldFpOut

  def exuOutToRfWrite(x: Valid[ExuOutput]): RfWritePort = {
    val rfWrite = Wire(new RfWritePort)
    rfWrite.wen := x.valid
    rfWrite.addr := x.bits.uop.pdest
    rfWrite.data := x.bits.data
    rfWrite
  }

  intRf.io.writePorts <> wbIntArbiter.io.out.map(exuOutToRfWrite)
  fpRf.io.writePorts <> wbFpArbiter.io.out.map(exuOutToRfWrite)

  rename.io.wbIntResults <> wbIntArbiter.io.out
  rename.io.wbFpResults <> wbFpArbiter.io.out

  io.mem.stout.foreach(_.ready := true.B)
  io.mem.ldout.foreach(_.ready := true.B)
  roq.io.exeWbResults.take(wbSize - 1).zip(
    wbIntExus.map(e => {
      val toInt = WireInit(e.io.toInt)
      if(e.config.hasRedirect){
        toInt.valid := e.io.toInt.valid && !toInt.bits.redirectValid
      }
      toInt
    }) ++ wbFpExus.map(_.io.toFp) ++ io.mem.ldout ++ io.mem.stout
  ).foreach {
    case (x, y) =>
      x.bits := y.bits
      x.valid := y.fire()
  }
  roq.io.exeWbResults.last := brq.io.out

  val debugIntReg, debugFpReg = WireInit(VecInit(Seq.fill(32)(0.U(XLEN.W))))
  ExcitingUtils.addSink(debugIntReg, "DEBUG_INT_ARCH_REG", ExcitingUtils.Debug)
  ExcitingUtils.addSink(debugFpReg, "DEBUG_FP_ARCH_REG", ExcitingUtils.Debug)
  val debugArchReg = WireInit(VecInit(debugIntReg ++ debugFpReg))
  if (!env.FPGAPlatform) {
    ExcitingUtils.addSource(debugArchReg, "difftestRegs", ExcitingUtils.Debug)
  }

}
