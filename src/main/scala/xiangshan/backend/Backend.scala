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


  val aluExeUnits =Array.tabulate(exuParameters.AluCnt)(_ => Module(new AluExeUnit))
  val jmpExeUnit = Module(new JmpExeUnit)
  val mulExeUnits = Array.tabulate(exuParameters.MulCnt)(_ => Module(new MulExeUnit))
  val mduExeUnits = Array.tabulate(exuParameters.MduCnt)(_ => Module(new MulDivExeUnit))
  val fmacExeUnits = Array.tabulate(exuParameters.FmacCnt)(_ => Module(new FmacExeUnit))
  val fmiscExeUnits = Array.tabulate(exuParameters.FmiscCnt)(_ => Module(new FmiscExeUnit))
  // val fmiscDivSqrtExeUnits = Array.tabulate(exuParameters.FmiscDivSqrtCnt)(_ => Module(new FmiscDivSqrtExeUnit))
  val exeUnits = jmpExeUnit +: (aluExeUnits ++ mulExeUnits ++ mduExeUnits ++ fmacExeUnits ++ fmiscExeUnits)
  exeUnits.foreach(_.io.csrOnly := DontCare)
  exeUnits.foreach(_.io.mcommit := DontCare)

  fmacExeUnits.foreach(_.frm := jmpExeUnit.frm)
  fmiscExeUnits.foreach(_.frm := jmpExeUnit.frm)

  val decode = Module(new DecodeStage)
  val brq = Module(new Brq)
  val decBuf = Module(new DecodeBuffer)
  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch)
  val roq = Module(new Roq)
  val intRf = Module(new Regfile(
    numReadPorts = NRIntReadPorts,
    numWirtePorts = NRIntWritePorts,
    hasZero = true
  ))
  val fpRf = Module(new Regfile(
    numReadPorts = NRFpReadPorts,
    numWirtePorts = NRFpWritePorts,
    hasZero = false
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

  val memConfigs =
    Seq.fill(exuParameters.LduCnt)(Exu.ldExeUnitCfg) ++
    Seq.fill(exuParameters.StuCnt)(Exu.stExeUnitCfg)

  val exuConfigs = exeUnits.map(_.config) ++ memConfigs

  val exeWbReqs = exeUnits.map(_.io.out) ++ io.mem.ldout ++ io.mem.stout

  def needWakeup(cfg: ExuConfig): Boolean =
    (cfg.readIntRf && cfg.writeIntRf) || (cfg.readFpRf && cfg.writeFpRf)

  def needData(a: ExuConfig, b: ExuConfig): Boolean =
    (a.readIntRf && b.writeIntRf) || (a.readFpRf && b.writeFpRf)

  val reservedStations  = exuConfigs.zipWithIndex.map({ case (cfg, i) =>

    // NOTE: exu could have certern and uncertaion latency
    // but could not have multiple certern latency
    var certainLatency = -1
    if(cfg.hasCertainLatency) { certainLatency = cfg.latency.latencyVal.get }

    val writeBackedData = exuConfigs.zip(exeWbReqs).filter(x => x._1.hasCertainLatency && needData(cfg, x._1)).map(_._2.bits.data)
    val wakeupCnt = writeBackedData.length

    val extraListenPorts = exuConfigs
      .zip(exeWbReqs)
      .filter(x => x._1.hasUncertainlatency && needData(cfg, x._1))
      .map(_._2)
    val extraListenPortsCnt = extraListenPorts.length

    val feedback = (cfg == Exu.ldExeUnitCfg) || (cfg == Exu.stExeUnitCfg)
    
    println(s"${i}: exu:${cfg.name} wakeupCnt: ${wakeupCnt} extraListenPorts: ${extraListenPortsCnt} delay:${certainLatency} feedback:${feedback}")
  
    val rs = Module(new ReservationStationNew(cfg, wakeupCnt, extraListenPortsCnt, fixedDelay = certainLatency, feedback = feedback))

    rs.io.redirect <> redirect
    rs.io.numExist <> dispatch.io.numExist(i)
    rs.io.enqCtrl <> dispatch.io.enqIQCtrl(i)
    rs.io.enqData <> dispatch.io.enqIQData(i)

    rs.io.writeBackedData <> writeBackedData
    for((x, y) <- rs.io.extraListenPorts.zip(extraListenPorts)){
      x.valid := y.fire()
      x.bits := y.bits
    }

    cfg match {
      case Exu.ldExeUnitCfg =>
      case Exu.stExeUnitCfg =>
      case otherCfg =>
        exeUnits(i).io.in <> rs.io.deq
        exeUnits(i).io.redirect <> redirect
        rs.io.tlbFeedback := DontCare
    }

    rs
  })

  for(rs <- reservedStations){
    rs.io.broadcastedUops <> reservedStations.
      filter(x => x.exuCfg.hasCertainLatency && needData(rs.exuCfg, x.exuCfg)).
      map(_.io.selectedUop)
  }

  io.mem.commits <> roq.io.commits
  io.mem.roqDeqPtr := roq.io.roqDeqPtr

  io.mem.ldin <> reservedStations.filter(_.exuCfg == Exu.ldExeUnitCfg).map(_.io.deq)
  io.mem.stin <> reservedStations.filter(_.exuCfg == Exu.stExeUnitCfg).map(_.io.deq)
  jmpExeUnit.io.csrOnly.exception.valid := roq.io.redirect.valid && roq.io.redirect.bits.isException
  jmpExeUnit.io.csrOnly.exception.bits := roq.io.exception
  jmpExeUnit.fflags := roq.io.fflags
  jmpExeUnit.dirty_fs := roq.io.dirty_fs
  jmpExeUnit.io.csrOnly.externalInterrupt := io.externalInterrupt
  jmpExeUnit.io.csrOnly.memExceptionVAddr := io.mem.exceptionAddr.vaddr
  jmpExeUnit.fenceToSbuffer <> io.mem.fenceToSbuffer
  io.mem.sfence <> jmpExeUnit.sfence
  io.mem.csr <> jmpExeUnit.tlbCsrIO

  io.mem.exceptionAddr.lsIdx.lsroqIdx := roq.io.exception.lsroqIdx
  io.mem.exceptionAddr.lsIdx.lqIdx := roq.io.exception.lqIdx
  io.mem.exceptionAddr.lsIdx.sqIdx := roq.io.exception.sqIdx
  io.mem.exceptionAddr.isStore := CommitType.lsInstIsStore(roq.io.exception.ctrl.commitType)

  io.mem.tlbFeedback <> reservedStations.filter(
	x => x.exuCfg == Exu.ldExeUnitCfg || x.exuCfg == Exu.stExeUnitCfg
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
    x.bits := y.io.out.bits
    x.valid := y.io.out.fire() && y.io.out.bits.redirectValid
  }
  decode.io.brTags <> brq.io.brTags
  decBuf.io.isWalking := ParallelOR(roq.io.commits.map(c => c.valid && c.bits.isWalk)) // TODO: opt this
  decBuf.io.redirect <> redirect
  decBuf.io.in <> decode.io.out

  rename.io.redirect <> redirect
  rename.io.roqCommits <> roq.io.commits
  rename.io.in <> decBuf.io.out
  rename.io.intRfReadAddr <> dispatch.io.readIntRf.map(_.addr) ++ dispatch.io.memIntRf.map(_.addr)
  rename.io.intPregRdy <> dispatch.io.intPregRdy ++ dispatch.io.intMemRegRdy
  rename.io.fpRfReadAddr <> dispatch.io.readFpRf.map(_.addr) ++ dispatch.io.memFpRf.map(_.addr)
  rename.io.fpPregRdy <> dispatch.io.fpPregRdy ++ dispatch.io.fpMemRegRdy
  rename.io.replayPregReq <> dispatch.io.replayPregReq
  dispatch.io.redirect <> redirect
  dispatch.io.fromRename <> rename.io.out

  roq.io.memRedirect <> io.mem.replayAll
  roq.io.brqRedirect <> brq.io.redirect
  roq.io.dp1Req <> dispatch.io.toRoq
  roq.io.intrBitSet := jmpExeUnit.io.csrOnly.interrupt
  roq.io.trapTarget := jmpExeUnit.io.csrOnly.trapTarget
  dispatch.io.roqIdxs <> roq.io.roqIdxs
  io.mem.dp1Req <> dispatch.io.toLsroq
  dispatch.io.lsIdxs <> io.mem.lsIdxs
  dispatch.io.dequeueRoqIndex.valid := roq.io.commitRoqIndex.valid || io.mem.oldestStore.valid
  // store writeback must be after commit roqIdx
  dispatch.io.dequeueRoqIndex.bits := Mux(io.mem.oldestStore.valid, io.mem.oldestStore.bits, roq.io.commitRoqIndex.bits)


  intRf.io.readPorts <> dispatch.io.readIntRf ++ dispatch.io.memIntRf
  fpRf.io.readPorts <> dispatch.io.readFpRf ++ dispatch.io.memFpRf

  io.mem.redirect <> redirect

  val wbu = Module(new Wbu(exuConfigs))
  wbu.io.in <> exeWbReqs

  val wbIntResults = wbu.io.toIntRf
  val wbFpResults = wbu.io.toFpRf

  def exuOutToRfWrite(x: Valid[ExuOutput]): RfWritePort = {
    val rfWrite = Wire(new RfWritePort)
    rfWrite.wen := x.valid
    rfWrite.addr := x.bits.uop.pdest
    rfWrite.data := x.bits.data
    rfWrite
  }
  intRf.io.writePorts <> wbIntResults.map(exuOutToRfWrite)
  fpRf.io.writePorts <> wbFpResults.map(exuOutToRfWrite)

  rename.io.wbIntResults <> wbIntResults
  rename.io.wbFpResults <> wbFpResults

  roq.io.exeWbResults.take(exeWbReqs.length).zip(wbu.io.toRoq).foreach(x => x._1 := x._2)
  roq.io.exeWbResults.last := brq.io.out


  val debugIntReg, debugFpReg = WireInit(VecInit(Seq.fill(32)(0.U(XLEN.W))))
  ExcitingUtils.addSink(debugIntReg, "DEBUG_INT_ARCH_REG", ExcitingUtils.Debug)
  ExcitingUtils.addSink(debugFpReg, "DEBUG_FP_ARCH_REG", ExcitingUtils.Debug)
  val debugArchReg = WireInit(VecInit(debugIntReg ++ debugFpReg))
  if (!env.FPGAPlatform) {
    ExcitingUtils.addSource(debugArchReg, "difftestRegs", ExcitingUtils.Debug)
  }

}
