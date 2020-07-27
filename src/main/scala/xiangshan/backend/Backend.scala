package xiangshan.backend

import bus.simplebus.SimpleBusUC
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import noop.MemMMUIO
import xiangshan._
import xiangshan.backend.decode.{DecodeBuffer, DecodeStage}
import xiangshan.backend.rename.Rename
import xiangshan.backend.brq.Brq
import xiangshan.backend.dispatch.Dispatch
import xiangshan.backend.exu._
import xiangshan.backend.fu.FunctionUnit
import xiangshan.backend.issue.{IssueQueue, RegfileReader, ReservedStation}
import xiangshan.backend.regfile.{Regfile, RfWritePort}
import xiangshan.backend.roq.Roq
import xiangshan.mem._


/** Backend Pipeline:
  * Decode -> Rename -> Dispatch-1 -> Dispatch-2 -> Issue -> Exe
  */
class Backend(implicit val p: XSConfig) extends XSModule
  with NeedImpl {
  val io = IO(new Bundle {
    // val dmem = new SimpleBusUC(addrBits = VAddrBits)
    val memMMU = Flipped(new MemMMUIO)
    val frontend = Flipped(new FrontendToBackendIO)
    val mem = Flipped(new MemToBackendIO)
  })


  val aluExeUnits = Array.tabulate(exuParameters.AluCnt)(_ => Module(new AluExeUnit))
  val jmpExeUnit = Module(new JmpExeUnit)
  val mulExeUnits = Array.tabulate(exuParameters.MulCnt)(_ => Module(new MulExeUnit))
  val mduExeUnits = Array.tabulate(exuParameters.MduCnt)(_ => Module(new MulDivExeUnit))
  //  val fmacExeUnits = Array.tabulate(exuParameters.FmacCnt)(_ => Module(new Fmac))
  //  val fmiscExeUnits = Array.tabulate(exuParameters.FmiscCnt)(_ => Module(new Fmisc))
  //  val fmiscDivSqrtExeUnits = Array.tabulate(exuParameters.FmiscDivSqrtCnt)(_ => Module(new FmiscDivSqrt))
  val exeUnits = jmpExeUnit +: (aluExeUnits ++ mulExeUnits ++ mduExeUnits)
  exeUnits.foreach(_.io.exception := DontCare)
  exeUnits.foreach(_.io.dmem := DontCare)
  exeUnits.foreach(_.io.mcommit := DontCare)

  val decode = Module(new DecodeStage)
  val brq = Module(new Brq)
  val decBuf = Module(new DecodeBuffer)
  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch(exeUnits.map(_.config)))
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
  val memRf = Module(new Regfile(
    numReadPorts = 2*exuParameters.StuCnt + exuParameters.LduCnt,
    numWirtePorts = NRIntWritePorts,
    hasZero = true,
    isMemRf = true
  ))

  // backend redirect, flush pipeline
  val redirect = Mux(roq.io.redirect.valid, roq.io.redirect, brq.io.redirect)

  val redirectInfo = Wire(new RedirectInfo)
  // exception or misprediction
  redirectInfo.valid := roq.io.redirect.valid || brq.io.out.valid
  redirectInfo.misPred := !roq.io.redirect.valid && brq.io.redirect.valid
  redirectInfo.redirect := redirect.bits



  val memConfigs =
    Seq.fill(exuParameters.LduCnt)(Exu.ldExeUnitCfg) ++
    Seq.fill(exuParameters.StuCnt)(Exu.stExeUnitCfg)

  val exuConfigs = exeUnits.map(_.config) ++ memConfigs

  val exeWbReqs = exeUnits.map(_.io.out) ++ io.mem.ldout ++ io.mem.stout

  def needWakeup(cfg: ExuConfig): Boolean =
    (cfg.readIntRf && cfg.writeIntRf) || (cfg.readFpRf && cfg.writeFpRf)

  def needData(a: ExuConfig, b: ExuConfig): Boolean =
    (a.readIntRf && b.writeIntRf) || (a.readFpRf && b.writeFpRf)

  val reservedStations = exeUnits.
    zipWithIndex.
    map({ case (exu, i) =>

      val cfg = exu.config

      val wakeUpDateVec = exuConfigs.zip(exeWbReqs).filter(x => needData(cfg, x._1)).map(_._2)
      val bypassCnt = exuConfigs.count(c => c.enableBypass && needData(cfg, c))

      println(s"exu:${cfg.name} wakeupCnt:${wakeUpDateVec.length} bypassCnt:$bypassCnt")

      val rs = Module(new ReservedStation(
        cfg, wakeUpDateVec.length, bypassCnt, cfg.enableBypass, false
      ))
      rs.io.redirect <> redirect
      rs.io.numExist <> dispatch.io.numExist(i)
      rs.io.enqCtrl <> dispatch.io.enqIQCtrl(i)
      rs.io.enqData <> dispatch.io.enqIQData(i)
      for(
        (wakeUpPort, exuOut) <-
        rs.io.wakeUpPorts.zip(wakeUpDateVec)
      ){
        wakeUpPort.bits := exuOut.bits
        wakeUpPort.valid := exuOut.valid
      }

      exu.io.in <> rs.io.deq
      exu.io.redirect <> redirect
      rs
    })

  for( rs <- reservedStations){
    rs.io.bypassUops <> reservedStations.
      filter(x => x.enableBypass && needData(rs.exuCfg, x.exuCfg)).
      map(_.io.selectedUop)

    val bypassDataVec = exuConfigs.zip(exeWbReqs).
      filter(x => x._1.enableBypass && needData(rs.exuCfg, x._1)).map(_._2)

    for(i <- bypassDataVec.indices){
      rs.io.bypassData(i).valid := bypassDataVec(i).valid
      rs.io.bypassData(i).bits := bypassDataVec(i).bits
    }
  }

  val issueQueues = exuConfigs.
    zipWithIndex.
    takeRight(exuParameters.LduCnt + exuParameters.StuCnt).
    map({case (cfg, i) =>
      val wakeUpDateVec = exuConfigs.zip(exeWbReqs).filter(x => needData(cfg, x._1)).map(_._2)
      val bypassUopVec = reservedStations.filter(r => r.exuCfg.enableBypass && needData(cfg, r.exuCfg)).map(_.io.selectedUop)
      val iq = Module(new IssueQueue(
        cfg, wakeUpDateVec.length, bypassUopVec.length
      ))
      println(s"exu:${cfg.name} wakeupCnt:${wakeUpDateVec.length} bypassCnt:${bypassUopVec.length}")
      iq.io.redirect <> redirect
      iq.io.enq <> dispatch.io.enqIQCtrl(i)
      dispatch.io.numExist(i) := iq.io.numExist
      for(
        (wakeUpPort, exuOut) <-
        iq.io.wakeUpPorts.zip(wakeUpDateVec)
      ){
        wakeUpPort.bits := exuOut.bits
        wakeUpPort.valid := exuOut.valid
      }
      iq.io.bypassUops <> bypassUopVec
      iq
    })

  val rfReader = Module(new RegfileReader(memConfigs, exeWbReqs.length))
  rfReader.io.in <> issueQueues.map(_.io.deq)
  rfReader.io.bypasses <> exeWbReqs

  io.mem.mcommit := roq.io.mcommit
  io.mem.ldin <> rfReader.io.out.take(exuParameters.LduCnt)
  io.mem.stin <> rfReader.io.out.takeRight(exuParameters.StuCnt)
  jmpExeUnit.io.exception.valid := roq.io.redirect.valid
  jmpExeUnit.io.exception.bits := roq.io.exception

  io.frontend.redirectInfo <> redirectInfo
  io.frontend.inOrderBrInfo <> brq.io.inOrderBrInfo

  decode.io.in <> io.frontend.cfVec
  brq.io.roqRedirect <> roq.io.redirect
  brq.io.bcommit := roq.io.bcommit
  brq.io.enqReqs <> decode.io.toBrq
  for ((x, y) <- brq.io.exuRedirect.zip(exeUnits.filter(_.config.hasRedirect))) {
    x.bits := y.io.out.bits
    x.valid := y.io.out.fire() && y.io.out.bits.redirectValid
  }
  decode.io.brTags <> brq.io.brTags
  decBuf.io.redirect <> redirect
  decBuf.io.in <> decode.io.out

  rename.io.redirect <> redirect
  rename.io.roqCommits <> roq.io.commits
  rename.io.in <> decBuf.io.out
  rename.io.intRfReadAddr <> dispatch.io.readIntRf.map(_.addr) ++ issueQueues.flatMap(_.io.intRfReadAddr)
  rename.io.intPregRdy <> dispatch.io.intPregRdy ++ issueQueues.flatMap(_.io.intSrcRdy)
  rename.io.fpRfReadAddr <> dispatch.io.readFpRf.map(_.addr) ++ issueQueues.flatMap(_.io.fpRfReadAddr)
  rename.io.fpPregRdy <> dispatch.io.fpPregRdy ++ issueQueues.flatMap(_.io.fpSrcRdy)
  dispatch.io.redirect <> redirect
  dispatch.io.fromRename <> rename.io.out

  roq.io.brqRedirect <> brq.io.redirect
  roq.io.dp1Req <> dispatch.io.toRoq
  dispatch.io.roqIdxs <> roq.io.roqIdxs
  io.mem.dp1Req <> dispatch.io.toMoq
  dispatch.io.moqIdxs <> io.mem.moqIdxs

  intRf.io.readPorts <> dispatch.io.readIntRf
  fpRf.io.readPorts <> dispatch.io.readFpRf ++ rfReader.io.readFpRf
  memRf.io.readPorts <> rfReader.io.readIntRf

  val wbIntIdx = exuConfigs.zipWithIndex.filter(_._1.writeIntRf).map(_._2)
  val wbFpIdx = exuConfigs.zipWithIndex.filter(_._1.writeFpRf).map(_._2)

  val wbu = Module(new Wbu(wbIntIdx, wbFpIdx))
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
  val intRfWrite = wbIntResults.map(exuOutToRfWrite)
  intRf.io.writePorts <> intRfWrite
  memRf.io.writePorts <> intRfWrite
  fpRf.io.writePorts <> wbFpResults.map(exuOutToRfWrite)

  rename.io.wbIntResults <> wbIntResults
  rename.io.wbFpResults <> wbFpResults

  roq.io.exeWbResults.take(exeWbReqs.length).zip(wbu.io.toRoq).foreach(x => x._1 := x._2)
  roq.io.exeWbResults.last := brq.io.out


  // TODO: Remove sink and source
  val tmp = WireInit(0.U)
  val sinks = Array[String](
    "DTLBFINISH",
    "DTLBPF",
    "DTLBENABLE",
    "perfCntCondMdcacheLoss",
    "perfCntCondMl2cacheLoss",
    "perfCntCondMdcacheHit",
    "lsuMMIO",
    "perfCntCondMl2cacheHit",
    "perfCntCondMl2cacheReq",
    "mtip",
    "perfCntCondMdcacheReq",
    "meip"
  )
  for (s <- sinks) {
    BoringUtils.addSink(tmp, s)
  }

  val debugIntReg, debugFpReg = WireInit(VecInit(Seq.fill(32)(0.U(XLEN.W))))
  BoringUtils.addSink(debugIntReg, "DEBUG_INT_ARCH_REG")
  BoringUtils.addSink(debugFpReg, "DEBUG_FP_ARCH_REG")
  val debugArchReg = WireInit(VecInit(debugIntReg ++ debugFpReg))
  if (!p.FPGAPlatform) {
    BoringUtils.addSource(debugArchReg, "difftestRegs")
  }

}
