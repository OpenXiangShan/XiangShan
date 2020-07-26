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
import xiangshan.backend.issue.IssueQueue
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
  val lsuExeUnits = Array.tabulate(exuParameters.StuCnt)(_ => Module(new LsExeUnit))
  val exeUnits = jmpExeUnit +: (aluExeUnits ++ mulExeUnits ++ mduExeUnits ++ lsuExeUnits)
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
    numReadPorts = NRReadPorts,
    numWirtePorts = NRWritePorts,
    hasZero = true
  ))
  val fpRf = Module(new Regfile(
    numReadPorts = NRReadPorts,
    numWirtePorts = NRWritePorts,
    hasZero = false
  ))

  // backend redirect, flush pipeline
  val redirect = Mux(roq.io.redirect.valid, roq.io.redirect, brq.io.redirect)

  val redirectInfo = Wire(new RedirectInfo)
  // exception or misprediction
  redirectInfo.valid := roq.io.redirect.valid || brq.io.out.valid
  redirectInfo.misPred := !roq.io.redirect.valid && brq.io.redirect.valid
  redirectInfo.redirect := redirect.bits

  var iqInfo = new StringBuilder
  val issueQueues = exeUnits.zipWithIndex.map({ case (eu, i) =>
    def needBypass(cfg: ExuConfig): Boolean = cfg.enableBypass

    val bypassCnt = exeUnits.map(_.config).count(needBypass)
    def needWakeup(cfg: ExuConfig): Boolean =
      (cfg.readIntRf && cfg.writeIntRf) || (cfg.readFpRf && cfg.writeFpRf)

    val wakeupCnt = exeUnits.map(_.config).count(needWakeup)
    assert(!(needBypass(eu.config) && !needWakeup(eu.config))) // needBypass but dont needWakeup is not allowed
    val iq = Module(new IssueQueue(
      eu.config,
      wakeupCnt,
      bypassCnt,
      eu.config.enableBypass,
      fifo = eu.config.supportedFuncUnits.contains(FunctionUnit.lsuCfg)
    ))
    iq.io.redirect <> redirect
    iq.io.numExist <> dispatch.io.numExist(i)
    iq.io.enqCtrl <> dispatch.io.enqIQCtrl(i)
    iq.io.enqData <> dispatch.io.enqIQData(i)
    for(
      (wakeUpPort, exuOut) <-
      iq.io.wakeUpPorts.zip(exeUnits.filter(e => needWakeup(e.config)).map(_.io.out))
    ){
      wakeUpPort.bits := exuOut.bits
      wakeUpPort.valid := exuOut.valid
    }
    iqInfo ++= {
      s"[$i] ${eu.name} Queue wakeupCnt:$wakeupCnt bypassCnt:$bypassCnt" +
        s" Supported Function:[" +
        s"${
          eu.config.supportedFuncUnits.map(
            fu => FuType.functionNameMap(fu.fuType.litValue())).mkString(", "
          )
        }]\n"
    }
    eu.io.in <> iq.io.deq
    eu.io.redirect <> redirect
    iq
  })

  val bypassQueues = issueQueues.filter(_.enableBypass)
  val bypassUnits = exeUnits.filter(_.config.enableBypass)
  issueQueues.foreach(iq => {
    for (i <- iq.io.bypassUops.indices) {
      iq.io.bypassData(i).bits := bypassUnits(i).io.out.bits
      iq.io.bypassData(i).valid := bypassUnits(i).io.out.valid
    }
    iq.io.bypassUops <> bypassQueues.map(_.io.selectedUop)
  })

  lsuExeUnits.foreach(_.io.dmem <> DontCare) // TODO
  lsuExeUnits.foreach(_.io.mcommit <> roq.io.mcommit)
  io.mem.mcommit := roq.io.mcommit
  io.mem.ldin := DontCare // TODO
  io.mem.stin := DontCare // TODO
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
  rename.io.intRfReadAddr <> dispatch.io.readIntRf.map(_.addr)
  rename.io.fpRfReadAddr <> dispatch.io.readFpRf.map(_.addr)
  rename.io.intPregRdy <> dispatch.io.intPregRdy
  rename.io.fpPregRdy <> dispatch.io.fpPregRdy

  dispatch.io.redirect <> redirect
  dispatch.io.fromRename <> rename.io.out

  roq.io.brqRedirect <> brq.io.redirect
  roq.io.dp1Req <> dispatch.io.toRoq
  dispatch.io.roqIdxs <> roq.io.roqIdxs
  io.mem.dp1Req <> dispatch.io.toMoq
  dispatch.io.moqIdxs <> io.mem.moqIdxs

  intRf.io.readPorts <> dispatch.io.readIntRf
  fpRf.io.readPorts <> dispatch.io.readFpRf

  val exeWbReqs = exeUnits.map(_.io.out)

  val wbIntIdx = exeUnits.zipWithIndex.filter(_._1.config.writeIntRf).map(_._2)
  val wbFpIdx = exeUnits.zipWithIndex.filter(_._1.config.writeFpRf).map(_._2)

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

  intRf.io.writePorts <> wbIntResults.map(exuOutToRfWrite)
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

  print(iqInfo)

}
