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
import xiangshan.backend.issue.IssueQueue
import xiangshan.backend.regfile.{Regfile, RfWritePort}
import xiangshan.backend.roq.Roq


/** Backend Pipeline:
  * Decode -> Rename -> Dispatch-1 -> Dispatch-2 -> Issue -> Exe
  */
class Backend(implicit val p: XSConfig) extends XSModule
  with HasExeUnits
  with NeedImpl
{
  val io = IO(new Bundle {
    val dmem = new SimpleBusUC(addrBits = VAddrBits)
    val memMMU = Flipped(new MemMMUIO)
    val frontend = Flipped(new FrontendToBackendIO)
  })


  val decode = Module(new DecodeStage)
  val brq = Module(new Brq)
  val decBuf = Module(new DecodeBuffer)
  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch)
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
  val redirect = Mux(roq.io.redirect.valid, roq.io.redirect, brq.io.redirect)
  val issueQueues = exeUnits.zipWithIndex.map({ case(eu, i) =>
    def needBypass(x: Exu): Boolean = (eu.enableBypass)
    val bypassCnt = exeUnits.count(needBypass)//if(eu.fuTypeInt == FuType.alu.litValue()) exuConfig.AluCnt else 0
    def needWakeup(x: Exu): Boolean = (eu.readIntRf && x.writeIntRf) || (eu.readFpRf && x.writeFpRf)
    val wakeupCnt = exeUnits.count(needWakeup)
    assert(!(needBypass(eu) && !needWakeup(eu))) // needBypass but dont needWakeup is not allowed
    val iq = Module(new IssueQueue(eu.fuTypeInt, wakeupCnt, bypassCnt, eu.fixedDelay))
    iq.io.redirect <> redirect
    iq.io.enqCtrl <> dispatch.io.enqIQCtrl(i)
    iq.io.enqData <> dispatch.io.enqIQData(i)
    val wuUnitsOut = exeUnits.filter(e => needWakeup(e)).map(_.io.out)
    for(i <- iq.io.wakeUpPorts.indices) {
      iq.io.wakeUpPorts(i).bits <> wuUnitsOut(i).bits
      iq.io.wakeUpPorts(i).valid := wuUnitsOut(i).valid
    }
    println(s"[$i] $eu Queue wakeupCnt:$wakeupCnt bypassCnt:$bypassCnt")
    eu.io.in <> iq.io.deq
    eu.io.redirect <> redirect
    iq
  })

  val bypassQueues = issueQueues.filter(_.bypassCnt > 0)
  val bypassUnits = exeUnits.filter(_.enableBypass)
  bypassQueues.foreach(iq => {
    for(i <- iq.io.bypassUops.indices) {
      iq.io.bypassData(i).bits := bypassUnits(i).io.out.bits
      iq.io.bypassData(i).valid := bypassUnits(i).io.out.valid
    }
    iq.io.bypassUops <> bypassQueues.map(_.io.selectedUop)
  })
  // val aluQueues = issueQueues.filter(_.fuTypeInt == FuType.alu.litValue())
  // aluQueues.foreach(aluQ => {
  //   aluQ.io.bypassUops <> aluQueues.map(_.io.selectedUop)
  //   aluQ.io.bypassData <> aluExeUnits.map(_.io.out)
  // })

  lsuExeUnits.foreach(_.io.dmem <> io.dmem)

  io.frontend.redirect <> redirect
  io.frontend.commits <> roq.io.commits

  decode.io.in <> io.frontend.cfVec
  brq.io.roqRedirect <> roq.io.redirect
  brq.io.enqReqs <> decode.io.toBrq
  for(i <- bjUnits.indices){
    brq.io.exuRedirect(i).bits := bjUnits(i).io.out.bits
    brq.io.exuRedirect(i).valid := bjUnits(i).io.out.fire()
  }
  decode.io.brMasks <> brq.io.brMasks
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

  intRf.io.readPorts <> dispatch.io.readIntRf
  fpRf.io.readPorts <> dispatch.io.readFpRf

  val exeWbReqs = exeUnits.map(_.io.out)
  val wbIntReqs = (bruExeUnit +: (aluExeUnits ++ mulExeUnits ++ mduExeUnits)).map(_.io.out)
  val wbFpReqs = (fmacExeUnits ++ fmiscExeUnits ++ fmiscDivSqrtExeUnits).map(_.io.out)
  val intWbArb = Module(new WriteBackArbMtoN(wbIntReqs.length, NRWritePorts))
  val fpWbArb = Module(new WriteBackArbMtoN(wbFpReqs.length, NRWritePorts))
  val wbIntResults = intWbArb.io.out
  val wbFpResults = fpWbArb.io.out

  def exuOutToRfWrite(x: Valid[ExuOutput]) = {
    val rfWrite = Wire(new RfWritePort)
    rfWrite.wen := x.valid
    rfWrite.addr := x.bits.uop.pdest
    rfWrite.data := x.bits.data
    rfWrite
  }

  intWbArb.io.in <> wbIntReqs
  intRf.io.writePorts <> wbIntResults.map(exuOutToRfWrite)

  fpWbArb.io.in <> wbFpReqs
  fpRf.io.writePorts <> wbFpResults.map(exuOutToRfWrite)

  rename.io.wbIntResults <> wbIntResults
  rename.io.wbFpResults <> wbFpResults

  roq.io.exeWbResults.zip(exeWbReqs).foreach({case (x,y) => {
    x.bits := y.bits
    x.valid := y.fire()
  }})


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
  for (s <- sinks){ BoringUtils.addSink(tmp, s) }

  // A fake commit
  // TODO: difftest 6 insts per cycle
  val commit = RegNext(RegNext(RegNext(true.B)))
  val pc = WireInit("h80000000".U)
  val inst = WireInit("h66666666".U)

  if(!p.FPGAPlatform){
    BoringUtils.addSource(commit, "difftestCommit")
    BoringUtils.addSource(pc, "difftestThisPC")
    BoringUtils.addSource(inst, "difftestThisINST")
    BoringUtils.addSource(tmp, "difftestIsMMIO")
    BoringUtils.addSource(tmp, "difftestIsRVC")
    BoringUtils.addSource(tmp, "difftestIntrNO")
    BoringUtils.addSource(VecInit(Seq.fill(64)(tmp)), "difftestRegs")
    BoringUtils.addSource(tmp, "difftestMode")
    BoringUtils.addSource(tmp, "difftestMstatus")
    BoringUtils.addSource(tmp, "difftestSstatus")
    BoringUtils.addSource(tmp, "difftestMepc")
    BoringUtils.addSource(tmp, "difftestSepc")
    BoringUtils.addSource(tmp, "difftestMcause")
    BoringUtils.addSource(tmp, "difftestScause")
  }

}
