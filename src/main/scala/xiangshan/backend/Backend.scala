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
//  val dispatch1 = Module(new Dispatch1)
  val roq = Module(new Roq)
//  val dispatch2 = Module(new Dispatch2)
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
    def needWakeup(x: Exu): Boolean = (eu.readIntRf && x.writeIntRf) || (eu.readFpRf && x.writeFpRf)
    val wakeupCnt = exeUnits.count(needWakeup)
    val bypassCnt = if(eu.fuTypeInt == FuType.alu.litValue()) exuConfig.AluCnt else 0
    val iq = Module(new IssueQueue(eu.fuTypeInt, wakeupCnt, bypassCnt))
    iq.io.redirect <> redirect
//    iq.io.enqCtrl <> dispatch2.io.enqIQCtrl(i)
//    iq.io.enqData <> dispatch2.io.enqIQData(i)
    iq.io.enqCtrl <> dispatch.io.enqIQCtrl(i)
    iq.io.enqData <> dispatch.io.enqIQData(i)
    iq.io.wakeUpPorts <> exeUnits.filter(needWakeup).map(_.io.out)
    println(s"[$i] $eu Queue wakeupCnt:$wakeupCnt bypassCnt:$bypassCnt")
    eu.io.in <> iq.io.deq
    iq
  })

  val aluQueues = issueQueues.filter(_.fuTypeInt == FuType.alu.litValue())
  aluQueues.foreach(aluQ => {
    aluQ.io.bypassUops <> aluQueues.map(_.io.selectedUop)
    aluQ.io.bypassData <> aluExeUnits.map(_.io.out)
  })

  io.frontend.redirect <> redirect
  io.frontend.commits <> roq.io.commits

  decode.io.in <> io.frontend.cfVec
  brq.io.roqRedirect <> roq.io.redirect
  brq.io.enqReqs <> decode.io.toBrq
  decode.io.brMasks <> brq.io.brMasks
  decode.io.brTags <> brq.io.brTags
  decBuf.io.in <> decode.io.out

  rename.io.redirect <> redirect
  rename.io.roqCommits <> roq.io.commits
  rename.io.in <> decBuf.io.out

//  dispatch1.io.redirect <> redirect
//  dispatch1.io.in <> rename.io.out
  dispatch.io.redirect <> redirect
  dispatch.io.fromRename <> rename.io.out
  roq.io.brqRedirect <> brq.io.redirect
//<<<<<<< HEAD
//  roq.io.dp1Req <> dispatch1.io.toRoq
//  dispatch1.io.roqIdxs <> roq.io.roqIdxs
  roq.io.dp1Req <> dispatch.io.toRoq
  dispatch.io.roqIdxs <> roq.io.roqIdxs

//  dispatch2.io.in <> dispatch1.io.out
//  intRf.io.readPorts <> dispatch2.io.readIntRf
//  fpRf.io.readPorts <> dispatch2.io.readFpRf
  intRf.io.readPorts <> dispatch.io.readIntRf
  fpRf.io.readPorts <> dispatch.io.readFpRf
//=======
//  roq.io.dp1Req <> dispatch1.io.toRoq
//  dispatch1.io.roqIdxs <> roq.io.roqIdxs
//
//  dispatch2.io.in <> dispatch1.io.out
//  dispatch2.io.intPregRdy <> rename.io.intPregRdy
//  dispatch2.io.fpPregRdy <> rename.io.fpPregRdy
//  intRf.io.readPorts <> dispatch2.io.readIntRf
//  rename.io.intRfReadAddr <> dispatch2.io.readIntRf.map(_.addr)
//  fpRf.io.readPorts <> dispatch2.io.readFpRf
//  rename.io.fpRfReadAddr <> dispatch2.io.readFpRf.map(_.addr)
//
//>>>>>>> d43dd6a5febdaa239b3a31d11582e3adbaa3014d

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

  roq.io.exeWbResults <> exeWbReqs


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
