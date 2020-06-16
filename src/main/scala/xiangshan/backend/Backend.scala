package xiangshan.backend

import bus.simplebus.SimpleBusUC
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import noop.MemMMUIO
import xiangshan._
import xiangshan.backend.decode.DecodeStage
import xiangshan.backend.rename.Rename
import xiangshan.backend.brq.Brq
import xiangshan.backend.dispatch.{Dispatch1, Dispatch2}
import xiangshan.backend.exu.{ExeUnits, HasExuHelper, WriteBackArbMtoN}
import xiangshan.backend.issue.IssueStage
import xiangshan.backend.regfile.Regfile
import xiangshan.backend.roq.Roq


/** Backend Pipeline:
  * Decode -> Rename -> Dispatch-1 -> Dispatch-2 -> Issue -> Exe
  */
class Backend(implicit val p: XSConfig) extends XSModule
  with HasExuHelper
  with NeedImpl
{
  val io = IO(new Bundle {
    val dmem = new SimpleBusUC(addrBits = VAddrBits)
    val memMMU = Flipped(new MemMMUIO)
    val frontend = Flipped(new FrontendToBackendIO)
  })


  val decode = Module(new DecodeStage)
  val rename = Module(new Rename)
  val dispatch1 = Module(new Dispatch1)
  val roq = Module(new Roq)
  val brq = Module(new Brq)
  val dispatch2 = Module(new Dispatch2)
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
  val issue = Module(new IssueStage)
  val exeUnits = Module(new ExeUnits)
  val exeWbReqs = exeUnits.io.wbReqs
  val exeWbResults = exeUnits.io.wbResults


  val redirect = Mux(roq.io.redirect.valid, roq.io.redirect, brq.io.redirect)

  io.frontend.redirect <> redirect
  io.frontend.commits <> roq.io.commits

  decode.io.in <> io.frontend.cfVec

  rename.io.redirect <> redirect
  rename.io.roqCommits <> roq.io.commits
  rename.io.in <> decode.io.out

  dispatch1.io.redirect <> redirect
  dispatch1.io.in <> rename.io.out
  roq.io.brqRedirect <> brq.io.redirect
  roq.io.dp1Req <> dispatch1.io.toRoq
  dispatch1.io.roqIdxs <> roq.io.roqIdxs
  brq.io.roqRedirect <> roq.io.redirect
  brq.io.dp1Req <> dispatch1.io.toBrq
  dispatch1.io.brTags <> brq.io.brTags
  dispatch1.io.brMasks <> brq.io.brMasks

  dispatch2.io.in <> dispatch1.io.out
  intRf.io.readPorts <> dispatch2.io.readIntRf
  fpRf.io.readPorts <> dispatch2.io.readFpRf

  issue.io.redirect <> redirect
  issue.io.in <> dispatch2.io.out
  issue.io.exeWbResults <> exeWbResults

  exeUnits.io.redirect <> redirect
  exeUnits.io.roqCommits <> roq.io.commits
  exeUnits.io.in <> issue.io.out

  roq.io.exeWbResults <> exeWbResults


  //TODO: filter fp/int write back req in lsResults
  val wbIntReqs = exeWbReqs.getBru +:
    (
      exeWbReqs.getAluVec ++ exeWbReqs.getMulVec ++ exeWbReqs.getMduVec ++ exeWbReqs.getLsuVec
    )
  val intWbArb = Module(new WriteBackArbMtoN(wbIntReqs.size, NRWritePorts))
  intWbArb.io.in <> wbIntReqs
  intRf.io.writePorts <> intWbArb.io.out

  val wbFpReqs = exeWbReqs.getFmacVec ++ exeWbReqs.getFmiscVec ++ exeWbReqs.getFmiscDivSqrtVec
  val fpWbArb = Module(new WriteBackArbMtoN(wbFpReqs.size, NRWritePorts))
  fpWbArb.io.in <> wbFpReqs
  fpRf.io.writePorts <> fpWbArb.io.out



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
