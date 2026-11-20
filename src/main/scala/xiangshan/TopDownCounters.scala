/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

// Hierarchical TopDown enum. Declaration order is the stall priority / .id.
// L1/L2 names may be reused under different parents.
abstract class TopDownEnumeration extends Enumeration {
  trait Level2 {
    def l1Name: String
    def l2Name: String
  }

  abstract class L1(val l1Name: String) { l1 =>
    abstract class L2(val l2Name: String) extends Level2 {
      def l1Name: String = l1.l1Name
    }
  }

  class TopDownVal(i: Int, name: String, val l1: String, val l2: String) extends Val(i, name) {
    def displayName: String = s"${l1}_${l2}_$name"
  }

  private var nextTopDownId = 0

  protected def Value(name: String, group: Level2): TopDownVal = {
    val id = nextTopDownId
    nextTopDownId += 1
    new TopDownVal(id, name, group.l1Name, group.l2Name)
  }
}

object TopDownCounters extends TopDownEnumeration {
  object Base extends L1("Base") {
    object Retiring extends L2("Retiring")
  }
  object Frontend extends L1("Frontend") {
    object FetchLatency   extends L2("FetchLatency")
    object FetchBandwidth extends L2("FetchBandwidth")
    object Ftq            extends L2("Ftq")
  }
  object BadSpec extends L1("BadSpec") {
    object BpuMissBubble extends L2("BpuMissBubble")
    object RedirectStall extends L2("RedirectStall")
    object ReplayStall   extends L2("ReplayStall")
    object FlushedInst   extends L2("FlushedInst")
  }
  object Core extends L1("Core") {
    object IssueCancel    extends L2("IssueCancel")
    object IssueDelay     extends L2("IssueDelay")
    object Exec           extends L2("Exec")
    object Rob            extends L2("Rob")
    object Fusion         extends L2("Fusion")
    object DispatchPolicy extends L2("DispatchPolicy")
    object IQPolicy       extends L2("IQPolicy")
    object IQFull         extends L2("IQFull")
    object RabWalk        extends L2("RabWalk")
    object Other          extends L2("Other")
  }
  object Load extends L1("Load") {
    object Dcache extends L2("Dcache")
    object Dtlb   extends L2("Dtlb")
    object Queue  extends L2("Queue")
    object Replay extends L2("Replay")
    object Exec   extends L2("Exec")
  }
  object Store extends L1("Store") {
    object Exec  extends L2("Exec")
    object Queue extends L2("Queue")
  }
  object Freelist extends L1("Freelist") {
    object Rename extends L2("Rename")
  }
  object Privileged extends L1("Privileged") {
    object Special extends L2("Special")
  }
  object Misc extends L1("Misc") {
    object Atomic extends L2("Atomic")
    object Other  extends L2("Other")
  }

  val NoStall = Value("NoStall", Base.Retiring)
  // frontend
  val OverrideBubble = Value("OverrideBubble", Frontend.Ftq)
  val FtqUpdateBubble = Value("FtqUpdateBubble", BadSpec.BpuMissBubble)
  // val ControlRedirectBubble = Value("ControlRedirectBubble")
  val TAGEMissBubble = Value("TAGEMissBubble", BadSpec.BpuMissBubble)
  val SCMissBubble = Value("SCMissBubble", BadSpec.BpuMissBubble)
  val ITTAGEMissBubble = Value("ITTAGEMissBubble", BadSpec.BpuMissBubble)
  val RASMissBubble = Value("RASMissBubble", BadSpec.BpuMissBubble)
  val MemVioRedirectBubble = Value("MemVioRedirectBubble", BadSpec.RedirectStall)
  val OtherRedirectBubble = Value("OtherRedirectBubble", Misc.Other)
  val FtqFullStall = Value("FtqFullStall", Frontend.Ftq)

  val ICacheMissBubble = Value("ICacheMissBubble", Frontend.FetchLatency)
  val ITLBMissBubble = Value("ITLBMissBubble", Frontend.FetchLatency)
  val BTBMissBubble = Value("BTBMissBubble", BadSpec.BpuMissBubble)
  val FetchFragBubble = Value("FetchFragBubble", Frontend.FetchBandwidth)
  val FrontendOtherCoreStall = Value("FrontendOtherCoreStall", Core.Other)

  // backend
  // long inst stall at rob head
  val IssueCancelStallOg0 = Value("IssueCancelStallOg0", Core.IssueCancel)
  val IssueCancelStallOg1 = Value("IssueCancelStallOg1", Core.IssueCancel)
  val IssueCancelStallLd = Value("IssueCancelStallLd", Load.Exec)
  val IssueCancelStallSt = Value("IssueCancelStallSt", Store.Exec)
  val IssueCancelStallOther = Value("IssueCancelStallOther", Core.IssueCancel)
  val IssueDelayStall = Value("IssueDelayStall", Core.IssueDelay)
  val DivStall = Value("DivStall", Core.Exec) // int div, float div/sqrt
  val IntNotReadyStall = Value("IntNotReadyStall", Core.Exec) // int-inst at rob head exec long
  val FPNotReadyStall = Value("FPNotReadyStall", Core.Exec) // fp-inst at rob head exec long
  val MemNotReadyStall = Value("MemNotReadyStall", Load.Exec) // mem-inst at rob head exec long
  val OtherNotReadyStall = Value("OtherNotReadyStall", Core.Exec)
  val RobStall = Value("RobStall", Core.Rob)
  val LqStall = Value("LqStall", Load.Queue)
  val SqStall = Value("SqStall", Store.Queue)
  // freelist full
  val IntFlStall = Value("IntFlStall", Freelist.Rename)
  val FpFlStall = Value("FpFlStall", Freelist.Rename)
  val VecFlStall = Value("VecFlStall", Freelist.Rename)
  val V0FlStall = Value("V0FlStall", Freelist.Rename)
  val VlFlStall = Value("VlFlStall", Freelist.Rename)
  val MultiFlStall = Value("MultiFlStall", Freelist.Rename)
  // fusion bubble
  val FusionBubble = Value("FusionBubble", Core.Fusion)
  // dispatch stall
  // dispatch stall for dispatch policy
  // TODO: explain only load store exist
  val LoadDispatchPolicyStall = Value("LoadDispatchPolicyStall", Core.DispatchPolicy)
  val StoreDispatchPolicyStall = Value("StoreDispatchPolicyStall", Core.DispatchPolicy)
  val OtherDispatchPolicyStall = Value("OtherDispatchPolicyStall", Core.DispatchPolicy)
  // dispatch stall for issuequeue full
  val BalanceDispatchPolicyStallAlu = Value("BalanceDispatchPolicyStallAlu", Core.DispatchPolicy)
  val BalanceDispatchPolicyStallBrh = Value("BalanceDispatchPolicyStallBrh", Core.DispatchPolicy)
  val BalanceDispatchPolicyStallInt = Value("BalanceDispatchPolicyStallInt", Core.DispatchPolicy)
  val BalanceDispatchPolicyStallFp = Value("BalanceDispatchPolicyStallFp", Core.DispatchPolicy)
  val BalanceDispatchPolicyStallVec = Value("BalanceDispatchPolicyStallVec", Core.DispatchPolicy)
  val BalanceDispatchPolicyStallLoad = Value("BalanceDispatchPolicyStallLoad", Core.DispatchPolicy)
  val BalanceDispatchPolicyStallStore = Value("BalanceDispatchPolicyStallStore", Core.DispatchPolicy)
  val OtherBalanceDispatchPolicyStall = Value("OtherBalanceDispatchPolicyStall", Core.DispatchPolicy)
  val IQEnqPolicyStallIssued = Value("IQEnqPolicyStallIssued", Core.IQPolicy)
  val IQEnqPolicyStall = Value("IQEnqPolicyStall", Core.IQPolicy)
  val IntIQFullStallAlu = Value("IntIQFullStallAlu", Core.IQFull)
  val IntIQFullStallBrh = Value("IntIQFullStallBrh", Core.IQFull)
  val IntIQFullStallOther = Value("IntIQFullStallOther", Core.IQFull)
  val FpIQFullStall = Value("FpIQFullStall", Core.IQFull)
  val VecIQFullStall = Value("VecIQFullStall", Core.IQFull)
  val LoadIQFullStall = Value("LoadIQFullStall", Core.IQFull)
  val StoreIQFullStall = Value("StoreIQFullStall", Core.IQFull)
  val OtherIQFullStall = Value("OtherIQFullStall", Core.IQFull)
  val RobHeadNotIssued = Value("RobHeadNotIssued", Core.IQPolicy)

  // memblock
  val LoadTLBStall = Value("LoadTLBStall", Load.Dtlb)
  val LoadL1Stall = Value("LoadL1Stall", Load.Dcache)
  val LoadL2Stall = Value("LoadL2Stall", Load.Dcache)
  val LoadL3Stall = Value("LoadL3Stall", Load.Dcache)
  val LoadMemStall = Value("LoadMemStall", Load.Dcache)
  val StoreStall = Value("StoreStall", Store.Exec) // include store tlb miss
  val AtomicStall = Value("AtomicStall", Misc.Atomic) // atomic, load reserved, store conditional

  // xs replay (different to gem5)
  val LoadVioReplayStall = Value("LoadVioReplayStall", BadSpec.ReplayStall)
  val LoadMSHRReplayStall = Value("LoadMSHRReplayStall", Load.Replay)

  // bad speculation
  val ControlRedirectStall = Value("ControlRedirectStall", BadSpec.RedirectStall)
  val MemVioRedirectStall = Value("MemVioRedirectStall", BadSpec.RedirectStall)
  val OtherRedirectStall = Value("OtherRedirectStall", BadSpec.RedirectStall)
  val ControlRecoveryStall = Value("ControlRecoveryStall", Core.RabWalk)
  val MemVioRecoveryStall = Value("MemVioRecoveryStall", Core.RabWalk)
  val OtherRecoveryStall = Value("OtherRecoveryStall", Core.RabWalk)

  val FlushedInsts = Value("FlushedInsts", BadSpec.FlushedInst) // control flushed, memvio flushed, others
  val SpecialInsts = Value("SpecialInsts", Privileged.Special)

  val BackendOtherCoreStall = Value("BackendOtherCoreStall", Core.Other)

  val NumStallReasons = Value("NumStallReasons")

  val SchemaVersion: Int = 1

  def perfCounters: Seq[TopDownVal] =
    values.toSeq.collect { case v: TopDownVal => v }.sortBy(_.id)

  def renderJson(): String = {
    val counters = perfCounters.map { c =>
      s"""    {
        |      "name": ${escapeJson(c.toString)},
        |      "display_name": ${escapeJson(c.displayName)},
        |      "l1_group": ${escapeJson(c.l1)},
        |      "l2_group": ${escapeJson(c.l2)},
        |      "priority": ${c.id},
        |      "aliases": []
        |    }""".stripMargin
    }.mkString(",\n")
    s"""{
      |  "version": $SchemaVersion,
      |  "source": "xiangshan.TopDownCounters",
      |  "counters": [
      |$counters
      |  ]
      |}
      |""".stripMargin
  }

  def writeJson(path: String): Unit = {
    val p = Paths.get(path)
    Option(p.getParent).foreach(parent => Files.createDirectories(parent))
    Files.write(p, renderJson().getBytes(StandardCharsets.UTF_8))
  }

  private def escapeJson(s: String): String = {
    val escaped = s.flatMap {
      case '\\'  => "\\\\"
      case '"'   => "\\\""
      case '\n'  => "\\n"
      case '\r'  => "\\r"
      case '\t'  => "\\t"
      case other => other.toString
    }
    "\"" + escaped + "\""
  }
}

object ExportTopDownCounters {
  def main(args: Array[String]): Unit = {
    val dest = args.headOption.getOrElse {
      throw new IllegalArgumentException("usage: ExportTopDownCounters <output-json-path>")
    }
    TopDownCounters.writeJson(dest)
    Console.out.println(s"[TopDownCounters] wrote ${TopDownCounters.perfCounters.size} counters to $dest")
  }
}
