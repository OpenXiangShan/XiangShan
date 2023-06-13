package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.HasCircularQueuePtrHelper
import xiangshan._
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.mem.{MemWaitUpdateReq, SqPtr}
import xiangshan.backend.Bundles.{DynInst, IssueQueueIssueBundle, IssueQueueWakeUpBundle}
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.exu.ExeUnitParams

class FuBusyTableRead(val idx: Int, isWb: Boolean, isVf: Boolean = false)(implicit p: Parameters, iqParams: IssueBlockParams) extends XSModule {
  val io = IO(new FuBusyTableReadIO(idx, isWb, isVf))

  val fuBusyTableSplit = if (!isWb) io.in.fuBusyTable.asBools.reverse else io.in.fuBusyTable.asBools
  val latencyMap = if (!isWb) iqParams.exuBlockParams(idx).fuLatencyMap
                   else if (isVf) iqParams.exuBlockParams(idx).vfFuLatencyMap
                   else iqParams.exuBlockParams(idx).intFuLatencyMap
  val fuTypeRegVec = io.in.fuTypeRegVec


  val isReadLatencyNumVec2 = fuBusyTableSplit.zipWithIndex.map { case (en, latencyIdx) =>
    val latencyHitVec = WireInit(0.U(iqParams.numEntries.W))
    when(en) {
      latencyHitVec := VecInit(fuTypeRegVec.map { case futype =>
        val latencyHitFuType = latencyMap.get.filter(_._2 == latencyIdx).map(_._1)
        val isLatencyNum = Cat(latencyHitFuType.map(_.U === futype)).asUInt.orR
        isLatencyNum
      }).asUInt
    }
    latencyHitVec
  }

  io.out.fuBusyTableMask := isReadLatencyNumVec2.fold(0.U(iqParams.numEntries.W))(_ | _)
}

class FuBusyTableReadIO(val idx: Int, isWb: Boolean, isVf: Boolean)(implicit p: Parameters, iqParams: IssueBlockParams) extends XSBundle {
  val in = new Bundle {
    val fuBusyTable = if (!isWb) Input(UInt(iqParams.exuBlockParams(idx).latencyValMax.get.W))
                      else if (isVf) Input(UInt((iqParams.exuBlockParams(idx).vfLatencyValMax.get + 1).W))
                      else Input(UInt((iqParams.exuBlockParams(idx).intLatencyValMax.get + 1).W))
    val fuTypeRegVec = Input(Vec(iqParams.numEntries, FuType()))
  }
  val out = new Bundle {
    val fuBusyTableMask = Output(UInt(iqParams.numEntries.W))
  }
}