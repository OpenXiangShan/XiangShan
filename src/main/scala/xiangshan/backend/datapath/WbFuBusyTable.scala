package xiangshan.backend.datapath

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.OptionWrapper
import xiangshan._
import xiangshan.backend._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.exu.ExeUnitParams

class WbFuBusyTable(implicit  p: Parameters, params: BackendParams) extends XSModule {
  val io = IO(new WbFuBusyTableIO)

  private val intSchdBusyTable = io.in.intSchdBusyTable
  private val vfSchdBusyTable = io.in.vfSchdBusyTable
  private val memSchdBusyTable = io.in.memSchdBusyTable
  private val intRespRead = io.out.intRespRead
  private val vfRespRead = io.out.vfRespRead
  private val memRespRead = io.out.memRespRead
  private val intAllWbConflictFlag = io.out.wbConflictRead.flatten.flatten.map(_.intConflict)
  private val vfAllWbConflictFlag = io.out.wbConflictRead.flatten.flatten.map(_.vfConflict)

  private val intAllBusyTable = (intSchdBusyTable ++ vfSchdBusyTable ++ memSchdBusyTable).flatten.map(_.intWbBusyTable)
  private val vfAllBusyTable = (intSchdBusyTable ++ vfSchdBusyTable ++ memSchdBusyTable).flatten.map(_.vfWbBusyTable)
  private val intAllDeqRespSet = (intSchdBusyTable ++ vfSchdBusyTable ++ memSchdBusyTable).flatten.map(_.intDeqRespSet)
  private val vfAllDeqRespSet = (intSchdBusyTable ++ vfSchdBusyTable ++ memSchdBusyTable).flatten.map(_.vfDeqRespSet)
  private val intAllRespRead = (intRespRead ++ vfRespRead ++ memRespRead).flatten.map(_.intWbBusyTable)
  private val vfAllRespRead = (intRespRead ++ vfRespRead ++ memRespRead).flatten.map(_.vfWbBusyTable)

  private val allExuParams = params.allExuParams
  private val intAllBusyTableWithParms = intAllBusyTable.zip(allExuParams)
  private val vfAllBusyTableWithParms = vfAllBusyTable.zip(allExuParams)
  private val intAllDeqRespSetWithParms = intAllDeqRespSet.zip(allExuParams)
  private val vfAllDeqRespSetWithParms = vfAllDeqRespSet.zip(allExuParams)

  private val intWbLatencyMax = params.getIntWBExeGroup.map { case (portId, seq) => (portId, seq.map(_.intLatencyValMax).max, seq.forall(_.intLatencyCertain)) }
  private val vfWbLatencyMax = params.getVfWBExeGroup.map { case (portId, seq) => (portId, seq.map(_.vfLatencyValMax).max, seq.forall(_.vfLatencyCertain)) }
  private val intWbBundle = intWbLatencyMax.map { case (portId, latMax, latCertain) => (portId, OptionWrapper(latCertain, Wire(UInt((latMax + 1).W))), OptionWrapper(latCertain, Reg(Bool()))) }.toSeq
  private val vfWbBundle = vfWbLatencyMax.map { case (portId, latMax, latCertain) => (portId, OptionWrapper(latCertain, Wire(UInt((latMax + 1).W))), OptionWrapper(latCertain, Reg(Bool()))) }.toSeq

  def hitWbPort(source: Option[UInt], p: ExeUnitParams, portId: Int) = {
    p.wbPortConfigs.collectFirst { case x => x.port }.getOrElse(-1) == portId && source.nonEmpty
  }

  def writeWbBundle(wbBundle: Seq[(Int, Option[UInt], Option[Bool])], busyTableWithParams: IndexedSeq[(Option[UInt], ExeUnitParams)], deqRespSetWithParams: IndexedSeq[(Option[UInt], ExeUnitParams)]) = {
    wbBundle.map { case (portId, busyTable, conflict) =>
      if (busyTable.nonEmpty) {
        busyTable.get := busyTableWithParams.filter { case (busyTable, p) => hitWbPort(busyTable, p, portId) }.map(_._1.get).reduce(_ | _)
        conflict.get := deqRespSetWithParams.filter { case (deqRespSet, p) => hitWbPort(deqRespSet, p, portId) }.map(_._1.get).reduce(_ & _).orR
      }
    }
  }

  def readWbBundle[T <: Data](sink: IndexedSeq[Option[T]], wbBundle: Seq[(Int, Option[UInt], Option[Bool])]) = {
    for (i <- 0 until sink.size) {
      if(sink(i).nonEmpty) {
        sink(i).get := wbBundle.map { case (portId, busyTable, conflictFlag) =>
          val src = if (sink(i).get.isInstanceOf[UInt]) busyTable else conflictFlag
          if (hitWbPort(src, allExuParams(i), portId)) {
            src.get.asTypeOf(sink(i).get).asUInt
          } else {
            0.U.asTypeOf(sink(i).get).asUInt
          }
        }.reduce(_.asUInt | _.asUInt)
      }
    }
  }

  //per wbPort fuBusyTable and conflict gen
  writeWbBundle(intWbBundle, intAllBusyTableWithParms, intAllDeqRespSetWithParms)
  writeWbBundle(vfWbBundle, vfAllBusyTableWithParms, vfAllDeqRespSetWithParms)
  //read wbPort fuBusyTable to per exe
  readWbBundle(intAllRespRead, intWbBundle)
  readWbBundle(vfAllRespRead, vfWbBundle)
  //read wbPort conflict to dataPath
  readWbBundle(intAllWbConflictFlag, intWbBundle)
  readWbBundle(vfAllWbConflictFlag, vfWbBundle)

}

class WbFuBusyTableIO(implicit p: Parameters, params: BackendParams) extends XSBundle {
  val in = new Bundle {
    val intSchdBusyTable = MixedVec(params.intSchdParams.get.issueBlockParams.map(x => Input(x.genWbFuBusyTableWriteBundle)))
    val vfSchdBusyTable = MixedVec(params.vfSchdParams.get.issueBlockParams.map(x => Input(x.genWbFuBusyTableWriteBundle)))
    val memSchdBusyTable = MixedVec(params.memSchdParams.get.issueBlockParams.map(x => Input(x.genWbFuBusyTableWriteBundle)))
  }
  val out = new Bundle {
    val intRespRead = MixedVec(params.intSchdParams.get.issueBlockParams.map(x => Output(x.genWbFuBusyTableReadBundle)))
    val vfRespRead = MixedVec(params.vfSchdParams.get.issueBlockParams.map(x => Output(x.genWbFuBusyTableReadBundle)))
    val memRespRead = MixedVec(params.memSchdParams.get.issueBlockParams.map(x => Output(x.genWbFuBusyTableReadBundle)))
    val wbConflictRead = MixedVec(params.allSchdParams.map(x => MixedVec(x.issueBlockParams.map(x => Output(x.genWbConflictBundle())))))
  }
}