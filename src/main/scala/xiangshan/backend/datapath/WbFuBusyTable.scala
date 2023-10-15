package xiangshan.backend.datapath

import scala.collection.Seq
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import utils.OptionWrapper
import xiangshan._
import xiangshan.backend._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.implicitCast._

class WbFuBusyTable(bp: BackendParams)(implicit  p: Parameters) extends LazyModule {
  override def shouldBeInlined: Boolean = false
  implicit val params: BackendParams = bp
  lazy val module = new WbFuBusyTableImp(this)
}

class WbFuBusyTableImp(override val wrapper: WbFuBusyTable)(implicit  p: Parameters, params: BackendParams) extends LazyModuleImp(wrapper) {
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
  private val intAllBusyTableWithParms = intAllBusyTable.zip(allExuParams).toSeq
  private val vfAllBusyTableWithParms = vfAllBusyTable.zip(allExuParams).toSeq
  private val intAllDeqRespSetWithParms = intAllDeqRespSet.zip(allExuParams).toSeq
  private val vfAllDeqRespSetWithParms = vfAllDeqRespSet.zip(allExuParams).toSeq

  private val intWbLatencyMax = params.getIntWBExeGroup.map { case (portId, seq) => (portId, seq.map(_.intLatencyValMax).max, seq.forall(_.intLatencyCertain)) }
  private val vfWbLatencyMax = params.getVfWBExeGroup.map { case (portId, seq) => (portId, seq.map(_.vfLatencyValMax).max, seq.forall(_.vfLatencyCertain)) }
  private val intWbBusyTable: Map[Int, Option[UInt]] = intWbLatencyMax.map { case (portId, latMax, latCertain) => (portId, OptionWrapper(latCertain, Wire(UInt((latMax + 1).W)))) }.toMap
  private val vfWbBusyTable = vfWbLatencyMax.map { case (portId, latMax, latCertain) => (portId, OptionWrapper(latCertain, Wire(UInt((latMax + 1).W)))) }.toMap
  private val intConflict: Map[Int, Option[Bool]] = intWbLatencyMax.map { case (portId, latMax, latCertain) => (portId, OptionWrapper(latCertain, Reg(Bool()))) }.toMap
  private val vfConflict = vfWbLatencyMax.map { case (portId, latMax, latCertain) => (portId, OptionWrapper(latCertain, Reg(Bool()))) }.toMap

  def hitWbPort[T <: Data](source: Option[T], p: ExeUnitParams, portId: Int, isInt: Boolean) = {
    if(isInt){
      p.wbPortConfigs.collectFirst { case x : IntWB => x.port }.getOrElse(-1) == portId && source.nonEmpty
    } else {
      p.wbPortConfigs.collectFirst { case x : VfWB => x.port }.getOrElse(-1) == portId && source.nonEmpty
    }
  }

  def writeBusyTable(wtBusyTable: Map[Int, Option[UInt]], busyTableWithParams: Seq[(Option[UInt], ExeUnitParams)], isInt: Boolean) = {
    wtBusyTable.foreach { case (portId, busyTable) =>
      if (busyTable.nonEmpty) {
        busyTable.get := busyTableWithParams.filter { case (busyTable, p) => hitWbPort(busyTable, p, portId, isInt) }.map(_._1.get).reduce(_ | _)
      }
    }
  }

  def writeConflict(wtConflict: Map[Int, Option[Bool]], deqRespSetWithParams: Seq[(Option[UInt], ExeUnitParams)], isInt: Boolean) = {
    wtConflict.foreach { case (portId, conflict) =>
      if (conflict.nonEmpty) {
        val deqRespSel = deqRespSetWithParams.filter { case (deqRespSet, p) => hitWbPort(deqRespSet, p, portId, isInt) }.map(_._1.get)
        val width = deqRespSel.map(x => x.getWidth).max
        val deqRespSelUnify = deqRespSel.map(x => x.asTypeOf(UInt(width.W))).toSeq
        conflict.get := (0 until width).map{ case i =>
          OnesMoreThan(deqRespSelUnify.map(x => x(i)), 2)
        }.reduce(_ | _)
      }
    }
  }

  def readRes[T <: Data](sink: IndexedSeq[Option[T]], source: Map[Int, Option[T]], isInt: Boolean) = {
    for(i <- 0 until sink.size) {
      if(sink(i).nonEmpty) {
        sink(i).get := source.map { case (portId, src) =>
          if(hitWbPort(src, allExuParams(i), portId, isInt)) {
            src.get.asTypeOf(sink(i).get).asUInt
          } else {
            0.U.asTypeOf(sink(i).get).asUInt
          }
        }.reduce(_ | _)
      }
    }
  }



  //per wbPort fuBusyTable
  writeBusyTable(intWbBusyTable, intAllBusyTableWithParms, true)
  writeBusyTable(vfWbBusyTable, vfAllBusyTableWithParms, false)
  //per wbPort conflict
  writeConflict(intConflict, intAllDeqRespSetWithParms, true)
  writeConflict(vfConflict, vfAllDeqRespSetWithParms, false)
  //read wbPort fuBusyTable to per exe
  readRes(intAllRespRead, intWbBusyTable, true)
  readRes(vfAllRespRead, vfWbBusyTable, false)
  //read wbPort conflict to dataPath
  readRes(intAllWbConflictFlag, intConflict, true)
  readRes(vfAllWbConflictFlag, vfConflict, false)

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