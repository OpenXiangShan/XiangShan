package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils.MapUtils
import xiangshan._
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Utils
import xiangshan.backend.issue.EntryBundles.RespType

class FuBusyTableWrite(fuLatencyMap: Map[FuType.OHType, Int]) (implicit p: Parameters, iqParams: IssueBlockParams) extends XSModule {
  private val latencyValMax: Int = fuLatencyMap.values.fold(0)(_ max _)
  private val tableSize = latencyValMax + 1
  val io = IO(new FuBusyTableWriteIO(latencyValMax))

  val deqResp = io.in.deqResp
  val og0Resp = io.in.og0Resp
  val og1Resp = io.in.og1Resp

  // instance of FuBusyTable insists only when latencyValMax > 0
  private val fuBusyTable = RegInit(0.U(tableSize.W))

  private val fuBusyTableNext = Wire(UInt(tableSize.W))

  fuBusyTable := fuBusyTableNext

  /**
    * Map[latency, Set[fuType]]
    */
  private val latMappedFuTypeSet: Map[Int, Set[FuType.OHType]] = MapUtils.groupByValueUnique(fuLatencyMap)

  private val deqRespSuccess = deqResp.valid
  private val og0RespFail = og0Resp.failed
  private val og1RespFail = og1Resp.failed

  private val deqRespMatchVec = getMatchVecFromResp(deqResp.bits)
  private val og0RespMatchVec = getMatchVecFromResp(og0Resp)
  private val og1RespMatchVec = getMatchVecFromResp(og1Resp)

  def getMatchVecFromResp(resp: IssueQueueDeqRespBundle) : Vec[Bool] = {
    VecInit((0 until tableSize).map {
      lat =>
        Cat(
          latMappedFuTypeSet.getOrElse(lat, Set()).toSeq.sorted.map(
            fuType => resp.fuType(fuType.id)
          ).toSeq
        ).orR
    })
  }

  private val fuBusyTableShift = (fuBusyTable >> 1).asUInt
  private val deqRespSetShift = Mux(deqRespSuccess, deqRespMatchVec.asUInt >> 1, Utils.NZeros(tableSize))
  private val og0RespClearShift = Mux(og0RespFail, og0RespMatchVec.asUInt >> 2, Utils.NZeros(tableSize))
  private val og1RespClearShift = Mux(og1RespFail, og1RespMatchVec.asUInt >> 3, Utils.NZeros(tableSize))

  fuBusyTableNext := fuBusyTableShift & (~og0RespClearShift).asUInt & (~og1RespClearShift).asUInt | deqRespSetShift.asUInt

  io.out.fuBusyTable := fuBusyTable
  io.out.deqRespSet := Mux(deqRespSuccess, deqRespMatchVec.asUInt, Utils.NZeros(tableSize)).asUInt
}

class FuBusyTableWriteIO(latencyValMax: Int)(implicit p: Parameters, iqParams: IssueBlockParams) extends XSBundle {
  private val tableSize = latencyValMax + 1
  val in = new Bundle {
    // TODO: change deqResp logic
    val deqResp =  Flipped(ValidIO(new IssueQueueDeqRespBundle))
    val og0Resp = Flipped(new IssueQueueDeqRespBundle)
    val og1Resp = Flipped(new IssueQueueDeqRespBundle)
  }
  val out = new Bundle {
    val fuBusyTable = Output(UInt(tableSize.W))
    val deqRespSet = Output(UInt(tableSize.W))
  }
}