package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.MapUtils
import xiangshan._
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Utils

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

  private val deqRespSuccess = deqResp.valid && deqResp.bits.respType === RSFeedbackType.issueSuccess
  private val og0RespFail = og0Resp.valid && og0Resp.bits.respType === RSFeedbackType.rfArbitFail
  private val og1RespFail = og1Resp.valid && og1Resp.bits.respType === RSFeedbackType.fuBusy

  private val deqRespMatchVec = getMatchVecFromResp(deqResp)
  private val og0RespMatchVec = getMatchVecFromResp(og0Resp)
  private val og1RespMatchVec = getMatchVecFromResp(og1Resp)

  def getMatchVecFromResp(resp: Valid[IssueQueueDeqRespBundle]) : Vec[Bool] = {
    VecInit((0 until tableSize).map {
      lat =>
        Cat(
          latMappedFuTypeSet.getOrElse(lat, Set()).map(
            fuType => resp.bits.fuType === fuType.U
          ).toSeq
        ).orR
    })
  }

  private val fuBusyTableShift = (fuBusyTable >> 1).asUInt
  private val deqRespSetShift = Mux(deqRespSuccess, deqRespMatchVec.asUInt >> 1, Utils.NZeros(tableSize))
  private val og0RespClearShift = Mux(og0RespFail, og0RespMatchVec.asUInt >> 2, Utils.NZeros(tableSize))
  private val og1RespClearShift = Mux(og1RespFail, og1RespMatchVec.asUInt >> 3, Utils.NZeros(tableSize))

  // Just for more readable verilog
  dontTouch(fuBusyTableShift)
  dontTouch(deqRespSetShift)
  dontTouch(og0RespClearShift)
  dontTouch(og1RespClearShift)

  fuBusyTableNext := fuBusyTableShift & (~og0RespClearShift).asUInt & (~og1RespClearShift).asUInt | deqRespSetShift.asUInt

  io.out.fuBusyTable := fuBusyTable
  io.out.deqRespSet := Mux(deqRespSuccess, deqRespMatchVec.asUInt, Utils.NZeros(tableSize)).asUInt
}

class FuBusyTableWriteIO(latencyValMax: Int)(implicit p: Parameters, iqParams: IssueBlockParams) extends XSBundle {
  private val tableSize = latencyValMax + 1
  val in = new Bundle {
    val deqResp =  Flipped(ValidIO(new IssueQueueDeqRespBundle))
    val og0Resp = Flipped(ValidIO(new IssueQueueDeqRespBundle))
    val og1Resp = Flipped(ValidIO(new IssueQueueDeqRespBundle))
  }
  val out = new Bundle {
    val fuBusyTable = Output(UInt(tableSize.W))
    val deqRespSet = Output(UInt(tableSize.W))
  }
}