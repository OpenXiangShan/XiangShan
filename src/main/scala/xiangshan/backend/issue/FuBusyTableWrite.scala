package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.MapUtils
import xiangshan._
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Utils

class FuBusyTableWrite(val idx: Int) (implicit p: Parameters, iqParams: IssueBlockParams) extends XSModule {
  private val latencyValMax: Int = iqParams.exuBlockParams(idx).latencyValMax
  private val tableSize = latencyValMax + 1
  private val fuLatencyMap: Map[Int, Int] = iqParams.exuBlockParams(idx).fuLatencyMap
  val io = IO(new FuBusyTableWriteIO(idx))

  val deqResp = io.in.deqResp(idx)
  val og0Resp = io.in.og0Resp(idx)
  val og1Resp = io.in.og1Resp(idx)
  val fuTypeVec = io.in.fuTypeRegVec

  // instance of FuBusyTable insists only when latencyValMax > 0
  private val fuBusyTable = RegInit(0.U(tableSize.W))

  private val fuBusyTableNext = Wire(UInt(tableSize.W))

  fuBusyTable := fuBusyTableNext

  /**
    * Map[latency, Set[fuType]]
    */
  private val latMappedFuTypeSet: Map[Int, Set[Int]] = MapUtils.groupByValueUnique(fuLatencyMap)

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
            fuType => Mux1H(resp.bits.addrOH, fuTypeVec) === fuType.U
          ).toSeq
        ).orR
    })
  }

  private val fuBusyTableShift = (fuBusyTable >> 1).asUInt
  private val deqRespSet = Mux(deqRespSuccess, deqRespMatchVec.asUInt >> 1, Utils.NZeros(tableSize))
  private val og0RespClear = Mux(og0RespFail, og0RespMatchVec.asUInt >> 2, Utils.NZeros(tableSize))
  private val og1RespClear = Mux(og1RespFail, og1RespMatchVec.asUInt >> 3, Utils.NZeros(tableSize))

  // Just for more readable verilog
  dontTouch(fuBusyTableShift)
  dontTouch(deqRespSet)
  dontTouch(og0RespClear)
  dontTouch(og1RespClear)

  fuBusyTableNext := fuBusyTableShift & (~og0RespClear).asUInt & (~og1RespClear).asUInt | deqRespSet.asUInt

  io.out.fuBusyTable := fuBusyTable
}

class FuBusyTableWriteIO(val idx: Int)(implicit p: Parameters, iqParams: IssueBlockParams) extends XSBundle {
  private val tableSize = iqParams.exuBlockParams(idx).latencyValMax + 1
  val in = new Bundle {
    val deqResp = Vec(iqParams.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
    val og0Resp = Vec(iqParams.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
    val og1Resp = Vec(iqParams.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
    val fuTypeRegVec = Input(Vec(iqParams.numEntries, FuType()))
  }
  val out = new Bundle {
    val fuBusyTable = Output(UInt(tableSize.W))
  }
}