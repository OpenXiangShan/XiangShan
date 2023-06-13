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

class FuBusyTableWrite(val idx: Int) (implicit  p: Parameters, iqParams: IssueBlockParams) extends XSModule {
  val io = IO(new FuBusyTableWriteIO(idx))

  val fuLatencyMap: Option[Seq[(Int, Int)]] = iqParams.exuBlockParams(idx).fuLatencyMap
  val latencyValMax: Option[Int] = iqParams.exuBlockParams(idx).latencyValMax

  val deqResp = io.in.deqResp
  val og0Resp = io.in.og0Resp
  val og1Resp = io.in.og1Resp
  val fuTypeRegVec = io.in.fuTypeRegVec

  val fuBusyTable = Reg(UInt(latencyValMax.get.W)) //instance of FuBusyTable insists only when latencyValMax > 0

  // fuBusyTable write
  val isLatencyNumVecDeq = Mux(deqResp(idx).valid && deqResp(idx).bits.respType === RSFeedbackType.issueSuccess,
    Cat((0 until latencyValMax.get).map { case num =>
      val latencyNumFuType = fuLatencyMap.get.filter(_._2 == num + 1).map(_._1) // futype with latency equal to num+1
      val isLatencyNum = Cat(latencyNumFuType.map(futype => fuTypeRegVec(OHToUInt(deqResp(idx).bits.addrOH)) === futype.U)).asUInt.orR // The latency of the deq inst is Num
      isLatencyNum
    }),
    0.U
  ) // |  when N cycle is 2 latency, N+1 cycle could not 1 latency
  val isLatencyNumVecOg0 = WireInit(~(0.U.asTypeOf(isLatencyNumVecDeq)))
  isLatencyNumVecOg0 := Mux(og0Resp(idx).valid && (og0Resp(idx).bits.respType === RSFeedbackType.rfArbitFail || og0Resp(idx).bits.respType === RSFeedbackType.fuBusy),
    ~(Cat(Cat((0 until latencyValMax.get).map { case num =>
      val latencyNumFuType = fuLatencyMap.get.filter(_._2 == num + 1).map(_._1) // futype with latency equal to num+1
      val isLatencyNum = Cat(latencyNumFuType.map(futype => fuTypeRegVec(OHToUInt(og0Resp(idx).bits.addrOH)) === futype.U)).asUInt.orR // The latency of the deq inst is Num
      isLatencyNum
    }), 0.U(1.W))),
    ~(0.U.asTypeOf(isLatencyNumVecDeq))
  )
  val isLatencyNumVecOg1 = WireInit(~(0.U.asTypeOf(isLatencyNumVecDeq)))
  isLatencyNumVecOg1 := Mux(og1Resp(idx).valid && og1Resp(idx).bits.respType === RSFeedbackType.fuBusy,
    ~(Cat(Cat((0 until latencyValMax.get).map { case num =>
      val latencyNumFuType = fuLatencyMap.get.filter(_._2 == num + 1).map(_._1) // futype with latency equal to num+1
      val isLatencyNum = Cat(latencyNumFuType.map(futype => fuTypeRegVec(OHToUInt(og1Resp(idx).bits.addrOH)) === futype.U)).asUInt.orR // The latency of the deq inst is Num
      isLatencyNum
    }), 0.U(2.W))),
    ~(0.U.asTypeOf(isLatencyNumVecDeq))
  )

  fuBusyTable := ((fuBusyTable << 1.U).asUInt & isLatencyNumVecOg0.asUInt & isLatencyNumVecOg1.asUInt) | isLatencyNumVecDeq

  io.out.fuBusyTable := fuBusyTable

}

class FuBusyTableWriteIO(val idx: Int)(implicit p: Parameters, iqParams: IssueBlockParams) extends XSBundle {
  val in = new Bundle {
    val deqResp = Vec(iqParams.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
    val og0Resp = Vec(iqParams.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
    val og1Resp = Vec(iqParams.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
    val fuTypeRegVec = Input(Vec(iqParams.numEntries, FuType()))
  }
  val out = new Bundle {
    val fuBusyTable = Output(UInt(iqParams.exuBlockParams(idx).latencyValMax.get.W))
  }
}