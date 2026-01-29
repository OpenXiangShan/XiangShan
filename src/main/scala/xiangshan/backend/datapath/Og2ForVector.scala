package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles._
import xiangshan.backend.issue.EntryBundles.{IssueQueueRespBundle, RespType}
import xiangshan.backend.issue._
import xiangshan.mem.{SqPtr, LqPtr}


class Og2ForVector(params: BackendParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new Og2ForVectorIO(params))

  private val fromOg1             = io.fromOg1VfArith
  private val toExu               = io.toVfArithExu
  private val toIQOg2Resp         = io.toVfIQOg2Resp

  private val s1_validVec2        = fromOg1.map(_.map(_.valid))
  private val s1_dataVec2         = fromOg1.map(_.map(_.bits))
  private val s1_readyVec2        = fromOg1.map(_.map(_.ready))
  private val toExuFire           = toExu.map(_.map(_.fire))
  private val toExuReady          = toExu.map(_.map(_.ready))
  private val og2IQNum: Int       = fromOg1.size
  private val og2IQPerExuNum      = fromOg1.map(_.size).toSeq

  val s2_toExuValid               = Reg(MixedVec(
    s1_validVec2.map(x => MixedVec(x.map(_.cloneType).toSeq)).toSeq
  ))
  val s2_toExuData                = Reg(MixedVec(
    s1_dataVec2.map(x => MixedVec(x.map(_.cloneType).toSeq)).toSeq
  ))

  for(i <- 0 until og2IQNum) {
    for (j <- 0 until og2IQPerExuNum(i)) {
      val s2_flush = s1_dataVec2(i)(j).robIdx.needFlush(Seq(io.flush, RegNextWithEnable(io.flush)))
      val s1_ldCancel = LoadShouldCancel(s1_dataVec2(i)(j).loadDependency, io.ldCancel)
      when(s1_validVec2(i)(j) && s1_readyVec2(i)(j) && !s2_flush && !s1_ldCancel) {
        s2_toExuValid(i)(j) := true.B
        s2_toExuData(i)(j) := s1_dataVec2(i)(j)
        s2_toExuData(i)(j).loadDependency.foreach(_ := s1_dataVec2(i)(j).loadDependency.get.map(_ << 1))
      }.otherwise {
        s2_toExuValid(i)(j) := false.B
      }
      s1_readyVec2(i)(j) := true.B
      toExu(i)(j).valid := s2_toExuValid(i)(j)
      toExu(i)(j).bits := s2_toExuData(i)(j)
    }
  }
  toIQOg2Resp.zipWithIndex.foreach {
    case (toIQ, iqId) =>
      toIQ.zipWithIndex.foreach {
        case (og2Resp, exuId) =>
          val og2Failed = s2_toExuValid(iqId)(exuId) && !toExuReady(iqId)(exuId)
          // VecMem need oldest uop, otherwise s0 will be not ready
          og2Resp.failed := (if (og2Resp.params.isVecMemIQ) false.B else og2Failed)
          og2Resp.finalSuccess := (
            if (og2Resp.params match { case x => x.isVecMemIQ })
              false.B
            else
              s2_toExuValid(iqId)(exuId) && toExuReady(iqId)(exuId)
          )
          og2Resp.fuType := s2_toExuData(iqId)(exuId).fuType
          og2Resp.sqIdx.foreach(_ := 0.U.asTypeOf(new SqPtr))
          og2Resp.lqIdx.foreach(_ := 0.U.asTypeOf(new LqPtr))
      }
  }
}

class Og2ForVectorIO(params: BackendParams)(implicit p: Parameters) extends XSBundle {
  private val vecSchdParams = params.schdParams(VecScheduler())

  val flush: ValidIO[Redirect]                                    = Flipped(ValidIO(new Redirect))
  val ldCancel                                                    = Vec(backendParams.LduCnt + backendParams.HyuCnt, Flipped(new LoadCancelIO))

  val fromOg1VfArith: MixedVec[MixedVec[DecoupledIO[ExuInput]]]   = Flipped(vecSchdParams.genExuInputBundle)

  val toVfArithExu                                                = MixedVec(vecSchdParams.genExuInputBundle)
  val toVfIQOg2Resp                                               = MixedVec(vecSchdParams.issueBlockParams.map(_.genOG2RespBundle))
}