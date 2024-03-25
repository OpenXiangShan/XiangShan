package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles._
import xiangshan.backend.issue.EntryBundles.{EntryDeqRespBundle, RespType}
import xiangshan.backend.issue.VfScheduler


class Og2ForVector(params: BackendParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new Og2ForVectorIO(params))

  private val s1_validVec2        = io.fromOg1NoReg.map(_.map(_.valid))
  private val s1_dataVec2         = io.fromOg1NoReg.map(_.map(_.bits))
  private val s1_readyVec2        = io.fromOg1NoReg.map(_.map(_.ready))
  private val toVfExuFire         = io.toVfExu.map(_.map(_.fire))
  private val toVfExuReady        = io.toVfExu.map(_.map(_.ready))
  private val vfIQNum: Int        = io.fromOg1NoReg.size
  private val vfIQPerExuNum       = io.fromOg1NoReg.map(_.size).toSeq

  val s2_toVfExuValid             = Reg(MixedVec(
    s1_validVec2.map(x => MixedVec(x.map(_.cloneType).toSeq)).toSeq
  ))
  val s2_toVfExuData              = Reg(MixedVec(
    s1_dataVec2.map(x => MixedVec(x.map(_.cloneType).toSeq)).toSeq
  ))

    for(i <- 0 until vfIQNum) {
      for (j <- 0 until vfIQPerExuNum(i)) {
        val s2_flush = s1_dataVec2(i)(j).robIdx.needFlush(Seq(io.flush, RegNextWithEnable(io.flush)))
        val og2Failed = s2_toVfExuValid(i)(j) && !toVfExuFire(i)(j)
        val s1_ldCancel = LoadShouldCancel(s1_dataVec2(i)(j).loadDependency, io.ldCancel)
        when(s1_validVec2(i)(j) && s1_readyVec2(i)(j) && !s2_flush && !og2Failed && !s1_ldCancel) {
          s2_toVfExuValid(i)(j) := s1_validVec2(i)(j)
          s2_toVfExuData(i)(j) := s1_dataVec2(i)(j)
          s2_toVfExuData(i)(j).loadDependency.foreach(_ := s1_dataVec2(i)(j).loadDependency.get.map(_ << 1))
        }.otherwise {
          s2_toVfExuValid(i)(j) := false.B
        }
        s1_readyVec2(i)(j) := (toVfExuReady(i)(j) || !s1_validVec2(i)(j)) && !og2Failed && !s1_ldCancel
        io.toVfExu(i)(j).valid := s2_toVfExuValid(i)(j)
        io.toVfExu(i)(j).bits := s2_toVfExuData(i)(j)
      }
  }
  io.toVfIQ.zipWithIndex.foreach {
    case (toVfExu, iqId) =>
      toVfExu.zipWithIndex.foreach {
        case (og2Resp, exuId) =>
          val og2Failed = s2_toVfExuValid(iqId)(exuId) && !toVfExuFire(iqId)(exuId)
          og2Resp.valid := s2_toVfExuValid(iqId)(exuId)
          og2Resp.bits.robIdx := s2_toVfExuData(iqId)(exuId).robIdx
          og2Resp.bits.uopIdx.foreach(_ := s2_toVfExuData(iqId)(exuId).vpu.get.vuopIdx)
          og2Resp.bits.resp := Mux(og2Failed, RespType.block, RespType.success)
          og2Resp.bits.fuType := s2_toVfExuData(iqId)(exuId).fuType
      }
  }
}

class Og2ForVectorIO(params: BackendParams)(implicit p: Parameters) extends XSBundle {
  private val vfSchdParams = params.schdParams(VfScheduler())

  val flush: ValidIO[Redirect]                                    = Flipped(ValidIO(new Redirect))
  val ldCancel                                                    = Vec(backendParams.LduCnt + backendParams.HyuCnt, Flipped(new LoadCancelIO))
  val fromOg1NoReg: MixedVec[MixedVec[DecoupledIO[ExuInput]]]     = Flipped(vfSchdParams.genExuInputBundle)
  val toVfExu                                                     = MixedVec(vfSchdParams.genExuInputBundle)
  val toVfIQ                                                      = MixedVec(vfSchdParams.issueBlockParams.map(_.genOG2RespBundle))

}