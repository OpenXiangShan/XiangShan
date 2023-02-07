package xiangshan.v2backend.dispatch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan._
import utils._
import xiangshan.backend.rename.BusyTableReadIO
import xiangshan.v2backend.Bundles.DynInst
import xiangshan.v2backend.{AluCfg, FuConfig, SchdBlockParams}

class Dispatch2Iq(val schdBlockParams : SchdBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val issueBlockParams = schdBlockParams.issueBlockParams

  val numIn = schdBlockParams.numUopIn
  require(issueBlockParams.size > 0 && issueBlockParams.forall(_.numEnq == issueBlockParams.head.numEnq), "issueBlock is null or the enq size of all issueBlock not be the same all\n")
  val numOut = issueBlockParams.head.numEnq
  val numIntSrc = issueBlockParams.map(_.exuBlockParams.map(_.numIntSrc).max)

  val numIntStateRead = numIntSrc.max * numIn

  lazy val module = new Dispatch2IqImp(this)(p)
}

class Dispatch2IqImp(outer: Dispatch2Iq)(implicit p: Parameters) extends LazyModuleImp(outer) with HasXSParameter {
  val numIntSrc = outer.numIntSrc.max
  val numIntStateRead = outer.numIntStateRead


  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val in = Flipped(Vec(outer.numIn, DecoupledIO(new DynInst)))
    val readIntState = if (numIntStateRead > 0) Some(Vec(numIntStateRead, Flipped(new BusyTableReadIO))) else None
    val out = Vec(outer.issueBlockParams.size, Vec(outer.numOut, DecoupledIO(new DynInst)))
  })

  val uopsIn = Wire(Vec(outer.numIn, DecoupledIO(new DynInst)))
  uopsIn <> io.in
  uopsIn.map(_.ready := false.B)

  // We always read physical register states when in gives the instructions.
  // This usually brings better timing.
  if (io.readIntState.isDefined) {
    val req = io.in.flatMap(in => in.bits.psrc.take(numIntSrc))
    io.readIntState.get.map(_.req).zip(req).foreach(x => x._1 := x._2)
  }


  // srcState is read from outside and connected directly
  if (io.readIntState.isDefined) {
    val intSrcStateVec = uopsIn.flatMap(_.bits.srcState.take(numIntSrc))
    io.readIntState.get.map(_.resp).zip(intSrcStateVec).foreach(x => x._2 := x._1)
  }

  // find number of IssueBlock which include Alu
  val aluIssueBlockIdx = outer.issueBlockParams.zipWithIndex.filter{case (issueBlockParam, idx) => issueBlockParam.getFuCfgs.contains(AluCfg)}.map(_._2).zipWithIndex.toMap
  val aluStep = aluIssueBlockIdx.size

  def filterCanAccept(fuConfigs: Seq[FuConfig], fuType: UInt, canAcceptAlu: Boolean): Bool = {
    if(canAcceptAlu) {
      Cat(fuConfigs.map(_.fuType.U === fuType)).orR
    }
    else{
      Mux(fuType === FuType.alu, false.B, Cat(fuConfigs.map(_.fuType.U === fuType)).orR)
    }
  }

  // arbiter
  for(((outs, issueBlockParam), blockIdx) <- io.out.zip(outer.issueBlockParams).zipWithIndex){
    if(aluIssueBlockIdx.get(blockIdx) != None){
      val startIdx = aluIssueBlockIdx.get(blockIdx).get
      val aluPosSeq = (startIdx until outer.numIn by aluStep)

      val canAccept = uopsIn.zipWithIndex.map{ case(in, idx) => filterCanAccept(issueBlockParam.getFuCfgs, in.bits.fuType, aluPosSeq.contains(idx))}
      outs.zipWithIndex.map{case (out, idx) => out <> uopsIn(PopCount(canAccept.take(idx)))}
    }
    else{

      val canAccept = uopsIn.map(x => issueBlockParam.canAccept(x.bits.fuType))
      outs.zipWithIndex.map{case (out, idx) => out <> uopsIn(PopCount(canAccept.take(idx)))}
    }
  }




  XSPerfAccumulate("in_valid", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("in_fire", PopCount(io.in.map(_.fire)))
//  XSPerfAccumulate("out_valid", PopCount(io.out.map(_.valid)))
//  XSPerfAccumulate("out_fire", PopCount(io.out.map(_.fire)))
}