package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.FuType._
import xiangshan.backend.fu.{CertainLatency, FuConfig, FuOutput, FunctionUnit, HasFuLatency, UncertainLatency}
import utils.ParallelOR
import xiangshan.backend.fu.FunctionUnit._

case class ExuParameters
(
  JmpCnt: Int,
  AluCnt: Int,
  MulCnt: Int,
  MduCnt: Int,
  FmacCnt: Int,
  FmiscCnt: Int,
  FmiscDivSqrtCnt: Int,
  LduCnt: Int,
  StuCnt: Int
){
  assert(JmpCnt == 1, "Only support 1 JmpUnit now!")
  def IntExuCnt = AluCnt + MulCnt + MduCnt + JmpCnt
  def FpExuCnt = FmacCnt + FmiscCnt + FmiscDivSqrtCnt
  def LsExuCnt = LduCnt + StuCnt
  def ExuCnt = IntExuCnt + FpExuCnt + LduCnt + StuCnt
  def NRFuType = 9
  def FuOpWidth = 7
}

case class ExuConfig
(
  name: String,
  supportedFuncUnits: Seq[FuConfig],
  wbIntPriority: Int,
  wbFpPriority: Int
){
  def max(in: Seq[Int]): Int = in.reduce((x, y) => if(x > y) x else y)
  val intSrcCnt = max(supportedFuncUnits.map(_.numIntSrc))
  val fpSrcCnt = max(supportedFuncUnits.map(_.numFpSrc))
  val readIntRf = intSrcCnt > 0
  val readFpRf = fpSrcCnt > 0
  val writeIntRf = supportedFuncUnits.map(_.writeIntRf).reduce(_||_)
  val writeFpRf = supportedFuncUnits.map(_.writeFpRf).reduce(_||_)
  val hasRedirect = supportedFuncUnits.map(_.hasRedirect).reduce(_||_)

  val latency: HasFuLatency = {
    val lats = supportedFuncUnits.map(_.latency)
    if(lats.exists(x => x.latencyVal.isEmpty)){
      UncertainLatency()
    } else {
      val x = lats.head
      for(l <- lats.drop(1)){
        require(x.latencyVal.get == l.latencyVal.get)
      }
      x
    }
  }
  val hasCertainLatency = latency.latencyVal.nonEmpty
  val hasUncertainlatency = latency.latencyVal.isEmpty

  def canAccept(fuType: UInt): Bool = {
    Cat(supportedFuncUnits.map(_.fuType === fuType)).orR()
  }
}

abstract class Exu[T <: FunctionUnit]
(
  val exuName: String,
  val fuGen: Seq[(() => T, T => Bool)],
  val wbIntPriority: Int,
  val wbFpPriority: Int
) extends XSModule {

  val io = IO(new ExuIO)

  val src1 = io.in.bits.src1
  val src2 = io.in.bits.src2
  val src3 = io.in.bits.src3
  val func = io.in.bits.uop.ctrl.fuOpType

  val supportedFunctionUnits = fuGen.map(_._1).map(gen => Module(gen()))

  val fuSel = supportedFunctionUnits.zip(fuGen.map(_._2)).map(x => x._2(x._1))

  def fuConfigs = supportedFunctionUnits.map(_.cfg)

  def config: ExuConfig = {
    ExuConfig(exuName, fuConfigs, wbIntPriority, wbFpPriority)
  }

  require(fuGen.nonEmpty)
  require(fuSel.size == fuGen.length)

  if(fuSel == null){
    println("fu sel is null")
  }
  if(supportedFunctionUnits == null){
    println("supported fu is null")
  }
  for((fu, sel) <- supportedFunctionUnits.zip(fuSel)){
    if(fu == null) println("aaa")
    if(sel == null) println("bbb")
    fu.io.in.valid := io.in.valid && sel
    fu.io.in.bits.uop := io.in.bits.uop
    if(fu.cfg.srcCnt > 0){
      fu.io.in.bits.src(0) := src1
    }
    if(fu.cfg.srcCnt > 1){
      fu.io.in.bits.src(1) := src2
    }
    if(fu.cfg.srcCnt > 2){
      fu.io.in.bits.src(2) := src3
    }
    fu.io.redirectIn := io.redirect
  }

  val outputArb = if(config.latency.latencyVal.nonEmpty && (config.latency.latencyVal.get == 0)){
    // do not need an arbiter
    println(config.name)
    io.in.ready := Cat(supportedFunctionUnits.map(_.io.in.ready)).andR()
    for(fu <- supportedFunctionUnits){
      fu.io.out.ready := io.out.ready
    }
    val out = Mux1H(supportedFunctionUnits.map(x => x.io.out.valid -> x.io.out))
    io.out.bits.data := out.bits.data
    io.out.bits.uop := out.bits.uop
    io.out.valid := out.valid
    None
  } else {
    io.in.ready := (if(supportedFunctionUnits.length > 1) {
      Cat(
        fuSel.zip(supportedFunctionUnits).map(x => x._1 && x._2.io.in.ready)
      ).orR()
    } else {
      supportedFunctionUnits.head.io.in.ready
    })
    val outputArb = Module(new Arbiter(new FuOutput, supportedFunctionUnits.length))
    outputArb.io.in <> VecInit(supportedFunctionUnits.map(_.io.out))
    io.out.bits.data := outputArb.io.out.bits.data
    io.out.bits.uop := outputArb.io.out.bits.uop
    io.out.valid := outputArb.io.out.valid
    outputArb.io.out.ready := io.out.ready
    Some(outputArb)
  }

  io.out.bits.brUpdate <> DontCare
  io.out.bits.fflags <> DontCare
  io.out.bits.debug.isMMIO := false.B
  io.out.bits.debug <> DontCare
  io.out.bits.redirect <> DontCare
  io.out.bits.redirectValid := false.B
  io.csrOnly <> DontCare
}

//object Exu {
//  val jmpExeUnitCfg = ExuConfig("JmpExu", Array(jmpCfg, i2fCfg, csrCfg, fenceCfg))
//  val aluExeUnitCfg = ExuConfig("AluExu", Array(aluCfg))
//  val mulExeUnitCfg = ExuConfig("MulExu", Array(mulCfg))
//  val divExeUnitCfg = ExuConfig("DivExu", Array(divCfg))
//  val fenceExeUnitCfg = ExuConfig("FenceCfg", Array(fenceCfg))
//  val i2fExeUnitCfg = ExuConfig("I2fExu", Array(i2fCfg))
//  val mulDivExeUnitCfg = ExuConfig("MulDivExu", Array(mulCfg, divCfg))
//  val mulDivFenceExeUnitCfg = ExuConfig("MulDivFenceExu", Array(mulCfg, divCfg, fenceCfg))
//  val ldExeUnitCfg = ExuConfig("LoadExu", Seq(lduCfg), requestFastWriteBack = true, uniqueInArbiter = false)
//  val stExeUnitCfg = ExuConfig("StoreExu", Seq(stuCfg, mouCfg), requestFastWriteBack = false, uniqueInArbiter = false)
//  val fmacExeUnitCfg = ExuConfig("FmacExu", Array(fmacCfg))
//  val fmiscExeUnitCfg = ExuConfig("FmiscExu", Array(fmiscCfg))
//  val fmiscDivExeUnitCfg = ExuConfig("FmiscDivExu", Array(fmiscCfg, fDivSqrtCfg))
//}