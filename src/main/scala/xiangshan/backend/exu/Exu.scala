package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.FuType._
import xiangshan.backend.fu.FuConfig
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
  supportedFuncUnits: Array[FuConfig],
  enableBypass: Boolean
){
  def max(in: Seq[Int]): Int = in.reduce((x, y) => if(x > y) x else y)
  val intSrcCnt = max(supportedFuncUnits.map(_.numIntSrc))
  val fpSrcCnt = max(supportedFuncUnits.map(_.numFpSrc))
  val readIntRf = intSrcCnt > 0
  val readFpRf = fpSrcCnt > 0
  val writeIntRf = supportedFuncUnits.map(_.writeIntRf).reduce(_||_)
  val writeFpRf = supportedFuncUnits.map(_.writeFpRf).reduce(_||_)
  val hasRedirect = supportedFuncUnits.map(_.hasRedirect).reduce(_||_)

  def canAccept(fuType: UInt): Bool = {
    ParallelOR(supportedFuncUnits.map(_.fuType === fuType)).asBool()
  }
}

abstract class Exu(val config: ExuConfig) extends XSModule {
  val io = IO(new ExuIO)
  io.dmem <> DontCare
  io.out.bits.brUpdate <> DontCare
  io.out.bits.debug.isMMIO := false.B
}

object Exu {
  val jmpExeUnitCfg = ExuConfig("JmpExu", Array(jmpCfg, i2fCfg, csrCfg), enableBypass = false)
  val aluExeUnitCfg = ExuConfig("AluExu", Array(aluCfg), enableBypass = true)
  val mulExeUnitCfg = ExuConfig("MulExu", Array(mulCfg), enableBypass = false)
  val divExeUnitCfg = ExuConfig("DivExu",Array(divCfg), enableBypass = false)
  val mulDivExeUnitCfg = ExuConfig("MulDivExu", Array(mulCfg, divCfg), enableBypass = false)
  val ldExeUnitCfg = ExuConfig("LoadExu", Array(lduCfg), enableBypass = false)
  val stExeUnitCfg =ExuConfig("StoreExu", Array(stuCfg), enableBypass = false)
  val fmacExeUnitCfg = ExuConfig("FmacExu", Array(fmacCfg), enableBypass = false)
  val fmiscExeUnitCfg = ExuConfig("FmiscExu", Array(fmiscCfg), enableBypass = false)
  val fmiscDivExeUnitCfg = ExuConfig("FmiscDivExu", Array(fmiscCfg, fDivSqrtCfg), enableBypass = false)
}
