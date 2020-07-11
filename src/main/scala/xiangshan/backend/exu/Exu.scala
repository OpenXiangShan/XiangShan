package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.FuType._
import xiangshan.backend.fu.FuConfig
import xiangshan.utils.ParallelOR
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
  io.out.bits.debug.isMMIO := false.B
}

object Exu {
  val jmpExeUnitCfg = ExuConfig("JmpExu", Array(jmpCfg, i2fCfg), enableBypass = false)
  val aluExeUnitCfg = ExuConfig("AluExu", Array(aluCfg), enableBypass = false)
  val mulExeUnitCfg = ExuConfig("MulExu", Array(mulCfg), enableBypass = false)
  val divExeUnitCfg = ExuConfig("DivExu",Array(divCfg), enableBypass = false)
  val mulDivExeUnitCfg = ExuConfig("MulDivExu", Array(mulCfg, divCfg), enableBypass = false)
  val lsuExeUnitCfg = ExuConfig("LsExu", Array(lsuCfg), enableBypass = false)
}

trait HasExeUnits{

  val aluExeUnits = Array.tabulate(exuParameters.AluCnt)(_ => Module(new AluExeUnit))
  val jmpExeUnit = Module(new JmpExeUnit)
  val mulExeUnits = Array.tabulate(exuParameters.MulCnt)(_ => Module(new MulExeUnit))
  val mduExeUnits = Array.tabulate(exuParameters.MduCnt)(_ => Module(new MulDivExeUnit))
//  val fmacExeUnits = Array.tabulate(exuParameters.FmacCnt)(_ => Module(new Fmac))
//  val fmiscExeUnits = Array.tabulate(exuParameters.FmiscCnt)(_ => Module(new Fmisc))
//  val fmiscDivSqrtExeUnits = Array.tabulate(exuParameters.FmiscDivSqrtCnt)(_ => Module(new FmiscDivSqrt))
  val lsuExeUnits = Array.tabulate(exuParameters.StuCnt)(_ => Module(new LsExeUnit))

  val exeUnits = jmpExeUnit +: (aluExeUnits ++ mulExeUnits ++ mduExeUnits ++ lsuExeUnits)

  exeUnits.foreach(_.io.dmem := DontCare)
  exeUnits.foreach(_.io.scommit := DontCare)
}
