package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.FuType._
import xiangshan.backend.fu.FuConfig
import xiangshan.utils.ParallelOR

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
  def ExuCnt = IntExuCnt + FpExuCnt + LduCnt
  def NRFuType = 9
  def FuOpWidth = 7
}

abstract class Exu
(
  val supportedFuncUnits: Array[FuConfig],
  val enableBypass: Boolean
) extends XSModule {
  val io = IO(new ExuIO)
  io.dmem <> DontCare
  io.out.bits.debug.isMMIO := false.B

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

trait HasExeUnits{

  val jmpExeUnit = Module(new JmpExeUnit)
  val aluExeUnits = Array.tabulate(exuParameters.AluCnt)(_ => Module(new AluExeUnit))
  val mulExeUnits = Array.tabulate(exuParameters.MulCnt)(_ => Module(new MulExeUnit))
  val mduExeUnits = Array.tabulate(exuParameters.MduCnt)(_ => Module(new MulDivExeUnit))
//  val fmacExeUnits = Array.tabulate(exuConfig.FmacCnt)(_ => Module(new Fmac))
//  val fmiscExeUnits = Array.tabulate(exuConfig.FmiscCnt)(_ => Module(new Fmisc))
//  val fmiscDivSqrtExeUnits = Array.tabulate(exuConfig.FmiscDivSqrtCnt)(_ => Module(new FmiscDivSqrt))
  val lsuExeUnits = Array.tabulate(exuParameters.LduCnt)(_ => Module(new LsExeUnit))

  val exeUnits = jmpExeUnit +: (aluExeUnits ++ mulExeUnits ++ mduExeUnits ++ lsuExeUnits)

  exeUnits.foreach(_.io.dmem := DontCare)
  exeUnits.foreach(_.io.scommit := DontCare)
}
