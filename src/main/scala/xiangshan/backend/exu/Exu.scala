package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.FuType._

case class ExuConfig
(
  BruCnt: Int,
  AluCnt: Int,
  MulCnt: Int,
  MduCnt: Int,
  FmacCnt: Int,
  FmiscCnt: Int,
  FmiscDivSqrtCnt: Int,
  LduCnt: Int,
  StuCnt: Int
){
  assert(BruCnt == 1, "Only support 1 Bru now!")
  def IntExuCnt = AluCnt + MulCnt + MduCnt + BruCnt
  def FpExuCnt = FmacCnt + FmiscCnt + FmiscDivSqrtCnt
  def ExuCnt = IntExuCnt + FpExuCnt + LduCnt
  def NRFuType = 9
  def FuOpWidth = 7
}

abstract class Exu
(
  val fuTypeInt: BigInt,
  val readIntRf: Boolean = true,
  val readFpRf: Boolean = false,
  val writeIntRf: Boolean = true,
  val writeFpRf: Boolean = false
) extends Module {
  val io = IO(new ExuIO)
}



class Alu extends Exu(alu.litValue()) with NeedImpl {
  override def toString: String = "Alu"
}

class Bru extends Exu(FuType.bru.litValue(), writeFpRf = true) with NeedImpl{
  override def toString: String = "Bru"
}

class Mul extends Exu(FuType.mul.litValue()) with NeedImpl{
  override def toString: String = "Mul"
}

class Mdu extends Exu(FuType.mdu.litValue()) with NeedImpl{
  override def toString: String = "MulDiv"
}

class Fmac extends Exu(
  FuType.fmac.litValue(),
  readIntRf = false,
  readFpRf = true,
  writeIntRf = false,
  writeFpRf = true
) with NeedImpl {
  override def toString: String = "Fmac"
}

class Fmisc extends Exu(
  FuType.fmisc.litValue(),
  readIntRf = false,
  readFpRf = true,
  writeIntRf = true,
  writeFpRf = true
) with NeedImpl {
  override def toString: String = "Fmisc"
}

class FmiscDivSqrt extends Exu(
  FuType.fmiscDivSqrt.litValue(),
  readIntRf = false,
  readFpRf = true,
  writeIntRf = false,
  writeFpRf = true
) with NeedImpl {
  override def toString: String = "FmiscDivSqrt"
}

class Lsu extends Exu(
  FuType.ldu.litValue(),
  readIntRf = true,
  readFpRf = true,
  writeIntRf = true,
  writeFpRf = true
) with NeedImpl {
  override def toString: String = "Lsu"
}

trait HasExeUnits{

  val aluExeUnits = Array.tabulate(exuConfig.AluCnt)(_ => Module(new Alu))
  val bruExeUnit = Module(new Bru)
  val mulExeUnits = Array.tabulate(exuConfig.MulCnt)(_ => Module(new Mul))
  val mduExeUnits = Array.tabulate(exuConfig.MduCnt)(_ => Module(new Mdu))
  val fmacExeUnits = Array.tabulate(exuConfig.FmacCnt)(_ => Module(new Fmac))
  val fmiscExeUnits = Array.tabulate(exuConfig.FmiscCnt)(_ => Module(new Fmisc))
  val fmiscDivSqrtExeUnits = Array.tabulate(exuConfig.FmiscDivSqrtCnt)(_ => Module(new FmiscDivSqrt))
  val lsuExeUnits = Array.tabulate(exuConfig.LduCnt)(_ => Module(new Lsu))

  val exeUnits = bruExeUnit +: (aluExeUnits ++ mulExeUnits ++ mduExeUnits ++
    fmacExeUnits ++ fmiscExeUnits ++ fmiscDivSqrtExeUnits ++ lsuExeUnits)
}

class WriteBackArbMtoN(m: Int, n: Int) extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val in = Vec(m, Flipped(DecoupledIO(new ExuOutput)))
    val out = Vec(n, ValidIO(new ExuOutput))
  })
}




