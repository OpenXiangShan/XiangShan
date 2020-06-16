package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.regfile.RfWritePort

case class ExuConfig
(
  AluCnt: Int,
  BruCnt: Int,
  MulCnt: Int,
  MduCnt: Int,
  FmacCnt: Int,
  FmiscCnt: Int,
  FmiscDivSqrtCnt: Int,
  LsuCnt: Int
){
  assert(BruCnt == 1, "Only support 1 Bru now!")
  def IntExuCnt = AluCnt + MulCnt + MduCnt + BruCnt
  def FpExuCnt = FmacCnt + FmiscCnt + FmiscDivSqrtCnt
  def ExuCnt = IntExuCnt + FpExuCnt + LsuCnt
}

abstract class Exu extends Module {
  val io = IO(new ExuIO)
}

class Alu extends Exu with NeedImpl
class Bru extends Exu with NeedImpl
class Mul extends Exu with NeedImpl
class Mdu extends Exu with NeedImpl
class Fmac extends Exu with NeedImpl
class Fmisc extends Exu with NeedImpl
class FmiscDivSqrt extends Exu with NeedImpl
class Lsu extends Exu with NeedImpl

class ExeUnits extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val roqCommits = Vec(CommitWidth, Flipped(ValidIO(new RoqCommit)))
    val redirect = Flipped(ValidIO(new Redirect))
    val in = Vec(exuConfig.ExuCnt, Flipped(DecoupledIO(new ExuInput)))
    val wbReqs = Vec(exuConfig.ExuCnt, DecoupledIO(new ExuOutput))
    val wbResults = Vec(exuConfig.ExuCnt, ValidIO(new ExuOutput))
  })
}

// TODO: refactor exu io logic, this is ugly...
trait HasExuHelper extends HasXSParameter {
  implicit class ExuHelper[T <: Data](xs: Vec[T]){
    private val bruIdx = 0
    private val aluIdx = bruIdx + 1
    private val mulIdx = aluIdx + exuConfig.AluCnt
    private val mduIdx = mulIdx + exuConfig.MulCnt
    private val fmacIdx = mduIdx + exuConfig.MduCnt
    private val fmiscIdx = fmacIdx + exuConfig.FmacCnt
    private val fmiscDivSqrtIdx = fmiscIdx + exuConfig.FmiscDivSqrtCnt
    private val lsuIdx = fmiscDivSqrtIdx + exuConfig.LsuCnt

    def getBru: T = {
      xs(bruIdx)
    }
    def getAluVec: Vec[T] = {
      VecInit(xs.slice(aluIdx, mulIdx))
    }
    def getMulVec: Vec[T] = {
      VecInit(xs.slice(mulIdx, mduIdx))
    }
    def getMduVec: Vec[T] = {
      VecInit(xs.slice(mduIdx, fmacIdx))
    }
    def getFmacVec: Vec[T] = {
      VecInit(xs.slice(fmacIdx, fmiscIdx))
    }
    def getFmiscVec: Vec[T] = {
      VecInit(xs.slice(fmiscIdx, fmiscDivSqrtIdx))
    }
    def getFmiscDivSqrtVec: Vec[T] = {
      VecInit(xs.slice(fmiscDivSqrtIdx, lsuIdx))
    }
    def getLsuVec: Vec[T] = {
      VecInit(xs.drop(lsuIdx))
    }
  }
}

class WriteBackArbMtoN(m: Int, n: Int) extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val in = Vec(m, Flipped(DecoupledIO(new ExuOutput)))
    val out = Vec(n, Flipped(new RfWritePort))
  })
}




