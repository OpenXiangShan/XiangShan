package xiangshan.backend.fu

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util.BitPat
import utils.EnumUtils.OHEnumeration

import scala.language.implicitConversions

object FuType extends OHEnumeration {
  class OHType(i: Int, name: String) extends super.OHVal(i: Int, name: String)

  def OHType(i: Int, name: String): OHType = new OHType(i, name)

  implicit class fromOHValToLiteral(x: OHType) {
    def U: UInt = x.ohid.U
    def U(width: Width): UInt = x.ohid.U(width)
  }

  private var initVal = 0

  private def addType(name: String): OHType = {
    val ohval = OHType(initVal, name)
    initVal += 1
    ohval
  }

  // int
  val jmp = addType(name = "jmp")
  val brh = addType(name = "brh")
  val i2f = addType(name = "i2f")
  val i2v = addType(name = "i2v")
  val csr = addType(name = "csr")
  val alu = addType(name = "alu")
  val mul = addType(name = "mul")
  val div = addType(name = "div")
  val fence = addType(name = "fence")
  val bku = addType(name = "bku")

  // fp
  val fmac = addType(name = "fmac")
  val fmisc = addType(name = "fmisc")
  val fDivSqrt = addType(name = "fDivSqrt")

  // ls
  val ldu = addType(name = "ldu")
  val stu = addType(name = "stu")
  val mou = addType(name = "mou")

  // vec
  val vipu = addType(name = "vipu")
  val vialuF = addType(name = "vialuF")
  val vppu = addType(name = "vppu")
  val vimac = addType(name = "vimac")
  val vfpu = addType(name = "vfpu") // will be deleted
  val vfalu = addType(name = "vfalu")
  val vfma = addType(name = "vfma")
  val vfdiv = addType(name = "vfdiv")
  val vfcvt = addType(name = "vfcvt")
  val vsetiwi = addType(name = "vsetiwi") // vset read rs write rd
  val vsetiwf = addType(name = "vsetiwf") // vset read rs write vconfig
  val vsetfwf = addType(name = "vsetfwf") // vset read old vl write vconfig

  // vec ls
  val vldu = addType(name = "vldu")
  val vstu = addType(name = "vstu")

  val intArithAll = Seq(jmp, brh, i2f, i2v, csr, alu, mul, div, fence, bku)
  val fpArithAll = Seq(fmac, fmisc, fDivSqrt)
  val scalaMemAll = Seq(ldu, stu, mou)
  val vecOPI = Seq(vipu, vialuF, vppu, vimac)
  val vecOPF = Seq(vfpu, vfalu, vfma, vfdiv, vfcvt)
  val vecVSET = Seq(vsetiwi, vsetiwf, vsetfwf)
  val vecArith = vecOPI ++ vecOPF
  val vecMem = Seq(vldu, vstu)
  val vecArithOrMem = vecArith ++ vecMem
  val vecAll = vecVSET ++ vecMem

  def X = BitPat.N(num) // Todo: Don't Care

  def num = this.values.size

  def width = num

  def apply() = UInt(num.W)

  def isInt(fuType: UInt): Bool = FuTypeOrR(fuType, intArithAll) || FuTypeOrR(fuType, vsetiwi, vsetiwf)

  def isVset(fuType: UInt): Bool = FuTypeOrR(fuType, vecVSET)

  def isJump(fuType: UInt): Bool = FuTypeOrR(fuType, jmp)

  def isFArith(fuType: UInt): Bool = FuTypeOrR(fuType, fpArithAll)

  def isMem(fuType: UInt): Bool = FuTypeOrR(fuType, scalaMemAll)

  def isLoadStore(fuType: UInt): Bool = FuTypeOrR(fuType, ldu, stu)

  def isLoad(fuType: UInt): Bool = FuTypeOrR(fuType, ldu)

  def isStore(fuType: UInt): Bool = FuTypeOrR(fuType, stu)

  def isAMO(fuType: UInt): Bool = FuTypeOrR(fuType, mou)

  def isFence(fuType: UInt): Bool = FuTypeOrR(fuType, fence)

  def isVArith(fuType: UInt): Bool = FuTypeOrR(fuType, vecArith)

  def isVls(fuType: UInt): Bool = FuTypeOrR(fuType, vldu, vstu)

  def isVLoad(fuType: UInt): Bool = FuTypeOrR(fuType, vldu)

  def isVStore(fuType: UInt): Bool = FuTypeOrR(fuType, vstu)

  def isVecOPF(fuType: UInt): Bool = FuTypeOrR(fuType, vecOPF)

  def isVArithMem(fuType: UInt): Bool = FuTypeOrR(fuType, vecArithOrMem) // except vset

  def isDivSqrt(fuType: UInt): Bool = FuTypeOrR(fuType, div, fDivSqrt)

  def storeIsAMO(fuType: UInt): Bool = FuTypeOrR(fuType, mou)

  object FuTypeOrR {
    def apply(fuType: UInt, fu0: OHType, fus: OHType*): Bool = {
      apply(fuType, fu0 +: fus)
    }

    def apply(fuType: UInt, fus: Seq[OHType]): Bool = {
      fus.map(x => fuType(x.id)).reduce(_ || _)
    }
  }

  val functionNameMap = Map(
    jmp -> "jmp",
    brh -> "brh",
    i2f -> "int_to_float",
    i2v -> "int_to_vector",
    csr -> "csr",
    alu -> "alu",
    mul -> "mul",
    div -> "div",
    fence -> "fence",
    bku -> "bku",
    fmac -> "fmac",
    fmisc -> "fmisc",
    fDivSqrt -> "fdiv_fsqrt",
    ldu -> "load",
    stu -> "store",
    mou -> "mou",
    vsetiwi -> "vsetiwi",
    vsetiwf -> "vsetiwf",
    vsetfwf -> "vsetfwf",
    vipu -> "vipu",
    vialuF -> "vialuF",
    vfpu -> "vfpu",
    vldu -> "vldu",
    vstu -> "vstu",
    vppu -> "vppu",
    vimac -> "vimac",
    vfalu -> "vfalu",
    vfma -> "vfma",
    vfdiv -> "vfdiv"
  )
}

