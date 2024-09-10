package xiangshan.backend.fu

import chisel3._
import chisel3.util.BitPat
import utils.EnumUtils.OHEnumeration
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey

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
  val jmp  : OHType = addType(name = "jmp")
  val brh  : OHType = addType(name = "brh")
  val i2f  : OHType = addType(name = "i2f")
  val i2v  : OHType = addType(name = "i2v")
  val f2v  : OHType = addType(name = "f2v")
  val csr  : OHType = addType(name = "csr")
  val alu  : OHType = addType(name = "alu")
  val mul  : OHType = addType(name = "mul")
  val div  : OHType = addType(name = "div")
  val fence: OHType = addType(name = "fence")
  val bku  : OHType = addType(name = "bku")

  // fp
  val falu    : OHType = addType(name = "falu")
  val fmac    : OHType = addType(name = "fmac")
  val fcvt    : OHType = addType(name = "fcvt")
  val fDivSqrt: OHType = addType(name = "fDivSqrt")

  // ls
  val ldu: OHType = addType(name = "ldu")
  val stu: OHType = addType(name = "stu")
  val mou: OHType = addType(name = "mou") // AMO Unit (?)

  // vec
  val vipu   : OHType = addType(name = "vipu")
  val vialuF : OHType = addType(name = "vialuF")
  val vppu   : OHType = addType(name = "vppu")
  val vimac  : OHType = addType(name = "vimac")
  val vidiv  : OHType = addType(name = "vidiv")
  val vfpu   : OHType = addType(name = "vfpu") // will be deleted
  val vfalu  : OHType = addType(name = "vfalu")
  val vfma   : OHType = addType(name = "vfma")
  val vfdiv  : OHType = addType(name = "vfdiv")
  val vfcvt  : OHType = addType(name = "vfcvt")
  val vsetiwi: OHType = addType(name = "vsetiwi") // vset read rs write rd
  val vsetiwf: OHType = addType(name = "vsetiwf") // vset read rs write vconfig
  val vsetfwf: OHType = addType(name = "vsetfwf") // vset read old vl write vconfig

  // vec ls
  val vldu   : OHType = addType(name = "vldu")
  val vstu   : OHType = addType(name = "vstu")
  val vsegldu: OHType = addType(name = "vsegldu")
  val vsegstu: OHType = addType(name = "vsegstu")

  val intArithAll = Seq(jmp, brh, i2f, i2v, csr, alu, mul, div, fence, bku)
  // dq0 includes int's iq0 and iq1
  // dq1 includes int's iq2 and iq3
  def dq0OHTypeSeq(implicit p: Parameters): Seq[Seq[OHType]] = {
    val intIQParams = p(XSCoreParamsKey).backendParams.intSchdParams.get.issueBlockParams
    val dq0IQNums = intIQParams.size / 2
    val iqParams = intIQParams.take(dq0IQNums)
    val exuParams = iqParams.flatMap(_.exuBlockParams)
    exuParams.map(_.fuConfigs.map(_.fuType))
  }
  def dq1OHTypeSeq(implicit p: Parameters): Seq[Seq[OHType]] = {
    val intIQParams = p(XSCoreParamsKey).backendParams.intSchdParams.get.issueBlockParams
    val dq0IQNums = intIQParams.size / 2
    val iqParams = intIQParams.slice(dq0IQNums,intIQParams.size)
    val exuParams = iqParams.flatMap(_.exuBlockParams)
    exuParams.map(_.fuConfigs.map(_.fuType))
  }
  def intDq0All(implicit p: Parameters): Seq[OHType] = {
    dq0OHTypeSeq.flatten.distinct
  }
  def intDq0Deq0(implicit p: Parameters): Seq[OHType] = {
    val fuTypes = dq0OHTypeSeq(p)(0) ++ dq0OHTypeSeq(p)(2)
    fuTypes.distinct
  }
  def intDq0Deq1(implicit p: Parameters): Seq[OHType] = {
    val fuTypes = dq0OHTypeSeq(p)(1) ++ dq0OHTypeSeq(p)(3)
    fuTypes.distinct
  }
  def intDq1All(implicit p: Parameters): Seq[OHType] = {
    dq1OHTypeSeq.flatten.distinct
  }
  def intDq1Deq0(implicit p: Parameters): Seq[OHType] = {
    val fuTypes = dq1OHTypeSeq(p)(0) ++ dq1OHTypeSeq(p)(2)
    fuTypes.distinct
  }
  def intDq1Deq1(implicit p: Parameters): Seq[OHType] = {
    val fuTypes = dq1OHTypeSeq(p)(1) ++ dq1OHTypeSeq(p)(3)
    fuTypes.distinct
  }
  def intBothDeq0(implicit p: Parameters): Seq[OHType] = {
    val fuTypes = dq0OHTypeSeq(p)(0).intersect(dq0OHTypeSeq(p)(2)).intersect(dq1OHTypeSeq(p)(0)).intersect(dq1OHTypeSeq(p)(2))
    fuTypes.distinct
  }
  def intBothDeq1(implicit p: Parameters): Seq[OHType] = {
    val fuTypes = dq0OHTypeSeq(p)(1).intersect(dq0OHTypeSeq(p)(3)).intersect(dq1OHTypeSeq(p)(1)).intersect(dq1OHTypeSeq(p)(3))
    fuTypes.distinct
  }
  def is0latency(fuType: UInt): Bool = {
    val fuTypes = FuConfig.allConfigs.filter(_.latency == CertainLatency(0)).map(_.fuType)
    FuTypeOrR(fuType, fuTypes)
  }
  val fpArithAll = Seq(falu, fcvt, fmac, fDivSqrt, f2v)
  val scalaMemAll = Seq(ldu, stu, mou)
  val vecOPI = Seq(vipu, vialuF, vppu, vimac, vidiv)
  val vecOPF = Seq(vfpu, vfalu, vfma, vfdiv, vfcvt)
  val vecVSET = Seq(vsetiwi, vsetiwf, vsetfwf)
  val vecArith = vecOPI ++ vecOPF
  val vecMem = Seq(vldu, vstu, vsegldu, vsegstu)
  val vecArithOrMem = vecArith ++ vecMem
  val vecAll = vecVSET ++ vecArithOrMem
  val fpOP = fpArithAll ++ Seq(i2f, i2v)
  val scalaNeedFrm = Seq(i2f, fmac, fDivSqrt)
  val vectorNeedFrm = Seq(vfalu, vfma, vfdiv, vfcvt)

  /**
   * Different type of an operation request
   */

  def X: BitPat = BitPat.N(num) // Todo: Don't Care

  def num: Int = this.values.size

  def width: Int = num

  def apply() = UInt(num.W)

  /** is Integer operation to Dispatch Queue 0 */
  def isIntDq0(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intDq0All)

  /** is Integer operation to Dispatch Queue 1 */
  def isIntDq1(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intDq1All)

  /** is Integer operation to Dispatch Queue 0, Issue Queue's Dequeue 0 (IQ0) */
  def isIntDq0Deq0(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intDq0Deq0)

  /** is Integer operation to Dispatch Queue 0, Issue Queue's Dequeue 1 (IQ1) */
  def isIntDq0Deq1(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intDq0Deq1)

  /** is Integer operation to Dispatch Queue 1, Issue Queue's Dequeue 0 (IQ0) */
  def isIntDq1Deq0(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intDq1Deq0)

  /** is Integer operation to Dispatch Queue 1, Issue Queue's Dequeue 1 (IQ1) */
  def isIntDq1Deq1(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intDq1Deq1)

  /** is Integer operation to Both Issue Queue's Dequeue 0 (IQ0/2) */
  def isBothDeq0(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intBothDeq0)

  /** is Integer operation to Both Issue Queue's Dequeue 1 (IQ1/3) */
  def isBothDeq1(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intBothDeq1)

  /** is Integer operation */
  def isInt(fuType: UInt): Bool = FuTypeOrR(fuType, intArithAll ++ Seq(vsetiwi, vsetiwf))

  /** is Arithmetic Logic Unit operation */
  def isAlu(implicit fuType: UInt): Bool = FuTypeOrR(alu) // TODO

  /** is Branch operation */
  def isBrh(implicit fuType: UInt): Bool = FuTypeOrR(brh) // TODO

  /** is vset{i}vl{i} type operation */
  def isVset(implicit fuType: UInt): Bool = FuTypeOrR(vecVSET) // TODO

  /** is Jump operation */
  def isJump(implicit fuType: UInt): Bool = FuTypeOrR(jmp) // TODO

  /** is Jump operation */
  def isBrhJump(implicit fuType: UInt): Bool = FuTypeOrR(brh, jmp) // TODO

  /** is Float-point Arithmetic operation */
  def isFArith(implicit fuType: UInt): Bool = FuTypeOrR(fpArithAll) // TODO

  /** is Memory operation */
  def isMem(implicit fuType: UInt): Bool = FuTypeOrR(scalaMemAll) // TODO

  /** is Load/Store operation */
  def isLoadStore(implicit fuType: UInt): Bool = FuTypeOrR(ldu, stu) // TODO

  /** is Load operation */
  def isLoad(implicit fuType: UInt): Bool = FuTypeOrR(ldu) // TODO

  /** is Store operation */
  def isStore(implicit fuType: UInt): Bool = FuTypeOrR(stu) // TODO

  /** is AMO atomic operation */
  def isAMO(implicit fuType: UInt): Bool = FuTypeOrR(mou) // TODO

  /** is Fence operation */
  def isFence(implicit fuType: UInt): Bool = FuTypeOrR(fence) // TODO

  /** is CSR operation */
  def isCsr(implicit fuType: UInt): Bool = FuTypeOrR(csr) // TODO

  def isVsetRvfWvf(implicit fuType: UInt): Bool = FuTypeOrR(vsetfwf) // TODO

  /** is Vector Arithmetic operation */
  def isVArith(implicit fuType: UInt): Bool = FuTypeOrR(vecArith) // TODO

  /** is Vector Load/Store operation */
  def isVls(implicit fuType: UInt): Bool = FuTypeOrR(vldu, vstu, vsegldu, vsegstu) // TODO

  /** is Vector Non-segment Load/Store operation */
  def isVNonsegls(implicit fuType: UInt): Bool = FuTypeOrR(vldu, vstu)

  /** is Vector Segment Load/Store operation */
  def isVSegls(implicit futype: UInt): Bool = FuTypeOrR(vsegldu, vsegstu) // TODO

  /** is Vector Load operation */
  def isVLoad(implicit fuType: UInt): Bool = FuTypeOrR(vldu, vsegldu) // TODO

  /** is Vector Store operation */
  def isVStore(implicit fuType: UInt): Bool = FuTypeOrR(vstu, vsegstu) // TODO

  /** is Vector Segment Load operation */
  def isVSegLoad(implicit fuType: UInt): Bool = FuTypeOrR(vsegldu)

  /** is Vector Segment Store operation */
  def isVSegStore(implicit fuType: UInt): Bool = FuTypeOrR(vsegstu)

  /** is Vector Non-segment Load operation */
  def isVNonsegLoad(implicit fuType: UInt): Bool = FuTypeOrR(vldu)

  /** is Vector Non-segment Store operation */
  def isVNonsegStore(implicit fuType: UInt): Bool = FuTypeOrR(vstu)

  def isVecOPF(implicit fuType: UInt): Bool = FuTypeOrR(vecOPF)

  def isVArithMem(implicit fuType: UInt): Bool = FuTypeOrR(vecArithOrMem) // except vset // TODO

  def isVAll(implicit fuType: UInt): Bool = FuTypeOrR(vecAll)

  def isDivSqrt(implicit fuType: UInt): Bool = FuTypeOrR(div, fDivSqrt) // TODO

  def storeIsAMO(implicit fuType: UInt): Bool = FuTypeOrR(mou) // TODO

  def isVppu(implicit fuType: UInt): Bool = FuTypeOrR(vppu)

  def isScalaNeedFrm(implicit fuType: UInt): Bool = FuTypeOrR(scalaNeedFrm)

  def isVectorNeedFrm(implicit fuType: UInt): Bool = FuTypeOrR(vectorNeedFrm)

  object FuTypeOrR {
    def apply(fuType: UInt, fus: Seq[OHType]): Bool = {
      fus.map(x => fuType(x.id)).fold(false.B)(_ || _)
    }

    def apply(fuType: OHType, fus: Seq[OHType]): Boolean = {
      fus.map(fu => fu == fuType).fold(false)(_ || _)
    }

    // Overload with argument list of function units
    def apply(fuType: UInt, fu0: OHType, fus: OHType*)  : Bool    = apply(fuType, fu0 +: fus)
    def apply(fuType: OHType, fu0: OHType, fus: OHType*): Boolean = apply(fuType, fu0 +: fus)

    // Overload, passing fuType implicitly
    def apply(fus: Seq[OHType])(implicit fuType: UInt): Bool = apply(fuType, fus)
    def apply(fu0: OHType, fus: OHType*)(implicit fuType: UInt): Bool = apply(fuType, fu0 +: fus)
  }

  val functionNameMap: Map[OHType, String] = Map(
    jmp -> "jmp",
    brh -> "brh",
    i2f -> "int_to_float",
    i2v -> "int_to_vector",
    f2v -> "float_to_vector",
    csr -> "csr",
    alu -> "alu",
    mul -> "mul",
    div -> "div",
    fence -> "fence",
    bku -> "bku",
    fmac -> "fmac",
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
    vidiv -> "vidiv",
    vfalu -> "vfalu",
    vfma -> "vfma",
    vfdiv -> "vfdiv",
    vfcvt -> "vfcvt"
  )
}

