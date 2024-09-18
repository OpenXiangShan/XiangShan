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

  /** The initial value for function unit type encoding. */
  private var initVal = 0

  /**
   * Add a new function unit type, record its name and return the function unit type.
   * Also increment the initial value for function unit type encoding.
   * @param name name of the function unit type
   * @return the function unit type
   */
  private def addType(name: String): OHType = {
    val oHVal = OHType(initVal, name)
    initVal += 1
    oHVal
  }

  /* Add Integer operation one-hot types into FuType */
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

  /* Add Float-Point operation one-hot types into FuType */
  val falu    : OHType = addType(name = "falu")
  val fmac    : OHType = addType(name = "fmac")
  val fcvt    : OHType = addType(name = "fcvt")
  val fDivSqrt: OHType = addType(name = "fDivSqrt")

  /* Add Load/Store operation one-hot types into FuType */
  val ldu: OHType = addType(name = "ldu")
  val stu: OHType = addType(name = "stu")
  val mou: OHType = addType(name = "mou") // AMO Unit (?)

  /* Add Vector operation one-hot types into FuType */
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

  /* Add Vector Load/Store operation one-hot types into FuType */
  val vldu   : OHType = addType(name = "vldu")
  val vstu   : OHType = addType(name = "vstu")
  val vsegldu: OHType = addType(name = "vsegldu")
  val vsegstu: OHType = addType(name = "vsegstu")

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

  private val intArithAll   = Seq(jmp, brh, i2f, i2v, csr, alu, mul, div, fence, bku)
  private val fpArithAll    = Seq(falu, fcvt, fmac, fDivSqrt, f2v)
  private val scalaMemAll   = Seq(ldu, stu, mou)
  private val vecOPI        = Seq(vipu, vialuF, vppu, vimac, vidiv)
  private val vecOPF        = Seq(vfpu, vfalu, vfma, vfdiv, vfcvt)
  private val vecVSET       = Seq(vsetiwi, vsetiwf, vsetfwf)
  private val vecArith      = vecOPI ++ vecOPF
  private val vecMem        = Seq(vldu, vstu, vsegldu, vsegstu)
  private val vecArithOrMem = vecArith ++ vecMem
  private val vecAll        = vecVSET ++ vecArithOrMem
  private val fpOP          = fpArithAll ++ Seq(i2f, i2v)
  private val scalaNeedFrm  = Seq(i2f, fmac, fDivSqrt)
  private val vectorNeedFrm = Seq(vfalu, vfma, vfdiv, vfcvt)


  /**
   * BitPat for a function unit type that is not known at compile time.
   */
  def X: BitPat = BitPat.N(num)

  /**
   * Number of function unit types.
   */
  val num: Int = this.values.size

  /**
   * Width of function unit type encoding.
   */
  val width: Int = num

  /**
   * Apply function for creating a UInt for a function unit type.
   */
  def apply(): UInt = UInt(num.W)

  /**
   * Check whether fuType argument equals to a type in given sequence
   * @param fus sequence of function unit types
   * @param fuType function unit type to check
   * @return true if fuType is in fus, false otherwise
   */
  private def fuTypeCheckLogic(fus: Seq[OHType])(fuType: OHType): Boolean = FuTypeOrR(fuType, fus)

  /**
   * Check whether fuType argument equals to a type in given sequence
   * @param fus argument list of function unit types
   * @param fuType function unit type to check
   * @return true if fuType is in fus, false otherwise
   */
  private def fuTypeCheckLogic(fus: OHType*)(fuType: OHType): Boolean = FuTypeOrR(fuType, fus)

  /**
   * Check whether fuType argument equals to a type in given arg list
   * @param fus sequence of function unit types
   * @param fuType wire with function unit type to check
   * @return true.B if fuType is in fus, false.B otherwise
   */
  private def fuTypeCheck(fus: Seq[OHType])(fuType: UInt): Bool = FuTypeOrR(fuType, fus)

  /**
   * Check whether fuType argument equals to a type in given arg list
   * @param fus argument list of function unit types
   * @param fuType wire with function unit type to check
   * @return true.B if fuType is in fus, false.B otherwise
   */
  private def fuTypeCheck(fus: OHType*)(fuType: UInt): Bool = FuTypeOrR(fuType, fus)

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

  /** Definition of following FuType-checking functions' type */
  type FuTChk = UInt => Bool
  type FuTChkLogic = OHType => Boolean

  /** is Integer operation */
  val isInt: FuTChk = fuTypeCheck(intArithAll ++ Seq(vsetiwi, vsetiwf))

  /** is Arithmetic Logic Unit operation */
  val isAlu: FuTChk = fuTypeCheck(alu)

  /** is Branch operation */
  val isBrh: FuTChk = fuTypeCheck(brh)

  /** is vset{i}vl{i} type operation */
  val isVset: FuTChk = fuTypeCheck(vecVSET)

  /** is Jump operation */
  val isJump: FuTChk = fuTypeCheck(jmp)

  /** is Jump operation */
  val isBrhJump: FuTChk = fuTypeCheck(brh, jmp)

  /** is Float-point Arithmetic operation */
  val isFArith: FuTChk = fuTypeCheck(fpArithAll)

  /** is Float-point related operation */
  val isFp: FuTChk = fuTypeCheck(fpOP)

  /** is Memory operation */
  val isMem: FuTChk = fuTypeCheck(scalaMemAll)

  /** is Load/Store operation */
  val isLoadStore: FuTChk = fuTypeCheck(ldu, stu)

  /** is Load operation */
  val isLoad: FuTChk = fuTypeCheck(ldu)

  /** is Store operation */
  val isStore: FuTChk = fuTypeCheck(stu)

  /** is AMO atomic operation */
  val isAMO: FuTChk = fuTypeCheck(mou)

  /** is Fence operation */
  val isFence: FuTChk = fuTypeCheck(fence)

  /** is CSR operation */
  val isCsr: FuTChk = fuTypeCheck(csr)

  val isVsetRvfWvf: FuTChk = fuTypeCheck(vsetfwf)

  /** is Vector Arithmetic operation */
  val isVArith: FuTChk = fuTypeCheck(vecArith)

  /** is Vector Load/Store operation */
  val isVls: FuTChk = fuTypeCheck(vecMem)
  val isVlsLogic: FuTChkLogic = fuTypeCheckLogic(vecMem)

  /** is Vector Non-segment Load/Store operation */
  val isVNonsegls: FuTChk = fuTypeCheck(vldu, vstu)
  val isVNonseglsLogic: FuTChkLogic = fuTypeCheck(vldu, vstu)

  /** is Vector Segment Load/Store operation */
  val isVSegls: FuTChk = fuTypeCheck(vsegldu, vsegstu)

  /** is Vector Load operation */
  val isVLoad: FuTChk = fuTypeCheck(vldu, vsegldu)

  /** is Vector Store operation */
  val isVStore: FuTChk = fuTypeCheck(vstu, vsegstu)

  /** is Vector Segment Load operation */
  val isVSegLoad: FuTChk = fuTypeCheck(vsegldu)

  /** is Vector Segment Store operation */
  val isVSegStore: FuTChk = fuTypeCheck(vsegstu)

  /** is Vector Non-segment Load operation */
  val isVNonsegLoad: FuTChk = fuTypeCheck(vldu)

  /** is Vector Non-segment Store operation */
  val isVNonsegStore: FuTChk = fuTypeCheck(vstu)

  val isVecOPF: FuTChk = fuTypeCheck(vecOPF)

  /** is Vector Arithmetic or Memory operation */
  val isVArithMem: FuTChk = fuTypeCheck(vecArithOrMem) // except vset

  /** is Vector operation */
  val isVAll: FuTChk = fuTypeCheck(vecAll)

  /** is Divide/Square-root operation */
  val isDivSqrt: FuTChk = fuTypeCheck(div, fDivSqrt)

  val isVppu: FuTChk = fuTypeCheck(vppu)

  val isScalaNeedFrm: FuTChk = fuTypeCheck(scalaNeedFrm)

  val isVectorNeedFrm: FuTChk = fuTypeCheck(vectorNeedFrm)

  /**
   * Perform OR operation on 1 or more function unit types.
   * Check whether fuType argument equals to a given type.
   */
  object FuTypeOrR {
    /**
     * Perform OR operation on 1 or more function unit types.
     * Check whether fuType argument equals to a given type.
     * @param fuType function unit type wire to check
     * @param fus sequence of function unit types
     * @return true.B if fuType is in fus, false.B otherwise
     */
    def apply(fuType: UInt, fus: Seq[OHType]): Bool = {
      fus.map(x => fuType(x.id)).fold(false.B)(_ || _)
    }

    /**
     * Perform OR operation on 1 or more function unit types.
     * Check whether fuType argument equals to a given type.
     * @param fuType function unit type to check
     * @param fus sequence of function unit types
     * @return true if fuType is in fus, false otherwise
     */
    def apply(fuType: OHType, fus: Seq[OHType]): Boolean = {
      fus.map(fu => fu == fuType).fold(false)(_ || _)
    }

    /**
     * Perform OR operation on 1 or more function unit types.
     * Check whether fuType argument equals to a given type.
     * @param fuType function unit type wire to check
     * @param fus argument list of function unit types
     * @return true if fuType is in fus, false otherwise
     */
    def apply(fuType: UInt, fu0: OHType, fus: OHType*): Bool = apply(fuType, fu0 +: fus)

    /**
     * Perform OR operation on 1 or more function unit types.
     * Check whether fuType argument equals to a given type.
     * @param fuType function unit type to check
     * @param fus argument list of function unit types
     * @return true if fuType is in fus, false otherwise
     */
    def apply(fuType: OHType, fu0: OHType, fus: OHType*): Boolean = apply(fuType, fu0 +: fus)
  }

  /** A map from each function unit type to its name. Generated by the addType() function. */
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

    falu -> "falu",
    fmac -> "fmac",
    fcvt -> "fcvt",
    fDivSqrt -> "fdiv_fsqrt",

    ldu -> "load",
    stu -> "store",
    mou -> "mou",

    vipu -> "vipu",
    vialuF -> "vialuF",
    vppu -> "vppu",
    vimac -> "vimac",
    vidiv -> "vidiv",
    vfpu -> "vfpu",
    vfalu -> "vfalu",
    vfma -> "vfma",
    vfdiv -> "vfdiv",
    vfcvt -> "vfcvt",
    vsetiwi -> "vsetiwi",
    vsetiwf -> "vsetiwf",
    vsetfwf -> "vsetfwf",

    vldu -> "vldu",
    vstu -> "vstu",
    vsegldu -> "vsegldu",
    vsegstu -> "vsegstu",
  )
}

