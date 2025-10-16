package xiangshan.backend.vector.Decoder.InstPattern

import chisel3.util.BitPat
import chisel3.util.experimental.decode.DecodePattern
import xiangshan.backend.vector.util.BString.BinaryStringHelper

import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer

sealed trait InstPattern extends DecodePattern {
  implicit val rawInst: BitPat

  override def bitPat: BitPat = rawInst

  override def toString: String = {
    getName() + "@" + getClass.getSimpleName
  }

  @BeanProperty var name: String = ""

  def rd : BitPat = this.rawInst(11, 7)
  def rs1: BitPat = this.rawInst(19, 15)
  def rs2: BitPat = this.rawInst(24, 20)

  def opcode7: BitPat = this.rawInst(6, 0)
}

sealed abstract class IntInstPattern(implicit val rawInst: BitPat) extends InstPattern

sealed abstract class FpInstPattern(implicit val rawInst: BitPat) extends InstPattern {
  def fmt: BitPat = this.rawInst(26, 25)
  def rm: BitPat = this.rawInst(14, 12)
  def rs3: BitPat = this.rawInst(31, 27)
}

sealed abstract class VecInstPattern(implicit val rawInst: BitPat) extends InstPattern {
  override def toString: String = {
    getName() + "@" + getClass.getSimpleName
  }

  def vd : BitPat = this.rd
  def vs1: BitPat = this.rs1
  def vs2: BitPat = this.rs2
}

sealed class IntRTypePattern(implicit rawInst: BitPat) extends IntInstPattern

object IntRTypePattern {
  def apply()(implicit rawInst: BitPat): IntRTypePattern = new IntRTypePattern
}

sealed class IntITypePattern(implicit rawInst: BitPat) extends IntInstPattern

object IntITypePattern {
  def apply()(implicit rawInst: BitPat): IntITypePattern = new IntITypePattern
}

sealed class IntSTypePattern(implicit rawInst: BitPat) extends IntInstPattern

object IntSTypePattern {
  def apply()(implicit rawInst: BitPat): IntSTypePattern = new IntSTypePattern
}

case class IntBTypePattern()(implicit rawInst: BitPat) extends IntInstPattern

case class IntUTypePattern()(implicit rawInst: BitPat) extends IntInstPattern

case class IntJTypePattern()(implicit rawInst: BitPat) extends IntInstPattern

case class SystemInstPattern()(implicit rawInst: BitPat) extends IntITypePattern

case class HyperLoadInstPattern()(implicit rawInst: BitPat) extends IntITypePattern

case class CSRInstPattern()(implicit rawInst: BitPat) extends IntITypePattern

case class AmoLrInstPattern()(implicit rawInst: BitPat) extends IntITypePattern

case class CboInstPattern()(implicit rawInst: BitPat) extends IntITypePattern

case class FenceInstPattern()(implicit rawInst: BitPat) extends IntITypePattern

case class FenceiInstPattern()(implicit rawInst: BitPat) extends IntITypePattern

case class AmoInstPattern()(implicit rawInst: BitPat) extends IntRTypePattern

case class HyperStoreInstPattern()(implicit rawInst: BitPat) extends IntSTypePattern

sealed class FpITypeInstPattern(implicit rawInst: BitPat) extends FpInstPattern

object FpITypeInstPattern {
  def apply(implicit rawInst: BitPat): FpITypeInstPattern = new FpITypeInstPattern
}

case class FpITypeF2fInstPattern()(implicit rawInst: BitPat) extends FpITypeInstPattern

case class FpITypeF2iInstPattern()(implicit rawInst: BitPat) extends FpITypeInstPattern

object FpITypeI2fInstPattern {
  def apply()(implicit rawInst: BitPat): FpITypeI2fInstPattern = new FpITypeI2fInstPattern
}

class FpITypeI2fInstPattern(implicit rawInst: BitPat) extends FpITypeInstPattern

case class FpITypeImmInstPattern()(implicit rawInst: BitPat) extends FpITypeI2fInstPattern

case class FpITypeLoadInstPattern()(implicit rawInst: BitPat) extends FpITypeInstPattern

sealed abstract class FpRTypeInstPattern(implicit rawInst: BitPat) extends FpInstPattern

case class FpRTypeIntDestInstPattern()(implicit rawInst: BitPat) extends FpRTypeInstPattern

case class FpRTypeFpDestInstPattern()(implicit rawInst: BitPat) extends FpRTypeInstPattern

case class FpR4TypeInstPattern()(implicit rawInst: BitPat) extends FpInstPattern

case class FpSTypeInstPattern()(implicit rawInst: BitPat) extends FpInstPattern

abstract class VecMemInstPattern(
  implicit rawInst: BitPat,
) extends VecInstPattern() {
  def nf    : BitPat = rawInst(31, 29)
  def mew   : BitPat = rawInst(28)
  def mop   : BitPat = rawInst(27, 26)
  def vm    : BitPat = rawInst(25)
  def width : BitPat = rawInst(14, 12)
  def lumop : BitPat = rs2
  def sumop : BitPat = rs2

  def vs3   : BitPat = vd

  def isLoad: Boolean = this.opcode7.rawString == InstPattern.Opcode5.LOAD_FP

  def isStore: Boolean = this.opcode7.rawString == InstPattern.Opcode5.STORE_FP

  def isUnitStrideNormal: Boolean = isUnitStride && this.lumop.rawString == "00000"
  def isUnitStrideWhole : Boolean = isUnitStride && this.lumop.rawString == "01000"
  def isUnitStrideMask  : Boolean = isUnitStride && this.lumop.rawString == "01011"
  def isUnitStrideFF    : Boolean = isUnitStride && this.lumop.rawString == "10000"

  def isUnitStride    : Boolean = this.mop.rawString == "00"
  def isIndexedUnorder: Boolean = this.mop.rawString == "01"
  def isStrided       : Boolean = this.mop.rawString == "10"
  def isIndexedOrder  : Boolean = this.mop.rawString == "11"

  def eewValue: Int = this.width.rawString match {
    case "000" => 8
    case "101" => 16
    case "110" => 32
    case "111" => 64
  }
}

trait UnitStride extends VecMemInstPattern
trait Strided extends VecMemInstPattern
trait OrderIndex extends VecMemInstPattern
trait UnorderIndex extends VecMemInstPattern
trait VecLoadInstPattern extends VecMemInstPattern
trait VecStoreInstPattern extends VecMemInstPattern

abstract class VecUnitStrideLoad()(implicit rawInst: BitPat) extends VecLoadInstPattern with UnitStride
case class VecUnitStrideNormalLoad()(implicit rawInst: BitPat) extends VecUnitStrideLoad
case class VecWholeLoad()(implicit rawInst: BitPat) extends VecUnitStrideLoad
case class VecMaskLoad()(implicit rawInst: BitPat) extends VecUnitStrideLoad
case class VecUnitStrideFFLoad()(implicit rawInst: BitPat) extends VecUnitStrideLoad

case class VecStridedLoad()(implicit rawInst: BitPat) extends VecLoadInstPattern with Strided
case class VecOrderLoad()(implicit rawInst: BitPat) extends VecLoadInstPattern with OrderIndex
case class VecUnorderLoad()(implicit rawInst: BitPat) extends VecLoadInstPattern with UnorderIndex

abstract class VecUnitStrideStore()(implicit rawInst: BitPat) extends VecStoreInstPattern with UnitStride
case class VecUnitStrideNormalStore()(implicit rawInst: BitPat) extends VecUnitStrideStore
case class VecWholeStore()(implicit rawInst: BitPat) extends VecUnitStrideStore
case class VecMaskStore()(implicit rawInst: BitPat) extends VecUnitStrideStore

case class VecStridedStore()(implicit rawInst: BitPat) extends VecStoreInstPattern with Strided
case class VecOrderStore()(implicit rawInst: BitPat) extends VecStoreInstPattern with OrderIndex
case class VecUnorderStore()(implicit rawInst: BitPat) extends VecStoreInstPattern with UnorderIndex

case class VecArithInstPattern()(
  implicit rawInst: BitPat,
) extends VecInstPattern() {
  def func6 : BitPat = rawInst(31, 26)
  def vm    : BitPat = rawInst(25)
  def category : BitPat = rawInst(14, 12)

  override def bitPat: BitPat = genPattern

  val genPattern = rawInst.ensuring(_.getWidth == 32)
}

case class VecConfigInstPattern()(
  implicit rawInst: BitPat,
) extends VecInstPattern() {
  def bit31_20 = rawInst(31, 20)
  def func3 = b"111"
  def zimm10_0 = rawInst(30, 20)
  def zimm9_0 = rawInst(29, 20)

  def illegalVsetvli = zimm10_0

  override def bitPat: BitPat = genPattern

  val genPattern = rawInst.ensuring(_.getWidth == 32)
}

object VecLoadInstPattern {
  def apply()(implicit rawInst: BitPat): VecMemInstPattern = {
    val mew = rawInst(28)
    val mop = rawInst(27, 26)
    val lumop = rawInst(24, 20)

    mew.rawString match {
      case "0" =>
        mop.rawString match {
          case "00" =>
            lumop.rawString match {
              case "00000" => VecUnitStrideNormalLoad()
              case "01000" => VecWholeLoad()
              case "01011" => VecMaskLoad()
              case "10000" => VecUnitStrideFFLoad()
              case _ => null
            }
          case "01" => VecUnorderLoad()
          case "10" => VecStridedLoad()
          case "11" => VecOrderLoad()
          case _ => null
        }
      case "1" => null
    }
  }
}

object VecStoreInstPattern {
  def apply()(implicit rawInst: BitPat): VecMemInstPattern = {
    val mew = rawInst(28)
    val mop = rawInst(27, 26)
    val lumop = rawInst(24, 20)

    mew.rawString match {
      case "0" =>
        mop.rawString match {
          case "00" =>
            lumop.rawString match {
              case "00000" => VecUnitStrideNormalStore()
              case "01000" => VecWholeStore()
              case "01011" => VecMaskStore()
              case _ => null
            }
          case "01" => VecUnorderStore()
          case "10" => VecStridedStore()
          case "11" => VecOrderStore()
          case _ => null
        }
      case "1" => null
    }
  }
}

object InstPattern {
  import Opcode5._

  def apply(implicit rawInst: BitPat): Option[InstPattern] = {
    val opcode2 = rawInst(1, 0)
    val opcode5 = rawInst(6, 2)
    val func3 = rawInst(14, 12)
    val func7 = rawInst(31, 25)
    val funct5 = rawInst(31, 27)
    val rs2 = rawInst(24, 20)
    val width = func3

    // filter not C extension
    if (opcode2.rawString != "11") {
      return None
    }

    // |inst[4:2]    |000    |001      |010        |011      |100    |101   |110       |111 (>32b)
    // |inst[6:5]
    // |00           |LOAD   |LOAD_FP  |CUSTOM_0   |MISC_MEM |OP_IMM |AUIPC |OP_IMM_32 |INST48b
    // |01           |STORE  |STORE_FP |CUSTOM_1   |AMO      |OP     |LUI   |OP_32     |INST64b
    // |10           |MADD   |MSUB     |NMSUB      |NMADD    |OP_FP  |OP_V  |CUSTOM_2  |INST48b
    // |11           |BRANCH |JALR     |_reserved_ |JAL      |SYSTEM |OP_VE |CUSTOM_3  |INSTge80b

    val res: InstPattern = opcode5.rawString match {
      // inst[6:5] == b"00"
      case LOAD => IntITypePattern()
      case LOAD_FP =>
        width.rawString match {
          case "000" | "101" | "110" | "111" => VecLoadInstPattern()
          case "001" | "010" | "011" => FpITypeLoadInstPattern()
          case _ => null
        }
      case CUSTOM_0 => null
      case MSIC_MEM =>
        func3.rawString match {
          case "000" => FenceInstPattern() // FENCE
          case "001" => FenceiInstPattern() // FENCE_I
          case "010" => CboInstPattern()  // CBO
          case _ => null
        }
      case OP_IMM => IntITypePattern()
      case AUIPC => IntUTypePattern()
      case OP_IMM_32 => IntITypePattern()
      case INST48b_0 => null

      // inst[6:5] == b"01"
      case STORE => IntSTypePattern()
      case STORE_FP =>
        width.rawString match {
          case "000" | "101" | "110" | "111" => VecStoreInstPattern()
          case "001" | "010" | "011" => FpSTypeInstPattern()
          case _ => null
        }
      case CUSTOM_1 => null
      case AMO =>
        funct5.rawString match {
          case "00010" => AmoLrInstPattern()
          case _ => AmoInstPattern()
        }
      case OP => IntRTypePattern()
      case LUI => IntUTypePattern()
      case OP_32 => IntRTypePattern()
      case INST64b => null

      // inst[6:5] == b"10"
      case MADD | MSUB | NMADD | NMSUB => FpR4TypeInstPattern()

      case OP_FP =>
        rawInst(31,30).rawString match {
          case "00" => FpRTypeFpDestInstPattern()
          case "01" => FpITypeF2fInstPattern()
          case "10" => FpRTypeIntDestInstPattern()
          case "11" =>
            rawInst(29, 28).rawString match {
              case "00" => FpITypeF2iInstPattern() // FCVT.int.fp
              case "01" => FpITypeI2fInstPattern() // FCVT.fp.int
              case "10" => FpITypeF2iInstPattern() // FMV.int.fp, FCLASS
              case "11" =>
                rs2.rawString match {
                  case "00001" => FpITypeImmInstPattern() // FLI.fp
                  case _ => FpITypeI2fInstPattern()       // FMV.fp.int
                }
          }
        }
      case OP_V =>
        func3.rawString match {
          case "111" => VecConfigInstPattern()
          case _ => VecArithInstPattern()
        }
      case CUSTOM_2 => null
      case INST48b_1 => null

      // inst[6:5] == b"11"
      case BRANCH => IntBTypePattern()
      case JALR => IntITypePattern()
      case RESERVED => null
      case JAL => IntJTypePattern()
      case SYSTEM =>
        func3.rawString match {
          case "000" => SystemInstPattern()
          case "100" =>
            func7.rawString(0) match {
              case '0' => HyperLoadInstPattern()
              case '1' => HyperStoreInstPattern()
            }
          // Todo: Check it
          case _ => CSRInstPattern()
        }
      case OP_VE => null // Todo: Add Vector Crypto Inst
      case CUSTOM_3 => null
      case INSTge80b => null
      case _ => throw new IllegalArgumentException(s"The opcode7(${opcode5.rawString}) of inst${rawInst.rawString} is unknown")
    }

    Option(res)
  }

  lazy val all: Seq[InstPattern] = {
    import scala.reflect.runtime.currentMirror
    import scala.reflect.runtime.universe._
    val objectType = typeOf[freechips.rocketchip.rocket.Instructions.type]
    val methods: Iterable[MethodSymbol] = objectType.decls.collect {
      case m: MethodSymbol if m.returnType =:= typeOf[BitPat] && m.paramLists.isEmpty => m
    }
    val instanceMirror = currentMirror.reflect(freechips.rocketchip.rocket.Instructions)

    val instMethods = ArrayBuffer.empty[MethodSymbol]

    methods.map { method =>
      val methodMirror: MethodMirror = instanceMirror.reflectMethod(method)
      val bitpat = methodMirror().asInstanceOf[BitPat]
      val pattern: Option[InstPattern] = InstPattern(bitpat)
      if (pattern.nonEmpty) {
        instMethods += method
      }
      pattern.foreach(_.setName(method.name.toString))
      pattern
    }.filter(_.nonEmpty).map(_.get).toSeq
  }

  object Opcode5 {
    val LOAD      = "00000"
    val LOAD_FP   = "00001"
    val CUSTOM_0  = "00010"
    val MSIC_MEM  = "00011"
    val OP_IMM    = "00100"
    val AUIPC     = "00101"
    val OP_IMM_32 = "00110"
    val INST48b_0 = "00111"

    val STORE     = "01000"
    val STORE_FP  = "01001"
    val CUSTOM_1  = "01010"
    val AMO       = "01011"
    val OP        = "01100"
    val LUI       = "01101"
    val OP_32     = "01110"
    val INST64b   = "01111"

    val MADD      = "10000"
    val MSUB      = "10001"
    val NMSUB     = "10010"
    val NMADD     = "10011"
    val OP_FP     = "10100"
    val OP_V      = "10101"
    val CUSTOM_2  = "10110"
    val INST48b_1 = "10111"

    val BRANCH     = "11000"
    val JALR       = "11001"
    val RESERVED   = "11010"
    val JAL        = "11011"
    val SYSTEM     = "11100"
    val OP_VE      = "11101"
    val CUSTOM_3   = "11110"
    val INSTge80b  = "11111"
  }
}

object VecInstPattern {
  import InstPattern.Opcode5._

  def apply(implicit rawInst: BitPat): Option[VecInstPattern] = {

    val opcode5 = rawInst(6, 2)
    val func3 = rawInst(14, 12)
    val width = func3
    val mew = rawInst(28)

    Option(
      opcode5.rawString match {
        case OP_V =>
          func3.rawString match {
            case "111" => VecConfigInstPattern()
            case _ => VecArithInstPattern()
          }
        case LOAD_FP if Seq("000", "101", "110", "111").contains(width.rawString) => VecLoadInstPattern()
        case STORE_FP if Seq("000", "101", "110", "111").contains(width.rawString) => VecStoreInstPattern()
        case _ => throw new IllegalArgumentException(s"Unsupported opcode5(${opcode5.rawString})")
      }
    )
  }

  lazy val all: Seq[VecInstPattern] = {
    import scala.reflect.runtime.currentMirror
    import scala.reflect.runtime.universe._
    val objectType = typeOf[freechips.rocketchip.rocket.Instructions.type]
    val methods: Iterable[MethodSymbol] = objectType.decls.collect {
      case m: MethodSymbol if m.returnType =:= typeOf[BitPat] && m.paramLists.isEmpty => m
    }
    val instanceMirror = currentMirror.reflect(freechips.rocketchip.rocket.Instructions)

    val vectorInstMethods = ArrayBuffer.empty[MethodSymbol]

    methods.map { method =>
      val methodMirror: MethodMirror = instanceMirror.reflectMethod(method)
      val bitpat = methodMirror().asInstanceOf[BitPat]
      val pattern: Option[VecInstPattern] = VecInstPattern(bitpat)
      if (pattern.nonEmpty) {
        vectorInstMethods += method
      }
      pattern.foreach(_.setName(method.name.toString))
      pattern
    }.filter(_.nonEmpty).map(_.get).toSeq
  }

  lazy val mem: Seq[VecInstPattern] = {
    this.all.filter(_.isInstanceOf[VecMemInstPattern])
  }

  lazy val set: Seq[VecConfigInstPattern] = {
    this.all.collect { case x: VecConfigInstPattern => x }
  }

  object Category extends Enumeration {
    case class CategoryStr(str: String) extends super.Val

    import scala.language.implicitConversions
    implicit def valueToPlanetVal(x: Value): CategoryStr = x.asInstanceOf[CategoryStr]
    implicit def valueToString(x: Value): String = x.str

    val OPIVV = CategoryStr("000")
    val OPFVV = CategoryStr("001")
    val OPMVV = CategoryStr("010")
    val OPIVI = CategoryStr("011")
    val OPIVX = CategoryStr("100")
    val OPFVF = CategoryStr("101")
    val OPMVX = CategoryStr("110")
    val OPCFG = CategoryStr("111")

    val arithValueSet = ValueSet(OPIVV, OPFVV, OPMVV, OPIVI, OPIVX, OPFVF, OPMVX)
  }
}

object RVVInstPatternMain extends App {
  import scala.reflect.runtime.currentMirror
  import scala.reflect.runtime.universe._
  val objectType = typeOf[freechips.rocketchip.rocket.Instructions.type]

  val methods: Iterable[MethodSymbol] = objectType.decls.collect {
    case m: MethodSymbol if m.returnType =:= typeOf[BitPat] && m.paramLists.isEmpty => m
  }

  val instanceMirror = currentMirror.reflect(freechips.rocketchip.rocket.Instructions)

  val vectorInstMethods = ArrayBuffer.empty[MethodSymbol]

  val results: Iterable[Option[VecInstPattern]] = methods.map { method =>
    val methodMirror: MethodMirror = instanceMirror.reflectMethod(method)
    val pattern: Option[VecInstPattern] = VecInstPattern(methodMirror().asInstanceOf[BitPat])
    if (pattern.nonEmpty) {
      vectorInstMethods += method
    }
    pattern.foreach(_.setName(method.name.toString))
    pattern
  }.filterNot(_.isEmpty)

  println(s"Results: $results")
  println(vectorInstMethods.map(_.name))
}

object InstPatternMain extends App {
  import scala.reflect.runtime.currentMirror
  import scala.reflect.runtime.universe._
  val objectType = typeOf[freechips.rocketchip.rocket.Instructions.type]

  val methods: Iterable[MethodSymbol] = objectType.decls.collect {
    case m: MethodSymbol if m.returnType =:= typeOf[BitPat] && m.paramLists.isEmpty => m
  }

  val instanceMirror = currentMirror.reflect(freechips.rocketchip.rocket.Instructions)

  val instMethods = ArrayBuffer.empty[MethodSymbol]

  val results: Iterable[Option[InstPattern]] = methods.map { method =>
    val methodMirror: MethodMirror = instanceMirror.reflectMethod(method)
    val pattern: Option[InstPattern] = InstPattern(methodMirror().asInstanceOf[BitPat])
    if (pattern.isEmpty) {
      println(s"inst ${method.name.toString} has not been handled")
    }
    if (pattern.nonEmpty) {
      instMethods += method
    }
    pattern.foreach(_.setName(method.name.toString))
    pattern
  }.filterNot(_.isEmpty)

  results.map(x => println(x.get))
}
