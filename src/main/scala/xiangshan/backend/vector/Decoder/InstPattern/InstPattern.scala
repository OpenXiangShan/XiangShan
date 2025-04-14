package xiangshan.backend.vector.Decoder.InstPattern

import chisel3.util.BitPat
import chisel3.util.experimental.decode.DecodePattern
import xiangshan.backend.vector.Decoder.RVVDecodeUtil
import xiangshan.backend.vector.util.BString.BinaryStringHelper
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.util.ScalaTypeExt._

import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer

sealed abstract class InstPattern() extends DecodePattern {
  val rawInst: BitPat

  override def bitPat: BitPat = rawInst

  override def toString: String = {
    getName() + "@" + getClass.getSimpleName
  }

  @BeanProperty var name: String = ""

  def rd : BitPat = this.rawInst(11, 7)
  def rs1: BitPat = this.rawInst(19, 15)
  def rs2: BitPat = this.rawInst(24, 20)
  def rs3: BitPat = this.rawInst(31, 27)

  def fmt: BitPat = this.rawInst(26, 25)
  def rm: BitPat = this.rawInst(14, 12)

  def opcode7: BitPat = this.rawInst(6, 0)
}

object InstPattern {
  import Opcode7._

  def apply(rawInst: BitPat): Option[InstPattern] = {
    val opcode7 = rawInst(6, 0)
    val func3 = rawInst(14, 12)
    val func7 = rawInst(31, 25)
    val funct5 = rawInst(31, 27)
    val rs2 = rawInst(24, 20)
    val width = func3

    opcode7.rawString match {
      case OP_V => VecInstPattern(rawInst)
      case LOAD_FP =>
        if (Seq("000", "101", "110", "111").contains(width.rawString))
          VecInstPattern(rawInst)
        else if (Seq("001", "010", "011").contains(width.rawString))
          Some(FpITypeLoadInstPattern(rawInst))
        else
          None
      case STORE_FP =>
        if (Seq("000", "101", "110", "111").contains(width.rawString))
          VecInstPattern(rawInst)
        else if (Seq("001", "010", "011").contains(width.rawString))
          Some(FpSTypeInstPattern(rawInst))
        else
          None
      case OP_FP =>
        rawInst(31,30).rawString match {
          case "00" => Some(FpRTypeFpDestInstPattern(rawInst))
          case "01" => Some(FpITypeF2fInstPattern(rawInst))
          case "10" => Some(FpRTypeIntDestInstPattern(rawInst))
          case "11" => rawInst(29, 28).rawString match {
            case "00" => Some(FpITypeF2iInstPattern(rawInst)) // FCVT.int.fp
            case "01" => Some(FpITypeI2fInstPattern(rawInst)) // FCVT.fp.int
            case "10" => Some(FpITypeF2iInstPattern(rawInst)) // FMV.int.fp, FCLASS
            case "11" =>
              if (rs2.rawString == "00001")
                Some(FpITypeI2fR0InstPattern(rawInst)) // FMV.fp.int
              else
                Some(FpITypeI2fInstPattern(rawInst)) // FMV.fp.int
          }
        }
      case MADD | MSUB | NMADD | NMSUB => Some(FpR4TypeInstPattern(rawInst))
      case OP_IMM | OP_IMM_32 | LOAD | JALR => Some(IntITypePattern(rawInst))
      case OP | OP_32 => Some(IntRTypePattern(rawInst))
      case AUIPC | LUI => Some(IntUTypePattern(rawInst))
      case MSIC_MEM =>
        func3.rawString match {
          case "000" => Some(FenceInstPattern(rawInst)) // FENCE
          case "001" => Some(FenceiInstPattern(rawInst)) // FENCE_I
          case "010" => Some(CboInstPattern(rawInst))  // CBO
          case _ => None
        }
      case STORE => Some(IntSTypePattern(rawInst))
      case AMO =>
        funct5.rawString match {
          case "00010" => Some(AmoLrInstPattern(rawInst))
          case _ => Some(AmoInstPattern(rawInst))
        }
        Some(AmoInstPattern(rawInst))
      case JAL => Some(IntJTypePattern(rawInst))
      case BRANCH => Some(IntBTypePattern(rawInst))
      case SYSTEM =>
        Some(
          func3.rawString match {
            case "000" => SystemInstPattern(rawInst)
            case "100" =>
              func7.rawString(0) match {
                case '0' => HyperLoadInstPattern(rawInst)
                case '1' => HyperStoreInstPattern(rawInst)
              }
            case _ => CSRInstPattern(rawInst)
          }
        )
      case _ => None
    }
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

  object Opcode7 {
    val LOAD      = "0000011"
    val LOAD_FP   = "0000111"
    val CUSTOM_0  = "0001011"
    val MSIC_MEM  = "0001111"
    val OP_IMM    = "0010011"
    val AUIPC     = "0010111"
    val OP_IMM_32 = "0011011"
    val INST48b_0 = "0011111"

    val STORE     = "0100011"
    val STORE_FP  = "0100111"
    val CUSTOM_1  = "0101011"
    val AMO       = "0101111"
    val OP        = "0110011"
    val LUI       = "0110111"
    val OP_32     = "0111011"
    val INST64b   = "0111111"

    val MADD      = "1000011"
    val MSUB      = "1000111"
    val NMSUB     = "1001011"
    val NMADD     = "1001111"
    val OP_FP     = "1010011"
    val OP_V      = "1010111"
    val CUSTOM_2  = "1011011"
    val INST48b_1 = "1011111"

    val BRANCH     = "1100011"
    val JALR       = "1100111"
    val RESERVED_0 = "1101011"
    val JAL        = "1101111"
    val SYSTEM     = "1110011"
    val RESERVED_1 = "1110111"
    val CUSTOM_3   = "1111011"
    val INSTge80b  = "1111111"
  }
}

class IntRTypePattern(val rawInst: BitPat) extends InstPattern

sealed class IntITypePattern(val rawInst: BitPat) extends InstPattern

class IntSTypePattern(val rawInst: BitPat) extends InstPattern

object IntRTypePattern {
  def apply(rawInst: BitPat): IntRTypePattern = new IntRTypePattern(rawInst)
}

object IntITypePattern {
  def apply(rawInst: BitPat): IntITypePattern = new IntITypePattern(rawInst)
}

object IntSTypePattern {
  def apply(rawInst: BitPat): IntSTypePattern = new IntSTypePattern(rawInst)
}

case class IntBTypePattern(rawInst: BitPat) extends InstPattern

case class IntUTypePattern(rawInst: BitPat) extends InstPattern

case class IntJTypePattern(rawInst: BitPat) extends InstPattern

case class SystemInstPattern(override val rawInst: BitPat) extends IntITypePattern(rawInst)

case class HyperLoadInstPattern(override val rawInst: BitPat) extends IntITypePattern(rawInst)

case class HyperStoreInstPattern(override val rawInst: BitPat) extends IntSTypePattern(rawInst)

case class CSRInstPattern(override val rawInst: BitPat) extends IntITypePattern(rawInst)

case class AmoLrInstPattern(override val rawInst: BitPat) extends IntITypePattern(rawInst)

case class AmoInstPattern(override val rawInst: BitPat) extends IntRTypePattern(rawInst)

case class CboInstPattern(override val rawInst: BitPat) extends IntITypePattern(rawInst)

case class FenceInstPattern(override val rawInst: BitPat) extends IntITypePattern(rawInst)

case class FenceiInstPattern(override val rawInst: BitPat) extends IntITypePattern(rawInst)

sealed abstract class FpInstPattern extends InstPattern

sealed class FpITypeInstPattern(val rawInst: BitPat) extends FpInstPattern

object FpITypeInstPattern {
  def apply(rawInst: BitPat): FpITypeInstPattern = new FpITypeInstPattern(rawInst)
}

case class FpITypeF2fInstPattern(override val rawInst: BitPat) extends FpITypeInstPattern(rawInst)

case class FpITypeF2iInstPattern(override val rawInst: BitPat) extends FpITypeInstPattern(rawInst)

object FpITypeI2fInstPattern {
  def apply(rawInst: BitPat): FpITypeI2fInstPattern = new FpITypeI2fInstPattern(rawInst)
}

class FpITypeI2fInstPattern(override val rawInst: BitPat) extends FpITypeInstPattern(rawInst)

case class FpITypeI2fR0InstPattern(override val rawInst: BitPat) extends FpITypeI2fInstPattern(rawInst)

case class FpITypeLoadInstPattern(override val rawInst: BitPat) extends FpITypeInstPattern(rawInst)

class FpRTypeInstPattern(val rawInst: BitPat) extends FpInstPattern

object FpRTypeInstPattern {
  def apply(rawInst: BitPat): FpRTypeInstPattern = new FpRTypeInstPattern(rawInst)
}

case class FpRTypeIntDestInstPattern(override val rawInst: BitPat) extends FpRTypeInstPattern(rawInst)

case class FpRTypeFpDestInstPattern(override val rawInst: BitPat) extends FpRTypeInstPattern(rawInst)

case class FpR4TypeInstPattern(rawInst: BitPat) extends FpInstPattern

case class FpSTypeInstPattern(rawInst: BitPat) extends FpInstPattern

sealed abstract class VecInstPattern() extends InstPattern {
  val rawInst: BitPat

  override def toString: String = {
    getName() + "@" + getClass.getSimpleName
  }

  def vd : BitPat = this.rd
  def vs1: BitPat = this.rs1
  def vs2: BitPat = this.rs2
}

case class VecMemInstPattern(
  rawInst: BitPat,
) extends VecInstPattern() {
  def nf    : BitPat = rawInst(31, 29)
  def mew   : BitPat = rawInst(28)
  def mop   : BitPat = rawInst(27, 26)
  def vm    : BitPat = rawInst(25)
  def width : BitPat = rawInst(14, 12)
  def lumop : BitPat = rs2
  def sumop : BitPat = rs2

  def vs3   : BitPat = vd

  def isLoad: Boolean = this.opcode7.rawString == InstPattern.Opcode7.LOAD_FP

  def isStore: Boolean = this.opcode7.rawString == InstPattern.Opcode7.STORE_FP

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

case class VecArithInstPattern(
  rawInst: BitPat,
) extends VecInstPattern() {
  def func6 : BitPat = rawInst(31, 26)
  def vm    : BitPat = rawInst(25)
  def func3 : BitPat = rawInst(14, 12)

  override def bitPat: BitPat = genPattern

  val genPattern = rawInst.ensuring(_.getWidth == 32)
}

case class VecConfigInstPattern(
  rawInst: BitPat,
) extends VecInstPattern() {
  def bit31_20 = rawInst(31, 20)
  def func3 = b"111"
  def zimm10_0 = rawInst(30, 20)
  def zimm9_0 = rawInst(29, 20)

  def illegalVsetvli = zimm10_0

  override def bitPat: BitPat = genPattern

  val genPattern = rawInst.ensuring(_.getWidth == 32)
}

object VecInstPattern {
  import InstPattern.Opcode7._

  def apply(rawInst: BitPat): Option[VecInstPattern] = {
    val opcode7 = rawInst(6, 0)
    val func3 = rawInst(14, 12)
    val width = func3
    val mew = rawInst(28)

    opcode7.rawString match {
      case OP_V =>
        if (func3.rawString != "111")
          Some(VecArithInstPattern(rawInst))
        else
          Some(VecConfigInstPattern(rawInst))
      case LOAD_FP | STORE_FP =>
        if (Seq("000", "101", "110", "111").contains(width.rawString) && mew.rawString == "0") {
          Some(VecMemInstPattern(rawInst))
        } else {
          None
        }
      case _ =>
        None
    }
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

  println(s"Results: ${results.map(_.get)}")
}
