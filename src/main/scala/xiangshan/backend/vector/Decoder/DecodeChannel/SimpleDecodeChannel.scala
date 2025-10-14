package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util.experimental.decode.{BoolDecodeField, DecodeField, DecodeTable}
import chisel3.util.{BitPat, ValidIO}
import xiangshan._
import xiangshan.backend.decode.isa.bitfield.{Riscv32BitInst, XSInstBitFields}
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.Types.{Operand, OperandType}
import xiangshan.backend.vector.Decoder.SrcRenType
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.util.Verilog
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq

@instantiable
class SimpleDecodeChannel(instSeq: Seq[InstPattern]) extends Module {
  import SimpleDecodeChannelUtil._

  @public val in = IO(Input(new DecodeChannelInput))
  @public val out = IO(Output(ValidIO(new SimpleDecodeChannelOutput)))

  val instFields = in.rawInst.asTypeOf(new XSInstBitFields)

  val patterns = instSeq.collect { case x: InstPattern if !x.isInstanceOf[VecInstPattern] => x }

  val fields = Seq(
    LegalField,
    Src1Field,
    Src2Field,
    Src3Field,
    GpWenField,
    FpWenField,
    NoSpecField,
    BlockBackField,
    FlushPipeField,
    SelImmField,
  )

  println(s"The length of DecodeTable in SimpleDecodeChannel: ${patterns.length}")
  val table = new DecodeTable(patterns, fields)

  val result = table.decode(in.rawInst)

  out.valid := result(LegalField)
  out.bits.src1RenType := result(Src1Field)
  out.bits.src2RenType := result(Src2Field)
  out.bits.src3RenType := result(Src3Field)
  out.bits.lsrc1 := instFields.RS1
  out.bits.lsrc2 := instFields.RS2
  out.bits.lsrc3 := instFields.FS3
  out.bits.gpWen := result(GpWenField)
  out.bits.fpWen := result(FpWenField)
  out.bits.ldest := instFields.RD
  out.bits.noSpec := result(NoSpecField)
  out.bits.blockBack := result(BlockBackField)
  out.bits.flushPipe := result(FlushPipeField)
  out.bits.selImm := result(SelImmField)
}

class SimpleDecodeChannelOutput() extends Bundle {
  val src1RenType = new SrcRenType
  val src2RenType = new SrcRenType
  val src3RenType = new SrcRenType
  val lsrc1 = UInt(5.W)
  val lsrc2 = UInt(5.W)
  val lsrc3 = UInt(5.W)
  val gpWen = Bool()
  val fpWen = Bool()
  val ldest = UInt(5.W)
  val noSpec = Bool()
  val blockBack = Bool()
  val flushPipe = Bool()
  val selImm = SelImm()
}

object SimpleDecodeChannelMain extends App {
  Verilog.emitVerilog(
    new SimpleDecodeChannel(InstPattern.all),
    Array("--full-stacktrace"),
  )
}

object SimpleDecodeChannelUtil {

  object Src1Field extends DecodeField[InstPattern, SrcRenType] {
    override def name: String = "src1"

    override def chiselType: SrcRenType = new SrcRenType

    override def genTable(op: InstPattern): BitPat = {
      val operand: OperandType =
        op match {
          case _: IntRTypePattern => Operand.GP
          case pattern : IntITypePattern => pattern match {
            case CboInstPattern() => null
            case SystemInstPattern() => null
            case _ =>  Operand.GP
          }
          case _: IntSTypePattern => Operand.GP
          case IntBTypePattern() => Operand.GP
          case IntUTypePattern() => null
          case IntJTypePattern() => null
          case pattern: FpInstPattern =>
            pattern match {
              case FpITypeF2fInstPattern()     => Operand.FP
              case FpITypeF2iInstPattern()     => Operand.FP
              case FpITypeImmInstPattern()     => null
              case _: FpITypeI2fInstPattern           => Operand.GP
              case FpITypeLoadInstPattern()    => Operand.GP
              case FpRTypeIntDestInstPattern() => Operand.FP
              case FpRTypeFpDestInstPattern()  => Operand.FP
              case FpR4TypeInstPattern()       => Operand.FP
              case FpSTypeInstPattern()        => Operand.GP
            }
          case pattern: VecInstPattern =>
            throw new IllegalArgumentException(s"not support VecInstPattern $op in Src1Field")
        }

      operand match {
        case null => SrcRenType.genBitPat(None)
        case operandType: OperandType => SrcRenType.genBitPat(operandType)
      }
    }
  }

  object Src2Field extends DecodeField[InstPattern, SrcRenType] {
    override def name: String = "src2"

    override def chiselType: SrcRenType = new SrcRenType

    override def genTable(op: InstPattern): BitPat = {
      val operand = op match {
        case _: IntRTypePattern => Operand.GP
        case _: IntITypePattern => Operand.IMM
        case _: IntSTypePattern => Operand.GP
        case IntBTypePattern() => Operand.GP
        case IntUTypePattern() => Operand.IMM
        case IntJTypePattern() => Operand.IMM
        case pattern: FpInstPattern => pattern match {
          case FpITypeF2fInstPattern() => null
          case FpITypeF2iInstPattern() => null
          case _: FpITypeI2fInstPattern => null
          case FpITypeLoadInstPattern() => null
          case FpRTypeIntDestInstPattern() => Operand.FP
          case FpRTypeFpDestInstPattern() => Operand.FP
          case FpR4TypeInstPattern() => Operand.FP
          case FpSTypeInstPattern() => Operand.FP
        }
        case pattern: VecInstPattern =>
          throw new IllegalArgumentException(s"not support VecInstPattern $op in Src2Field")
      }

      operand match {
        case null => SrcRenType.genBitPat(None)
        case operandType: OperandType => SrcRenType.genBitPat(operandType)
      }
    }
  }

  object Src3Field extends DecodeField[InstPattern, SrcRenType] {
    override def name: String = "src3"

    override def chiselType: SrcRenType = new SrcRenType

    override def genTable(op: InstPattern): BitPat = {
      val operand = op match {
        case pattern: FpInstPattern => pattern match {
          case FpR4TypeInstPattern() => Operand.FP
          case _ => null
        }
        case pattern: VecInstPattern =>
          throw new IllegalArgumentException(s"not support VecInstPattern $op in Src3Field")
        case _ => null
      }

      operand match {
        case null => SrcRenType.genBitPat(None)
        case operandType: OperandType => SrcRenType.genBitPat(operandType)
      }
    }
  }

  object GpWenField extends BoolDecodeField[InstPattern] {

    override def name: String = "gpWen"

    override def genTable(op: InstPattern): BitPat = {
      op match {
        case _: IntRTypePattern => y
        case _: IntITypePattern => y
        case _: IntSTypePattern => n
        case IntBTypePattern() => n
        case IntUTypePattern() => y
        case IntJTypePattern() => y
        case fp: FpInstPattern =>
          fp match {
            case pattern :FpITypeInstPattern =>
              pattern match {
                case FpITypeF2iInstPattern() => y
                case _ => n
              }
            case _: FpRTypeInstPattern => n
            case FpR4TypeInstPattern() => n
            case FpSTypeInstPattern() => n
          }
        case pattern: VecInstPattern =>
          throw new IllegalArgumentException(s"not support VecInstPattern $op in GpWenField")
      }
    }
  }

  object FpWenField extends BoolDecodeField[InstPattern] {

    override def name: String = "fpWen"

    override def genTable(op: InstPattern): BitPat = {
      op match {
        case fp: FpInstPattern =>
          fp match {
            case pattern: FpITypeInstPattern => pattern match {
              case FpITypeF2fInstPattern() => y
              case FpITypeF2iInstPattern() => n
              case pattern: FpITypeI2fInstPattern => y
              case FpITypeLoadInstPattern() => y
            }
            case pattern: FpRTypeInstPattern => y
            case FpR4TypeInstPattern() => y
            case FpSTypeInstPattern() => n
          }
        case pattern: VecInstPattern =>
          throw new IllegalArgumentException(s"not support VecInstPattern $op in FpWenField")
        case _ => n
      }
    }
  }

  object NoSpecField extends BoolDecodeField[InstPattern] {

    override def name: String = "noSpec"

    override def genTable(op: InstPattern): BitPat = {
      op match {
        case pattern: IntRTypePattern => n
        case pattern: IntITypePattern => pattern match {
          case AmoLrInstPattern() => y
          case CSRInstPattern() => y
          case CboInstPattern() => n
          case HyperLoadInstPattern() => n
          case SystemInstPattern() => y
          case FenceInstPattern() => y
          case FenceiInstPattern() => y
          case _ => n
        }
        case pattern: IntSTypePattern => n
        case IntBTypePattern() => n
        case IntUTypePattern() => n
        case IntJTypePattern() => n
        case pattern: FpInstPattern => n
        case pattern: VecInstPattern => n
      }
    }
  }

  object BlockBackField extends BoolDecodeField[InstPattern] {

    override def name: String = "blockBack"

    override def genTable(op: InstPattern): BitPat = {
      op match {
        case pattern: IntRTypePattern => n
        case pattern: IntITypePattern => pattern match {
          case AmoLrInstPattern() => y
          case CSRInstPattern() => y
          case CboInstPattern() => n
          case HyperLoadInstPattern() => n
          case SystemInstPattern() => y
          case FenceInstPattern() => y
          case FenceiInstPattern() => y
          case _ => n
        }
        case pattern: IntSTypePattern => n
        case IntBTypePattern() => n
        case IntUTypePattern() => n
        case IntJTypePattern() => n
        case pattern: FpInstPattern => n
        case pattern: VecInstPattern => n
      }
    }
  }

  object FlushPipeField extends BoolDecodeField[InstPattern] {
    import freechips.rocketchip.rocket.Instructions._

    override def name: String = "flushPipe"

    override def genTable(op: InstPattern): BitPat = {
      if (instSeq.contains(op.name))
        y
      else
        n
    }

    val instSeq = getVariableNameSeq(
      SFENCE_VMA,
      FENCE_I,
      FENCE,
      PAUSE,
      SFENCE_INVAL_IR,
      HFENCE_GVMA,
      HFENCE_VVMA,
    )
  }

  object SelImmField extends DecodeField[InstPattern, UInt] {

    override def name: String = "selImm"

    override def chiselType: UInt = SelImm()

    override def genTable(op: InstPattern): BitPat = {
      {
        op match {
          case pattern: IntRTypePattern => SelImm.X
          case pattern: IntITypePattern =>
            pattern match {
              case SystemInstPattern() => SelImm.X
              case HyperLoadInstPattern() => SelImm.X
              case CSRInstPattern() => SelImm.IMM_Z.toBitPat
              case AmoLrInstPattern() => SelImm.X
              case CboInstPattern() => SelImm.X
              case FenceInstPattern() => SelImm.X
              case FenceiInstPattern() => SelImm.X
              case _ => SelImm.X
            }
          case pattern: IntSTypePattern => SelImm.IMM_S.toBitPat
          case IntBTypePattern() => SelImm.IMM_SB.toBitPat
          case IntUTypePattern() => SelImm.IMM_U.toBitPat
          case IntJTypePattern() => SelImm.IMM_UJ.toBitPat
          case pattern: FpInstPattern =>
            pattern match {
              case _: FpITypeImmInstPattern => SelImm.X // Todo: SelImm.FLI
              case _ => SelImm.X
            }
          case pattern: VecInstPattern =>
            throw new IllegalArgumentException(s"not support VecInstPattern $op in SelImmField")
        }
      }.pad0To(4).ensuring(_.getWidth == 4)
    }
  }

  object LegalField extends BoolDecodeField[InstPattern] {

    override def default: BitPat = n

    override def name: String = "legal"

    override def genTable(op: InstPattern): BitPat = {
      y
    }
  }
}
