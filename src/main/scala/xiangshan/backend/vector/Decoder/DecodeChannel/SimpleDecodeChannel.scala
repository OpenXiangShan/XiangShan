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
            case CboInstPattern(rawInst) => null
            case SystemInstPattern(rawInst) => null
            case _ =>  Operand.GP
          }
          case _: IntSTypePattern => Operand.GP
          case IntBTypePattern(rawInst) => Operand.GP
          case IntUTypePattern(rawInst) => null
          case IntJTypePattern(rawInst) => null
          case pattern: FpInstPattern =>
            pattern match {
              case FpITypeF2fInstPattern(rawInst)     => Operand.FP
              case FpITypeF2iInstPattern(rawInst)     => Operand.FP
              case FpITypeI2fR0InstPattern(rawInst)   => null
              case _: FpITypeI2fInstPattern     => Operand.GP
              case FpITypeLoadInstPattern(rawInst)    => Operand.GP
              case FpRTypeIntDestInstPattern(rawInst) => Operand.FP
              case FpRTypeFpDestInstPattern(rawInst)  => Operand.FP
              case FpR4TypeInstPattern(rawInst)       => Operand.FP
              case FpSTypeInstPattern(rawInst)        => Operand.GP
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
        case IntBTypePattern(rawInst) => Operand.GP
        case IntUTypePattern(rawInst) => Operand.IMM
        case IntJTypePattern(rawInst) => Operand.IMM
        case pattern: FpInstPattern => pattern match {
          case FpITypeF2fInstPattern(rawInst) => null
          case FpITypeF2iInstPattern(rawInst) => null
          case _: FpITypeI2fInstPattern => null
          case FpITypeLoadInstPattern(rawInst) => null
          case FpRTypeIntDestInstPattern(rawInst) => Operand.FP
          case FpRTypeFpDestInstPattern(rawInst) => Operand.FP
          case FpR4TypeInstPattern(rawInst) => Operand.FP
          case FpSTypeInstPattern(rawInst) => Operand.FP
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
          case FpR4TypeInstPattern(rawInst) => Operand.FP
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
        case IntBTypePattern(rawInst) => n
        case IntUTypePattern(rawInst) => y
        case IntJTypePattern(rawInst) => y
        case fp: FpInstPattern =>
          fp match {
            case pattern :FpITypeInstPattern =>
              pattern match {
                case FpITypeF2iInstPattern(rawInst) => y
                case _ => n
              }
            case _: FpRTypeInstPattern => n
            case FpR4TypeInstPattern(rawInst) => n
            case FpSTypeInstPattern(rawInst) => n
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
              case FpITypeF2fInstPattern(rawInst) => y
              case FpITypeF2iInstPattern(rawInst) => n
              case pattern: FpITypeI2fInstPattern => y
              case FpITypeLoadInstPattern(rawInst) => y
            }
            case pattern: FpRTypeInstPattern => y
            case FpR4TypeInstPattern(rawInst) => y
            case FpSTypeInstPattern(rawInst) => n
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
          case AmoLrInstPattern(rawInst) => y
          case CSRInstPattern(rawInst) => y
          case CboInstPattern(rawInst) => n
          case HyperLoadInstPattern(rawInst) => n
          case SystemInstPattern(rawInst) => y
          case FenceInstPattern(rawInst) => y
          case FenceiInstPattern(rawInst) => y
          case _ => n
        }
        case pattern: IntSTypePattern => n
        case IntBTypePattern(rawInst) => n
        case IntUTypePattern(rawInst) => n
        case IntJTypePattern(rawInst) => n
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
          case AmoLrInstPattern(rawInst) => y
          case CSRInstPattern(rawInst) => y
          case CboInstPattern(rawInst) => n
          case HyperLoadInstPattern(rawInst) => n
          case SystemInstPattern(rawInst) => y
          case FenceInstPattern(rawInst) => y
          case FenceiInstPattern(rawInst) => y
          case _ => n
        }
        case pattern: IntSTypePattern => n
        case IntBTypePattern(rawInst) => n
        case IntUTypePattern(rawInst) => n
        case IntJTypePattern(rawInst) => n
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
              case SystemInstPattern(rawInst) => SelImm.X
              case HyperLoadInstPattern(rawInst) => SelImm.X
              case CSRInstPattern(rawInst) => SelImm.IMM_Z.toBitPat
              case AmoLrInstPattern(rawInst) => SelImm.X
              case CboInstPattern(rawInst) => SelImm.X
              case FenceInstPattern(rawInst) => SelImm.X
              case FenceiInstPattern(rawInst) => SelImm.X
              case _ => SelImm.X
            }
          case pattern: IntSTypePattern => SelImm.IMM_S.toBitPat
          case IntBTypePattern(rawInst) => SelImm.IMM_SB.toBitPat
          case IntUTypePattern(rawInst) => SelImm.IMM_U.toBitPat
          case IntJTypePattern(rawInst) => SelImm.IMM_UJ.toBitPat
          case pattern: FpInstPattern =>
            pattern match {
              case _: FpITypeI2fR0InstPattern => SelImm.X // Todo: SelImm.FLI
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
