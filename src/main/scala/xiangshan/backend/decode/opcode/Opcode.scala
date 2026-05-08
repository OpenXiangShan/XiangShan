package xiangshan.backend.decode.opcode

import chisel3._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util._
import xiangshan.backend.decode.opcode.OpcodeTraits._
import xiangshan.backend.vector.Decoder.Types
import xiangshan.backend.vector.Decoder.Types.{DecodeSelImm, MaskType, Operand, OperandType}
import xiangshan.backend.vector.Decoder.Uop.{UopInfoRename, UopInfoRenameSimple}
import xiangshan.backend.vector.util.BString.BinaryStringHelper
import yunsuan.encoding.Opcode.Opcodes

import scala.language.implicitConversions


object Opcode {
  def main(args: Array[String]): Unit = {
    val opcodes = Seq(
      AluOpcodes,
      BruOpcodes,
      JmpOpcodes,
      MulOpcodes,
      DivOpcodes,
      LsuOpcodes,
      CsrOpcodes,
      FenceOpcodes,
      BkuOpcodes,
      FCvtOpcodes,
      FMacOpcodes,
      FDivOpcodes,
      FMiscOpcodes,
      VSetOpcodes,
      VIAluOpcodes,
      VMAluOpcodes,
      VIMacOpcodes,
      VIDivOpcodes,
      VIRedOpcodes,
      VIPermOpcodes,
      VFCvtOpcodes,
      VFMacOpcodes,
      VFDivOpcodes,
      VFMiscOpcodes,
      VFRedOpcodes,
    )

    for (opcodeCls <- opcodes) {
      for (opcode <- opcodeCls.all) {
        println(s"${opcode}")
      }
    }
  }

  def apply(): UInt = yunsuan.encoding.Opcode.Opcode.apply()
  def getWidth: Int = yunsuan.encoding.Opcode.Opcodes.getWidth

  type Opcode = yunsuan.encoding.Opcode.Opcode
  type Opcodes = yunsuan.encoding.Opcode.Opcodes
  val Opcodes = yunsuan.encoding.Opcode.Opcodes
  val opcodes = yunsuan.encoding.Opcode.Opcodes
  val VIAluOpcodes      = opcodes.VIAluOpcode
  val VMAluOpcodes      = opcodes.VMAluOpcode
  val VIMacOpcodes      = opcodes.VIMacOpcode
  val VIDivOpcodes      = opcodes.VIDivOpcode
  val VIRedOpcodes      = opcodes.VIRedOpcode
  val VIPermOpcodes     = opcodes.VIPermOpcode
  val VMoveOpcodes      = opcodes.VMoveOpcode
  val VSha256msOpcodes  = opcodes.VSha256msOpcode
  val VSha256cOpcodes   = opcodes.VSha256cOpcode
  val FCvtOpcodes       = opcodes.FCvtOpcode
  val FMiscOpcodes      = opcodes.FMiscOpcode
  val FMacOpcodes       = opcodes.FMacOpcode
  val VFMiscOpcodes     = opcodes.VFMiscOpcode
  val VFCvtOpcodes      = opcodes.VFCvtOpcode
  val VFMacOpcodes      = opcodes.VFMacOpcode

  // Todo: remove these
  def X = BitPat("b0_0000_0000")
  def FMVXF = BitPat("b1_1000_0000") //for fmv_x_d & fmv_x_w

  //  var opcodes = collection.mutable.Map[Opcode[_], Seq[Opcode]]()
  //
  //  def getWidth = width
  //
  //  def updateWidth(w: Int): Unit = Opcode.width = w.max(Opcode.width)

  //  class AluOpcode(val encode: UInt) extends Opcode
  //  class BruOpcode(val encode: UInt) extends Opcode
  //  class JmpOpcode(val encode: UInt) extends Opcode
  //  class MulOpcode(val encode: UInt) extends Opcode
  //  class DivOpcode(val encode: UInt) extends Opcode
  //  class LduOpcodes(val encode: UInt) extends Opcode
  //  class Value(val encode: UInt) extends Opcode
  //  class AmoOpcode(val encode: UInt) extends Opcode
  //  class BkuOpcode(val encode: UInt) extends Opcode
  //  class CsrOpcode(val encode: UInt) extends Opcode
  //  class FenceOpcode(val encode: UInt) extends Opcode
  //  class FmacOpcode(val encode: UInt) extends Opcode
  //  class FdivOpcode(val encode: UInt) extends Opcode
  //  class FcvtOpcode(val encode: UInt) extends Opcode
  //  class FmiscOpcode(val encode: UInt) extends Opcode
  //  class VSetOpcode(val encode: UInt) extends Opcode
  //  class VIAluOpcode(val encode: UInt) extends Opcode
  //  class VMAluOpcode(val encode: UInt) extends Opcode
  //  class VIMacOpcode(val encode: UInt) extends Opcode
  //  class VIRedOpcode(val encode: UInt) extends Opcode
  //  class VIPermOpcode(val encode: UInt) extends Opcode
  //  class VIDivOpcode(val encode: UInt) extends Opcode

  //  abstract class OpcodeApply[T <: Opcode](gen: UInt => T) {
  //    def apply(uint: UInt*)(implicit opcodes: Opcode[T], name: SourceName): T = {
  //      val opcode: T = gen(uint.reduce(_ ### _))
  //      opcode.setName(name.value)
  //      Opcode.opcodes(opcodes) :+= opcode
  //      opcode
  //    }
  //  }

  //  object AluOpcode extends OpcodeApply(new AluOpcode(_))
  //  object BruOpcode extends OpcodeApply(new BruOpcode(_))
  //  object JmpOpcode extends OpcodeApply(new JmpOpcode(_))
  //  object MulOpcode extends OpcodeApply(new MulOpcode(_))
  //  object DivOpcode extends OpcodeApply(new DivOpcode(_))
  //  object LduOpcodes extends OpcodeApply(new LduOpcodes(_))
  //  object Value extends OpcodeApply(new Value(_))
  //  object AmoOpcode extends OpcodeApply(new AmoOpcode(_))
  //  object BkuOpcode extends OpcodeApply(new BkuOpcode(_))
  //  object CsrOpcode extends OpcodeApply(new CsrOpcode(_))
  //  object FenceOpcode extends OpcodeApply(new FenceOpcode(_))
  //  object FmacOpcode extends OpcodeApply(new FmacOpcode(_))
  //  object FdivOpcode extends OpcodeApply(new FdivOpcode(_))
  //  object FcvtOpcode extends OpcodeApply(new FcvtOpcode(_))
  //  object FmiscOpcode extends OpcodeApply(new FmiscOpcode(_))
  //  object VSetOpcode extends OpcodeApply(new VSetOpcode(_))
  //  object VIAluOpcode extends OpcodeApply(new VIAluOpcode(_))
  //  object VMAluOpcode extends OpcodeApply(new VMAluOpcode(_))
  //  object VIMacOpcode extends OpcodeApply(new VIMacOpcode(_))
  //  object VIRedOpcode extends OpcodeApply(new VIRedOpcode(_))
  //  object VIPermOpcode extends OpcodeApply(new VIPermOpcode(_))
  //  object VIDivOpcode extends OpcodeApply(new VIDivOpcode(_))

  //  sealed abstract class Opcode[T <: Opcode] {
  //    import scala.reflect.runtime.universe._
  //
  //    implicit val implOpcodes: Opcode[T] = this
  //
  //    type OpcodeType = T
  //
  //    lazy val mirror: InstanceMirror = runtimeMirror(getClass.getClassLoader).reflect(this)
  //
  //    Opcode.opcodes += (this -> Seq[OpcodeType]())
  //
  //    def allOpcodes: Set[OpcodeType] = Set()
  //
  //    protected def reflectGetAllOpcodes[TT](implicit tag: TypeTag[TT]): Set[OpcodeType] = {
  //      val objectType = typeOf[TT]
  //      val fields: Iterable[TermSymbol] = objectType.members.collect {
  //        case f: TermSymbol if f.isVal && f.typeSignature <:< typeOf[Opcode] => f
  //      }
  //      fields.map {
  //        field =>
  //          val fieldMirror = this.mirror.reflectField(field)
  //          fieldMirror.get.asInstanceOf[OpcodeType]
  //      }.toSet
  //    }
  //  }

  implicit def castToUInt(op: Opcode): UInt = bitPatToUInt(op.encode)

  implicit def castToBitPat(op: Opcode): BitPat = op.encode

  object AluOpcodes extends Opcodes {
    // slliuw: ZEXT(src1[31:0]) << shamt
    // sll:     src1 << src2
    val slliuw     = IntIType(bb"000_0000")
    val sll        = IntRType(bb"000_0001")

    // bclr:    src1 & ~(1 << src2[5:0])
    // bset:    src1 | (1 << src2[5:0])
    // binv:    src1 ^ (1 << src2[5:0])
    val bclr       = IntRType(bb"000_0010")
    val bset       = IntRType(bb"000_0011")
    val binv       = IntRType(bb"000_0100")

    // srl:     src1 >> src2
    // bext:    (src1 >> src2)[0]
    // sra:     src1 >> src2 (arithmetic)
    val srl        = IntRType(bb"000_0101")
    val bext       = IntRType(bb"000_0110")
    val sra        = IntRType(bb"000_0111")

    // rol:     (src1 << src2) | (src1 >> (xlen - src2))
    // ror:     (src1 >> src2) | (src1 << (xlen - src2))
    val rol        = IntRType(bb"000_1001")
    val ror        = IntRType(bb"000_1011")

    // addw:      SEXT((src1 + src2)[31:0])
    // oddaddw:   SEXT((src1[0] + src2)[31:0])
    // subw:      SEXT((src1 - src2)[31:0])
    // lui32addw: SEXT(SEXT(src2[11:0], 32) + {src2[31:12], 12'b0}, 64)
    val addw       = IntRType(bb"001_0000")
    val oddaddw    = Value(bb"001_0001") // TODO: fusion decode
    val subw       = IntRType(bb"001_0010")
    val lui32addw  = Value(bb"001_0011") // TODO: fusion decode

    // addwbit:   (src1 + src2)[0]
    // addwbyte:  (src1 + src2)[7:0]
    // addwzexth: ZEXT((src1  + src2)[15:0])
    // addwsexth: SEXT((src1  + src2)[15:0])
    val addwbit    = Value(bb"001_0100") // TODO: fusion decode
    val addwbyte   = Value(bb"001_0101") // TODO: fusion decode
    val addwzexth  = Value(bb"001_0110") // TODO: fusion decode
    val addwsexth  = Value(bb"001_0111") // TODO: fusion decode

    // sllw:     SEXT((src1 << src2)[31:0])
    // srlw:     SEXT((src1[31:0] >> src2)[31:0])
    // sraw:     SEXT((src1[31:0] >> src2)[31:0])
    val sllw       = IntRType(bb"001_1000")
    val srlw       = IntRType(bb"001_1001")
    val sraw       = IntRType(bb"001_1010")
    val rolw       = IntRType(bb"001_1100")
    val rorw       = IntRType(bb"001_1101")

    // adduw:  src1[31:0]  + src2
    // oddadd:  src1[0]     + src2
    // add:     src1        + src2
    // lui32add: SEXT(src2[11:0]) + {src2[63:12], 12'b0}
    val adduw      = IntRType(bb"010_0000")
    val oddadd     = Value(bb"010_0001") // TODO: fusion decode
    val add        = IntRType(bb"010_0010")
    val lui32add   = Value(bb"010_0011") // TODO: fusion decode

    // sr29add: src1[63:29] + src2
    // sr30add: src1[63:30] + src2
    // sr31add: src1[63:31] + src2
    // sr32add: src1[63:32] + src2
    val sr29add    = Value(bb"010_0100") // TODO: fusion decode
    val sr30add    = Value(bb"010_0101") // TODO: fusion decode
    val sr31add    = Value(bb"010_0110") // TODO: fusion decode
    val sr32add    = Value(bb"010_0111") // TODO: fusion decode

    // sh1adduw: {src1[31:0], 1'b0} + src2
    // sh1add: {src1[62:0], 1'b0} + src2
    // sh2add_uw: {src1[31:0], 2'b0} + src2
    // sh2add: {src1[61:0], 2'b0} + src2
    // sh3add_uw: {src1[31:0], 3'b0} + src2
    // sh3add: {src1[60:0], 3'b0} + src2
    // sh4add: {src1[59:0], 4'b0} + src2
    val sh1adduw   = IntRType(bb"010_1000")
    val sh1add     = IntRType(bb"010_1001")
    val sh2adduw   = IntRType(bb"010_1010")
    val sh2add     = IntRType(bb"010_1011")
    val sh3adduw   = IntRType(bb"010_1100")
    val sh3add     = IntRType(bb"010_1101")
    val sh4add     = Value(bb"010_1111") // TODO: fusion decode

    // SUB-op: src1 - src2
    val sub        = IntRType(bb"011_0000")
    val sltu       = IntRType(bb"011_0001")
    val slt        = IntRType(bb"011_0010")
    val maxu       = IntRType(bb"011_0100")
    val minu       = IntRType(bb"011_0101")
    val max        = IntRType(bb"011_0110")
    val min        = IntRType(bb"011_0111")

    // Zicond
    val czero_eqz  = IntRType(bb"111_0100")
    val czero_nez  = IntRType(bb"111_0110")

    // misc optype
    val and        = IntRType(bb"100_0000")
    val andn       = IntRType(bb"100_0001")
    val or         = IntRType(bb"100_0010")
    val orn        = IntRType(bb"100_0011")
    val xor        = IntRType(bb"100_0100")
    val xnor       = IntRType(bb"100_0101")
    val orcb       = IntIType(bb"100_0110")

    val sextb      = IntIType(bb"100_1000")
    val packh      = IntRType(bb"100_1001")
    val sexth      = IntIType(bb"100_1010")
    val packw      = IntRType(bb"100_1011")

    val revb       = IntIType(bb"101_0000")
    val rev8       = IntIType(bb"101_0001")
    val pack       = IntRType(bb"101_0010")
    val orh48      = Value(bb"101_0011") // TODO: fusion decode

    val szewl1     = Value(bb"101_1000") // TODO: fusion decode
    val szewl2     = Value(bb"101_1001") // TODO: fusion decode
    val szewl3     = Value(bb"101_1010") // TODO: fusion decode
    val byte2      = Value(bb"101_1011") // TODO: fusion decode

    val andlsb     = Value(bb"110_0000")
    val andzexth   = Value(bb"110_0001")
    val orlsb      = Value(bb"110_0010")
    val orzexth    = Value(bb"110_0011")
    val xorlsb     = Value(bb"110_0100")
    val xorzexth   = Value(bb"110_0101")
    val orcblsb    = Value(bb"110_0110")
    val orcbzexth  = Value(bb"110_0111")
    // for xstrap
    val xstrap     = IntIType(bb"111_1111") + BlockBack // CustomTrapPattern

    // this.all.foreach(_ + GpWen)
    // this.all.foreach(_ + Src1Gp)
    // (this.all.toSet -- Seq(sextb, sexth, orcb, rev8, revb)).foreach(_ + Src2En)

    def logicToLsb(func: UInt) = Cat("b110".U(3.W), func(3, 1), 0.U(1.W))
    def logicToZexth(func: UInt) = Cat("b110".U(3.W), func(3, 1), 1.U(1.W))

    def isShift(func: UInt): Bool      = func(6, 4) === "b000".U
    def isWiden(func: UInt): Bool      = func(6, 4) === "b001".U
    def isAddOp(func: UInt): Bool      = func(6, 4) === "b010".U
    def isCompare(func: UInt): Bool    = func(6, 4) === "b011".U
    def isMisc(func: UInt): Bool       = func(6) & (!func(5) | !func(4))

    def isAddw(func: UInt): Bool       = func(3, 0) === "b0000".U
    def isOddaddw(func: UInt): Bool    = func(3, 0) === "b0001".U
    def isSubw(func: UInt): Bool       = func(3, 0) === "b0010".U
    def isLui32addw(func: UInt): Bool  = func(3, 0) === "b0011".U
    def isAddwOrSubw(func: UInt): Bool = !func(3) && !func(2) && !func(0) || func(2)
    def isSr29add(func: UInt): Bool    = func(1, 0) === "b00".U
    def isSr30add(func: UInt): Bool    = func(1, 0) === "b01".U
    def isSr31add(func: UInt): Bool    = func(1, 0) === "b10".U
    def isSr32add(func: UInt): Bool    = func(1, 0) === "b11".U
    def isSh1add(func: UInt): Bool     = func(2, 1) === "b00".U
    def isSh2add(func: UInt): Bool     = func(2, 1) === "b01".U
    def isSh3add(func: UInt): Bool     = func(2, 1) === "b10".U
    def isSh4add(func: UInt): Bool     = func(2, 1) === "b11".U

    def isAdd(func: UInt): Bool     = isAddOp(func) && func(3, 2) === "b00".U
    def isSradd(func: UInt): Bool   = isAddOp(func) && func(3, 2) === "b01".U
    def isShadd(func: UInt): Bool   = isAddOp(func) && func(3)
    def isMaxMin(func: UInt): Bool  = isCompare(func) && func(2, 1) === "b11".U
    def isMaxMinU(func: UInt): Bool = isCompare(func) && func(2, 1) === "b10".U
    def isSlt(func: UInt): Bool     = isCompare(func) && func(2, 1) === "b01".U
    def isSltu(func: UInt): Bool    = isCompare(func) && func(2, 0) === "b001".U
    def isSub(func: UInt): Bool     = isCompare(func) && func(2, 0) === "b000".U
    def isSll(func: UInt): Bool     = isShift(func) && func(3, 1) === "b000".U
    def isBclr(func: UInt): Bool    = isShift(func) && func(3, 0) === "b0010".U
    def isBset(func: UInt): Bool    = isShift(func) && func(3, 0) === "b0011".U
    def isBinv(func: UInt): Bool    = isShift(func) && func(3, 0) === "b0100".U
    def isSrl(func: UInt): Bool     = isShift(func) && func(3, 0) === "b0101".U
    def isBext(func: UInt): Bool    = isShift(func) && func(3, 0) === "b0110".U
    def isSra(func: UInt): Bool     = isShift(func) && func(3, 0) === "b0111".U
    def isRol(func: UInt): Bool     = isShift(func) && func(3) && !func(1)
    def isRor(func: UInt): Bool     = isShift(func) && func(3) &&  func(1)
    def isAddwOp(func: UInt): Bool  = isWiden(func) && (!func(3) & !func(2) & (!func(1) | func(0)) | !func(3) & func(2))
    def isSubwOp(func: UInt): Bool  = isWiden(func) && func(3, 0) === "b0010".U
    def isSllw(func: UInt): Bool    = isWiden(func) && func(3, 0) === "b1000".U
    def isSrlw(func: UInt): Bool    = isWiden(func) && func(3, 2) === "b10".U && func(0)
    def isSraw(func: UInt): Bool    = isWiden(func) && func(3, 1) === "b101".U
    def isRolw(func: UInt): Bool    = isWiden(func) && func(3, 2) === "b11".U && !func(0)
    def isRorw(func: UInt): Bool    = isWiden(func) && func(3, 2) === "b11".U &&  func(0)

    def isZicond(func: UInt): Bool  = func(6, 4).andR && !func(3)
    def isJmp(func: UInt): Bool     = func(6, 3).andR && !func(2)
    def isNewJmp(func: UInt): Bool  = func(6, 2).andR && !func(1)
  }

  object BruOpcodes extends Opcodes {
    // branch
    val beq        = IntBSType(bb"000_000")
    val bne        = IntBSType(bb"000_001")
    val blt        = IntBSType(bb"000_100")
    val bge        = IntBSType(bb"000_101")
    val bltu       = IntBSType(bb"001_000")
    val bgeu       = IntBSType(bb"001_001")

    def getBranchType(func: UInt) = func(3, 1)
    def isBranchInvert(func: UInt) = func(0)
  }

  object JmpOpcodes extends Opcodes {
    val jal        = IntUJType(bb"111_1000")
    val jalr       = IntIType(bb"111_1001")
    val auipc      = IntUJType(bb"111_1010")

    def jumpUopisJalr(op: UInt) = op(0)
    def jumpUopisAuipc(op: UInt) = op(1)
  }

  object LinkOpcodes extends Opcodes {
    val link      = Value(bb"001") + GpWen
    val auipc_new = IntUJType(bb"010")

    def linkUopisLink(op: UInt) = op(0)
    def linkUopisAuipc(op: UInt) = op(1)
  }

  object NewJmpOpcodes extends Opcodes {
    val j  = Value(bb"111_1100")
    val jr = Value(bb"111_1101") + Src1Gp

    def jumpUopisjr(op: UInt) = op(0)
  }

  object MulOpcodes extends Opcodes {
    // mul
    // bit encoding: | type (2bit) | isWord(1bit) | opcode(2bit) |
    val mul    = IntRType(bb"00000")
    val mulh   = IntRType(bb"00001")
    val mulhsu = IntRType(bb"00010")
    val mulhu  = IntRType(bb"00011")
    val mulw   = IntRType(bb"00100")
    val mulw7  = IntRType(bb"01100")
    def isSign(op: UInt) = !op(1)
    def isW(op: UInt) = op(2)
    def isH(op: UInt) = op(1, 0) =/= 0.U
    def getOp(op: UInt) = Cat(op(3), op(1, 0))
  }

  object DivOpcodes extends Opcodes {
    // div
    // bit encoding: | type (2bit) | isWord(1bit) | isSign(1bit) | opcode(1bit) |
    val div    = IntRType(bb"10000")
    val divu   = IntRType(bb"10010")
    val rem    = IntRType(bb"10001")
    val remu   = IntRType(bb"10011")

    val divw   = IntRType(bb"10100")
    val divuw  = IntRType(bb"10110")
    val remw   = IntRType(bb"10101")
    val remuw  = IntRType(bb"10111")

    def isSign(op: UInt) = !op(1)
    def isW(op: UInt) = op(2)
    def isH(op: UInt) = op(0)
  }

  trait LsuTrait {
    sealed abstract class Size(val encode: BitPat) {
      def U: UInt = bitPatToUInt(this.encode)
    }

    case object B  extends Size(bb"000")
    case object H  extends Size(bb"001")
    case object W  extends Size(bb"010")
    case object D  extends Size(bb"011")
    case object Q  extends Size(bb"100")
    case object VB extends Size(bb"100")
    case object VH extends Size(bb"101")
    case object VW extends Size(bb"110")
    case object VD extends Size(bb"111")

    implicit def SizeToUInt(s: Size): UInt = s.U
    implicit def SizeToBitPat(s: Size): BitPat = s.encode

    object Size {
      val width = 3
    }

    def size(op: UInt): UInt = op(Size.width, 1)

    def vecElemSize(op: UInt): UInt = op(2, 1)

    def getSignSize(op: UInt): UInt = op(4, 1)

    def sizeIs(sz: this.type => this.Size)(op: UInt): Bool = {
      op(Size.width, 1) === sz(this).U
    }

    def makeLsUop(isHlv: Bool, isHlvx: Bool, size: UInt): UInt = {
      Cat(isHlv, isHlvx, bitPatToUInt(sign), size.pad(Size.width), 0.U(1.W))
    }

    def getVecLSMop(fuOpType: UInt): UInt = fuOpType(6, 5)

    val unsign = bb"1"
    val sign = bb"0"

    protected val isH = bb"1"
    protected val nonH = bb"0"

    // isX means that the uop needs execute permission.
    // E.g. hlvx.hu, hlvx.wu
    protected val isX = bb"1"
    protected val nonX = bb"0"

    protected val SCALA  = bb"0000"
    protected val US     = bb"0100" // Unit-Stride
    protected val CS     = bb"0101" // Const-Strided
    protected val WHOLE  = bb"0110"
    protected val MASK   = bb"0111"
    protected val IUEI8  = bb"1000" // Index-Unordered
    protected val IUEI16 = bb"1001" // Index-Unordered
    protected val IUEI32 = bb"1010" // Index-Unordered
    protected val IUEI64 = bb"1011" // Index-Unordered
    protected val IOEI8  = bb"1100" // Index-Ordered
    protected val IOEI16 = bb"1101" // Index-Ordered
    protected val IOEI32 = bb"1110" // Index-Ordered
    protected val IOEI64 = bb"1111" // Index-Ordered

    protected val isFof = bb"1"
    protected val nonFof = bb"0"

    protected def getMemOpType(op: UInt): UInt = op(10, 7)

    def isScalaOp(op: UInt): Bool = getMemOpType(op) === SCALA

    def isVecMemOp(op: UInt): Bool = getMemOpType(op) =/= SCALA

    def isVecMemContinousOp(op: UInt): Bool = Cat(Seq(US, WHOLE, MASK).map(_ === getMemOpType(op))).orR
    // vle, vlr, vlm, vleff
    // vse, vsr, vsm
    def isAllUS   (op: UInt): Bool = Cat(Seq(US, WHOLE, MASK).map(_ === getMemOpType(op))).orR
    // vle, vse
    def isUStride (op: UInt): Bool = getMemOpType(op) === US
    // vlnr, vsnf
    def isWhole   (op: UInt): Bool = getMemOpType(op) === WHOLE
    // vlm, vsm
    def isMasked  (op: UInt): Bool = getMemOpType(op) === MASK
    // vlse, vsse
    def isStrided (op: UInt): Bool = getMemOpType(op) === CS
    // vlxuei, vlxoei
    // vsxuei, vsxoei
    def isIndexed (op: UInt): Bool = getMemOpType(op).head(1) === 1.U
  }

  trait LduOpcodes extends Opcodes with LsuTrait with DataType {
    protected val uopPrefetch = bb"1"
    protected val uopLoad = bb"0"

    // normal load
    val lb     = IntIType(SCALA, nonH, nonX, sign  , B, uopLoad)
    val lh     = IntIType(SCALA, nonH, nonX, sign  , H, uopLoad)
    val lw     = IntIType(SCALA, nonH, nonX, sign  , W, uopLoad)
    val ld     = IntIType(SCALA, nonH, nonX, sign  , D, uopLoad)
    val lq     = IntIType(SCALA, nonH, nonX, sign  , Q, uopLoad) // TODO: no corresponding store instruction
    val lbu    = IntIType(SCALA, nonH, nonX, unsign, B, uopLoad)
    val lhu    = IntIType(SCALA, nonH, nonX, unsign, H, uopLoad)
    val lwu    = IntIType(SCALA, nonH, nonX, unsign, W, uopLoad)
    // hypervior load
    val hlvb   = IntIType(SCALA, isH, nonX, sign  , B, uopLoad)
    val hlvh   = IntIType(SCALA, isH, nonX, sign  , H, uopLoad)
    val hlvw   = IntIType(SCALA, isH, nonX, sign  , W, uopLoad)
    val hlvd   = IntIType(SCALA, isH, nonX, sign  , D, uopLoad)
    val hlvbu  = IntIType(SCALA, isH, nonX, unsign, B, uopLoad)
    val hlvhu  = IntIType(SCALA, isH, nonX, unsign, H, uopLoad)
    val hlvwu  = IntIType(SCALA, isH, nonX, unsign, W, uopLoad)
    val hlvxhu = IntIType(SCALA, isH, isX , unsign, H, uopLoad)
    val hlvxwu = IntIType(SCALA, isH, isX , unsign, W, uopLoad)

    def isHlv(op: UInt): Bool = op(6) === isH && op(0) === uopLoad
    def isHlvx(op: UInt): Bool = (op(6, 5) === (isH ## isX)) && op(0) === uopLoad

    /**
     * Segment related fields are not encoded in Opcode.
     * Since segment number and the opcode are orthogonal, segment number will be passed separatedly in some field of bundle.
     */

    val vle8        = Value(US    , nonH, nonX, nonFof, VB, uopLoad) + VpWen + Src1Gp
    val vle16       = Value(US    , nonH, nonX, nonFof, VH, uopLoad) + VpWen + Src1Gp
    val vle32       = Value(US    , nonH, nonX, nonFof, VW, uopLoad) + VpWen + Src1Gp
    val vle64       = Value(US    , nonH, nonX, nonFof, VD, uopLoad) + VpWen + Src1Gp

    val vle8ff      = Value(US    , nonH, nonX, isFof , VB, uopLoad) + VpWen + Src1Gp
    val vle16ff     = Value(US    , nonH, nonX, isFof , VH, uopLoad) + VpWen + Src1Gp
    val vle32ff     = Value(US    , nonH, nonX, isFof , VW, uopLoad) + VpWen + Src1Gp
    val vle64ff     = Value(US    , nonH, nonX, isFof , VD, uopLoad) + VpWen + Src1Gp

    val vlse8       = Value(CS    , nonH, nonX, nonFof, VB, uopLoad) + VpWen + Src1Gp + Src2Gp
    val vlse16      = Value(CS    , nonH, nonX, nonFof, VH, uopLoad) + VpWen + Src1Gp + Src2Gp
    val vlse32      = Value(CS    , nonH, nonX, nonFof, VW, uopLoad) + VpWen + Src1Gp + Src2Gp
    val vlse64      = Value(CS    , nonH, nonX, nonFof, VD, uopLoad) + VpWen + Src1Gp + Src2Gp

    val vlm         = Value(MASK  , nonH, nonX, nonFof, VB, uopLoad) + VpWen + VmWen + Src1Gp

    val vlnre8      = Value(MASK  , nonH, nonX, nonFof, VB, uopLoad) + VpWen + Src1Gp
    val vlnre16     = Value(MASK  , nonH, nonX, nonFof, VH, uopLoad) + VpWen + Src1Gp
    val vlnre32     = Value(MASK  , nonH, nonX, nonFof, VW, uopLoad) + VpWen + Src1Gp
    val vlnre64     = Value(MASK  , nonH, nonX, nonFof, VD, uopLoad) + VpWen + Src1Gp

    val vluxei8e8   = Value(IUEI8 , nonH, nonX, nonFof, VB, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vluxei8e16  = Value(IUEI8 , nonH, nonX, nonFof, VH, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vluxei8e32  = Value(IUEI8 , nonH, nonX, nonFof, VW, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vluxei8e64  = Value(IUEI8 , nonH, nonX, nonFof, VD, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vluxei16e8  = Value(IUEI16, nonH, nonX, nonFof, VB, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vluxei16e16 = Value(IUEI16, nonH, nonX, nonFof, VH, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vluxei16e32 = Value(IUEI16, nonH, nonX, nonFof, VW, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vluxei16e64 = Value(IUEI16, nonH, nonX, nonFof, VD, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vluxei32e8  = Value(IUEI32, nonH, nonX, nonFof, VB, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vluxei32e16 = Value(IUEI32, nonH, nonX, nonFof, VH, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vluxei32e32 = Value(IUEI32, nonH, nonX, nonFof, VW, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vluxei32e64 = Value(IUEI32, nonH, nonX, nonFof, VD, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vluxei64e8  = Value(IUEI64, nonH, nonX, nonFof, VB, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vluxei64e16 = Value(IUEI64, nonH, nonX, nonFof, VH, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vluxei64e32 = Value(IUEI64, nonH, nonX, nonFof, VW, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vluxei64e64 = Value(IUEI64, nonH, nonX, nonFof, VD, uopLoad) + VpWen + Src1Gp + Src2Vp

    val vloxei8e8   = Value(IOEI8 , nonH, nonX, nonFof, VB, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vloxei8e16  = Value(IOEI8 , nonH, nonX, nonFof, VH, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vloxei8e32  = Value(IOEI8 , nonH, nonX, nonFof, VW, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vloxei8e64  = Value(IOEI8 , nonH, nonX, nonFof, VD, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vloxei16e8  = Value(IOEI16, nonH, nonX, nonFof, VB, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vloxei16e16 = Value(IOEI16, nonH, nonX, nonFof, VH, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vloxei16e32 = Value(IOEI16, nonH, nonX, nonFof, VW, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vloxei16e64 = Value(IOEI16, nonH, nonX, nonFof, VD, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vloxei32e8  = Value(IOEI32, nonH, nonX, nonFof, VB, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vloxei32e16 = Value(IOEI32, nonH, nonX, nonFof, VH, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vloxei32e32 = Value(IOEI32, nonH, nonX, nonFof, VW, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vloxei32e64 = Value(IOEI32, nonH, nonX, nonFof, VD, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vloxei64e8  = Value(IOEI64, nonH, nonX, nonFof, VB, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vloxei64e16 = Value(IOEI64, nonH, nonX, nonFof, VH, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vloxei64e32 = Value(IOEI64, nonH, nonX, nonFof, VW, uopLoad) + VpWen + Src1Gp + Src2Vp
    val vloxei64e64 = Value(IOEI64, nonH, nonX, nonFof, VD, uopLoad) + VpWen + Src1Gp + Src2Vp

    private val prefetchI = bb"0000"
    private val prefetchR = bb"0001"
    private val prefetchW = bb"0010"

    // Zicbop software prefetch
    val prefetch_i = Value(SCALA, nonH, nonX, prefetchI, uopPrefetch)
    val prefetch_r = Value(SCALA, nonH, nonX, prefetchR, uopPrefetch)
    val prefetch_w = Value(SCALA, nonH, nonX, prefetchW, uopPrefetch)

    def getUopType(op: UInt): UInt = op(0)

    def isPrefetch(op: UInt): Bool = getUopType(op) === uopPrefetch
  }

  trait StuOpcodes extends Opcodes with LsuTrait with DataType {
    protected val uopStore = bb"0"
    protected val uopCbo = bb"1"

    // Todo: Ugly encoding, just match defination in comments in [[xiangshan.cache.CMOReq]]
    protected object CBO {
      val clean = bb"000"
      val flush = bb"001"
      val inval = bb"010"
      val zero  = bb"011"
    }

    // store pipeline
    // normal store
    val sb = IntBSType(SCALA, nonH, nonX, sign, B, uopStore)
    val sh = IntBSType(SCALA, nonH, nonX, sign, H, uopStore)
    val sw = IntBSType(SCALA, nonH, nonX, sign, W, uopStore)
    val sd = IntBSType(SCALA, nonH, nonX, sign, D, uopStore)
    val sq = IntBSType(SCALA, nonH, nonX, sign, Q, uopStore) // TODO: no corresponding store instruction

    //hypervisor store
    val hsvb = IntBSType(SCALA, isH, nonX, sign, B, uopStore)
    val hsvh = IntBSType(SCALA, isH, nonX, sign, H, uopStore)
    val hsvw = IntBSType(SCALA, isH, nonX, sign, W, uopStore)
    val hsvd = IntBSType(SCALA, isH, nonX, sign, D, uopStore)

    def isHsv(op: UInt): Bool = op(6) === isH && op(0) === uopStore

    /**
     * Segment related fields are not encoded in Opcode.
     * Since segment number and the opcode are orthogonal, segment number will be passed separatedly in some field of bundle.
     */

    val vse8        = Value(US    , nonH, nonX, nonFof, VB, uopStore) + Src1Gp + Src3Vp
    val vse16       = Value(US    , nonH, nonX, nonFof, VH, uopStore) + Src1Gp + Src3Vp
    val vse32       = Value(US    , nonH, nonX, nonFof, VW, uopStore) + Src1Gp + Src3Vp
    val vse64       = Value(US    , nonH, nonX, nonFof, VD, uopStore) + Src1Gp + Src3Vp

    val vse8ff      = Value(US    , nonH, nonX, isFof , VB, uopStore) + Src1Gp + Src3Vp
    val vse16ff     = Value(US    , nonH, nonX, isFof , VH, uopStore) + Src1Gp + Src3Vp
    val vse32ff     = Value(US    , nonH, nonX, isFof , VW, uopStore) + Src1Gp + Src3Vp
    val vse64ff     = Value(US    , nonH, nonX, isFof , VD, uopStore) + Src1Gp + Src3Vp

    val vsse8       = Value(CS    , nonH, nonX, nonFof, VB, uopStore) + Src1Gp + Src3Vp + Src2Gp
    val vsse16      = Value(CS    , nonH, nonX, nonFof, VH, uopStore) + Src1Gp + Src3Vp + Src2Gp
    val vsse32      = Value(CS    , nonH, nonX, nonFof, VW, uopStore) + Src1Gp + Src3Vp + Src2Gp
    val vsse64      = Value(CS    , nonH, nonX, nonFof, VD, uopStore) + Src1Gp + Src3Vp + Src2Gp

    val vsm         = Value(MASK  , nonH, nonX, nonFof, VB, uopStore) + Src1Gp + Src3Vp

    val vsnre8      = Value(MASK  , nonH, nonX, nonFof, VB, uopStore) + Src1Gp + Src3Vp
    val vsnre16     = Value(MASK  , nonH, nonX, nonFof, VH, uopStore) + Src1Gp + Src3Vp
    val vsnre32     = Value(MASK  , nonH, nonX, nonFof, VW, uopStore) + Src1Gp + Src3Vp
    val vsnre64     = Value(MASK  , nonH, nonX, nonFof, VD, uopStore) + Src1Gp + Src3Vp

    val vsuxei8e8   = Value(IUEI8 , nonH, nonX, nonFof, VB, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsuxei8e16  = Value(IUEI8 , nonH, nonX, nonFof, VH, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsuxei8e32  = Value(IUEI8 , nonH, nonX, nonFof, VW, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsuxei8e64  = Value(IUEI8 , nonH, nonX, nonFof, VD, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsuxei16e8  = Value(IUEI16, nonH, nonX, nonFof, VB, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsuxei16e16 = Value(IUEI16, nonH, nonX, nonFof, VH, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsuxei16e32 = Value(IUEI16, nonH, nonX, nonFof, VW, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsuxei16e64 = Value(IUEI16, nonH, nonX, nonFof, VD, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsuxei32e8  = Value(IUEI32, nonH, nonX, nonFof, VB, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsuxei32e16 = Value(IUEI32, nonH, nonX, nonFof, VH, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsuxei32e32 = Value(IUEI32, nonH, nonX, nonFof, VW, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsuxei32e64 = Value(IUEI32, nonH, nonX, nonFof, VD, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsuxei64e8  = Value(IUEI64, nonH, nonX, nonFof, VB, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsuxei64e16 = Value(IUEI64, nonH, nonX, nonFof, VH, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsuxei64e32 = Value(IUEI64, nonH, nonX, nonFof, VW, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsuxei64e64 = Value(IUEI64, nonH, nonX, nonFof, VD, uopStore) + Src1Gp + Src3Vp + Src2Vp

    val vsoxei8e8   = Value(IOEI8 , nonH, nonX, nonFof, VB, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsoxei8e16  = Value(IOEI8 , nonH, nonX, nonFof, VH, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsoxei8e32  = Value(IOEI8 , nonH, nonX, nonFof, VW, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsoxei8e64  = Value(IOEI8 , nonH, nonX, nonFof, VD, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsoxei16e8  = Value(IOEI16, nonH, nonX, nonFof, VB, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsoxei16e16 = Value(IOEI16, nonH, nonX, nonFof, VH, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsoxei16e32 = Value(IOEI16, nonH, nonX, nonFof, VW, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsoxei16e64 = Value(IOEI16, nonH, nonX, nonFof, VD, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsoxei32e8  = Value(IOEI32, nonH, nonX, nonFof, VB, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsoxei32e16 = Value(IOEI32, nonH, nonX, nonFof, VH, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsoxei32e32 = Value(IOEI32, nonH, nonX, nonFof, VW, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsoxei32e64 = Value(IOEI32, nonH, nonX, nonFof, VD, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsoxei64e8  = Value(IOEI64, nonH, nonX, nonFof, VB, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsoxei64e16 = Value(IOEI64, nonH, nonX, nonFof, VH, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsoxei64e32 = Value(IOEI64, nonH, nonX, nonFof, VW, uopStore) + Src1Gp + Src3Vp + Src2Vp
    val vsoxei64e64 = Value(IOEI64, nonH, nonX, nonFof, VD, uopStore) + Src1Gp + Src3Vp + Src2Vp

    // l1 cache op
    val cbo_zero  = IntIType(SCALA, nonH, nonX, sign, CBO.zero , uopCbo) + NoSpec + BlockBack
    // llc op
    val cbo_clean = IntIType(SCALA, nonH, nonX, sign, CBO.clean, uopCbo) + NoSpec + BlockBack
    val cbo_flush = IntIType(SCALA, nonH, nonX, sign, CBO.flush, uopCbo) + NoSpec + BlockBack
    val cbo_inval = IntIType(SCALA, nonH, nonX, sign, CBO.inval, uopCbo) + NoSpec + BlockBack

    def getCmoOpcode(op: UInt): UInt = op(3, 1)
    def isCbo(op: UInt): Bool = op(0) === uopCbo && getCmoOpcode(op) === CBO.zero
    def isCboAll(op: UInt): Bool = op(0) === uopCbo
    def isCboClean(op: UInt): Bool = isCbo(op) && getCmoOpcode(op) === CBO.clean
    def isCboFlush(op: UInt): Bool = isCbo(op) && getCmoOpcode(op) === CBO.flush
    def isCboInval(op: UInt): Bool = isCbo(op) && getCmoOpcode(op) === CBO.inval
  }

  trait AmoOpcodes extends Opcodes with LsuTrait {


    protected val noALU = bb"0"
    protected val withALU = bb"1"

    object NoALU {
      val lr   = bb"000"
      val sc   = bb"001"
      val swap = bb"100"
      val cas  = bb"101"
    }

    object WithALU {
      val add  = bb"000"
      val xor  = bb"001"
      val and  = bb"011"
      val or   = bb"010"
      val min  = bb"100"
      val max  = bb"101"
      val minu = bb"110"
      val maxu = bb"111"
    }


    // atomics
    // bit(1, 0) are size
    // since atomics use a different fu type
    // so we can safely reuse other load/store's encodings
    // bit encoding: | optype (3bit) | size (3bit) | alu (1bit) |
    def AMOFuOpWidth = 7
    //                    4b     3b             3b 1b
    val amoswap_b = IntRType(SCALA, NoALU.swap   , B, noALU)   + NoSpec + BlockBack
    val amoadd_b  = IntRType(SCALA, WithALU.add  , B, withALU) + NoSpec + BlockBack
    val amoxor_b  = IntRType(SCALA, WithALU.xor  , B, withALU) + NoSpec + BlockBack
    val amoand_b  = IntRType(SCALA, WithALU.and  , B, withALU) + NoSpec + BlockBack
    val amoor_b   = IntRType(SCALA, WithALU.or   , B, withALU) + NoSpec + BlockBack
    val amomin_b  = IntRType(SCALA, WithALU.min  , B, withALU) + NoSpec + BlockBack
    val amomax_b  = IntRType(SCALA, WithALU.max  , B, withALU) + NoSpec + BlockBack
    val amominu_b = IntRType(SCALA, WithALU.minu , B, withALU) + NoSpec + BlockBack
    val amomaxu_b = IntRType(SCALA, WithALU.maxu , B, withALU) + NoSpec + BlockBack

    val amoswap_h = IntRType(SCALA, NoALU.swap   , H, noALU)   + NoSpec + BlockBack
    val amoadd_h  = IntRType(SCALA, WithALU.add  , H, withALU) + NoSpec + BlockBack
    val amoxor_h  = IntRType(SCALA, WithALU.xor  , H, withALU) + NoSpec + BlockBack
    val amoand_h  = IntRType(SCALA, WithALU.and  , H, withALU) + NoSpec + BlockBack
    val amoor_h   = IntRType(SCALA, WithALU.or   , H, withALU) + NoSpec + BlockBack
    val amomin_h  = IntRType(SCALA, WithALU.min  , H, withALU) + NoSpec + BlockBack
    val amomax_h  = IntRType(SCALA, WithALU.max  , H, withALU) + NoSpec + BlockBack
    val amominu_h = IntRType(SCALA, WithALU.minu , H, withALU) + NoSpec + BlockBack
    val amomaxu_h = IntRType(SCALA, WithALU.maxu , H, withALU) + NoSpec + BlockBack

    val lr_w      = IntIType(SCALA, NoALU.lr     , W, noALU) + NoSpec + BlockBack
    val sc_w      = IntRType(SCALA, NoALU.sc     , W, noALU)   + NoSpec + BlockBack
    val amoswap_w = IntRType(SCALA, NoALU.swap   , W, noALU)   + NoSpec + BlockBack
    val amocas_w  = IntRType(SCALA, NoALU.cas    , W, noALU)   + NoSpec + BlockBack
    val amoadd_w  = IntRType(SCALA, WithALU.add  , W, withALU) + NoSpec + BlockBack
    val amoxor_w  = IntRType(SCALA, WithALU.xor  , W, withALU) + NoSpec + BlockBack
    val amoand_w  = IntRType(SCALA, WithALU.and  , W, withALU) + NoSpec + BlockBack
    val amoor_w   = IntRType(SCALA, WithALU.or   , W, withALU) + NoSpec + BlockBack
    val amomin_w  = IntRType(SCALA, WithALU.min  , W, withALU) + NoSpec + BlockBack
    val amomax_w  = IntRType(SCALA, WithALU.max  , W, withALU) + NoSpec + BlockBack
    val amominu_w = IntRType(SCALA, WithALU.minu , W, withALU) + NoSpec + BlockBack
    val amomaxu_w = IntRType(SCALA, WithALU.maxu , W, withALU) + NoSpec + BlockBack

    val lr_d      = IntIType(SCALA, NoALU.lr     , D, noALU) + NoSpec + BlockBack
    val sc_d      = IntRType(SCALA, NoALU.sc     , D, noALU)   + NoSpec + BlockBack
    val amoswap_d = IntRType(SCALA, NoALU.swap   , D, noALU)   + NoSpec + BlockBack
    val amoadd_d  = IntRType(SCALA, WithALU.add  , D, withALU) + NoSpec + BlockBack
    val amoxor_d  = IntRType(SCALA, WithALU.xor  , D, withALU) + NoSpec + BlockBack
    val amoand_d  = IntRType(SCALA, WithALU.and  , D, withALU) + NoSpec + BlockBack
    val amoor_d   = IntRType(SCALA, WithALU.or   , D, withALU) + NoSpec + BlockBack
    val amomin_d  = IntRType(SCALA, WithALU.min  , D, withALU) + NoSpec + BlockBack
    val amomax_d  = IntRType(SCALA, WithALU.max  , D, withALU) + NoSpec + BlockBack
    val amominu_d = IntRType(SCALA, WithALU.minu , D, withALU) + NoSpec + BlockBack
    val amomaxu_d = IntRType(SCALA, WithALU.maxu , D, withALU) + NoSpec + BlockBack

    val amocas_b  = IntRType(SCALA, NoALU.cas, B, noALU) + NoSpec + BlockBack
    val amocas_h  = IntRType(SCALA, NoALU.cas, H, noALU) + NoSpec + BlockBack
    val amocas_d  = IntRType(SCALA, NoALU.cas, D, noALU) + NoSpec + BlockBack
    val amocas_q  = IntRType(SCALA, NoALU.cas, Q, noALU) + NoSpec + BlockBack

    def getAmocasUopIdx(opType: UInt): UInt = (opType >> this.AMOFuOpWidth).asUInt

    def getAmoOp(op: UInt): UInt = op(6, 4) ## op(0)
    def getAmoOpAndSize(op: UInt): UInt = op(6, 0)

    // for easy usage
    object AmoOp {
      val lr   = NoALU.lr     ## noALU
      val sc   = NoALU.sc     ## noALU
      val swap = NoALU.swap   ## noALU
      val cas  = NoALU.cas    ## noALU
      val add  = WithALU.add  ## withALU
      val xor  = WithALU.xor  ## withALU
      val and  = WithALU.and  ## withALU
      val or   = WithALU.or   ## withALU
      val min  = WithALU.min  ## withALU
      val max  = WithALU.max  ## withALU
      val minu = WithALU.minu ## withALU
      val maxu = WithALU.maxu ## withALU
    }

    def isLr      (op: UInt): Bool = getAmoOp(op) === AmoOp.lr
    def isSc      (op: UInt): Bool = getAmoOp(op) === AmoOp.sc
    def isAMOCAS  (op: UInt): Bool = getAmoOp(op) === AmoOp.cas
    def isAMOCASQ (op: UInt): Bool = isAMOCAS(op) && size(op) === Q
    def isAMOCASWD(op: UInt): Bool = isAMOCAS(op) && (size(op) === W || size(op) === D)

    object DifftestOpcode {
      def lr_w      = "b000010".U
      def sc_w      = "b000110".U
      def amoswap_w = "b001010".U
      def amoadd_w  = "b001110".U
      def amoxor_w  = "b010010".U
      def amoand_w  = "b010110".U
      def amoor_w   = "b011010".U
      def amomin_w  = "b011110".U
      def amomax_w  = "b100010".U
      def amominu_w = "b100110".U
      def amomaxu_w = "b101010".U
      def amocas_w  = "b101110".U

      def lr_d      = "b000011".U
      def sc_d      = "b000111".U
      def amoswap_d = "b001011".U
      def amoadd_d  = "b001111".U
      def amoxor_d  = "b010011".U
      def amoand_d  = "b010111".U
      def amoor_d   = "b011011".U
      def amomin_d  = "b011111".U
      def amomax_d  = "b100011".U
      def amominu_d = "b100111".U
      def amomaxu_d = "b101011".U
      def amocas_d  = "b101111".U

      def amocas_q  = "b101100".U
    }

    val difftestAmoOpMap: Seq[(BitPat, UInt)] = Seq(
      lr_w      -> DifftestOpcode.lr_w,
      sc_w      -> DifftestOpcode.sc_w,
      amoswap_w -> DifftestOpcode.amoswap_w,
      amocas_w  -> DifftestOpcode.amocas_w,
      amoadd_w  -> DifftestOpcode.amoadd_w,
      amoxor_w  -> DifftestOpcode.amoxor_w,
      amoand_w  -> DifftestOpcode.amoand_w,
      amoor_w   -> DifftestOpcode.amoor_w,
      amomin_w  -> DifftestOpcode.amomin_w,
      amomax_w  -> DifftestOpcode.amomax_w,
      amominu_w -> DifftestOpcode.amominu_w,
      amomaxu_w -> DifftestOpcode.amomaxu_w,

      lr_d -> DifftestOpcode.lr_d,
      sc_d -> DifftestOpcode.sc_d,
      amoswap_d -> DifftestOpcode.amoswap_d,
      amocas_d  -> DifftestOpcode.amocas_d,
      amoadd_d  -> DifftestOpcode.amoadd_d,
      amoxor_d  -> DifftestOpcode.amoxor_d,
      amoand_d  -> DifftestOpcode.amoand_d,
      amoor_d   -> DifftestOpcode.amoor_d,
      amomin_d  -> DifftestOpcode.amomin_d,
      amomax_d  -> DifftestOpcode.amomax_d,
      amominu_d -> DifftestOpcode.amominu_d,
      amomaxu_d -> DifftestOpcode.amomaxu_d,

      amocas_q  -> DifftestOpcode.amocas_q,
    ).map{ case (k, v) => k.encode -> v }
  }

  object LduOpcodes extends LduOpcodes

  object StuOpcodes extends StuOpcodes

  object AmoOpcodes extends AmoOpcodes

  object LsuOpcodes extends LduOpcodes with StuOpcodes with AmoOpcodes

  object CsrOpcodes extends Opcodes {
    //                        | func3|
    val jmp     = IntIType(bb"010_000") - Src1Gp + BlockBack + NoSpec
    val wfi     = IntIType(bb"100_000") - Src1Gp + BlockBack + NoSpec
    val wrs_nto = IntIType(bb"100_010") - Src1Gp + BlockBack + NoSpec
    val wrs_sto = IntIType(bb"100_011") - Src1Gp + BlockBack + NoSpec
    val wrt     = IntIType(bb"001_001")          + BlockBack + NoSpec
    val set     = IntIType(bb"001_010")          + BlockBack + NoSpec
    val clr     = IntIType(bb"001_011")          + BlockBack + NoSpec
    val wrti    = IntIType(bb"001_101")          + BlockBack + NoSpec
    val seti    = IntIType(bb"001_110")          + BlockBack + NoSpec
    val clri    = IntIType(bb"001_111")          + BlockBack + NoSpec

    def isSystemOp (op: UInt): Bool = op(4)
    def isWfi      (op: UInt): Bool = op(5) && !op(1)
    def isWrsNto   (op: UInt): Bool = op(5) && op(1, 0) === "b10".U
    def isWrsSto   (op: UInt): Bool = op(5) && op(1, 0) === "b11".U
    def isCsrAccess(op: UInt): Bool = op(3)
    def isReadOnly (op: UInt): Bool = op(3) && op(2, 0) === 0.U
    def notReadOnly(op: UInt): Bool = op(3) && op(2, 0) =/= 0.U
    def isCSRRW    (op: UInt): Bool = op(3) && op(1, 0) === "b01".U
    def isCSRRSorRC(op: UInt): Bool = op(3) && op(1)

    def getCSROp(op: UInt) = op(1, 0)
    def needImm(op: UInt) = op(2)

    def getFunc3(op: UInt) = op(2, 0)
  }

  object FenceOpcodes extends Opcodes {
    val fence    = IntIType(bb"10000") - Src1Gp + NoSpec + BlockBack + FlushPipe // FENCE           / PAUSE
    val sfence   = IntIType(bb"10001") - Src1Gp + NoSpec + BlockBack + FlushPipe // SFENCE_VMA      / SINVAL_VMA (no flushpipe)
    val fencei   = IntIType(bb"10010") - Src1Gp + NoSpec + BlockBack + FlushPipe // FENCE_I
    val hfence_v = IntIType(bb"10011") - Src1Gp + NoSpec + BlockBack + FlushPipe // HFENCE_VVMA     / HINVAL_VVMA (no flushpipe)
    val hfence_g = IntIType(bb"10100") - Src1Gp + NoSpec + BlockBack + FlushPipe // HFENCE_GVMA     / HINVAL_GVMA (no flushpipe)
    val nofence  = IntIType(bb"00000") - Src1Gp + NoSpec + BlockBack + FlushPipe // SFENCE_INVAL_IR / SFENCE_W_INVAL (no flushpipe)
  }

  object BkuOpcodes extends Opcodes {
    val clmul       = IntRType(bb"000000")
    val clmulh      = IntRType(bb"000001")
    val clmulr      = IntRType(bb"000010")
    val xpermn      = IntRType(bb"000100")
    val xpermb      = IntRType(bb"000101")

    val clz         = IntIType(bb"001000")
    val clzw        = IntIType(bb"001001")
    val ctz         = IntIType(bb"001010")
    val ctzw        = IntIType(bb"001011")
    val cpop        = IntIType(bb"001100")
    val cpopw       = IntIType(bb"001101")

    // 01xxxx is reserve
    val aes64es     = IntRType(bb"100000")
    val aes64esm    = IntRType(bb"100001")
    val aes64ds     = IntRType(bb"100010")
    val aes64dsm    = IntRType(bb"100011")
    val aes64im     = IntIType(bb"100100")
    val aes64ks1i   = IntIType(bb"100101")
    val aes64ks2    = IntRType(bb"100110")

    // merge to two instruction sm4ks & sm4ed
    val sm4ed0      = IntRType(bb"101000")
    val sm4ed1      = IntRType(bb"101001")
    val sm4ed2      = IntRType(bb"101010")
    val sm4ed3      = IntRType(bb"101011")
    val sm4ks0      = IntRType(bb"101100")
    val sm4ks1      = IntRType(bb"101101")
    val sm4ks2      = IntRType(bb"101110")
    val sm4ks3      = IntRType(bb"101111")

    val sha256sum0  = IntIType(bb"110000")
    val sha256sum1  = IntIType(bb"110001")
    val sha256sig0  = IntIType(bb"110010")
    val sha256sig1  = IntIType(bb"110011")
    val sha512sum0  = IntIType(bb"110100")
    val sha512sum1  = IntIType(bb"110101")
    val sha512sig0  = IntIType(bb"110110")
    val sha512sig1  = IntIType(bb"110111")

    val sm3p0       = IntIType(bb"111000")
    val sm3p1       = IntIType(bb"111001")
  }

  object I2fOpcodes extends Opcodes.FCvtOpcode

  trait FDivOpcodes extends Opcodes with DataType {
    private val FDIV  = bb"0"
    private val FSQRT = bb"1"

    val fdiv_fp16 : Opcode = FpRTypeFpDestInst(FDIV , FP16, F)
    val fdiv_fp32 : Opcode = FpRTypeFpDestInst(FDIV , FP32, F)
    val fdiv_fp64 : Opcode = FpRTypeFpDestInst(FDIV , FP64, F)
    val fsqrt_fp16: Opcode = FpITypeF2fInst(FSQRT, FP16, F)
    val fsqrt_fp32: Opcode = FpITypeF2fInst(FSQRT, FP32, F)
    val fsqrt_fp64: Opcode = FpITypeF2fInst(FSQRT, FP64, F)

    val vfdiv_fp16 : Opcode = DvSvlS2vS1(FDIV , FP16, V)
    val vfsqrt_fp16: Opcode = DvSvlS2vS1(FSQRT, FP16, V)
    val vfdiv_fp32 : Opcode = DvSvlS2vS1(FDIV , FP32, V)
    val vfsqrt_fp32: Opcode = DvSvlS2vS1(FSQRT, FP32, V)
    val vfdiv_fp64 : Opcode = DvSvlS2vS1(FDIV , FP64, V)
    val vfsqrt_fp64: Opcode = DvSvlS2vS1(FSQRT, FP64, V)
  }

  object FDivOpcodes extends FDivOpcodes

  object FAluOpcodes extends Opcodes.FMiscOpcode
  object VFRedOpcodes extends Opcodes.VFRedOpcode
  object VFDivOpcodes extends Opcodes.VFDivOpcode

  trait VSetOpcodes extends Opcodes {
    // vtype is from imm
    private val vtypeI   = bb"0"
    // vtype is from gp
    private val vtypeX   = bb"1"
    private val avlX     = bb"00"
    private val avlVlmax = bb"01"
    private val avlVl    = bb"10"
    private val avlImm   = bb"11"

    private val isIll = bb"1"
    private val notIll = bb"0"

    private val vset = bb"0"
    private val rdvl = bb"1"

    private def dc(n: Int) = BitPat.dontCare(n)

    /**
     * [[uvset_vtypex_vlx]] is used for VSETVL when rs1 != x0
     * [[uvset_vtypex_vlmax]] is used for VSETVL when rs1 == x0 and rd != x0
     * [[uvset_vtypex_vll]] is used for VSETVL when rs1 == x0 and rd == x0
     * [[uvset_vtypei_vlx]] is used for VSETVLI when rs1 != x0
     * [[uvset_vtypei_vlmax]] is used for VSETVLI when rs1 == x0
     * [[uvset_vtypei_nop]] is used for VSETVLI when rs1 == x0 and rd == x0
     * This uop does not change vl but modifies vtype.
     * if vlmax shrink, [[uvset_vtypei_ill]] should be used to set vill
     * [[uvset_vtypei_vli]] is used for VSETIVLI
     * [[uvset_ill]] is used for illegal VSETVLI and VSETIVLI when rs1 == x0 and rd == x0.
     * When rs1 == x0, rd == x0 and SEW/LMUL ratio is changed, the instruction is reserved.
     * This uop will set vill = 1 and vl = 0.
     */

    val uvset_vtypex_vlx   = Value(vset, notIll, vtypeX, avlX)     + GpWen + VlWen + Src2Gp + Src1Gp
    val uvset_vtypex_vlmax = Value(vset, notIll, vtypeX, avlVlmax) + GpWen + VlWen + Src2Gp
    val uvset_vtypex_vll   = Value(vset, notIll, vtypeX, avlVl)            + VlWen + Src2Gp          + VlRen
    val uvset_vtypei_vlx   = Value(vset, notIll, vtypeI, avlX)     + GpWen + VlWen          + Src1Gp
    val uvset_vtypei_vlmax = Value(vset, notIll, vtypeI, avlVlmax) + GpWen + VlWen
    val uvset_vtypei_vli   = Value(vset, notIll, vtypeI, avlImm)   + GpWen + VlWen
    val uvset_vtypei_nop   = Value(vset, notIll, vtypeI, avlVl)
    val uvset_ill          = Value(vset, isIll,  vtypeI, avlImm)   + GpWen + VlWen
    val readvl             = Value(rdvl, notIll, dc(1),  avlVl)    + GpWen                           + VlRen

    def getVlType(op: UInt): UInt = op(1, 0)
    def getVTypeType(op: UInt): UInt = op(2)
    def isIll(op: UInt): Bool = op(3) === isIll
    def isVSet(op: UInt): Bool = op(4) === vset

    def isVSetvl(op: UInt): Bool = isVSet(op) && getVTypeType(op) === vtypeX
    def isVSetvli(op: UInt): Bool = isVSet(op) && getVTypeType(op) === vtypeI && getVlType(op) =/= avlImm
    def isVSetivli(op: UInt): Bool = isVSet(op) && getVTypeType(op) === vtypeI && getVlType(op) === avlImm
    def isReadVl(op: UInt): Bool = !isVSet(op)

    def vlIsReg(op: UInt): Bool = getVlType(op) === avlX
    def vlIsVlmax(op: UInt): Bool = getVlType(op) === avlVlmax
    def vlIsKeep(op: UInt): Bool = getVlType(op) === avlVl
    def vlIsImm(op: UInt): Bool = getVlType(op) === avlImm
    def rdIsZero(op: UInt): Bool = getVlType(op) === avlVl
    def rs1IsZero(op: UInt): Bool = Cat(Seq(avlVl, avlVlmax).map(_ === getVlType(op))).orR
  }

  object VSetOpcodes extends VSetOpcodes

  val ALUOpType = AluOpcodes
  val BRUOpType = BruOpcodes
  val JumpOpType = JmpOpcodes
  val NewJumpOpType = NewJmpOpcodes
  val LinkOpType = LinkOpcodes
  val FenceOpType = FenceOpcodes
  val MULOpType = MulOpcodes
  val DIVOpType = DivOpcodes
  val CSROpType = CsrOpcodes
  val LSUOpType = LsuOpcodes
  val BKUOpType = BkuOpcodes

  class OpcodeUtil(opcode: Opcode) {
    def traits: Set[OpcodeTrait] = opcode.getTraits

    def vsi: Opcode = {
      opcode + Src1Imm(DecodeSelImm.OPIVIS)
    }

    def vui: Opcode = {
      opcode + Src1Imm(DecodeSelImm.OPIVIU)
    }

    def dx: Opcode = {
      opcode + GpWen
    }

    def df: Opcode = {
      opcode + FpWen
    }

    /**
     * Generate the BitPat for uop-info to rename stage based on the traits of the opcode.
     * The order of the bits in the BitPat should be consistent with the order of the fields in the UopInfoRename.
     * @return the BitPat for uop-info to rename stage
     */
    def genUopInfoRenameBitPat: BitPat = {
      UopInfoRename.genBitPat(
        src1Type = this.getSrc1Type,
        src2Type = this.getSrc2Type,
        vlRen = traits.contains(VlRen),
        maskType = this.getMaskType,
        intRmRen = traits.contains(VxrmRen),
        readVdAsSrc = this.getSrc3Type.contains(Operand.VP),
        gpWen = traits.contains(GpWen),
        fpWen = traits.contains(FpWen),
        vpWen = traits.exists(_.isInstanceOf[VecWenTrait]),
        vlWen = traits.contains(VlWen),
        vxsatWen = traits.contains(VxsatWen),
        vdAlloc = !traits.contains(NoDestAlloc),
      )
    }

    def genUopInfoRenameSimpleBitPat: BitPat = {
      UopInfoRenameSimple.genBitPat(
        src1Type = this.getSrc1Type,
        src2Type = this.getSrc2Type,
        src3Type = this.getSrc3Type,
        gpWen = traits.contains(GpWen), // further modified at SimpleDecoddeChannel for RD=0 won't write back
        fpWen = traits.contains(FpWen),
        noSpec = traits.contains(NoSpec),
        blockBack = traits.contains(BlockBack),
        flushPipe = traits.contains(FlushPipe),
      )
    }

    def getSrc1Type: Option[OperandType] = {
      val ts = opcode.getTraits.collect{ case t : Src1Trait => t }.toSeq

      require(
        ts.size <= 1,
        s"opcode${opcode} should only contain one Src1Trait, but it has $ts"
      )

      getSrcType(ts.headOption.map(_.srcType))
    }

    def getSrc2Type: Option[OperandType] = {
      val ts = opcode.getTraits.collect{ case t : Src2Trait => t }.toSeq

      require(
        ts.size <= 1,
        s"opcode${opcode} should only contain one Src2Trait, but it has $ts"
      )

      getSrcType(ts.headOption.map(_.srcType))
    }

    def getSrc3Type: Option[OperandType] = {
      val ts = opcode.getTraits.collect{ case t : Src3Trait => t }.toSeq

      require(
        ts.size <= 1,
        s"opcode${opcode} should only contain one Src3Trait, but it has $ts"
      )

      getSrcType(ts.headOption.map(_.srcType))
    }

    def getMaskType: MaskType = {
      val ts = opcode.getTraits.collect{ case t : MaskTrait => t }.toSeq

      require(
        ts.size <= 1,
        s"opcode${opcode} should only contain one MaskTrait, but it has $ts"
      )

      ts.headOption match {
        case Some(typ) =>
          typ match {
            case OpcodeTraits.DestMask => Types.DestMask
            case OpcodeTraits.NoMask => Types.NoMask
            case OpcodeTraits.Src12Mask => Types.Src12Mask
            case OpcodeTraits.Src2Mask => Types.Src2Mask
            case _ => ???
          }
        case None => Types.NoMask
      }
    }

    private def getSrcType(srcType: Option[SrcType]): Option[OperandType] = srcType.map {
      case Gp => Operand.GP
      case Fp => Operand.FP
      case Vp | Vs | Vw | Vws => Operand.VP
      case Imm => Operand.IMM
    }
  }

  implicit def toOpcodeUtil(opcode: Opcode): OpcodeUtil = new OpcodeUtil(opcode)
}
