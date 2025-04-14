package xiangshan.backend.decode.opcode

import chisel3.util.BitPat.bitPatToUInt
import chisel3.util._
import chisel3.{UInt, _}
import xiangshan.backend.decode.opcode.OpcodeTraits._
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

  val opcodes = yunsuan.encoding.Opcode.Opcodes
  val VIAluOpcodes  = opcodes.VIAluOpcode
  val VMAluOpcodes  = opcodes.VMAluOpcode
  val VIMacOpcodes  = opcodes.VIMacOpcode
  val VIDivOpcodes  = opcodes.VIDivOpcode
  val VIRedOpcodes  = opcodes.VIRedOpcode
  val VIPermOpcodes = opcodes.VIPermOpcode
  val VMoveOpcodes  = opcodes.VMoveOpcode

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
    val slliuw     = Value(bb"000_0000")
    val sll        = Value(bb"000_0001")

    // bclr:    src1 & ~(1 << src2[5:0])
    // bset:    src1 | (1 << src2[5:0])
    // binv:    src1 ^ (1 << src2[5:0])
    val bclr       = Value(bb"000_0010")
    val bset       = Value(bb"000_0011")
    val binv       = Value(bb"000_0100")

    // srl:     src1 >> src2
    // bext:    (src1 >> src2)[0]
    // sra:     src1 >> src2 (arithmetic)
    val srl        = Value(bb"000_0101")
    val bext       = Value(bb"000_0110")
    val sra        = Value(bb"000_0111")

    // rol:     (src1 << src2) | (src1 >> (xlen - src2))
    // ror:     (src1 >> src2) | (src1 << (xlen - src2))
    val rol        = Value(bb"000_1001")
    val ror        = Value(bb"000_1011")

    // addw:      SEXT((src1 + src2)[31:0])
    // oddaddw:   SEXT((src1[0] + src2)[31:0])
    // subw:      SEXT((src1 - src2)[31:0])
    // lui32addw: SEXT(SEXT(src2[11:0], 32) + {src2[31:12], 12'b0}, 64)
    val addw       = Value(bb"001_0000")
    val oddaddw    = Value(bb"001_0001")
    val subw       = Value(bb"001_0010")
    val lui32addw  = Value(bb"001_0011")

    // addwbit:   (src1 + src2)[0]
    // addwbyte:  (src1 + src2)[7:0]
    // addwzexth: ZEXT((src1  + src2)[15:0])
    // addwsexth: SEXT((src1  + src2)[15:0])
    val addwbit    = Value(bb"001_0100")
    val addwbyte   = Value(bb"001_0101")
    val addwzexth  = Value(bb"001_0110")
    val addwsexth  = Value(bb"001_0111")

    // sllw:     SEXT((src1 << src2)[31:0])
    // srlw:     SEXT((src1[31:0] >> src2)[31:0])
    // sraw:     SEXT((src1[31:0] >> src2)[31:0])
    val sllw       = Value(bb"001_1000")
    val srlw       = Value(bb"001_1001")
    val sraw       = Value(bb"001_1010")
    val rolw       = Value(bb"001_1100")
    val rorw       = Value(bb"001_1101")

    // adduw:  src1[31:0]  + src2
    // oddadd:  src1[0]     + src2
    // add:     src1        + src2
    // lui32add: SEXT(src2[11:0]) + {src2[63:12], 12'b0}
    val adduw      = Value(bb"010_0000")
    val oddadd     = Value(bb"010_0001")
    val add        = Value(bb"010_0010")
    val lui32add   = Value(bb"010_0011")

    // sr29add: src1[63:29] + src2
    // sr30add: src1[63:30] + src2
    // sr31add: src1[63:31] + src2
    // sr32add: src1[63:32] + src2
    val sr29add    = Value(bb"010_0100")
    val sr30add    = Value(bb"010_0101")
    val sr31add    = Value(bb"010_0110")
    val sr32add    = Value(bb"010_0111")

    // sh1adduw: {src1[31:0], 1'b0} + src2
    // sh1add: {src1[62:0], 1'b0} + src2
    // sh2add_uw: {src1[31:0], 2'b0} + src2
    // sh2add: {src1[61:0], 2'b0} + src2
    // sh3add_uw: {src1[31:0], 3'b0} + src2
    // sh3add: {src1[60:0], 3'b0} + src2
    // sh4add: {src1[59:0], 4'b0} + src2
    val sh1adduw   = Value(bb"010_1000")
    val sh1add     = Value(bb"010_1001")
    val sh2adduw   = Value(bb"010_1010")
    val sh2add     = Value(bb"010_1011")
    val sh3adduw   = Value(bb"010_1100")
    val sh3add     = Value(bb"010_1101")
    val sh4add     = Value(bb"010_1111")

    // SUB-op: src1 - src2
    val sub        = Value(bb"011_0000")
    val sltu       = Value(bb"011_0001")
    val slt        = Value(bb"011_0010")
    val maxu       = Value(bb"011_0100")
    val minu       = Value(bb"011_0101")
    val max        = Value(bb"011_0110")
    val min        = Value(bb"011_0111")

    // Zicond
    val czero_eqz  = Value(bb"111_0100")
    val czero_nez  = Value(bb"111_0110")

    // misc optype
    val and        = Value(bb"100_0000")
    val andn       = Value(bb"100_0001")
    val or         = Value(bb"100_0010")
    val orn        = Value(bb"100_0011")
    val xor        = Value(bb"100_0100")
    val xnor       = Value(bb"100_0101")
    val orcb       = Value(bb"100_0110")

    val sextb      = Value(bb"100_1000")
    val packh      = Value(bb"100_1001")
    val sexth      = Value(bb"100_1010")
    val packw      = Value(bb"100_1011")

    val revb       = Value(bb"101_0000")
    val rev8       = Value(bb"101_0001")
    val pack       = Value(bb"101_0010")
    val orh48      = Value(bb"101_0011")

    val szewl1     = Value(bb"101_1000")
    val szewl2     = Value(bb"101_1001")
    val szewl3     = Value(bb"101_1010")
    val byte2      = Value(bb"101_1011")

    val andlsb     = Value(bb"110_0000")
    val andzexth   = Value(bb"110_0001")
    val orlsb      = Value(bb"110_0010")
    val orzexth    = Value(bb"110_0011")
    val xorlsb     = Value(bb"110_0100")
    val xorzexth   = Value(bb"110_0101")
    val orcblsb    = Value(bb"110_0110")
    val orcbzexth  = Value(bb"110_0111")
    // for xstrap
    val xstrap     = Value(bb"111_1111")

    this.all.foreach(_ + GpWen)
    this.all.foreach(_ + Src1Gp)
    (this.all.toSet -- Seq(sextb, sexth, orcb, rev8, revb)).foreach(_ + Src2En)

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
  }

  object BruOpcodes extends Opcodes {
    // branch
    val beq        = Value(bb"000_000")
    val bne        = Value(bb"000_001")
    val blt        = Value(bb"000_100")
    val bge        = Value(bb"000_101")
    val bltu       = Value(bb"001_000")
    val bgeu       = Value(bb"001_001")

    def getBranchType(func: UInt) = func(3, 1)
    def isBranchInvert(func: UInt) = func(0)
  }

  object JmpOpcodes extends Opcodes {
    val jal        = Value(bb"111_1000")
    val jalr       = Value(bb"111_1001")
    val auipc      = Value(bb"111_1010")

    def jumpOpisJalr(op: UInt) = op(0)
    def jumpOpisAuipc(op: UInt) = op(1)
  }

  object MulOpcodes extends Opcodes {
    // mul
    // bit encoding: | type (2bit) | isWord(1bit) | opcode(2bit) |
    val mul    = Value(bb"00000")
    val mulh   = Value(bb"00001")
    val mulhsu = Value(bb"00010")
    val mulhu  = Value(bb"00011")
    val mulw   = Value(bb"00100")
    val mulw7  = Value(bb"01100")
    def isSign(op: UInt) = !op(1)
    def isW(op: UInt) = op(2)
    def isH(op: UInt) = op(1, 0) =/= 0.U
    def getOp(op: UInt) = Cat(op(3), op(1, 0))
  }

  object DivOpcodes extends Opcodes {
    // div
    // bit encoding: | type (2bit) | isWord(1bit) | isSign(1bit) | opcode(1bit) |
    val div    = Value(bb"10000")
    val divu   = Value(bb"10010")
    val rem    = Value(bb"10001")
    val remu   = Value(bb"10011")

    val divw   = Value(bb"10100")
    val divuw  = Value(bb"10110")
    val remw   = Value(bb"10101")
    val remuw  = Value(bb"10111")

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
    val lb     = Value(SCALA, nonH, nonX, sign  , B, uopLoad)
    val lh     = Value(SCALA, nonH, nonX, sign  , H, uopLoad)
    val lw     = Value(SCALA, nonH, nonX, sign  , W, uopLoad)
    val ld     = Value(SCALA, nonH, nonX, sign  , D, uopLoad)
    val lq     = Value(SCALA, nonH, nonX, sign  , Q, uopLoad)
    val lbu    = Value(SCALA, nonH, nonX, unsign, B, uopLoad)
    val lhu    = Value(SCALA, nonH, nonX, unsign, H, uopLoad)
    val lwu    = Value(SCALA, nonH, nonX, unsign, W, uopLoad)
    // hypervior load
    val hlvb   = Value(SCALA, isH, nonX, sign  , B, uopLoad)
    val hlvh   = Value(SCALA, isH, nonX, sign  , H, uopLoad)
    val hlvw   = Value(SCALA, isH, nonX, sign  , W, uopLoad)
    val hlvd   = Value(SCALA, isH, nonX, sign  , D, uopLoad)
    val hlvbu  = Value(SCALA, isH, nonX, unsign, B, uopLoad)
    val hlvhu  = Value(SCALA, isH, nonX, unsign, H, uopLoad)
    val hlvwu  = Value(SCALA, isH, nonX, unsign, W, uopLoad)
    val hlvxhu = Value(SCALA, isH, isX , unsign, H, uopLoad)
    val hlvxwu = Value(SCALA, isH, isX , unsign, W, uopLoad)

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

    val vlm         = Value(MASK  , nonH, nonX, nonFof, VB, uopLoad) + VpWen + Src1Gp

    val vlnr8       = Value(MASK  , nonH, nonX, nonFof, VB, uopLoad) + VpWen + Src1Gp
    val vlnr16      = Value(MASK  , nonH, nonX, nonFof, VH, uopLoad) + VpWen + Src1Gp
    val vlnr32      = Value(MASK  , nonH, nonX, nonFof, VW, uopLoad) + VpWen + Src1Gp
    val vlnr64      = Value(MASK  , nonH, nonX, nonFof, VD, uopLoad) + VpWen + Src1Gp

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

    private val prefetchI = bb"000"
    private val prefetchR = bb"001"
    private val prefetchW = bb"010"

    // Zicbop software prefetch
    val prefetch_i = Value(SCALA, nonH, nonX, nonFof, prefetchI, uopPrefetch)
    val prefetch_r = Value(SCALA, nonH, nonX, nonFof, prefetchR, uopPrefetch)
    val prefetch_w = Value(SCALA, nonH, nonX, nonFof, prefetchW, uopPrefetch)

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
    val sb = Value(SCALA, nonH, nonX, sign, B, uopStore)
    val sh = Value(SCALA, nonH, nonX, sign, H, uopStore)
    val sw = Value(SCALA, nonH, nonX, sign, W, uopStore)
    val sd = Value(SCALA, nonH, nonX, sign, D, uopStore)
    val sq = Value(SCALA, nonH, nonX, sign, Q, uopStore)

    //hypervisor store
    val hsvb = Value(SCALA, isH, nonX, sign, B, uopStore)
    val hsvh = Value(SCALA, isH, nonX, sign, H, uopStore)
    val hsvw = Value(SCALA, isH, nonX, sign, W, uopStore)
    val hsvd = Value(SCALA, isH, nonX, sign, D, uopStore)

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

    val vsnr8       = Value(MASK  , nonH, nonX, nonFof, VB, uopStore) + Src1Gp + Src3Vp
    val vsnr16      = Value(MASK  , nonH, nonX, nonFof, VH, uopStore) + Src1Gp + Src3Vp
    val vsnr32      = Value(MASK  , nonH, nonX, nonFof, VW, uopStore) + Src1Gp + Src3Vp
    val vsnr64      = Value(MASK  , nonH, nonX, nonFof, VD, uopStore) + Src1Gp + Src3Vp

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
    val cbo_zero  = Value(SCALA, nonH, nonX, sign, CBO.zero , uopCbo)
    // llc op
    val cbo_clean = Value(SCALA, nonH, nonX, sign, CBO.clean, uopCbo)
    val cbo_flush = Value(SCALA, nonH, nonX, sign, CBO.flush, uopCbo)
    val cbo_inval = Value(SCALA, nonH, nonX, sign, CBO.inval, uopCbo)

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
    val amoswap_b = Value(SCALA, NoALU.swap   , B, noALU)
    val amoadd_b  = Value(SCALA, WithALU.add  , B, withALU)
    val amoxor_b  = Value(SCALA, WithALU.xor  , B, withALU)
    val amoand_b  = Value(SCALA, WithALU.and  , B, withALU)
    val amoor_b   = Value(SCALA, WithALU.or   , B, withALU)
    val amomin_b  = Value(SCALA, WithALU.min  , B, withALU)
    val amomax_b  = Value(SCALA, WithALU.max  , B, withALU)
    val amominu_b = Value(SCALA, WithALU.minu , B, withALU)
    val amomaxu_b = Value(SCALA, WithALU.maxu , B, withALU)

    val amoswap_h = Value(SCALA, NoALU.swap   , H, noALU)
    val amoadd_h  = Value(SCALA, WithALU.add  , H, withALU)
    val amoxor_h  = Value(SCALA, WithALU.xor  , H, withALU)
    val amoand_h  = Value(SCALA, WithALU.and  , H, withALU)
    val amoor_h   = Value(SCALA, WithALU.or   , H, withALU)
    val amomin_h  = Value(SCALA, WithALU.min  , H, withALU)
    val amomax_h  = Value(SCALA, WithALU.max  , H, withALU)
    val amominu_h = Value(SCALA, WithALU.minu , H, withALU)
    val amomaxu_h = Value(SCALA, WithALU.maxu , H, withALU)

    val lr_w      = Value(SCALA, NoALU.lr     , W, noALU)
    val sc_w      = Value(SCALA, NoALU.sc     , W, noALU)
    val amoswap_w = Value(SCALA, NoALU.swap   , W, noALU)
    val amocas_w  = Value(SCALA, NoALU.cas    , W, noALU)
    val amoadd_w  = Value(SCALA, WithALU.add  , W, withALU)
    val amoxor_w  = Value(SCALA, WithALU.xor  , W, withALU)
    val amoand_w  = Value(SCALA, WithALU.and  , W, withALU)
    val amoor_w   = Value(SCALA, WithALU.or   , W, withALU)
    val amomin_w  = Value(SCALA, WithALU.min  , W, withALU)
    val amomax_w  = Value(SCALA, WithALU.max  , W, withALU)
    val amominu_w = Value(SCALA, WithALU.minu , W, withALU)
    val amomaxu_w = Value(SCALA, WithALU.maxu , W, withALU)

    val lr_d      = Value(SCALA, NoALU.lr     , D, noALU)
    val sc_d      = Value(SCALA, NoALU.sc     , D, noALU)
    val amoswap_d = Value(SCALA, NoALU.swap   , D, noALU)
    val amoadd_d  = Value(SCALA, WithALU.add  , D, withALU)
    val amoxor_d  = Value(SCALA, WithALU.xor  , D, withALU)
    val amoand_d  = Value(SCALA, WithALU.and  , D, withALU)
    val amoor_d   = Value(SCALA, WithALU.or   , D, withALU)
    val amomin_d  = Value(SCALA, WithALU.min  , D, withALU)
    val amomax_d  = Value(SCALA, WithALU.max  , D, withALU)
    val amominu_d = Value(SCALA, WithALU.minu , D, withALU)
    val amomaxu_d = Value(SCALA, WithALU.maxu , D, withALU)

    val amocas_b  = Value(SCALA, NoALU.cas, B, noALU)
    val amocas_h  = Value(SCALA, NoALU.cas, H, noALU)
    val amocas_d  = Value(SCALA, NoALU.cas, D, noALU)
    val amocas_q  = Value(SCALA, NoALU.cas, Q, noALU)

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
    val jmp     = Value(bb"010_000")
    val wfi     = Value(bb"100_000")
    val wrs_nto = Value(bb"100_010")
    val wrs_sto = Value(bb"100_011")
    val wrt     = Value(bb"001_001")
    val set     = Value(bb"001_010")
    val clr     = Value(bb"001_011")
    val wrti    = Value(bb"001_101")
    val seti    = Value(bb"001_110")
    val clri    = Value(bb"001_111")

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
    val fence  = Value(bb"10000")
    val sfence = Value(bb"10001")
    val fencei = Value(bb"10010")
    val hfence_v = Value(bb"10011")
    val hfence_g = Value(bb"10100")
    val nofence = Value(bb"00000")
  }

  object BkuOpcodes extends Opcodes {
    val clmul       = Value(bb"000000")
    val clmulh      = Value(bb"000001")
    val clmulr      = Value(bb"000010")
    val xpermn      = Value(bb"000100")
    val xpermb      = Value(bb"000101")

    val clz         = Value(bb"001000")
    val clzw        = Value(bb"001001")
    val ctz         = Value(bb"001010")
    val ctzw        = Value(bb"001011")
    val cpop        = Value(bb"001100")
    val cpopw       = Value(bb"001101")

    // 01xxxx is reserve
    val aes64es     = Value(bb"100000")
    val aes64esm    = Value(bb"100001")
    val aes64ds     = Value(bb"100010")
    val aes64dsm    = Value(bb"100011")
    val aes64im     = Value(bb"100100")
    val aes64ks1i   = Value(bb"100101")
    val aes64ks2    = Value(bb"100110")

    // merge to two instruction sm4ks & sm4ed
    val sm4ed0      = Value(bb"101000")
    val sm4ed1      = Value(bb"101001")
    val sm4ed2      = Value(bb"101010")
    val sm4ed3      = Value(bb"101011")
    val sm4ks0      = Value(bb"101100")
    val sm4ks1      = Value(bb"101101")
    val sm4ks2      = Value(bb"101110")
    val sm4ks3      = Value(bb"101111")

    val sha256sum0  = Value(bb"110000")
    val sha256sum1  = Value(bb"110001")
    val sha256sig0  = Value(bb"110010")
    val sha256sig1  = Value(bb"110011")
    val sha512sum0  = Value(bb"110100")
    val sha512sum1  = Value(bb"110101")
    val sha512sig0  = Value(bb"110110")
    val sha512sig1  = Value(bb"110111")

    val sm3p0       = Value(bb"111000")
    val sm3p1       = Value(bb"111001")
  }

  trait FCvtOpcodes extends Opcodes with DataType {
    private val F2F = bb"00"
    private val F2I = bb"01"
    private val I2F = bb"10"
    private val OTHER = bb"11"

    /**
     * Three sub opcode of [[F2F]]
     * [[cvt]], [[rnd]], [[rndnx]]
     */

    private val cvt = bb"00"

    private val rnd = bb"10"

    private val rndnx = bb"11"

    /**
     * sub-opcode of [[F2I]]
     */

    private val F2S = bb"00"
    private val F2U = bb"01"
    private val F2SMOD = bb"10"

    /**
     * sub-opcode of [[I2F]]
     */

    private val S2F = bb"00"
    private val U2F = bb"01"
    private val FMVI2F = bb"11"

    /**
     * sub-opcode of [[OTHER]]
     */

    private val FCLASS  = bb"0100"
    private val FREC7   = bb"1000"
    private val FRSQRT7 = bb"0101"
    private val FMVF2I  = bb"0000"

    val fcvt_fp32_fp16: Opcode = Value(FP32, FP16, cvt, F2F, F)
    val fcvt_fp64_fp16: Opcode = Value(FP64, FP16, cvt, F2F, F)
    val fcvt_fp16_fp32: Opcode = Value(FP16, FP32, cvt, F2F, F)
    val fcvt_fp64_fp32: Opcode = Value(FP64, FP32, cvt, F2F, F)
    val fcvt_fp16_fp64: Opcode = Value(FP16, FP64, cvt, F2F, F)
    val fcvt_fp32_fp64: Opcode = Value(FP32, FP64, cvt, F2F, F)

    val frnd_fp16: Opcode = Value(FP16, FP16, rnd, F2F, F)
    val frnd_fp32: Opcode = Value(FP32, FP32, rnd, F2F, F)
    val frnd_fp64: Opcode = Value(FP64, FP64, rnd, F2F, F)

    val frndnx_fp16: Opcode = Value(FP16, FP16, rndnx, F2F, F)
    val frndnx_fp32: Opcode = Value(FP32, FP32, rndnx, F2F, F)
    val frndnx_fp64: Opcode = Value(FP64, FP64, rndnx, F2F, F)

    // two narrow
    val vfcvt_fp16_fp32: Opcode = Value(FP16, FP32, cvt, F2F, V)
    val vfcvt_fp32_fp64: Opcode = Value(FP32, FP64, cvt, F2F, V)

    // two widen
    val vfcvt_fp32_fp16: Opcode = Value(FP32, FP16, cvt, F2F, V)
    val vfcvt_fp64_fp32: Opcode = Value(FP64, FP32, cvt, F2F, V)

    val fcvt_si32_fp16: Opcode = Value(I32, FP16, F2S, F2I, F)
    val fcvt_ui32_fp16: Opcode = Value(I32, FP16, F2U, F2I, F)
    val fcvt_si64_fp16: Opcode = Value(I64, FP16, F2S, F2I, F)
    val fcvt_ui64_fp16: Opcode = Value(I64, FP16, F2U, F2I, F)
    val fcvt_si32_fp32: Opcode = Value(I32, FP32, F2S, F2I, F)
    val fcvt_ui32_fp32: Opcode = Value(I32, FP32, F2U, F2I, F)
    val fcvt_si64_fp32: Opcode = Value(I64, FP32, F2S, F2I, F)
    val fcvt_ui64_fp32: Opcode = Value(I64, FP32, F2U, F2I, F)
    val fcvt_si32_fp64: Opcode = Value(I32, FP64, F2S, F2I, F)
    val fcvt_ui32_fp64: Opcode = Value(I32, FP64, F2U, F2I, F)
    val fcvt_si64_fp64: Opcode = Value(I64, FP64, F2S, F2I, F)
    val fcvt_ui64_fp64: Opcode = Value(I64, FP64, F2U, F2I, F)
    val fcvtmod_si32_fp64: Opcode = Value(I32, FP64, F2SMOD, F2I, F)

    val vfcvt_si8_fp16 : Opcode = Value(I8, FP16, F2S, F2I, V)
    val vfcvt_ui8_fp16 : Opcode = Value(I8, FP16, F2U, F2I, V)
    val vfcvt_si16_fp16: Opcode = Value(I16, FP16, F2S, F2I, V)
    val vfcvt_ui16_fp16: Opcode = Value(I16, FP16, F2U, F2I, V)
    val vfcvt_si32_fp16: Opcode = Value(I32, FP16, F2S, F2I, V)
    val vfcvt_ui32_fp16: Opcode = Value(I32, FP16, F2U, F2I, V)
    val vfcvt_si16_fp32: Opcode = Value(I16, FP32, F2S, F2I, V)
    val vfcvt_ui16_fp32: Opcode = Value(I16, FP32, F2U, F2I, V)
    val vfcvt_si32_fp32: Opcode = Value(I32, FP32, F2S, F2I, V)
    val vfcvt_ui32_fp32: Opcode = Value(I32, FP32, F2U, F2I, V)
    val vfcvt_si64_fp32: Opcode = Value(I64, FP32, F2S, F2I, V)
    val vfcvt_ui64_fp32: Opcode = Value(I64, FP32, F2U, F2I, V)
    val vfcvt_si32_fp64: Opcode = Value(I32, FP64, F2S, F2I, V)
    val vfcvt_ui32_fp64: Opcode = Value(I32, FP64, F2U, F2I, V)
    val vfcvt_si64_fp64: Opcode = Value(I64, FP64, F2S, F2I, V)
    val vfcvt_ui64_fp64: Opcode = Value(I64, FP64, F2U, F2I, V)

    val fcvt_fp16_si32: Opcode = Value(FP16, I32,    S2F, I2F, F)
    val fcvt_fp16_ui32: Opcode = Value(FP16, I32,    U2F, I2F, F)
    val fcvt_fp16_si64: Opcode = Value(FP16, I64,    S2F, I2F, F)
    val fcvt_fp16_ui64: Opcode = Value(FP16, I64,    U2F, I2F, F)
    val fcvt_fp32_si32: Opcode = Value(FP32, I32,    S2F, I2F, F)
    val fcvt_fp32_ui32: Opcode = Value(FP32, I32,    U2F, I2F, F)
    val fcvt_fp32_si64: Opcode = Value(FP32, I64,    S2F, I2F, F)
    val fcvt_fp32_ui64: Opcode = Value(FP32, I64,    U2F, I2F, F)
    val fcvt_fp64_si32: Opcode = Value(FP64, I32,    S2F, I2F, F)
    val fcvt_fp64_ui32: Opcode = Value(FP64, I32,    U2F, I2F, F)
    val fcvt_fp64_si64: Opcode = Value(FP64, I64,    S2F, I2F, F)
    val fcvt_fp64_ui64: Opcode = Value(FP64, I64,    U2F, I2F, F)
    val fmv_fp16_i    : Opcode = Value(FP16, I64, FMVI2F, I2F, F)
    val fmv_fp32_i    : Opcode = Value(FP32, I64, FMVI2F, I2F, F)
    val fmv_fp64_i    : Opcode = Value(FP64, I64, FMVI2F, I2F, F)

    val vfcvt_fp16_si16: Opcode = Value(FP16, I16, S2F, I2F, V)
    val vfcvt_fp16_ui16: Opcode = Value(FP16, I16, U2F, I2F, V)
    val vfcvt_fp16_si32: Opcode = Value(FP16, I32, S2F, I2F, V)
    val vfcvt_fp16_ui32: Opcode = Value(FP16, I32, U2F, I2F, V)
    val vfcvt_fp32_si16: Opcode = Value(FP32, I16, S2F, I2F, V)
    val vfcvt_fp32_ui16: Opcode = Value(FP32, I16, U2F, I2F, V)
    val vfcvt_fp32_si32: Opcode = Value(FP32, I32, S2F, I2F, V)
    val vfcvt_fp32_ui32: Opcode = Value(FP32, I32, U2F, I2F, V)
    val vfcvt_fp32_si64: Opcode = Value(FP32, I64, S2F, I2F, V)
    val vfcvt_fp32_ui64: Opcode = Value(FP32, I64, U2F, I2F, V)
    val vfcvt_fp64_si32: Opcode = Value(FP64, I32, S2F, I2F, V)
    val vfcvt_fp64_ui32: Opcode = Value(FP64, I32, U2F, I2F, V)
    val vfcvt_fp64_si64: Opcode = Value(FP64, I64, S2F, I2F, V)
    val vfcvt_fp64_ui64: Opcode = Value(FP64, I64, U2F, I2F, V)

    val fclass_fp16: Opcode = Value(FP16, FCLASS, OTHER, F)
    val fclass_fp32: Opcode = Value(FP32, FCLASS, OTHER, F)
    val fclass_fp64: Opcode = Value(FP64, FCLASS, OTHER, F)
    val fmv_i_fp16 : Opcode = Value(FP16, FMVF2I, OTHER, F)
    val fmv_i_fp32 : Opcode = Value(FP32, FMVF2I, OTHER, F)
    val fmv_i_fp64 : Opcode = Value(FP64, FMVF2I, OTHER, F)

    val vfclass_fp16 : Opcode = Value(FP16, FCLASS, OTHER, V)
    val vfclass_fp32 : Opcode = Value(FP32, FCLASS, OTHER, V)
    val vfclass_fp64 : Opcode = Value(FP64, FCLASS, OTHER, V)
    val vfrec7_fp16  : Opcode = Value(FP16, FREC7, OTHER, V)
    val vfrec7_fp32  : Opcode = Value(FP32, FREC7, OTHER, V)
    val vfrec7_fp64  : Opcode = Value(FP64, FREC7, OTHER, V)
    val vfrsqrt7_fp16: Opcode = Value(FP16, FRSQRT7, OTHER, V)
    val vfrsqrt7_fp32: Opcode = Value(FP32, FRSQRT7, OTHER, V)
    val vfrsqrt7_fp64: Opcode = Value(FP64, FRSQRT7, OTHER, V)
  }

  object FCvtOpcodes extends FCvtOpcodes

  trait FMacOpcodes extends Opcodes with DataType {
    private val OP2 = bb"0"
    private val OP3 = bb"1"

    private val DV = bb"0"
    private val DW = bb"1"
    private val S2V = bb"0"
    private val S2W = bb"1"
    private val NOTADD = bb"0"
    private val USEADD = bb"1"
    private val NOTMUL = bb"0"
    private val USEMUL = bb"1"

    // bit(2): 0 -> vs1 * vd, 1 -> vs1 * vs2
    // bit(1): 0 -> add     , 1 -> sub
    // bit(0): 0 -> pos     , 1 -> neg
    private val FMADD  = bb"000" // +((vs1[i] * vd[i]) + vs2[i])
    private val FNMADD = bb"001" // -((vs1[i] * vd[i]) + vs2[i])
    private val FMSUB  = bb"010" // +((vs1[i] * vd[i]) - vs2[i])
    private val FNMSUB = bb"011" // -((vs1[i] * vd[i]) - vs2[i])
    private val FMACC  = bb"100" // +((vs1[i] * vs2[i]) + vd[i])
    private val FNMACC = bb"101" // -((vs1[i] * vs2[i]) + vd[i])
    private val FMSAC  = bb"110" // +((vs1[i] * vs2[i]) - vd[i])
    private val FNMSAC = bb"111" // -((vs1[i] * vs2[i]) - vd[i])

    private val FADD = bb"000"
    private val FSUB = bb"001"
    private val FWADD4 = bb"010"

    private val FMUL = bb"100"

    val fmadd_fp16   : Opcode = Value(FMADD , OP3, S2V, DV, FP16, F)
    val fmsub_fp16   : Opcode = Value(FMSUB , OP3, S2V, DV, FP16, F)
    val fnmsub_fp16  : Opcode = Value(FNMSUB, OP3, S2V, DV, FP16, F)
    val fnmadd_fp16  : Opcode = Value(FNMADD, OP3, S2V, DV, FP16, F)
    val fadd_fp16    : Opcode = Value(FADD  , OP2, S2V, DV, FP16, F)
    val fsub_fp16    : Opcode = Value(FSUB  , OP2, S2V, DV, FP16, F)
    val fmul_fp16    : Opcode = Value(FMUL  , OP2, S2V, DV, FP16, F)
    val vfadd_fp16   : Opcode = Value(FADD  , OP2, S2V, DV, FP16, V)
    val vfsub_fp16   : Opcode = Value(FSUB  , OP2, S2V, DV, FP16, V)
    val vfmul_fp16   : Opcode = Value(FMUL  , OP2, S2V, DV, FP16, V)
    val vfmadd_fp16  : Opcode = Value(FMADD , OP3, S2V, DV, FP16, V)
    val vfnmadd_fp16 : Opcode = Value(FNMADD, OP3, S2V, DV, FP16, V)
    val vfmsub_fp16  : Opcode = Value(FMSUB , OP3, S2V, DV, FP16, V)
    val vfnmsub_fp16 : Opcode = Value(FNMSUB, OP3, S2V, DV, FP16, V)
    val vfmacc_fp16  : Opcode = Value(FMACC , OP3, S2V, DV, FP16, V)
    val vfnmacc_fp16 : Opcode = Value(FNMACC, OP3, S2V, DV, FP16, V)
    val vfmsac_fp16  : Opcode = Value(FMSAC , OP3, S2V, DV, FP16, V)
    val vfnmsac_fp16 : Opcode = Value(FNMSAC, OP3, S2V, DV, FP16, V)
    val vfwadd_fp16  : Opcode = Value(FADD  , OP2, S2V, DW, FP16, V)
    val vfwsub_fp16  : Opcode = Value(FSUB  , OP2, S2V, DW, FP16, V)
    val vfwadd_w_fp16: Opcode = Value(FADD  , OP2, S2W, DW, FP16, V)
    val vfwsub_w_fp16: Opcode = Value(FSUB  , OP2, S2W, DW, FP16, V)
    val vfwmul_fp16  : Opcode = Value(FMUL  , OP2, S2V, DW, FP16, V)
    val vfwmacc_fp16 : Opcode = Value(FMACC , OP3, S2V, DW, FP16, V)
    val vfwnmacc_fp16: Opcode = Value(FNMACC, OP3, S2V, DW, FP16, V)
    val vfwmsac_fp16 : Opcode = Value(FMSAC , OP3, S2V, DW, FP16, V)
    val vfwnmsac_fp16: Opcode = Value(FNMSAC, OP3, S2V, DW, FP16, V)
    val fmadd_fp32   : Opcode = Value(FMADD , OP3, S2V, DV, FP32, F)
    val fmsub_fp32   : Opcode = Value(FMSUB , OP3, S2V, DV, FP32, F)
    val fnmsub_fp32  : Opcode = Value(FNMSUB, OP3, S2V, DV, FP32, F)
    val fnmadd_fp32  : Opcode = Value(FNMADD, OP3, S2V, DV, FP32, F)
    val fadd_fp32    : Opcode = Value(FADD  , OP2, S2V, DV, FP32, F)
    val fsub_fp32    : Opcode = Value(FSUB  , OP2, S2V, DV, FP32, F)
    val fmul_fp32    : Opcode = Value(FMUL  , OP2, S2V, DV, FP32, F)
    val vfadd_fp32   : Opcode = Value(FADD  , OP2, S2V, DV, FP32, V)
    val vfsub_fp32   : Opcode = Value(FSUB  , OP2, S2V, DV, FP32, V)
    val vfmul_fp32   : Opcode = Value(FMUL  , OP2, S2V, DV, FP32, V)
    val vfmadd_fp32  : Opcode = Value(FMADD , OP3, S2V, DV, FP32, V)
    val vfnmadd_fp32 : Opcode = Value(FNMADD, OP3, S2V, DV, FP32, V)
    val vfmsub_fp32  : Opcode = Value(FMSUB , OP3, S2V, DV, FP32, V)
    val vfnmsub_fp32 : Opcode = Value(FNMSUB, OP3, S2V, DV, FP32, V)
    val vfmacc_fp32  : Opcode = Value(FMACC , OP3, S2V, DV, FP32, V)
    val vfnmacc_fp32 : Opcode = Value(FNMACC, OP3, S2V, DV, FP32, V)
    val vfmsac_fp32  : Opcode = Value(FMSAC , OP3, S2V, DV, FP32, V)
    val vfnmsac_fp32 : Opcode = Value(FNMSAC, OP3, S2V, DV, FP32, V)
    val vfwadd_fp32  : Opcode = Value(FADD  , OP2, S2V, DW, FP32, V)
    val vfwsub_fp32  : Opcode = Value(FSUB  , OP2, S2V, DW, FP32, V)
    val vfwadd_w_fp32: Opcode = Value(FADD  , OP2, S2W, DW, FP32, V)
    val vfwsub_w_fp32: Opcode = Value(FSUB  , OP2, S2W, DW, FP32, V)
    val vfwmul_fp32  : Opcode = Value(FMUL  , OP2, S2V, DW, FP32, V)
    val vfwmacc_fp32 : Opcode = Value(FMACC , OP3, S2V, DW, FP32, V)
    val vfwnmacc_fp32: Opcode = Value(FNMACC, OP3, S2V, DW, FP32, V)
    val vfwmsac_fp32 : Opcode = Value(FMSAC , OP3, S2V, DW, FP32, V)
    val vfwnmsac_fp32: Opcode = Value(FNMSAC, OP3, S2V, DW, FP32, V)
    val fmadd_fp64   : Opcode = Value(FMADD , OP3, S2V, DV, FP64, F)
    val fmsub_fp64   : Opcode = Value(FMSUB , OP3, S2V, DV, FP64, F)
    val fnmsub_fp64  : Opcode = Value(FNMSUB, OP3, S2V, DV, FP64, F)
    val fnmadd_fp64  : Opcode = Value(FNMADD, OP3, S2V, DV, FP64, F)
    val fadd_fp64    : Opcode = Value(FADD  , OP2, S2V, DV, FP64, F)
    val fsub_fp64    : Opcode = Value(FSUB  , OP2, S2V, DV, FP64, F)
    val fmul_fp64    : Opcode = Value(FMUL  , OP2, S2V, DV, FP64, F)
    val vfadd_fp64   : Opcode = Value(FADD  , OP2, S2V, DV, FP64, V)
    val vfsub_fp64   : Opcode = Value(FSUB  , OP2, S2V, DV, FP64, V)
    val vfmul_fp64   : Opcode = Value(FMUL  , OP2, S2V, DV, FP64, V)
    val vfmadd_fp64  : Opcode = Value(FMADD , OP3, S2V, DV, FP64, V)
    val vfnmadd_fp64 : Opcode = Value(FNMADD, OP3, S2V, DV, FP64, V)
    val vfmsub_fp64  : Opcode = Value(FMSUB , OP3, S2V, DV, FP64, V)
    val vfnmsub_fp64 : Opcode = Value(FNMSUB, OP3, S2V, DV, FP64, V)
    val vfmacc_fp64  : Opcode = Value(FMACC , OP3, S2V, DV, FP64, V)
    val vfnmacc_fp64 : Opcode = Value(FNMACC, OP3, S2V, DV, FP64, V)
    val vfmsac_fp64  : Opcode = Value(FMSAC , OP3, S2V, DV, FP64, V)
    val vfnmsac_fp64 : Opcode = Value(FNMSAC, OP3, S2V, DV, FP64, V)

    val vfwadd4_fp16 : Opcode = Value(FWADD4, OP2, S2V, DW, FP16, V)
    val vfwadd4_fp32 : Opcode = Value(FWADD4, OP2, S2V, DW, FP32, V)
    // There are no widen uops when FP64

    def getOpNum(op: UInt): Bool = op(5)

    def getSubOpcode(op: UInt): UInt = op(8, 6)

    def useADD(op: UInt): Bool = {
      getOpNum(op) === OP3 ||
        getOpNum(op) === OP2 && (getSubOpcode(op) === FADD || getSubOpcode(op) === FSUB)
    }

    def useMUL(op: UInt): Bool = {
      getOpNum(op) === OP3 ||
        getOpNum(op) === OP2 && getSubOpcode(op) === FMUL
    }
  }

  object FMacOpcodes extends FMacOpcodes

  trait FDivOpcodes extends Opcodes with DataType {
    private val FDIV  = bb"0"
    private val FSQRT = bb"1"

    val fdiv_fp16 : Opcode = Value(FDIV , FP16, F)
    val fdiv_fp32 : Opcode = Value(FDIV , FP32, F)
    val fdiv_fp64 : Opcode = Value(FDIV , FP64, F)
    val fsqrt_fp16: Opcode = Value(FSQRT, FP16, F)
    val fsqrt_fp32: Opcode = Value(FSQRT, FP32, F)
    val fsqrt_fp64: Opcode = Value(FSQRT, FP64, F)

    val vfdiv_fp16 : Opcode = Value(FDIV , FP16, V)
    val vfsqrt_fp16: Opcode = Value(FSQRT, FP16, V)
    val vfdiv_fp32 : Opcode = Value(FDIV , FP32, V)
    val vfsqrt_fp32: Opcode = Value(FSQRT, FP32, V)
    val vfdiv_fp64 : Opcode = Value(FDIV , FP64, V)
    val vfsqrt_fp64: Opcode = Value(FSQRT, FP64, V)
  }

  object FDivOpcodes extends FDivOpcodes

  trait FMiscOpcodes extends Opcodes with DataType {
    private val FSGNJ  = bb"0000"
    private val FSGNJN = bb"0001"
    private val FSGNJX = bb"0010"
    private val FMIN   = bb"0100"
    private val FMAX   = bb"0101"
    private val FMINM  = bb"0110"
    private val FMAXM  = bb"0111"

    private val FEQ  = bb"0000"
    private val FLE  = bb"0010"
    private val FLT  = bb"0110"
    private val FNE  = bb"1000"
    private val FGT  = bb"1010"
    private val FGE  = bb"1110"
    private val FLEQ = bb"0011"
    private val FLTQ = bb"0111"

    private val DM = bb"1"
    private val DV = bb"0"

    val feq_fp16    : Opcode = Value(FEQ   , DM, FP16, F)
    val fle_fp16    : Opcode = Value(FLE   , DM, FP16, F)
    val flt_fp16    : Opcode = Value(FLT   , DM, FP16, F)
    val fleq_fp16   : Opcode = Value(FLEQ  , DM, FP16, F)
    val fltq_fp16   : Opcode = Value(FLTQ  , DM, FP16, F)
    val fmin_fp16   : Opcode = Value(FMIN  , DV, FP16, F)
    val fmax_fp16   : Opcode = Value(FMAX  , DV, FP16, F)
    val fminm_fp16  : Opcode = Value(FMINM , DV, FP16, F)
    val fmaxm_fp16  : Opcode = Value(FMAXM , DV, FP16, F)
    val fsgnj_fp16  : Opcode = Value(FSGNJ , DV, FP16, F)
    val fsgnjn_fp16 : Opcode = Value(FSGNJN, DV, FP16, F)
    val fsgnjx_fp16 : Opcode = Value(FSGNJX, DV, FP16, F)
    val vmfeq_fp16  : Opcode = Value(FEQ   , DM, FP16, V)
    val vmfle_fp16  : Opcode = Value(FLE   , DM, FP16, V)
    val vmflt_fp16  : Opcode = Value(FLT   , DM, FP16, V)
    val vmfne_fp16  : Opcode = Value(FNE   , DM, FP16, V)
    val vmfgt_fp16  : Opcode = Value(FGT   , DM, FP16, V)
    val vmfge_fp16  : Opcode = Value(FGE   , DM, FP16, V)
    val vfmin_fp16  : Opcode = Value(FMIN  , DV, FP16, V)
    val vfmax_fp16  : Opcode = Value(FMAX  , DV, FP16, V)
    val vfsgnj_fp16 : Opcode = Value(FSGNJ , DV, FP16, V)
    val vfsgnjn_fp16: Opcode = Value(FSGNJN, DV, FP16, V)
    val vfsgnjx_fp16: Opcode = Value(FSGNJX, DV, FP16, V)
    val feq_fp32    : Opcode = Value(FEQ   , DM, FP32, F)
    val fle_fp32    : Opcode = Value(FLE   , DM, FP32, F)
    val flt_fp32    : Opcode = Value(FLT   , DM, FP32, F)
    val fleq_fp32   : Opcode = Value(FLEQ  , DM, FP32, F)
    val fltq_fp32   : Opcode = Value(FLTQ  , DM, FP32, F)
    val fmin_fp32   : Opcode = Value(FMIN  , DV, FP32, F)
    val fmax_fp32   : Opcode = Value(FMAX  , DV, FP32, F)
    val fminm_fp32  : Opcode = Value(FMINM , DV, FP32, F)
    val fmaxm_fp32  : Opcode = Value(FMAXM , DV, FP32, F)
    val fsgnj_fp32  : Opcode = Value(FSGNJ , DV, FP32, F)
    val fsgnjn_fp32 : Opcode = Value(FSGNJN, DV, FP32, F)
    val fsgnjx_fp32 : Opcode = Value(FSGNJX, DV, FP32, F)
    val vmfeq_fp32  : Opcode = Value(FEQ   , DM, FP32, V)
    val vmfle_fp32  : Opcode = Value(FLE   , DM, FP32, V)
    val vmflt_fp32  : Opcode = Value(FLT   , DM, FP32, V)
    val vmfne_fp32  : Opcode = Value(FNE   , DM, FP32, V)
    val vmfgt_fp32  : Opcode = Value(FGT   , DM, FP32, V)
    val vmfge_fp32  : Opcode = Value(FGE   , DM, FP32, V)
    val vfmin_fp32  : Opcode = Value(FMIN  , DV, FP32, V)
    val vfmax_fp32  : Opcode = Value(FMAX  , DV, FP32, V)
    val vfsgnj_fp32 : Opcode = Value(FSGNJ , DV, FP32, V)
    val vfsgnjn_fp32: Opcode = Value(FSGNJN, DV, FP32, V)
    val vfsgnjx_fp32: Opcode = Value(FSGNJX, DV, FP32, V)
    val feq_fp64    : Opcode = Value(FEQ   , DM, FP64, F)
    val fle_fp64    : Opcode = Value(FLE   , DM, FP64, F)
    val flt_fp64    : Opcode = Value(FLT   , DM, FP64, F)
    val fleq_fp64   : Opcode = Value(FLEQ  , DM, FP64, F)
    val fltq_fp64   : Opcode = Value(FLTQ  , DM, FP64, F)
    val fmin_fp64   : Opcode = Value(FMIN  , DV, FP64, F)
    val fmax_fp64   : Opcode = Value(FMAX  , DV, FP64, F)
    val fminm_fp64  : Opcode = Value(FMINM , DV, FP64, F)
    val fmaxm_fp64  : Opcode = Value(FMAXM , DV, FP64, F)
    val fsgnj_fp64  : Opcode = Value(FSGNJ , DV, FP64, F)
    val fsgnjn_fp64 : Opcode = Value(FSGNJN, DV, FP64, F)
    val fsgnjx_fp64 : Opcode = Value(FSGNJX, DV, FP64, F)
    val vmfeq_fp64  : Opcode = Value(FEQ   , DM, FP64, V)
    val vmfle_fp64  : Opcode = Value(FLE   , DM, FP64, V)
    val vmflt_fp64  : Opcode = Value(FLT   , DM, FP64, V)
    val vmfne_fp64  : Opcode = Value(FNE   , DM, FP64, V)
    val vmfgt_fp64  : Opcode = Value(FGT   , DM, FP64, V)
    val vmfge_fp64  : Opcode = Value(FGE   , DM, FP64, V)
    val vfmin_fp64  : Opcode = Value(FMIN  , DV, FP64, V)
    val vfmax_fp64  : Opcode = Value(FMAX  , DV, FP64, V)
    val vfsgnj_fp64 : Opcode = Value(FSGNJ , DV, FP64, V)
    val vfsgnjn_fp64: Opcode = Value(FSGNJN, DV, FP64, V)
    val vfsgnjx_fp64: Opcode = Value(FSGNJX, DV, FP64, V)
  }

  object FMiscOpcodes extends FMiscOpcodes

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

    // Todo: use BitPat.dontCare
    private def dc(n: Int) = BitPat.N(n)

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
    //
  //  trait VIAluOpcodes extends Opcode with DataType {
  //    private val ADDER  = bb"000"
  //    private val CMP    = bb"100"
  //    private val LOGIC  = bb"010"
  //    private val MOVE   = bb"110"
  //    private val CADDER = bb"001"
  //    private val SHIFT  = bb"011"
  //    private val CSHIFT = bb"111"
  //
  //    private val S2VDV  = bb"000"
  //    private val S2VDW  = bb"001"
  //    private val S2WDV  = bb"010"
  //    private val S2WDW  = bb"011"
  //    private val S2VDM  = bb"100"
  //    private val S2F8DV = bb"101"
  //    private val S2F4DV = bb"110"
  //    private val S2F2DV = bb"111"
  //
  //    /**
  //     * sub opcode of [[ADDER]]
  //     */
  //    private val ADD  = bb"000"
  //    private val SUB  = bb"001"
  //    private val ADC  = bb"010"
  //    private val SBC  = bb"011"
  //    private val MINU = bb"100"
  //    private val MIN  = bb"101"
  //    private val MAXU = bb"110"
  //    private val MAX  = bb"111"
  //
  //    /**
  //     * sub opcode of [[CMP]]
  //     */
  //    private val EQ  = bb"000"
  //    private val NE  = bb"001"
  //    private val LTU = bb"010"
  //    private val LT  = bb"011"
  //    private val LEU = bb"100"
  //    private val LE  = bb"101"
  //    private val GTU = bb"110"
  //    private val GT  = bb"111"
  //
  //    /**
  //     * sub opcode of [[LOGIC]]
  //     */
  //    private val ANDN = bb"000"
  //    private val AND  = bb"001"
  //    private val OR   = bb"010"
  //    private val XOR  = bb"011"
  //    private val ORN  = bb"100"
  //    private val NAND = bb"101"
  //    private val NOR  = bb"110"
  //    private val XNOR = bb"111"
  //
  //    /**
  //     * sub opcode of [[MOVE]]
  //     */
  //    private val MERGE  = bb"000"
  //    private val VMV_NR = bb"001"
  //    private val VMV_VS = bb"010"
  //    private val ZEXT   = bb"100"
  //    private val SEXT   = bb"101"
  //
  //    /**
  //     * sub opcode of [[CADDER]]
  //     * [[AADDU]] and [[WADDU]] will be distinguashed by [[S2VDV]] or [[S2VDW]]
  //     */
  //    private val AADDU = bb"000"
  //    private val AADD  = bb"001"
  //    private val ASUBU = bb"010"
  //    private val ASUB  = bb"011"
  //    private val SADDU = bb"100"
  //    private val SADD  = bb"101"
  //    private val SSUBU = bb"110"
  //    private val SSUB  = bb"111"
  //    private val WADDU = bb"000"
  //    private val WADD  = bb"001"
  //    private val WSUBU = bb"010"
  //    private val WSUB  = bb"011"
  //
  //    /**
  //     * WADD4U:
  //     *   vd(i) = waddu( vs1(2i) + vs2(2i) ) + waddu( vs1(2i + 1) + vs2(2i + 1) )
  //     */
  //    private val WADD4U = bb"110"
  //
  //    /**
  //     * WADD4:
  //     *   vd(i) = wadd( vs1(2i) + vs2(2i) ) + wadd( vs1(2i + 1) + vs2(2i + 1) )
  //     */
  //    private val WADD4  = bb"111"
  //
  //    /**
  //     * sub opcode of [[SHIFT]] and [[CSHIFT]]
  //     */
  //    private val SLL   = bb"000"
  //    private val SRL   = bb"001"
  //    private val SRA   = bb"010"
  //    private val ROR   = bb"100"
  //    private val ROL   = bb"101"
  //    private val CLIPU = bb"110"
  //    private val CLIP  = bb"111"
  //
  //
  //    val vadd_e8   = VIAluOpcode(ADD, ADDER, S2VDV, E8)
  //    val vadd_e16  = VIAluOpcode(ADD, ADDER, S2VDV, E16)
  //    val vadd_e32  = VIAluOpcode(ADD, ADDER, S2VDV, E32)
  //    val vadd_e64  = VIAluOpcode(ADD, ADDER, S2VDV, E64)
  //    val vsub_e8   = VIAluOpcode(SUB, ADDER, S2VDV, E8)
  //    val vsub_e16  = VIAluOpcode(SUB, ADDER, S2VDV, E16)
  //    val vsub_e32  = VIAluOpcode(SUB, ADDER, S2VDV, E32)
  //    val vsub_e64  = VIAluOpcode(SUB, ADDER, S2VDV, E64)
  //    val vadc_e8   = VIAluOpcode(ADC, ADDER, S2VDV, E8)
  //    val vadc_e16  = VIAluOpcode(ADC, ADDER, S2VDV, E16)
  //    val vadc_e32  = VIAluOpcode(ADC, ADDER, S2VDV, E32)
  //    val vadc_e64  = VIAluOpcode(ADC, ADDER, S2VDV, E64)
  //    val vmadc_e8  = VIAluOpcode(ADC, ADDER, S2VDM, E8)
  //    val vmadc_e16 = VIAluOpcode(ADC, ADDER, S2VDM, E16)
  //    val vmadc_e32 = VIAluOpcode(ADC, ADDER, S2VDM, E32)
  //    val vmadc_e64 = VIAluOpcode(ADC, ADDER, S2VDM, E64)
  //    val vsbc_e8   = VIAluOpcode(SBC, ADDER, S2VDV, E8)
  //    val vsbc_e16  = VIAluOpcode(SBC, ADDER, S2VDV, E16)
  //    val vsbc_e32  = VIAluOpcode(SBC, ADDER, S2VDV, E32)
  //    val vsbc_e64  = VIAluOpcode(SBC, ADDER, S2VDV, E64)
  //    val vmsbc_e8  = VIAluOpcode(SBC, ADDER, S2VDM, E8)
  //    val vmsbc_e16 = VIAluOpcode(SBC, ADDER, S2VDM, E16)
  //    val vmsbc_e32 = VIAluOpcode(SBC, ADDER, S2VDM, E32)
  //    val vmsbc_e64 = VIAluOpcode(SBC, ADDER, S2VDM, E64)
  //
  //    val vminu_e8  = VIAluOpcode(MINU, ADDER, S2VDV, E8)
  //    val vminu_e16 = VIAluOpcode(MINU, ADDER, S2VDV, E16)
  //    val vminu_e32 = VIAluOpcode(MINU, ADDER, S2VDV, E32)
  //    val vminu_e64 = VIAluOpcode(MINU, ADDER, S2VDV, E64)
  //    val vmin_e8   = VIAluOpcode(MIN,  ADDER, S2VDV, E8)
  //    val vmin_e16  = VIAluOpcode(MIN,  ADDER, S2VDV, E16)
  //    val vmin_e32  = VIAluOpcode(MIN,  ADDER, S2VDV, E32)
  //    val vmin_e64  = VIAluOpcode(MIN,  ADDER, S2VDV, E64)
  //    val vmaxu_e8  = VIAluOpcode(MAXU, ADDER, S2VDV, E8)
  //    val vmaxu_e16 = VIAluOpcode(MAXU, ADDER, S2VDV, E16)
  //    val vmaxu_e32 = VIAluOpcode(MAXU, ADDER, S2VDV, E32)
  //    val vmaxu_e64 = VIAluOpcode(MAXU, ADDER, S2VDV, E64)
  //    val vmax_e8   = VIAluOpcode(MAX,  ADDER, S2VDV, E8)
  //    val vmax_e16  = VIAluOpcode(MAX,  ADDER, S2VDV, E16)
  //    val vmax_e32  = VIAluOpcode(MAX,  ADDER, S2VDV, E32)
  //    val vmax_e64  = VIAluOpcode(MAX,  ADDER, S2VDV, E64)
  //
  //    val vmseq_e8   = VIAluOpcode(EQ , CMP, S2VDM, E8)
  //    val vmseq_e16  = VIAluOpcode(EQ , CMP, S2VDM, E16)
  //    val vmseq_e32  = VIAluOpcode(EQ , CMP, S2VDM, E32)
  //    val vmseq_e64  = VIAluOpcode(EQ , CMP, S2VDM, E64)
  //    val vmsne_e8   = VIAluOpcode(NE , CMP, S2VDM, E8)
  //    val vmsne_e16  = VIAluOpcode(NE , CMP, S2VDM, E16)
  //    val vmsne_e32  = VIAluOpcode(NE , CMP, S2VDM, E32)
  //    val vmsne_e64  = VIAluOpcode(NE , CMP, S2VDM, E64)
  //    val vmsltu_e8  = VIAluOpcode(LTU, CMP, S2VDM, E8)
  //    val vmsltu_e16 = VIAluOpcode(LTU, CMP, S2VDM, E16)
  //    val vmsltu_e32 = VIAluOpcode(LTU, CMP, S2VDM, E32)
  //    val vmsltu_e64 = VIAluOpcode(LTU, CMP, S2VDM, E64)
  //    val vmslt_e8   = VIAluOpcode(LT , CMP, S2VDM, E8)
  //    val vmslt_e16  = VIAluOpcode(LT , CMP, S2VDM, E16)
  //    val vmslt_e32  = VIAluOpcode(LT , CMP, S2VDM, E32)
  //    val vmslt_e64  = VIAluOpcode(LT , CMP, S2VDM, E64)
  //    val vmsleu_e8  = VIAluOpcode(LEU, CMP, S2VDM, E8)
  //    val vmsleu_e16 = VIAluOpcode(LEU, CMP, S2VDM, E16)
  //    val vmsleu_e32 = VIAluOpcode(LEU, CMP, S2VDM, E32)
  //    val vmsleu_e64 = VIAluOpcode(LEU, CMP, S2VDM, E64)
  //    val vmsle_e8   = VIAluOpcode(LE , CMP, S2VDM, E8)
  //    val vmsle_e16  = VIAluOpcode(LE , CMP, S2VDM, E16)
  //    val vmsle_e32  = VIAluOpcode(LE , CMP, S2VDM, E32)
  //    val vmsle_e64  = VIAluOpcode(LE , CMP, S2VDM, E64)
  //    val vmsgtu_e8  = VIAluOpcode(GTU, CMP, S2VDM, E8)
  //    val vmsgtu_e16 = VIAluOpcode(GTU, CMP, S2VDM, E16)
  //    val vmsgtu_e32 = VIAluOpcode(GTU, CMP, S2VDM, E32)
  //    val vmsgtu_e64 = VIAluOpcode(GTU, CMP, S2VDM, E64)
  //    val vmsgt_e8   = VIAluOpcode(GT , CMP, S2VDM, E8)
  //    val vmsgt_e16  = VIAluOpcode(GT , CMP, S2VDM, E16)
  //    val vmsgt_e32  = VIAluOpcode(GT , CMP, S2VDM, E32)
  //    val vmsgt_e64  = VIAluOpcode(GT , CMP, S2VDM, E64)
  //
  //    val vandn_e8  = VIAluOpcode(ANDN, LOGIC, S2VDV, E8)
  //    val vandn_e16 = VIAluOpcode(ANDN, LOGIC, S2VDV, E16)
  //    val vandn_e32 = VIAluOpcode(ANDN, LOGIC, S2VDV, E32)
  //    val vandn_e64 = VIAluOpcode(ANDN, LOGIC, S2VDV, E64)
  //    val vand_e8   = VIAluOpcode(AND , LOGIC, S2VDV, E8)
  //    val vand_e16  = VIAluOpcode(AND , LOGIC, S2VDV, E16)
  //    val vand_e32  = VIAluOpcode(AND , LOGIC, S2VDV, E32)
  //    val vand_e64  = VIAluOpcode(AND , LOGIC, S2VDV, E64)
  //    val vor_e8    = VIAluOpcode(OR  , LOGIC, S2VDV, E8)
  //    val vor_e16   = VIAluOpcode(OR  , LOGIC, S2VDV, E16)
  //    val vor_e32   = VIAluOpcode(OR  , LOGIC, S2VDV, E32)
  //    val vor_e64   = VIAluOpcode(OR  , LOGIC, S2VDV, E64)
  //    val vxor_e8   = VIAluOpcode(XOR , LOGIC, S2VDV, E8)
  //    val vxor_e16  = VIAluOpcode(XOR , LOGIC, S2VDV, E16)
  //    val vxor_e32  = VIAluOpcode(XOR , LOGIC, S2VDV, E32)
  //    val vxor_e64  = VIAluOpcode(XOR , LOGIC, S2VDV, E64)
  //
  //    val vmandn = VIAluOpcode(ANDN, LOGIC, S2VDM, EX)
  //    val vmand  = VIAluOpcode(AND , LOGIC, S2VDM, EX)
  //    val vmor   = VIAluOpcode(OR  , LOGIC, S2VDM, EX)
  //    val vmxor  = VIAluOpcode(XOR , LOGIC, S2VDM, EX)
  //    val vmorn  = VIAluOpcode(ORN , LOGIC, S2VDM, EX)
  //    val vmnand = VIAluOpcode(NAND, LOGIC, S2VDM, EX)
  //    val vmnor  = VIAluOpcode(NOR , LOGIC, S2VDM, EX)
  //    val vmxnor = VIAluOpcode(XNOR, LOGIC, S2VDM, EX)
  //
  //    val vmerge_e8  = VIAluOpcode(MERGE, MOVE, S2VDV, E8 )
  //    val vmerge_e16 = VIAluOpcode(MERGE, MOVE, S2VDV, E16)
  //    val vmerge_e32 = VIAluOpcode(MERGE, MOVE, S2VDV, E32)
  //    val vmerge_e64 = VIAluOpcode(MERGE, MOVE, S2VDV, E64)
  //    val vmvnr      = VIAluOpcode(VMV_NR, MOVE, S2VDV, EX )
  //    val vmv_x2vs_e8  = VIAluOpcode(VMV_VS, MOVE, S2VDV, E8 )
  //    val vmv_x2vs_e16 = VIAluOpcode(VMV_VS, MOVE, S2VDV, E16)
  //    val vmv_x2vs_e32 = VIAluOpcode(VMV_VS, MOVE, S2VDV, E32)
  //    val vmv_x2vs_e64 = VIAluOpcode(VMV_VS, MOVE, S2VDV, E64)
  //    val vzext2_e8  = VIAluOpcode(ZEXT, MOVE, S2F2DV, E8 ) // vzext.vf2 when sew=e16
  //    val vzext2_e16 = VIAluOpcode(ZEXT, MOVE, S2F2DV, E16)
  //    val vzext2_e32 = VIAluOpcode(ZEXT, MOVE, S2F2DV, E32)
  //    val vzext4_e8  = VIAluOpcode(ZEXT, MOVE, S2F4DV, E8 )
  //    val vzext4_e16 = VIAluOpcode(ZEXT, MOVE, S2F4DV, E16)
  //    val vzext8_e8  = VIAluOpcode(ZEXT, MOVE, S2F4DV, E8 )
  //    val vsext2_e8  = VIAluOpcode(SEXT, MOVE, S2F2DV, E8 ) // vsext.vf2 when sew=e16
  //    val vsext2_e16 = VIAluOpcode(SEXT, MOVE, S2F2DV, E16)
  //    val vsext2_e32 = VIAluOpcode(SEXT, MOVE, S2F2DV, E32)
  //    val vsext4_e8  = VIAluOpcode(SEXT, MOVE, S2F4DV, E8 )
  //    val vsext4_e16 = VIAluOpcode(SEXT, MOVE, S2F4DV, E16)
  //    val vsext8_e8  = VIAluOpcode(SEXT, MOVE, S2F4DV, E8 )
  //
  //    val vwaddu_e8    = VIAluOpcode(WADDU, CADDER, S2VDW, E8)
  //    val vwaddu_e16   = VIAluOpcode(WADDU, CADDER, S2VDW, E16)
  //    val vwaddu_e32   = VIAluOpcode(WADDU, CADDER, S2VDW, E32)
  //    val vwaddu_e64   = VIAluOpcode(WADDU, CADDER, S2VDW, E64)
  //    val vwadd_e8     = VIAluOpcode(WADD , CADDER, S2VDW, E8)
  //    val vwadd_e16    = VIAluOpcode(WADD , CADDER, S2VDW, E16)
  //    val vwadd_e32    = VIAluOpcode(WADD , CADDER, S2VDW, E32)
  //    val vwadd_e64    = VIAluOpcode(WADD , CADDER, S2VDW, E64)
  //    val vwsubu_e8    = VIAluOpcode(WSUBU, CADDER, S2VDW, E8)
  //    val vwsubu_e16   = VIAluOpcode(WSUBU, CADDER, S2VDW, E16)
  //    val vwsubu_e32   = VIAluOpcode(WSUBU, CADDER, S2VDW, E32)
  //    val vwsubu_e64   = VIAluOpcode(WSUBU, CADDER, S2VDW, E64)
  //    val vwsub_e8     = VIAluOpcode(WSUB , CADDER, S2VDW, E8)
  //    val vwsub_e16    = VIAluOpcode(WSUB , CADDER, S2VDW, E16)
  //    val vwsub_e32    = VIAluOpcode(WSUB , CADDER, S2VDW, E32)
  //    val vwsub_e64    = VIAluOpcode(WSUB , CADDER, S2VDW, E64)
  //    val vwaddu_w_e8  = VIAluOpcode(WADDU, CADDER, S2WDW, E8)
  //    val vwaddu_w_e16 = VIAluOpcode(WADDU, CADDER, S2WDW, E16)
  //    val vwaddu_w_e32 = VIAluOpcode(WADDU, CADDER, S2WDW, E32)
  //    val vwaddu_w_e64 = VIAluOpcode(WADDU, CADDER, S2WDW, E64)
  //    val vwadd_w_e8   = VIAluOpcode(WADD , CADDER, S2WDW, E8)
  //    val vwadd_w_e16  = VIAluOpcode(WADD , CADDER, S2WDW, E16)
  //    val vwadd_w_e32  = VIAluOpcode(WADD , CADDER, S2WDW, E32)
  //    val vwadd_w_e64  = VIAluOpcode(WADD , CADDER, S2WDW, E64)
  //    val vwsubu_w_e8  = VIAluOpcode(WSUBU, CADDER, S2WDW, E8)
  //    val vwsubu_w_e16 = VIAluOpcode(WSUBU, CADDER, S2WDW, E16)
  //    val vwsubu_w_e32 = VIAluOpcode(WSUBU, CADDER, S2WDW, E32)
  //    val vwsubu_w_e64 = VIAluOpcode(WSUBU, CADDER, S2WDW, E64)
  //    val vwsub_w_e8   = VIAluOpcode(WSUB , CADDER, S2WDW, E8)
  //    val vwsub_w_e16  = VIAluOpcode(WSUB , CADDER, S2WDW, E16)
  //    val vwsub_w_e32  = VIAluOpcode(WSUB , CADDER, S2WDW, E32)
  //    val vwsub_w_e64  = VIAluOpcode(WSUB , CADDER, S2WDW, E64)
  //
  //    // internal uop to support reduction sum
  //    val vwadd4u_e8  = VIAluOpcode(WADD4U, CADDER, S2VDW, E8 )
  //    val vwadd4u_e16 = VIAluOpcode(WADD4U, CADDER, S2VDW, E16)
  //    val vwadd4u_e32 = VIAluOpcode(WADD4U, CADDER, S2VDW, E32)
  //    val vwadd4_e8   = VIAluOpcode(WADD4 , CADDER, S2VDW, E8 )
  //    val vwadd4_e16  = VIAluOpcode(WADD4 , CADDER, S2VDW, E16)
  //    val vwadd4_e32  = VIAluOpcode(WADD4 , CADDER, S2VDW, E32)
  //
  //    val vaaddu_e8  = VIAluOpcode(AADDU, CADDER, S2VDV, E8)
  //    val vaaddu_e16 = VIAluOpcode(AADDU, CADDER, S2VDV, E16)
  //    val vaaddu_e32 = VIAluOpcode(AADDU, CADDER, S2VDV, E32)
  //    val vaaddu_e64 = VIAluOpcode(AADDU, CADDER, S2VDV, E64)
  //    val vaadd_e8   = VIAluOpcode(AADD , CADDER, S2VDV, E8)
  //    val vaadd_e16  = VIAluOpcode(AADD , CADDER, S2VDV, E16)
  //    val vaadd_e32  = VIAluOpcode(AADD , CADDER, S2VDV, E32)
  //    val vaadd_e64  = VIAluOpcode(AADD , CADDER, S2VDV, E64)
  //    val vasubu_e8  = VIAluOpcode(ASUBU, CADDER, S2VDV, E8)
  //    val vasubu_e16 = VIAluOpcode(ASUBU, CADDER, S2VDV, E16)
  //    val vasubu_e32 = VIAluOpcode(ASUBU, CADDER, S2VDV, E32)
  //    val vasubu_e64 = VIAluOpcode(ASUBU, CADDER, S2VDV, E64)
  //    val vasub_e8   = VIAluOpcode(ASUB , CADDER, S2VDV, E8)
  //    val vasub_e16  = VIAluOpcode(ASUB , CADDER, S2VDV, E16)
  //    val vasub_e32  = VIAluOpcode(ASUB , CADDER, S2VDV, E32)
  //    val vasub_e64  = VIAluOpcode(ASUB , CADDER, S2VDV, E64)
  //    val vsaddu_e8  = VIAluOpcode(SADDU, CADDER, S2VDV, E8)
  //    val vsaddu_e16 = VIAluOpcode(SADDU, CADDER, S2VDV, E16)
  //    val vsaddu_e32 = VIAluOpcode(SADDU, CADDER, S2VDV, E32)
  //    val vsaddu_e64 = VIAluOpcode(SADDU, CADDER, S2VDV, E64)
  //    val vsadd_e8   = VIAluOpcode(SADD , CADDER, S2VDV, E8)
  //    val vsadd_e16  = VIAluOpcode(SADD , CADDER, S2VDV, E16)
  //    val vsadd_e32  = VIAluOpcode(SADD , CADDER, S2VDV, E32)
  //    val vsadd_e64  = VIAluOpcode(SADD , CADDER, S2VDV, E64)
  //    val vssubu_e8  = VIAluOpcode(SSUBU, CADDER, S2VDV, E8)
  //    val vssubu_e16 = VIAluOpcode(SSUBU, CADDER, S2VDV, E16)
  //    val vssubu_e32 = VIAluOpcode(SSUBU, CADDER, S2VDV, E32)
  //    val vssubu_e64 = VIAluOpcode(SSUBU, CADDER, S2VDV, E64)
  //    val vssub_e8   = VIAluOpcode(SSUB , CADDER, S2VDV, E8)
  //    val vssub_e16  = VIAluOpcode(SSUB , CADDER, S2VDV, E16)
  //    val vssub_e32  = VIAluOpcode(SSUB , CADDER, S2VDV, E32)
  //    val vssub_e64  = VIAluOpcode(SSUB , CADDER, S2VDV, E64)
  //
  //    val vsll_e8   = VIAluOpcode(SLL, SHIFT, S2VDV, E8)
  //    val vsll_e16  = VIAluOpcode(SLL, SHIFT, S2VDV, E16)
  //    val vsll_e32  = VIAluOpcode(SLL, SHIFT, S2VDV, E32)
  //    val vsll_e64  = VIAluOpcode(SLL, SHIFT, S2VDV, E64)
  //    val vsrl_e8   = VIAluOpcode(SRL, SHIFT, S2VDV, E8)
  //    val vsrl_e16  = VIAluOpcode(SRL, SHIFT, S2VDV, E16)
  //    val vsrl_e32  = VIAluOpcode(SRL, SHIFT, S2VDV, E32)
  //    val vsrl_e64  = VIAluOpcode(SRL, SHIFT, S2VDV, E64)
  //    val vsra_e8   = VIAluOpcode(SRA, SHIFT, S2VDV, E8)
  //    val vsra_e16  = VIAluOpcode(SRA, SHIFT, S2VDV, E16)
  //    val vsra_e32  = VIAluOpcode(SRA, SHIFT, S2VDV, E32)
  //    val vsra_e64  = VIAluOpcode(SRA, SHIFT, S2VDV, E64)
  //    val vror_e8   = VIAluOpcode(ROR, SHIFT, S2VDV, E8)
  //    val vror_e16  = VIAluOpcode(ROR, SHIFT, S2VDV, E16)
  //    val vror_e32  = VIAluOpcode(ROR, SHIFT, S2VDV, E32)
  //    val vror_e64  = VIAluOpcode(ROR, SHIFT, S2VDV, E64)
  //    val vrol_e8   = VIAluOpcode(ROL, SHIFT, S2VDV, E8)
  //    val vrol_e16  = VIAluOpcode(ROL, SHIFT, S2VDV, E16)
  //    val vrol_e32  = VIAluOpcode(ROL, SHIFT, S2VDV, E32)
  //    val vrol_e64  = VIAluOpcode(ROL, SHIFT, S2VDV, E64)
  //
  //    val vssrl_e8    = VIAluOpcode(SRL  , CSHIFT, S2VDV, E8)
  //    val vssrl_e16   = VIAluOpcode(SRL  , CSHIFT, S2VDV, E16)
  //    val vssrl_e32   = VIAluOpcode(SRL  , CSHIFT, S2VDV, E32)
  //    val vssrl_e64   = VIAluOpcode(SRL  , CSHIFT, S2VDV, E64)
  //    val vssra_e8    = VIAluOpcode(SRA  , CSHIFT, S2VDV, E8)
  //    val vssra_e16   = VIAluOpcode(SRA  , CSHIFT, S2VDV, E16)
  //    val vssra_e32   = VIAluOpcode(SRA  , CSHIFT, S2VDV, E32)
  //    val vssra_e64   = VIAluOpcode(SRA  , CSHIFT, S2VDV, E64)
  //    val vwsll_e8    = VIAluOpcode(SLL  , CSHIFT, S2VDW, E8)
  //    val vwsll_e16   = VIAluOpcode(SLL  , CSHIFT, S2VDW, E16)
  //    val vwsll_e32   = VIAluOpcode(SLL  , CSHIFT, S2VDW, E32)
  //    val vwsll_e64   = VIAluOpcode(SLL  , CSHIFT, S2VDW, E64)
  //    val vnsrl_e8    = VIAluOpcode(SRL  , CSHIFT, S2WDV, E8)
  //    val vnsrl_e16   = VIAluOpcode(SRL  , CSHIFT, S2WDV, E16)
  //    val vnsrl_e32   = VIAluOpcode(SRL  , CSHIFT, S2WDV, E32)
  //    val vnsrl_e64   = VIAluOpcode(SRL  , CSHIFT, S2WDV, E64)
  //    val vnsra_e8    = VIAluOpcode(SRA  , CSHIFT, S2WDV, E8)
  //    val vnsra_e16   = VIAluOpcode(SRA  , CSHIFT, S2WDV, E16)
  //    val vnsra_e32   = VIAluOpcode(SRA  , CSHIFT, S2WDV, E32)
  //    val vnsra_e64   = VIAluOpcode(SRA  , CSHIFT, S2WDV, E64)
  //    val vnclipu_e8  = VIAluOpcode(CLIPU, CSHIFT, S2WDV, E8)
  //    val vnclipu_e16 = VIAluOpcode(CLIPU, CSHIFT, S2WDV, E16)
  //    val vnclipu_e32 = VIAluOpcode(CLIPU, CSHIFT, S2WDV, E32)
  //    val vnclipu_e64 = VIAluOpcode(CLIPU, CSHIFT, S2WDV, E64)
  //    val vnclip_e8   = VIAluOpcode(CLIP , CSHIFT, S2WDV, E8)
  //    val vnclip_e16  = VIAluOpcode(CLIP , CSHIFT, S2WDV, E16)
  //    val vnclip_e32  = VIAluOpcode(CLIP , CSHIFT, S2WDV, E32)
  //    val vnclip_e64  = VIAluOpcode(CLIP , CSHIFT, S2WDV, E64)
  //  }
  //
  //  object VIAluOpcodes extends VIAluOpcodes
  //
  //  trait VMAluOpcodes extends Opcode[VMAluOpcode] with DataType {
  //    private val DV = bb"00"
  //    private val DX = bb"01"
  //    private val DM = bb"11"
  //
  //    private val CPOP_M = bb"000"
  //    private val CPOP_V = bb"001"
  //    private val FIRST = bb"010"
  //    private val ID    = bb"011"
  //    private val MSBF  = bb"100"
  //    private val MSIF  = bb"101"
  //    private val MSOF  = bb"110"
  //    private val IOTA  = bb"111"
  //
  //    val vcpop_m = VMAluOpcode(CPOP_M, DX, EX)
  //    val vfirst  = VMAluOpcode(FIRST , DX, EX)
  //    val vmsbf   = VMAluOpcode(MSBF  , DM, EX)
  //    val vmsif   = VMAluOpcode(MSIF  , DM, EX)
  //    val vmsof   = VMAluOpcode(MSOF  , DM, EX)
  //
  //    val vcpop_v_e8  = VMAluOpcode(CPOP_V, DV, E8 )
  //    val vcpop_v_e16 = VMAluOpcode(CPOP_V, DV, E16)
  //    val vcpop_v_e32 = VMAluOpcode(CPOP_V, DV, E32)
  //    val vcpop_v_e64 = VMAluOpcode(CPOP_V, DV, E64)
  //    val viota_e8    = VMAluOpcode(IOTA  , DV, E8 )
  //    val viota_e16   = VMAluOpcode(IOTA  , DV, E16)
  //    val viota_e32   = VMAluOpcode(IOTA  , DV, E32)
  //    val viota_e64   = VMAluOpcode(IOTA  , DV, E64)
  //    val vid_e8      = VMAluOpcode(ID    , DV, E8 )
  //    val vid_e16     = VMAluOpcode(ID    , DV, E16)
  //    val vid_e32     = VMAluOpcode(ID    , DV, E32)
  //    val vid_e64     = VMAluOpcode(ID    , DV, E64)
  //
  //  }
  //
  //  object VMAluOpcodes extends VMAluOpcodes
  //
  //  trait VIMacOpcodes extends Opcode[VIMacOpcode] with DataType {
  //    private val S1U = bb"0"
  //    private val S1S = bb"1"
  //    private val S2U = bb"0"
  //    private val S2S = bb"1"
  //
  //    private val DW = bb"1"
  //    private val DV = bb"0"
  //
  //    private val OP2 = bb"0"
  //    private val OP3 = bb"1"
  //
  //    private val MADD  = bb"000"
  //    private val NMSUB = bb"001"
  //    private val MACC  = bb"010"
  //    private val NMSAC = bb"011"
  //
  //    private val MUL  = bb"000"
  //    private val MULH = bb"001"
  //    private val SMUL = bb"010"
  //    private val WADD4 = bb"011"
  //
  //    val vmulhu_e8   = VIMacOpcode(S2U, S1U, MULH , OP2, DV, E8 )
  //    val vmulhu_e16  = VIMacOpcode(S2U, S1U, MULH , OP2, DV, E16)
  //    val vmulhu_e32  = VIMacOpcode(S2U, S1U, MULH , OP2, DV, E32)
  //    val vmulhu_e64  = VIMacOpcode(S2U, S1U, MULH , OP2, DV, E64)
  //    val vmul_e8     = VIMacOpcode(S2S, S1S, MUL  , OP2, DV, E8 )
  //    val vmul_e16    = VIMacOpcode(S2S, S1S, MUL  , OP2, DV, E16)
  //    val vmul_e32    = VIMacOpcode(S2S, S1S, MUL  , OP2, DV, E32)
  //    val vmul_e64    = VIMacOpcode(S2S, S1S, MUL  , OP2, DV, E64)
  //    val vmulhsu_e8  = VIMacOpcode(S2S, S1U, MULH , OP2, DV, E8 )
  //    val vmulhsu_e16 = VIMacOpcode(S2S, S1U, MULH , OP2, DV, E16)
  //    val vmulhsu_e32 = VIMacOpcode(S2S, S1U, MULH , OP2, DV, E32)
  //    val vmulhsu_e64 = VIMacOpcode(S2S, S1U, MULH , OP2, DV, E64)
  //    val vmulh_e8    = VIMacOpcode(S2S, S1S, MULH , OP2, DV, E8 )
  //    val vmulh_e16   = VIMacOpcode(S2S, S1S, MULH , OP2, DV, E16)
  //    val vmulh_e32   = VIMacOpcode(S2S, S1S, MULH , OP2, DV, E32)
  //    val vmulh_e64   = VIMacOpcode(S2S, S1S, MULH , OP2, DV, E64)
  //
  //    val vmadd_e8    = VIMacOpcode(S2S, S1S, MADD , OP3, DV, E8 )
  //    val vmadd_e16   = VIMacOpcode(S2S, S1S, MADD , OP3, DV, E16)
  //    val vmadd_e32   = VIMacOpcode(S2S, S1S, MADD , OP3, DV, E32)
  //    val vmadd_e64   = VIMacOpcode(S2S, S1S, MADD , OP3, DV, E64)
  //    val vnmsub_e8   = VIMacOpcode(S2S, S1S, NMSUB, OP3, DV, E8 )
  //    val vnmsub_e16  = VIMacOpcode(S2S, S1S, NMSUB, OP3, DV, E16)
  //    val vnmsub_e32  = VIMacOpcode(S2S, S1S, NMSUB, OP3, DV, E32)
  //    val vnmsub_e64  = VIMacOpcode(S2S, S1S, NMSUB, OP3, DV, E64)
  //    val vmacc_e8    = VIMacOpcode(S2S, S1S, MACC , OP3, DV, E8 )
  //    val vmacc_e16   = VIMacOpcode(S2S, S1S, MACC , OP3, DV, E16)
  //    val vmacc_e32   = VIMacOpcode(S2S, S1S, MACC , OP3, DV, E32)
  //    val vmacc_e64   = VIMacOpcode(S2S, S1S, MACC , OP3, DV, E64)
  //    val vnmsac_e8   = VIMacOpcode(S2S, S1S, NMSAC, OP3, DV, E8 )
  //    val vnmsac_e16  = VIMacOpcode(S2S, S1S, NMSAC, OP3, DV, E16)
  //    val vnmsac_e32  = VIMacOpcode(S2S, S1S, NMSAC, OP3, DV, E32)
  //    val vnmsac_e64  = VIMacOpcode(S2S, S1S, NMSAC, OP3, DV, E64)
  //
  //    val vwmulu_e8   = VIMacOpcode(S2U, S1U, MUL, OP2, DW, E8 )
  //    val vwmulu_e16  = VIMacOpcode(S2U, S1U, MUL, OP2, DW, E16)
  //    val vwmulu_e32  = VIMacOpcode(S2U, S1U, MUL, OP2, DW, E32)
  //    val vwmulsu_e8  = VIMacOpcode(S2S, S1U, MUL, OP2, DW, E8 )
  //    val vwmulsu_e16 = VIMacOpcode(S2S, S1U, MUL, OP2, DW, E16)
  //    val vwmulsu_e32 = VIMacOpcode(S2S, S1U, MUL, OP2, DW, E32)
  //    val vwmul_e8    = VIMacOpcode(S2S, S1S, MUL, OP2, DW, E8 )
  //    val vwmul_e16   = VIMacOpcode(S2S, S1S, MUL, OP2, DW, E16)
  //    val vwmul_e32   = VIMacOpcode(S2S, S1S, MUL, OP2, DW, E32)
  //
  //    val vwmaccu_e8   = VIMacOpcode(S2U, S1U, MACC, OP2, DW, E8 )
  //    val vwmaccu_e16  = VIMacOpcode(S2U, S1U, MACC, OP2, DW, E16)
  //    val vwmaccu_e32  = VIMacOpcode(S2U, S1U, MACC, OP2, DW, E32)
  //    val vwmacc_e8    = VIMacOpcode(S2S, S1S, MACC, OP2, DW, E8 )
  //    val vwmacc_e16   = VIMacOpcode(S2S, S1S, MACC, OP2, DW, E16)
  //    val vwmacc_e32   = VIMacOpcode(S2S, S1S, MACC, OP2, DW, E32)
  //    val vwmaccus_e8  = VIMacOpcode(S2S, S1U, MACC, OP2, DW, E8 )
  //    val vwmaccus_e16 = VIMacOpcode(S2S, S1U, MACC, OP2, DW, E16)
  //    val vwmaccus_e32 = VIMacOpcode(S2S, S1U, MACC, OP2, DW, E32)
  //    val vwmaccsu_e8  = VIMacOpcode(S2U, S1S, MACC, OP2, DW, E8 )
  //    val vwmaccsu_e16 = VIMacOpcode(S2U, S1S, MACC, OP2, DW, E16)
  //    val vwmaccsu_e32 = VIMacOpcode(S2U, S1S, MACC, OP2, DW, E32)
  //
  //    val vsmul_e8  = VIMacOpcode(S2S, S1S, SMUL, OP2, DV, E8 )
  //    val vsmul_e16 = VIMacOpcode(S2S, S1S, SMUL, OP2, DV, E16)
  //    val vsmul_e32 = VIMacOpcode(S2S, S1S, SMUL, OP2, DV, E32)
  //    val vsmul_e64 = VIMacOpcode(S2S, S1S, SMUL, OP2, DV, E64)
  //  }
  //
  //  object VIMacOpcodes extends VIMacOpcodes
  //
  //  trait VIDivOpcodes extends Opcode[VIDivOpcode] with DataType {
  //    private val DIVU = bb"00"
  //    private val DIV  = bb"01"
  //    private val REMU = bb"10"
  //    private val REM  = bb"11"
  //
  //    val vdivu_e8  = VIDivOpcode(DIVU, E8 )
  //    val vdivu_e16 = VIDivOpcode(DIVU, E16)
  //    val vdivu_e32 = VIDivOpcode(DIVU, E32)
  //    val vdivu_e64 = VIDivOpcode(DIVU, E64)
  //    val vdiv_e8   = VIDivOpcode(DIV , E8 )
  //    val vdiv_e16  = VIDivOpcode(DIV , E16)
  //    val vdiv_e32  = VIDivOpcode(DIV , E32)
  //    val vdiv_e64  = VIDivOpcode(DIV , E64)
  //    val vremu_e8  = VIDivOpcode(REMU, E8 )
  //    val vremu_e16 = VIDivOpcode(REMU, E16)
  //    val vremu_e32 = VIDivOpcode(REMU, E32)
  //    val vremu_e64 = VIDivOpcode(REMU, E64)
  //    val vrem_e8   = VIDivOpcode(REM , E8 )
  //    val vrem_e16  = VIDivOpcode(REM , E16)
  //    val vrem_e32  = VIDivOpcode(REM , E32)
  //    val vrem_e64  = VIDivOpcode(REM , E64)
  //  }
  //
  //  object VIDivOpcodes extends VIDivOpcodes
  //
  //  trait VIRedOpcodes extends Opcode[VIRedOpcode] with DataType {
  //    private val DV = bb"0"
  //    private val DW = bb"1"
  //
  //    private val SUM  = bb"0000"
  //    private val AND  = bb"0001"
  //    private val OR   = bb"0010"
  //    private val XOR  = bb"0011"
  //    private val MINU = bb"0100"
  //    private val MIN  = bb"0101"
  //    private val MAXU = bb"0110"
  //    private val MAX  = bb"0111"
  //
  //    private val SUMU = bb"1000"
  //
  //    val vredsum_e8   = VIRedOpcode(SUM , DV, E8 )
  //    val vredsum_e16  = VIRedOpcode(SUM , DV, E16)
  //    val vredsum_e32  = VIRedOpcode(SUM , DV, E32)
  //    val vredsum_e64  = VIRedOpcode(SUM , DV, E64)
  //    val vredand_e8   = VIRedOpcode(AND , DV, E8 )
  //    val vredand_e16  = VIRedOpcode(AND , DV, E16)
  //    val vredand_e32  = VIRedOpcode(AND , DV, E32)
  //    val vredand_e64  = VIRedOpcode(AND , DV, E64)
  //    val vredor_e8    = VIRedOpcode(OR  , DV, E8 )
  //    val vredor_e16   = VIRedOpcode(OR  , DV, E16)
  //    val vredor_e32   = VIRedOpcode(OR  , DV, E32)
  //    val vredor_e64   = VIRedOpcode(OR  , DV, E64)
  //    val vredxor_e8   = VIRedOpcode(XOR , DV, E8 )
  //    val vredxor_e16  = VIRedOpcode(XOR , DV, E16)
  //    val vredxor_e32  = VIRedOpcode(XOR , DV, E32)
  //    val vredxor_e64  = VIRedOpcode(XOR , DV, E64)
  //    val vredminu_e8  = VIRedOpcode(MINU, DV, E8 )
  //    val vredminu_e16 = VIRedOpcode(MINU, DV, E16)
  //    val vredminu_e32 = VIRedOpcode(MINU, DV, E32)
  //    val vredminu_e64 = VIRedOpcode(MINU, DV, E64)
  //    val vredmin_e8   = VIRedOpcode(MIN , DV, E8 )
  //    val vredmin_e16  = VIRedOpcode(MIN , DV, E16)
  //    val vredmin_e32  = VIRedOpcode(MIN , DV, E32)
  //    val vredmin_e64  = VIRedOpcode(MIN , DV, E64)
  //    val vredmaxu_e8  = VIRedOpcode(MAXU, DV, E8 )
  //    val vredmaxu_e16 = VIRedOpcode(MAXU, DV, E16)
  //    val vredmaxu_e32 = VIRedOpcode(MAXU, DV, E32)
  //    val vredmaxu_e64 = VIRedOpcode(MAXU, DV, E64)
  //    val vredmax_e8   = VIRedOpcode(MAX , DV, E8 )
  //    val vredmax_e16  = VIRedOpcode(MAX , DV, E16)
  //    val vredmax_e32  = VIRedOpcode(MAX , DV, E32)
  //    val vredmax_e64  = VIRedOpcode(MAX , DV, E64)
  //
  //    val vwredsum_e8   = VIRedOpcode(SUM, DW, E8 )
  //    val vwredsum_e16  = VIRedOpcode(SUM, DW, E16)
  //    val vwredsum_e32  = VIRedOpcode(SUM, DW, E32)
  //    val vwredsumu_e8  = VIRedOpcode(SUMU, DW, E8 )
  //    val vwredsumu_e16 = VIRedOpcode(SUMU, DW, E16)
  //    val vwredsumu_e32 = VIRedOpcode(SUMU, DW, E32)
  //  }
  //
  //  object VIRedOpcodes extends VIRedOpcodes
  //
  //  trait VIPermOpcodes extends Opcode[VIPermOpcode] with DataType {
  //
  //    // funct6(4) ## funct6(1,0) ## funct3(2,1)
  //    private val RGATHER_V     = bb"00000"
  //    private val RGATHER_X     = bb"00010"
  //    private val RGATHER_I     = bb"00001"
  //    private val RGATHER_EI16  = bb"01000"
  //    private val SLIDEUP       = bb"01010"
  //    private val SLIDEDOWN     = bb"01110"
  //    private val COMPRESS      = bb"11101"
  //    private val SLIDE1UP      = bb"01001"
  //    private val SLIDE1DOWN    = bb"01101"
  //
  //    val vrgather_v_e8     = VIPermOpcode(RGATHER_V   , E8 )
  //    val vrgather_v_e16    = VIPermOpcode(RGATHER_V   , E16)
  //    val vrgather_v_e32    = VIPermOpcode(RGATHER_V   , E32)
  //    val vrgather_v_e64    = VIPermOpcode(RGATHER_V   , E64)
  //    val vrgather_x_e8     = VIPermOpcode(RGATHER_X   , E8 )
  //    val vrgather_x_e16    = VIPermOpcode(RGATHER_X   , E16)
  //    val vrgather_x_e32    = VIPermOpcode(RGATHER_X   , E32)
  //    val vrgather_x_e64    = VIPermOpcode(RGATHER_X   , E64)
  //    val vrgather_i_e8     = VIPermOpcode(RGATHER_I   , E8 )
  //    val vrgather_i_e16    = VIPermOpcode(RGATHER_I   , E16)
  //    val vrgather_i_e32    = VIPermOpcode(RGATHER_I   , E32)
  //    val vrgather_i_e64    = VIPermOpcode(RGATHER_I   , E64)
  //    val vrgather_ei16_e8  = VIPermOpcode(RGATHER_EI16, E8 )
  //    val vrgather_ei16_e16 = VIPermOpcode(RGATHER_EI16, E16)
  //    val vrgather_ei16_e32 = VIPermOpcode(RGATHER_EI16, E32)
  //    val vrgather_ei16_e64 = VIPermOpcode(RGATHER_EI16, E64)
  //    val vslideup_e8       = VIPermOpcode(SLIDEUP     , E8 )
  //    val vslideup_e16      = VIPermOpcode(SLIDEUP     , E16)
  //    val vslideup_e32      = VIPermOpcode(SLIDEUP     , E32)
  //    val vslideup_e64      = VIPermOpcode(SLIDEUP     , E64)
  //    val vslidedown_e8     = VIPermOpcode(SLIDEDOWN   , E8 )
  //    val vslidedown_e16    = VIPermOpcode(SLIDEDOWN   , E16)
  //    val vslidedown_e32    = VIPermOpcode(SLIDEDOWN   , E32)
  //    val vslidedown_e64    = VIPermOpcode(SLIDEDOWN   , E64)
  //    val vcompress_e8      = VIPermOpcode(COMPRESS    , E8 )
  //    val vcompress_e16     = VIPermOpcode(COMPRESS    , E16)
  //    val vcompress_e32     = VIPermOpcode(COMPRESS    , E32)
  //    val vcompress_e64     = VIPermOpcode(COMPRESS    , E64)
  //    val vslide1up_e8      = VIPermOpcode(SLIDE1UP    , E8 )
  //    val vslide1up_e16     = VIPermOpcode(SLIDE1UP    , E16)
  //    val vslide1up_e32     = VIPermOpcode(SLIDE1UP    , E32)
  //    val vslide1up_e64     = VIPermOpcode(SLIDE1UP    , E64)
  //    val vslide1down_e8    = VIPermOpcode(SLIDE1DOWN  , E8 )
  //    val vslide1down_e16   = VIPermOpcode(SLIDE1DOWN  , E16)
  //    val vslide1down_e32   = VIPermOpcode(SLIDE1DOWN  , E32)
  //    val vslide1down_e64   = VIPermOpcode(SLIDE1DOWN  , E64)
  //  }
  //
  //  object VIPermOpcodes extends VIPermOpcodes

  val ALUOpType = AluOpcodes
  val BRUOpType = BruOpcodes
  val JumpOpType = JmpOpcodes
  val FenceOpType = FenceOpcodes
  val MULOpType = MulOpcodes
  val DIVOpType = DivOpcodes
  val CSROpType = CsrOpcodes
  val LSUOpType = LsuOpcodes
  val BKUOpType = BkuOpcodes
}
