package xiangshan.backend.decode.opcode

import chisel3._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util._
import utility.LookupTree
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
  val VIAluOpcodes  = Opcodes.VIAluOpcode
  val VMAluOpcodes  = Opcodes.VMAluOpcode
  val VIMacOpcodes  = Opcodes.VIMacOpcode
  val VIDivOpcodes  = Opcodes.VIDivOpcode
  val VIRedOpcodes  = Opcodes.VIRedOpcode
  val VIPermOpcodes = Opcodes.VIPermOpcode
  val VMoveOpcodes  = Opcodes.VMoveOpcode
  val VSha256msOpcodes = Opcodes.VSha256msOpcode
  val VSha256cOpcodes = Opcodes.VSha256cOpcode
  val FCvtOpcodes = Opcodes.FCvtOpcode
  val VFCvtOpcodes = Opcodes.VFCvtOpcode
  val FMiscOpcodes = Opcodes.FMiscOpcode
  val VFMiscOpcodes = Opcodes.VFMiscOpcode
  val FMacOpcodes = Opcodes.FMacOpcode
  val VFMacOpcodes = Opcodes.VFMacOpcode

  // Todo: remove these
  def X = BitPat("b0_0000_0000")
  def FMVXF = BitPat("b1_1000_0000") //for fmv_x_d & fmv_x_w

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

    def jumpOpisJalr(op: UInt) = op(0)
    def jumpOpisAuipc(op: UInt) = op(1)
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

    val idxDC = bb"00" // Don't care
    val idx0 = bb"00"
    val idx1 = bb"01"
    val idx2 = bb"10"
    val idx3 = bb"11"

    val unsign = bb"1"
    val sign = bb"0"

    protected val isH = bb"1"
    protected val nonH = bb"0"

    // isX means that the uop needs execute permission.
    // E.g. hlvx.hu, hlvx.wu
    protected val isX = bb"1"
    protected val nonX = bb"0"

    protected val SCALAR_PREFIX = bb"00"
    protected val SCALAR0 = SCALAR_PREFIX ## bb"00"
    protected val SCALAR1 = SCALAR_PREFIX ## bb"01"
    protected val SCALAR2 = SCALAR_PREFIX ## bb"10"
    protected val SCALAR3 = SCALAR_PREFIX ## bb"11"
    protected val SCALAR  = SCALAR0
    protected val US      = bb"0100" // Unit-Stride
    protected val CS      = bb"0101" // Const-Strided
    protected val WHOLE   = bb"0110"
    protected val MASK    = bb"0111"
    protected val IUEI8   = bb"1000" // Index-Unordered
    protected val IUEI16  = bb"1001" // Index-Unordered
    protected val IUEI32  = bb"1010" // Index-Unordered
    protected val IUEI64  = bb"1011" // Index-Unordered
    protected val IOEI8   = bb"1100" // Index-Ordered
    protected val IOEI16  = bb"1101" // Index-Ordered
    protected val IOEI32  = bb"1110" // Index-Ordered
    protected val IOEI64  = bb"1111" // Index-Ordered

    protected val isFof = bb"1"
    protected val nonFof = bb"0"

    protected def getMemOpType(op: UInt): UInt = op(10, 7)

    def isScalaOp(op: UInt): Bool = getMemOpType(op)(3, 2) === SCALAR_PREFIX

    def isVecMemOp(op: UInt): Bool = getMemOpType(op)(3, 2) =/= SCALAR_PREFIX

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
    val lb     = IntIType(SCALAR, nonH, nonX, sign  , B, uopLoad)
    val lh     = IntIType(SCALAR, nonH, nonX, sign  , H, uopLoad)
    val lw     = IntIType(SCALAR, nonH, nonX, sign  , W, uopLoad)
    val ld     = IntIType(SCALAR, nonH, nonX, sign  , D, uopLoad)
    val lq     = IntIType(SCALAR, nonH, nonX, sign  , Q, uopLoad) // TODO: no corresponding store instruction
    val lbu    = IntIType(SCALAR, nonH, nonX, unsign, B, uopLoad)
    val lhu    = IntIType(SCALAR, nonH, nonX, unsign, H, uopLoad)
    val lwu    = IntIType(SCALAR, nonH, nonX, unsign, W, uopLoad)
    // hypervior load
    val hlvb   = IntIType(SCALAR, isH, nonX, sign  , B, uopLoad)
    val hlvh   = IntIType(SCALAR, isH, nonX, sign  , H, uopLoad)
    val hlvw   = IntIType(SCALAR, isH, nonX, sign  , W, uopLoad)
    val hlvd   = IntIType(SCALAR, isH, nonX, sign  , D, uopLoad)
    val hlvbu  = IntIType(SCALAR, isH, nonX, unsign, B, uopLoad)
    val hlvhu  = IntIType(SCALAR, isH, nonX, unsign, H, uopLoad)
    val hlvwu  = IntIType(SCALAR, isH, nonX, unsign, W, uopLoad)
    val hlvxhu = IntIType(SCALAR, isH, isX , unsign, H, uopLoad)
    val hlvxwu = IntIType(SCALAR, isH, isX , unsign, W, uopLoad)

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
    val prefetch_i = Value(SCALAR, nonH, nonX, prefetchI, uopPrefetch)
    val prefetch_r = Value(SCALAR, nonH, nonX, prefetchR, uopPrefetch)
    val prefetch_w = Value(SCALAR, nonH, nonX, prefetchW, uopPrefetch)

    def getUopType(op: UInt): UInt = op(0)

    def isPrefetch(op: UInt): Bool = getUopType(op) === uopPrefetch

    def getIsH(op: UInt): UInt = op(6)

    def getIsX(op: UInt): UInt = op(5)

    def getSign(op: UInt): UInt = op(4)

    def getLdType(op: UInt): UInt = op(10, 7)

    def getLduOp(op: UInt): UInt = getLdType(op) ## getIsH(op) ## getIsX(op) ## getSign(op)

    def formDifftestLduOpcode(op: UInt): UInt = {
      val lduOp = getLduOp(op)
      val sz = size(op)
      val diffLduOp: UInt = LookupTree(lduOp, difftestLduOpMap)
      Cat(diffLduOp, sz(1, 0))
    }

    object LduDifftestOpcode {
      def ls = "b000".U
      def lu = "b001".U
      def hlvs = "b100".U
      def hlvu = "b101".U
      def hlvxu = "b111".U
    }

    object LduOp {
      val ls    = SCALAR ## nonH ## nonX ## sign
      val lu    = SCALAR ## nonH ## nonX ## unsign
      val hlvs  = SCALAR ##  isH ## nonX ## sign
      val hlvu  = SCALAR ##  isH ## nonX ## unsign
      val hlvxu = SCALAR ##  isH ##  isX ## unsign
    }

    val difftestLduOpMap: Seq[(BitPat, UInt)] = Seq(
      LduOp.ls    -> LduDifftestOpcode.ls   ,
      LduOp.lu    -> LduDifftestOpcode.lu   ,
      LduOp.hlvs  -> LduDifftestOpcode.hlvs ,
      LduOp.hlvu  -> LduDifftestOpcode.hlvu ,
      LduOp.hlvxu -> LduDifftestOpcode.hlvxu,
    )
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
    val sb = IntBSType(SCALAR, nonH, nonX, sign, B, uopStore)
    val sh = IntBSType(SCALAR, nonH, nonX, sign, H, uopStore)
    val sw = IntBSType(SCALAR, nonH, nonX, sign, W, uopStore)
    val sd = IntBSType(SCALAR, nonH, nonX, sign, D, uopStore)
    val sq = IntBSType(SCALAR, nonH, nonX, sign, Q, uopStore) // TODO: no corresponding store instruction

    //hypervisor store
    val hsvb = IntBSType(SCALAR, isH, nonX, sign, B, uopStore)
    val hsvh = IntBSType(SCALAR, isH, nonX, sign, H, uopStore)
    val hsvw = IntBSType(SCALAR, isH, nonX, sign, W, uopStore)
    val hsvd = IntBSType(SCALAR, isH, nonX, sign, D, uopStore)

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
    val cbo_zero  = IntIType(SCALAR, nonH, nonX, sign, CBO.zero , uopCbo) + NoSpec + BlockBack
    // llc op
    val cbo_clean = IntIType(SCALAR, nonH, nonX, sign, CBO.clean, uopCbo) + NoSpec + BlockBack
    val cbo_flush = IntIType(SCALAR, nonH, nonX, sign, CBO.flush, uopCbo) + NoSpec + BlockBack
    val cbo_inval = IntIType(SCALAR, nonH, nonX, sign, CBO.inval, uopCbo) + NoSpec + BlockBack

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
    //                       4b        3b        3b   1b
    val amoswap_b = IntRType(SCALAR, NoALU.swap  , B, noALU)   + NoSpec + BlockBack
    val amoadd_b  = IntRType(SCALAR, WithALU.add , B, withALU) + NoSpec + BlockBack
    val amoxor_b  = IntRType(SCALAR, WithALU.xor , B, withALU) + NoSpec + BlockBack
    val amoand_b  = IntRType(SCALAR, WithALU.and , B, withALU) + NoSpec + BlockBack
    val amoor_b   = IntRType(SCALAR, WithALU.or  , B, withALU) + NoSpec + BlockBack
    val amomin_b  = IntRType(SCALAR, WithALU.min , B, withALU) + NoSpec + BlockBack
    val amomax_b  = IntRType(SCALAR, WithALU.max , B, withALU) + NoSpec + BlockBack
    val amominu_b = IntRType(SCALAR, WithALU.minu, B, withALU) + NoSpec + BlockBack
    val amomaxu_b = IntRType(SCALAR, WithALU.maxu, B, withALU) + NoSpec + BlockBack

    val amoswap_h = IntRType(SCALAR, NoALU.swap  , H, noALU)   + NoSpec + BlockBack
    val amoadd_h  = IntRType(SCALAR, WithALU.add , H, withALU) + NoSpec + BlockBack
    val amoxor_h  = IntRType(SCALAR, WithALU.xor , H, withALU) + NoSpec + BlockBack
    val amoand_h  = IntRType(SCALAR, WithALU.and , H, withALU) + NoSpec + BlockBack
    val amoor_h   = IntRType(SCALAR, WithALU.or  , H, withALU) + NoSpec + BlockBack
    val amomin_h  = IntRType(SCALAR, WithALU.min , H, withALU) + NoSpec + BlockBack
    val amomax_h  = IntRType(SCALAR, WithALU.max , H, withALU) + NoSpec + BlockBack
    val amominu_h = IntRType(SCALAR, WithALU.minu, H, withALU) + NoSpec + BlockBack
    val amomaxu_h = IntRType(SCALAR, WithALU.maxu, H, withALU) + NoSpec + BlockBack

    val lr_w      = IntIType(SCALAR, NoALU.lr    , W, noALU)   + NoSpec + BlockBack
    val sc_w      = IntRType(SCALAR, NoALU.sc    , W, noALU)   + NoSpec + BlockBack
    val amoswap_w = IntRType(SCALAR, NoALU.swap  , W, noALU)   + NoSpec + BlockBack
    val amoadd_w  = IntRType(SCALAR, WithALU.add , W, withALU) + NoSpec + BlockBack
    val amoxor_w  = IntRType(SCALAR, WithALU.xor , W, withALU) + NoSpec + BlockBack
    val amoand_w  = IntRType(SCALAR, WithALU.and , W, withALU) + NoSpec + BlockBack
    val amoor_w   = IntRType(SCALAR, WithALU.or  , W, withALU) + NoSpec + BlockBack
    val amomin_w  = IntRType(SCALAR, WithALU.min , W, withALU) + NoSpec + BlockBack
    val amomax_w  = IntRType(SCALAR, WithALU.max , W, withALU) + NoSpec + BlockBack
    val amominu_w = IntRType(SCALAR, WithALU.minu, W, withALU) + NoSpec + BlockBack
    val amomaxu_w = IntRType(SCALAR, WithALU.maxu, W, withALU) + NoSpec + BlockBack

    val lr_d      = IntIType(SCALAR, NoALU.lr    , D, noALU)   + NoSpec + BlockBack
    val sc_d      = IntRType(SCALAR, NoALU.sc    , D, noALU)   + NoSpec + BlockBack
    val amoswap_d = IntRType(SCALAR, NoALU.swap  , D, noALU)   + NoSpec + BlockBack
    val amoadd_d  = IntRType(SCALAR, WithALU.add , D, withALU) + NoSpec + BlockBack
    val amoxor_d  = IntRType(SCALAR, WithALU.xor , D, withALU) + NoSpec + BlockBack
    val amoand_d  = IntRType(SCALAR, WithALU.and , D, withALU) + NoSpec + BlockBack
    val amoor_d   = IntRType(SCALAR, WithALU.or  , D, withALU) + NoSpec + BlockBack
    val amomin_d  = IntRType(SCALAR, WithALU.min , D, withALU) + NoSpec + BlockBack
    val amomax_d  = IntRType(SCALAR, WithALU.max , D, withALU) + NoSpec + BlockBack
    val amominu_d = IntRType(SCALAR, WithALU.minu, D, withALU) + NoSpec + BlockBack
    val amomaxu_d = IntRType(SCALAR, WithALU.maxu, D, withALU) + NoSpec + BlockBack

    val amocas_b_0 = Value(SCALAR1, NoALU.cas, B, noALU)                  + Src2Gp + NoSpec
    val amocas_b_1 = Value(SCALAR0, NoALU.cas, B, noALU) + GpWen + Src1Gp + Src2Gp          + BlockBack
    val amocas_h_0 = Value(SCALAR1, NoALU.cas, H, noALU)                  + Src2Gp + NoSpec
    val amocas_h_1 = Value(SCALAR0, NoALU.cas, H, noALU) + GpWen + Src1Gp + Src2Gp          + BlockBack
    val amocas_w_0 = Value(SCALAR1, NoALU.cas, W, noALU)                  + Src2Gp + NoSpec
    val amocas_w_1 = Value(SCALAR0, NoALU.cas, W, noALU) + GpWen + Src1Gp + Src2Gp          + BlockBack
    val amocas_d_0 = Value(SCALAR1, NoALU.cas, D, noALU)                  + Src2Gp + NoSpec
    val amocas_d_1 = Value(SCALAR0, NoALU.cas, D, noALU) + GpWen + Src1Gp + Src2Gp          + BlockBack
    val amocas_q_0 = Value(SCALAR1, NoALU.cas, Q, noALU)                  + Src2Gp + NoSpec
    val amocas_q_1 = Value(SCALAR0, NoALU.cas, Q, noALU) + GpWen + Src1Gp + Src2Gp
    val amocas_q_2 = Value(SCALAR3, NoALU.cas, Q, noALU)                  + Src2Gp
    val amocas_q_3 = Value(SCALAR2, NoALU.cas, Q, noALU) + GpWen          + Src2Gp          + BlockBack

    // Note: Amo instructions all have SCALAR MemOptype
    def getAmocasUopIdx(op: UInt): UInt = getMemOpType(op)(1, 0)

    def amoSize(op: UInt): UInt = op(AMOSize.width, 1)

    def amoSizeIs(sz: AMOSize.type => AMOSize)(op: UInt): Bool = amoSize(op) === sz(AMOSize).U

    sealed abstract class AMOSize(uint: UInt) {
      def U: UInt = this.uint
    }
    object AMOSize {
      val width = 3
      case object B extends AMOSize("b000".U(width.W))
      case object H extends AMOSize("b001".U(width.W))
      case object W extends AMOSize("b010".U(width.W))
      case object D extends AMOSize("b011".U(width.W))
      case object Q extends AMOSize("b100".U(width.W))
    }

    def getAmoOp(op: UInt): UInt = op(6, 4) ## op(0)

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
    def isAMOCASNotQ(op: UInt): Bool = isAMOCAS(op) && !isAMOCASQ(op)
    def isAMOCASWD(op: UInt): Bool = isAMOCAS(op) && (size(op) === W || size(op) === D)
    def formDifftestOpcode(op: UInt): UInt = {
      val amoOp = getAmoOp(op)
      val sz = size(op)
      val diffAmoOp: UInt = LookupTree(amoOp, difftestAmoOpMap)
      Cat(diffAmoOp, sz)
    }

    object DifftestOpcode {
      def lr      = "b0000".U
      def sc      = "b0001".U
      def amoswap = "b0010".U
      def amoadd  = "b0011".U
      def amoxor  = "b0100".U
      def amoand  = "b0101".U
      def amoor   = "b0110".U
      def amomin  = "b0111".U
      def amomax  = "b1000".U
      def amominu = "b1001".U
      def amomaxu = "b1010".U
      def amocas  = "b1011".U
    }

    val difftestAmoOpMap: Seq[(BitPat, UInt)] = Seq(
      AmoOp.lr   -> DifftestOpcode.lr,
      AmoOp.sc   -> DifftestOpcode.sc,
      AmoOp.swap -> DifftestOpcode.amoswap,
      AmoOp.cas  -> DifftestOpcode.amocas,
      AmoOp.add  -> DifftestOpcode.amoadd,
      AmoOp.xor  -> DifftestOpcode.amoxor,
      AmoOp.and  -> DifftestOpcode.amoand,
      AmoOp.or   -> DifftestOpcode.amoor,
      AmoOp.min  -> DifftestOpcode.amomin,
      AmoOp.max  -> DifftestOpcode.amomax,
      AmoOp.minu -> DifftestOpcode.amominu,
      AmoOp.maxu -> DifftestOpcode.amomaxu,
    )
  }

  object LduOpcodes extends LduOpcodes

  object StuOpcodes extends StuOpcodes

  object AmoOpcodes extends AmoOpcodes

  object LsuOpcodes extends LduOpcodes with StuOpcodes with AmoOpcodes {
    def formLoadEventOpcode(isAtomic: Bool, opcode: UInt): UInt = {
      Mux(
        isAtomic,
        super[AmoOpcodes].formDifftestOpcode(opcode),
        super[LduOpcodes].formDifftestLduOpcode(opcode),
      )
    }
  }

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
    val fence    = Value    (bb"10000") + NoSpec + BlockBack + FlushPipe // FENCE           / PAUSE
    val sfence   = IntBSType(bb"10001") + NoSpec + BlockBack + FlushPipe // SFENCE_VMA      / SINVAL_VMA (no flushpipe)
    val mfence   = IntBSType(bb"10111") + NoSpec + BlockBack + FlushPipe         // HasMptCheck self defined instruction
    val fencei   = Value    (bb"10010") + NoSpec + BlockBack + FlushPipe // FENCE_I
    val hfence_v = IntBSType(bb"10011") + NoSpec + BlockBack + FlushPipe // HFENCE_VVMA     / HINVAL_VVMA (no flushpipe)
    val hfence_g = IntBSType(bb"10100") + NoSpec + BlockBack + FlushPipe // HFENCE_GVMA     / HINVAL_GVMA (no flushpipe)
    val nofence  = Value    (bb"00000") + NoSpec + BlockBack + FlushPipe // SFENCE_INVAL_IR / SFENCE_W_INVAL (no flushpipe)
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

  object FAluOpcodes extends Opcodes.FMacOpcode
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
        noSpec = traits.contains(NoSpec),
        blockBack = traits.contains(BlockBack),
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
