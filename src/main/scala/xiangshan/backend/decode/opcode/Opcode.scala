package xiangshan.backend.decode.opcode

import chisel3._
import chisel3.util._
import xiangshan.FuOpType
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField
import chisel3.UInt
import xiangshan.backend.vector.util.BString.BinaryStringHelper
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField

import scala.language.implicitConversions

sealed abstract class Opcode(private val uint: UInt*) {
  private var width: Int = 0
  val encode: UInt = uint.reverse.reduce(_ ### _)

  Opcode.updateCommonWidth(encode.getWidth)

  this.updateWidth(encode.getWidth)

  private def updateWidth(w: Int): Unit = this.width = w max this.width

  def getWidth: Int = this.width
}

object Opcode {
  def apply(): UInt = UInt(getCommonWidth.W)

  private var commonWidth: Int = 0

  def updateCommonWidth(w: Int): Unit = Opcode.commonWidth = w.max(Opcode.commonWidth)

  def getCommonWidth: Int = Opcode.commonWidth

  case class AluOpcode(private val uint: UInt*) extends Opcode(uint: _*)
  case class BruOpcode(private val uint: UInt*) extends Opcode(uint: _*)
  case class JmpOpcode(private val uint: UInt*) extends Opcode(uint: _*)
  case class MulOpcode(private val uint: UInt*) extends Opcode(uint: _*)
  case class DivOpcode(private val uint: UInt*) extends Opcode(uint: _*)
  case class LduOpcode(private val uint: UInt*) extends Opcode(uint: _*)
  case class StuOpcode(private val uint: UInt*) extends Opcode(uint: _*)
  case class AmoOpcode(private val uint: UInt*) extends Opcode(uint: _*)
  case class BkuOpcode(private val uint: UInt*) extends Opcode(uint: _*)
  case class CsrOpcode(private val uint: UInt*) extends Opcode(uint: _*)
  case class FenceOpcode(private val uint: UInt*) extends Opcode(uint: _*)
  case class FmacOpcode(private val uint: UInt*) extends Opcode(uint: _*)
  case class FdivOpcode(private val uint: UInt*) extends Opcode(uint: _*)
  case class FcvtOpcode(private val uint: UInt*) extends Opcode(uint: _*)
  case class FmiscOpcode(private val uint: UInt*) extends Opcode(uint: _*)

  object AluOpcodes {
    val slliuw     = AluOpcode("b000_0000".U) // slliuw: ZEXT(src1[31:0]) << shamt
    val sll        = AluOpcode("b000_0001".U) // sll:     src1 << src2

    val bclr       = AluOpcode("b000_0010".U) // bclr:    src1 & ~(1 << src2[5:0])
    val bset       = AluOpcode("b000_0011".U) // bset:    src1 | (1 << src2[5:0])
    val binv       = AluOpcode("b000_0100".U) // binv:    src1 ^ (1 << src2[5:0])

    val srl        = AluOpcode("b000_0101".U) // srl:     src1 >> src2
    val bext       = AluOpcode("b000_0110".U) // bext:    (src1 >> src2)[0]
    val sra        = AluOpcode("b000_0111".U) // sra:     src1 >> src2 (arithmetic)

    val rol        = AluOpcode("b000_1001".U) // rol:     (src1 << src2) | (src1 >> (xlen - src2))
    val ror        = AluOpcode("b000_1011".U) // ror:     (src1 >> src2) | (src1 << (xlen - src2))

    // RV64 32bit optype
    val addw       = AluOpcode("b001_0000".U) // addw:      SEXT((src1 + src2)[31:0])
    val oddaddw    = AluOpcode("b001_0001".U) // oddaddw:   SEXT((src1[0] + src2)[31:0])
    val subw       = AluOpcode("b001_0010".U) // subw:      SEXT((src1 - src2)[31:0])
    val lui32addw  = AluOpcode("b001_0011".U) // lui32addw: SEXT(SEXT(src2[11:0], 32) + {src2[31:12], 12'b0}, 64)

    val addwbit    = AluOpcode("b001_0100".U) // addwbit:   (src1 + src2)[0]
    val addwbyte   = AluOpcode("b001_0101".U) // addwbyte:  (src1 + src2)[7:0]
    val addwzexth  = AluOpcode("b001_0110".U) // addwzexth: ZEXT((src1  + src2)[15:0])
    val addwsexth  = AluOpcode("b001_0111".U) // addwsexth: SEXT((src1  + src2)[15:0])

    val sllw       = AluOpcode("b001_1000".U) // sllw:     SEXT((src1 << src2)[31:0])
    val srlw       = AluOpcode("b001_1001".U) // srlw:     SEXT((src1[31:0] >> src2)[31:0])
    val sraw       = AluOpcode("b001_1010".U) // sraw:     SEXT((src1[31:0] >> src2)[31:0])
    val rolw       = AluOpcode("b001_1100".U)
    val rorw       = AluOpcode("b001_1101".U)

    // ADD-op
    val adduw      = AluOpcode("b010_0000".U) // adduw:  src1[31:0]  + src2
    val oddadd     = AluOpcode("b010_0001".U) // oddadd:  src1[0]     + src2
    val add        = AluOpcode("b010_0010".U) // add:     src1        + src2
    val lui32add   = AluOpcode("b010_0011".U) // lui32add: SEXT(src2[11:0]) + {src2[63:12], 12'b0}

    val sr29add    = AluOpcode("b010_0100".U) // sr29add: src1[63:29] + src2
    val sr30add    = AluOpcode("b010_0101".U) // sr30add: src1[63:30] + src2
    val sr31add    = AluOpcode("b010_0110".U) // sr31add: src1[63:31] + src2
    val sr32add    = AluOpcode("b010_0111".U) // sr32add: src1[63:32] + src2

    val sh1adduw   = AluOpcode("b010_1000".U) // sh1adduw: {src1[31:0], 1'b0} + src2
    val sh1add     = AluOpcode("b010_1001".U) // sh1add: {src1[62:0], 1'b0} + src2
    val sh2adduw   = AluOpcode("b010_1010".U) // sh2add_uw: {src1[31:0], 2'b0} + src2
    val sh2add     = AluOpcode("b010_1011".U) // sh2add: {src1[61:0], 2'b0} + src2
    val sh3adduw   = AluOpcode("b010_1100".U) // sh3add_uw: {src1[31:0], 3'b0} + src2
    val sh3add     = AluOpcode("b010_1101".U) // sh3add: {src1[60:0], 3'b0} + src2
    val sh4add     = AluOpcode("b010_1111".U) // sh4add: {src1[59:0], 4'b0} + src2

    // SUB-op: src1 - src2
    val sub        = AluOpcode("b011_0000".U)
    val sltu       = AluOpcode("b011_0001".U)
    val slt        = AluOpcode("b011_0010".U)
    val maxu       = AluOpcode("b011_0100".U)
    val minu       = AluOpcode("b011_0101".U)
    val max        = AluOpcode("b011_0110".U)
    val min        = AluOpcode("b011_0111".U)

    // Zicond
    val czero_eqz  = AluOpcode("b111_0100".U)
    val czero_nez  = AluOpcode("b111_0110".U)

    // misc optype
    val and        = AluOpcode("b100_0000".U)
    val andn       = AluOpcode("b100_0001".U)
    val or         = AluOpcode("b100_0010".U)
    val orn        = AluOpcode("b100_0011".U)
    val xor        = AluOpcode("b100_0100".U)
    val xnor       = AluOpcode("b100_0101".U)
    val orcb       = AluOpcode("b100_0110".U)

    val sextb      = AluOpcode("b100_1000".U)
    val packh      = AluOpcode("b100_1001".U)
    val sexth      = AluOpcode("b100_1010".U)
    val packw      = AluOpcode("b100_1011".U)

    val revb       = AluOpcode("b101_0000".U)
    val rev8       = AluOpcode("b101_0001".U)
    val pack       = AluOpcode("b101_0010".U)
    val orh48      = AluOpcode("b101_0011".U)

    val szewl1     = AluOpcode("b101_1000".U)
    val szewl2     = AluOpcode("b101_1001".U)
    val szewl3     = AluOpcode("b101_1010".U)
    val byte2      = AluOpcode("b101_1011".U)

    val andlsb     = AluOpcode("b110_0000".U)
    val andzexth   = AluOpcode("b110_0001".U)
    val orlsb      = AluOpcode("b110_0010".U)
    val orzexth    = AluOpcode("b110_0011".U)
    val xorlsb     = AluOpcode("b110_0100".U)
    val xorzexth   = AluOpcode("b110_0101".U)
    val orcblsb    = AluOpcode("b110_0110".U)
    val orcbzexth  = AluOpcode("b110_0111".U)
    // for xstrap
    val xstrap     = AluOpcode("b111_1111".U)

    def isAddw(func: UInt) = func(6, 4) === "b001".U && !func(3) && !func(1)
    def isSimpleLogic(func: UInt) = func(6, 4) === "b100".U && !func(0)
    def logicToLsb(func: UInt) = Cat("b110".U(3.W), func(3, 1), 0.U(1.W))
    def logicToZexth(func: UInt) = Cat("b110".U(3.W), func(3, 1), 1.U(1.W))

    def isLui32add(func: UInt): Bool = func(6, 4) === "b001".U && !func(2) && func(1) && func(0) || func === lui32add
    def isOddadd(func: UInt): Bool = (func(6, 4) === "b001".U || func(6, 4) === "b010".U) && func(3, 0) === "b0001".U
    def isAdduw(func: UInt): Bool = func(6, 4) === "b010".U && !func(3, 0).orR
    def isSradd(func: UInt): Bool = func(6, 4) === "b010".U && !func(3) && func(2)
    def isSr29add(func: UInt): Bool = !func(1) && !func(0)
    def isSr30add(func: UInt): Bool = !func(1) &&  func(0)
    def isSr31add(func: UInt): Bool =  func(1) && !func(0)
    def isSr32add(func: UInt): Bool =  func(1) &&  func(0)
    def isShadd(func: UInt): Bool = func(6, 4) === "b010".U && func(3)
    def isSh1add(func: UInt): Bool = !func(2) && !func(1)
    def isSh2add(func: UInt): Bool = !func(2) &&  func(1)
    def isSh3add(func: UInt): Bool =  func(2) && !func(1)
    def isSh4add(func: UInt): Bool =  func(2) &&  func(1)
    def isZicond(func: UInt): Bool = func(6, 4).andR && !func(3)
    def isJmp(func: UInt): Bool = func(6, 3).andR & !func(2)

    def apply() = UInt(FuOpType.width.W)
  }

  object BruOpcodes {
    // branch
    val beq        = BruOpcode("b000_000".U)
    val bne        = BruOpcode("b000_001".U)
    val blt        = BruOpcode("b000_100".U)
    val bge        = BruOpcode("b000_101".U)
    val bltu       = BruOpcode("b001_000".U)
    val bgeu       = BruOpcode("b001_001".U)

    def getBranchType(func: UInt) = func(3, 1)
    def isBranchInvert(func: UInt) = func(0)
  }

  object JmpOpcodes {
    val jal        = JmpOpcode("b111_1000".U)
    val jalr       = JmpOpcode("b111_1001".U)
    val auipc      = JmpOpcode("b111_1010".U)

    def jumpOpisJalr(op: UInt) = op(0)
    def jumpOpisAuipc(op: UInt) = op(1)
  }

  object MulOpcodes {
    // mul
    // bit encoding: | type (2bit) | isWord(1bit) | opcode(2bit) |
    val mul    = MulOpcode("b00000".U)
    val mulh   = MulOpcode("b00001".U)
    val mulhsu = MulOpcode("b00010".U)
    val mulhu  = MulOpcode("b00011".U)
    val mulw   = MulOpcode("b00100".U)

    val mulw7  = MulOpcode("b01100".U)
    def isSign(op: UInt) = !op(1)
    def isW(op: UInt) = op(2)
    def isH(op: UInt) = op(1, 0) =/= 0.U
    def getOp(op: UInt) = Cat(op(3), op(1, 0))
  }

  object DivOpcodes {
    // div
    // bit encoding: | type (2bit) | isWord(1bit) | isSign(1bit) | opcode(1bit) |
    val div    = DivOpcode("b10000".U)
    val divu   = DivOpcode("b10010".U)
    val rem    = DivOpcode("b10001".U)
    val remu   = DivOpcode("b10011".U)

    val divw   = DivOpcode("b10100".U)
    val divuw  = DivOpcode("b10110".U)
    val remw   = DivOpcode("b10101".U)
    val remuw  = DivOpcode("b10111".U)

    def isSign(op: UInt) = !op(1)
    def isW(op: UInt) = op(2)
    def isH(op: UInt) = op(0)
  }

  trait LsuTrait {
    protected val sizeB = b"000"
    protected val sizeH = b"001"
    protected val sizeW = b"010"
    protected val sizeD = b"011"
    protected val sizeQ = b"100"

    protected val unsign = b"1"
    protected val sign = b"0"

    protected val isH = b"1"
    protected val nonH = b"0"

    // isX means that the uop needs execute permission.
    // E.g. hlvx.hu, hlvx.wu
    protected val isX = b"1"
    protected val nonX = b"0"

    def size(op: UInt): UInt = op(1, 0)
  }

  object LduOpcodes extends LsuTrait with DataType {
    protected val uopPrefetch = b"1"
    protected val uopLoad = b"0"

    // normal load
    val lb     = LduOpcode(nonH, nonX, sign  , sizeB, uopLoad)
    val lh     = LduOpcode(nonH, nonX, sign  , sizeH, uopLoad)
    val lw     = LduOpcode(nonH, nonX, sign  , sizeW, uopLoad)
    val ld     = LduOpcode(nonH, nonX, sign  , sizeD, uopLoad)
    val lq     = LduOpcode(nonH, nonX, sign  , sizeQ, uopLoad)
    val lbu    = LduOpcode(nonH, nonX, unsign, sizeB, uopLoad)
    val lhu    = LduOpcode(nonH, nonX, unsign, sizeH, uopLoad)
    val lwu    = LduOpcode(nonH, nonX, unsign, sizeW, uopLoad)
    // hypervior load
    val hlvb   = LduOpcode(isH, nonX, sign  , sizeB, uopLoad)
    val hlvh   = LduOpcode(isH, nonX, sign  , sizeH, uopLoad)
    val hlvw   = LduOpcode(isH, nonX, sign  , sizeW, uopLoad)
    val hlvd   = LduOpcode(isH, nonX, sign  , sizeD, uopLoad)
    val hlvbu  = LduOpcode(isH, nonX, unsign, sizeB, uopLoad)
    val hlvhu  = LduOpcode(isH, nonX, unsign, sizeH, uopLoad)
    val hlvwu  = LduOpcode(isH, nonX, unsign, sizeW, uopLoad)
    val hlvxhu = LduOpcode(isH, isX , unsign, sizeH, uopLoad)
    val hlvxwu = LduOpcode(isH, isX , unsign, sizeW, uopLoad)

    def isHlv(op: UInt): Bool = op(6) === isH && op(0) === uopLoad
    def isHlvx(op: UInt): Bool = op(6, 5) === isH ### isX && op(0) === uopLoad

    private val prefetchI = b"00"
    private val prefetchR = b"01"
    private val prefetchW = b"10"

    // Zicbop software prefetch
    val prefetch_i = LduOpcode(nonH, nonX, prefetchI, uopPrefetch)
    val prefetch_r = LduOpcode(nonH, nonX, prefetchR, uopPrefetch)
    val prefetch_w = LduOpcode(nonH, nonX, prefetchW, uopPrefetch)

    def getUopType(op: UInt): UInt = op(0)

    def isPrefetch(op: UInt): Bool = getUopType(op) === uopPrefetch
  }

  object StuOpcodes extends LsuTrait with DataType {
    protected val uopStore = b"0"
    protected val uopCbo = b"1"
    protected object CBO {
      val inval = b"000"
      val clean = b"001"
      val flush = b"010"
      val zero = b"100"
    }

    // store pipeline
    // normal store
    val sb = StuOpcode(nonH, nonX, sign, sizeB, uopStore)
    val sh = StuOpcode(nonH, nonX, sign, sizeH, uopStore)
    val sw = StuOpcode(nonH, nonX, sign, sizeW, uopStore)
    val sd = StuOpcode(nonH, nonX, sign, sizeD, uopStore)
    val sq = StuOpcode(nonH, nonX, sign, sizeQ, uopStore)

    //hypervisor store
    val hsvb = StuOpcode(isH, nonX, sign, sizeB, uopStore)
    val hsvh = StuOpcode(isH, nonX, sign, sizeH, uopStore)
    val hsvw = StuOpcode(isH, nonX, sign, sizeW, uopStore)
    val hsvd = StuOpcode(isH, nonX, sign, sizeD, uopStore)

    def isHsv(op: UInt): Bool = op(6) === isH && op(0) === uopStore

    // l1 cache op
    val cbo_zero  = StuOpcode(nonH, nonX, sign, CBO.zero , uopCbo)
    // llc op
    val cbo_clean = StuOpcode(nonH, nonX, sign, CBO.clean, uopCbo)
    val cbo_flush = StuOpcode(nonH, nonX, sign, CBO.flush, uopCbo)
    val cbo_inval = StuOpcode(nonH, nonX, sign, CBO.inval, uopCbo)

    def isCbo(op: UInt): Bool = op(0) === uopCbo && op(3, 1) === CBO.zero
    def isCboAll(op: UInt): Bool = op(0) === uopCbo
    def isCboClean(op: UInt): Bool = isCbo(op) && (op(3, 1) === CBO.clean)
    def isCboFlush(op: UInt): Bool = isCbo(op) && (op(3, 1) === CBO.flush)
    def isCboInval(op: UInt): Bool = isCbo(op) && (op(3, 1) === CBO.inval)
  }

  object AmoOpcodes extends LsuTrait {
    // atomics
    // bit(1, 0) are size
    // since atomics use a different fu type
    // so we can safely reuse other load/store's encodings
    // bit encoding: | optype(4bit) | size (2bit) |
    def AMOFuOpWidth = 7

    protected val noALU = b"0"
    protected val withALU = b"1"

    protected object NoALU {
      val lr   = b"000"
      val sc   = b"001"
      val swap = b"100"
      val cas  = b"101"
    }

    protected object WithALU {
      val add  = b"000"
      val xor  = b"001"
      val and  = b"011"
      val or   = b"010"
      val min  = b"100"
      val max  = b"101"
      val minu = b"110"
      val maxu = b"111"
    }

    //                        3b             3b     1b
    val amoswap_b = AmoOpcode(NoALU.swap   , sizeB, noALU)
    val amocas_b  = AmoOpcode(NoALU.cas    , sizeB, noALU)
    val amoadd_b  = AmoOpcode(WithALU.add  , sizeB, withALU)
    val amoxor_b  = AmoOpcode(WithALU.xor  , sizeB, withALU)
    val amoand_b  = AmoOpcode(WithALU.and  , sizeB, withALU)
    val amoor_b   = AmoOpcode(WithALU.or   , sizeB, withALU)
    val amomin_b  = AmoOpcode(WithALU.min  , sizeB, withALU)
    val amomax_b  = AmoOpcode(WithALU.max  , sizeB, withALU)
    val amominu_b = AmoOpcode(WithALU.minu , sizeB, withALU)
    val amomaxu_b = AmoOpcode(WithALU.maxu , sizeB, withALU)

    val amoswap_h = AmoOpcode(NoALU.swap   , sizeH, noALU)
    val amocas_h  = AmoOpcode(NoALU.cas    , sizeH, noALU)
    val amoadd_h  = AmoOpcode(WithALU.add  , sizeH, withALU)
    val amoxor_h  = AmoOpcode(WithALU.xor  , sizeH, withALU)
    val amoand_h  = AmoOpcode(WithALU.and  , sizeH, withALU)
    val amoor_h   = AmoOpcode(WithALU.or   , sizeH, withALU)
    val amomin_h  = AmoOpcode(WithALU.min  , sizeH, withALU)
    val amomax_h  = AmoOpcode(WithALU.max  , sizeH, withALU)
    val amominu_h = AmoOpcode(WithALU.minu , sizeH, withALU)
    val amomaxu_h = AmoOpcode(WithALU.maxu , sizeH, withALU)

    val lr_w      = AmoOpcode(NoALU.lr     , sizeW, noALU)
    val sc_w      = AmoOpcode(NoALU.sc     , sizeW, noALU)
    val amoswap_w = AmoOpcode(NoALU.swap   , sizeW, noALU)
    val amocas_w  = AmoOpcode(NoALU.cas    , sizeW, noALU)
    val amoadd_w  = AmoOpcode(WithALU.add  , sizeW, withALU)
    val amoxor_w  = AmoOpcode(WithALU.xor  , sizeW, withALU)
    val amoand_w  = AmoOpcode(WithALU.and  , sizeW, withALU)
    val amoor_w   = AmoOpcode(WithALU.or   , sizeW, withALU)
    val amomin_w  = AmoOpcode(WithALU.min  , sizeW, withALU)
    val amomax_w  = AmoOpcode(WithALU.max  , sizeW, withALU)
    val amominu_w = AmoOpcode(WithALU.minu , sizeW, withALU)
    val amomaxu_w = AmoOpcode(WithALU.maxu , sizeW, withALU)

    val lr_d      = AmoOpcode(NoALU.lr     , sizeD, noALU)
    val sc_d      = AmoOpcode(NoALU.sc     , sizeD, noALU)
    val amoswap_d = AmoOpcode(NoALU.swap   , sizeD, noALU)
    val amocas_d  = AmoOpcode(NoALU.cas    , sizeD, noALU)
    val amoadd_d  = AmoOpcode(WithALU.add  , sizeD, withALU)
    val amoxor_d  = AmoOpcode(WithALU.xor  , sizeD, withALU)
    val amoand_d  = AmoOpcode(WithALU.and  , sizeD, withALU)
    val amoor_d   = AmoOpcode(WithALU.or   , sizeD, withALU)
    val amomin_d  = AmoOpcode(WithALU.min  , sizeD, withALU)
    val amomax_d  = AmoOpcode(WithALU.max  , sizeD, withALU)
    val amominu_d = AmoOpcode(WithALU.minu , sizeD, withALU)
    val amomaxu_d = AmoOpcode(WithALU.maxu , sizeD, withALU)
    
    val amocas_q  = AmoOpcode(NoALU.cas , sizeQ, noALU)

    def isLr      (op: UInt): Bool = op(0) === noALU && op(6, 4) === NoALU.lr
    def isSc      (op: UInt): Bool = op(0) === noALU && op(6, 4) === NoALU.sc
    def isAMOCAS  (op: UInt): Bool = op(0) === noALU && op(6, 4) === NoALU.cas
    def isAMOCASQ (op: UInt): Bool = op(0) === isAMOCAS(op) && op(3, 1) === sizeQ
    def isAMOCASWD(op: UInt): Bool = op(0) === isAMOCAS(op) && (op(3, 1) === sizeW || op(3, 1) === sizeD)
  }

  object CsrOpcodes {
    //                 | func3|
    val jmp     = CsrOpcode("b010_000".U)
    val wfi     = CsrOpcode("b100_000".U)
    val wrs_nto = CsrOpcode("b100_010".U)
    val wrs_sto = CsrOpcode("b100_011".U)
    val wrt     = CsrOpcode("b001_001".U)
    val set     = CsrOpcode("b001_010".U)
    val clr     = CsrOpcode("b001_011".U)
    val wrti    = CsrOpcode("b001_101".U)
    val seti    = CsrOpcode("b001_110".U)
    val clri    = CsrOpcode("b001_111".U)

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

  object FenceOpcodes {
    val fence  = FenceOpcode("b10000".U)
    val sfence = FenceOpcode("b10001".U)
    val fencei = FenceOpcode("b10010".U)
    val hfence_v = FenceOpcode("b10011".U)
    val hfence_g = FenceOpcode("b10100".U)
    val nofence = FenceOpcode("b00000".U)
  }

  object BkuOpcodes {
    val clmul       = BkuOpcode("b000000".U)
    val clmulh      = BkuOpcode("b000001".U)
    val clmulr      = BkuOpcode("b000010".U)
    val xpermn      = BkuOpcode("b000100".U)
    val xpermb      = BkuOpcode("b000101".U)

    val clz         = BkuOpcode("b001000".U)
    val clzw        = BkuOpcode("b001001".U)
    val ctz         = BkuOpcode("b001010".U)
    val ctzw        = BkuOpcode("b001011".U)
    val cpop        = BkuOpcode("b001100".U)
    val cpopw       = BkuOpcode("b001101".U)

    // 01xxxx is reserve
    val aes64es     = BkuOpcode("b100000".U)
    val aes64esm    = BkuOpcode("b100001".U)
    val aes64ds     = BkuOpcode("b100010".U)
    val aes64dsm    = BkuOpcode("b100011".U)
    val aes64im     = BkuOpcode("b100100".U)
    val aes64ks1i   = BkuOpcode("b100101".U)
    val aes64ks2    = BkuOpcode("b100110".U)

    // merge to two instruction sm4ks & sm4ed
    val sm4ed0      = BkuOpcode("b101000".U)
    val sm4ed1      = BkuOpcode("b101001".U)
    val sm4ed2      = BkuOpcode("b101010".U)
    val sm4ed3      = BkuOpcode("b101011".U)
    val sm4ks0      = BkuOpcode("b101100".U)
    val sm4ks1      = BkuOpcode("b101101".U)
    val sm4ks2      = BkuOpcode("b101110".U)
    val sm4ks3      = BkuOpcode("b101111".U)

    val sha256sum0  = BkuOpcode("b110000".U)
    val sha256sum1  = BkuOpcode("b110001".U)
    val sha256sig0  = BkuOpcode("b110010".U)
    val sha256sig1  = BkuOpcode("b110011".U)
    val sha512sum0  = BkuOpcode("b110100".U)
    val sha512sum1  = BkuOpcode("b110101".U)
    val sha512sig0  = BkuOpcode("b110110".U)
    val sha512sig1  = BkuOpcode("b110111".U)

    val sm3p0       = BkuOpcode("b111000".U)
    val sm3p1       = BkuOpcode("b111001".U)
  }

  trait FCvtOpcodes extends DataType {
    private val F2F = b"00"
    private val F2I = b"01"
    private val I2F = b"10"
    private val OTHER = b"11"

    /**
     * Three sub opcode of [[F2F]]
     * [[cvt]], [[rnd]], [[rndnx]]
     */

    private val cvt = b"00"

    private val rnd = b"10"

    private val rndnx = b"11"

    /**
     * sub-opcode of [[F2I]]
     */

    private val F2S = b"00"
    private val F2U = b"01"
    private val F2SMOD = b"10"

    /**
     * sub-opcode of [[I2F]]
     */

    private val S2F = b"00"
    private val U2F = b"01"
    private val FMVI2F = b"11"

    /**
     * sub-opcode of [[OTHER]]
     */

    private val FCLASS  = b"0100"
    private val FREC7   = b"1000"
    private val FRSQRT7 = b"0101"
    private val FMVF2I  = b"0000"

    val fcvt_fp32_fp16: Opcode = FcvtOpcode(FP32, FP16, cvt, F2F, F)
    val fcvt_fp64_fp16: Opcode = FcvtOpcode(FP64, FP16, cvt, F2F, F)
    val fcvt_fp16_fp32: Opcode = FcvtOpcode(FP16, FP32, cvt, F2F, F)
    val fcvt_fp64_fp32: Opcode = FcvtOpcode(FP64, FP32, cvt, F2F, F)
    val fcvt_fp16_fp64: Opcode = FcvtOpcode(FP16, FP64, cvt, F2F, F)
    val fcvt_fp32_fp64: Opcode = FcvtOpcode(FP32, FP64, cvt, F2F, F)

    val frnd_fp16: Opcode = FcvtOpcode(FP16, FP16, rnd, F2F, F)
    val frnd_fp32: Opcode = FcvtOpcode(FP32, FP32, rnd, F2F, F)
    val frnd_fp64: Opcode = FcvtOpcode(FP64, FP64, rnd, F2F, F)

    val frndnx_fp16: Opcode = FcvtOpcode(FP16, FP16, F2F, F)
    val frndnx_fp32: Opcode = FcvtOpcode(FP32, FP32, F2F, F)
    val frndnx_fp64: Opcode = FcvtOpcode(FP64, FP64, F2F, F)

    // two narrow
    val vfcvt_fp16_fp32: Opcode = FcvtOpcode(FP16, FP32, cvt, F2F, V)
    val vfcvt_fp32_fp64: Opcode = FcvtOpcode(FP32, FP64, cvt, F2F, V)

    // two widen
    val vfcvt_fp32_fp16: Opcode = FcvtOpcode(FP32, FP16, cvt, F2F, V)
    val vfcvt_fp64_fp32: Opcode = FcvtOpcode(FP64, FP32, cvt, F2F, V)

    val fcvt_si32_fp16: Opcode = FcvtOpcode(I32, FP16, F2S, F2I, F)
    val fcvt_ui32_fp16: Opcode = FcvtOpcode(I32, FP16, F2U, F2I, F)
    val fcvt_si64_fp16: Opcode = FcvtOpcode(I64, FP16, F2S, F2I, F)
    val fcvt_ui64_fp16: Opcode = FcvtOpcode(I64, FP16, F2U, F2I, F)
    val fcvt_si32_fp32: Opcode = FcvtOpcode(I32, FP32, F2S, F2I, F)
    val fcvt_ui32_fp32: Opcode = FcvtOpcode(I32, FP32, F2U, F2I, F)
    val fcvt_si64_fp32: Opcode = FcvtOpcode(I64, FP32, F2S, F2I, F)
    val fcvt_ui64_fp32: Opcode = FcvtOpcode(I64, FP32, F2U, F2I, F)
    val fcvt_si32_fp64: Opcode = FcvtOpcode(I32, FP64, F2S, F2I, F)
    val fcvt_ui32_fp64: Opcode = FcvtOpcode(I32, FP64, F2U, F2I, F)
    val fcvt_si64_fp64: Opcode = FcvtOpcode(I64, FP64, F2S, F2I, F)
    val fcvt_ui64_fp64: Opcode = FcvtOpcode(I64, FP64, F2U, F2I, F)
    val fcvtmod_si32_fp64: Opcode = FcvtOpcode(I32, FP64, F2SMOD, F2I, F)

    val vfcvt_si8_fp16 : Opcode = FcvtOpcode(I8, FP16, F2S, F2I, V)
    val vfcvt_ui8_fp16 : Opcode = FcvtOpcode(I8, FP16, F2U, F2I, V)
    val vfcvt_si16_fp16: Opcode = FcvtOpcode(I16, FP16, F2S, F2I, V)
    val vfcvt_ui16_fp16: Opcode = FcvtOpcode(I16, FP16, F2U, F2I, V)
    val vfcvt_si32_fp16: Opcode = FcvtOpcode(I32, FP16, F2S, F2I, V)
    val vfcvt_ui32_fp16: Opcode = FcvtOpcode(I32, FP16, F2U, F2I, V)
    val vfcvt_si16_fp32: Opcode = FcvtOpcode(I16, FP32, F2S, F2I, V)
    val vfcvt_ui16_fp32: Opcode = FcvtOpcode(I16, FP32, F2U, F2I, V)
    val vfcvt_si32_fp32: Opcode = FcvtOpcode(I32, FP32, F2S, F2I, V)
    val vfcvt_ui32_fp32: Opcode = FcvtOpcode(I32, FP32, F2U, F2I, V)
    val vfcvt_si64_fp32: Opcode = FcvtOpcode(I64, FP32, F2S, F2I, V)
    val vfcvt_ui64_fp32: Opcode = FcvtOpcode(I64, FP32, F2U, F2I, V)
    val vfcvt_si32_fp64: Opcode = FcvtOpcode(I32, FP64, F2S, F2I, V)
    val vfcvt_ui32_fp64: Opcode = FcvtOpcode(I32, FP64, F2U, F2I, V)
    val vfcvt_si64_fp64: Opcode = FcvtOpcode(I64, FP64, F2S, F2I, V)
    val vfcvt_ui64_fp64: Opcode = FcvtOpcode(I64, FP64, F2U, F2I, V)

    val fcvt_fp16_si32: Opcode = FcvtOpcode(FP16, I32,    S2F, I2F, F)
    val fcvt_fp16_ui32: Opcode = FcvtOpcode(FP16, I32,    U2F, I2F, F)
    val fcvt_fp16_si64: Opcode = FcvtOpcode(FP16, I64,    S2F, I2F, F)
    val fcvt_fp16_ui64: Opcode = FcvtOpcode(FP16, I64,    U2F, I2F, F)
    val fcvt_fp32_si32: Opcode = FcvtOpcode(FP32, I32,    S2F, I2F, F)
    val fcvt_fp32_ui32: Opcode = FcvtOpcode(FP32, I32,    U2F, I2F, F)
    val fcvt_fp32_si64: Opcode = FcvtOpcode(FP32, I64,    S2F, I2F, F)
    val fcvt_fp32_ui64: Opcode = FcvtOpcode(FP32, I64,    U2F, I2F, F)
    val fcvt_fp64_si32: Opcode = FcvtOpcode(FP64, I32,    S2F, I2F, F)
    val fcvt_fp64_ui32: Opcode = FcvtOpcode(FP64, I32,    U2F, I2F, F)
    val fcvt_fp64_si64: Opcode = FcvtOpcode(FP64, I64,    S2F, I2F, F)
    val fcvt_fp64_ui64: Opcode = FcvtOpcode(FP64, I64,    U2F, I2F, F)
    val fmv_fp16_i    : Opcode = FcvtOpcode(FP16, I64, FMVI2F, I2F, F)
    val fmv_fp32_i    : Opcode = FcvtOpcode(FP32, I64, FMVI2F, I2F, F)
    val fmv_fp64_i    : Opcode = FcvtOpcode(FP64, I64, FMVI2F, I2F, F)

    val vfcvt_fp16_si32: Opcode = FcvtOpcode(FP16, I32, S2F, I2F, V)
    val vfcvt_fp16_ui32: Opcode = FcvtOpcode(FP16, I32, U2F, I2F, V)
    val vfcvt_fp16_si64: Opcode = FcvtOpcode(FP16, I64, S2F, I2F, V)
    val vfcvt_fp16_ui64: Opcode = FcvtOpcode(FP16, I64, U2F, I2F, V)
    val vfcvt_fp32_si32: Opcode = FcvtOpcode(FP32, I32, S2F, I2F, V)
    val vfcvt_fp32_ui32: Opcode = FcvtOpcode(FP32, I32, U2F, I2F, V)
    val vfcvt_fp32_si64: Opcode = FcvtOpcode(FP32, I64, S2F, I2F, V)
    val vfcvt_fp32_ui64: Opcode = FcvtOpcode(FP32, I64, U2F, I2F, V)
    val vfcvt_fp64_si32: Opcode = FcvtOpcode(FP64, I32, S2F, I2F, V)
    val vfcvt_fp64_ui32: Opcode = FcvtOpcode(FP64, I32, U2F, I2F, V)
    val vfcvt_fp64_si64: Opcode = FcvtOpcode(FP64, I64, S2F, I2F, V)
    val vfcvt_fp64_ui64: Opcode = FcvtOpcode(FP64, I64, U2F, I2F, V)

    val fclass_fp16: Opcode = FcvtOpcode(FP16, FCLASS, OTHER, F)
    val fclass_fp32: Opcode = FcvtOpcode(FP32, FCLASS, OTHER, F)
    val fclass_fp64: Opcode = FcvtOpcode(FP64, FCLASS, OTHER, F)
    val fmv_i_fp16 : Opcode = FcvtOpcode(FP16, FMVF2I, OTHER, F)
    val fmv_i_fp32 : Opcode = FcvtOpcode(FP32, FMVF2I, OTHER, F)
    val fmv_i_fp64 : Opcode = FcvtOpcode(FP64, FMVF2I, OTHER, F)

    val vflass_fp16  : Opcode = FcvtOpcode(FP16, FCLASS, OTHER, V)
    val vflass_fp32  : Opcode = FcvtOpcode(FP32, FCLASS, OTHER, V)
    val vflass_fp64  : Opcode = FcvtOpcode(FP64, FCLASS, OTHER, V)
    val vfrec7_fp16  : Opcode = FcvtOpcode(FP16, FREC7, OTHER, V)
    val vfrec7_fp32  : Opcode = FcvtOpcode(FP32, FREC7, OTHER, V)
    val vfrec7_fp64  : Opcode = FcvtOpcode(FP64, FREC7, OTHER, V)
    val vfrsqrt7_fp16: Opcode = FcvtOpcode(FP16, FRSQRT7, OTHER, V)
    val vfrsqrt7_fp32: Opcode = FcvtOpcode(FP32, FRSQRT7, OTHER, V)
    val vfrsqrt7_fp64: Opcode = FcvtOpcode(FP64, FRSQRT7, OTHER, V)
  }

  object FCvtOpcodes extends FCvtOpcodes

  trait FMacOpcodes extends DataType {
    private val OP2 = b"0"
    private val OP3 = b"1"

    private val DV = b"0"
    private val DW = b"1"
    private val S2V = b"0"
    private val S2W = b"1"
    private val NOTADD = b"0"
    private val USEADD = b"1"
    private val NOTMUL = b"0"
    private val USEMUL = b"1"

    // bit(2): 0 -> vs1 * vd, 1 -> vs1 * vs2
    // bit(1): 0 -> add     , 1 -> sub
    // bit(0): 0 -> pos     , 1 -> neg
    private val FMADD  = b"000" // +((vs1[i] * vd[i]) + vs2[i])
    private val FNMADD = b"001" // -((vs1[i] * vd[i]) + vs2[i])
    private val FMSUB  = b"010" // +((vs1[i] * vd[i]) - vs2[i])
    private val FNMSUB = b"011" // -((vs1[i] * vd[i]) - vs2[i])
    private val FMACC  = b"100" // +((vs1[i] * vs2[i]) + vd[i])
    private val FNMACC = b"101" // -((vs1[i] * vs2[i]) + vd[i])
    private val FMSAC  = b"110" // +((vs1[i] * vs2[i]) - vd[i])
    private val FNMSAC = b"111" // -((vs1[i] * vs2[i]) - vd[i])

    private val FADD = b"000"
    private val FSUB = b"001"

    private val FMUL = b"100"

    val fmadd_fp16   : Opcode = FmacOpcode(FMADD , OP3, S2V, DV, FP16, F)
    val fmsub_fp16   : Opcode = FmacOpcode(FMSUB , OP3, S2V, DV, FP16, F)
    val fnmsub_fp16  : Opcode = FmacOpcode(FNMSUB, OP3, S2V, DV, FP16, F)
    val fnmadd_fp16  : Opcode = FmacOpcode(FNMADD, OP3, S2V, DV, FP16, F)
    val fadd_fp16    : Opcode = FmacOpcode(FADD  , OP2, S2V, DV, FP16, F)
    val fsub_fp16    : Opcode = FmacOpcode(FSUB  , OP2, S2V, DV, FP16, F)
    val fmul_fp16    : Opcode = FmacOpcode(FMUL  , OP2, S2V, DV, FP16, F)
    val vfadd_fp16   : Opcode = FmacOpcode(FADD  , OP2, S2V, DV, FP16, V)
    val vfsub_fp16   : Opcode = FmacOpcode(FSUB  , OP2, S2V, DV, FP16, V)
    val vfmul_fp16   : Opcode = FmacOpcode(FMUL  , OP2, S2V, DV, FP16, V)
    val vfmadd_fp16  : Opcode = FmacOpcode(FMADD , OP3, S2V, DV, FP16, V)
    val vfnmadd_fp16 : Opcode = FmacOpcode(FNMADD, OP3, S2V, DV, FP16, V)
    val vfmsub_fp16  : Opcode = FmacOpcode(FMSUB , OP3, S2V, DV, FP16, V)
    val vfnmsub_fp16 : Opcode = FmacOpcode(FNMSUB, OP3, S2V, DV, FP16, V)
    val vfmacc_fp16  : Opcode = FmacOpcode(FMACC , OP3, S2V, DV, FP16, V)
    val vfnmacc_fp16 : Opcode = FmacOpcode(FNMACC, OP3, S2V, DV, FP16, V)
    val vfmsac_fp16  : Opcode = FmacOpcode(FMSAC , OP3, S2V, DV, FP16, V)
    val vfnmsac_fp16 : Opcode = FmacOpcode(FNMSAC, OP3, S2V, DV, FP16, V)
    val vfwadd_fp16  : Opcode = FmacOpcode(FADD  , OP2, S2V, DW, FP16, V)
    val vfwsub_fp16  : Opcode = FmacOpcode(FSUB  , OP2, S2V, DW, FP16, V)
    val vfwadd_w_fp16: Opcode = FmacOpcode(FADD  , OP2, S2W, DW, FP16, V)
    val vfwsub_w_fp16: Opcode = FmacOpcode(FSUB  , OP2, S2W, DW, FP16, V)
    val vfwmul_fp16  : Opcode = FmacOpcode(FMUL  , OP2, S2V, DW, FP16, V)
    val vfwmacc_fp16 : Opcode = FmacOpcode(FMACC , OP3, S2V, DW, FP16, V)
    val vfwnmacc_fp16: Opcode = FmacOpcode(FNMACC, OP3, S2V, DW, FP16, V)
    val vfwmsac_fp16 : Opcode = FmacOpcode(FMSAC , OP3, S2V, DW, FP16, V)
    val vfwnmsac_fp16: Opcode = FmacOpcode(FNMSAC, OP3, S2V, DW, FP16, V)
    val fmadd_fp32   : Opcode = FmacOpcode(FMADD , OP3, S2V, DV, FP32, F)
    val fmsub_fp32   : Opcode = FmacOpcode(FMSUB , OP3, S2V, DV, FP32, F)
    val fnmsub_fp32  : Opcode = FmacOpcode(FNMSUB, OP3, S2V, DV, FP32, F)
    val fnmadd_fp32  : Opcode = FmacOpcode(FNMADD, OP3, S2V, DV, FP32, F)
    val fadd_fp32    : Opcode = FmacOpcode(FADD  , OP2, S2V, DV, FP32, F)
    val fsub_fp32    : Opcode = FmacOpcode(FSUB  , OP2, S2V, DV, FP32, F)
    val fmul_fp32    : Opcode = FmacOpcode(FMUL  , OP2, S2V, DV, FP32, F)
    val vfadd_fp32   : Opcode = FmacOpcode(FADD  , OP2, S2V, DV, FP32, V)
    val vfsub_fp32   : Opcode = FmacOpcode(FSUB  , OP2, S2V, DV, FP32, V)
    val vfmul_fp32   : Opcode = FmacOpcode(FMUL  , OP2, S2V, DV, FP32, V)
    val vfmadd_fp32  : Opcode = FmacOpcode(FMADD , OP3, S2V, DV, FP32, V)
    val vfnmadd_fp32 : Opcode = FmacOpcode(FNMADD, OP3, S2V, DV, FP32, V)
    val vfmsub_fp32  : Opcode = FmacOpcode(FMSUB , OP3, S2V, DV, FP32, V)
    val vfnmsub_fp32 : Opcode = FmacOpcode(FNMSUB, OP3, S2V, DV, FP32, V)
    val vfmacc_fp32  : Opcode = FmacOpcode(FMACC , OP3, S2V, DV, FP32, V)
    val vfnmacc_fp32 : Opcode = FmacOpcode(FNMACC, OP3, S2V, DV, FP32, V)
    val vfmsac_fp32  : Opcode = FmacOpcode(FMSAC , OP3, S2V, DV, FP32, V)
    val vfnmsac_fp32 : Opcode = FmacOpcode(FNMSAC, OP3, S2V, DV, FP32, V)
    val vfwadd_fp32  : Opcode = FmacOpcode(FADD  , OP2, S2V, DW, FP32, V)
    val vfwsub_fp32  : Opcode = FmacOpcode(FSUB  , OP2, S2V, DW, FP32, V)
    val vfwadd_w_fp32: Opcode = FmacOpcode(FADD  , OP2, S2W, DW, FP32, V)
    val vfwsub_w_fp32: Opcode = FmacOpcode(FSUB  , OP2, S2W, DW, FP32, V)
    val vfwmul_fp32  : Opcode = FmacOpcode(FMUL  , OP2, S2V, DW, FP32, V)
    val vfwmacc_fp32 : Opcode = FmacOpcode(FMACC , OP3, S2V, DW, FP32, V)
    val vfwnmacc_fp32: Opcode = FmacOpcode(FNMACC, OP3, S2V, DW, FP32, V)
    val vfwmsac_fp32 : Opcode = FmacOpcode(FMSAC , OP3, S2V, DW, FP32, V)
    val vfwnmsac_fp32: Opcode = FmacOpcode(FNMSAC, OP3, S2V, DW, FP32, V)
    val fmadd_fp64   : Opcode = FmacOpcode(FMADD , OP3, S2V, DV, FP64, F)
    val fmsub_fp64   : Opcode = FmacOpcode(FMSUB , OP3, S2V, DV, FP64, F)
    val fnmsub_fp64  : Opcode = FmacOpcode(FNMSUB, OP3, S2V, DV, FP64, F)
    val fnmadd_fp64  : Opcode = FmacOpcode(FNMADD, OP3, S2V, DV, FP64, F)
    val fadd_fp64    : Opcode = FmacOpcode(FADD  , OP2, S2V, DV, FP64, F)
    val fsub_fp64    : Opcode = FmacOpcode(FSUB  , OP2, S2V, DV, FP64, F)
    val fmul_fp64    : Opcode = FmacOpcode(FMUL  , OP2, S2V, DV, FP64, F)
    val vfadd_fp64   : Opcode = FmacOpcode(FADD  , OP2, S2V, DV, FP64, V)
    val vfsub_fp64   : Opcode = FmacOpcode(FSUB  , OP2, S2V, DV, FP64, V)
    val vfmul_fp64   : Opcode = FmacOpcode(FMUL  , OP2, S2V, DV, FP64, V)
    val vfmadd_fp64  : Opcode = FmacOpcode(FMADD , OP3, S2V, DV, FP64, V)
    val vfnmadd_fp64 : Opcode = FmacOpcode(FNMADD, OP3, S2V, DV, FP64, V)
    val vfmsub_fp64  : Opcode = FmacOpcode(FMSUB , OP3, S2V, DV, FP64, V)
    val vfnmsub_fp64 : Opcode = FmacOpcode(FNMSUB, OP3, S2V, DV, FP64, V)
    val vfmacc_fp64  : Opcode = FmacOpcode(FMACC , OP3, S2V, DV, FP64, V)
    val vfnmacc_fp64 : Opcode = FmacOpcode(FNMACC, OP3, S2V, DV, FP64, V)
    val vfmsac_fp64  : Opcode = FmacOpcode(FMSAC , OP3, S2V, DV, FP64, V)
    val vfnmsac_fp64 : Opcode = FmacOpcode(FNMSAC, OP3, S2V, DV, FP64, V)
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

  trait FDivOpcodes extends DataType {
    private val FDIV  = b"0"
    private val FSQRT = b"1"

    val fdiv_fp16 : Opcode = FdivOpcode(FDIV , FP16, F)
    val fsqrt_fp16: Opcode = FdivOpcode(FSQRT, FP16, F)
    val fdiv_fp32 : Opcode = FdivOpcode(FDIV , FP32, F)
    val fsqrt_fp32: Opcode = FdivOpcode(FSQRT, FP32, F)
    val fdiv_fp64 : Opcode = FdivOpcode(FDIV , FP64, F)
    val fsqrt_fp64: Opcode = FdivOpcode(FSQRT, FP64, F)

    val vfdiv_fp16 : Opcode = FdivOpcode(FDIV , FP16, V)
    val vfsqrt_fp16: Opcode = FdivOpcode(FSQRT, FP16, V)
    val vfdiv_fp32 : Opcode = FdivOpcode(FDIV , FP32, V)
    val vfsqrt_fp32: Opcode = FdivOpcode(FSQRT, FP32, V)
    val vfdiv_fp64 : Opcode = FdivOpcode(FDIV , FP64, V)
    val vfsqrt_fp64: Opcode = FdivOpcode(FSQRT, FP64, V)
  }

  object FDivOpcodes extends FDivOpcodes

  trait FMiscOpcodes extends DataType {
    private val FSGNJ  = b"0000"
    private val FSGNJN = b"0001"
    private val FSGNJX = b"0010"
    private val FMIN   = b"0100"
    private val FMAX   = b"0101"
    private val FMINM  = b"0110"
    private val FMAXM  = b"0111"

    private val FEQ  = b"0000"
    private val FLE  = b"0010"
    private val FLT  = b"0110"
    private val FNE  = b"1000"
    private val FGT  = b"1010"
    private val FGE  = b"1110"
    private val FLEQ = b"0011"
    private val FLTQ = b"0111"

    private val DM = b"1"
    private val DV = b"0"

    val feq_fp16    : Opcode = FmiscOpcode(FEQ   , DM, FP16, F)
    val fle_fp16    : Opcode = FmiscOpcode(FLE   , DM, FP16, F)
    val flt_fp16    : Opcode = FmiscOpcode(FLT   , DM, FP16, F)
    val fleq_fp16   : Opcode = FmiscOpcode(FLEQ  , DM, FP16, F)
    val fltq_fp16   : Opcode = FmiscOpcode(FLTQ  , DM, FP16, F)
    val fmin_fp16   : Opcode = FmiscOpcode(FMIN  , DV, FP16, F)
    val fmax_fp16   : Opcode = FmiscOpcode(FMAX  , DV, FP16, F)
    val fminm_fp16  : Opcode = FmiscOpcode(FMINM , DV, FP16, F)
    val fmaxm_fp16  : Opcode = FmiscOpcode(FMAXM , DV, FP16, F)
    val fsgnj_fp16  : Opcode = FmiscOpcode(FSGNJ , DV, FP16, F)
    val fsgnjn_fp16 : Opcode = FmiscOpcode(FSGNJN, DV, FP16, F)
    val fsgnjx_fp16 : Opcode = FmiscOpcode(FSGNJX, DV, FP16, F)
    val vmfeq_fp16  : Opcode = FmiscOpcode(FEQ   , DM, FP16, V)
    val vmfle_fp16  : Opcode = FmiscOpcode(FLE   , DM, FP16, V)
    val vmflt_fp16  : Opcode = FmiscOpcode(FLT   , DM, FP16, V)
    val vmfne_fp16  : Opcode = FmiscOpcode(FNE   , DM, FP16, V)
    val vmfgt_fp16  : Opcode = FmiscOpcode(FGT   , DM, FP16, V)
    val vmfge_fp16  : Opcode = FmiscOpcode(FGE   , DM, FP16, V)
    val vfmin_fp16  : Opcode = FmiscOpcode(FMIN  , DV, FP16, V)
    val vfmax_fp16  : Opcode = FmiscOpcode(FMAX  , DV, FP16, V)
    val vfsgnj_fp16 : Opcode = FmiscOpcode(FSGNJ , DV, FP16, V)
    val vfsgnjn_fp16: Opcode = FmiscOpcode(FSGNJN, DV, FP16, V)
    val vfsgnjx_fp16: Opcode = FmiscOpcode(FSGNJX, DV, FP16, V)
    val feq_fp32    : Opcode = FmiscOpcode(FEQ   , DM, FP32, F)
    val fle_fp32    : Opcode = FmiscOpcode(FLE   , DM, FP32, F)
    val flt_fp32    : Opcode = FmiscOpcode(FLT   , DM, FP32, F)
    val fleq_fp32   : Opcode = FmiscOpcode(FLEQ  , DM, FP32, F)
    val fltq_fp32   : Opcode = FmiscOpcode(FLTQ  , DM, FP32, F)
    val fmin_fp32   : Opcode = FmiscOpcode(FMIN  , DV, FP32, F)
    val fmax_fp32   : Opcode = FmiscOpcode(FMAX  , DV, FP32, F)
    val fminm_fp32  : Opcode = FmiscOpcode(FMINM , DV, FP32, F)
    val fmaxm_fp32  : Opcode = FmiscOpcode(FMAXM , DV, FP32, F)
    val fsgnj_fp32  : Opcode = FmiscOpcode(FSGNJ , DV, FP32, F)
    val fsgnjn_fp32 : Opcode = FmiscOpcode(FSGNJN, DV, FP32, F)
    val fsgnjx_fp32 : Opcode = FmiscOpcode(FSGNJX, DV, FP32, F)
    val vmfeq_fp32  : Opcode = FmiscOpcode(FEQ   , DM, FP32, V)
    val vmfle_fp32  : Opcode = FmiscOpcode(FLE   , DM, FP32, V)
    val vmflt_fp32  : Opcode = FmiscOpcode(FLT   , DM, FP32, V)
    val vmfne_fp32  : Opcode = FmiscOpcode(FNE   , DM, FP32, V)
    val vmfgt_fp32  : Opcode = FmiscOpcode(FGT   , DM, FP32, V)
    val vmfge_fp32  : Opcode = FmiscOpcode(FGE   , DM, FP32, V)
    val vfmin_fp32  : Opcode = FmiscOpcode(FMIN  , DV, FP32, V)
    val vfmax_fp32  : Opcode = FmiscOpcode(FMAX  , DV, FP32, V)
    val vfsgnj_fp32 : Opcode = FmiscOpcode(FSGNJ , DV, FP32, V)
    val vfsgnjn_fp32: Opcode = FmiscOpcode(FSGNJN, DV, FP32, V)
    val vfsgnjx_fp32: Opcode = FmiscOpcode(FSGNJX, DV, FP32, V)
    val feq_fp64    : Opcode = FmiscOpcode(FEQ   , DM, FP64, F)
    val fle_fp64    : Opcode = FmiscOpcode(FLE   , DM, FP64, F)
    val flt_fp64    : Opcode = FmiscOpcode(FLT   , DM, FP64, F)
    val fleq_fp64   : Opcode = FmiscOpcode(FLEQ  , DM, FP64, F)
    val fltq_fp64   : Opcode = FmiscOpcode(FLTQ  , DM, FP64, F)
    val fmin_fp64   : Opcode = FmiscOpcode(FMIN  , DV, FP64, F)
    val fmax_fp64   : Opcode = FmiscOpcode(FMAX  , DV, FP64, F)
    val fminm_fp64  : Opcode = FmiscOpcode(FMINM , DV, FP64, F)
    val fmaxm_fp64  : Opcode = FmiscOpcode(FMAXM , DV, FP64, F)
    val fsgnj_fp64  : Opcode = FmiscOpcode(FSGNJ , DV, FP64, F)
    val fsgnjn_fp64 : Opcode = FmiscOpcode(FSGNJN, DV, FP64, F)
    val fsgnjx_fp64 : Opcode = FmiscOpcode(FSGNJX, DV, FP64, F)
    val vmfeq_fp64  : Opcode = FmiscOpcode(FEQ   , DM, FP64, V)
    val vmfle_fp64  : Opcode = FmiscOpcode(FLE   , DM, FP64, V)
    val vmflt_fp64  : Opcode = FmiscOpcode(FLT   , DM, FP64, V)
    val vmfne_fp64  : Opcode = FmiscOpcode(FNE   , DM, FP64, V)
    val vmfgt_fp64  : Opcode = FmiscOpcode(FGT   , DM, FP64, V)
    val vmfge_fp64  : Opcode = FmiscOpcode(FGE   , DM, FP64, V)
    val vfmin_fp64  : Opcode = FmiscOpcode(FMIN  , DV, FP64, V)
    val vfmax_fp64  : Opcode = FmiscOpcode(FMAX  , DV, FP64, V)
    val vfsgnj_fp64 : Opcode = FmiscOpcode(FSGNJ , DV, FP64, V)
    val vfsgnjn_fp64: Opcode = FmiscOpcode(FSGNJN, DV, FP64, V)
    val vfsgnjx_fp64: Opcode = FmiscOpcode(FSGNJX, DV, FP64, V)
  }

  object FMiscOpcodes extends FMiscOpcodes

  implicit def caseToUInt(opcode: Opcode): UInt = opcode.encode

  val ALUOpType: AluOpcodes.type = AluOpcodes
  val BRUOpType: BruOpcodes.type = BruOpcodes
  val MULOpType: MulOpcodes.type = MulOpcodes
  val DIVOpType: DivOpcodes.type = DivOpcodes
//  val LSUOpType: LsuOpcodes.type = LsuOpcodes
  val BKUOpType: BkuOpcodes.type = BkuOpcodes
}