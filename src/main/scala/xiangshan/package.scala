/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tile.XLen
import xiangshan.ExceptionNO._
import xiangshan.backend.issue._
import xiangshan.backend.fu._
import xiangshan.backend.fu.fpu._
import xiangshan.backend.fu.vector._
import xiangshan.backend.exu._
import xiangshan.backend.{Std, ScheLaneConfig}

package object xiangshan {
  object SrcType {
    def imm = "b000".U
    def pc  = "b000".U
    def xp  = "b001".U
    def fp  = "b010".U
    def vp  = "b100".U

    // alias
    def reg = this.xp
    def DC  = imm // Don't Care
    def X   = BitPat("b000")

    def isPc(srcType: UInt) = srcType===pc
    def isImm(srcType: UInt) = srcType===imm
    def isReg(srcType: UInt) = srcType(0)
    def isFp(srcType: UInt) = srcType(1)
    def isVp(srcType: UInt) = srcType(2)
    def isPcOrImm(srcType: UInt) = isPc(srcType) || isImm(srcType)

    def isNull(srcType: UInt) = !(isPcOrImm(srcType) || isReg(srcType) ||
      isFp(srcType) || isVp(srcType))

    def apply() = UInt(3.W)
  }

  object SrcState {
    def busy    = "b0".U
    def rdy     = "b1".U
    // def specRdy = "b10".U // speculative ready, for future use
    def apply() = UInt(1.W)
  }

  // Todo: Use OH instead
  object FuType {
    def jmp          = "b00000".U
    def i2f          = "b00001".U
    def csr          = "b00010".U
    def alu          = "b00110".U
    def mul          = "b00100".U
    def div          = "b00101".U
    def fence        = "b00011".U
    def bku          = "b00111".U

    def fmac         = "b01000".U
    def fmisc        = "b01011".U
    def fDivSqrt     = "b01010".U

    def ldu          = "b01100".U
    def stu          = "b01101".U
    def mou          = "b01111".U // for amo, lr, sc, fence

    def vipu         = "b10000".U
    def vialuF       = "b10001".U // for VIALU Fixed-Point instructions
    def vfpu         = "b11000".U
    def vldu         = "b11100".U
    def vstu         = "b11101".U
    def vppu         = "b11001".U // for Permutation Unit
    def X            = BitPat("b00000") // TODO: It may be a potential bug

    def num = 19

    def apply() = UInt(log2Up(num).W)

    // TODO: Optimize FuTpye and its method
    // FIXME: Vector FuType coding is not ready
    def isVecExu(fuType: UInt) = fuType(4)
    def isIntExu(fuType: UInt) = !isVecExu(fuType) && !fuType(3)
    def isJumpExu(fuType: UInt) = fuType === jmp
    def isFpExu(fuType: UInt) = !isVecExu(fuType) && (fuType(3, 2) === "b10".U)
    def isMemExu(fuType: UInt) = !isVecExu(fuType) && (fuType(3, 2) === "b11".U)
    def isLoadStore(fuType: UInt) = isMemExu(fuType) && !fuType(1)
    def isStoreExu(fuType: UInt) = isMemExu(fuType) && fuType(0)
    def isAMO(fuType: UInt) = fuType(1)
    def isFence(fuType: UInt) = fuType === fence
    def isSvinvalBegin(fuType: UInt, func: UInt, flush: Bool) = isFence(fuType) && func === FenceOpType.nofence && !flush
    def isSvinval(fuType: UInt, func: UInt, flush: Bool) = isFence(fuType) && func === FenceOpType.sfence && !flush
    def isSvinvalEnd(fuType: UInt, func: UInt, flush: Bool) = isFence(fuType) && func === FenceOpType.nofence && flush

    def jmpCanAccept(fuType: UInt) = !fuType(2)
    def mduCanAccept(fuType: UInt) = fuType(2) && !fuType(1) || fuType(2) && fuType(1) && fuType(0)
    def aluCanAccept(fuType: UInt) = fuType(2) && fuType(1) && !fuType(0)

    def fmacCanAccept(fuType: UInt) = !fuType(1)
    def fmiscCanAccept(fuType: UInt) = fuType(1)

    def loadCanAccept(fuType: UInt) = !fuType(0)
    def storeCanAccept(fuType: UInt) = fuType(0)

    def storeIsAMO(fuType: UInt) = fuType(1)

    val functionNameMap = Map(
      jmp.litValue() -> "jmp",
      i2f.litValue() -> "int_to_float",
      csr.litValue() -> "csr",
      alu.litValue() -> "alu",
      mul.litValue() -> "mul",
      div.litValue() -> "div",
      fence.litValue() -> "fence",
      bku.litValue() -> "bku",
      fmac.litValue() -> "fmac",
      fmisc.litValue() -> "fmisc",
      fDivSqrt.litValue() -> "fdiv_fsqrt",
      ldu.litValue() -> "load",
      stu.litValue() -> "store",
      mou.litValue() -> "mou"
    )
  }

  def FuOpTypeWidth = 8
  object FuOpType {
    def apply() = UInt(FuOpTypeWidth.W)
    def X = BitPat("b00000000")
  }

  // move VipuType and VfpuType into YunSuan/package.scala
  // object VipuType {
  //   def dummy = 0.U(7.W)
  // }

  // object VfpuType {
  //   def dummy = 0.U(7.W)
  // }

  object VlduType {
    def dummy = 0.U
  }

  object VstuType {
    def dummy = 0.U
  }

  object CommitType {
    def NORMAL = "b000".U  // int/fp
    def BRANCH = "b001".U  // branch
    def LOAD   = "b010".U  // load
    def STORE  = "b011".U  // store

    def apply() = UInt(3.W)
    def isFused(commitType: UInt): Bool = commitType(2)
    def isLoadStore(commitType: UInt): Bool = !isFused(commitType) && commitType(1)
    def lsInstIsStore(commitType: UInt): Bool = commitType(0)
    def isStore(commitType: UInt): Bool = isLoadStore(commitType) && lsInstIsStore(commitType)
    def isBranch(commitType: UInt): Bool = commitType(0) && !commitType(1) && !isFused(commitType)
  }

  object RedirectLevel {
    def flushAfter = "b0".U
    def flush      = "b1".U

    def apply() = UInt(1.W)
    // def isUnconditional(level: UInt) = level(1)
    def flushItself(level: UInt) = level(0)
    // def isException(level: UInt) = level(1) && level(0)
  }

  object ExceptionVec {
    def apply() = Vec(16, Bool())
  }

  object PMAMode {
    def R = "b1".U << 0 //readable
    def W = "b1".U << 1 //writeable
    def X = "b1".U << 2 //executable
    def I = "b1".U << 3 //cacheable: icache
    def D = "b1".U << 4 //cacheable: dcache
    def S = "b1".U << 5 //enable speculative access
    def A = "b1".U << 6 //enable atomic operation, A imply R & W
    def C = "b1".U << 7 //if it is cacheable is configable
    def Reserved = "b0".U

    def apply() = UInt(7.W)

    def read(mode: UInt) = mode(0)
    def write(mode: UInt) = mode(1)
    def execute(mode: UInt) = mode(2)
    def icache(mode: UInt) = mode(3)
    def dcache(mode: UInt) = mode(4)
    def speculate(mode: UInt) = mode(5)
    def atomic(mode: UInt) = mode(6)
    def configable_cache(mode: UInt) = mode(7)

    def strToMode(s: String) = {
      var result = 0.U(8.W)
      if (s.toUpperCase.indexOf("R") >= 0) result = result + R
      if (s.toUpperCase.indexOf("W") >= 0) result = result + W
      if (s.toUpperCase.indexOf("X") >= 0) result = result + X
      if (s.toUpperCase.indexOf("I") >= 0) result = result + I
      if (s.toUpperCase.indexOf("D") >= 0) result = result + D
      if (s.toUpperCase.indexOf("S") >= 0) result = result + S
      if (s.toUpperCase.indexOf("A") >= 0) result = result + A
      if (s.toUpperCase.indexOf("C") >= 0) result = result + C
      result
    }
  }


  object CSROpType {
    def jmp  = "b000".U
    def wrt  = "b001".U
    def set  = "b010".U
    def clr  = "b011".U
    def wfi  = "b100".U
    def wrti = "b101".U
    def seti = "b110".U
    def clri = "b111".U
    def needAccess(op: UInt): Bool = op(1, 0) =/= 0.U
  }

  // jump
  object JumpOpType {
    def jal  = "b00".U
    def jalr = "b01".U
    def auipc = "b10".U
//    def call = "b11_011".U
//    def ret  = "b11_100".U
    def jumpOpisJalr(op: UInt) = op(0)
    def jumpOpisAuipc(op: UInt) = op(1)
  }

  object FenceOpType {
    def fence  = "b10000".U
    def sfence = "b10001".U
    def fencei = "b10010".U
    def nofence= "b00000".U
  }

  object ALUOpType {
    // shift optype
    def slliuw     = "b000_0000".U // slliuw: ZEXT(src1[31:0]) << shamt
    def sll        = "b000_0001".U // sll:     src1 << src2

    def bclr       = "b000_0010".U // bclr:    src1 & ~(1 << src2[5:0])
    def bset       = "b000_0011".U // bset:    src1 | (1 << src2[5:0])
    def binv       = "b000_0100".U // binv:    src1 ^ ~(1 << src2[5:0])

    def srl        = "b000_0101".U // srl:     src1 >> src2
    def bext       = "b000_0110".U // bext:    (src1 >> src2)[0]
    def sra        = "b000_0111".U // sra:     src1 >> src2 (arithmetic)

    def rol        = "b000_1001".U // rol:     (src1 << src2) | (src1 >> (xlen - src2))
    def ror        = "b000_1011".U // ror:     (src1 >> src2) | (src1 << (xlen - src2))

    // RV64 32bit optype
    def addw       = "b001_0000".U // addw:      SEXT((src1 + src2)[31:0])
    def oddaddw    = "b001_0001".U // oddaddw:   SEXT((src1[0] + src2)[31:0])
    def subw       = "b001_0010".U // subw:      SEXT((src1 - src2)[31:0])

    def addwbit    = "b001_0100".U // addwbit:   (src1 + src2)[0]
    def addwbyte   = "b001_0101".U // addwbyte:  (src1 + src2)[7:0]
    def addwzexth  = "b001_0110".U // addwzexth: ZEXT((src1  + src2)[15:0])
    def addwsexth  = "b001_0111".U // addwsexth: SEXT((src1  + src2)[15:0])

    def sllw       = "b001_1000".U // sllw:     SEXT((src1 << src2)[31:0])
    def srlw       = "b001_1001".U // srlw:     SEXT((src1[31:0] >> src2)[31:0])
    def sraw       = "b001_1010".U // sraw:     SEXT((src1[31:0] >> src2)[31:0])
    def rolw       = "b001_1100".U
    def rorw       = "b001_1101".U

    // ADD-op
    def adduw      = "b010_0000".U // adduw:  src1[31:0]  + src2
    def add        = "b010_0001".U // add:     src1        + src2
    def oddadd     = "b010_0010".U // oddadd:  src1[0]     + src2

    def sr29add    = "b010_0100".U // sr29add: src1[63:29] + src2
    def sr30add    = "b010_0101".U // sr30add: src1[63:30] + src2
    def sr31add    = "b010_0110".U // sr31add: src1[63:31] + src2
    def sr32add    = "b010_0111".U // sr32add: src1[63:32] + src2

    def sh1adduw   = "b010_1000".U // sh1adduw: {src1[31:0], 1'b0} + src2
    def sh1add     = "b010_1001".U // sh1add: {src1[62:0], 1'b0} + src2
    def sh2adduw   = "b010_1010".U // sh2add_uw: {src1[31:0], 2'b0} + src2
    def sh2add     = "b010_1011".U // sh2add: {src1[61:0], 2'b0} + src2
    def sh3adduw   = "b010_1100".U // sh3add_uw: {src1[31:0], 3'b0} + src2
    def sh3add     = "b010_1101".U // sh3add: {src1[60:0], 3'b0} + src2
    def sh4add     = "b010_1111".U // sh4add: {src1[59:0], 4'b0} + src2

    // SUB-op: src1 - src2
    def sub        = "b011_0000".U
    def sltu       = "b011_0001".U
    def slt        = "b011_0010".U
    def maxu       = "b011_0100".U
    def minu       = "b011_0101".U
    def max        = "b011_0110".U
    def min        = "b011_0111".U

    // branch
    def beq        = "b111_0000".U
    def bne        = "b111_0010".U
    def blt        = "b111_1000".U
    def bge        = "b111_1010".U
    def bltu       = "b111_1100".U
    def bgeu       = "b111_1110".U

    // misc optype
    def and        = "b100_0000".U
    def andn       = "b100_0001".U
    def or         = "b100_0010".U
    def orn        = "b100_0011".U
    def xor        = "b100_0100".U
    def xnor       = "b100_0101".U
    def orcb       = "b100_0110".U

    def sextb      = "b100_1000".U
    def packh      = "b100_1001".U
    def sexth      = "b100_1010".U
    def packw      = "b100_1011".U

    def revb       = "b101_0000".U
    def rev8       = "b101_0001".U
    def pack       = "b101_0010".U
    def orh48      = "b101_0011".U

    def szewl1     = "b101_1000".U
    def szewl2     = "b101_1001".U
    def szewl3     = "b101_1010".U
    def byte2      = "b101_1011".U

    def andlsb     = "b110_0000".U
    def andzexth   = "b110_0001".U
    def orlsb      = "b110_0010".U
    def orzexth    = "b110_0011".U
    def xorlsb     = "b110_0100".U
    def xorzexth   = "b110_0101".U
    def orcblsb    = "b110_0110".U
    def orcbzexth  = "b110_0111".U
    def vsetvli1    = "b1000_0000".U
    def vsetvli2    = "b1000_0100".U
    def vsetvl1     = "b1000_0001".U
    def vsetvl2     = "b1000_0101".U
    def vsetivli1   = "b1000_0010".U
    def vsetivli2   = "b1000_0110".U

    def isAddw(func: UInt) = func(6, 4) === "b001".U && !func(3) && !func(1)
    def isSimpleLogic(func: UInt) = func(6, 4) === "b100".U && !func(0)
    def logicToLsb(func: UInt) = Cat("b110".U(3.W), func(3, 1), 0.U(1.W))
    def logicToZexth(func: UInt) = Cat("b110".U(3.W), func(3, 1), 1.U(1.W))
    def isBranch(func: UInt) = func(6, 4) === "b111".U
    def getBranchType(func: UInt) = func(3, 2)
    def isBranchInvert(func: UInt) = func(1)
    def isVset(func: UInt) = func(7, 3) === "b1000_0".U
    def isVsetvl(func: UInt) = isVset(func) && func(0)
    def isVsetvli(func: UInt) = isVset(func) && !func(1, 0).orR
    def vsetExchange(func: UInt) = Cat(func(7, 3), "b1".U, func(1, 0))

    def apply() = UInt(FuOpTypeWidth.W)
  }

  object MDUOpType {
    // mul
    // bit encoding: | type (2bit) | isWord(1bit) | opcode(2bit) |
    def mul    = "b00000".U
    def mulh   = "b00001".U
    def mulhsu = "b00010".U
    def mulhu  = "b00011".U
    def mulw   = "b00100".U

    def mulw7  = "b01100".U

    // div
    // bit encoding: | type (2bit) | isWord(1bit) | isSign(1bit) | opcode(1bit) |
    def div    = "b10000".U
    def divu   = "b10010".U
    def rem    = "b10001".U
    def remu   = "b10011".U

    def divw   = "b10100".U
    def divuw  = "b10110".U
    def remw   = "b10101".U
    def remuw  = "b10111".U

    def isMul(op: UInt) = !op(4)
    def isDiv(op: UInt) = op(4)

    def isDivSign(op: UInt) = isDiv(op) && !op(1)
    def isW(op: UInt) = op(2)
    def isH(op: UInt) = (isDiv(op) && op(0)) || (isMul(op) && op(1, 0) =/= 0.U)
    def getMulOp(op: UInt) = op(1, 0)
  }

  object LSUOpType {
    // load pipeline

    // normal load
    // Note: bit(1, 0) are size, DO NOT CHANGE
    // bit encoding: | load 0 | is unsigned(1bit) | size(2bit) |
    def lb       = "b0000".U
    def lh       = "b0001".U
    def lw       = "b0010".U
    def ld       = "b0011".U
    def lbu      = "b0100".U
    def lhu      = "b0101".U
    def lwu      = "b0110".U

    // Zicbop software prefetch
    // bit encoding: | prefetch 1 | 0 | prefetch type (2bit) |
    def prefetch_i = "b1000".U // TODO
    def prefetch_r = "b1001".U
    def prefetch_w = "b1010".U

    def isPrefetch(op: UInt): Bool = op(3)

    // store pipeline
    // normal store
    // bit encoding: | store 00 | size(2bit) |
    def sb       = "b0000".U
    def sh       = "b0001".U
    def sw       = "b0010".U
    def sd       = "b0011".U

    // l1 cache op
    // bit encoding: | cbo_zero 01 | size(2bit) 11 |
    def cbo_zero  = "b0111".U

    // llc op
    // bit encoding: | prefetch 11 | suboptype(2bit) |
    def cbo_clean = "b1100".U
    def cbo_flush = "b1101".U
    def cbo_inval = "b1110".U

    def isCbo(op: UInt): Bool = op(3, 2) === "b11".U

    // atomics
    // bit(1, 0) are size
    // since atomics use a different fu type
    // so we can safely reuse other load/store's encodings
    // bit encoding: | optype(4bit) | size (2bit) |
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

    def size(op: UInt) = op(1,0)
  }

  object BKUOpType {

    def clmul       = "b000000".U
    def clmulh      = "b000001".U
    def clmulr      = "b000010".U
    def xpermn      = "b000100".U
    def xpermb      = "b000101".U

    def clz         = "b001000".U
    def clzw        = "b001001".U
    def ctz         = "b001010".U
    def ctzw        = "b001011".U
    def cpop        = "b001100".U
    def cpopw       = "b001101".U

    // 01xxxx is reserve
    def aes64es     = "b100000".U
    def aes64esm    = "b100001".U
    def aes64ds     = "b100010".U
    def aes64dsm    = "b100011".U
    def aes64im     = "b100100".U
    def aes64ks1i   = "b100101".U
    def aes64ks2    = "b100110".U

    // merge to two instruction sm4ks & sm4ed
    def sm4ed0      = "b101000".U
    def sm4ed1      = "b101001".U
    def sm4ed2      = "b101010".U
    def sm4ed3      = "b101011".U
    def sm4ks0      = "b101100".U
    def sm4ks1      = "b101101".U
    def sm4ks2      = "b101110".U
    def sm4ks3      = "b101111".U

    def sha256sum0  = "b110000".U
    def sha256sum1  = "b110001".U
    def sha256sig0  = "b110010".U
    def sha256sig1  = "b110011".U
    def sha512sum0  = "b110100".U
    def sha512sum1  = "b110101".U
    def sha512sig0  = "b110110".U
    def sha512sig1  = "b110111".U

    def sm3p0       = "b111000".U
    def sm3p1       = "b111001".U
  }

  object BTBtype {
    def B = "b00".U  // branch
    def J = "b01".U  // jump
    def I = "b10".U  // indirect
    def R = "b11".U  // return

    def apply() = UInt(2.W)
  }

  object SelImm {
    def IMM_X  = "b0111".U
    def IMM_S  = "b1110".U
    def IMM_SB = "b0001".U
    def IMM_U  = "b0010".U
    def IMM_UJ = "b0011".U
    def IMM_I  = "b0100".U
    def IMM_Z  = "b0101".U
    def INVALID_INSTR = "b0110".U
    def IMM_B6 = "b1000".U

    def IMM_OPIVIS = "b1001".U
    def IMM_OPIVIU = "b1010".U
    def IMM_VSETVLI   = "b1100".U
    def IMM_VSETIVLI  = "b1101".U

    def X      = BitPat("b0000")

    def apply() = UInt(4.W)
  }

  object UopDivType {
    def SCA_SIM          = "b000000".U //
    def DIR              = "b010001".U // dirty: vset
    def VEC_VVV          = "b010010".U // VEC_VVV
    def VEC_VXV          = "b010011".U // VEC_VXV
    def VEC_0XV          = "b010100".U // VEC_0XV
    def VEC_VVW          = "b010101".U // VEC_VVW
    def VEC_WVW          = "b010110".U // VEC_WVW
    def VEC_VXW          = "b010111".U // VEC_VXW
    def VEC_WXW          = "b011000".U // VEC_WXW
    def VEC_WVV          = "b011001".U // VEC_WVV
    def VEC_WXV          = "b011010".U // VEC_WXV
    def VEC_EXT2         = "b011011".U // VF2 0 -> V
    def VEC_EXT4         = "b011100".U // VF4 0 -> V
    def VEC_EXT8         = "b011101".U // VF8 0 -> V
    def VEC_VVM          = "b011110".U // VEC_VVM
    def VEC_VXM          = "b011111".U // VEC_VXM
    def VEC_SLIDE1UP     = "b100000".U // vslide1up.vx
    def VEC_FSLIDE1UP    = "b100001".U // vfslide1up.vf
    def VEC_SLIDE1DOWN   = "b100010".U // vslide1down.vx
    def VEC_FSLIDE1DOWN  = "b100011".U // vfslide1down.vf
    def VEC_MMM          = "b000000".U // VEC_MMM
    def dummy     = "b111111".U

    def X = BitPat("b000000")

    def apply() = UInt(6.W)
    def needSplit(UopDivType: UInt) = UopDivType(4) || UopDivType(5)
  }

  object ExceptionNO {
    def instrAddrMisaligned = 0
    def instrAccessFault    = 1
    def illegalInstr        = 2
    def breakPoint          = 3
    def loadAddrMisaligned  = 4
    def loadAccessFault     = 5
    def storeAddrMisaligned = 6
    def storeAccessFault    = 7
    def ecallU              = 8
    def ecallS              = 9
    def ecallM              = 11
    def instrPageFault      = 12
    def loadPageFault       = 13
    // def singleStep          = 14
    def storePageFault      = 15
    def priorities = Seq(
      breakPoint, // TODO: different BP has different priority
      instrPageFault,
      instrAccessFault,
      illegalInstr,
      instrAddrMisaligned,
      ecallM, ecallS, ecallU,
      storeAddrMisaligned,
      loadAddrMisaligned,
      storePageFault,
      loadPageFault,
      storeAccessFault,
      loadAccessFault
    )
    def all = priorities.distinct.sorted
    def frontendSet = Seq(
      instrAddrMisaligned,
      instrAccessFault,
      illegalInstr,
      instrPageFault
    )
    def partialSelect(vec: Vec[Bool], select: Seq[Int]): Vec[Bool] = {
      val new_vec = Wire(ExceptionVec())
      new_vec.foreach(_ := false.B)
      select.foreach(i => new_vec(i) := vec(i))
      new_vec
    }
    def selectFrontend(vec: Vec[Bool]): Vec[Bool] = partialSelect(vec, frontendSet)
    def selectAll(vec: Vec[Bool]): Vec[Bool] = partialSelect(vec, ExceptionNO.all)
    def selectByFu(vec:Vec[Bool], fuConfig: FuConfig): Vec[Bool] =
      partialSelect(vec, fuConfig.exceptionOut)
    def selectByExu(vec:Vec[Bool], exuConfig: ExuConfig): Vec[Bool] =
      partialSelect(vec, exuConfig.exceptionOut)
    def selectByExu(vec:Vec[Bool], exuConfigs: Seq[ExuConfig]): Vec[Bool] =
      partialSelect(vec, exuConfigs.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted)
  }

  def dividerGen(p: Parameters) = new DividerWrapper(p(XLen))(p)
  def multiplierGen(p: Parameters) = new ArrayMultiplier(p(XLen) + 1)(p)
  def aluGen(p: Parameters) = new Alu()(p)
  def bkuGen(p: Parameters) = new Bku()(p)
  def jmpGen(p: Parameters) = new Jump()(p)
  def fenceGen(p: Parameters) = new Fence()(p)
  def csrGen(p: Parameters) = new CSR()(p)
  def i2fGen(p: Parameters) = new IntToFP()(p)
  def fmacGen(p: Parameters) = new FMA()(p)
  def f2iGen(p: Parameters) = new FPToInt()(p)
  def f2fGen(p: Parameters) = new FPToFP()(p)
  def fdivSqrtGen(p: Parameters) = new FDivSqrt()(p)
  def stdGen(p: Parameters) = new Std()(p)
  def mouDataGen(p: Parameters) = new Std()(p)
  def vipuGen(p: Parameters) = new VIPU()(p)
  def vialuFGen(p: Parameters) = new VIAluFix()(p)
  def vppuGen(p: Parameters) = new VPerm()(p)
  def vfpuGen(p: Parameters) = new VFPU()(p)

  def f2iSel(uop: MicroOp): Bool = {
    uop.ctrl.rfWen
  }

  def i2fSel(uop: MicroOp): Bool = {
    uop.ctrl.fpu.fromInt
  }

  def f2fSel(uop: MicroOp): Bool = {
    val ctrl = uop.ctrl.fpu
    ctrl.fpWen && !ctrl.div && !ctrl.sqrt
  }

  def fdivSqrtSel(uop: MicroOp): Bool = {
    val ctrl = uop.ctrl.fpu
    ctrl.div || ctrl.sqrt
  }

  val aluCfg = FuConfig(
    name = "alu",
    fuGen = aluGen,
    fuSel = (uop: MicroOp) => uop.ctrl.fuType === FuType.alu,
    fuType = FuType.alu,
    numIntSrc = 2,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    hasRedirect = true,
  )

  val jmpCfg = FuConfig(
    name = "jmp",
    fuGen = jmpGen,
    fuSel = (uop: MicroOp) => uop.ctrl.fuType === FuType.jmp,
    fuType = FuType.jmp,
    numIntSrc = 1,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    hasRedirect = true,
  )

  val fenceCfg = FuConfig(
    name = "fence",
    fuGen = fenceGen,
    fuSel = (uop: MicroOp) => uop.ctrl.fuType === FuType.fence,
    FuType.fence, 2, 0, writeIntRf = false, writeFpRf = false,
    latency = UncertainLatency(), exceptionOut = Seq(illegalInstr), // TODO: need rewrite latency structure, not just this value,
    flushPipe = true
  )

  val csrCfg = FuConfig(
    name = "csr",
    fuGen = csrGen,
    fuSel = (uop: MicroOp) => uop.ctrl.fuType === FuType.csr,
    fuType = FuType.csr,
    numIntSrc = 1,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    exceptionOut = Seq(illegalInstr, breakPoint, ecallU, ecallS, ecallM),
    flushPipe = true
  )

  val i2fCfg = FuConfig(
    name = "i2f",
    fuGen = i2fGen,
    fuSel = i2fSel,
    FuType.i2f,
    numIntSrc = 1,
    numFpSrc = 0,
    writeIntRf = false,
    writeFpRf = true,
    writeFflags = true,
    latency = CertainLatency(2),
    fastUopOut = true, fastImplemented = true
  )

  val divCfg = FuConfig(
    name = "div",
    fuGen = dividerGen,
    fuSel = (uop: MicroOp) => uop.ctrl.fuType === FuType.div,
    FuType.div,
    2,
    0,
    writeIntRf = true,
    writeFpRf = false,
    latency = UncertainLatency(),
    fastUopOut = true,
    fastImplemented = true,
    hasInputBuffer = (true, 4, true)
  )

  val mulCfg = FuConfig(
    name = "mul",
    fuGen = multiplierGen,
    fuSel = (uop: MicroOp) => uop.ctrl.fuType === FuType.mul,
    FuType.mul,
    2,
    0,
    writeIntRf = true,
    writeFpRf = false,
    latency = CertainLatency(2),
    fastUopOut = true,
    fastImplemented = true
  )

  val bkuCfg = FuConfig(
    name = "bku",
    fuGen = bkuGen,
    fuSel = (uop: MicroOp) => uop.ctrl.fuType === FuType.bku,
    fuType = FuType.bku,
    numIntSrc = 2,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    latency = CertainLatency(1),
    fastUopOut = true,
    fastImplemented = true
 )

  val fmacCfg = FuConfig(
    name = "fmac",
    fuGen = fmacGen,
    fuSel = (uop: MicroOp) => uop.ctrl.fuType === FuType.fmac,
    FuType.fmac, 0, 3, writeIntRf = false, writeFpRf = true, writeFflags = true,
    latency = UncertainLatency(), fastUopOut = true, fastImplemented = true
  )

  val f2iCfg = FuConfig(
    name = "f2i",
    fuGen = f2iGen,
    fuSel = f2iSel,
    FuType.fmisc, 0, 1, writeIntRf = true, writeFpRf = false, writeFflags = true, latency = CertainLatency(2),
    fastUopOut = true, fastImplemented = true
  )

  val f2fCfg = FuConfig(
    name = "f2f",
    fuGen = f2fGen,
    fuSel = f2fSel,
    FuType.fmisc, 0, 1, writeIntRf = false, writeFpRf = true, writeFflags = true, latency = CertainLatency(2),
    fastUopOut = true, fastImplemented = true
  )

  val fdivSqrtCfg = FuConfig(
    name = "fdivSqrt",
    fuGen = fdivSqrtGen,
    fuSel = fdivSqrtSel,
    FuType.fDivSqrt, 0, 2, writeIntRf = false, writeFpRf = true, writeFflags = true, latency = UncertainLatency(),
    fastUopOut = true, fastImplemented = true, hasInputBuffer = (true, 8, true)
  )

  val lduCfg = FuConfig(
    "ldu",
    null, // DontCare
    (uop: MicroOp) => FuType.loadCanAccept(uop.ctrl.fuType),
    FuType.ldu, 1, 0, writeIntRf = true, writeFpRf = true,
    latency = UncertainLatency(),
    exceptionOut = Seq(loadAddrMisaligned, loadAccessFault, loadPageFault),
    flushPipe = true,
    replayInst = true,
    hasLoadError = true
  )

  val staCfg = FuConfig(
    "sta",
    null,
    (uop: MicroOp) => FuType.storeCanAccept(uop.ctrl.fuType),
    FuType.stu, 1, 0, writeIntRf = false, writeFpRf = false,
    latency = UncertainLatency(),
    exceptionOut = Seq(storeAddrMisaligned, storeAccessFault, storePageFault)
  )

  val stdCfg = FuConfig(
    "std",
    fuGen = stdGen, fuSel = (uop: MicroOp) => FuType.storeCanAccept(uop.ctrl.fuType), FuType.stu, 1, 1,
    writeIntRf = false, writeFpRf = false, latency = CertainLatency(1)
  )

  val mouCfg = FuConfig(
    "mou",
    null,
    (uop: MicroOp) => FuType.storeCanAccept(uop.ctrl.fuType),
    FuType.mou, 1, 0, writeIntRf = false, writeFpRf = false,
    latency = UncertainLatency(), exceptionOut = lduCfg.exceptionOut ++ staCfg.exceptionOut
  )

  val mouDataCfg = FuConfig(
    "mou",
    mouDataGen,
    (uop: MicroOp) => FuType.storeCanAccept(uop.ctrl.fuType),
    FuType.mou, 1, 0, writeIntRf = false, writeFpRf = false,
    latency = UncertainLatency()
  )

  val vipuCfg = FuConfig(
    name = "vipu",
    fuGen = vipuGen,
    fuSel = (uop: MicroOp) => FuType.vipu === uop.ctrl.fuType,
    fuType = FuType.vipu,
    numIntSrc = 0, numFpSrc = 0, writeIntRf = false, writeFpRf = false, writeFflags = false, writeVxsat = true,
    numVecSrc = 4, writeVecRf = true,
    fastUopOut = false, // TODO: check
    fastImplemented = true, //TODO: check
  )

  val vialuFCfg = FuConfig(
    name = "vialuF",
    fuGen = vialuFGen,
    fuSel = (uop: MicroOp) => FuType.vialuF === uop.ctrl.fuType,
    fuType = FuType.vialuF,
    numIntSrc = 0, numFpSrc = 0, writeIntRf = false, writeFpRf = false, writeFflags = false, writeVxsat = true,
    numVecSrc = 4, writeVecRf = true,
    fastUopOut = false, // TODO: check
    fastImplemented = true, //TODO: check
  )

  val vppuCfg = FuConfig(
    name = "vppu",
    fuGen = vppuGen,
    fuSel = (uop: MicroOp) => FuType.vppu === uop.ctrl.fuType,
    fuType = FuType.vppu,
    numIntSrc = 0, numFpSrc = 1, writeIntRf = false, writeFpRf = false, writeFflags = false,
    numVecSrc = 1, writeVecRf = true,
    fastUopOut = false, // TODO: check
    fastImplemented = true, //TODO: check
  )

  val vfpuCfg = FuConfig(
    name = "vfpu",
    fuGen = vfpuGen,
    fuSel = (uop: MicroOp) => FuType.vfpu === uop.ctrl.fuType,
    fuType = FuType.vfpu,
    numIntSrc = 0, numFpSrc = 1, writeIntRf = false, writeFpRf = false, writeFflags = true,
    numVecSrc = 3, writeVecRf = true,
    fastUopOut = false, // TODO: check
    fastImplemented = true, //TODO: check
    // latency = CertainLatency(2)
  )

  val JumpExeUnitCfg = ExuConfig("JmpExeUnit", "Int", Seq(jmpCfg, i2fCfg), 2, Int.MaxValue)
  val AluExeUnitCfg = ExuConfig("AluExeUnit", "Int", Seq(aluCfg), 0, Int.MaxValue)
  val JumpCSRExeUnitCfg = ExuConfig("JmpCSRExeUnit", "Int", Seq(jmpCfg, csrCfg, fenceCfg, i2fCfg), 2, Int.MaxValue)
  val MulDivExeUnitCfg = ExuConfig("MulDivExeUnit", "Int", Seq(mulCfg, divCfg, bkuCfg), 1, Int.MaxValue)
  val FmacExeUnitCfg = ExuConfig("FmacExeUnit", "Fp", Seq(fmacCfg, vipuCfg, vppuCfg, vfpuCfg, vialuFCfg), Int.MaxValue, 0)
  val FmiscExeUnitCfg = ExuConfig(
    "FmiscExeUnit",
    "Fp",
    Seq(f2iCfg, f2fCfg, fdivSqrtCfg),
    Int.MaxValue, 1
  )
  val LdExeUnitCfg = ExuConfig("LoadExu", "Mem", Seq(lduCfg), wbIntPriority = 0, wbFpPriority = 0, extendsExu = false)
  val StaExeUnitCfg = ExuConfig("StaExu", "Mem", Seq(staCfg, mouCfg), wbIntPriority = Int.MaxValue, wbFpPriority = Int.MaxValue, extendsExu = false)
  val StdExeUnitCfg = ExuConfig("StdExu", "Mem", Seq(stdCfg, mouDataCfg), wbIntPriority = Int.MaxValue, wbFpPriority = Int.MaxValue, extendsExu = false)

  // def jumpRSWrapperGen(p: Parameters) = new JumpRSWrapper()(p)
  // def mulRSWrapperGen(p: Parameters) = new MulRSWrapper()(p)
  // def loadRSWrapperGen(p: Parameters) = new LoadRSWrapper()(p)
  // def stdRSWrapperGen(p: Parameters) = new StdRSWrapper()(p)
  // def staRSWrapperGen(p: Parameters) = new StaRSWrapper()(p)
  // def fmaRSWrapperGen(p: Parameters) = new FMARSWrapper()(p)
  // def fmiscRSWrapperGen(p: Parameters) = new FMiscRSWrapper()(p)

  val aluRSMod = new RSMod(
    rsWrapperGen = (modGen: RSMod, p: Parameters) => new ALURSWrapper(modGen)(p),
    rsGen = (a: RSParams, b: Parameters) => new ALURS(a)(b),
    immExtractorGen = (src: Int, width: Int, p: Parameters) => new AluImmExtractor()(p)
  )
  val fmaRSMod = new RSMod(
    rsWrapperGen = (modGen: RSMod, p: Parameters) => new FMARSWrapper(modGen)(p),
    rsGen = (a: RSParams, b: Parameters) => new FMARS(a)(b),
  )
  val fmiscRSMod = new RSMod(
    rsWrapperGen = (modGen: RSMod, p: Parameters) => new FMiscRSWrapper(modGen)(p),
    rsGen = (a: RSParams, b: Parameters) => new FMiscRS(a)(b),
  )
  val jumpRSMod = new RSMod(
    rsWrapperGen = (modGen: RSMod, p: Parameters) => new JumpRSWrapper(modGen)(p),
    rsGen = (a: RSParams, b: Parameters) => new JumpRS(a)(b),
    immExtractorGen = (src: Int, width: Int, p: Parameters) => new JumpImmExtractor()(p)
  )
  val loadRSMod = new RSMod(
    rsWrapperGen = (modGen: RSMod, p: Parameters) => new LoadRSWrapper(modGen)(p),
    rsGen = (a: RSParams, b: Parameters) => new LoadRS(a)(b),
    immExtractorGen = (src: Int, width: Int, p: Parameters) => new LoadImmExtractor()(p)
  )
  val mulRSMod = new RSMod(
    rsWrapperGen = (modGen: RSMod, p: Parameters) => new MulRSWrapper(modGen)(p),
    rsGen = (a: RSParams, b: Parameters) => new MulRS(a)(b),
    immExtractorGen = (src: Int, width: Int, p: Parameters) => new MduImmExtractor()(p)
  )
  val staRSMod = new RSMod(
    rsWrapperGen = (modGen: RSMod, p: Parameters) => new StaRSWrapper(modGen)(p),
    rsGen = (a: RSParams, b: Parameters) => new StaRS(a)(b),
  )
  val stdRSMod = new RSMod(
    rsWrapperGen = (modGen: RSMod, p: Parameters) => new StdRSWrapper(modGen)(p),
    rsGen = (a: RSParams, b: Parameters) => new StdRS(a)(b),
  )
}
