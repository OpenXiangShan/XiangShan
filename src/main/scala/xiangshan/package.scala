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
import xiangshan.ExceptionNO._
import xiangshan.backend.fu._
import xiangshan.backend.fu.fpu._
import xiangshan.backend.fu.vector._
import xiangshan.backend.issue._
import xiangshan.backend.fu.FuConfig

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
    def isXp(srcType: UInt) = srcType(0)
    def isFp(srcType: UInt) = srcType(1)
    def isVp(srcType: UInt) = srcType(2)
    def isPcOrImm(srcType: UInt) = isPc(srcType) || isImm(srcType)
    def isNotReg(srcType: UInt): Bool = !srcType.orR
    def isVfp(srcType: UInt) = isVp(srcType) || isFp(srcType)
    def apply() = UInt(3.W)
  }

  object SrcState {
    def busy    = "b0".U
    def rdy     = "b1".U
    // def specRdy = "b10".U // speculative ready, for future use
    def apply() = UInt(1.W)

    def isReady(state: UInt): Bool = state === this.rdy
    def isBusy(state: UInt): Bool = state === this.busy
  }

  def FuOpTypeWidth = 9
  object FuOpType {
    def apply() = UInt(FuOpTypeWidth.W)
    def X = BitPat("b00000000")
  }

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
    val ExceptionVecSize = 16
    def apply() = Vec(ExceptionVecSize, Bool())
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

    def isAddw(func: UInt) = func(6, 4) === "b001".U && !func(3) && !func(1)
    def isSimpleLogic(func: UInt) = func(6, 4) === "b100".U && !func(0)
    def logicToLsb(func: UInt) = Cat("b110".U(3.W), func(3, 1), 0.U(1.W))
    def logicToZexth(func: UInt) = Cat("b110".U(3.W), func(3, 1), 1.U(1.W))

    def apply() = UInt(FuOpTypeWidth.W)
  }

  object VSETOpType {
    val setVlmaxBit = 0
    val keepVlBit   = 1
    // destTypeBit == 0: write vl to rd
    // destTypeBit == 1: write vconfig
    val destTypeBit = 5

    // vsetvli's uop
    //   rs1!=x0, normal
    //     uop0: r(rs1), w(vconfig)     | x[rs1],vtypei  -> vconfig
    //     uop1: r(rs1), w(rd)          | x[rs1],vtypei  -> x[rd]
    def uvsetvcfg_xi        = "b1010_0000".U
    def uvsetrd_xi          = "b1000_0000".U
    //   rs1==x0, rd!=x0, set vl to vlmax, set rd to vlmax, set vtype
    //     uop0: w(vconfig)             | vlmax, vtypei  -> vconfig
    //     uop1: w(rd)                  | vlmax, vtypei  -> x[rd]
    def uvsetvcfg_vlmax_i   = "b1010_0001".U
    def uvsetrd_vlmax_i     = "b1000_0001".U
    //   rs1==x0, rd==x0, keep vl, set vtype
    //     uop0: r(vconfig), w(vconfig) | ld_vconfig.vl, vtypei -> vconfig
    def uvsetvcfg_keep_v    = "b1010_0010".U

    // vsetvl's uop
    //   rs1!=x0, normal
    //     uop0: r(rs1,rs2), w(vconfig) | x[rs1],x[rs2]  -> vconfig
    //     uop1: r(rs1,rs2), w(rd)      | x[rs1],x[rs2]  -> x[rd]
    def uvsetvcfg_xx        = "b0110_0000".U
    def uvsetrd_xx          = "b0100_0000".U
    //   rs1==x0, rd!=x0, set vl to vlmax, set rd to vlmax, set vtype
    //     uop0: r(rs2), w(vconfig)     | vlmax, vtypei  -> vconfig
    //     uop1: r(rs2), w(rd)          | vlmax, vtypei  -> x[rd]
    def uvsetvcfg_vlmax_x   = "b0110_0001".U
    def uvsetrd_vlmax_x     = "b0100_0001".U
    //   rs1==x0, rd==x0, keep vl, set vtype
    //     uop0: r(rs2), w(vtmp)             | x[rs2]               -> vtmp
    //     uop0: r(vconfig,vtmp), w(vconfig) | old_vconfig.vl, vtmp -> vconfig
    def uvmv_v_x            = "b0110_0010".U
    def uvsetvcfg_vv        = "b0111_0010".U

    // vsetivli's uop
    //     uop0: w(vconfig)             | vli, vtypei    -> vconfig
    //     uop1: w(rd)                  | vli, vtypei    -> x[rd]
    def uvsetvcfg_ii        = "b0010_0000".U
    def uvsetrd_ii          = "b0000_0000".U

    def isVsetvl  (func: UInt)  = func(6)
    def isVsetvli (func: UInt)  = func(7)
    def isVsetivli(func: UInt)  = func(7, 6) === 0.U
    def isNormal  (func: UInt)  = func(1, 0) === 0.U
    def isSetVlmax(func: UInt)  = func(setVlmaxBit)
    def isKeepVl  (func: UInt)  = func(keepVlBit)
    // RG: region
    def writeIntRG(func: UInt)  = !func(5)
    def writeVecRG(func: UInt)  = func(5)
    def readIntRG (func: UInt)  = !func(4)
    def readVecRG (func: UInt)  = func(4)
    // modify fuOpType
    def switchDest(func: UInt)  = func ^ (1 << destTypeBit).U
    def keepVl(func: UInt)      = func | (1 << keepVlBit).U
    def setVlmax(func: UInt)    = func | (1 << setVlmaxBit).U
  }

  object BRUOpType {
    // branch
    def beq        = "b000_000".U
    def bne        = "b000_001".U
    def blt        = "b000_100".U
    def bge        = "b000_101".U
    def bltu       = "b001_000".U
    def bgeu       = "b001_001".U

    def getBranchType(func: UInt) = func(3, 1)
    def isBranchInvert(func: UInt) = func(0)
  }

  object MULOpType {
    // mul
    // bit encoding: | type (2bit) | isWord(1bit) | opcode(2bit) |
    def mul    = "b00000".U
    def mulh   = "b00001".U
    def mulhsu = "b00010".U
    def mulhu  = "b00011".U
    def mulw   = "b00100".U

    def mulw7  = "b01100".U
    def isSign(op: UInt) = !op(1)
    def isW(op: UInt) = op(2)
    def isH(op: UInt) = op(1, 0) =/= 0.U
    def getOp(op: UInt) = Cat(op(3), op(1, 0))
  }

  object DIVOpType {
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

    def isSign(op: UInt) = !op(1)
    def isW(op: UInt) = op(2)
    def isH(op: UInt) = op(0)
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

    def mkString(immType: UInt) : String = {
      val strMap = Map(
        IMM_S.litValue         -> "S",
        IMM_SB.litValue        -> "SB",
        IMM_U.litValue         -> "U",
        IMM_UJ.litValue        -> "UJ",
        IMM_I.litValue         -> "I",
        IMM_Z.litValue         -> "Z",
        IMM_B6.litValue        -> "B6",
        IMM_OPIVIS.litValue    -> "VIS",
        IMM_OPIVIU.litValue    -> "VIU",
        IMM_VSETVLI.litValue   -> "VSETVLI",
        IMM_VSETIVLI.litValue  -> "VSETIVLI",
        INVALID_INSTR.litValue -> "INVALID",
      )
      strMap(immType.litValue)
    }
  }

  object UopSplitType {
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
    def VEC_VRED         = "b100100".U // VEC_VRED
    def VEC_SLIDEUP      = "b100101".U // VEC_SLIDEUP
    def VEC_ISLIDEUP     = "b100110".U // VEC_ISLIDEUP
    def VEC_SLIDEDOWN    = "b100111".U // VEC_SLIDEDOWN
    def VEC_ISLIDEDOWN   = "b101000".U // VEC_ISLIDEDOWN
    def VEC_M0X          = "b101001".U // VEC_M0X  0MV
    def VEC_MVV          = "b101010".U // VEC_MVV  VMV
    def VEC_M0X_VFIRST   = "b101011".U //
    def VEC_VWW          = "b101100".U //
    def VEC_RGATHER      = "b101101".U // vrgather.vv, vrgather.vi
    def VEC_RGATHER_VX   = "b101110".U // vrgather.vx
    def VEC_RGATHEREI16  = "b101111".U // vrgatherei16.vv
    def VEC_COMPRESS     = "b110000".U // vcompress.vm
    def VEC_US_LD        = "b110001".U // vector unit strided load
    def VEC_VFV          = "b111000".U // VEC_VFV
    def VEC_VFW          = "b111001".U // VEC_VFW
    def VEC_WFW          = "b111010".U // VEC_WVW
    def VEC_M0M          = "b000000".U // VEC_M0M
    def VEC_MMM          = "b000000".U // VEC_MMM
    def dummy     = "b111111".U

    def X = BitPat("b000000")

    def apply() = UInt(6.W)
    def needSplit(UopSplitType: UInt) = UopSplitType(4) || UopSplitType(5)
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
  }
}
