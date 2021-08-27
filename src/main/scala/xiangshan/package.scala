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
import xiangshan.backend.fu._
import xiangshan.backend.fu.fpu._
import xiangshan.backend.exu._
import xiangshan.backend.Std

package object xiangshan {
  object SrcType {
    def reg = "b00".U
    def pc  = "b01".U
    def imm = "b01".U
    def fp  = "b10".U

    def DC = imm // Don't Care

    def isReg(srcType: UInt) = srcType===reg
    def isPc(srcType: UInt) = srcType===pc
    def isImm(srcType: UInt) = srcType===imm
    def isFp(srcType: UInt) = srcType===fp
    def isPcImm(srcType: UInt) = srcType(0)
    def isRegFp(srcType: UInt) = !srcType(0)

    def apply() = UInt(2.W)
  }

  object SrcState {
    def busy    = "b0".U
    def rdy     = "b1".U
    // def specRdy = "b10".U // speculative ready, for future use
    def apply() = UInt(1.W)
  }

  object FuType {
    def jmp          = "b0000".U
    def i2f          = "b0001".U
    def csr          = "b0010".U
    def alu          = "b0110".U
    def mul          = "b0100".U
    def div          = "b0101".U
    def fence        = "b0011".U
    def bmu          = "b0111".U

    def fmac         = "b1000".U
    def fmisc        = "b1011".U
    def fDivSqrt     = "b1010".U

    def ldu          = "b1100".U
    def stu          = "b1101".U
    def mou          = "b1111".U // for amo, lr, sc, fence

    def num = 14

    def apply() = UInt(log2Up(num).W)

    def isIntExu(fuType: UInt) = !fuType(3)
    def isJumpExu(fuType: UInt) = fuType === jmp
    def isFpExu(fuType: UInt) = fuType(3, 2) === "b10".U
    def isMemExu(fuType: UInt) = fuType(3, 2) === "b11".U
    def isLoadStore(fuType: UInt) = isMemExu(fuType) && !fuType(1)
    def isStoreExu(fuType: UInt) = isMemExu(fuType) && fuType(0)
    def isAMO(fuType: UInt) = fuType(1)

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
      i2f.litValue() -> "int to float",
      csr.litValue() -> "csr",
      alu.litValue() -> "alu",
      mul.litValue() -> "mul",
      div.litValue() -> "div",
      fence.litValue() -> "fence",
      fmac.litValue() -> "fmac",
      fmisc.litValue() -> "fmisc",
      fDivSqrt.litValue() -> "fdiv/fsqrt",
      ldu.litValue() -> "load",
      stu.litValue() -> "store"
    )

  }

  object FuOpType {
    def apply() = UInt(8.W)
  }

  object CommitType {
    def NORMAL = "b00".U  // int/fp
    def BRANCH = "b01".U  // branch
    def LOAD   = "b10".U  // load
    def STORE  = "b11".U  // store

    def apply() = UInt(2.W)
    def isLoadStore(commitType: UInt) = commitType(1)
    def lsInstIsStore(commitType: UInt) = commitType(0)
    def isStore(commitType: UInt) = isLoadStore(commitType) && lsInstIsStore(commitType)
    def isBranch(commitType: UInt) = commitType(0) && !commitType(1)
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
    def wrti = "b101".U
    def seti = "b110".U
    def clri = "b111".U
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
  }

  object ALUOpType {
    // misc & branch optype
    def and         = "b0_00_00_000".U
    def andn        = "b0_00_00_001".U
    def or          = "b0_00_00_010".U
    def orn         = "b0_00_00_011".U
    def xor         = "b0_00_00_100".U
    def xnor        = "b0_00_00_101".U

    def sext_b      = "b0_00_01_000".U
    def sext_h      = "b0_00_01_001".U
    def zext_h      = "b0_00_01_010".U
    def orc_b       = "b0_00_01_100".U
    def rev8        = "b0_00_01_101".U

    def beq         = "b0_00_10_000".U
    def bne         = "b0_00_10_001".U
    def blt         = "b0_00_10_100".U
    def bge         = "b0_00_10_101".U
    def bltu        = "b0_00_10_110".U
    def bgeu        = "b0_00_10_111".U

    // add & sub optype
    def add         = "b0_01_00_000".U
    def add_uw      = "b0_01_00_001".U
    def sh1add      = "b0_01_00_010".U
    def sh1add_uw   = "b0_01_00_011".U
    def sh2add      = "b0_01_00_100".U
    def sh2add_uw   = "b0_01_00_101".U
    def sh3add      = "b0_01_00_110".U
    def sh3add_uw   = "b0_01_00_111".U


    // shift optype
    def sll         = "b0_10_00_000".U
    def slli_uw     = "b0_10_00_001".U
    def bclr        = "b0_10_00_100".U
    def bset        = "b0_10_00_101".U
    def binv        = "b0_10_00_110".U
    
    def srl         = "b0_10_01_001".U
    def bext        = "b0_10_01_010".U
    def sra         = "b0_10_01_100".U

    def rol         = "b0_10_10_000".U

    def ror         = "b0_10_11_000".U

    def sub         = "b0_11_00_000".U
    def sltu        = "b0_11_00_001".U
    def slt         = "b0_11_00_010".U
    def maxu        = "b0_11_00_100".U
    def minu        = "b0_11_00_101".U
    def max         = "b0_11_00_110".U
    def min         = "b0_11_00_111".U
    


    // RV64 32bit optype
    def addw        = "b1_01_00_000".U
    def subw        = "b1_11_00_000".U
    def sllw        = "b1_10_00_000".U
    def srlw        = "b1_10_01_001".U
    def sraw        = "b1_10_01_100".U
    def rolw        = "b1_10_10_000".U
    def rorw        = "b1_10_11_000".U

    def isWordOp(func: UInt) = func(7)
    def isBranch(func: UInt) = func(6, 3) === "b0010".U
    def getBranchType(func: UInt) = func(2, 1)
    def isBranchInvert(func: UInt) = func(0)

    def apply() = UInt(8.W)
  }

  object MDUOpType {
    // mul
    // bit encoding: | type (2bit) | isWord(1bit) | opcode(2bit) |
    def mul    = "b00000".U
    def mulh   = "b00001".U
    def mulhsu = "b00010".U
    def mulhu  = "b00011".U
    def mulw   = "b00100".U

    // div
    // bit encoding: | type (2bit) | isWord(1bit) | isSign(1bit) | opcode(1bit) |
    def div    = "b01000".U
    def divu   = "b01010".U
    def rem    = "b01001".U
    def remu   = "b01011".U

    def divw   = "b01100".U
    def divuw  = "b01110".U
    def remw   = "b01101".U
    def remuw  = "b01111".U

    // fence
    // bit encoding: | type (2bit) | padding(1bit)(zero) | opcode(2bit) |
    def fence    = "b10000".U
    def sfence   = "b10001".U
    def fencei   = "b10010".U

    // the highest bits are for instruction types
    def typeMSB = 4
    def typeLSB = 3

    def MulType     = "b00".U
    def DivType     = "b01".U
    def FenceType   = "b10".U

    def isMul(op: UInt)     = op(typeMSB, typeLSB) === MulType
    def isDiv(op: UInt)     = op(typeMSB, typeLSB) === DivType
    def isFence(op: UInt)   = op(typeMSB, typeLSB) === FenceType

    def isDivSign(op: UInt) = isDiv(op) && !op(1)
    def isW(op: UInt) = op(2)
    def isH(op: UInt) = (isDiv(op) && op(0)) || (isMul(op) && op(1,0)=/=0.U)
    def getMulOp(op: UInt) = op(1,0)
  }

  object LSUOpType {
    // normal load/store
    // bit(1, 0) are size
    def lb   = "b000000".U
    def lh   = "b000001".U
    def lw   = "b000010".U
    def ld   = "b000011".U
    def lbu  = "b000100".U
    def lhu  = "b000101".U
    def lwu  = "b000110".U
    def sb   = "b001000".U
    def sh   = "b001001".U
    def sw   = "b001010".U
    def sd   = "b001011".U

    def isLoad(op: UInt): Bool = !op(3)
    def isStore(op: UInt): Bool = op(3)

    // atomics
    // bit(1, 0) are size
    // since atomics use a different fu type
    // so we can safely reuse other load/store's encodings
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
  }

  object BMUOpType {
    
    def clmul       = "b0000".U
    def clmulh      = "b0010".U
    def clmulr      = "b0100".U

    def clz         = "b1000".U
    def clzw        = "b1001".U
    def ctz         = "b1010".U
    def ctzw        = "b1011".U
    def cpop        = "b1100".U
    def cpopw       = "b1101".U
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
    def IMM_S  = "b0000".U
    def IMM_SB = "b0001".U
    def IMM_U  = "b0010".U
    def IMM_UJ = "b0011".U
    def IMM_I  = "b0100".U
    def IMM_Z  = "b0101".U
    def INVALID_INSTR = "b0110".U
    def IMM_B6 = "b1000".U

    def apply() = UInt(4.W)
  }

  def dividerGen(p: Parameters) = new SRT4Divider(p(XLen))(p)
  def multiplierGen(p: Parameters) = new ArrayMultiplier(p(XLen) + 1, Seq(0, 2))(p)
  def aluGen(p: Parameters) = new Alu()(p)
  def bmuGen(p: Parameters) = new Bmu()(p)
  def jmpGen(p: Parameters) = new Jump()(p)
  def fenceGen(p: Parameters) = new Fence()(p)
  def csrGen(p: Parameters) = new CSR()(p)
  def i2fGen(p: Parameters) = new IntToFP()(p)
  def fmacGen(p: Parameters) = new FMA()(p)
  def f2iGen(p: Parameters) = new FPToInt()(p)
  def f2fGen(p: Parameters) = new FPToFP()(p)
  def fdivSqrtGen(p: Parameters) = new FDivSqrt()(p)
  def stdGen(p: Parameters) = new Std()(p)

  def f2iSel(x: FunctionUnit): Bool = {
    x.io.in.bits.uop.ctrl.rfWen
  }

  def i2fSel(x: FunctionUnit): Bool = {
    x.io.in.bits.uop.ctrl.fpu.fromInt
  }

  def f2fSel(x: FunctionUnit): Bool = {
    val ctrl = x.io.in.bits.uop.ctrl.fpu
    ctrl.fpWen && !ctrl.div && !ctrl.sqrt
  }

  def fdivSqrtSel(x: FunctionUnit): Bool = {
    val ctrl = x.io.in.bits.uop.ctrl.fpu
    ctrl.div || ctrl.sqrt
  }

  val aluCfg = FuConfig(
    name = "alu",
    fuGen = aluGen,
    fuSel = (x: FunctionUnit) => x.io.in.bits.uop.ctrl.fuType === FuType.alu,
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
    fuSel = (x: FunctionUnit) => x.io.in.bits.uop.ctrl.fuType === FuType.jmp,
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
    fuSel = (x: FunctionUnit) => x.io.in.bits.uop.ctrl.fuType === FuType.fence,
    FuType.fence, 1, 0, writeIntRf = false, writeFpRf = false, hasRedirect = false,
    UncertainLatency() // TODO: need rewrite latency structure, not just this value
  )

  val csrCfg = FuConfig(
    name = "csr",
    fuGen = csrGen,
    fuSel = (x: FunctionUnit) => x.io.in.bits.uop.ctrl.fuType === FuType.csr,
    fuType = FuType.csr,
    numIntSrc = 1,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    hasRedirect = false
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
    hasRedirect = false,
    UncertainLatency()
  )

  val divCfg = FuConfig(
    name = "div",
    fuGen = dividerGen,
    fuSel = (x: FunctionUnit) => MDUOpType.isDiv(x.io.in.bits.uop.ctrl.fuOpType),
    FuType.div,
    2,
    0,
    writeIntRf = true,
    writeFpRf = false,
    hasRedirect = false,
    latency = UncertainLatency(),
    fastUopOut = true,
    fastImplemented = false
  )

  val mulCfg = FuConfig(
    name = "mul",
    fuGen = multiplierGen,
    fuSel = (x: FunctionUnit) => MDUOpType.isMul(x.io.in.bits.uop.ctrl.fuOpType),
    FuType.mul,
    2,
    0,
    writeIntRf = true,
    writeFpRf = false,
    hasRedirect = false,
    // TODO: change this back to 2 when mul is ready for fastUopOut
    latency = CertainLatency(3),
    fastUopOut = true,
    fastImplemented = false
  )

  val bmuCfg = FuConfig(
    name = "bmu",
    fuGen = bmuGen,
    fuSel = (x: FunctionUnit) => x.io.in.bits.uop.ctrl.fuType === FuType.bmu,
    fuType = FuType.bmu,
    numIntSrc = 2,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    hasRedirect = false,
    latency = CertainLatency(1),
    fastUopOut = true,
    fastImplemented = false
 )

  val fmacCfg = FuConfig(
    name = "fmac",
    fuGen = fmacGen,
    fuSel = _ => true.B,
    FuType.fmac, 0, 3, writeIntRf = false, writeFpRf = true, hasRedirect = false, CertainLatency(4)
  )

  val f2iCfg = FuConfig(
    name = "f2i",
    fuGen = f2iGen,
    fuSel = f2iSel,
    FuType.fmisc, 0, 1, writeIntRf = true, writeFpRf = false, hasRedirect = false, CertainLatency(2),
    fastUopOut = true, fastImplemented = false
  )

  val f2fCfg = FuConfig(
    name = "f2f",
    fuGen = f2fGen,
    fuSel = f2fSel,
    FuType.fmisc, 0, 1, writeIntRf = false, writeFpRf = true, hasRedirect = false, CertainLatency(2),
    fastUopOut = true, fastImplemented = false
  )

  val fdivSqrtCfg = FuConfig(
    name = "fdivSqrt",
    fuGen = fdivSqrtGen,
    fuSel = fdivSqrtSel,
    FuType.fDivSqrt, 0, 2, writeIntRf = false, writeFpRf = true, hasRedirect = false, UncertainLatency(),
    fastUopOut = true, fastImplemented = false
  )

  val lduCfg = FuConfig(
    "ldu",
    null, // DontCare
    null,
    FuType.ldu, 1, 0, writeIntRf = true, writeFpRf = true, hasRedirect = false,
    UncertainLatency()
  )

  val staCfg = FuConfig(
    "sta",
    null,
    null,
    FuType.stu, 1, 0, writeIntRf = false, writeFpRf = false, hasRedirect = false,
    UncertainLatency()
  )

  val stdCfg = FuConfig(
    "std",
    fuGen = stdGen, fuSel = _ => true.B, FuType.stu, 1, 1,
    writeIntRf = false, writeFpRf = false, hasRedirect = false, UncertainLatency()
  )

  val mouCfg = FuConfig(
    "mou",
    null,
    null,
    FuType.mou, 1, 0, writeIntRf = false, writeFpRf = false, hasRedirect = false,
    UncertainLatency()
  )

  val JumpExeUnitCfg = ExuConfig("JmpExeUnit", "Int", Seq(jmpCfg, i2fCfg), 2, Int.MaxValue)
  val AluExeUnitCfg = ExuConfig("AluExeUnit", "Int", Seq(aluCfg), 0, Int.MaxValue)
  val JumpCSRExeUnitCfg = ExuConfig("JmpCSRExeUnit", "Int", Seq(jmpCfg, csrCfg, fenceCfg, i2fCfg), 2, Int.MaxValue)
  val MulDivExeUnitCfg = ExuConfig("MulDivExeUnit", "Int", Seq(mulCfg, divCfg, bmuCfg), 1, Int.MaxValue)
  val FmacExeUnitCfg = ExuConfig("FmacExeUnit", "Fp", Seq(fmacCfg), Int.MaxValue, 0)
  val FmiscExeUnitCfg = ExuConfig(
    "FmiscExeUnit",
    "Fp",
    Seq(f2iCfg, f2fCfg, fdivSqrtCfg),
    Int.MaxValue, 1
  )
  val LdExeUnitCfg = ExuConfig("LoadExu", "Mem", Seq(lduCfg), wbIntPriority = 0, wbFpPriority = 0)
  val StaExeUnitCfg = ExuConfig("StaExu", "Mem", Seq(staCfg, mouCfg), wbIntPriority = Int.MaxValue, wbFpPriority = Int.MaxValue)
  val StdExeUnitCfg = ExuConfig("StdExu", "Mem", Seq(stdCfg), wbIntPriority = Int.MaxValue, wbFpPriority = Int.MaxValue)
}
