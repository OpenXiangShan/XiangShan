package core

import chisel3._
import chisel3.util._

object Decode {
  /* instruction type */
  private val InstrTypeNum = 7
  val InstrN = "b000".U
  val InstrI = "b100".U
  val InstrR = "b101".U
  val InstrS = "b010".U
  val InstrB = "b001".U
  val InstrU = "b110".U
  val InstrJ = "b111".U
  val InstrTypeWidth = log2Up(InstrTypeNum).W

  def isrfWen(instrType : UInt): Bool = instrType(2)

  /* src1 type */
  private val Src1TypeNum = 2
  val Src1Reg = "b0".U
  val Src1Pc  = "b1".U
  val Src1TypeWidth = log2Up(Src1TypeNum).W

  /* src2 type */
  private val Src2TypeNum = 2
  val Src2Imm = "b0".U
  val Src2Reg = "b1".U
  val Src2TypeWidth = log2Up(Src2TypeNum).W

  val SrcTypeTable = List(
    InstrI -> (Src1Reg, Src2Imm),
    InstrR -> (Src1Reg, Src2Reg),
    InstrS -> (Src1Reg, Src2Imm),
    InstrB -> (Src1Reg, Src2Imm),
    InstrU -> (Src1Pc , Src2Imm),
    InstrJ -> (Src1Pc , Src2Imm),
    InstrN -> (Src1Pc , Src2Imm)
  )

  /* function unit type */
  private val FuTypeNum = 4
  val FuAlu = "b00".U
  val FuBru = "b01".U
  val FuLsu = "b10".U
  val FuMdu = "b11".U
  val FuTypeWidth = log2Up(FuTypeNum).W

  /* ALU operation type */
  private val FuOpTypeAluNum  = 11
  val AluAdd  = "b0000".U
  val AluSll  = "b0001".U
  val AluSlt  = "b0010".U
  val AluSltu = "b0011".U
  val AluXor  = "b0100".U
  val AluSrl  = "b0101".U
  val AluOr   = "b0110".U
  val AluAnd  = "b0111".U
  val AluSub  = "b1000".U
  val AluSra  = "b1101".U
  val AluLui  = "b1111".U

  /* BRU operation type */
  private val FuOpTypeBruNum  = 10
  val BruJal  = "b1000".U
  val BruJalr = "b1001".U
  val BruBeq  = "b0000".U
  val BruBne  = "b0001".U
  val BruBlt  = "b0100".U
  val BruBge  = "b0101".U

  /* LSU operation type */
  private val FuOpTypeLsuNum  = 10
  val LsuLw   = "b0010".U
  val LsuSw   = "b1010".U

  /* MDU operation type */
  private val FuOpTypeMduNum  = 0

  private val FuOpTypeMaxNum = List(FuOpTypeAluNum, FuOpTypeBruNum,
    FuOpTypeLsuNum, FuOpTypeMduNum).reduce(math.max)
  val FuOpTypeWidth = log2Up(FuOpTypeMaxNum).W


  /* instruction pattern */
  val ADDI    = BitPat("b????????????_?????_000_?????_0010011")
  val SLTIU   = BitPat("b????????????_?????_011_?????_0010011")
  val SLLI    = BitPat("b0000000?????_?????_001_?????_0010011")
  val SRLI    = BitPat("b0000000?????_?????_101_?????_0010011")
  val ANDI    = BitPat("b????????????_?????_111_?????_0010011")

  val ADD     = BitPat("b0000000_?????_?????_000_?????_0110011")
  val SLT     = BitPat("b0000000_?????_?????_010_?????_0110011")
  val SLTU    = BitPat("b0000000_?????_?????_011_?????_0110011")
  val XOR     = BitPat("b0000000_?????_?????_100_?????_0110011")
  val SRL     = BitPat("b0000000_?????_?????_101_?????_0110011")
  val OR      = BitPat("b0000000_?????_?????_110_?????_0110011")
  val SUB     = BitPat("b0100000_?????_?????_000_?????_0110011")
  val SRA     = BitPat("b0100000_?????_?????_101_?????_0110011")

  val AUIPC   = BitPat("b????????????????????_?????_0010111")
  val LUI     = BitPat("b????????????????????_?????_0110111")

  val JAL     = BitPat("b????????????????????_?????_1101111")
  val JALR    = BitPat("b????????????_?????_000_?????_1100111")

  val BNE     = BitPat("b???????_?????_?????_001_?????_1100011")
  val BEQ     = BitPat("b???????_?????_?????_000_?????_1100011")
  val BLT     = BitPat("b???????_?????_?????_100_?????_1100011")
  val BGE     = BitPat("b???????_?????_?????_101_?????_1100011")

  val LW      = BitPat("b????????????_?????_010_?????_0000011")
  val SW      = BitPat("b???????_?????_?????_010_?????_0100011")

  val TRAP    = BitPat("b????????????_?????_000_?????_1101011")


  /* decode table */
  val DecodeDefault = List(InstrN, FuAlu, AluAdd)
  val DecodeTable = Array(
    /*                     Instr |  FU  | FU OP |
     *                     Type  | Type |  Type | */
    ADDI           -> List(InstrI, FuAlu, AluAdd),
    SLTIU          -> List(InstrI, FuAlu, AluSltu),
    SLLI           -> List(InstrI, FuAlu, AluSll),
    SRLI           -> List(InstrI, FuAlu, AluSrl),
    ANDI           -> List(InstrI, FuAlu, AluAnd),

    ADD            -> List(InstrR, FuAlu, AluAdd),
    SLT            -> List(InstrR, FuAlu, AluSlt),
    SLTU           -> List(InstrR, FuAlu, AluSltu),
    XOR            -> List(InstrR, FuAlu, AluXor),
    SRL            -> List(InstrR, FuAlu, AluSrl),
    OR             -> List(InstrR, FuAlu, AluOr ),
    SUB            -> List(InstrR, FuAlu, AluSub),
    SRA            -> List(InstrR, FuAlu, AluSra),

    AUIPC          -> List(InstrU, FuAlu, AluAdd),
    LUI            -> List(InstrU, FuAlu, AluLui),

    JAL            -> List(InstrJ, FuBru, BruJal),
    JALR           -> List(InstrI, FuBru, BruJalr),

    BEQ            -> List(InstrB, FuBru, BruBeq),
    BNE            -> List(InstrB, FuBru, BruBne),
    BLT            -> List(InstrB, FuBru, BruBlt),
    BGE            -> List(InstrB, FuBru, BruBge),

    LW             -> List(InstrI, FuLsu, LsuLw),
    SW             -> List(InstrS, FuLsu, LsuSw),

    TRAP           -> List(InstrI, FuAlu, AluAdd)
  )
}
