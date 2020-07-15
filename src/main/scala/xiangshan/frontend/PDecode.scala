package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

object BrType {
  def NOTBR   = "b000".U(3.W)
  def JAL     = "b001".U(3.W)
  def JALR    = "b010".U(3.W)
  def BRANCH  = "b011".U(3.W)
  def CALL    = "b100".U(3.W)
  def RET     = "b101".U(3.W)

}

object RV32I_BrInstr {
  def JAL     = BitPat("b????????????????????_?????_1101111")
  def JALR    = BitPat("b????????????_?????_000_?????_1100111")
  def BRANCH  = BitPat("b???????_?????_?????_???_?????_1100011")
  def CALL1   = BitPat("b????????????????????_???01_1101111")     //(JALR || JAL) && (rd === 1 || rd === 5)
  def CALL2   = BitPat("b?????????????????_000_???01_1100111")
  def RET     = BitPat("b????????????_???01_000_?????_1100111")   //JALR && (rs === 1 || rs === 5)
}

object RVC_BrInstr {
  def JAL     = BitPat("b????????????????_?01_?_??_???_??_???_01")
  def JALR    = BitPat("b????????????????_100_?_??_???_00_000_10")
  def BRANCH    = BitPat("b????????????????_11?_?_??_???_??_???_01")
}

object SbbInstr { //for loopbuffer
  def SBB_JAL = BitPat("b1111_???????_111111111_?????_1101111")
  def SBB_BRANCH= BitPat("b111111?_?????_?????_???_????1_1100011")
}

class PDecodeInfo extends XSBundle{  // 8 bit
  val isRVC = Bool()
  val isSBB = Bool()  // to loopbuffer
  val brTpye = UInt(3.W)
  val reserve = UInt(3.W)  // TODO:reserve for exception
}

class CacheLine extends XSBundle {
  val cacheLine = Output(UInt((FetchWidth * 32).W))
}


class PDecode extends XSModule {
  val io = IO(new Bundle() {
    // CacheLine from L1-PLUS Cachegi
    val in = Flipped(ValidIO(new CacheLine))  //TODO:consider non-aligned
    // CacheLine to L1 Cache
    val out = ValidIO(new CacheLine)
    // preDecodeInfo to L1 Cache
    val preDecodeInfo = ValidIO(Vec(FetchWidth, new PDecodeInfo))
  })

  val cacheInstr = (0 until FetchWidth).map(i => io.in.bits.cacheLine(i * 32 + 31, i * 32))
  val preDecodeInfo = Reg(Vec(FetchWidth, new PDecodeInfo))
  val cacheLineTemp = Reg((new CacheLine).cacheLine)
  val validLatch = RegInit(false.B)


  def isRVC(low2bit: UInt) = low2bit =/= "b11".U        // use to instruction split
  def isSBB(instr: UInt) = { instr === SbbInstr.SBB_BRANCH || instr === SbbInstr.SBB_JAL }  // to loopbuffer
  def brType(instr: UInt) = {                          // to bpu for choosing prediction algorithm
    val instRVC = isRVC(instr(1,0))
    val r = WireInit(BrType.NOTBR)

    when(instRVC){
      when(instr === RVC_BrInstr.JAL) { r := BrType.JAL }  //C_ADDIW?
      when(instr === RVC_BrInstr.JALR) { r := BrType.JALR }
      when(instr === RVC_BrInstr.BRANCH) { r := BrType.BRANCH }
    }
    when(!instRVC){
      when(instr === RV32I_BrInstr.JAL) { r := BrType.JAL }
      when(instr === RV32I_BrInstr.JALR) { r := BrType.JALR }
      when(instr === RV32I_BrInstr.BRANCH) { r := BrType.BRANCH }
      when(instr === RV32I_BrInstr.RET) { r := BrType.RET }
      when(instr === RV32I_BrInstr.CALL1 || instr === RV32I_BrInstr.CALL2) { r := BrType.CALL }
    }
    r
  }

  for(i <- 0 until FetchWidth) {
    preDecodeInfo(i).isRVC := isRVC(cacheInstr(i)(1, 0))
    preDecodeInfo(i).isSBB := isSBB(cacheInstr(i))
    preDecodeInfo(i).brTpye := brType(cacheInstr(i))
    preDecodeInfo(i).reserve := "b0000".U
  }


  validLatch := io.in.valid
  io.preDecodeInfo.bits := preDecodeInfo
  io.preDecodeInfo.valid := validLatch

  cacheLineTemp := io.in.bits.cacheLine
  io.out.valid := validLatch
  io.out.bits.cacheLine := cacheLineTemp

//  XSDebug("cacheinstr:\n")
//  for(i <- 0 until FetchWidth) {
//    XSDebug(io.out.valid, p"${Binary(cacheInstr(i))}\n")
//  }

  for(i <- 0 until FetchWidth) {
    XSDebug(io.preDecodeInfo.valid,p"RVC = ${Binary(io.preDecodeInfo.bits(i).isRVC)}, BrType = ${Binary(io.preDecodeInfo.bits(i).brTpye)}, reverse = ${Binary(io.preDecodeInfo.bits(i).reserve)}\n")
  }
  XSDebug(io.out.valid, p"to L1 Cache = ${Binary(io.out.bits.cacheLine)}")
}
