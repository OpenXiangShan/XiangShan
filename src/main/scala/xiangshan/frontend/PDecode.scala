package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.decode.isa.predecode.PreDecode

object BrType {
  def notBr   = "b00".U
  def branch  = "b01".U
  def jal     = "b10".U
  def jalr    = "b11".U
  def apply() = UInt(2.W)
}

object ExcType {  //TODO:add exctype
  def notExc = "b000".U
  def apply() = UInt(3.W)
}

class PDecodeInfo extends XSBundle{  // 8 bit
  val isRVC   = Bool()
  val brTpye  = UInt(2.W)
  val isCall  = Bool()
  val isRet   = Bool()
  val excType = UInt(3.W)
}

class CacheLine extends XSBundle {
  val cacheLine = Output(UInt(CacheLineSize.W))
}

//how to load predecode information?
//use PC(6,1) to locate predecode information
class PDecode extends XSModule {
  val io = IO(new Bundle() {
    // CacheLine from L1-PLUS Cache
    val in = Flipped(ValidIO(new CacheLine))
    // CacheLine to L1 Cache
    val out = ValidIO(new CacheLine)
    // preDecodeInfo to L1 Cache
    val preDecodeInfo = ValidIO(Vec(CacheLineHalfWord, new PDecodeInfo))
  })

  val catCacheLine = Cat(0.U(16.W),io.in.bits.cacheLine)    //TODO:add span two Cache-Line
  val cacheInstr = (0 until CacheLineHalfWord).map(i => catCacheLine(i*16+31,i*16))

  val preDecodeTemp = Reg(Vec(CacheLineHalfWord, new PDecodeInfo))
  val cacheLineTemp = Reg((new CacheLine).cacheLine)
  val validLatch = RegInit(false.B)

  def isRVC(instr: UInt) = instr(1,0) =/= "b11".U
  def isLink(reg:UInt) = reg === 1.U || reg === 5.U
  def brInfo(instr: UInt) = {
    val rd = instr(11,7)
    val rs = instr(19,15)
    val res::Nil = ListLookup(instr, List(BrType.notBr), PreDecode.brTable)
    val isCall = (res === BrType.jal || res === BrType.jalr) && isLink(rd) && !isRVC(instr)
    val isRet = res === BrType.jalr && isLink(rs) && !isLink(rd) && !isRVC(instr)
    List(res, isCall, isRet)
  }

  for(i <- 0 until CacheLineHalfWord) {
    val brType::isCall::isRet::Nil = brInfo(cacheInstr(i))
    preDecodeTemp(i).isRVC  := isRVC(cacheInstr(i))
    preDecodeTemp(i).brTpye := brType
    preDecodeTemp(i).isCall := isCall
    preDecodeTemp(i).isRet  := isRet
    preDecodeTemp(i).excType := ExcType.notExc
  }


  validLatch := io.in.valid
  io.preDecodeInfo.bits := preDecodeTemp
  io.preDecodeInfo.valid := validLatch

  cacheLineTemp := io.in.bits.cacheLine
  io.out.valid := validLatch
  io.out.bits.cacheLine := cacheLineTemp

//  XSDebug("cacheinstr:\n")
//  for(i <- 0 until FetchWidth) {
//    XSDebug(io.out.valid, p"${Binary(cacheInstr(i))}\n")
//  }

  for(i <- 0 until CacheLineHalfWord) {
    XSDebug(io.preDecodeInfo.valid,
      p"instr ${Binary(cacheInstr(i))} " +
      p"RVC = ${Binary(io.preDecodeInfo.bits(i).isRVC)}, " +
      p"BrType = ${Binary(io.preDecodeInfo.bits(i).brTpye)}, " +
      p"isCall = ${Binary(io.preDecodeInfo.bits(i).isCall)}, " +
      p"isRet = ${Binary(io.preDecodeInfo.bits(i).isRet)} \n")
  }
}
