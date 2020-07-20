package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.decode.isa.predecode.PreDecode

object BrType {
  def notBr   = "b00".U
  def jal     = "b01".U
  def jalr    = "b10".U
  def branch  = "b11".U
  def apply() = UInt(2.W)
}

class PDecodeInfo extends XSBundle{  // 8 bit
  val isRVC   = Bool()
  val brTpye  = UInt(2.W)
  val isCall  = Bool()
  val isRet   = Bool()
  val reserve = UInt(3.W)  // TODO:reserve for exception
}

class CacheLine extends XSBundle {  //TODO:simplebusUC
  val cacheLine = Output(UInt((FetchWidth * 32).W))
}


class PDecode extends XSModule {
  val io = IO(new Bundle() {
    // CacheLine from L1-PLUS Cachegi
    val in = Flipped(ValidIO(new CacheLine))
    // CacheLine to L1 Cache
    val out = ValidIO(new CacheLine)
    // preDecodeInfo to L1 Cache
    val preDecodeInfo = ValidIO(Vec(FetchWidth * 2, new PDecodeInfo))
  })

  val cacheInstr = (0 until FetchWidth * 2).map(i => io.in.bits.cacheLine(i*16+15,i*16))

  val preDecodeTemp = Reg(Vec(FetchWidth * 2, new PDecodeInfo))
  val cacheLineTemp = Reg((new CacheLine).cacheLine)
  val validLatch = RegInit(false.B)

  def isRVC(instr: UInt) = instr(1,0) =/= "b11".U
  def isLink(reg:UInt) = reg === 1.U || reg === 5.U
  def brInfo(instr: UInt) = {
    val isCall = WireInit(false.B)
    val isRet = WireInit(false.B)  //TODO:add retInfo
    val rd = instr(11,7)
    val res::Nil = ListLookup(instr, List(BrType.notBr), PreDecode.brTable)
    when((res === BrType.jal || res === BrType.jalr) && isLink(rd) && !isRVC(instr)) { isCall := true.B }
    List(res, isCall, isRet)
  }

  for(i <- 0 until FetchWidth * 2) {
    val brType::isCall::isRet::Nil = brInfo(cacheInstr(i))
    preDecodeTemp(i).isRVC  := isRVC(cacheInstr(i))
    preDecodeTemp(i).brTpye := brType
    preDecodeTemp(i).isCall := isCall
    preDecodeTemp(i).isRet  := isRet
    preDecodeTemp(i).reserve := "b000".U
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

  for(i <- 0 until 2 * FetchWidth) {
    XSDebug(io.preDecodeInfo.valid,
      p"instr ${Binary(cacheInstr(i))} " +
      p"RVC = ${Binary(io.preDecodeInfo.bits(i).isRVC)}, " +
      p"BrType = ${Binary(io.preDecodeInfo.bits(i).brTpye)}, " +
      p"isCall = ${Binary(io.preDecodeInfo.bits(i).isCall)}, " +
      p"isRet = ${Binary(io.preDecodeInfo.bits(i).isRet)} \n")
  }
}
