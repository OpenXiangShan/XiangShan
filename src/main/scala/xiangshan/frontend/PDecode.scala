package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.decode.isa.predecode.PreDecode

object BrType {
  def notBr   = "b000".U
  def jal     = "b001".U
  def jalr    = "b010".U
  def call    = "b011".U
  def branch  = "b100".U
  //def ret    = "b101".U
  def apply() = UInt(3.W)
}

class PDecodeInfo extends XSBundle{  // 8 bit
  val isRVC = Bool()
  val brTpye = UInt(3.W)
  val reserve = UInt(4.W)  // TODO:reserve for exception
}

class CacheLine extends XSBundle {
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
  def brType(instr: UInt) = {
    val res::Nil = ListLookup(instr, List(BrType.notBr), PreDecode.brTable)
    Mux((res === BrType.jal || res === BrType.jalr) && isLink(instr(11,7)) && !isRVC(instr) ,BrType.call, res)
    //judge in bpu: ret - (res === BrType.jalr && isLink(20,16) && !isRVC(instr))
  }

  for(i <- 0 until FetchWidth * 2) {
    preDecodeTemp(i).isRVC := isRVC(cacheInstr(i))
    preDecodeTemp(i).brTpye := brType(cacheInstr(i))
    preDecodeTemp(i).reserve := "b0000".U
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
    XSDebug(io.preDecodeInfo.valid,p"instr ${Binary(cacheInstr(i))} RVC = ${Binary(io.preDecodeInfo.bits(i).isRVC)}, BrType = ${Binary(io.preDecodeInfo.bits(i).brTpye)}, reverse = ${Binary(io.preDecodeInfo.bits(i).reserve)}\n")
  }
}
