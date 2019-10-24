package noop

import chisel3._
import chisel3.util._

object RVAInstr extends HasInstrType {
  // Note: use instr(14,12) to distinguish D/W inst
  def LR      = BitPat("b00010??00000_?????_???_?????_0101111")
  def SC      = BitPat("b00011??00000_?????_???_?????_0101111")

  def AMOSWAP = BitPat("b00001??00000_?????_01?_?????_0101111")
  def AMOADD  = BitPat("b00000??00000_?????_01?_?????_0101111")
  def AMOXOR  = BitPat("b00100??00000_?????_01?_?????_0101111")
  def AMOAND  = BitPat("b01100??00000_?????_01?_?????_0101111")
  def AMOOR   = BitPat("b01000??00000_?????_01?_?????_0101111")
  def AMOMIN  = BitPat("b10000??00000_?????_01?_?????_0101111")
  def AMOMAX  = BitPat("b10100??00000_?????_01?_?????_0101111")
  def AMOMINU = BitPat("b11000??00000_?????_01?_?????_0101111")
  def AMOMAXU = BitPat("b11100??00000_?????_01?_?????_0101111")
  // funct3 === 010 or 011
  
  val table = Array(
    LR          -> List(InstrI, FuType.lsu, LSUOpType.lr),
    SC          -> List(InstrS, FuType.lsu, LSUOpType.sc),
    AMOSWAP     -> List(InstrR, FuType.lsu, LSUOpType.amoswap),
    AMOADD      -> List(InstrR, FuType.lsu, LSUOpType.amoadd),
    AMOXOR      -> List(InstrR, FuType.lsu, LSUOpType.amoxor),
    AMOAND      -> List(InstrR, FuType.lsu, LSUOpType.amoand),
    AMOOR       -> List(InstrR, FuType.lsu, LSUOpType.amoor),
    AMOMIN      -> List(InstrR, FuType.lsu, LSUOpType.amomin),
    AMOMAX      -> List(InstrR, FuType.lsu, LSUOpType.amomax),
    AMOMINU     -> List(InstrR, FuType.lsu, LSUOpType.amominu),
    AMOMAXU     -> List(InstrR, FuType.lsu, LSUOpType.amomaxu)
  )
}
