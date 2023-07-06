package xiangshan.backend.fu

import chisel3._
import chisel3.util.BitPat

object FuType {
  private def OHInt(n: Int) = {
    require(n < 63)
    1 << n
  }
  val jmp = OHInt(0)
  val brh = OHInt(1)
  val i2f = OHInt(2)
  val csr = OHInt(3)
  val alu = OHInt(4)
  val mul = OHInt(5)
  val div = OHInt(6)
  val fence = OHInt(7)
  val bku = OHInt(8)
  val vsetiwi = OHInt(9) // vset read rs write rd
  val fmac = OHInt(10)
  val fmisc = OHInt(11)
  val fDivSqrt = OHInt(12)
  val ldu = OHInt(13)
  val stu = OHInt(14)
  val mou = OHInt(15)
  val vipu = OHInt(16)
  val vialuF = OHInt(17)
  val vfpu = OHInt(18) // will be delated
  val vldu = OHInt(19)
  val vstu = OHInt(20)
  val vppu = OHInt(21)
  val vsetiwf = OHInt(22) // vset read rs write vconfig
  val vsetfwf = OHInt(23) // vset read old vl write vconfig
  val vimac = OHInt(24)
  val vfalu = OHInt(25)
  val vfma  = OHInt(26)
  val vfdiv = OHInt(27) // Todo

  def X = BitPat.N(num) // Todo: Don't Care

  def num = 28

  def width = num

  def apply() = UInt(num.W)

  def isInt(fuType: UInt): Bool = fuType(9, 0).orR || fuType(22)// from jmp to vset

  def isVset(fuType: UInt): Bool = fuType(9) || fuType(22) || fuType(23)

  def isJump(fuType: UInt): Bool = fuType(0)

  def isFp(fuType: UInt): Bool = fuType(12, 10).orR || fuType(25) || fuType(26) || fuType(27)

  def isMem(fuType: UInt): Bool = fuType(15, 13).orR

  def isLoadStore(fuType: UInt): Bool = fuType(14, 13).orR

  def isLoad(fuType: UInt): Bool = fuType(13)

  def isStore(fuType: UInt): Bool = fuType(14).orR

  def isAMO(fuType: UInt): Bool = fuType(15).orR

  def isFence(fuType: UInt): Bool = fuType(7)

  def isVpu(fuType: UInt): Bool = fuType(18, 16).orR || fuType(21) || fuType(24)

  def isVls(fuType: UInt): Bool = fuType(20, 19).orR

  def isVLoad(fuType: UInt): Bool = fuType(19)

  def isVStore(fuType: UInt): Bool = fuType(20)

  def storeIsAMO(fuType: UInt): Bool = fuType(15)

  val functionNameMap = Map(
    jmp -> "jmp",
    i2f -> "int_to_float",
    csr -> "csr",
    alu -> "alu",
    mul -> "mul",
    div -> "div",
    fence -> "fence",
    bku -> "bku",
    fmac -> "fmac",
    fmisc -> "fmisc",
    fDivSqrt -> "fdiv_fsqrt",
    ldu -> "load",
    stu -> "store",
    mou -> "mou"
  )
}

