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
  val vsetiwf = OHInt(10) // vset read rs write vconfig
  val vsetfwf = OHInt(11) // vset read old vl write vconfig
  val ldu = OHInt(12)
  val stu = OHInt(13)
  val mou = OHInt(14)

  val vipu = OHInt(15)
  val vialuF = OHInt(16)
  val vimac = OHInt(17)
  val vldu = OHInt(18)
  val vstu = OHInt(19)
  val vppu = OHInt(20)
  val vfalu = OHInt(21)
  val vfma  = OHInt(22)
  val vfdiv = OHInt(23)
  val vfcvt = OHInt(24)

  def X = BitPat.N(num) // Todo: Don't Care

  def num = 25

  def width = num

  def apply() = UInt(num.W)

  def isInt(fuType: UInt): Bool = fuType(10, 0).orR // from jmp to vset

  def isVset(fuType: UInt): Bool = fuType(11, 9).orR

  def isJump(fuType: UInt): Bool = fuType(0)

  def isFp(fuType: UInt): Bool = fuType(24, 21).orR

  def isMem(fuType: UInt): Bool = fuType(14, 12).orR

  def isLoadStore(fuType: UInt): Bool = fuType(13, 12).orR

  def isLoad(fuType: UInt): Bool = fuType(12)

  def isStore(fuType: UInt): Bool = fuType(13).orR

  def isAMO(fuType: UInt): Bool = fuType(14).orR

  def isFence(fuType: UInt): Bool = fuType(7)

  def isVpu(fuType: UInt): Bool = fuType(17, 15).orR || fuType(20)

  def isVls(fuType: UInt): Bool = fuType(19, 18).orR

  def isVLoad(fuType: UInt): Bool = fuType(18)

  def isVStore(fuType: UInt): Bool = fuType(19)

  def isVfp(fuType: UInt): Bool = fuType(24, 21).orR // scalar & vector float

  def isVector(fuType: UInt): Bool = fuType(24, 15).orR

  def storeIsAMO(fuType: UInt): Bool = fuType(14)

  val functionNameMap = Map(
    jmp -> "jmp",
    i2f -> "int_to_float",
    csr -> "csr",
    alu -> "alu",
    mul -> "mul",
    div -> "div",
    fence -> "fence",
    bku -> "bku",
    ldu -> "load",
    stu -> "store",
    mou -> "mou",
    vsetiwi -> "vsetiwi",
    vsetiwf -> "vsetiwf",
    vsetfwf -> "vsetfwf",
    vipu -> "vipu",
    vialuF -> "vialuF",
    vldu -> "vldu",
    vstu -> "vstu",
    vppu -> "vppu",
    vimac -> "vimac",
    vfalu -> "vfalu",
    vfma -> "vfma",
    vfdiv -> "vfdiv",
    vfcvt -> "vfcvt"
  )
}

