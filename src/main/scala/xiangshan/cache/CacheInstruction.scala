package xiangshan.cache

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.frontend._
import utils._
import chipsalliance.rocketchip.config.Parameters

object CacheOpMap{
  def apply(opcode: String, optype: String,  name: String ): Map[String, String] = {
    Map(
      "opcode" -> opcode,
      "optype" -> optype,
      "name"   -> name,
    )
  }
}

object CacheRegMap{ 
  def apply(offset: String,  width: String, authority: String, name: String ): Pair[String, Map[String, String]] = {
    name -> Map(
      "offset" -> offset,
      "width"  -> width,
      "authority" -> authority,
    )
  }
}

trait CacheControlConst{
  def maxDataRowSupport = 8
}

object CacheInstrucion{
  def CacheOperation = List(
    CacheOpMap("b00000", "CHECK",  "READ_TAG_ECC"),
    CacheOpMap("b00001", "CHECK",  "READ_DATA_ECC"),
    CacheOpMap("b00010", "LOAD",   "READ_TAG"),
    CacheOpMap("b00011", "LOAD",   "READ_DATA"),
    CacheOpMap("b00100", "STORE",  "WRITE_TAG_ECC"),
    CacheOpMap("b00101", "STORE",  "WRITE_DATA_ECC"),
    CacheOpMap("b00110", "STORE",  "WRITE_TAG"),
    CacheOpMap("b00111", "STORE",  "WRITE_DATA"),
    CacheOpMap("b01000", "FLUSH",  "FLUSH_BLOCK")
  )

  def CacheInsRegisterList = Map(
    /**         offset    width     authority   name */
    CacheRegMap("0",      "64",     "RW",       "CACHE_LEVEL"),
    CacheRegMap("1",      "64",     "RW",       "CACHE_WAY"),
    CacheRegMap("2",      "64",     "RW",       "CACHE_IDX"),
    CacheRegMap("3",      "64",     "RW",       "CACHE_TAG_ECC"),
    CacheRegMap("4",      "64",     "RW",       "CACHE_TAG_BITS"),
    CacheRegMap("5",      "64",     "RW",       "CACHE_TAG_LOW"),
    CacheRegMap("6",      "64",     "RW",       "CACHE_TAG_HIGH"),
    CacheRegMap("7",      "64",     "R",        "CACHE_ECC_WIDTH"),
    CacheRegMap("8",      "64",     "RW",       "CACHE_ECC_NUM"),
    CacheRegMap("9",      "64",     "RW",       "CACHE_DATA_ECC"),
    CacheRegMap("10",     "64",     "RW",       "CACHE_DATA_0"),
    CacheRegMap("11",     "64",     "RW",       "CACHE_DATA_1"),
    CacheRegMap("12",     "64",     "RW",       "CACHE_DATA_2"),
    CacheRegMap("13",     "64",     "RW",       "CACHE_DATA_3"),
    CacheRegMap("14",     "64",     "RW",       "CACHE_DATA_4"),
    CacheRegMap("15",     "64",     "RW",       "CACHE_DATA_5"),
    CacheRegMap("16",     "64",     "RW",       "CACHE_DATA_6"),
    CacheRegMap("17",     "64",     "RW",       "CACHE_DATA_7"),
    CacheRegMap("18",     "64",     "R",        "OP_FINISH") ,
    CacheRegMap("19",     "64",     "RW",       "CACHE_OP")
  )

  def COP_CHECK = 0.U
  def COP_LOAD  = 1.U
  def COP_STORE = 2.U
  def COP_FLUSH = 3.U

  def isReadTagECC(opcode: UInt)            = opcode === "b00000".U
  def isReadDataECC(opcode: UInt)           = opcode === "b00001".U
  def isReadTag(opcode: UInt)               = opcode === "b00010".U
  def isReadData(opcode: UInt)              = opcode === "b00011".U
  def isWriteTagECC(opcode: UInt)           = opcode === "b00101".U
  def isWriteTag(opcode: UInt)              = opcode === "b00110".U
  def isWriteData(opcode: UInt)             = opcode === "b00111".U
  def isFlush(opcode: UInt)                 = opcode === "b01000".U
}

class CacheCtrlReqInfo(implicit p: Parameters) extends XSBundle with CacheControlConst {
  val wayNum          = UInt(XLEN.W)
  val index           = UInt(XLEN.W)
  val opCode          = UInt(XLEN.W)
  val write_tag_high  = UInt(XLEN.W)
  val write_tag_low   = UInt(XLEN.W)
  val write_tag_ecc   = UInt(XLEN.W)
  val write_data_vec  = Vec(maxDataRowSupport, UInt(XLEN.W))
  val write_data_ecc  = UInt(XLEN.W)
  val ecc_num         = UInt(XLEN.W)
}

class CacheCtrlRespInfo(implicit p: Parameters) extends XSBundle with HasICacheParameters with CacheControlConst{
  val read_tag_high  = UInt(XLEN.W)
  val read_tag_low   = UInt(XLEN.W)
  val read_tag_ecc   = UInt(XLEN.W)
  val read_data_vec  = Vec(maxDataRowSupport, UInt(XLEN.W))
  val read_data_ecc  = UInt(XLEN.W)
  val ecc_num        = UInt(XLEN.W)
}


// class CSRCacheInsIO(implicit p: Parameters) extends XSModule{
//   // TODO: extend pmp cfg interface
// }
