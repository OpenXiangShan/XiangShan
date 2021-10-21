package xiangshan.cache

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.frontend._
import utils._
import chipsalliance.rocketchip.config.Parameters
import xiangshan.backend.fu.util.HasCSRConst

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

abstract class CacheCtrlModule(implicit p: Parameters) extends XSModule with HasCSRConst with CacheControlConst

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
    //         offset     width    authority  name
    CacheRegMap("0",      "64",    "RW",      "CACHE_OP"),
    CacheRegMap("1",      "64",    "RW",      "OP_FINISH"),
    CacheRegMap("2",      "64",    "RW",      "CACHE_LEVEL"),
    CacheRegMap("3",      "64",    "RW",      "CACHE_WAY"),
    CacheRegMap("4",      "64",    "RW",      "CACHE_IDX"),
    CacheRegMap("5",      "64",    "RW",      "CACHE_BANK_NUM"),
    CacheRegMap("6",      "64",    "RW",      "CACHE_TAG_ECC"),
    CacheRegMap("7",      "64",    "RW",      "CACHE_TAG_BITS"), // TODO
    CacheRegMap("8",      "64",    "RW",      "CACHE_TAG_LOW"),
    CacheRegMap("9",      "64",    "RW",      "CACHE_TAG_HIGH"), // not used in 64 bit arch
    CacheRegMap("10",     "64",    "RW",      "CACHE_ECC_WIDTH"), // TODO
    CacheRegMap("11",     "64",    "RW",      "CACHE_DATA_ECC"),
    CacheRegMap("12",     "64",    "RW",      "CACHE_DATA_0"),
    CacheRegMap("13",     "64",    "RW",      "CACHE_DATA_1"),
    CacheRegMap("14",     "64",    "RW",      "CACHE_DATA_2"),
    CacheRegMap("15",     "64",    "RW",      "CACHE_DATA_3"),
    CacheRegMap("16",     "64",    "RW",      "CACHE_DATA_4"),
    CacheRegMap("17",     "64",    "RW",      "CACHE_DATA_5"),
    CacheRegMap("18",     "64",    "RW",      "CACHE_DATA_6"),
    CacheRegMap("19",     "64",    "RW",      "CACHE_DATA_7"),
  )

  // Usage:
  // val cacheopMapping = CacheInstrucion.CacheInsRegisterList.map{case (name, attribute) => {
  //   doSthWith(name, attribute("offset"), attribute("width"))
  // }}

  def COP_CHECK = 0.U
  def COP_LOAD  = 1.U
  def COP_STORE = 2.U
  def COP_FLUSH = 3.U

  def COP_ID_ICACHE = 0
  def COP_ID_DCACHE = 1

  def COP_RESULT_CODE_IDLE = 0.U
  def COP_RESULT_CODE_OK = 1.U
  def COP_RESULT_CODE_ERROR = 2.U

  def isReadTagECC(opcode: UInt)            = opcode === "b00000".U
  def isReadDataECC(opcode: UInt)           = opcode === "b00001".U
  def isReadTag(opcode: UInt)               = opcode === "b00010".U
  def isReadData(opcode: UInt)              = opcode === "b00011".U
  def isWriteTagECC(opcode: UInt)           = opcode === "b00100".U
  def isWriteDataECC(opcode: UInt)          = opcode === "b00101".U
  def isWriteTag(opcode: UInt)              = opcode === "b00110".U
  def isWriteData(opcode: UInt)             = opcode === "b00111".U
  def isFlush(opcode: UInt)                 = opcode === "b01000".U

  def isReadOp(opcode: UInt) = isReadTagECC(opcode) ||
    isReadDataECC(opcode) ||
    isReadTag(opcode) ||
    isReadData(opcode)
}

class CacheCtrlReqInfo(implicit p: Parameters) extends XSBundle with CacheControlConst {
  val level           = UInt(XLEN.W) // op target id
  val wayNum          = UInt(XLEN.W)
  val index           = UInt(XLEN.W)
  val opCode          = UInt(XLEN.W)
  val write_tag_high  = UInt(XLEN.W)
  val write_tag_low   = UInt(XLEN.W)
  val write_tag_ecc   = UInt(XLEN.W)
  val write_data_vec  = Vec(maxDataRowSupport, UInt(XLEN.W))
  val write_data_ecc  = UInt(XLEN.W)
  val bank_num         = UInt(XLEN.W)
}

class CacheCtrlRespInfo(implicit p: Parameters) extends XSBundle with HasICacheParameters with CacheControlConst{
  val read_tag_high  = UInt(XLEN.W)
  val read_tag_low   = UInt(XLEN.W)
  val read_tag_ecc   = UInt(XLEN.W)
  val read_data_vec  = Vec(maxDataRowSupport, UInt(XLEN.W))
  val read_data_ecc  = UInt(XLEN.W)
  val bank_num        = UInt(XLEN.W)
}

class L1CacheToCsrIO(implicit p: Parameters) extends DCacheBundle {
  val distribute_csr = Flipped(new DistributedCSRIO)
  val update = new DistributedCSRUpdateReq
}

class DCacheInnerOpIO(implicit p: Parameters) extends DCacheBundle {
  val req  = Valid(new CacheCtrlReqInfo)
  val resp = Flipped(Valid(new CacheCtrlRespInfo))
}

class CSRCacheOpDecoder(decoder_name: String, id: Int)(implicit p: Parameters) extends CacheCtrlModule {
  val io = IO(new Bundle {
    val csr = new L1CacheToCsrIO
    val cache = new DCacheInnerOpIO
  })

  // CSRCacheOpDecoder state
  val w_csr_op_req = RegInit(true.B) // waiting for csr "CACHE_OP" being write  
  val w_cache_op_resp = RegInit(false.B) // waiting for dcache to finish dcache op
  val s_csr_op_resp_data = RegInit(false.B) // ready to write data readed from cache back to csr  
  val s_csr_op_resp_finish = RegInit(false.B) // ready to write "OP_FINISH" csr 
  // val cache_op_resp_timer = RegInit(0.U(4.W))
  val data_transfer_finished = WireInit(false.B)
  val data_transfer_cnt = RegInit(0.U(log2Up(maxDataRowSupport).W))

  // Translate CSR write to cache op
  val translated_cache_req = Reg(new CacheCtrlReqInfo)
  println("Cache op decoder (" + decoder_name + "):")
  println("  Id " + id)
  // CacheInsRegisterList.map{case (name, attribute) => {
  //   println("  Register CSR mirror " + name)
  // }}

  def cacheop_csr_is_being_write(csr_name: String): Bool = {
    io.csr.distribute_csr.w.bits.addr === (CacheInstrucion.CacheInsRegisterList(csr_name)("offset").toInt + Scachebase).U &&
      io.csr.distribute_csr.w.valid
  }

  def update_cache_req_when_write(csr_name: String, req_field: Data) = {
    when(
      cacheop_csr_is_being_write(csr_name)
    ){
      req_field := io.csr.distribute_csr.w.bits.data
      assert(w_csr_op_req)
    }
  }

  update_cache_req_when_write("CACHE_OP", translated_cache_req.opCode)
  update_cache_req_when_write("CACHE_LEVEL", translated_cache_req.level)
  update_cache_req_when_write("CACHE_WAY", translated_cache_req.wayNum)
  update_cache_req_when_write("CACHE_IDX", translated_cache_req.index)
  update_cache_req_when_write("CACHE_BANK_NUM", translated_cache_req.bank_num)
  update_cache_req_when_write("CACHE_TAG_HIGH", translated_cache_req.write_tag_high)
  update_cache_req_when_write("CACHE_TAG_LOW", translated_cache_req.write_tag_low)
  update_cache_req_when_write("CACHE_DATA_ECC", translated_cache_req.write_tag_ecc)
  update_cache_req_when_write("CACHE_DATA_0", translated_cache_req.write_data_vec(0))
  update_cache_req_when_write("CACHE_DATA_1", translated_cache_req.write_data_vec(1))
  update_cache_req_when_write("CACHE_DATA_2", translated_cache_req.write_data_vec(2))
  update_cache_req_when_write("CACHE_DATA_3", translated_cache_req.write_data_vec(3))
  update_cache_req_when_write("CACHE_DATA_4", translated_cache_req.write_data_vec(4))
  update_cache_req_when_write("CACHE_DATA_5", translated_cache_req.write_data_vec(5))
  update_cache_req_when_write("CACHE_DATA_6", translated_cache_req.write_data_vec(6))
  update_cache_req_when_write("CACHE_DATA_7", translated_cache_req.write_data_vec(7))
  update_cache_req_when_write("CACHE_DATA_ECC", translated_cache_req.write_data_ecc)

  val cache_op_start = WireInit(cacheop_csr_is_being_write("CACHE_OP") && id.U === translated_cache_req.level)
  when(cache_op_start) {
    w_csr_op_req := false.B
  }

  // Send cache op to cache
  io.cache.req.valid := RegNext(cache_op_start)
  io.cache.req.bits := translated_cache_req
  when(io.cache.req.fire()){
    w_cache_op_resp := true.B
  }

  // Receive cache op resp from cache
  val raw_cache_resp = Reg(new CacheCtrlRespInfo)
  when(io.cache.resp.fire()){
    w_cache_op_resp := false.B
    raw_cache_resp := io.cache.resp.bits
    when(CacheInstrucion.isReadOp(translated_cache_req.opCode)){
      s_csr_op_resp_data := true.B
      s_csr_op_resp_finish := false.B
      assert(data_transfer_cnt === 0.U)
    }.otherwise{
      s_csr_op_resp_data := false.B
      s_csr_op_resp_finish := true.B
    }
  }

  // Translate cache op resp to CSR write, send it back to CSR
  when(io.csr.update.w.fire() && s_csr_op_resp_data && data_transfer_finished){
    s_csr_op_resp_data := false.B
    s_csr_op_resp_finish := true.B
  }
  when(io.csr.update.w.fire() && s_csr_op_resp_finish){
    s_csr_op_resp_finish := false.B
    w_csr_op_req := true.B
  }

  io.csr.update.w.valid := s_csr_op_resp_data || s_csr_op_resp_finish
  io.csr.update.w.bits := DontCare

  val isReadTagECC = WireInit(CacheInstrucion.isReadTagECC(translated_cache_req.opCode))
  val isReadDataECC = WireInit(CacheInstrucion.isReadDataECC(translated_cache_req.opCode))
  val isReadTag = WireInit(CacheInstrucion.isReadTag(translated_cache_req.opCode))
  val isReadData = WireInit(CacheInstrucion.isReadData(translated_cache_req.opCode))

  when(s_csr_op_resp_data){
    io.csr.update.w.bits.addr := Mux1H(List(
      isReadTagECC -> (CacheInstrucion.CacheInsRegisterList("CACHE_TAG_ECC")("offset").toInt + Scachebase).U,
      isReadDataECC -> (CacheInstrucion.CacheInsRegisterList("CACHE_BANK_NUM")("offset").toInt + Scachebase).U,
      isReadTag -> ((CacheInstrucion.CacheInsRegisterList("CACHE_TAG_LOW")("offset").toInt + Scachebase).U + data_transfer_cnt),
      isReadData -> ((CacheInstrucion.CacheInsRegisterList("CACHE_DATA_0")("offset").toInt + Scachebase).U + data_transfer_cnt), 
    ))
    io.csr.update.w.bits.data := Mux1H(List(
      isReadTagECC -> raw_cache_resp.read_tag_ecc,
      isReadDataECC -> raw_cache_resp.read_tag_ecc,
      isReadTag -> raw_cache_resp.read_tag_low,
      isReadData -> raw_cache_resp.read_data_vec(data_transfer_cnt),
    ))
    data_transfer_finished := Mux(isReadData(translated_cache_req.opCode),
      data_transfer_cnt === (maxDataRowSupport-1).U,
      true.B
    )
    data_transfer_cnt := data_transfer_cnt + 1.U
  }

  when(s_csr_op_resp_finish){
    io.csr.update.w.bits.addr := (CacheInstrucion.CacheInsRegisterList("OP_FINISH")("offset").toInt + Scachebase).U
    io.csr.update.w.bits.data := CacheInstrucion.COP_RESULT_CODE_OK
    data_transfer_cnt := 0.U
  }
}
