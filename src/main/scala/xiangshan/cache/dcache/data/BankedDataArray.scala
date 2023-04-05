/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import utils._
import utility._
import chisel3.util._
import freechips.rocketchip.tilelink.{ClientMetadata, TLClientParameters, TLEdgeOut}
import xiangshan.{L1CacheErrorInfo, XSCoreParamsKey}

import scala.math.max

class BankConflictDB(implicit p: Parameters) extends DCacheBundle{
  val addr = Vec(LoadPipelineWidth, Bits(PAddrBits.W))
  val set_index = Vec(LoadPipelineWidth, UInt((DCacheAboveIndexOffset - DCacheSetOffset).W))
  val bank_index = UInt((DCacheSetOffset - DCacheBankOffset).W)
  val way_index = UInt(wayBits.W)
  val fake_rr_bank_conflict = Bool()
}

class L1BankedDataReadReq(implicit p: Parameters) extends DCacheBundle
{
  val way_en = Bits(DCacheWays.W)
  val addr = Bits(PAddrBits.W)
}

class L1BankedDataReadLineReq(implicit p: Parameters) extends L1BankedDataReadReq
{
  val rmask = Bits(DCacheBanks.W)
}

// Now, we can write a cache-block in a single cycle
class L1BankedDataWriteReq(implicit p: Parameters) extends L1BankedDataReadReq
{
  val wmask = Bits(DCacheBanks.W)
  val data = Vec(DCacheBanks, Bits(DCacheSRAMRowBits.W))
}

// cache-block write request without data
class L1BankedDataWriteReqCtrl(implicit p: Parameters) extends L1BankedDataReadReq

class L1BankedDataReadResult(implicit p: Parameters) extends DCacheBundle
{
  // you can choose which bank to read to save power
  val ecc = Bits(eccBits.W)
  val raw_data = Bits(DCacheSRAMRowBits.W)
  val error_delayed = Bool() // 1 cycle later than data resp

  def asECCData() = {
    Cat(ecc, raw_data)
  }
}

class DataSRAMBankWriteReq(implicit p: Parameters) extends DCacheBundle {
  val en = Bool()
  val addr = UInt()
  val way_en = UInt(DCacheWays.W)
  val data = UInt(DCacheSRAMRowBits.W)
}

//                     Banked DCache Data
// -----------------------------------------------------------------
// | Bank0 | Bank1 | Bank2 | Bank3 | Bank4 | Bank5 | Bank6 | Bank7 |
// -----------------------------------------------------------------
// | Way0  | Way0  | Way0  | Way0  | Way0  | Way0  | Way0  | Way0  |
// | Way1  | Way1  | Way1  | Way1  | Way1  | Way1  | Way1  | Way1  |
// | ....  | ....  | ....  | ....  | ....  | ....  | ....  | ....  |
// -----------------------------------------------------------------
abstract class AbstractBankedDataArray(implicit p: Parameters) extends DCacheModule
{
  val ReadlinePortErrorIndex = LoadPipelineWidth
  val io = IO(new DCacheBundle {
    // load pipeline read word req
    val read = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new L1BankedDataReadReq)))
    // main pipeline read / write line req
    val readline_intend = Input(Bool())
    val readline = Flipped(DecoupledIO(new L1BankedDataReadLineReq))
    val write = Flipped(DecoupledIO(new L1BankedDataWriteReq))
    val write_dup = Vec(DCacheBanks, Flipped(Decoupled(new L1BankedDataWriteReqCtrl)))
    // data for readline and loadpipe
    val readline_resp = Output(Vec(DCacheBanks, new L1BankedDataReadResult()))
    val readline_error_delayed = Output(Bool())
    val read_resp_delayed = Output(Vec(LoadPipelineWidth, new L1BankedDataReadResult()))
    val read_error_delayed = Output(Vec(LoadPipelineWidth, Bool()))
    // val nacks = Output(Vec(LoadPipelineWidth, Bool()))
    // val errors = Output(Vec(LoadPipelineWidth + 1, new L1CacheErrorInfo)) // read ports + readline port
    // when bank_conflict, read (1) port should be ignored
    val bank_conflict_slow = Output(Vec(LoadPipelineWidth, Bool()))
    val bank_conflict_fast = Output(Vec(LoadPipelineWidth, Bool()))
    val disable_ld_fast_wakeup = Output(Vec(LoadPipelineWidth, Bool()))
    // customized cache op port 
    val cacheOp = Flipped(new L1CacheInnerOpIO)
    val cacheOp_req_dup = Vec(11, Flipped(Valid(new CacheCtrlReqInfo)))
    val cacheOp_req_bits_opCode_dup = Input(Vec(11, UInt(XLEN.W)))
  })

  def pipeMap[T <: Data](f: Int => T) = VecInit((0 until LoadPipelineWidth).map(f))

  def dumpRead() = {
    (0 until LoadPipelineWidth) map { w =>
      when(io.read(w).valid) {
        XSDebug(s"DataArray Read channel: $w valid way_en: %x addr: %x\n",
          io.read(w).bits.way_en, io.read(w).bits.addr)
      }
    }
    when(io.readline.valid) {
      XSDebug(s"DataArray Read Line, valid way_en: %x addr: %x rmask %x\n",
        io.readline.bits.way_en, io.readline.bits.addr, io.readline.bits.rmask)
    }
  }

  def dumpWrite() = {
    when(io.write.valid) {
      XSDebug(s"DataArray Write valid way_en: %x addr: %x\n",
        io.write.bits.way_en, io.write.bits.addr)

      (0 until DCacheBanks) map { r =>
        XSDebug(s"cycle: $r data: %x wmask: %x\n",
          io.write.bits.data(r), io.write.bits.wmask(r))
      }
    }
  }

  def dumpResp() = {
    XSDebug(s"DataArray ReadeResp channel:\n")
    (0 until LoadPipelineWidth) map { r =>
      XSDebug(s"cycle: $r data: %x\n", io.read_resp_delayed(r).raw_data)
    }
  }

  def dump() = {
    dumpRead
    dumpWrite
    dumpResp
  }
}

class BankedDataArray(implicit p: Parameters) extends AbstractBankedDataArray {
  def getECCFromEncWord(encWord: UInt) = {
    require(encWord.getWidth == encWordBits)
    encWord(encWordBits - 1, wordBits)
  }

  val ReduceReadlineConflict = false

  io.write.ready := true.B
  io.write_dup.foreach(_.ready := true.B)

  // wrap a sram
  class DataSRAM(bankIdx:Int, wayIdx:Int) extends Module {
    val io = IO(new Bundle() {
      val w = new Bundle() {
        val en = Input(Bool())
        val addr = Input(UInt())
        val data = Input(UInt(DCacheSRAMRowBits.W))
      }

      val r = new Bundle() {
        val en = Input(Bool())
        val addr = Input(UInt())
        val data = Output(UInt(DCacheSRAMRowBits.W))
      }
    })

    // data sram
    val data_sram = Module(new SRAMTemplate(
      Bits(DCacheSRAMRowBits.W),
      set = DCacheSets,
      way = 1,
      shouldReset = false,
      holdRead = false,
      singlePort = true
    ))

    val wenReg = RegNext(io.w.en)
    val waddrReg = RegNext(io.w.addr)
    val wdataReg = RegNext(io.w.data)
    data_sram.io.w.req.valid := wenReg
    data_sram.io.w.req.bits.apply(
      setIdx = waddrReg,
      data = wdataReg,
      waymask = 1.U
    )
    data_sram.io.r.req.valid := io.r.en
    data_sram.io.r.req.bits.apply(setIdx = io.r.addr)
    io.r.data := data_sram.io.r.resp.data(0)
    XSPerfAccumulate("data_sram_read_counter", data_sram.io.r.req.valid)

    def dump_r() = {
      when(RegNext(io.r.en)) {
        XSDebug("bank read set %x bank %x way %x data %x\n",
          RegNext(io.r.addr),
          bankIdx.U,
          wayIdx.U,
          io.r.data
        )
      }
    }

    def dump_w() = {
      when(io.w.en) {
        XSDebug("bank write set %x bank %x way %x data %x\n",
          io.w.addr,
          bankIdx.U,
          wayIdx.U,
          io.w.data
        )
      }
    }

    def dump() = {
      dump_w()
      dump_r()
    }
  }

  val data_banks = List.tabulate(DCacheBanks)(i => List.tabulate(DCacheWays)(j => Module(new DataSRAM(i,j))))
  // ecc_banks also needs to be changed to two-dimensional to align with data_banks
  val ecc_banks = List.tabulate(DCacheBanks)(i => List.tabulate(DCacheWays)(j => Module(new SRAMTemplate(
    Bits(eccBits.W),
    set = DCacheSets,
    way = 1,
    shouldReset = false,
    holdRead = false,
    singlePort = true
    ))))

  data_banks.map(_.map(_.dump()))

  val way_en = Wire(Vec(LoadPipelineWidth, io.read(0).bits.way_en.cloneType))
  val way_en_reg = RegNext(way_en)
  val set_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val bank_addrs = Wire(Vec(LoadPipelineWidth, UInt()))

  // read data_banks and ecc_banks
  // for single port SRAM, do not allow read and write in the same cycle
  val rwhazard = RegNext(io.write.valid)
  val rrhazard = false.B // io.readline.valid
  (0 until LoadPipelineWidth).map(rport_index => {
    set_addrs(rport_index) := addr_to_dcache_set(io.read(rport_index).bits.addr)
    bank_addrs(rport_index) := addr_to_dcache_bank(io.read(rport_index).bits.addr)

    io.read(rport_index).ready := !(rwhazard || rrhazard)

    // use way_en to select a way after data read out
    assert(!(RegNext(io.read(rport_index).fire() && PopCount(io.read(rport_index).bits.way_en) > 1.U)))
    way_en(rport_index) := io.read(rport_index).bits.way_en
  })
  io.readline.ready := !(rwhazard)

  // read conflict
  val rr_bank_conflict = Seq.tabulate(LoadPipelineWidth)(x => Seq.tabulate(LoadPipelineWidth)(y =>
    bank_addrs(x) === bank_addrs(y) && io.read(x).valid && io.read(y).valid && io.read(x).bits.way_en === io.read(y).bits.way_en && set_addrs(x) =/= set_addrs(y)
  ))
  val rrl_bank_conflict = Wire(Vec(LoadPipelineWidth, Bool()))
  if (ReduceReadlineConflict) {
    (0 until LoadPipelineWidth).foreach(i => rrl_bank_conflict(i) := io.read(i).valid && io.readline.valid && io.readline.bits.rmask(bank_addrs(i)))
  } else {
    (0 until LoadPipelineWidth).foreach(i => rrl_bank_conflict(i) := io.read(i).valid && io.readline.valid && io.readline.bits.way_en === way_en(i) && addr_to_dcache_set(io.readline.bits.addr)=/=set_addrs(i))
  }
  val rrl_bank_conflict_intend = Wire(Vec(LoadPipelineWidth, Bool()))
  if (ReduceReadlineConflict) {
    (0 until LoadPipelineWidth).foreach(i => rrl_bank_conflict_intend(i) := io.read(i).valid && io.readline_intend && io.readline.bits.rmask(bank_addrs(i)))
  } else {
    (0 until LoadPipelineWidth).foreach(i => rrl_bank_conflict_intend(i) := io.read(i).valid && io.readline_intend && io.readline.bits.way_en === way_en(i) && addr_to_dcache_set(io.readline.bits.addr)=/=set_addrs(i))
  }

  val rw_bank_conflict = VecInit(Seq.tabulate(LoadPipelineWidth)(io.read(_).valid && rwhazard))
  val perf_multi_read = PopCount(io.read.map(_.valid)) >= 2.U
  (0 until LoadPipelineWidth).foreach(i => {
    io.bank_conflict_fast(i) := rw_bank_conflict(i) || rrl_bank_conflict(i) ||
      (if (i == 0) 0.B else (0 until i).map(rr_bank_conflict(_)(i)).reduce(_ || _))
    io.bank_conflict_slow(i) := RegNext(io.bank_conflict_fast(i))
    io.disable_ld_fast_wakeup(i) := rw_bank_conflict(i) || rrl_bank_conflict_intend(i) ||
      (if (i == 0) 0.B else (0 until i).map(rr_bank_conflict(_)(i)).reduce(_ || _))
  })
  XSPerfAccumulate("data_array_multi_read", perf_multi_read)
  (1 until LoadPipelineWidth).foreach(y => (0 until y).foreach(x =>
    XSPerfAccumulate(s"data_array_rr_bank_conflict_${x}_${y}", rr_bank_conflict(x)(y))
  ))
  (0 until LoadPipelineWidth).foreach(i => {
    XSPerfAccumulate(s"data_array_rrl_bank_conflict_${i}", rrl_bank_conflict(i))
    XSPerfAccumulate(s"data_array_rw_bank_conflict_${i}", rw_bank_conflict(i))
    XSPerfAccumulate(s"data_array_read_${i}", io.read(i).valid)
  })
  XSPerfAccumulate("data_array_access_total", PopCount(io.read.map(_.valid)))
  XSPerfAccumulate("data_array_read_line", io.readline.valid)
  XSPerfAccumulate("data_array_write", io.write.valid)

  val read_result = Wire(Vec(DCacheBanks, Vec(DCacheWays,new L1BankedDataReadResult())))
  val read_error_delayed_result = Wire(Vec(DCacheBanks, Vec(DCacheWays, Bool())))
  dontTouch(read_result)
  dontTouch(read_error_delayed_result)
  val data_read_oh = WireInit(VecInit(Seq.fill(DCacheBanks * DCacheWays)(0.U(1.W))))
  for (bank_index <- 0 until DCacheBanks) {
    for (way_index <- 0 until DCacheWays) {
      //     Set Addr & Read Way Mask
      //
      //    Pipe 0   ....  Pipe (n-1)
      //      +      ....     +
      //      |      ....     |
      // +----+---------------+-----+
      //  X                        X
      //   X                      +------+ Bank Addr Match
      //    +---------+----------+
      //              |
      //     +--------+--------+
      //     |    Data Bank    |
      //     +-----------------+
      val loadpipe_en = WireInit(VecInit(List.tabulate(LoadPipelineWidth)(i => {
        bank_addrs(i) === bank_index.U && io.read(i).valid && way_en(i)(way_index)
      })))
      val readline_en = Wire(Bool())
      if (ReduceReadlineConflict) {
        readline_en := io.readline.valid && io.readline.bits.rmask(bank_index) && io.readline.bits.way_en(way_index)
      } else {
        readline_en := io.readline.valid && io.readline.bits.way_en(way_index)
      }
      val sram_set_addr = Mux(readline_en,
        addr_to_dcache_set(io.readline.bits.addr),
        PriorityMux(Seq.tabulate(LoadPipelineWidth)(i => loadpipe_en(i) -> set_addrs(i)))
      )
      val read_en = loadpipe_en.asUInt.orR || readline_en
      // read raw data
      val data_bank = data_banks(bank_index)(way_index)
      data_bank.io.r.en := read_en
      data_bank.io.r.addr := sram_set_addr
      val ecc_bank = ecc_banks(bank_index)(way_index)
      ecc_bank.io.r.req.valid := read_en
      ecc_bank.io.r.req.bits.apply(setIdx = sram_set_addr)

      read_result(bank_index)(way_index).raw_data := data_bank.io.r.data
      read_result(bank_index)(way_index).ecc := ecc_bank.io.r.resp.data(0)

      // use ECC to check error
      val ecc_data = read_result(bank_index)(way_index).asECCData()
      val ecc_data_delayed = RegEnable(ecc_data, RegNext(read_en))
      read_result(bank_index)(way_index).error_delayed := dcacheParameters.dataCode.decode(ecc_data_delayed).error
      read_error_delayed_result(bank_index)(way_index) := read_result(bank_index)(way_index).error_delayed
      data_read_oh(bank_index * DCacheBanks + way_index) := read_en
    }
  }
  XSPerfAccumulate("data_read_counter", PopCount(Cat(data_read_oh)))

  // read result: expose banked read result
  /*
  (0 until LoadPipelineWidth).map(i => {
    io.read_resp(i) := read_result(RegNext(bank_addrs(i)))(RegNext(OHToUInt(way_en(i))))
   })
  */
  val read_result_delayed = RegNext(read_result)
  (0 until LoadPipelineWidth).map(i => {
    io.read_resp_delayed(i) := read_result_delayed(RegNext(RegNext(bank_addrs(i))))(RegNext(RegNext(OHToUInt(way_en(i)))))
  })
  (0 until DCacheBanks).map(i => {
    io.readline_resp(i) := read_result(i)(RegNext(OHToUInt(io.readline.bits.way_en)))
  })

  // error detection
  // normal read ports
  (0 until LoadPipelineWidth).map(rport_index => {
    io.read_error_delayed(rport_index) := RegNext(RegNext(io.read(rport_index).fire())) &&
      read_error_delayed_result(RegNext(RegNext(bank_addrs(rport_index))))(RegNext(RegNext(OHToUInt(way_en(rport_index))))) &&
      !RegNext(io.bank_conflict_slow(rport_index))
  })
  // readline port
  io.readline_error_delayed := RegNext(RegNext(io.readline.fire())) &&
    VecInit((0 until DCacheBanks).map(i => io.readline_resp(i).error_delayed)).asUInt().orR

  // write data_banks & ecc_banks
  val sram_waddr = addr_to_dcache_set(io.write.bits.addr)
  val sram_waddr_dup = io.write_dup.map(x => addr_to_dcache_set(x.bits.addr))
  for (bank_index <- 0 until DCacheBanks) {
    for (way_index <- 0 until DCacheWays) {
      // data write
      val data_bank = data_banks(bank_index)(way_index)
      data_bank.io.w.en := io.write_dup(bank_index).valid && io.write.bits.wmask(bank_index) && io.write_dup(bank_index).bits.way_en(way_index)
      data_bank.io.w.addr := sram_waddr_dup(bank_index)
      data_bank.io.w.data := io.write.bits.data(bank_index)
      // ecc write
      val ecc_bank = ecc_banks(bank_index)(way_index)
      ecc_bank.io.w.req.valid := RegNext(io.write_dup(bank_index).valid && io.write.bits.wmask(bank_index) && io.write_dup(bank_index).bits.way_en(way_index))
      ecc_bank.io.w.req.bits.apply(
        setIdx = RegNext(sram_waddr_dup(bank_index)),
        data = RegNext(getECCFromEncWord(cacheParams.dataCode.encode((io.write.bits.data(bank_index))))),
        waymask = 1.U
      )
      when(ecc_bank.io.w.req.valid) {
        XSDebug("write in ecc sram: bank %x set %x data %x waymask %x\n",
          bank_index.U,
          sram_waddr,
          getECCFromEncWord(cacheParams.dataCode.encode((io.write.bits.data(bank_index)))),
          io.write.bits.way_en
        );
      }
    }
  }

  // deal with customized cache op
  require(nWays <= 32)
  io.cacheOp.resp.bits := DontCare
  val cacheOpShouldResp = WireInit(false.B)
  val eccReadResult = Wire(Vec(DCacheBanks, UInt(eccBits.W)))

  when (io.cacheOp.req.valid && CacheInstrucion.isReadData(io.cacheOp.req.bits.opCode)) { 
    for (bank_index <- 0 until (DCacheBanks / 3)) {
      for(way_index <- 0 until DCacheWays){
        val data_bank = data_banks(bank_index)(way_index)
        data_bank.io.r.en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(way_index)
        data_bank.io.r.addr := io.cacheOp.req.bits.index
      }
    }
    cacheOpShouldResp := true.B
  }
  when (io.cacheOp_req_dup(0).valid && CacheInstrucion.isReadDataECC(io.cacheOp_req_bits_opCode_dup(0))) {
    for (bank_index <- 0 until (DCacheBanks / 3)) {
      for(way_index <- 0 until DCacheWays){
        val ecc_bank = ecc_banks(bank_index)(way_index)
        ecc_bank.io.r.req.valid := true.B
        ecc_bank.io.r.req.bits.setIdx := io.cacheOp.req.bits.index
      }
    }
    cacheOpShouldResp := true.B
  }
  when(io.cacheOp_req_dup(1).valid && CacheInstrucion.isWriteData(io.cacheOp_req_bits_opCode_dup(1))){
    for (bank_index <- 0 until (DCacheBanks / 3)) {
      for(way_index <- 0 until DCacheWays){
        val data_bank = data_banks(bank_index)(way_index)
        data_bank.io.w.en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(way_index)
        data_bank.io.w.addr := io.cacheOp.req.bits.index
        data_bank.io.w.data := io.cacheOp.req.bits.write_data_vec(bank_index)
      }
    }
    cacheOpShouldResp := true.B
  }
  when(io.cacheOp_req_dup(2).valid && CacheInstrucion.isWriteDataECC(io.cacheOp_req_bits_opCode_dup(2))){
    for (bank_index <- 0 until (DCacheBanks / 3)) {
      for(way_index <- 0 until DCacheWays){
        val ecc_bank = ecc_banks(bank_index)(way_index)
        ecc_bank.io.w.req.valid := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(way_index)
        ecc_bank.io.w.req.bits.apply(
          setIdx = io.cacheOp.req.bits.index,
          data = io.cacheOp.req.bits.write_data_ecc,
          waymask = 1.U
        )
      }
    }
    cacheOpShouldResp := true.B
  }
  

  when (io.cacheOp_req_dup(3).valid && CacheInstrucion.isReadData(io.cacheOp_req_bits_opCode_dup(3))) { 
    for (bank_index <- (DCacheBanks / 3) until ((DCacheBanks / 3) * 2)) {
      for (way_index <- 0 until DCacheWays) {
        val data_bank = data_banks(bank_index)(way_index)
        data_bank.io.r.en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(way_index)
        data_bank.io.r.addr := io.cacheOp.req.bits.index
      }
    }
    cacheOpShouldResp := true.B
  }
  when (io.cacheOp_req_dup(4).valid && CacheInstrucion.isReadDataECC(io.cacheOp_req_bits_opCode_dup(4))) {
    for (bank_index <- (DCacheBanks / 3) until ((DCacheBanks / 3) * 2)) {
      for(way_index <- 0 until DCacheWays){
        val ecc_bank = ecc_banks(bank_index)(way_index)
        ecc_bank.io.r.req.valid := true.B
        ecc_bank.io.r.req.bits.setIdx := io.cacheOp.req.bits.index
      }
    }
    cacheOpShouldResp := true.B
  }
  when(io.cacheOp_req_dup(5).valid && CacheInstrucion.isWriteData(io.cacheOp_req_bits_opCode_dup(5))){
    for (bank_index <- (DCacheBanks / 3) until ((DCacheBanks / 3) * 2)) {
      for (way_index <- 0 until DCacheWays) {
        val data_bank = data_banks(bank_index)(way_index)
        data_bank.io.w.en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(way_index)
        data_bank.io.w.addr := io.cacheOp.req.bits.index
        data_bank.io.w.data := io.cacheOp.req.bits.write_data_vec(bank_index)
      }
    }
    cacheOpShouldResp := true.B
  }
  when(io.cacheOp_req_dup(6).valid && CacheInstrucion.isWriteDataECC(io.cacheOp_req_bits_opCode_dup(6))){
    for (bank_index <- (DCacheBanks / 3) until ((DCacheBanks / 3) * 2)) {
      for(way_index <- 0 until DCacheWays){
        val ecc_bank = ecc_banks(bank_index)(way_index)
        ecc_bank.io.w.req.valid := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(way_index)
        ecc_bank.io.w.req.bits.apply(
          setIdx = io.cacheOp.req.bits.index,
          data = io.cacheOp.req.bits.write_data_ecc,
          waymask = 1.U
        )
      }
    }
    cacheOpShouldResp := true.B
  }

  when (io.cacheOp_req_dup(7).valid && CacheInstrucion.isReadData(io.cacheOp_req_bits_opCode_dup(7))) { 
    for (bank_index <- ((DCacheBanks / 3) * 2) until DCacheBanks) {
      for (way_index <- 0 until DCacheWays) {
        val data_bank = data_banks(bank_index)(way_index)
        data_bank.io.r.en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(way_index)
        data_bank.io.r.addr := io.cacheOp.req.bits.index
      }
    }
    cacheOpShouldResp := true.B
  }
  when (io.cacheOp_req_dup(8).valid && CacheInstrucion.isReadDataECC(io.cacheOp_req_bits_opCode_dup(8))) {
    for (bank_index <- ((DCacheBanks / 3) * 2) until DCacheBanks) {
      for(way_index <- 0 until DCacheWays){
        val ecc_bank = ecc_banks(bank_index)(way_index)
        ecc_bank.io.r.req.valid := true.B
        ecc_bank.io.r.req.bits.setIdx := io.cacheOp.req.bits.index
      }
    }
    cacheOpShouldResp := true.B
  }
  when(io.cacheOp_req_dup(9).valid && CacheInstrucion.isWriteData(io.cacheOp_req_bits_opCode_dup(9))){
    for (bank_index <- ((DCacheBanks / 3) * 2) until DCacheBanks) {
      for (way_index <- 0 until DCacheWays) {
        val data_bank = data_banks(bank_index)(way_index)
        data_bank.io.w.en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(way_index)
        data_bank.io.w.addr := io.cacheOp.req.bits.index
        data_bank.io.w.data := io.cacheOp.req.bits.write_data_vec(bank_index)
      }
    }
    cacheOpShouldResp := true.B
  }
  when(io.cacheOp_req_dup(10).valid && CacheInstrucion.isWriteDataECC(io.cacheOp_req_bits_opCode_dup(10))){
    for (bank_index <- ((DCacheBanks / 3) * 2) until DCacheBanks) {
      for(way_index <- 0 until DCacheWays){
        val ecc_bank = ecc_banks(bank_index)(way_index)
        ecc_bank.io.w.req.valid := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(way_index)
        ecc_bank.io.w.req.bits.apply(
          setIdx = io.cacheOp.req.bits.index,
          data = io.cacheOp.req.bits.write_data_ecc,
          waymask = 1.U
        )
      }
    }
    cacheOpShouldResp := true.B
  }
  
  io.cacheOp.resp.valid := RegNext(io.cacheOp.req.valid && cacheOpShouldResp)
  for (bank_index <- 0 until DCacheBanks) {
    io.cacheOp.resp.bits.read_data_vec(bank_index) := read_result(bank_index)(RegNext(io.cacheOp.req.bits.wayNum(4, 0))).raw_data
	  eccReadResult(bank_index) := read_result(bank_index)(RegNext(io.cacheOp.req.bits.wayNum(4, 0))).ecc
  }
  io.cacheOp.resp.bits.read_data_ecc := Mux(io.cacheOp.resp.valid, 
    eccReadResult(RegNext(io.cacheOp.req.bits.bank_num)),
    0.U
  )

  val tableName =  "BankConflict" + p(XSCoreParamsKey).HartId.toString
  val siteName = "BankedDataArray" + p(XSCoreParamsKey).HartId.toString
  val bankConflictTable = ChiselDB.createTable(tableName, new BankConflictDB)
  val bankConflictData = Wire(new BankConflictDB)
  for (i <- 0 until LoadPipelineWidth) {
    bankConflictData.set_index(i) := set_addrs(i)
    bankConflictData.addr(i) := io.read(i).bits.addr
  }

  // FIXME: rr_bank_conflict(0)(1) no generalization
  when(rr_bank_conflict(0)(1)) {
    bankConflictData.bank_index := bank_addrs(0)
    bankConflictData.way_index  := OHToUInt(way_en(0))
    bankConflictData.fake_rr_bank_conflict := set_addrs(0) === set_addrs(1)
  }.otherwise {
    bankConflictData.bank_index := 0.U
    bankConflictData.way_index := 0.U
    bankConflictData.fake_rr_bank_conflict := false.B
  }

  bankConflictTable.log(
    data = bankConflictData,
    en = rr_bank_conflict(0)(1),
    site = siteName,
    clock = clock,
    reset = reset
  )

  (1 until LoadPipelineWidth).foreach(y => (0 until y).foreach(x =>
    XSPerfAccumulate(s"data_array_fake_rr_bank_conflict_${x}_${y}", rr_bank_conflict(x)(y) && set_addrs(x)===set_addrs(y))
  ))

}