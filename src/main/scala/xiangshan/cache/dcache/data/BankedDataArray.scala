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
import chisel3.util._
import freechips.rocketchip.tilelink.{ClientMetadata, TLClientParameters, TLEdgeOut}
import utils.{Code, ParallelOR, ReplacementPolicy, SRAMTemplate, XSDebug, XSPerfAccumulate}
import xiangshan.L1CacheErrorInfo

import scala.math.max

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

class L1BankedDataReadResult(implicit p: Parameters) extends DCacheBundle
{
  // you can choose which bank to read to save power
  val ecc = Bits(eccBits.W)
  val raw_data = Bits(DCacheSRAMRowBits.W)

  def asECCData() = {
    Cat(ecc, raw_data)
  }
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
  val io = IO(new DCacheBundle {
    // load pipeline read word req
    val read = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new L1BankedDataReadReq)))
    // main pipeline read / write line req
    val readline = Flipped(DecoupledIO(new L1BankedDataReadLineReq))
    val write = Flipped(DecoupledIO(new L1BankedDataWriteReq))
    // data bank read resp (all banks)
    val resp = Output(Vec(DCacheBanks, new L1BankedDataReadResult()))
    // val nacks = Output(Vec(LoadPipelineWidth, Bool()))
    val errors = Output(Vec(LoadPipelineWidth, new L1CacheErrorInfo))
    // when bank_conflict, read (1) port should be ignored
    val bank_conflict_slow = Output(Vec(LoadPipelineWidth, Bool()))
    val bank_conflict_fast = Output(Vec(LoadPipelineWidth, Bool()))
    // customized cache op port 
    val cacheOp = Flipped(new DCacheInnerOpIO)
  })
  assert(LoadPipelineWidth == 2) // BankedDataArray is designed for 2 port

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
    (0 until DCacheBanks) map { r =>
      XSDebug(s"cycle: $r data: %x\n", io.resp(r).raw_data)
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

  // wrap data rows of 8 ways
  class DataSRAMBank(index: Int) extends Module {
    val io = IO(new Bundle() {
      val w = new Bundle() {
        val en = Input(Bool())
        val addr = Input(UInt())
        val way_en = Input(UInt(DCacheWays.W))
        val data = Input(UInt(DCacheSRAMRowBits.W))
      }

      val r = new Bundle() {
        val en = Input(Bool())
        val addr = Input(UInt())
        val way_en = Input(UInt(DCacheWays.W))
        val data = Output(UInt(DCacheSRAMRowBits.W))
      }
    })

    val r_way_en_reg = RegNext(io.r.way_en)

    // multiway data bank
    val data_bank = Array.fill(DCacheWays) {
      Module(new SRAMTemplate(
        Bits(DCacheSRAMRowBits.W),
        set = DCacheSets,
        way = 1,
        shouldReset = false,
        holdRead = false,
        singlePort = true
      ))
    }

    for (w <- 0 until DCacheWays) {
      val wen = io.w.en && io.w.way_en(w)
      data_bank(w).io.w.req.valid := wen
      data_bank(w).io.w.req.bits.apply(
        setIdx = io.w.addr,
        data = io.w.data,
        waymask = 1.U
      )
      data_bank(w).io.r.req.valid := io.r.en
      data_bank(w).io.r.req.bits.apply(setIdx = io.r.addr)
    }

    val half = nWays / 2
    val data_read = data_bank.map(_.io.r.resp.data(0))
    val data_left = Mux1H(r_way_en_reg.tail(half), data_read.take(half))
    val data_right = Mux1H(r_way_en_reg.head(half), data_read.drop(half))

    val sel_low = r_way_en_reg.tail(half).orR()
    val row_data = Mux(sel_low, data_left, data_right)

    io.r.data := row_data

    def dump_r() = {
      when(RegNext(io.r.en)) {
        XSDebug("bank read addr %x way_en %x data %x\n",
          RegNext(io.r.addr),
          RegNext(io.r.way_en),
          io.r.data
        )
      }
    }

    def dump_w() = {
      when(io.w.en) {
        XSDebug("bank write addr %x way_en %x data %x\n",
          io.w.addr,
          io.w.way_en,
          io.w.data
        )
      }
    }

    def dump() = {
      dump_w()
      dump_r()
    }
  }

  val data_banks = List.tabulate(DCacheBanks)(i => Module(new DataSRAMBank(i)))
  val ecc_banks = List.fill(DCacheBanks)(Module(new SRAMTemplate(
    Bits(eccBits.W),
    set = DCacheSets,
    way = DCacheWays,
    shouldReset = false,
    holdRead = false,
    singlePort = true
  )))

  data_banks.map(_.dump())

  val way_en = Wire(Vec(LoadPipelineWidth, io.read(0).bits.way_en.cloneType))
  val way_en_reg = RegNext(way_en)
  val set_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val bank_addrs = Wire(Vec(LoadPipelineWidth, UInt()))

  // read data_banks and ecc_banks
  // for single port SRAM, do not allow read and write in the same cycle
  val rwhazard = io.write.valid
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

  // read each bank, get bank result
  val bank_result = Wire(Vec(DCacheBanks, new L1BankedDataReadResult()))
  dontTouch(bank_result)
  val row_error = Wire(Vec(DCacheBanks, Bool()))
  dontTouch(row_error)
  val rr_bank_conflict = bank_addrs(0) === bank_addrs(1) && io.read(0).valid && io.read(1).valid
  val rrl_bank_conflict_0 = Wire(Bool())
  val rrl_bank_conflict_1 = Wire(Bool())
  if (ReduceReadlineConflict) {
    rrl_bank_conflict_0 := io.read(0).valid && io.readline.valid && io.readline.bits.rmask(bank_addrs(0))
    rrl_bank_conflict_1 := io.read(1).valid && io.readline.valid && io.readline.bits.rmask(bank_addrs(1))
  } else {
    rrl_bank_conflict_0 := io.read(0).valid && io.readline.valid
    rrl_bank_conflict_1 := io.read(1).valid && io.readline.valid
  }
  
  val rw_bank_conflict_0 = io.read(0).valid && rwhazard
  val rw_bank_conflict_1 = io.read(1).valid && rwhazard
  val perf_multi_read = io.read(0).valid && io.read(1).valid
  io.bank_conflict_fast(0) := rw_bank_conflict_0 || rrl_bank_conflict_0
  io.bank_conflict_slow(0) := RegNext(io.bank_conflict_fast(0))
  io.bank_conflict_fast(1) := rw_bank_conflict_1 || rrl_bank_conflict_1 || rr_bank_conflict
  io.bank_conflict_slow(1) := RegNext(io.bank_conflict_fast(1))
  XSPerfAccumulate("data_array_multi_read", perf_multi_read)
  XSPerfAccumulate("data_array_rr_bank_conflict", rr_bank_conflict)
  XSPerfAccumulate("data_array_rrl_bank_conflict_0", rrl_bank_conflict_0)
  XSPerfAccumulate("data_array_rrl_bank_conflict_1", rrl_bank_conflict_1)
  XSPerfAccumulate("data_array_rw_bank_conflict_0", rw_bank_conflict_0)
  XSPerfAccumulate("data_array_rw_bank_conflict_1", rw_bank_conflict_1)
  XSPerfAccumulate("data_array_access_total", io.read(0).valid +& io.read(1).valid)
  XSPerfAccumulate("data_array_read_0", io.read(0).valid)
  XSPerfAccumulate("data_array_read_1", io.read(1).valid)
  XSPerfAccumulate("data_array_read_line", io.readline.valid)
  XSPerfAccumulate("data_array_write", io.write.valid)

  for (bank_index <- 0 until DCacheBanks) {
    //     Set Addr & Read Way Mask
    //
    //      Pipe 0      Pipe 1
    //        +           +
    //        |           |
    // +------+-----------+-------+
    //  X                        X
    //   X                      +------+ Bank Addr Match
    //    +---------+----------+
    //              |
    //     +--------+--------+
    //     |    Data Bank    |
    //     +-----------------+
    val bank_addr_matchs = WireInit(VecInit(List.tabulate(LoadPipelineWidth)(i => {
      bank_addrs(i) === bank_index.U && io.read(i).valid
    })))
    val readline_match = Wire(Bool())
    if (ReduceReadlineConflict) {
      readline_match := io.readline.valid && io.readline.bits.rmask(bank_index)
    } else {
      readline_match := io.readline.valid
    }
    val bank_way_en = Mux(readline_match,
      io.readline.bits.way_en,
      Mux(bank_addr_matchs(0), way_en(0), way_en(1))
    )
    val bank_set_addr = Mux(readline_match,
      addr_to_dcache_set(io.readline.bits.addr),
      Mux(bank_addr_matchs(0), set_addrs(0), set_addrs(1))
    )

    // read raw data
    val data_bank = data_banks(bank_index)
    data_bank.io.r.en := bank_addr_matchs.asUInt.orR || readline_match
    data_bank.io.r.way_en := bank_way_en
    data_bank.io.r.addr := bank_set_addr
    bank_result(bank_index).raw_data := data_bank.io.r.data

    // read ECC
    val ecc_bank = ecc_banks(bank_index)
    ecc_bank.io.r.req.valid := bank_addr_matchs.asUInt.orR
    ecc_bank.io.r.req.bits.apply(setIdx = bank_set_addr)
    bank_result(bank_index).ecc := Mux1H(RegNext(bank_way_en), ecc_bank.io.r.resp.data)

    // use ECC to check error
    val data = bank_result(bank_index).asECCData()
    row_error(bank_index) := dcacheParameters.dataCode.decode(data).error && RegNext(bank_addr_matchs.asUInt.orR)
  }

  // Select final read result
  (0 until LoadPipelineWidth).map(rport_index => {
    io.errors(rport_index).ecc_error.valid := RegNext(io.read(rport_index).fire()) && row_error.asUInt.orR()
    io.errors(rport_index).ecc_error.bits := true.B
    io.errors(rport_index).paddr.valid := io.errors(rport_index).ecc_error.valid
    io.errors(rport_index).paddr.bits := RegNext(io.read(rport_index).bits.addr)
  })
  io.resp := bank_result

  // write data_banks & ecc_banks
  val sram_waddr = addr_to_dcache_set(io.write.bits.addr)
  for (bank_index <- 0 until DCacheBanks) {
    // data write
    val data_bank = data_banks(bank_index)
    data_bank.io.w.en := io.write.valid && io.write.bits.wmask(bank_index)
    data_bank.io.w.way_en := io.write.bits.way_en
    data_bank.io.w.addr := sram_waddr
    data_bank.io.w.data := io.write.bits.data(bank_index)

    // ecc write
    val ecc_bank = ecc_banks(bank_index)
    ecc_bank.io.w.req.valid := io.write.valid && io.write.bits.wmask(bank_index)
    ecc_bank.io.w.req.bits.apply(
      setIdx = sram_waddr,
      data = getECCFromEncWord(cacheParams.dataCode.encode((io.write.bits.data(bank_index)))),
      waymask = io.write.bits.way_en
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

  // deal with customized cache op
  require(nWays <= 32)
  io.cacheOp.resp.bits := DontCare
  val cacheOpShouldResp = WireInit(false.B) 
  when(io.cacheOp.req.valid){
    when(
      CacheInstrucion.isReadData(io.cacheOp.req.bits.opCode) ||
      CacheInstrucion.isReadDataECC(io.cacheOp.req.bits.opCode)
    ){
      for (bank_index <- 0 until DCacheBanks) {
        val data_bank = data_banks(bank_index)
        data_bank.io.r.en := true.B
        data_bank.io.r.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
        data_bank.io.r.addr := io.cacheOp.req.bits.index
      }
      cacheOpShouldResp := true.B
    }
    when(CacheInstrucion.isWriteData(io.cacheOp.req.bits.opCode)){
      for (bank_index <- 0 until DCacheBanks) {
        val data_bank = data_banks(bank_index)
        data_bank.io.w.en := true.B
        data_bank.io.w.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
        data_bank.io.w.addr := io.cacheOp.req.bits.index
        data_bank.io.w.data := io.cacheOp.req.bits.write_data_vec(bank_index)
      }
      cacheOpShouldResp := true.B
    }
    when(CacheInstrucion.isWriteDataECC(io.cacheOp.req.bits.opCode)){
      for (bank_index <- 0 until DCacheBanks) {
        val ecc_bank = ecc_banks(bank_index)
        ecc_bank.io.w.req.valid := true.B
        ecc_bank.io.w.req.bits.apply(
          setIdx = io.cacheOp.req.bits.index,
          data = io.cacheOp.req.bits.write_data_ecc,
          waymask = UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
        )
      }
      cacheOpShouldResp := true.B
    }
  }
  io.cacheOp.resp.valid := RegNext(io.cacheOp.req.valid && cacheOpShouldResp)
  for (bank_index <- 0 until DCacheBanks) {
    io.cacheOp.resp.bits.read_data_vec(bank_index) := bank_result(bank_index).raw_data
  }
  io.cacheOp.resp.bits.read_data_ecc := Mux(io.cacheOp.resp.valid, 
    bank_result(io.cacheOp.req.bits.bank_num).ecc, 
    0.U
  )
}
