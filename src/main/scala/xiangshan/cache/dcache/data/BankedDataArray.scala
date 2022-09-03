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
import chisel3.util._
import freechips.rocketchip.tilelink.{ClientMetadata, TLClientParameters, TLEdgeOut}
import utils.{Code, ParallelOR, ReplacementPolicy, SRAMTemplate, XSDebug, XSPerfAccumulate}
import xiangshan.L1CacheErrorInfo

import scala.math.max

// L1 data cache BankedDataReadReq
//
// T0: generate addr, send it to regs in all banks (load_s0, mainpipe_s0)
// T1: read SRAM use RegNext(addr) (load_s1, mainpipe_s1)
// T2: get read result from SRAM (load_s2, mainpipe_s2)
// T3: load read result bank select, merge, etc. (load_s3)
//
// Note that ready for L1BankedDataReadReq.valid in T0 is generated in T1
class L1BankedDataReadReq(implicit p: Parameters) extends DCacheBundle
{
  // data SRAM read T0
  val addr_s0 = Bits(VAddrBits.W)
  // data SRAM read T1
  val way_en_s1 = Bits(DCacheWays.W)
}

class L1BankedDataReadLineReq(implicit p: Parameters) extends L1BankedDataReadReq
{
  // data SRAM read T1
  val rmask_s1 = Bits(DCacheBanks.W) // not used for now
  val keep_req_s1 = Bool() // dirty pipeline ctrl, to be refactored
}

class DebugL1BankedDataReadReq(implicit p: Parameters) extends DCacheBundle
{
  val addr = Bits(VAddrBits.W)
  val way_en = Bits(DCacheWays.W)
}

class DebugL1BankedDataReadLineReq(implicit p: Parameters) extends DCacheBundle
{
  val addr = Bits(VAddrBits.W)
  val way_en = Bits(DCacheWays.W)
  val rmask = Bits(DCacheBanks.W) // not used for now
}

// Now, we write a cache-block in 2 cycles
// cache-block write request without data
class L1BankedDataWriteReqCtrl(implicit p: Parameters) extends DCacheBundle
{
  // T0
  val addr = Bits(VAddrBits.W)
  val way_en = Bits(DCacheWays.W)
}

class L1BankedDataWriteReq(implicit p: Parameters) extends L1BankedDataWriteReqCtrl
{
  // T0
  val data = Vec(DCacheBanks, Bits(DCacheSRAMRowBits.W))
  val wmask = Bits(DCacheBanks.W)
}

class L1BankedDataReadResult(implicit p: Parameters) extends DCacheBundle
{
  // you can choose which bank to read to save power
  // data SRAM read T2
  val ecc = Bits(eccBits.W)
  val raw_data = Bits(DCacheSRAMRowBits.W)
  // data SRAM read T3
  val error_delayed = Bool() // 1 cycle later than data resp

  def asECCData() = {
    Cat(ecc, raw_data)
  }
}

class DataSRAMBankReadReq(implicit p: Parameters) extends DCacheBundle {
  // update in SRAM read T0, used in T1
  val read = Vec(LoadPipelineWidth, new DCacheBundle() {
    val en = Bool()
    val set = UInt()
    val bank_match = Bool()
  })
  val readline = new DCacheBundle() {
    val en = Bool()
    val set = UInt()
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
    // data bank read resp (all banks)
    val resp = Output(Vec(DCacheBanks, new L1BankedDataReadResult()))
    // val nacks = Output(Vec(LoadPipelineWidth, Bool()))
    // val errors = Output(Vec(LoadPipelineWidth + 1, new L1CacheErrorInfo)) // read ports + readline port
    val read_error_delayed = Output(Vec(LoadPipelineWidth, Bool()))
    val readline_error_delayed = Output(Bool())
    // when bank_conflict, read (1) port should be ignored
    val bank_conflict_slow = Output(Vec(LoadPipelineWidth, Bool()))
    val bank_conflict_fast = Output(Vec(LoadPipelineWidth, Bool()))
    val disable_ld_fast_wakeup = Output(Vec(LoadPipelineWidth, Bool()))
    // customized cache op port 
    val cacheOp = Flipped(new L1CacheInnerOpIO)
    val cacheOp_req_dup = Vec(11, Flipped(Valid(new CacheCtrlReqInfo)))
    val cacheOp_req_bits_opCode_dup = Input(Vec(11, UInt(XLEN.W)))
  })
  assert(LoadPipelineWidth <= 2) // BankedDataArray is designed for no more than 2 read ports

  // gengerate debug signals
  val debug_read = Wire(Vec(LoadPipelineWidth, Decoupled(new DebugL1BankedDataReadReq)))
  val debug_readline = Wire(Decoupled(new DebugL1BankedDataReadLineReq))

  (0 until LoadPipelineWidth) map { w =>
    debug_read(w).valid := RegNext(io.read(w).valid)
    debug_read(w).ready := io.read(w).ready
    debug_read(w).bits.addr := RegNext(io.read(w).bits.addr_s0)
    debug_read(w).bits.way_en := io.read(w).bits.way_en_s1
  }

  debug_readline.valid := RegNext(io.readline.valid)
  debug_readline.ready := io.readline.ready
  debug_readline.bits.addr := RegNext(io.readline.bits.addr_s0)
  debug_readline.bits.way_en := io.readline.bits.way_en_s1
  debug_readline.bits.rmask := io.readline.bits.rmask_s1

  def pipeMap[T <: Data](f: Int => T) = VecInit((0 until LoadPipelineWidth).map(f))

  def dumpRead() = {
    (0 until LoadPipelineWidth) map { w =>
      when(RegNext(io.read(w).valid)) { // use io signal to make chisel happy
        XSDebug(s"DataArray Read channel: $w valid way_en: %x addr: %x\n",
          debug_read(w).bits.way_en, debug_read(w).bits.addr)
      }
    }
    when(RegNext(io.readline.valid)) { // use io signal to make chisel happy
      XSDebug(s"DataArray Read Line, valid way_en: %x addr: %x rmask %x\n",
        debug_readline.bits.way_en, debug_readline.bits.addr, debug_readline.bits.rmask)
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
  io.write_dup.foreach(_.ready := true.B)

  // wrap data rows of 8 ways
  class DataSRAMBank(index: Int) extends Module {
    val io = IO(new Bundle() {
      val w = Input(new DataSRAMBankWriteReq)

      val r = new Bundle() {
        val en = Input(Bool())
        val addr = Input(UInt(log2Up(DCacheSets).W))
        val data = Output(Vec(DCacheWays, UInt(DCacheSRAMRowBits.W)))
      }
    })

    assert(RegNext(!io.w.en || PopCount(io.w.way_en) <= 1.U))

    val w_reg = RegNext(io.w)
    // val rw_bypass = RegNext(io.w.addr === io.r.addr && io.w.way_en === io.r.way_en && io.w.en)

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
      val wen = w_reg.en && w_reg.way_en(w)
      data_bank(w).io.w.req.valid := wen
      data_bank(w).io.w.req.bits.apply(
        setIdx = w_reg.addr,
        data = w_reg.data,
        waymask = 1.U
      )
      data_bank(w).io.r.req.valid := io.r.en
      data_bank(w).io.r.req.bits.apply(setIdx = io.r.addr)
    }

    io.r.data := VecInit(data_bank.map(_.io.r.resp.data(0)))
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

  // bank read result
  val bank_result = Wire(Vec(DCacheBanks, new L1BankedDataReadResult()))
  dontTouch(bank_result)
  val read_bank_error_delayed = Wire(Vec(DCacheBanks, Bool()))
  dontTouch(read_bank_error_delayed)

  // Read Pipeline
  // --------------------------------------------------------------------------------
  // stage 0
  // --------------------------------------------------------------------------------
  // read s0, get read valid addr and RegNext() them for all banks

  val s0_read_valid = Wire(Vec(LoadPipelineWidth, Bool()))
  val s0_read_set_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val s0_read_bank_addrs = Wire(Vec(LoadPipelineWidth, UInt()))

  val s0_readline_valid = io.readline.valid
  val s0_readline_set_addrs = addr_to_dcache_set(io.readline.bits.addr_s0)

  (0 until LoadPipelineWidth).map(rport_index => {
    s0_read_valid(rport_index) := io.read(rport_index).valid
    s0_read_set_addrs(rport_index) := addr_to_dcache_set(io.read(rport_index).bits.addr_s0)
    s0_read_bank_addrs(rport_index) := addr_to_dcache_bank(io.read(rport_index).bits.addr_s0)
  })

  // generate numBanks copies of read addrs near data SRAM, RegNext() them
  val s0_bank_read_req = Wire(Vec(DCacheBanks, new DataSRAMBankReadReq))
  s0_bank_read_req.zipWithIndex.map{case (i, index) => {
    (0 until LoadPipelineWidth).map(rport_index => {
      i.read(rport_index).en := s0_read_valid(rport_index)
      i.read(rport_index).set := s0_read_set_addrs(rport_index)
      i.read(rport_index).bank_match := s0_read_bank_addrs(rport_index) === index.U &&
        s0_read_valid(rport_index)
    })
    i.readline.en := s0_readline_valid
    i.readline.set := s0_readline_set_addrs
  }}

  // --------------------------------------------------------------------------------
  // stage 1
  // --------------------------------------------------------------------------------
  // read_s1, use RegNext(addr) to read data SRAM, get tag match result, RegNext() it

  val s1_read_valid = RegNext(s0_read_valid)
  val s1_read_set_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val s1_read_bank_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val s1_read_way_en = Wire(Vec(LoadPipelineWidth, io.read(0).bits.way_en_s1.cloneType))

  val s1_readline_valid = RegNext(s0_readline_valid)
  val s1_readline_set_addrs = RegEnable(s0_readline_set_addrs, s0_readline_valid)
  val s1_readline_rmask = io.readline.bits.rmask_s1
  val s1_readline_way_en = io.readline.bits.way_en_s1

  val s1_bank_read_req = Wire(Vec(DCacheBanks, new DataSRAMBankReadReq))

  val s1_read_fire = WireInit(VecInit(s1_read_valid.zipWithIndex.map{ case (valid, index) => {
    valid && io.read(index).ready
  }}))
  val s1_readline_fire = s1_readline_valid && io.readline.ready

  // --------------------------------------------------------
  // pipeline side logic
  // --------------------------------------------------------
  (0 until LoadPipelineWidth).map(rport_index => {
    s1_read_set_addrs(rport_index) := RegEnable(s0_read_set_addrs(rport_index), s0_read_valid(rport_index))
    s1_read_bank_addrs(rport_index) := RegEnable(s0_read_bank_addrs(rport_index), s0_read_valid(rport_index))
  })

  // conflict check
  // conflict check uses bank num reg near load pipeline

  // read/write conflict
  // for single port SRAM, do not allow read and write in the same cycle
  val s1_rwhazard = RegNext(io.write.valid)
  // s1_rrhazard is ignored, s1_rrhazard is detected by rrl_bank_conflict signal
  // if s1_rrhazard happens, normal read will be replayed
  val s1_rrhazard = false.B // io.readline.valid

  (0 until LoadPipelineWidth).map(rport_index => {
    io.read(rport_index).ready := !(s1_rwhazard || s1_rrhazard)

    // use way_en to select a way after data read out
    assert(!(RegNext(debug_read(rport_index).fire() && PopCount(debug_read(rport_index).bits.way_en) > 1.U)))
    s1_read_way_en(rport_index) := io.read(rport_index).bits.way_en_s1
  })
  io.readline.ready := !(s1_rwhazard)

  // read bank conflict
  val rr_bank_conflict = s1_read_bank_addrs(0) === s1_read_bank_addrs(1) && s1_read_valid(0) && s1_read_valid(1)
  val rrl_bank_conflict = Wire(Vec(LoadPipelineWidth, Bool()))
  if (ReduceReadlineConflict) {
    rrl_bank_conflict(0) := s1_read_valid(0) && s1_readline_valid && s1_readline_rmask(s1_read_bank_addrs(0))
    rrl_bank_conflict(1) := s1_read_valid(1) && s1_readline_valid && s1_readline_rmask(s1_read_bank_addrs(1))
  } else {
    rrl_bank_conflict(0) := s1_read_valid(0) && s1_readline_valid
    rrl_bank_conflict(1) := s1_read_valid(1) && s1_readline_valid
  }
  val rrl_bank_conflict_intend = Wire(Vec(LoadPipelineWidth, Bool()))
  if (ReduceReadlineConflict) {
    (0 until LoadPipelineWidth).foreach(i => rrl_bank_conflict_intend(i) := s1_read_valid(i) && s1_readline_valid && s1_readline_rmask(s1_read_bank_addrs(i)))
  } else {
    (0 until LoadPipelineWidth).foreach(i => rrl_bank_conflict_intend(i) := s1_read_valid(i) && s1_readline_valid)
  }

  val rw_bank_conflict = VecInit(Seq.tabulate(LoadPipelineWidth)(s1_read_valid(_) && s1_rwhazard))
  val perf_multi_read = PopCount(s1_read_valid.asUInt) >= 2.U
  (0 until LoadPipelineWidth).foreach(i => {
    io.bank_conflict_fast(i) := rw_bank_conflict(i) || rrl_bank_conflict(i) ||
      (if (i == 0) 0.B else rr_bank_conflict)
    io.bank_conflict_slow(i) := RegNext(io.bank_conflict_fast(i))
    io.disable_ld_fast_wakeup(i) := rw_bank_conflict(i) || rrl_bank_conflict_intend(i) ||
      (if (i == 0) 0.B else rr_bank_conflict)
  })
  XSPerfAccumulate("data_array_multi_read", perf_multi_read)
  XSPerfAccumulate("data_array_rr_bank_conflict", rr_bank_conflict)
  XSPerfAccumulate("data_array_rrl_bank_conflict(0)", rrl_bank_conflict(0))
  XSPerfAccumulate("data_array_rrl_bank_conflict(1)", rrl_bank_conflict(1))
  XSPerfAccumulate("data_array_rw_bank_conflict_0", rw_bank_conflict(0))
  XSPerfAccumulate("data_array_rw_bank_conflict_1", rw_bank_conflict(1))
  XSPerfAccumulate("data_array_access_total", io.read(0).valid +& io.read(1).valid)
  XSPerfAccumulate("data_array_read_0", io.read(0).valid)
  XSPerfAccumulate("data_array_read_1", io.read(1).valid)
  XSPerfAccumulate("data_array_read_line", io.readline.valid)
  XSPerfAccumulate("data_array_write", io.write.valid)

  // --------------------------------------------------------
  // SRAM side logic
  // --------------------------------------------------------
  // read data_banks and ecc_banks
  (0 until DCacheBanks).map{i => {
    val s0 = s0_bank_read_req(i)
    val s1 = s1_bank_read_req(i)
    (0 until LoadPipelineWidth).map(rport_index => {
      s1.read(rport_index).en := RegNext(s0.read(rport_index).en)
      s1.read(rport_index).set := RegEnable(s0.read(rport_index).set, s0.read(rport_index).en)
      s1.read(rport_index).bank_match := RegNext(s0.read(rport_index).bank_match)
    })
    s1.readline.en := RegNext(s0.readline.en || io.readline.bits.keep_req_s1 && s1.readline.en)
    s1.readline.set := RegEnable(s0.readline.set, s0.readline.en && !io.readline.bits.keep_req_s1)
  }}

  for (bank_index <- 0 until DCacheBanks) {
    val bank_read_req = s1_bank_read_req(bank_index)

    // Bank level read logic:
    //
    //         Set Addr & Read Way Mask
    //       Reg         Reg          Reg
    //      Pipe 0      Pipe 1      Readline
    //        +           +            +
    //        |           |            |
    // +------+-----------+----------------+
    //  X                                 X
    //   X                               +------+ Bank Addr Match
    //    +---------------+-------------+
    //                    |
    //           +--------+--------+
    //           |    Data Bank    |
    //           +-----------------+

    val s1_bank_addr_matchs = WireInit(VecInit(List.tabulate(LoadPipelineWidth)(i => {
      // bank_addrs(i) === bank_index.U && io.read(i).valid
      bank_read_req.read(i).bank_match
    })))
    val s1_readline_match = Wire(Bool())
    if (ReduceReadlineConflict) {
      // bad timing
      s1_readline_match := bank_read_req.readline.en && s1_readline_rmask(bank_index)
    } else {
      s1_readline_match := bank_read_req.readline.en
    }
    val s1_bank_way_en = Mux(s1_readline_match,
      s1_readline_way_en,
      Mux(s1_bank_addr_matchs(0), s1_read_way_en(0), s1_read_way_en(1))
    ) // slow to generate
    val s1_bank_set_addr = Mux(s1_readline_match,
      bank_read_req.readline.set,
      Mux(s1_bank_addr_matchs(0), bank_read_req.read(0).set, bank_read_req.read(1).set)
    )
    val s2_bank_way_en_dup_data = RegNext(s1_bank_way_en)
    val s2_bank_way_en_dup_ecc = RegNext(s1_bank_way_en)

    val s1_read_enable = s1_bank_addr_matchs.asUInt.orR || s1_readline_match

    // read raw data
    val data_bank = data_banks(bank_index)
    data_bank.io.r.en := s1_read_enable
    data_bank.io.r.addr := s1_bank_set_addr
    bank_result(bank_index).raw_data := Mux1H(s2_bank_way_en_dup_data, data_bank.io.r.data)

    // read ECC
    val ecc_bank = ecc_banks(bank_index)
    ecc_bank.io.r.req.valid := s1_read_enable
    ecc_bank.io.r.req.bits.apply(setIdx = s1_bank_set_addr)
    bank_result(bank_index).ecc := Mux1H(s2_bank_way_en_dup_ecc, ecc_bank.io.r.resp.data)

    // use ECC to check error
    val ecc_data = bank_result(bank_index).asECCData()
    val ecc_data_delayed = RegEnable(ecc_data, RegNext(s1_read_enable))
    bank_result(bank_index).error_delayed := dcacheParameters.dataCode.decode(ecc_data_delayed).error
    read_bank_error_delayed(bank_index) := bank_result(bank_index).error_delayed
  }

  // read result: expose banked read result 
  io.resp := bank_result

  // error detection
  // normal read ports
  (0 until LoadPipelineWidth).map(rport_index => {
    io.read_error_delayed(rport_index) := RegNext(RegNext(s1_read_fire(rport_index))) && 
      read_bank_error_delayed(RegNext(RegNext(s1_read_bank_addrs(rport_index)))) &&
      !RegNext(io.bank_conflict_slow(rport_index))
  })
  // readline port
  io.readline_error_delayed := RegNext(RegNext(s1_readline_fire)) && 
    VecInit((0 until DCacheBanks).map(i => io.resp(i).error_delayed)).asUInt().orR

  // write data_banks & ecc_banks
  val sram_waddr = addr_to_dcache_set(io.write.bits.addr)
  val sram_waddr_dup = io.write_dup.map(x => addr_to_dcache_set(x.bits.addr))
  for (bank_index <- 0 until DCacheBanks) {
    // data write
    val data_bank = data_banks(bank_index)
    data_bank.io.w.en := io.write_dup(bank_index).valid && io.write.bits.wmask(bank_index)
    data_bank.io.w.way_en := io.write_dup(bank_index).bits.way_en
    data_bank.io.w.addr := sram_waddr_dup(bank_index)
    data_bank.io.w.data := io.write.bits.data(bank_index)

    // ecc write
    val ecc_bank = ecc_banks(bank_index)
    ecc_bank.io.w.req.valid := RegNext(io.write_dup(bank_index).valid && io.write.bits.wmask(bank_index))
    ecc_bank.io.w.req.bits.apply(
      setIdx = RegNext(sram_waddr_dup(bank_index)),
      data = RegNext(getECCFromEncWord(cacheParams.dataCode.encode((io.write.bits.data(bank_index))))),
      waymask = RegNext(io.write_dup(bank_index).bits.way_en)
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
  val eccReadResult = Wire(Vec(DCacheBanks, UInt(eccBits.W)))

  when (io.cacheOp.req.valid && CacheInstrucion.isReadData(io.cacheOp.req.bits.opCode)) { 
    for (bank_index <- 0 until (DCacheBanks / 3)) {
      val data_bank = data_banks(bank_index)
      data_bank.io.r.en := true.B
      data_bank.io.r.addr := io.cacheOp.req.bits.index
    }
    cacheOpShouldResp := true.B
  }
  when (io.cacheOp_req_dup(0).valid && CacheInstrucion.isReadDataECC(io.cacheOp_req_bits_opCode_dup(0))) {
    for (bank_index <- 0 until (DCacheBanks / 3)) {
      val ecc_bank = ecc_banks(bank_index)
      ecc_bank.io.r.req.valid := true.B
      ecc_bank.io.r.req.bits.setIdx := io.cacheOp.req.bits.index
    }
    cacheOpShouldResp := true.B
  }
  when(io.cacheOp_req_dup(1).valid && CacheInstrucion.isWriteData(io.cacheOp_req_bits_opCode_dup(1))){
    for (bank_index <- 0 until (DCacheBanks / 3)) {
      val data_bank = data_banks(bank_index)
      data_bank.io.w.en := true.B
      data_bank.io.w.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
      data_bank.io.w.addr := io.cacheOp.req.bits.index
      data_bank.io.w.data := io.cacheOp.req.bits.write_data_vec(bank_index)
    }
    cacheOpShouldResp := true.B
  }
  when(io.cacheOp_req_dup(2).valid && CacheInstrucion.isWriteDataECC(io.cacheOp_req_bits_opCode_dup(2))){
    for (bank_index <- 0 until (DCacheBanks / 3)) {
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
  

  when (io.cacheOp_req_dup(3).valid && CacheInstrucion.isReadData(io.cacheOp_req_bits_opCode_dup(3))) { 
    for (bank_index <- (DCacheBanks / 3) until ((DCacheBanks / 3) * 2)) {
      val data_bank = data_banks(bank_index)
      data_bank.io.r.en := true.B
      data_bank.io.r.addr := io.cacheOp.req.bits.index
    }
    cacheOpShouldResp := true.B
  }
  when (io.cacheOp_req_dup(4).valid && CacheInstrucion.isReadDataECC(io.cacheOp_req_bits_opCode_dup(4))) {
    for (bank_index <- (DCacheBanks / 3) until ((DCacheBanks / 3) * 2)) {
      val ecc_bank = ecc_banks(bank_index)
      ecc_bank.io.r.req.valid := true.B
      ecc_bank.io.r.req.bits.setIdx := io.cacheOp.req.bits.index
    }
    cacheOpShouldResp := true.B
  }
  when(io.cacheOp_req_dup(5).valid && CacheInstrucion.isWriteData(io.cacheOp_req_bits_opCode_dup(5))){
    for (bank_index <- (DCacheBanks / 3) until ((DCacheBanks / 3) * 2)) {
      val data_bank = data_banks(bank_index)
      data_bank.io.w.en := true.B
      data_bank.io.w.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
      data_bank.io.w.addr := io.cacheOp.req.bits.index
      data_bank.io.w.data := io.cacheOp.req.bits.write_data_vec(bank_index)
    }
    cacheOpShouldResp := true.B
  }
  when(io.cacheOp_req_dup(6).valid && CacheInstrucion.isWriteDataECC(io.cacheOp_req_bits_opCode_dup(6))){
    for (bank_index <- (DCacheBanks / 3) until ((DCacheBanks / 3) * 2)) {
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

  when (io.cacheOp_req_dup(7).valid && CacheInstrucion.isReadData(io.cacheOp_req_bits_opCode_dup(7))) { 
    for (bank_index <- ((DCacheBanks / 3) * 2) until DCacheBanks) {
      val data_bank = data_banks(bank_index)
      data_bank.io.r.en := true.B
      data_bank.io.r.addr := io.cacheOp.req.bits.index
    }
    cacheOpShouldResp := true.B
  }
  when (io.cacheOp_req_dup(8).valid && CacheInstrucion.isReadDataECC(io.cacheOp_req_bits_opCode_dup(8))) {
    for (bank_index <- ((DCacheBanks / 3) * 2) until DCacheBanks) {
      val ecc_bank = ecc_banks(bank_index)
      ecc_bank.io.r.req.valid := true.B
      ecc_bank.io.r.req.bits.setIdx := io.cacheOp.req.bits.index
    }
      cacheOpShouldResp := true.B
  }
  when(io.cacheOp_req_dup(9).valid && CacheInstrucion.isWriteData(io.cacheOp_req_bits_opCode_dup(9))){
    for (bank_index <- ((DCacheBanks / 3) * 2) until DCacheBanks) {
      val data_bank = data_banks(bank_index)
      data_bank.io.w.en := true.B
      data_bank.io.w.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
      data_bank.io.w.addr := io.cacheOp.req.bits.index
      data_bank.io.w.data := io.cacheOp.req.bits.write_data_vec(bank_index)
    }
    cacheOpShouldResp := true.B
  }
  when(io.cacheOp_req_dup(10).valid && CacheInstrucion.isWriteDataECC(io.cacheOp_req_bits_opCode_dup(10))){
    for (bank_index <- ((DCacheBanks / 3) * 2) until DCacheBanks) {
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
  
  io.cacheOp.resp.valid := RegNext(io.cacheOp.req.valid && cacheOpShouldResp)
  for (bank_index <- 0 until DCacheBanks) {
    io.cacheOp.resp.bits.read_data_vec(bank_index) := bank_result(bank_index).raw_data
	  eccReadResult(bank_index) := ecc_banks(bank_index).io.r.resp.data(RegNext(io.cacheOp.req.bits.wayNum(4, 0)))
  }
  io.cacheOp.resp.bits.read_data_ecc := Mux(io.cacheOp.resp.valid, 
    eccReadResult(RegNext(io.cacheOp.req.bits.bank_num)),
    0.U
  )
}
