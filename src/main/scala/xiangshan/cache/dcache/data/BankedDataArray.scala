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

import org.chipsalliance.cde.config.Parameters
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
  val bank_index = Vec(VLEN/DCacheSRAMRowBits, UInt((DCacheSetOffset - DCacheBankOffset).W))
  val way_index = UInt(wayBits.W)
  val fake_rr_bank_conflict = Bool()
}

class L1BankedDataReadReq(implicit p: Parameters) extends DCacheBundle
{
  val way_en = Bits(DCacheWays.W)
  val addr = Bits(PAddrBits.W)
}

class L1BankedDataReadReqWithMask(implicit p: Parameters) extends DCacheBundle
{
  val way_en = Bits(DCacheWays.W)
  val addr = Bits(PAddrBits.W)
  val bankMask = Bits(DCacheBanks.W)
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

// wrap a sram
class DataSRAM(bankIdx: Int, wayIdx: Int)(implicit p: Parameters) extends DCacheModule {
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
    set = DCacheSets / DCacheSetDiv,
    way = 1,
    shouldReset = false,
    holdRead = false,
    singlePort = true
  ))

  data_sram.io.w.req.valid := io.w.en
  data_sram.io.w.req.bits.apply(
    setIdx = io.w.addr,
    data = io.w.data,
    waymask = 1.U
  )
  data_sram.io.r.req.valid := io.r.en
  data_sram.io.r.req.bits.apply(setIdx = io.r.addr)
  io.r.data := data_sram.io.r.resp.data(0)
  XSPerfAccumulate("part_data_read_counter", data_sram.io.r.req.valid)

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

// wrap data rows of 8 ways
class DataSRAMBank(index: Int)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val w = Input(new DataSRAMBankWriteReq)

    val r = new Bundle() {
      val en = Input(Bool())
      val addr = Input(UInt())
      val way_en = Input(UInt(DCacheWays.W))
      val data = Output(UInt(DCacheSRAMRowBits.W))
    }
  })

  assert(RegNext(!io.w.en || PopCount(io.w.way_en) <= 1.U))
  assert(RegNext(!io.r.en || PopCount(io.r.way_en) <= 1.U))

  val r_way_en_reg = RegNext(io.r.way_en)

  // external controls do not read and write at the same time
  val w_info = io.w
  // val rw_bypass = RegNext(io.w.addr === io.r.addr && io.w.way_en === io.r.way_en && io.w.en)

  // multiway data bank
  val data_bank = Array.fill(DCacheWays) {
    Module(new SRAMTemplate(
      Bits(DCacheSRAMRowBits.W),
      set = DCacheSets / DCacheSetDiv,
      way = 1,
      shouldReset = false,
      holdRead = false,
      singlePort = true
    ))
  }

  for (w <- 0 until DCacheWays) {
    val wen = w_info.en && w_info.way_en(w)
    data_bank(w).io.w.req.valid := wen
    data_bank(w).io.w.req.bits.apply(
      setIdx = w_info.addr,
      data = w_info.data,
      waymask = 1.U
    )
    data_bank(w).io.r.req.valid := io.r.en
    data_bank(w).io.r.req.bits.apply(setIdx = io.r.addr)
  }
  XSPerfAccumulate("part_data_read_counter", PopCount(Cat(data_bank.map(_.io.r.req.valid))))

  val half = nWays / 2
  val data_read = data_bank.map(_.io.r.resp.data(0))
  val data_left = Mux1H(r_way_en_reg.tail(half), data_read.take(half))
  val data_right = Mux1H(r_way_en_reg.head(half), data_read.drop(half))

  val sel_low = r_way_en_reg.tail(half).orR
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
  val enableEcc = false
  val ReadlinePortErrorIndex = LoadPipelineWidth
  val io = IO(new DCacheBundle {
    // load pipeline read word req
    val read = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new L1BankedDataReadReqWithMask)))
    val is128Req = Input(Vec(LoadPipelineWidth, Bool()))
    // main pipeline read / write line req
    val readline_intend = Input(Bool())
    val readline = Flipped(DecoupledIO(new L1BankedDataReadLineReq))
    val write = Flipped(DecoupledIO(new L1BankedDataWriteReq))
    val write_dup = Vec(DCacheBanks, Flipped(Decoupled(new L1BankedDataWriteReqCtrl)))
    // data for readline and loadpipe
    val readline_resp = Output(Vec(DCacheBanks, new L1BankedDataReadResult()))
    val readline_error_delayed = Output(Bool())
    val read_resp_delayed = Output(Vec(LoadPipelineWidth, Vec(VLEN/DCacheSRAMRowBits, new L1BankedDataReadResult())))
    val read_error_delayed = Output(Vec(LoadPipelineWidth,Vec(VLEN/DCacheSRAMRowBits, Bool())))
    // val nacks = Output(Vec(LoadPipelineWidth, Bool()))
    // val errors = Output(Vec(LoadPipelineWidth + 1, new L1CacheErrorInfo)) // read ports + readline port
    // when bank_conflict, read (1) port should be ignored
    val bank_conflict_slow = Output(Vec(LoadPipelineWidth, Bool()))
    val disable_ld_fast_wakeup = Output(Vec(LoadPipelineWidth, Bool()))
    // customized cache op port
    val cacheOp = Flipped(new L1CacheInnerOpIO)
    val cacheOp_req_dup = Vec(DCacheDupNum, Flipped(Valid(new CacheCtrlReqInfo)))
    val cacheOp_req_bits_opCode_dup = Input(Vec(DCacheDupNum, UInt(XLEN.W)))
  })

  def pipeMap[T <: Data](f: Int => T) = VecInit((0 until LoadPipelineWidth).map(f))

  def getECCFromEncWord(encWord: UInt) = {
    require(encWord.getWidth == encWordBits)
    encWord(encWordBits - 1, wordBits)
  }

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
      XSDebug(s"cycle: $r data: %x\n", Mux(io.is128Req(r),
        Cat(io.read_resp_delayed(r)(1).raw_data,io.read_resp_delayed(r)(0).raw_data),
        io.read_resp_delayed(r)(0).raw_data))
    }
  }

  def dump() = {
    dumpRead
    dumpWrite
    dumpResp
  }
}

// the smallest access unit is sram
class SramedDataArray(implicit p: Parameters) extends AbstractBankedDataArray {
  println("  DCacheType: SramedDataArray")
  val ReduceReadlineConflict = false

  io.write.ready := true.B
  io.write_dup.foreach(_.ready := true.B)

  val data_banks = List.tabulate(DCacheSetDiv)( k => List.tabulate(DCacheBanks)(i => List.tabulate(DCacheWays)(j => Module(new DataSRAM(i,j)))))
  // ecc_banks also needs to be changed to two-dimensional to align with data_banks
  val ecc_banks = List.tabulate(DCacheSetDiv)( k =>
    List.tabulate(DCacheWays)(j =>
      List.tabulate(DCacheBanks)(i =>
        Module(new SRAMTemplate(
          Bits(eccBits.W),
          set = DCacheSets / DCacheSetDiv,
          way = 1,
          shouldReset = false,
          holdRead = false,
          singlePort = true
        ))
    )))

  data_banks.map(_.map(_.map(_.dump())))

  val way_en = Wire(Vec(LoadPipelineWidth, io.read(0).bits.way_en.cloneType))
  val set_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val div_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val bank_addrs = Wire(Vec(LoadPipelineWidth, Vec(VLEN/DCacheSRAMRowBits, UInt())))

  val line_set_addr = addr_to_dcache_div_set(io.readline.bits.addr)
  val line_div_addr = addr_to_dcache_div(io.readline.bits.addr)
  // when WPU is enabled, line_way_en is all enabled when read data
  val line_way_en = Fill(DCacheWays, 1.U) // val line_way_en = io.readline.bits.way_en
  val line_way_en_reg = RegNext(io.readline.bits.way_en)

  val write_bank_mask_reg = RegNext(io.write.bits.wmask)
  val write_data_reg = RegNext(io.write.bits.data)
  val write_valid_reg = RegNext(io.write.valid)
  val write_valid_dup_reg = io.write_dup.map(x => RegNext(x.valid))
  val write_wayen_dup_reg = io.write_dup.map(x => RegNext(x.bits.way_en))
  val write_set_addr_dup_reg = io.write_dup.map(x => RegNext(addr_to_dcache_div_set(x.bits.addr)))
  val write_div_addr_dup_reg = io.write_dup.map(x => RegNext(addr_to_dcache_div(x.bits.addr)))

  // read data_banks and ecc_banks
  // for single port SRAM, do not allow read and write in the same cycle
  val rrhazard = false.B // io.readline.valid
  (0 until LoadPipelineWidth).map(rport_index => {
    div_addrs(rport_index) := addr_to_dcache_div(io.read(rport_index).bits.addr)
    set_addrs(rport_index) := addr_to_dcache_div_set(io.read(rport_index).bits.addr)
    bank_addrs(rport_index)(0) := addr_to_dcache_bank(io.read(rport_index).bits.addr)
    bank_addrs(rport_index)(1) := bank_addrs(rport_index)(0) + 1.U

    // use way_en to select a way after data read out
    assert(!(RegNext(io.read(rport_index).fire && PopCount(io.read(rport_index).bits.way_en) > 1.U)))
    way_en(rport_index) := io.read(rport_index).bits.way_en
  })

  // read conflict
  val rr_bank_conflict = Seq.tabulate(LoadPipelineWidth)(x => Seq.tabulate(LoadPipelineWidth)(y =>
    io.read(x).valid && io.read(y).valid &&
    div_addrs(x) === div_addrs(y) &&
    (io.read(x).bits.bankMask & io.read(y).bits.bankMask) =/= 0.U &&
    io.read(x).bits.way_en === io.read(y).bits.way_en &&
    set_addrs(x) =/= set_addrs(y)
  ))
  val rrl_bank_conflict = Wire(Vec(LoadPipelineWidth, Bool()))
  val rrl_bank_conflict_intend = Wire(Vec(LoadPipelineWidth, Bool()))
  (0 until LoadPipelineWidth).foreach { i =>
    val judge = if (ReduceReadlineConflict) io.read(i).valid && (io.readline.bits.rmask & io.read(i).bits.bankMask) =/= 0.U && line_div_addr === div_addrs(i) && line_set_addr =/= set_addrs(i)
                else io.read(i).valid && line_div_addr === div_addrs(i) && line_set_addr =/= set_addrs(i)
    rrl_bank_conflict(i) := judge && io.readline.valid
    rrl_bank_conflict_intend(i) := judge && io.readline_intend
  }
  val wr_bank_conflict = Seq.tabulate(LoadPipelineWidth)(x =>
    io.read(x).valid && write_valid_reg &&
    div_addrs(x) === write_div_addr_dup_reg.head &&
    way_en(x) === write_wayen_dup_reg.head &&
    (write_bank_mask_reg(bank_addrs(x)(0)) || write_bank_mask_reg(bank_addrs(x)(1)) && io.is128Req(x))
  )
  val wrl_bank_conflict = io.readline.valid && write_valid_reg && line_div_addr === write_div_addr_dup_reg.head
  // ready
  io.readline.ready := !(wrl_bank_conflict)
  io.read.zipWithIndex.map { case (x, i) => x.ready := !(wr_bank_conflict(i) || rrhazard) }

  val perf_multi_read = PopCount(io.read.map(_.valid)) >= 2.U
  val bank_conflict_fast = Wire(Vec(LoadPipelineWidth, Bool()))
  (0 until LoadPipelineWidth).foreach(i => {
    bank_conflict_fast(i) := wr_bank_conflict(i) || rrl_bank_conflict(i) ||
      (if (i == 0) 0.B else (0 until i).map(rr_bank_conflict(_)(i)).reduce(_ || _))
    io.bank_conflict_slow(i) := RegNext(bank_conflict_fast(i))
    io.disable_ld_fast_wakeup(i) := wr_bank_conflict(i) || rrl_bank_conflict_intend(i) ||
      (if (i == 0) 0.B else (0 until i).map(rr_bank_conflict(_)(i)).reduce(_ || _))
  })
  XSPerfAccumulate("data_array_multi_read", perf_multi_read)
  (1 until LoadPipelineWidth).foreach(y => (0 until y).foreach(x =>
    XSPerfAccumulate(s"data_array_rr_bank_conflict_${x}_${y}", rr_bank_conflict(x)(y))
  ))
  (0 until LoadPipelineWidth).foreach(i => {
    XSPerfAccumulate(s"data_array_rrl_bank_conflict_${i}", rrl_bank_conflict(i))
    XSPerfAccumulate(s"data_array_rw_bank_conflict_${i}", wr_bank_conflict(i))
    XSPerfAccumulate(s"data_array_read_${i}", io.read(i).valid)
  })
  XSPerfAccumulate("data_array_access_total", PopCount(io.read.map(_.valid)))
  XSPerfAccumulate("data_array_read_line", io.readline.valid)
  XSPerfAccumulate("data_array_write", io.write.valid)

  val read_result = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, Vec(DCacheWays,new L1BankedDataReadResult()))))
  val read_error_delayed_result = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, Vec(DCacheWays, Bool()))))
  dontTouch(read_result)
  dontTouch(read_error_delayed_result)
  for (div_index <- 0 until DCacheSetDiv){
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
          io.read(i).valid && div_addrs(i) === div_index.U && (bank_addrs(i)(0) === bank_index.U || bank_addrs(i)(1) === bank_index.U && io.is128Req(i)) && way_en(i)(way_index)
        })))
        val readline_en = Wire(Bool())
        if (ReduceReadlineConflict) {
          readline_en := io.readline.valid && io.readline.bits.rmask(bank_index) && line_way_en(way_index) && div_index.U === line_div_addr
        } else {
          readline_en := io.readline.valid && line_way_en(way_index) && div_index.U === line_div_addr
        }
        val sram_set_addr = Mux(readline_en,
          addr_to_dcache_div_set(io.readline.bits.addr),
          PriorityMux(Seq.tabulate(LoadPipelineWidth)(i => loadpipe_en(i) -> set_addrs(i)))
        )
        val read_en = loadpipe_en.asUInt.orR || readline_en
        // read raw data
        val data_bank = data_banks(div_index)(bank_index)(way_index)
        data_bank.io.r.en := read_en
        data_bank.io.r.addr := sram_set_addr
        val ecc_bank = ecc_banks(div_index)(bank_index)(way_index)
        ecc_bank.io.r.req.valid := read_en
        ecc_bank.io.r.req.bits.apply(setIdx = sram_set_addr)

        read_result(div_index)(bank_index)(way_index).raw_data := data_bank.io.r.data
        read_result(div_index)(bank_index)(way_index).ecc := ecc_bank.io.r.resp.data(0)

        // use ECC to check error
        val ecc_data = read_result(div_index)(bank_index)(way_index).asECCData()
        val ecc_data_delayed = RegEnable(ecc_data, RegNext(read_en))
        read_result(div_index)(bank_index)(way_index).error_delayed := dcacheParameters.dataCode.decode(ecc_data_delayed).error
        read_error_delayed_result(div_index)(bank_index)(way_index) := read_result(div_index)(bank_index)(way_index).error_delayed
      }
    }
  }

  val data_read_oh = WireInit(VecInit(Seq.fill(DCacheSetDiv * DCacheBanks * DCacheWays)(0.U(1.W))))
  for(div_index <- 0 until DCacheSetDiv){
    for (bank_index <- 0 until DCacheBanks) {
      for (way_index <- 0 until DCacheWays) {
        data_read_oh(div_index *  DCacheBanks * DCacheWays + bank_index * DCacheBanks + way_index) := data_banks(div_index)(bank_index)(way_index).io.r.en
      }
    }
  }
  XSPerfAccumulate("data_read_counter", PopCount(Cat(data_read_oh)))

  // read result: expose banked read result
  val read_result_delayed = RegNext(read_result)
  (0 until LoadPipelineWidth).map(i => {
    // io.read_resp(i) := read_result(RegNext(bank_addrs(i)))(RegNext(OHToUInt(way_en(i))))
    val rr_read_fire = RegNext(RegNext(io.read(i).fire))
    val rr_div_addr = RegNext(RegNext(div_addrs(i)))
    val rr_bank_addr = RegNext(RegNext(bank_addrs(i)))
    val rr_way_addr = RegNext(RegNext(OHToUInt(way_en(i))))
    (0 until VLEN/DCacheSRAMRowBits).map( j =>{
      io.read_resp_delayed(i)(j) := read_result_delayed(rr_div_addr)(rr_bank_addr(j))(rr_way_addr)
      // error detection
      // normal read ports
      if(enableEcc) {
        io.read_error_delayed(i)(j) := rr_read_fire && read_error_delayed_result(rr_div_addr)(rr_bank_addr(j))(rr_way_addr) && !RegNext(io.bank_conflict_slow(i))
      }else {
        io.read_error_delayed(i)(j) := RegNext(rr_read_fire && read_error_delayed_result(rr_div_addr)(rr_bank_addr(j))(rr_way_addr) && !RegNext(io.bank_conflict_slow(i)))
      }
    })
  })

  // readline port
  (0 until DCacheBanks).map(i => {
    io.readline_resp(i) := read_result(RegNext(line_div_addr))(i)(RegNext(OHToUInt(io.readline.bits.way_en)))
  })
  io.readline_error_delayed := RegNext(RegNext(io.readline.fire)) &&
    VecInit((0 until DCacheBanks).map(i => io.readline_resp(i).error_delayed)).asUInt.orR

  // write data_banks & ecc_banks
  for (div_index <- 0 until DCacheSetDiv) {
    for (bank_index <- 0 until DCacheBanks) {
      for (way_index <- 0 until DCacheWays) {
        // data write
        val wen_reg = write_bank_mask_reg(bank_index) &&
          write_valid_dup_reg(bank_index) &&
          write_div_addr_dup_reg(bank_index) === div_index.U &&
          write_wayen_dup_reg(bank_index)(way_index)
        val data_bank = data_banks(div_index)(bank_index)(way_index)
        data_bank.io.w.en := wen_reg

        data_bank.io.w.addr := write_set_addr_dup_reg(bank_index)
        data_bank.io.w.data := write_data_reg(bank_index)
        // ecc write
        val ecc_bank = ecc_banks(div_index)(bank_index)(way_index)
        ecc_bank.io.w.req.valid := wen_reg
        ecc_bank.io.w.req.bits.apply(
          setIdx = write_set_addr_dup_reg(bank_index),
          data = RegNext(getECCFromEncWord(cacheParams.dataCode.encode((io.write.bits.data(bank_index))))),
          waymask = 1.U
        )
        when(ecc_bank.io.w.req.valid) {
          XSDebug("write in ecc sram: bank %x set %x data %x waymask %x\n",
            bank_index.U,
            addr_to_dcache_div_set(io.write.bits.addr),
            getECCFromEncWord(cacheParams.dataCode.encode((io.write.bits.data(bank_index)))),
            io.write.bits.way_en
          );
        }
      }
    }
  }

  require(nWays <= 32)
  io.cacheOp.resp.bits := DontCare
  val cacheOpShouldResp = WireInit(false.B)
  val eccReadResult = Wire(Vec(DCacheBanks, UInt(eccBits.W)))
  // DCacheDupNum is 16
  // vec: the dupIdx for every bank and every group
  val rdata_dup_vec = Seq(0,0,1,1,2,2,3,3)
  val rdataEcc_dup_vec = Seq(4,4,5,5,6,6,7,7)
  val wdata_dup_vec = Seq(8,8,9,9,10,10,11,11)
  val wdataEcc_dup_vec = Seq(12,12,13,13,14,14,15,15)
  val cacheOpDivAddr = set_to_dcache_div(io.cacheOp.req.bits.index)
  val cacheOpSetAddr = set_to_dcache_div_set(io.cacheOp.req.bits.index)
  val cacheOpWayNum = io.cacheOp.req.bits.wayNum(4, 0)
  rdata_dup_vec.zipWithIndex.map{ case(dupIdx, bankIdx) =>
    for (divIdx <- 0 until DCacheSetDiv){
      for (wayIdx <- 0 until DCacheWays) {
        when(io.cacheOp_req_dup(dupIdx).valid && CacheInstrucion.isReadData(io.cacheOp_req_bits_opCode_dup(dupIdx))) {
          val data_bank = data_banks(divIdx)(bankIdx)(wayIdx)
          data_bank.io.r.en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(wayIdx) && cacheOpDivAddr === divIdx.U
          data_bank.io.r.addr := cacheOpSetAddr
          cacheOpShouldResp := true.B
        }
      }
    }
  }
  rdataEcc_dup_vec.zipWithIndex.map{ case(dupIdx, bankIdx) =>
    for (divIdx <- 0 until DCacheSetDiv) {
      for (wayIdx <- 0 until DCacheWays) {
        when(io.cacheOp_req_dup(dupIdx).valid && CacheInstrucion.isReadDataECC(io.cacheOp_req_bits_opCode_dup(dupIdx))) {
          val ecc_bank = ecc_banks(divIdx)(bankIdx)(wayIdx)
          ecc_bank.io.r.req.valid := true.B
          ecc_bank.io.r.req.bits.setIdx := cacheOpSetAddr
          cacheOpShouldResp := true.B
        }
      }
    }
  }
  wdata_dup_vec.zipWithIndex.map{ case(dupIdx, bankIdx) =>
    for (divIdx <- 0 until DCacheSetDiv) {
      for (wayIdx <- 0 until DCacheWays) {
        when(io.cacheOp_req_dup(dupIdx).valid && CacheInstrucion.isWriteData(io.cacheOp_req_bits_opCode_dup(dupIdx))) {
          val data_bank = data_banks(divIdx)(bankIdx)(wayIdx)
          data_bank.io.w.en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(wayIdx) && cacheOpDivAddr === divIdx.U
          data_bank.io.w.addr := cacheOpSetAddr
          data_bank.io.w.data := io.cacheOp.req.bits.write_data_vec(bankIdx)
          cacheOpShouldResp := true.B
        }
      }
    }
  }
  wdataEcc_dup_vec.zipWithIndex.map{ case(dupIdx, bankIdx) =>
    for (divIdx <- 0 until DCacheSetDiv) {
      for (wayIdx <- 0 until DCacheWays) {
        when(io.cacheOp_req_dup(dupIdx).valid && CacheInstrucion.isWriteDataECC(io.cacheOp_req_bits_opCode_dup(dupIdx))) {
          val ecc_bank = ecc_banks(divIdx)(bankIdx)(wayIdx)
          ecc_bank.io.w.req.valid := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))(wayIdx) && cacheOpDivAddr === divIdx.U
          ecc_bank.io.w.req.bits.apply(
            setIdx = cacheOpSetAddr,
            data = io.cacheOp.req.bits.write_data_ecc,
            waymask = 1.U
          )
          cacheOpShouldResp := true.B
        }
      }
    }
  }

  io.cacheOp.resp.valid := RegNext(io.cacheOp.req.valid && cacheOpShouldResp)
  for (bank_index <- 0 until DCacheBanks) {
    io.cacheOp.resp.bits.read_data_vec(bank_index) := read_result(RegNext(cacheOpDivAddr))(bank_index)(RegNext(cacheOpWayNum)).raw_data
    eccReadResult(bank_index) := read_result(RegNext(cacheOpDivAddr))(bank_index)(RegNext(cacheOpWayNum)).ecc
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
    (0 until (VLEN/DCacheSRAMRowBits)).map(i => {
      bankConflictData.bank_index(i) := bank_addrs(0)(i)
    })
    bankConflictData.way_index  := OHToUInt(way_en(0))
    bankConflictData.fake_rr_bank_conflict := set_addrs(0) === set_addrs(1) && div_addrs(0) === div_addrs(1)
  }.otherwise {
    (0 until (VLEN/DCacheSRAMRowBits)).map(i => {
      bankConflictData.bank_index(i) := 0.U
    })
    bankConflictData.way_index := 0.U
    bankConflictData.fake_rr_bank_conflict := false.B
  }

  val isWriteBankConflictTable = WireInit(Constantin.createRecord("isWriteBankConflictTable" + p(XSCoreParamsKey).HartId.toString))
  bankConflictTable.log(
    data = bankConflictData,
    en = isWriteBankConflictTable.orR && rr_bank_conflict(0)(1),
    site = siteName,
    clock = clock,
    reset = reset
  )

  (1 until LoadPipelineWidth).foreach(y => (0 until y).foreach(x =>
    XSPerfAccumulate(s"data_array_fake_rr_bank_conflict_${x}_${y}", rr_bank_conflict(x)(y) && set_addrs(x)===set_addrs(y) && div_addrs(x) === div_addrs(y))
  ))

}

// the smallest access unit is bank
class BankedDataArray(implicit p: Parameters) extends AbstractBankedDataArray {
  println("  DCacheType: BankedDataArray")
  val ReduceReadlineConflict = false

  io.write.ready := true.B
  io.write_dup.foreach(_.ready := true.B)

  val data_banks = List.fill(DCacheSetDiv)(List.tabulate(DCacheBanks)(i => Module(new DataSRAMBank(i))))
  val ecc_banks = List.fill(DCacheSetDiv)(List.fill(DCacheBanks)(Module(new SRAMTemplate(
    Bits(eccBits.W),
    set = DCacheSets / DCacheSetDiv,
    way = DCacheWays,
    shouldReset = false,
    holdRead = false,
    singlePort = true
  ))))

  data_banks.map(_.map(_.dump()))

  val way_en = Wire(Vec(LoadPipelineWidth, io.read(0).bits.way_en.cloneType))
  val set_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val div_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val bank_addrs = Wire(Vec(LoadPipelineWidth, Vec(VLEN/DCacheSRAMRowBits, UInt())))
  val way_en_reg = Wire(Vec(LoadPipelineWidth, io.read(0).bits.way_en.cloneType))
  val set_addrs_reg = Wire(Vec(LoadPipelineWidth, UInt()))

  val line_set_addr = addr_to_dcache_div_set(io.readline.bits.addr)
  val line_div_addr = addr_to_dcache_div(io.readline.bits.addr)
  val line_way_en = io.readline.bits.way_en

  val write_bank_mask_reg = RegNext(io.write.bits.wmask)
  val write_data_reg = RegNext(io.write.bits.data)
  val write_valid_reg = RegNext(io.write.valid)
  val write_valid_dup_reg = io.write_dup.map(x => RegNext(x.valid))
  val write_wayen_dup_reg = io.write_dup.map(x => RegNext(x.bits.way_en))
  val write_set_addr_dup_reg = io.write_dup.map(x => RegNext(addr_to_dcache_div_set(x.bits.addr)))
  val write_div_addr_dup_reg = io.write_dup.map(x => RegNext(addr_to_dcache_div(x.bits.addr)))

  // read data_banks and ecc_banks
  // for single port SRAM, do not allow read and write in the same cycle
  val rwhazard = RegNext(io.write.valid)
  val rrhazard = false.B // io.readline.valid
  (0 until LoadPipelineWidth).map(rport_index => {
    div_addrs(rport_index) := addr_to_dcache_div(io.read(rport_index).bits.addr)
    bank_addrs(rport_index)(0) := addr_to_dcache_bank(io.read(rport_index).bits.addr)
    bank_addrs(rport_index)(1) := Mux(io.is128Req(rport_index), bank_addrs(rport_index)(0) + 1.U, DCacheBanks.asUInt)
    set_addrs(rport_index) := addr_to_dcache_div_set(io.read(rport_index).bits.addr)
    set_addrs_reg(rport_index) := RegNext(addr_to_dcache_div_set(io.read(rport_index).bits.addr))

    // use way_en to select a way after data read out
    assert(!(RegNext(io.read(rport_index).fire && PopCount(io.read(rport_index).bits.way_en) > 1.U)))
    way_en(rport_index) := io.read(rport_index).bits.way_en
    way_en_reg(rport_index) := RegNext(io.read(rport_index).bits.way_en)
  })

  // read each bank, get bank result
  val rr_bank_conflict = Seq.tabulate(LoadPipelineWidth)(x => Seq.tabulate(LoadPipelineWidth)(y =>
    io.read(x).valid && io.read(y).valid &&
    div_addrs(x) === div_addrs(y) &&
    (io.read(x).bits.bankMask & io.read(y).bits.bankMask) =/= 0.U
  ))
  val rrl_bank_conflict = Wire(Vec(LoadPipelineWidth, Bool()))
  val rrl_bank_conflict_intend = Wire(Vec(LoadPipelineWidth, Bool()))
  (0 until LoadPipelineWidth).foreach { i =>
    val judge = if (ReduceReadlineConflict) io.read(i).valid && (io.readline.bits.rmask & io.read(i).bits.bankMask) =/= 0.U && div_addrs(i) === line_div_addr
                else io.read(i).valid && div_addrs(i)===line_div_addr
    rrl_bank_conflict(i) := judge && io.readline.valid
    rrl_bank_conflict_intend(i) := judge && io.readline_intend
  }
  val wr_bank_conflict = Seq.tabulate(LoadPipelineWidth)(x =>
    io.read(x).valid &&
    write_valid_reg &&
    div_addrs(x) === write_div_addr_dup_reg.head &&
    (write_bank_mask_reg(bank_addrs(x)(0)) || write_bank_mask_reg(bank_addrs(x)(1)) && io.is128Req(x))
  )
  val wrl_bank_conflict = io.readline.valid && write_valid_reg && line_div_addr === write_div_addr_dup_reg.head
  // ready
  io.readline.ready := !(wrl_bank_conflict)
  io.read.zipWithIndex.map{case(x, i) => x.ready := !(wr_bank_conflict(i) || rrhazard)}

  val perf_multi_read = PopCount(io.read.map(_.valid)) >= 2.U
  (0 until LoadPipelineWidth).foreach(i => {
    // remove fake rr_bank_conflict situation in s2
    val real_other_bank_conflict_reg = RegNext(wr_bank_conflict(i) || rrl_bank_conflict(i))
    val real_rr_bank_conflict_reg = (if (i == 0) 0.B else (0 until i).map{ j =>
      RegNext(rr_bank_conflict(j)(i)) &&
      (way_en_reg(j) =/= way_en_reg(i) || set_addrs_reg(j) =/= set_addrs_reg(i))
    }.reduce(_ || _))
    io.bank_conflict_slow(i) := real_other_bank_conflict_reg || real_rr_bank_conflict_reg

    // get result in s1
    io.disable_ld_fast_wakeup(i) := wr_bank_conflict(i) || rrl_bank_conflict_intend(i) ||
      (if (i == 0) 0.B else (0 until i).map(rr_bank_conflict(_)(i)).reduce(_ || _))
  })
  XSPerfAccumulate("data_array_multi_read", perf_multi_read)
  (1 until LoadPipelineWidth).foreach(y => (0 until y).foreach(x =>
    XSPerfAccumulate(s"data_array_rr_bank_conflict_${x}_${y}", rr_bank_conflict(x)(y))
  ))
  (0 until LoadPipelineWidth).foreach(i => {
    XSPerfAccumulate(s"data_array_rrl_bank_conflict_${i}", rrl_bank_conflict(i))
    XSPerfAccumulate(s"data_array_rw_bank_conflict_${i}", wr_bank_conflict(i))
    XSPerfAccumulate(s"data_array_read_${i}", io.read(i).valid)
  })
  XSPerfAccumulate("data_array_access_total", PopCount(io.read.map(_.valid)))
  XSPerfAccumulate("data_array_read_line", io.readline.valid)
  XSPerfAccumulate("data_array_write", io.write.valid)

  val bank_result = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, new L1BankedDataReadResult())))
  val ecc_result = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, Vec(DCacheWays, UInt(eccBits.W)))))
  val read_bank_error_delayed = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, Bool())))
  dontTouch(bank_result)
  dontTouch(read_bank_error_delayed)
  for (div_index <- 0 until DCacheSetDiv) {
    for (bank_index <- 0 until DCacheBanks) {
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
      val bank_addr_matchs = WireInit(VecInit(List.tabulate(LoadPipelineWidth)(i => {
        io.read(i).valid && div_addrs(i) === div_index.U && (bank_addrs(i)(0) === bank_index.U || bank_addrs(i)(1) === bank_index.U && io.is128Req(i))
      })))
      val readline_match = Wire(Bool())
      if (ReduceReadlineConflict) {
        readline_match := io.readline.valid && io.readline.bits.rmask(bank_index) && line_div_addr === div_index.U
      } else {
        readline_match := io.readline.valid && line_div_addr === div_index.U
      }
      val bank_way_en = Mux(readline_match,
        io.readline.bits.way_en,
        PriorityMux(Seq.tabulate(LoadPipelineWidth)(i => bank_addr_matchs(i) -> way_en(i)))
      )
      // it is too long of bank_way_en's caculation, so bank_way_en_reg can not be caculated by RegNext(bank_way_en)
      val bank_way_en_reg = Mux(RegNext(readline_match),
        RegNext(io.readline.bits.way_en),
        PriorityMux(Seq.tabulate(LoadPipelineWidth)(i => RegNext(bank_addr_matchs(i)) -> RegNext(way_en(i))))
      )
      val bank_set_addr = Mux(readline_match,
        line_set_addr,
        PriorityMux(Seq.tabulate(LoadPipelineWidth)(i => bank_addr_matchs(i) -> set_addrs(i)))
      )

      val read_enable = bank_addr_matchs.asUInt.orR || readline_match

      // read raw data
      val data_bank = data_banks(div_index)(bank_index)
      data_bank.io.r.en := read_enable
      data_bank.io.r.way_en := bank_way_en
      data_bank.io.r.addr := bank_set_addr
      bank_result(div_index)(bank_index).raw_data := data_bank.io.r.data

      // read ECC
      val ecc_bank = ecc_banks(div_index)(bank_index)
      ecc_bank.io.r.req.valid := read_enable
      ecc_bank.io.r.req.bits.apply(setIdx = bank_set_addr)
      ecc_result(div_index)(bank_index) := ecc_bank.io.r.resp.data
      bank_result(div_index)(bank_index).ecc := Mux1H(bank_way_en_reg, ecc_bank.io.r.resp.data)

      // use ECC to check error
      val ecc_data = bank_result(div_index)(bank_index).asECCData()
      val ecc_data_delayed = RegEnable(ecc_data, RegNext(read_enable))
      bank_result(div_index)(bank_index).error_delayed := dcacheParameters.dataCode.decode(ecc_data_delayed).error
      read_bank_error_delayed(div_index)(bank_index) := bank_result(div_index)(bank_index).error_delayed
    }
  }

  val data_read_oh = WireInit(VecInit(Seq.fill(DCacheSetDiv)(0.U(XLEN.W))))
  for (div_index <- 0 until DCacheSetDiv){
    val temp = WireInit(VecInit(Seq.fill(DCacheBanks)(0.U(XLEN.W))))
    for (bank_index <- 0 until DCacheBanks) {
      temp(bank_index) := PopCount(Fill(DCacheWays, data_banks(div_index)(bank_index).io.r.en.asUInt))
    }
    data_read_oh(div_index) := temp.reduce(_ + _)
  }
  XSPerfAccumulate("data_read_counter", data_read_oh.foldLeft(0.U)(_ + _))

  val bank_result_delayed = RegNext(bank_result)
  (0 until LoadPipelineWidth).map(i => {
    val rr_read_fire = RegNext(RegNext(io.read(i).fire))
    val rr_div_addr = RegNext(RegNext(div_addrs(i)))
    val rr_bank_addr = RegNext(RegNext(bank_addrs(i)))
    val rr_way_addr = RegNext(RegNext(OHToUInt(way_en(i))))
    (0 until VLEN/DCacheSRAMRowBits).map( j =>{
      io.read_resp_delayed(i)(j) := bank_result_delayed(rr_div_addr)(rr_bank_addr(j))
      // error detection
      if(enableEcc) {
        io.read_error_delayed(i)(j) := rr_read_fire && read_bank_error_delayed(rr_div_addr)(rr_bank_addr(j)) && !RegNext(io.bank_conflict_slow(i))
      }else {
        io.read_error_delayed(i)(j) := RegNext(rr_read_fire && read_bank_error_delayed(rr_div_addr)(rr_bank_addr(j)) && !RegNext(io.bank_conflict_slow(i)))
      }
    })
  })

  // read result: expose banked read result
  io.readline_resp := bank_result(RegNext(line_div_addr))
  io.readline_error_delayed := RegNext(RegNext(io.readline.fire)) &&
    VecInit((0 until DCacheBanks).map(i => io.readline_resp(i).error_delayed)).asUInt.orR

  // write data_banks & ecc_banks
  for (div_index <- 0 until DCacheSetDiv) {
    for (bank_index <- 0 until DCacheBanks) {
      // data write
      val wen_reg = write_bank_mask_reg(bank_index) &&
        write_valid_dup_reg(bank_index) &&
        write_div_addr_dup_reg(bank_index) === div_index.U
      val data_bank = data_banks(div_index)(bank_index)
      data_bank.io.w.en := wen_reg
      data_bank.io.w.way_en := write_wayen_dup_reg(bank_index)
      data_bank.io.w.addr := write_set_addr_dup_reg(bank_index)
      data_bank.io.w.data := write_data_reg(bank_index)

      // ecc write
      val ecc_bank = ecc_banks(div_index)(bank_index)
      ecc_bank.io.w.req.valid := wen_reg
      ecc_bank.io.w.req.bits.apply(
        setIdx = write_set_addr_dup_reg(bank_index),
        data = RegNext(getECCFromEncWord(cacheParams.dataCode.encode((io.write.bits.data(bank_index))))),
        waymask = write_wayen_dup_reg(bank_index)
      )
      when(ecc_bank.io.w.req.valid) {
        XSDebug("write in ecc sram: bank %x set %x data %x waymask %x\n",
          bank_index.U,
          addr_to_dcache_div_set(io.write.bits.addr),
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
  // DCacheDupNum is 16
  // vec: the dupIdx for every bank and every group
  val rdata_dup_vec = Seq(0, 0, 1, 1, 2, 2, 3, 3)
  val rdataEcc_dup_vec = Seq(4, 4, 5, 5, 6, 6, 7, 7)
  val wdata_dup_vec = Seq(8, 8, 9, 9, 10, 10, 11, 11)
  val wdataEcc_dup_vec = Seq(12, 12, 13, 13, 14, 14, 15, 15)
  val cacheOpDivAddr = set_to_dcache_div(io.cacheOp.req.bits.index)
  val cacheOpSetAddr = set_to_dcache_div_set(io.cacheOp.req.bits.index)
  val cacheOpWayMask = UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
  rdata_dup_vec.zipWithIndex.map{ case(dupIdx, bankIdx) =>
    for (divIdx <- 0 until DCacheSetDiv) {
      when(io.cacheOp_req_dup(dupIdx).valid && CacheInstrucion.isReadData(io.cacheOp_req_bits_opCode_dup(dupIdx))) {
        val data_bank = data_banks(divIdx)(bankIdx)
        data_bank.io.r.en := true.B
        data_bank.io.r.way_en := cacheOpWayMask
        data_bank.io.r.addr := cacheOpSetAddr
        cacheOpShouldResp := true.B
      }
    }
  }
  rdataEcc_dup_vec.zipWithIndex.map{ case(dupIdx, bankIdx) =>
    for (divIdx <- 0 until DCacheSetDiv) {
      when(io.cacheOp_req_dup(dupIdx).valid && CacheInstrucion.isReadDataECC(io.cacheOp_req_bits_opCode_dup(dupIdx))) {
        val ecc_bank = ecc_banks(divIdx)(bankIdx)
        ecc_bank.io.r.req.valid := true.B
        ecc_bank.io.r.req.bits.setIdx := cacheOpSetAddr
        cacheOpShouldResp := true.B
      }
    }
  }
  wdata_dup_vec.zipWithIndex.map{ case(dupIdx, bankIdx) =>
    for (divIdx <- 0 until DCacheSetDiv) {
      when(io.cacheOp_req_dup(dupIdx).valid && CacheInstrucion.isWriteData(io.cacheOp_req_bits_opCode_dup(dupIdx))) {
        val data_bank = data_banks(divIdx)(bankIdx)
        data_bank.io.w.en := cacheOpDivAddr === divIdx.U
        data_bank.io.w.way_en := cacheOpWayMask
        data_bank.io.w.addr := cacheOpSetAddr
        data_bank.io.w.data := io.cacheOp.req.bits.write_data_vec(bankIdx)
        cacheOpShouldResp := true.B
      }
    }
  }
  wdataEcc_dup_vec.zipWithIndex.map{ case(dupIdx, bankIdx) =>
    for (divIdx <- 0 until DCacheSetDiv) {
      when(io.cacheOp_req_dup(dupIdx).valid && CacheInstrucion.isWriteDataECC(io.cacheOp_req_bits_opCode_dup(dupIdx))) {
        val ecc_bank = ecc_banks(divIdx)(bankIdx)
        ecc_bank.io.w.req.valid := cacheOpDivAddr === divIdx.U
        ecc_bank.io.w.req.bits.apply(
          setIdx = cacheOpSetAddr,
          data = io.cacheOp.req.bits.write_data_ecc,
          waymask = cacheOpWayMask
        )
        cacheOpShouldResp := true.B
      }
    }
  }

  io.cacheOp.resp.valid := RegNext(io.cacheOp.req.valid && cacheOpShouldResp)
  for (bank_index <- 0 until DCacheBanks) {
    io.cacheOp.resp.bits.read_data_vec(bank_index) := bank_result(RegNext(cacheOpDivAddr))(bank_index).raw_data
    eccReadResult(bank_index) := Mux1H(RegNext(cacheOpWayMask), ecc_result(RegNext(cacheOpDivAddr))(bank_index))
  }

  io.cacheOp.resp.bits.read_data_ecc := Mux(io.cacheOp.resp.valid,
    eccReadResult(RegNext(io.cacheOp.req.bits.bank_num)),
    0.U
  )

  val tableName = "BankConflict" + p(XSCoreParamsKey).HartId.toString
  val siteName = "BankedDataArray" + p(XSCoreParamsKey).HartId.toString
  val bankConflictTable = ChiselDB.createTable(tableName, new BankConflictDB)
  val bankConflictData = Wire(new BankConflictDB)
  for (i <- 0 until LoadPipelineWidth) {
    bankConflictData.set_index(i) := set_addrs(i)
    bankConflictData.addr(i) := io.read(i).bits.addr
  }

  // FIXME: rr_bank_conflict(0)(1) no generalization
  when(rr_bank_conflict(0)(1)) {
    (0 until (VLEN/DCacheSRAMRowBits)).map(i => {
      bankConflictData.bank_index(i) := bank_addrs(0)(i)
    })
    bankConflictData.way_index := OHToUInt(way_en(0))
    bankConflictData.fake_rr_bank_conflict := set_addrs(0) === set_addrs(1) && div_addrs(0) === div_addrs(1)
  }.otherwise {
    (0 until (VLEN/DCacheSRAMRowBits)).map(i => {
      bankConflictData.bank_index(i) := 0.U
    })
    bankConflictData.way_index := 0.U
    bankConflictData.fake_rr_bank_conflict := false.B
  }

  val isWriteBankConflictTable = WireInit(Constantin.createRecord("isWriteBankConflictTable" + p(XSCoreParamsKey).HartId.toString))
  bankConflictTable.log(
    data = bankConflictData,
    en = isWriteBankConflictTable.orR && rr_bank_conflict(0)(1),
    site = siteName,
    clock = clock,
    reset = reset
  )

  (1 until LoadPipelineWidth).foreach(y => (0 until y).foreach(x =>
    XSPerfAccumulate(s"data_array_fake_rr_bank_conflict_${x}_${y}", rr_bank_conflict(x)(y) && set_addrs(x) === set_addrs(y) && div_addrs(x) === div_addrs(y))
  ))

}
