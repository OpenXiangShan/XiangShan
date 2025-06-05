/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] Gurindar S. Sohi, and Manoj Franklin. "[High-bandwidth data memory systems for superscalar processors.]
* (https://doi.org/10.1145/106972.106980)" 4th International Conference on Architectural Support for Programming
* Languages and Operating Systems (ASPLOS). 1991.
***************************************************************************************/

package xiangshan.cache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utils._
import utility._
import utility.sram.SRAMTemplate
import chisel3.util._
import utility.mbist.MbistPipeline
import xiangshan.mem.LqPtr
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
  val addr_dup = Bits(PAddrBits.W)
  val bankMask = Bits(DCacheBanks.W)
  val kill = Bool()
  val lqIdx = new LqPtr
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
  val ecc = Bits(dataECCBits.W)
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
  val data = UInt(encDataBits.W)
}

// wrap a sram
class DataSRAM(bankIdx: Int, wayIdx: Int)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val w = new Bundle() {
      val en = Input(Bool())
      val addr = Input(UInt())
      val data = Input(UInt(encDataBits.W))
    }

    val r = new Bundle() {
      val en = Input(Bool())
      val addr = Input(UInt())
      val data = Output(UInt(encDataBits.W))
    }
  })

  // data sram
  val data_sram = Module(new SRAMTemplate(
    Bits(encDataBits.W),
    set = DCacheSets / DCacheSetDiv,
    way = 1,
    shouldReset = false,
    holdRead = false,
    singlePort = true,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl
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
    XSDebug(RegNext(io.r.en),
      "bank read set %x bank %x way %x data %x\n",
      RegEnable(io.r.addr, io.r.en),
      bankIdx.U,
      wayIdx.U,
      io.r.data
    )
  }

  def dump_w() = {
    XSDebug(io.w.en,
      "bank write set %x bank %x way %x data %x\n",
      io.w.addr,
      bankIdx.U,
      wayIdx.U,
      io.w.data
    )
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
      val data = Output(Vec(DCacheWays, UInt(encDataBits.W)))
    }
  })

  assert(RegNext(!io.w.en || PopCount(io.w.way_en) <= 1.U))

  // external controls do not read and write at the same time
  val w_info = io.w
  // val rw_bypass = RegNext(io.w.addr === io.r.addr && io.w.way_en === io.r.way_en && io.w.en)

  // multiway data bank
  val data_bank = Seq.fill(DCacheWays) {
    Module(new SRAMTemplate(
      Bits(encDataBits.W),
      set = DCacheSets / DCacheSetDiv,
      way = 1,
      shouldReset = false,
      holdRead = false,
      singlePort = true,
      withClockGate = true,
      hasMbist = hasMbist,
      hasSramCtl = hasSramCtl,
      suffix = Some("dcsh_dat")
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

  io.r.data := data_bank.map(_.io.r.resp.data(0))

  def dump_r() = {
    XSDebug(RegNext(io.r.en),
      "bank read addr %x data %x\n",
      RegEnable(io.r.addr, io.r.en),
      io.r.data.asUInt
    )
  }

  def dump_w() = {
    XSDebug(io.w.en,
      "bank write addr %x way_en %x data %x\n",
      io.w.addr,
      io.w.way_en,
      io.w.data
    )
  }

  def dump() = {
    dump_w()
    dump_r()
  }
}

case object HasDataEccParam

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
  val DataEccParam = if(EnableDataEcc) Some(HasDataEccParam) else None
  val ReadlinePortErrorIndex = LoadPipelineWidth
  val io = IO(new DCacheBundle {
    // load pipeline read word req
    val read = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new L1BankedDataReadReqWithMask)))
    val is128Req = Input(Vec(LoadPipelineWidth, Bool()))
    // main pipeline read / write line req
    val readline_intend = Input(Bool())
    val readline = Flipped(DecoupledIO(new L1BankedDataReadLineReq))
    val readline_can_go = Input(Bool())
    val readline_stall = Input(Bool())
    val readline_can_resp = Input(Bool())
    val write = Flipped(DecoupledIO(new L1BankedDataWriteReq))
    val write_dup = Vec(DCacheBanks, Flipped(Decoupled(new L1BankedDataWriteReqCtrl)))
    // data for readline and loadpipe
    val readline_resp = Output(Vec(DCacheBanks, new L1BankedDataReadResult()))
    val readline_error = Output(Bool())
    val readline_error_delayed = Output(Bool())
    val read_resp          = Output(Vec(LoadPipelineWidth, Vec(VLEN/DCacheSRAMRowBits, new L1BankedDataReadResult())))
    val read_error_delayed = Output(Vec(LoadPipelineWidth,Vec(VLEN/DCacheSRAMRowBits, Bool())))
    // val nacks = Output(Vec(LoadPipelineWidth, Bool()))
    // val errors = Output(Vec(LoadPipelineWidth + 1, ValidIO(new L1CacheErrorInfo))) // read ports + readline port
    // when bank_conflict, read (1) port should be ignored
    val bank_conflict_slow = Output(Vec(LoadPipelineWidth, Bool()))
    val disable_ld_fast_wakeup = Output(Vec(LoadPipelineWidth, Bool()))
    val pseudo_error = Flipped(DecoupledIO(Vec(DCacheBanks, new CtrlUnitSignalingBundle)))
  })

  // bank (0, 1, 2, 3) each way use duplicate addr
  def DuplicatedQueryBankSeq = Seq(0, 1, 2, 3)

  def pipeMap[T <: Data](f: Int => T) = VecInit((0 until LoadPipelineWidth).map(f))

  def getECCFromEncWord(encWord: UInt) = {
    if (EnableDataEcc) {
      require(encWord.getWidth == encDataBits, s"encDataBits=$encDataBits != encDataBits=$encDataBits!")
      encWord(encDataBits-1, DCacheSRAMRowBits)
    } else {
      0.U
    }
  }

  def getDataFromEncWord(encWord: UInt) = {
    encWord(DCacheSRAMRowBits-1, 0)
  }

  def asECCData(ecc: UInt, data: UInt) = {
    if (EnableDataEcc) {
      Cat(ecc, data)
    } else {
      data
    }
  }

  def dumpRead = {
    (0 until LoadPipelineWidth) map { w =>
      XSDebug(io.read(w).valid,
        s"DataArray Read channel: $w valid way_en: %x addr: %x\n",
        io.read(w).bits.way_en, io.read(w).bits.addr)
    }
    XSDebug(io.readline.valid,
      s"DataArray Read Line, valid way_en: %x addr: %x rmask %x\n",
      io.readline.bits.way_en, io.readline.bits.addr, io.readline.bits.rmask)
  }

  def dumpWrite = {
    XSDebug(io.write.valid,
      s"DataArray Write valid way_en: %x addr: %x\n",
      io.write.bits.way_en, io.write.bits.addr)

    (0 until DCacheBanks) map { r =>
      XSDebug(io.write.valid,
        s"cycle: $r data: %x wmask: %x\n",
        io.write.bits.data(r), io.write.bits.wmask(r))
    }
  }

  def dumpResp = {
    XSDebug(s"DataArray ReadeResp channel:\n")
    (0 until LoadPipelineWidth) map { r =>
      XSDebug(s"cycle: $r data: %x\n", Mux(io.is128Req(r),
        Cat(io.read_resp(r)(1).raw_data,io.read_resp(r)(0).raw_data),
        io.read_resp(r)(0).raw_data))
    }
  }

  def dump() = {
    dumpRead
    dumpWrite
    dumpResp
  }

  def selcetOldestPort(valid: Seq[Bool], bits: Seq[LqPtr], index: Seq[UInt]):((Bool, LqPtr), UInt) = {
    require(valid.length == bits.length &&  bits.length == index.length, s"length must eq, valid:${valid.length}, bits:${bits.length}, index:${index.length}")
    ParallelOperation(valid zip bits zip index,
      (a: ((Bool, LqPtr), UInt), b: ((Bool, LqPtr), UInt)) => {
        val au = a._1._2
        val bu = b._1._2
        val aValid = a._1._1
        val bValid = b._1._1
        val bSel = au > bu
        val bits = Mux(
          aValid && bValid,
          Mux(bSel, b._1._2, a._1._2),
          Mux(aValid && !bValid, a._1._2, b._1._2)
        )
        val idx = Mux(
          aValid && bValid,
          Mux(bSel, b._2, a._2),
          Mux(aValid && !bValid, a._2, b._2)
        )
        ((aValid || bValid, bits), idx)
      }
    )
  }

}

// the smallest access unit is sram
class SramedDataArray(implicit p: Parameters) extends AbstractBankedDataArray {
  println("  DCacheType: SramedDataArray")
  val ReduceReadlineConflict = false

  io.write.ready := true.B
  io.write_dup.foreach(_.ready := true.B)

  val data_banks = List.tabulate(DCacheSetDiv)( k => {
    val banks = List.tabulate(DCacheBanks)(i => List.tabulate(DCacheWays)(j => Module(new DataSRAM(i,j))))
    val mbistPl = MbistPipeline.PlaceMbistPipeline(1, s"MbistPipeDataSet$k", hasMbist)
    banks
  })
  data_banks.map(_.map(_.map(_.dump())))

  val way_en = Wire(Vec(LoadPipelineWidth, io.read(0).bits.way_en.cloneType))
  val set_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val div_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val bank_addrs = Wire(Vec(LoadPipelineWidth, Vec(VLEN/DCacheSRAMRowBits, UInt())))

  val line_set_addr = addr_to_dcache_div_set(io.readline.bits.addr)
  val line_div_addr = addr_to_dcache_div(io.readline.bits.addr)
  // when WPU is enabled, line_way_en is all enabled when read data
  val line_way_en = Fill(DCacheWays, 1.U) // val line_way_en = io.readline.bits.way_en
  val line_way_en_reg = RegEnable(io.readline.bits.way_en, 0.U(DCacheWays.W),io.readline.valid)

  val write_bank_mask_reg = RegEnable(io.write.bits.wmask, 0.U(DCacheBanks.W), io.write.valid)
  val write_data_reg = RegEnable(io.write.bits.data, io.write.valid)
  val write_valid_reg = RegNext(io.write.valid)
  val write_valid_dup_reg = io.write_dup.map(x => RegNext(x.valid))
  val write_wayen_dup_reg = io.write_dup.map(x => RegEnable(x.bits.way_en, 0.U(DCacheWays.W), x.valid))
  val write_set_addr_dup_reg = io.write_dup.map(x => RegEnable(addr_to_dcache_div_set(x.bits.addr), x.valid))
  val write_div_addr_dup_reg = io.write_dup.map(x => RegEnable(addr_to_dcache_div(x.bits.addr), x.valid))

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
  val rr_bank_conflict = Seq.tabulate(LoadPipelineWidth)(x => Seq.tabulate(LoadPipelineWidth)(y => {
    if (x == y) {
      false.B
    } else {
      io.read(x).valid && io.read(y).valid &&
        div_addrs(x) === div_addrs(y) &&
        (io.read(x).bits.bankMask & io.read(y).bits.bankMask) =/= 0.U &&
        io.read(x).bits.way_en === io.read(y).bits.way_en &&
        set_addrs(x) =/= set_addrs(y)
    }
  }))
  val load_req_with_bank_conflict = rr_bank_conflict.map(_.reduce(_ || _))
  val load_req_valid = io.read.map(_.valid)
  val load_req_lqIdx = io.read.map(_.bits.lqIdx)
  val load_req_index = (0 until LoadPipelineWidth).map(_.asUInt)


  val load_req_bank_conflict_selcet = selcetOldestPort(load_req_with_bank_conflict, load_req_lqIdx, load_req_index)
  val load_req_bank_select_port  = UIntToOH(load_req_bank_conflict_selcet._2).asBools

  val rr_bank_conflict_oldest = (0 until LoadPipelineWidth).map(i =>
    !load_req_bank_select_port(i) && load_req_with_bank_conflict(i)
  )

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
    rr_bank_conflict_oldest(i)
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
  val read_result_delayed = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, Vec(DCacheWays,new L1BankedDataReadResult()))))
  val read_error_delayed_result = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, Vec(DCacheWays, Bool()))))
  dontTouch(read_result)
  dontTouch(read_error_delayed_result)

  val pseudo_data_toggle_mask = io.pseudo_error.bits.map {
    case bank =>
      Mux(io.pseudo_error.valid && bank.valid, bank.mask, 0.U)
  }
  val readline_hit = io.readline.fire &&
                     (io.readline.bits.rmask & VecInit(io.pseudo_error.bits.map(_.valid)).asUInt).orR
  val readbank_hit = io.read.zip(bank_addrs.zip(io.is128Req)).zipWithIndex.map {
                          case ((read, (bank_addr, is128Req)), i) =>
                            val error_bank0 = io.pseudo_error.bits(bank_addr(0))
                            val error_bank1 = io.pseudo_error.bits(bank_addr(1))
                            read.fire && (error_bank0.valid || error_bank1.valid && is128Req) && !io.bank_conflict_slow(i)
                      }.reduce(_|_)
  io.pseudo_error.ready := RegNext(readline_hit || readbank_hit)

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
          io.read(i).valid && div_addrs(i) === div_index.U && (bank_addrs(i)(0) === bank_index.U || bank_addrs(i)(1) === bank_index.U && io.is128Req(i)) &&
          way_en(i)(way_index) &&
          !rr_bank_conflict_oldest(i)
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

        read_result(div_index)(bank_index)(way_index).ecc := getECCFromEncWord(data_bank.io.r.data)
        read_result(div_index)(bank_index)(way_index).raw_data := getDataFromEncWord(data_bank.io.r.data) ^ pseudo_data_toggle_mask(bank_index)

        if (EnableDataEcc) {
          val ecc_data = read_result(div_index)(bank_index)(way_index).asECCData()
          val ecc_data_delayed = RegEnable(ecc_data, RegNext(read_en))
          read_result(div_index)(bank_index)(way_index).error_delayed := dcacheParameters.dataCode.decode(ecc_data_delayed).error
          read_error_delayed_result(div_index)(bank_index)(way_index) := read_result(div_index)(bank_index)(way_index).error_delayed
        } else {
          read_result(div_index)(bank_index)(way_index).error_delayed := false.B
          read_error_delayed_result(div_index)(bank_index)(way_index) := false.B
        }

        read_result_delayed(div_index)(bank_index)(way_index) := RegEnable(read_result(div_index)(bank_index)(way_index), RegNext(read_en))
      }
    }
  }

  val data_read_oh = WireInit(VecInit(Seq.fill(DCacheSetDiv * DCacheBanks * DCacheWays)(0.U(1.W))))
  for(div_index <- 0 until DCacheSetDiv){
    for (bank_index <- 0 until DCacheBanks) {
      for (way_index <- 0 until DCacheWays) {
        data_read_oh(div_index *  DCacheBanks * DCacheWays + bank_index * DCacheWays + way_index) := data_banks(div_index)(bank_index)(way_index).io.r.en
      }
    }
  }
  XSPerfAccumulate("data_read_counter", PopCount(Cat(data_read_oh)))

  // read result: expose banked read result
  // TODO: clock gate
  (0 until LoadPipelineWidth).map(i => {
    // io.read_resp(i) := read_result(RegNext(bank_addrs(i)))(RegNext(OHToUInt(way_en(i))))
    val r_read_fire = RegNext(io.read(i).fire)
    val r_div_addr  = RegEnable(div_addrs(i), io.read(i).fire)
    val r_bank_addr = RegEnable(bank_addrs(i), io.read(i).fire)
    val r_way_addr  = RegNext(OHToUInt(way_en(i)))
    val rr_read_fire = RegNext(RegNext(io.read(i).fire))
    val rr_div_addr = RegEnable(RegEnable(div_addrs(i), io.read(i).fire), r_read_fire)
    val rr_bank_addr = RegEnable(RegEnable(bank_addrs(i), io.read(i).fire), r_read_fire)
    val rr_way_addr = RegEnable(RegEnable(OHToUInt(way_en(i)), io.read(i).fire), r_read_fire)
    (0 until VLEN/DCacheSRAMRowBits).map( j =>{
      io.read_resp(i)(j) := read_result(r_div_addr)(r_bank_addr(j))(r_way_addr)
      // error detection
      // normal read ports
      io.read_error_delayed(i)(j) := rr_read_fire && read_error_delayed_result(rr_div_addr)(rr_bank_addr(j))(rr_way_addr) && !RegNext(io.bank_conflict_slow(i))
    })
  })

  // readline port
  val readline_error_delayed = Wire(Vec(DCacheBanks, Bool()))
  val readline_r_way_addr = RegEnable(OHToUInt(io.readline.bits.way_en), io.readline.valid)
  val readline_rr_way_addr = RegEnable(readline_r_way_addr, RegNext(io.readline.valid))
  val readline_r_div_addr = RegEnable(line_div_addr, io.readline.valid)
  val readline_rr_div_addr = RegEnable(readline_r_div_addr, RegNext(io.readline.valid))
  (0 until DCacheBanks).map(i => {
    io.readline_resp(i) := read_result(readline_r_div_addr)(i)(readline_r_way_addr)
    readline_error_delayed(i) := read_result(readline_rr_div_addr)(i)(readline_rr_way_addr).error_delayed
  })
  io.readline_error := RegNext(RegNext(io.readline.fire)) && readline_error_delayed.asUInt.orR
  io.readline_error_delayed := RegNext(RegNext(io.readline.fire)) && readline_error_delayed.asUInt.orR

  // write data_banks & ecc_banks
  for (div_index <- 0 until DCacheSetDiv) {
    for (bank_index <- 0 until DCacheBanks) {
      for (way_index <- 0 until DCacheWays) {
        // data write
        val wen_reg = write_bank_mask_reg(bank_index) &&
          write_valid_dup_reg(bank_index) &&
          write_div_addr_dup_reg(bank_index) === div_index.U &&
          write_wayen_dup_reg(bank_index)(way_index)
        val write_ecc_reg = RegEnable(getECCFromEncWord(cacheParams.dataCode.encode(io.write.bits.data(bank_index))), io.write.valid)
        val data_bank = data_banks(div_index)(bank_index)(way_index)
        data_bank.io.w.en := wen_reg
        data_bank.io.w.addr := write_set_addr_dup_reg(bank_index)
        data_bank.io.w.data := asECCData(write_ecc_reg, write_data_reg(bank_index))
      }
    }
  }

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

  val isWriteBankConflictTable = Constantin.createRecord(s"isWriteBankConflictTable${p(XSCoreParamsKey).HartId}")
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

  if (backendParams.debugEn){
    load_req_with_bank_conflict.map(dontTouch(_))
    dontTouch(read_result)
    dontTouch(read_error_delayed_result)
  }
}

// the smallest access unit is bank
class BankedDataArray(implicit p: Parameters) extends AbstractBankedDataArray {
  println("  DCacheType: BankedDataArray")
  val ReduceReadlineConflict = false

  io.write.ready := true.B
  io.write_dup.foreach(_.ready := true.B)

  val data_banks = Seq.tabulate(DCacheSetDiv, DCacheBanks)({(k, i) => Module(new DataSRAMBank(i))})
  val mbistPl = MbistPipeline.PlaceMbistPipeline(1, s"MbistPipeDCacheData", hasMbist)
  val mbistSramPorts = mbistPl.map(pl => Seq.tabulate(DCacheSetDiv, DCacheBanks, DCacheWays) ({ (i, j, k) =>
    pl.toSRAM(i * DCacheBanks * DCacheWays + j * DCacheWays + k)
  }))

  // read result: expose banked read result
  private val mbist_r_way = OHToUInt(mbistSramPorts.map(_.flatMap(_.map(w => Cat(w.map(_.re).reverse))).reduce(_ | _)).getOrElse(0.U(DCacheWays.W)))
  private val mbist_r_div = OHToUInt(mbistSramPorts.map(_.map(d => Cat(d.flatMap(w => w.map(_.re))).orR)).getOrElse(Seq.fill(DCacheSetDiv)(false.B)))
  private val mbist_ack = mbistPl.map(_.mbist.ack).getOrElse(false.B)

  data_banks.map(_.map(_.dump()))

  val way_en = Wire(Vec(LoadPipelineWidth, io.read(0).bits.way_en.cloneType))
  val set_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val set_addrs_dup = Wire(Vec(LoadPipelineWidth, UInt()))
  val div_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val div_addrs_dup = Wire(Vec(LoadPipelineWidth, UInt()))
  val bank_addrs = Wire(Vec(LoadPipelineWidth, Vec(VLEN/DCacheSRAMRowBits, UInt())))
  val bank_addrs_dup = Wire(Vec(LoadPipelineWidth, Vec(VLEN/DCacheSRAMRowBits, UInt())))
  val way_en_reg = Wire(Vec(LoadPipelineWidth, io.read(0).bits.way_en.cloneType))
  val set_addrs_reg = Wire(Vec(LoadPipelineWidth, UInt()))
  val set_addrs_dup_reg = Wire(Vec(LoadPipelineWidth, UInt()))

  val line_set_addr = addr_to_dcache_div_set(io.readline.bits.addr)
  val line_div_addr = addr_to_dcache_div(io.readline.bits.addr)
  val line_way_en = io.readline.bits.way_en

  val write_bank_mask_reg = RegEnable(io.write.bits.wmask, io.write.valid)
  val write_data_reg = RegEnable(io.write.bits.data, io.write.valid)
  val write_valid_reg = RegNext(io.write.valid)
  val write_valid_dup_reg = io.write_dup.map(x => RegNext(x.valid))
  val write_wayen_dup_reg = io.write_dup.map(x => RegEnable(x.bits.way_en, x.valid))
  val write_set_addr_dup_reg = io.write_dup.map(x => RegEnable(addr_to_dcache_div_set(x.bits.addr), x.valid))
  val write_div_addr_dup_reg = io.write_dup.map(x => RegEnable(addr_to_dcache_div(x.bits.addr), x.valid))

  // read data_banks and ecc_banks
  // for single port SRAM, do not allow read and write in the same cycle
  val rwhazard = RegNext(io.write.valid)
  val rrhazard = false.B // io.readline.valid
  (0 until LoadPipelineWidth).map(rport_index => {
    div_addrs(rport_index) := addr_to_dcache_div(io.read(rport_index).bits.addr)
    div_addrs_dup(rport_index) := addr_to_dcache_div(io.read(rport_index).bits.addr_dup)
    bank_addrs(rport_index)(0) := addr_to_dcache_bank(io.read(rport_index).bits.addr)
    bank_addrs(rport_index)(1) := Mux(io.is128Req(rport_index), bank_addrs(rport_index)(0) + 1.U, bank_addrs(rport_index)(0))
    bank_addrs_dup(rport_index)(0) := addr_to_dcache_bank(io.read(rport_index).bits.addr_dup)
    bank_addrs_dup(rport_index)(1) := Mux(io.is128Req(rport_index), bank_addrs_dup(rport_index)(0) + 1.U, bank_addrs_dup(rport_index)(0))
    set_addrs(rport_index) := addr_to_dcache_div_set(io.read(rport_index).bits.addr)
    set_addrs_dup(rport_index) := addr_to_dcache_div_set(io.read(rport_index).bits.addr_dup)
    set_addrs_reg(rport_index) := RegEnable(addr_to_dcache_div_set(io.read(rport_index).bits.addr), io.read(rport_index).valid)
    set_addrs_dup_reg(rport_index) := RegEnable(addr_to_dcache_div_set(io.read(rport_index).bits.addr_dup), io.read(rport_index).valid)

    // use way_en to select a way after data read out
    assert(!(RegNext(io.read(rport_index).fire && PopCount(io.read(rport_index).bits.way_en) > 1.U)))
    way_en(rport_index) := io.read(rport_index).bits.way_en
    way_en_reg(rport_index) := RegEnable(io.read(rport_index).bits.way_en, io.read(rport_index).valid)
  })

  // read each bank, get bank result
  val rr_bank_conflict = Seq.tabulate(LoadPipelineWidth)(x => Seq.tabulate(LoadPipelineWidth)(y => {
    if (x == y) {
      false.B
    } else {
      io.read(x).valid && io.read(y).valid &&
      div_addrs(x) === div_addrs(y) &&
      (io.read(x).bits.bankMask & io.read(y).bits.bankMask) =/= 0.U &&
      set_addrs(x) =/= set_addrs(y)
    }
  }
  ))

  val load_req_with_bank_conflict = rr_bank_conflict.map(_.reduce(_ || _))
  val load_req_valid = io.read.map(_.valid)
  val load_req_lqIdx = io.read.map(_.bits.lqIdx)
  val load_req_index = (0 until LoadPipelineWidth).map(_.asUInt)

  val load_req_bank_conflict_selcet = selcetOldestPort(load_req_with_bank_conflict, load_req_lqIdx, load_req_index)
  val load_req_bank_select_port  = UIntToOH(load_req_bank_conflict_selcet._2).asBools

  val rr_bank_conflict_oldest = (0 until LoadPipelineWidth).map(i =>
    !load_req_bank_select_port(i) && load_req_with_bank_conflict(i)
  )

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
    val real_rr_bank_conflict_reg = RegNext(rr_bank_conflict_oldest(i))
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

  val bank_result = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, Vec(DCacheWays, new L1BankedDataReadResult()))))
  val bank_result_delayed = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, Vec(DCacheWays, new L1BankedDataReadResult()))))
  val read_bank_error_delayed = Wire(Vec(DCacheSetDiv, Vec(DCacheBanks, Vec(DCacheWays, Bool()))))

  val pseudo_data_toggle_mask = io.pseudo_error.bits.map {
    case bank =>
      Mux(io.pseudo_error.valid && bank.valid, bank.mask, 0.U)
  }
  val readline_hit = io.readline.fire &&
                     (io.readline.bits.rmask & VecInit(io.pseudo_error.bits.map(_.valid)).asUInt).orR
  val readbank_hit = io.read.zip(bank_addrs.zip(io.is128Req)).zipWithIndex.map {
                          case ((read, (bank_addr, is128Req)), i) =>
                            val error_bank0 = io.pseudo_error.bits(bank_addr(0))
                            val error_bank1 = io.pseudo_error.bits(bank_addr(1))
                            read.fire && (error_bank0.valid || error_bank1.valid && is128Req) && !io.bank_conflict_slow(i)
                      }.reduce(_|_)
  io.pseudo_error.ready := RegNext(readline_hit || readbank_hit)

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
        io.read(i).valid && div_addrs(i) === div_index.U && (bank_addrs(i)(0) === bank_index.U || bank_addrs(i)(1) === bank_index.U && io.is128Req(i)) &&
          !rr_bank_conflict_oldest(i)
      })))
      val bank_addr_matchs_dup = WireInit(VecInit(List.tabulate(LoadPipelineWidth)(i => {
        io.read(i).valid && div_addrs_dup(i) === div_index.U && (bank_addrs_dup(i)(0) === bank_index.U || bank_addrs_dup(i)(1) === bank_index.U && io.is128Req(i)) &&
          !rr_bank_conflict_oldest(i)
      })))
      val readline_match = Wire(Bool())
      if (ReduceReadlineConflict) {
        readline_match := io.readline.valid && io.readline.bits.rmask(bank_index) && line_div_addr === div_index.U
      } else {
        readline_match := io.readline.valid && line_div_addr === div_index.U
      }

      val bank_set_addr = Mux(readline_match,
        line_set_addr,
        PriorityMux(Seq.tabulate(LoadPipelineWidth)(i => bank_addr_matchs(i) -> set_addrs(i)))
      )
      val bank_set_addr_dup = Mux(readline_match,
        line_set_addr,
        PriorityMux(Seq.tabulate(LoadPipelineWidth)(i => bank_addr_matchs_dup(i) -> set_addrs_dup(i)))
      )
      val read_enable = bank_addr_matchs.asUInt.orR || readline_match

      // read raw data
      val data_bank = data_banks(div_index)(bank_index)
      data_bank.io.r.en := read_enable

      if (DuplicatedQueryBankSeq.contains(bank_index)) {
        data_bank.io.r.addr := bank_set_addr_dup
      } else {
        data_bank.io.r.addr := bank_set_addr
      }
      for (way_index <- 0 until DCacheWays) {
        val mbistAck = mbistSramPorts.map(_(div_index)(bank_index)(way_index).ack).getOrElse(false.B)
        bank_result(div_index)(bank_index)(way_index).ecc := getECCFromEncWord(data_bank.io.r.data(way_index))
        bank_result(div_index)(bank_index)(way_index).raw_data := getDataFromEncWord(data_bank.io.r.data(way_index)) ^ Mux(mbistAck, 0.U, pseudo_data_toggle_mask(bank_index))

        if (EnableDataEcc) {
          val ecc_data = bank_result(div_index)(bank_index)(way_index).asECCData()
          val ecc_data_delayed = RegEnable(ecc_data, RegNext(read_enable))
          bank_result(div_index)(bank_index)(way_index).error_delayed := dcacheParameters.dataCode.decode(ecc_data_delayed).error
          read_bank_error_delayed(div_index)(bank_index)(way_index) := bank_result(div_index)(bank_index)(way_index).error_delayed
        } else {
          bank_result(div_index)(bank_index)(way_index).error_delayed := false.B
          read_bank_error_delayed(div_index)(bank_index)(way_index) := false.B
        }
        bank_result_delayed(div_index)(bank_index)(way_index) := RegEnable(bank_result(div_index)(bank_index)(way_index), RegNext(read_enable))
      }
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

  (0 until LoadPipelineWidth).map(i => {
    // 1 cycle after read fire(load s2)
    val r_read_fire = RegNext(io.read(i).fire)
    val r_div_addr = RegEnable(div_addrs(i), io.read(i).fire)
    val r_bank_addr = RegEnable(bank_addrs(i), io.read(i).fire)
    val r_way_addr = RegEnable(OHToUInt(way_en(i)), io.read(i).fire)
    // 2 cycles after read fire(load s3)
    val rr_read_fire = RegNext(r_read_fire)
    val rr_div_addr = RegEnable(RegEnable(div_addrs(i), io.read(i).fire), r_read_fire)
    val rr_bank_addr = RegEnable(RegEnable(bank_addrs(i), io.read(i).fire), r_read_fire)
    val rr_way_addr = RegEnable(RegEnable(OHToUInt(way_en(i)), io.read(i).fire), r_read_fire)
    (0 until VLEN/DCacheSRAMRowBits).map( j =>{
      io.read_resp(i)(j)          := bank_result(r_div_addr)(r_bank_addr(j))(r_way_addr)
      // error detection
      io.read_error_delayed(i)(j) := rr_read_fire && read_bank_error_delayed(rr_div_addr)(rr_bank_addr(j))(rr_way_addr) && !RegNext(io.bank_conflict_slow(i))
    })
  })

  val readline_error = Wire(Vec(DCacheBanks, Bool()))
  val readline_error_delayed = Wire(Vec(DCacheBanks, Bool()))
  val readline_r_way_addr = RegEnable(Mux(mbist_ack, mbist_r_way, OHToUInt(io.readline.bits.way_en)), io.readline.fire | mbist_ack)
  val readline_rr_way_addr = RegEnable(readline_r_way_addr, RegNext(io.readline.fire))
  val readline_r_div_addr = RegEnable(Mux(mbist_ack, mbist_r_div, line_div_addr), io.readline.fire | mbist_ack)
  val readline_rr_div_addr = RegEnable(readline_r_div_addr, RegNext(io.readline.fire))
  val readline_resp = Wire(io.readline_resp.cloneType)
  (0 until DCacheBanks).foreach(i => {
    mbistSramPorts.foreach(_.foreach(_(i).foreach(_.rdata := Cat(io.readline_resp(i).ecc, io.readline_resp(i).raw_data))))
    readline_resp(i) := Mux(
      io.readline_can_go | mbist_ack,
      bank_result(readline_r_div_addr)(i)(readline_r_way_addr),
      RegEnable(readline_resp(i), io.readline_stall | mbist_ack)
    )

    if (EnableDataEcc) {
      readline_error(i) := bank_result(readline_rr_div_addr)(i)(readline_rr_way_addr).error_delayed
     //
      val ecc_data_delayed = io.readline_resp(i).asECCData()
      readline_error_delayed(i) := dcacheParameters.dataCode.decode(ecc_data_delayed).error
    } else {
      readline_error(i) := false.B
      readline_error_delayed(i) := false.B
    }
  })
  io.readline_resp := RegEnable(readline_resp, io.readline_can_resp | mbist_ack)
  io.readline_error := readline_error.asUInt.orR
  io.readline_error_delayed := readline_error_delayed.asUInt.orR

  // write data_banks & ecc_banks
  for (div_index <- 0 until DCacheSetDiv) {
    for (bank_index <- 0 until DCacheBanks) {
      // data write
      val wen_reg = write_bank_mask_reg(bank_index) &&
        write_valid_dup_reg(bank_index) &&
        write_div_addr_dup_reg(bank_index) === div_index.U && RegNext(io.write.valid)
      val write_ecc_reg = RegEnable(getECCFromEncWord(cacheParams.dataCode.encode(io.write.bits.data(bank_index))), io.write.valid)
      val data_bank = data_banks(div_index)(bank_index)
      data_bank.io.w.en := wen_reg
      data_bank.io.w.way_en := write_wayen_dup_reg(bank_index)
      data_bank.io.w.addr := write_set_addr_dup_reg(bank_index)
      data_bank.io.w.data := asECCData(write_ecc_reg, write_data_reg(bank_index))
    }
  }

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

  val isWriteBankConflictTable = Constantin.createRecord(s"isWriteBankConflictTable${p(XSCoreParamsKey).HartId}")
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

  if (backendParams.debugEn){
    load_req_with_bank_conflict.map(dontTouch(_))
    dontTouch(bank_result)
    dontTouch(read_bank_error_delayed)
  }
}
