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

// For Dcache with Hash Tag Array
class HTADataArray(implicit p: Parameters) extends AbstractBankedDataArray {
  println("  DCache Data Array Type: HTADataArray")

  io.write.ready := true.B
  io.write_dup.foreach(_.ready := true.B)

  val data_banks = List.tabulate(DCacheSetDiv)( k => {
    val banks = List.tabulate(DCacheBanks)(i => List.tabulate(DCacheWays)(j => Module(new DataSRAM(i,j))))
    val mbistPl = MbistPipeline.PlaceMbistPipeline(1, s"MbistPipeDataSet$k", hasMbist)
    banks
  })
  data_banks.map(_.map(_.map(_.dump())))

  val way_en_htag = Wire(Vec(LoadPipelineWidth, io.read(0).bits.way_en.cloneType))
  val set_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val div_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val bank_addrs = Wire(Vec(LoadPipelineWidth, Vec(VLEN/DCacheSRAMRowBits, UInt())))

  val line_set_addr = addr_to_dcache_div_set(io.readline.bits.addr)
  val line_div_addr = addr_to_dcache_div(io.readline.bits.addr)
  val line_way_en = io.readline.bits.way_en

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
    bank_addrs(rport_index)(1) := Mux(io.is128Req(rport_index), bank_addrs(rport_index)(0) + 1.U, bank_addrs(rport_index)(0))

    // use way_en to select a way after data read out
    // assert(!(RegNext(io.read(rport_index).fire && PopCount(io.read(rport_index).bits.way_en) > 1.U)))
    way_en_htag(rport_index) := io.read(rport_index).bits.way_en
  })

//   // read conflict
//   val rr_bank_conflict = Seq.tabulate(LoadPipelineWidth)(x => Seq.tabulate(LoadPipelineWidth)(y => {
//     if (x == y) {
//       false.B
//     } else {
//       io.read(x).valid && io.read(y).valid &&
//         div_addrs(x) === div_addrs(y) &&
//         (io.read(x).bits.bankMask & io.read(y).bits.bankMask) =/= 0.U &&
//         io.read(x).bits.way_en === io.read(y).bits.way_en &&
//         set_addrs(x) =/= set_addrs(y)
//     }
//   }))
//   val load_req_with_bank_conflict = rr_bank_conflict.map(_.reduce(_ || _))
  val load_req_valid = io.read.map(_.valid)
  val load_req_lqIdx = io.read.map(_.bits.lqIdx)
//   val load_req_index = (0 until LoadPipelineWidth).map(_.asUInt)

  
  // Age matrix of lqIdx: if i < j and matrix(i, j) == true, then i is older than j
  val lqIdx_age_matrix = Seq.tabulate(LoadPipelineWidth)(i => Seq.tabulate(LoadPipelineWidth)(j => {
    if (i >= j) {
      false.B
    } else {
      io.read(i).bits.lqIdx < io.read(j).bits.lqIdx
    }
  }))

//   val load_req_bank_conflict_selcet = selcetOldestPort(load_req_with_bank_conflict, load_req_lqIdx, load_req_index)
//   val load_req_bank_select_port  = UIntToOH(load_req_bank_conflict_selcet._2).asBools

//   val rr_bank_conflict_oldest = (0 until LoadPipelineWidth).map(i =>
//     !load_req_bank_select_port(i) && load_req_with_bank_conflict(i)
//   )

  val rrl_bank_conflict = Wire(Vec(LoadPipelineWidth, Bool()))
  val rrl_bank_conflict_intend = Wire(Vec(LoadPipelineWidth, Bool()))
  (0 until LoadPipelineWidth).foreach { i =>
    // val judge = if (ReduceReadlineConflict) io.read(i).valid && (io.readline.bits.rmask & io.read(i).bits.bankMask) =/= 0.U && line_div_addr === div_addrs(i) && line_set_addr =/= set_addrs(i)
    //             else io.read(i).valid && line_div_addr === div_addrs(i) && line_set_addr =/= set_addrs(i)
    val judge = io.read(i).valid && line_div_addr === div_addrs(i) && line_set_addr =/= set_addrs(i) &&
                Mux(io.repl_dirty, (io.repl_way_en & way_en_htag(i)) =/= 0.U, true.B)
    rrl_bank_conflict(i) := judge && io.readline.valid
    rrl_bank_conflict_intend(i) := judge && io.readline_intend
  }
  val wr_bank_conflict = Seq.tabulate(LoadPipelineWidth)(x =>
    io.read(x).valid && write_valid_reg &&
    div_addrs(x) === write_div_addr_dup_reg.head &&
    // way_en(x) === write_wayen_dup_reg.head &&
    (write_bank_mask_reg(bank_addrs(x)(0)) || write_bank_mask_reg(bank_addrs(x)(1)) && io.is128Req(x)) &&
    (way_en_htag(x) & write_wayen_dup_reg.head) =/= 0.U
  )
  val wrl_bank_conflict = io.readline.valid && write_valid_reg && line_div_addr === write_div_addr_dup_reg.head &&
                          Mux(io.repl_dirty, (io.repl_way_en & write_wayen_dup_reg.head) =/= 0.U, true.B)
  // ready
  io.readline.ready := !(wrl_bank_conflict)
  io.read.zipWithIndex.map { case (x, i) => x.ready := !(wr_bank_conflict(i) || rrhazard) }

  val rr_bank_overlap = Seq.tabulate(LoadPipelineWidth)(x => Seq.tabulate(LoadPipelineWidth)(y => {
    if (x == y) {
      false.B
    } else {
      io.read(x).valid && io.read(y).valid &&
        div_addrs(x) === div_addrs(y) &&
        (io.read(x).bits.bankMask & io.read(y).bits.bankMask) =/= 0.U &&
        set_addrs(x) =/= set_addrs(y)
    }
  }))
  val rr_way_overlap = Seq.tabulate(LoadPipelineWidth)(x => Seq.tabulate(LoadPipelineWidth)(y => {
    if (x == y) {
      false.B
    } else {
      (way_en_htag(x).asUInt & way_en_htag(y).asUInt) =/= 0.U
    }
  }))
  val rr_conflict_evict = (0 until LoadPipelineWidth).map(i => {
    (0 until LoadPipelineWidth).map(j => {
      if (i > j) {
        lqIdx_age_matrix(j)(i) && rr_bank_overlap(j)(i) && rr_way_overlap(j)(i)
      } else if (i < j) {
        !lqIdx_age_matrix(i)(j) && rr_bank_overlap(i)(j) && rr_way_overlap(i)(j)
      } else {
        false.B
      }
    }).reduce(_ || _)
  })
  
  // rr_bank_conflict_oldest --> rr_conflict_evict
  val rr_bank_conflict = rr_bank_overlap

  val perf_multi_read = PopCount(io.read.map(_.valid)) >= 2.U
//   val bank_conflict_fast = Wire(Vec(LoadPipelineWidth, Bool()))
  (0 until LoadPipelineWidth).foreach(i => {
    // bank_conflict_fast(i) := wr_bank_conflict(i) || rrl_bank_conflict(i) ||
    // rr_bank_conflict_oldest(i)
    // io.bank_conflict_slow(i) := RegNext(bank_conflict_fast(i))
    io.bank_conflict_slow(i) := RegNext(wr_bank_conflict(i)) || RegNext(rrl_bank_conflict(i)) ||
                                RegNext(rr_conflict_evict(i))
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
        //   way_en(i)(way_index) &&
        //   !rr_bank_conflict_oldest(i)
          !rr_conflict_evict(i) &&
          way_en_htag(i)(way_index)
        })))
        val readline_en = Wire(Bool())
        // if (ReduceReadlineConflict) {
        //   readline_en := io.readline.valid && io.readline.bits.rmask(bank_index) && line_way_en(way_index) && div_index.U === line_div_addr
        // } else {
        //   readline_en := io.readline.valid && line_way_en(way_index) && div_index.U === line_div_addr
          readline_en := io.readline.valid && div_index.U === line_div_addr && Mux(io.repl_dirty, io.repl_way_en(way_index), true.B)
        // }
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
    val r_way_addr = OHToUInt(io.s2_tag_match_way(i))
    val rr_read_fire = RegNext(RegNext(io.read(i).fire))
    val rr_div_addr = RegEnable(RegEnable(div_addrs(i), io.read(i).fire), r_read_fire)
    val rr_bank_addr = RegEnable(RegEnable(bank_addrs(i), io.read(i).fire), r_read_fire)
    val rr_way_addr = RegEnable(r_way_addr, r_read_fire)
    (0 until VLEN/DCacheSRAMRowBits).map( j =>{
    //   io.read_resp(i)(j) := read_result(r_div_addr)(r_bank_addr(j))(r_way_addr)
      io.read_resp(i)(j) := Mux1H(io.s2_tag_match_way(i), read_result(r_div_addr)(r_bank_addr(j)))
      // error detection
      // normal read ports
      io.read_error_delayed(i)(j) := rr_read_fire && read_error_delayed_result(rr_div_addr)(rr_bank_addr(j))(rr_way_addr) && !RegNext(io.bank_conflict_slow(i))
    })
  })

  // readline port: latch read data at readline_can_go (one cycle after readline.fire) and
  // expose at readline_can_resp, matching SramedDataArray / MainPipe timing.
  val readline_error_delayed = Wire(Vec(DCacheBanks, Bool()))
  val readline_r_way_addr = RegEnable(OHToUInt(io.readline.bits.way_en), io.readline.fire)
  val readline_rr_way_addr = RegEnable(readline_r_way_addr, RegNext(io.readline.fire))
  val readline_r_div_addr = RegEnable(line_div_addr, io.readline.fire)
  val readline_rr_div_addr = RegEnable(readline_r_div_addr, RegNext(io.readline.fire))
  val readline_resp_hold = Wire(io.readline_resp.cloneType)
  (0 until DCacheBanks).map(i => {
    readline_resp_hold(i) := Mux(
      io.readline_can_go,
      read_result(readline_r_div_addr)(i)(readline_r_way_addr),
      RegEnable(readline_resp_hold(i), io.readline_stall)
    )
    readline_error_delayed(i) := read_result(readline_rr_div_addr)(i)(readline_rr_way_addr).error_delayed
  })
  io.readline_resp := RegEnable(readline_resp_hold, io.readline_can_resp)
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
    bankConflictData.way_index  := OHToUInt(way_en_htag(0))
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
    // rr_conflict_evict.map(dontTouch(_))
    dontTouch(read_result)
    dontTouch(read_error_delayed_result)
  }
}