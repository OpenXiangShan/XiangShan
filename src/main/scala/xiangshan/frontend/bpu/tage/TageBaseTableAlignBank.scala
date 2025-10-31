// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu.tage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import utility.sram.SRAMTemplate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.SaturateCounter

class TageBaseTableAlignBank(
    alignIdx: Int
)(implicit p: Parameters) extends TageModule with Helpers {
  class TageBaseTableAlignBankIO extends Bundle {
    class Read extends Bundle {
      class Req extends Bundle {
        // NOTE: this startVAddr is not from Bpu top, it's calculated in TageBaseTable top
        // i.e. vecRotateRight(VecInit.tabulate(NumAlignBanks)(startVAddr + _ * alignSize), startAlignIdx)(alignIdx)
        val startVAddr: PrunedAddr = new PrunedAddr(VAddrBits)
      }

      class Resp extends Bundle {
        val takenCtrs: Vec[SaturateCounter] = Vec(FetchBlockAlignInstNum, new SaturateCounter(BaseTableTakenCtrWidth))
      }

      val req:  Valid[Req] = Flipped(Valid(new Req))
      val resp: Resp       = Output(new Resp)
    }

    class Write extends Bundle {
      class Req extends Bundle {
        // NOTE: this startVAddr is not from Bpu top, it's calculated in TageBaseTable top
        // i.e. vecRotateRight(VecInit.tabulate(NumAlignBanks)(startVAddr + _ * alignSize), startAlignIdx)(alignIdx)
        val startVAddr: PrunedAddr           = new PrunedAddr(VAddrBits)
        val takenCtrs:  Vec[SaturateCounter] = Vec(FetchBlockAlignInstNum, new SaturateCounter(BaseTableTakenCtrWidth))
        val wayMask:    UInt                 = UInt(FetchBlockAlignInstNum.W)
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    val resetDone: Bool  = Output(Bool())
    val read:      Read  = new Read
    val write:     Write = new Write
  }

  val io: TageBaseTableAlignBankIO = IO(new TageBaseTableAlignBankIO)

  /* *** alias *** */
  private val r = io.read
  private val w = io.write

  private val sramBanks = Seq.tabulate(NumBanks) { i =>
    Module(new SRAMTemplate(
      new SaturateCounter(BaseTableTakenCtrWidth),
      set = BaseTableNumSets,
      way = FetchBlockAlignInstNum,
      singlePort = true,
      shouldReset = true,
      holdRead = true,
      withClockGate = true,
      hasMbist = hasMbist,
      hasSramCtl = hasSramCtl
    )).suggestName(s"tage_sram_align${alignIdx}_bank${i}")
  }

  // use a write buffer to store the write requests when read and write are both valid
  private val writeBuffers = Seq.tabulate(NumBanks) { i =>
    Module(new Queue(
      new BaseTableSramWriteReq,
      WriteBufferSize,
      pipe = true,
      flow = true
    ))
  }

  io.resetDone := sramBanks.map(_.io.r.req.ready).reduce(_ && _)

  /* *** read *** */
  private val s0_fire       = r.req.valid
  private val s0_startVAddr = r.req.bits.startVAddr
  private val s0_bankIdx    = getBaseTableBankIndex(s0_startVAddr)
  private val s0_bankMask   = UIntToOH(s0_bankIdx, NumBanks)
  private val s0_setIdx     = getBaseTableSetIndex(s0_startVAddr)

  sramBanks.zipWithIndex.foreach { case (bank, i) =>
    bank.io.r.req.valid       := s0_fire && s0_bankMask(i)
    bank.io.r.req.bits.setIdx := s0_setIdx
  }

  private val s1_bankMask = RegEnable(s0_bankMask, s0_fire)

  io.read.resp.takenCtrs := Mux1H(s1_bankMask, sramBanks.map(_.io.r.resp.data))

  /* *** write *** */
  private val t1_valid      = w.req.valid
  private val t1_startVAddr = w.req.bits.startVAddr
  private val t1_takenCtrs  = w.req.bits.takenCtrs
  private val t1_wayMask    = w.req.bits.wayMask

  private val t1_setIdx   = getBaseTableSetIndex(t1_startVAddr)
  private val t1_bankIdx  = getBankIndex(t1_startVAddr)
  private val t1_bankMask = UIntToOH(t1_bankIdx, NumBanks)

  writeBuffers.zipWithIndex.foreach { case (buffer, bankIdx) =>
    buffer.io.enq.valid          := t1_valid && t1_bankMask(bankIdx)
    buffer.io.enq.bits.setIdx    := t1_setIdx
    buffer.io.enq.bits.takenCtrs := t1_takenCtrs
    buffer.io.enq.bits.wayMask   := t1_wayMask
  }

  // write back to sram
  (sramBanks zip writeBuffers).foreach { case (bank, buffer) =>
    val valid   = buffer.io.deq.valid && !bank.io.r.req.valid
    val data    = buffer.io.deq.bits.takenCtrs
    val setIdx  = buffer.io.deq.bits.setIdx
    val wayMask = buffer.io.deq.bits.wayMask
    bank.io.w.apply(valid, data, setIdx, wayMask)

    buffer.io.deq.ready := bank.io.w.req.ready && !bank.io.r.req.valid
  }

  /* *** perf *** */
  XSPerfAccumulate("train_update_ctr", t1_valid && t1_wayMask.orR)
  XSPerfAccumulate(
    "write_buffer_drop_write",
    PopCount(writeBuffers.map(b => !b.io.enq.ready && b.io.enq.valid))
  )
}
