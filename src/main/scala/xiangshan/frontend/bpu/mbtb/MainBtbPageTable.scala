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

package xiangshan.frontend.bpu.mbtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import utility.sram.SRAMTemplate

class MainBtbPageTable(implicit p: Parameters) extends MainBtbModule {
  class MainBtbPageTableIO extends Bundle {
    class ReadWay extends Bundle {
      class Req extends Bundle {
        val setIdx:  UInt = UInt(PageTableSetIdxLen.W)
        val wayMask: UInt = UInt(NumPageTableWay.W)
      }
      class Resp extends Bundle {
        val entry: MainBtbPageTableEntry = new MainBtbPageTableEntry
      }

      val req:  Valid[Req] = Flipped(Valid(new Req))
      val resp: Resp       = Output(new Resp)
    }
    class ReadSet extends Bundle {
      class Req extends Bundle {
        val setIdx: UInt = UInt(PageTableSetIdxLen.W)
      }
      class Resp extends Bundle {
        val entries: Vec[MainBtbPageTableEntry] = Vec(NumPageTableWay, new MainBtbPageTableEntry)
      }

      val req:  Valid[Req] = Flipped(Valid(new Req))
      val resp: Resp       = Output(new Resp)
    }

    class Write extends Bundle {
      class Req extends Bundle {
        val setIdx:  UInt                       = UInt(PageTableSetIdxLen.W)
        val wayMask: UInt                       = UInt(NumPageTableWay.W)
        val entries: Vec[MainBtbPageTableEntry] = Vec(NumPageTableWay, new MainBtbPageTableEntry)
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    val resetDone: Bool = Output(Bool())

    val readWay: ReadWay = new ReadWay
    val readSet: ReadSet = new ReadSet
    val write:   Write   = new Write

    val readConflicts: Vec[Bool] = Output(Vec(NumPageTableWay, Bool()))
  }

  val io: MainBtbPageTableIO = IO(new MainBtbPageTableIO)

  if (PageTableUseSRAM) {
    val sramImpl = Module(new MainBtbPageTableSRAM)
    io <> sramImpl.io
  } else {
    val regImpl = Module(new MainBtbPageTableRegister)
    io <> regImpl.io
  }

  private class MainBtbPageTableSRAM(implicit p: Parameters) extends MainBtbModule {
    val io: MainBtbPageTableIO = IO(new MainBtbPageTableIO)

    private val entrySrams = Seq.tabulate(NumPageTableWay) { wayIdx =>
      Module(
        new SRAMTemplate(
          new MainBtbPageTableEntry,
          set = NumPageTableSet,
          way = 1,
          singlePort = true,
          shouldReset = true,
          holdRead = true,
          withClockGate = true,
          hasMbist = hasMbist,
          hasSramCtl = hasSramCtl,
          suffix = Option("bpu_mbtb_page_table")
        )
      ).suggestName(s"mbtb_sram_page_table_way_${wayIdx}")
    }

    private val entryWriteBuffer = Seq.tabulate(NumPageTableWay) { _ =>
      Module(new Queue(
        new MainBtbPageTableEntrySramWriteReq,
        entries = WriteBufferSize,
        pipe = true,
        flow = true
      ))
    }

    private val resetDone = RegInit(false.B)
    when(entrySrams.map(_.io.r.req.ready).reduce(_ && _)) {
      resetDone := true.B
    }
    io.resetDone := resetDone

    // ===== sram <-> io =====

    private val readWayMaskReg   = RegNext(io.readWay.req.bits.wayMask)
    private val readConflictsReg = RegInit(VecInit(Seq.fill(NumPageTableWay)(false.B)))

    // read way (used for prediction) has higher priority than read set (used for training)
    entrySrams.zipWithIndex.foreach { case (sram, idx) =>
      val readWayValid = io.readWay.req.valid && io.readWay.req.bits.wayMask(idx)
      val readSetValid = io.readSet.req.valid
      sram.io.r.req.valid := readWayValid || readSetValid
      sram.io.r.req.bits.setIdx := Mux(
        readWayValid,
        io.readWay.req.bits.setIdx,
        io.readSet.req.bits.setIdx
      )
      // track read conflicts
      readConflictsReg(idx) := readWayValid && readSetValid
    }

    // reply to io in the next cycle
    io.readWay.resp.entry := Mux1H(
      readWayMaskReg,
      entrySrams.map(_.io.r.resp.data.head)
    )
    io.readSet.resp.entries := VecInit(entrySrams.map(_.io.r.resp.data.head))
    io.readConflicts        := readConflictsReg

    // ===== writeBuffer -> sram =====
    (entrySrams zip entryWriteBuffer).foreach { case (way, buf) =>
      way.io.w.req.valid        := buf.io.deq.valid && !way.io.r.req.valid
      way.io.w.req.bits.data(0) := buf.io.deq.bits.entry
      way.io.w.req.bits.setIdx  := buf.io.deq.bits.setIdx
      buf.io.deq.ready          := way.io.w.req.ready && !way.io.r.req.valid
    }

    // ===== io -> writeBuffer =====

    // page table writes ways only when finishing reading a same set,
    // so we don't need flush logic
    entryWriteBuffer.zipWithIndex.foreach { case (buf, i) =>
      buf.io.enq.valid       := io.write.req.valid && io.write.req.bits.wayMask(i)
      buf.io.enq.bits.setIdx := io.write.req.bits.setIdx
      buf.io.enq.bits.entry  := io.write.req.bits.entries(i)
    }

    (0 until NumPageTableWay).foreach { i =>
      XSPerfAccumulate(
        s"page_table_read_conflict_way_$i",
        io.readConflicts(i)
      )

      XSPerfAccumulate(
        s"page_table_writebuffer_drop_way_$i",
        io.write.req.valid && io.write.req.bits.wayMask(i) &&
          !entryWriteBuffer(i).io.enq.ready
      )
    }
  }

  private class MainBtbPageTableRegister(implicit p: Parameters) extends MainBtbModule {
    val io: MainBtbPageTableIO = IO(new MainBtbPageTableIO)

    private val table = Seq.fill(NumPageTableWay) {
      Reg(Vec(NumPageTableSet, new MainBtbPageTableEntry))
    }

    when(reset.asBool)(table.foreach(_.foreach(_.valid := false.B)))

    /* *** read way *** */
    private val readWayRespEntryReg = RegInit(0.U.asTypeOf(new MainBtbPageTableEntry))
    private val readWayEntry = Mux1H(
      io.readWay.req.bits.wayMask,
      VecInit(table.map(_(io.readWay.req.bits.setIdx)))
    )

    // write bypass
    private val writeHitReadWay = io.write.req.valid && io.readWay.req.valid &&
      (io.write.req.bits.setIdx === io.readWay.req.bits.setIdx) &&
      (io.write.req.bits.wayMask & io.readWay.req.bits.wayMask).orR
    private val writeBypassEntry = Mux1H(
      io.write.req.bits.wayMask & io.readWay.req.bits.wayMask,
      io.write.req.bits.entries
    )

    private val readWayRespEntry = MuxCase(
      readWayRespEntryReg,
      Seq(
        writeHitReadWay      -> writeBypassEntry,
        io.readWay.req.valid -> readWayEntry
      )
    )
    readWayRespEntryReg   := readWayRespEntry
    io.readWay.resp.entry := readWayRespEntryReg

    /* *** read set *** */
    private val readSetRespEntriesReg = RegInit(0.U.asTypeOf(Vec(NumPageTableWay, new MainBtbPageTableEntry)))
    private val readSetEntries        = VecInit(table.map(_(io.readSet.req.bits.setIdx)))

    // write bypass
    private val writeHitReadSet = io.write.req.valid && io.readSet.req.valid &&
      io.write.req.bits.setIdx === io.readSet.req.bits.setIdx
    private val writeBypassEntries = VecInit((0 until NumPageTableWay).map { i =>
      Mux(io.write.req.bits.wayMask(i), io.write.req.bits.entries(i), readSetEntries(i))
    })

    private val readSetRespEntries = MuxCase(
      readSetRespEntriesReg,
      Seq(
        writeHitReadSet      -> writeBypassEntries,
        io.readSet.req.valid -> readSetEntries
      )
    )

    readSetRespEntriesReg   := readSetRespEntries
    io.readSet.resp.entries := readSetRespEntriesReg

    /* *** write *** */
    when(io.write.req.valid) {
      for (i <- 0 until NumPageTableWay) {
        when(io.write.req.bits.wayMask(i)) {
          table(i)(io.write.req.bits.setIdx) := io.write.req.bits.entries(i)
        }
      }
    }

    io.readConflicts := VecInit.fill(NumPageTableWay)(false.B)
    io.resetDone     := true.B
  }
}
