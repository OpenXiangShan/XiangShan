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

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class ICacheDataArray(implicit p: Parameters) extends ICacheModule with ICacheDataHelper {
  class ICacheDataArrayIO(implicit p: Parameters) extends ICacheBundle {
    val write: DataWriteBundle = Flipped(new DataWriteBundle)
    val read:  DataReadBundle  = Flipped(new DataReadBundle)
  }

  val io: ICacheDataArrayIO = IO(new ICacheDataArrayIO)

  // sanity check
  require(DataSramWidth == (new ICacheDataEntry).getWidth)

  private val banks = Seq.tabulate(DataBanks)(i => Module(new ICacheDataBank(i)))

  /* *** read *** */
  private val r0_valid = io.read.req.valid
  private val r0_req   = io.read.req.bits
  dontTouch(r0_req)

  io.read.req.ready := banks.map(_.io.read.req.ready).reduce(_ && _)

  // Save req->(bank, way) ownership to route SRAM response back per-req in the next cycle.
  private val r0_reqPerBankWayMask = Wire(Vec(MaxFetchReqNum, Vec(DataBanks, UInt(nWays.W))))
  dontTouch(r0_reqPerBankWayMask)

  banks.zipWithIndex.foreach { case (bank, bankIdx) =>
    val reqReadBankValid = Wire(Vec(MaxFetchReqNum, Bool())).suggestName(s"bank_${bankIdx}_reqReadBankValid")
    val reqReadSetIdx    = Wire(Vec(MaxFetchReqNum, UInt(idxBits.W))).suggestName(s"bank_${bankIdx}_reqReadSetIdx")
    val reqReadWayValid  = Wire(Vec(MaxFetchReqNum, UInt(nWays.W))).suggestName(s"bank_${bankIdx}_reqReadWayValid")

    dontTouch(reqReadBankValid)
    dontTouch(reqReadSetIdx)
    dontTouch(reqReadWayValid)

    (0 until MaxFetchReqNum).foreach { i =>
      val lineSelOH = Cat(r0_req(i).bits.bankSel(1)(bankIdx), r0_req(i).bits.bankSel(0)(bankIdx))
      reqReadBankValid(i)              := lineSelOH.orR && r0_req(i).valid
      reqReadSetIdx(i)                 := Mux1H(lineSelOH, r0_req(i).bits.vSetIdx)
      reqReadWayValid(i)               := Mux1H(lineSelOH, r0_req(i).bits.waymask) & Fill(nWays, reqReadBankValid(i))
      r0_reqPerBankWayMask(i)(bankIdx) := reqReadWayValid(i)

      when(r0_valid && r0_req(i).valid) {
        assert(PopCount(lineSelOH) <= 1.U, "ICache Data Array: one req on one bank can only select one line")
        assert(PopCount(reqReadWayValid(i)) <= 1.U, "ICache Data Array: wayMask multi hit")
      }
    }

    val perWayReadValid  = Wire(Vec(nWays, Bool())).suggestName(s"bank_${bankIdx}_perWayReadValid")
    val perWayReadSetIdx = Wire(Vec(nWays, UInt(idxBits.W))).suggestName(s"bank_${bankIdx}_perWayReadSetIdx")

    dontTouch(perWayReadValid)
    dontTouch(perWayReadSetIdx)

    (0 until nWays).foreach { wayIdx =>
      val reqSel = VecInit((0 until MaxFetchReqNum).map(reqIdx => reqReadWayValid(reqIdx)(wayIdx)))
      perWayReadValid(wayIdx)  := reqSel.reduce(_ || _)
      perWayReadSetIdx(wayIdx) := PriorityMux(reqSel, reqReadSetIdx)

      when(r0_valid && reqSel.reduce(_ && _)) {
        assert(
          reqReadSetIdx(0) === reqReadSetIdx(1),
          "ICache Data Array: one way cannot be selected by multiple reqs with different setIdx!"
        )
      }
    }
    bank.io.read.req.valid        := r0_valid && reqReadBankValid.reduce(_ || _)
    bank.io.read.req.bits.setIdx  := perWayReadSetIdx
    bank.io.read.req.bits.waymask := perWayReadValid.asUInt
  }

  private val r1_reqPerBankWayMask = RegEnable(r0_reqPerBankWayMask, io.read.req.fire)

  (0 until MaxFetchReqNum).foreach { reqIdx =>
    (0 until DataBanks).foreach { bankIdx =>
      val wayMask = r1_reqPerBankWayMask(reqIdx)(bankIdx)
      io.read.resp(reqIdx).datas(bankIdx) := Mux1H(wayMask, banks(bankIdx).io.read.resp.entries.map(_.data))
      io.read.resp(reqIdx).codes(bankIdx) := Mux1H(wayMask, banks(bankIdx).io.read.resp.entries.map(_.code))
    }
  }

  // TEST: force ECC to fail by setting parity codes to 0
  if (ForceDataEccFail) {
    io.read.resp.foreach(_.codes.foreach(_ := 0.U))
  }

  /* *** write *** */
  private val w0_valid   = io.write.req.valid
  private val w0_setIdx  = io.write.req.bits.vSetIdx
  private val w0_waymask = io.write.req.bits.waymask
  private val w0_entries = io.write.req.bits.entries

  io.write.req.ready := banks.map(_.io.write.req.ready).reduce(_ && _)
  banks.zipWithIndex.foreach { case (b, i) =>
    b.io.write.req.valid        := w0_valid
    b.io.write.req.bits.setIdx  := w0_setIdx
    b.io.write.req.bits.waymask := w0_waymask
    b.io.write.req.bits.entry   := w0_entries(i)
  }
}
