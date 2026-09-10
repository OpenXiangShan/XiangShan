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
import chisel3.util._
import xiangshan._
import xiangshan.backend.datapath.NewPipelineConnect
import oceanus.compactchi._

/*
 * D$ CtrlUnit as a serial Compact CHI Type 3 Completer.
 * Write: CompDBIDResp then NonCopyBackWrData.
 * Read: CompData on dndat.
 */
class DCacheCCHICtrlUnit(params: L1CacheCtrlParams)(implicit val p: Parameters) extends Module with HasDCacheParameters {
  val io = IO(new Bundle {
    val cchi = Flipped(new CCHIType3DownPort)
    val pseudoError = Vec(params.nSignalComps, DecoupledIO(Vec(DCacheBanks, new CtrlUnitSignalingBundle)))
  })

  require(params.maxBanks > 0, "At least one bank!")
  require(params.maxBanks == 1, "Is it necessary to have more than 1 bank?")

  val ctrlRegs = RegInit(VecInit(Seq.fill(1)(0.U(params.regWidth.W))))
  val delayRegs = RegInit(VecInit(Seq.fill(1)(0.U(params.regWidth.W))))
  val tagMaskReg = RegInit(0.U(params.tagMaskRegWidth.W))
  val dataMaskRegs = RegInit(VecInit(Seq.fill(DCacheBanks)(0.U(DCacheSRAMRowBits.W))))
  val counterRegs = RegInit(VecInit(Seq.fill(1)(0.U(params.regWidth.W))))
  val pseudoError_gen = Wire(Vec(params.nSignalComps, DecoupledIO(Vec(DCacheBanks, new CtrlUnitSignalingBundle))))
  val ctrlReg = ctrlRegs.head
  val ctrlRegBundle = ctrlRegs.head.asTypeOf(new CtrlUnitCtrlBundle)
  val delayReg = delayRegs.head
  val counterReg = counterRegs.head

  require(log2Up(params.nSignalComps) == ctrlRegBundle.comp.getWidth, "comp width must cover pseudo-error components!")

  pseudoError_gen.zipWithIndex.foreach {
    case (inj, i) =>
      inj.valid := ctrlRegBundle.ese && (ctrlRegBundle.comp === i.U) && (!ctrlRegBundle.ede || counterReg === 0.U)
  }
  pseudoError_gen(0).bits.zip(ctrlRegBundle.bank.asBools).foreach {
    case (bankOut, bankEnable) =>
      bankOut.valid := bankEnable
      bankOut.mask := tagMaskReg(tagBits - 1, 0)
  }
  pseudoError_gen(1).bits.zip(ctrlRegBundle.bank.asBools).zip(dataMaskRegs).foreach {
    case ((bankOut, bankEnable), mask) =>
      bankOut.valid := bankEnable
      bankOut.mask := mask.pad(tagBits)
  }

  when(pseudoError_gen.map(_.fire).reduce(_ || _)) {
    val newCtrlReg = WireDefault(ctrlRegBundle)
    newCtrlReg.ese := Mux(ctrlRegBundle.persist, ctrlRegBundle.ese, false.B)

    when(newCtrlReg.ese && newCtrlReg.ede) {
      counterReg := Mux(newCtrlReg.persist, delayReg, 0.U)
    }
    ctrlReg := newCtrlReg.asUInt
  }

  ctrlRegs.map(_.asTypeOf(new CtrlUnitCtrlBundle)).zip(counterRegs).zipWithIndex.foreach {
    case ((ctl, cnt), i) =>
      when(ctl.ese && ctl.ede && cnt =/= 0.U) {
        cnt := cnt - 1.U
      }
  }

  for (i <- 0 until params.nSignalComps) {
    NewPipelineConnect(
      pseudoError_gen(i), io.pseudoError(i), io.pseudoError(i).fire, false.B,
      Option(s"DCacheCCHICtrlUnitPseudoErrorPipelineConnect${i}")
    )
  }

  private val baseAddr = params.address.base.U(48.W)
  private val regSpaceBytes = params.dataMaskOffset + params.dataMaskRegBytes * DCacheBanks

  private def mergeWrite(oldVal: UInt, newVal: UInt, be: UInt, width: Int): UInt = {
    val nBytes = width / 8
    VecInit((0 until nBytes).map { i =>
      Mux(be(i), newVal(8 * (i + 1) - 1, 8 * i), oldVal(8 * (i + 1) - 1, 8 * i))
    }).asUInt
  }

  private def readReg(offset: UInt): UInt = {
    val ctrlHit = offset === params.ctrlOffset.U
    val delayHit = offset === params.delayOffset.U
    val tagHit = offset === params.tagMaskOffset.U
    val dataMaskHits = Wire(Vec(DCacheBanks, Bool()))
    for (i <- 0 until DCacheBanks) {
      dataMaskHits(i) := offset === (params.dataMaskOffset + params.dataMaskRegBytes * i).U
    }
    val dataMaskRead = Mux1H(dataMaskHits, dataMaskRegs.map(_.pad(params.regWidth)))
    Mux(
      ctrlHit, ctrlRegs.head,
      Mux(
        delayHit, delayRegs.head,
        Mux(tagHit, tagMaskReg.pad(params.regWidth), Mux(dataMaskHits.asUInt.orR, dataMaskRead, 0.U(params.regWidth.W)))
      )
    )
  }

  private def applyWrite(offset: UInt, data: UInt, be: UInt): Unit = {
    when(offset === params.ctrlOffset.U) {
      ctrlRegs(0) := mergeWrite(ctrlRegs(0), data, be, params.regWidth)
    }.elsewhen(offset === params.delayOffset.U) {
      val merged = mergeWrite(delayRegs(0), data, be, params.regWidth)
      delayRegs(0) := merged
      counterRegs(0) := merged
    }.elsewhen(offset === params.tagMaskOffset.U) {
      tagMaskReg := mergeWrite(tagMaskReg, data, be, params.tagMaskRegWidth)
    }
    for (i <- 0 until DCacheBanks) {
      when(offset === (params.dataMaskOffset + params.dataMaskRegBytes * i).U) {
        dataMaskRegs(i) := mergeWrite(dataMaskRegs(i), data, be, params.dataMaskRegWidth)
      }
    }
  }

  private def offsetValid(offset: UInt): Bool = offset < regSpaceBytes.U

  val sIdle :: sSendCompData :: sSendCompDbid :: sWaitWrData :: Nil = Enum(4)
  val state = RegInit(sIdle)
  val latchedTxnId = Reg(UInt(8.W))
  val latchedTgtId = Reg(UInt(8.W))
  val latchedOffset = Reg(UInt(log2Ceil(regSpaceBytes).W))

  val req = io.cchi.req
  val updat = io.cchi.updat
  val dnrsp = io.cchi.dnrsp
  val dndat = io.cchi.dndat

  val reqOffset = req.bits.Addr - baseAddr
  val isReadReq = CCHIOpcode.ReadNoSnp.is(req.bits.Opcode, req.valid)
  val isWriteReq = CCHIOpcode.WriteNoSnpPtl.is(req.bits.Opcode, req.valid) ||
    CCHIOpcode.WriteNoSnpFull.is(req.bits.Opcode, req.valid)

  req.ready := false.B
  updat.ready := false.B
  dnrsp.valid := false.B
  dnrsp.bits := DontCare
  dndat.valid := false.B
  dndat.bits := DontCare

  switch(state) {
    is(sIdle) {
      req.ready := true.B
      when(req.fire) {
        assert(isReadReq || isWriteReq, "DCacheCCHICtrlUnit: unsupported REQ opcode")
        assert(offsetValid(reqOffset), "DCacheCCHICtrlUnit: register offset out of range")
        latchedTxnId := req.bits.TxnID
        latchedTgtId := req.bits.SrcID
        latchedOffset := reqOffset
        when(isReadReq) {
          dndat.valid := true.B
          CtrlUnitCCHI.Tx.compData(dndat.bits, req.bits.TxnID, req.bits.SrcID, readReg(reqOffset))
          when(!dndat.ready) {
            state := sSendCompData
          }
        }.elsewhen(isWriteReq) {
          dnrsp.valid := true.B
          CtrlUnitCCHI.Tx.compDbidResp(dnrsp.bits, req.bits.TxnID, req.bits.SrcID)
          when(dnrsp.ready) {
            state := sWaitWrData
          }.otherwise {
            state := sSendCompDbid
          }
        }
      }
    }
    is(sSendCompData) {
      dndat.valid := true.B
      CtrlUnitCCHI.Tx.compData(dndat.bits, latchedTxnId, latchedTgtId, readReg(latchedOffset))
      when(dndat.fire) {
        state := sIdle
      }
    }
    is(sSendCompDbid) {
      dnrsp.valid := true.B
      CtrlUnitCCHI.Tx.compDbidResp(dnrsp.bits, latchedTxnId, latchedTgtId)
      when(dnrsp.fire) {
        state := sWaitWrData
      }
    }
    is(sWaitWrData) {
      updat.ready := true.B
      when(updat.fire) {
        assert(CtrlUnitCCHI.Rx.isWrData(updat.bits.Opcode), "DCacheCCHICtrlUnit: expected NonCopyBackWrData")
        assert(updat.bits.TxnID === latchedTxnId, "DCacheCCHICtrlUnit: write DAT TxnID mismatch")
        applyWrite(latchedOffset, updat.bits.Data, updat.bits.BE)
        state := sIdle
      }
    }
  }
}
