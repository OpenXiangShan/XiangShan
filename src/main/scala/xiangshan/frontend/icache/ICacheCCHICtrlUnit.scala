// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

import annotation.unused
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utils.EnumUInt
import xiangshan.cache._
import oceanus.compactchi._

/*
 * I$ CtrlUnit as a serial Compact CHI Type 3 Completer.
 * ECC enable + software-triggered parity/ECC inject into meta or data array.
 * Mirrors ICacheCtrlUnit register semantics.
 */
class ICacheCCHICtrlUnit(implicit val p: Parameters) extends Module
  with HasICacheCtrlUnitParameters
  with ICacheMetaHelper {

  val io = IO(new Bundle {
    val cchi = Flipped(new CCHIType3DownPort)
    val eccEnable = Output(Bool())
    val injecting = Output(Bool())
    val metaRead = new MetaReadBundle
    val metaWrite = new MetaWriteBundle
    val dataWrite = new DataWriteBundle
  })

  private def nInjError: Int = 8
  private object EccCtrlInjError extends EnumUInt(nInjError) {
    def NotEnabled:    UInt = 0.U(width.W)
    def TargetInvalid: UInt = 1.U(width.W)
    def NotFound:      UInt = 2.U(width.W)
    @unused def Rsvd3: UInt = 3.U(width.W)
    @unused def Rsvd4: UInt = 4.U(width.W)
    @unused def Rsvd5: UInt = 5.U(width.W)
    @unused def Rsvd6: UInt = 6.U(width.W)
    @unused def Rsvd7: UInt = 7.U(width.W)
  }

  private def nInjStatus: Int = 8
  private object EccCtrlInjStatus extends EnumUInt(nInjStatus) {
    def Idle:     UInt = 0.U(width.W)
    def Working:  UInt = 1.U(width.W)
    def Injected: UInt = 2.U(width.W)
    def Error:    UInt = 7.U(width.W)
    @unused def Rsvd3: UInt = 3.U(width.W)
    @unused def Rsvd4: UInt = 4.U(width.W)
    @unused def Rsvd5: UInt = 5.U(width.W)
    @unused def Rsvd6: UInt = 6.U(width.W)
  }

  private def nInjTarget: Int = 4
  private object EccCtrlInjTarget extends EnumUInt(nInjTarget) {
    def MetaArray: UInt = 0.U(width.W)
    def DataArray: UInt = 2.U(width.W)
    @unused def Rsvd1: UInt = 1.U(width.W)
    @unused def Rsvd3: UInt = 3.U(width.W)
  }

  private class EccCtrlBundle extends Bundle {
    val iError:  UInt = EccCtrlInjError()
    val iStatus: UInt = EccCtrlInjStatus()
    val iTarget: UInt = EccCtrlInjTarget()
    val inject:  Bool = Bool()
    val enable:  Bool = Bool()
  }

  private object EccCtrlBundle {
    def default: EccCtrlBundle = {
      val x = Wire(new EccCtrlBundle)
      x.iError  := EccCtrlInjError.NotEnabled
      x.iStatus := EccCtrlInjStatus.Idle
      x.iTarget := EccCtrlInjTarget.MetaArray
      x.inject  := false.B
      x.enable  := true.B
      x
    }
  }

  private class EccIAddrBundle extends Bundle {
    val pAddr: UInt = UInt(PAddrBits.W)
  }

  private object EccIAddrBundle {
    def default: EccIAddrBundle = {
      val x = Wire(new EccIAddrBundle)
      x.pAddr := 0.U
      x
    }
  }

  private val eccCtrl = RegInit(EccCtrlBundle.default.asUInt)
  private val eccCtrlBundle = eccCtrl.asTypeOf(new EccCtrlBundle)
  private val eccIAddr = RegInit(EccIAddrBundle.default.asUInt)
  private val eccIAddrBundle = eccIAddr.asTypeOf(new EccIAddrBundle)

  require(RegWidth >= eccCtrlBundle.asUInt.getWidth)
  require(RegWidth >= eccIAddrBundle.asUInt.getWidth)

  io.eccEnable := eccCtrlBundle.enable
  io.injecting := eccCtrlBundle.iStatus === EccCtrlInjStatus.Working

  private val iVSetIdx = get_idx(eccIAddrBundle.pAddr)
  private val iPAddr = eccIAddrBundle.pAddr
  private val iPTag = get_phy_tag(iPAddr)
  private val iWaymask = RegInit(0.U(nWays.W))

  private def nInjectFsmState: Int = 5
  private object InjectFsmState extends EnumUInt(nInjectFsmState) {
    def Idle:         UInt = 0.U(width.W)
    def ReadMetaReq:  UInt = 1.U(width.W)
    def ReadMetaResp: UInt = 2.U(width.W)
    def WriteMeta:    UInt = 3.U(width.W)
    def WriteData:    UInt = 4.U(width.W)
  }
  private val iState = RegInit(InjectFsmState.Idle)

  io.metaRead.req.valid := iState === InjectFsmState.ReadMetaReq
  io.metaRead.req.bits.isDoubleLine := false.B
  io.metaRead.req.bits.vSetIdx := VecInit(Seq.fill(PortNumber)(iVSetIdx))

  io.metaWrite.req.valid := iState === InjectFsmState.WriteMeta
  io.metaWrite.req.bits.generate(
    phyTag = get_phy_tag(iPAddr),
    maybeRvcMap = 0.U,
    vSetIdx = iVSetIdx,
    waymask = iWaymask,
    poison = true.B
  )

  io.dataWrite.req.valid := iState === InjectFsmState.WriteData
  io.dataWrite.req.bits.generate(
    data = 0.U,
    vSetIdx = iVSetIdx,
    waymask = iWaymask,
    poison = true.B
  )

  switch(iState) {
    is(InjectFsmState.Idle) {
      when(eccCtrlBundle.iStatus === EccCtrlInjStatus.Working) {
        iState := InjectFsmState.ReadMetaReq
      }
    }
    is(InjectFsmState.ReadMetaReq) {
      when(io.metaRead.req.fire) {
        iState := InjectFsmState.ReadMetaResp
      }
    }
    is(InjectFsmState.ReadMetaResp) {
      val waymask = getWaymask(iPTag, io.metaRead.resp.entries.head)
      iWaymask := waymask
      when(!waymask.orR) {
        iState := InjectFsmState.Idle
        eccCtrl := {
          val next = WireDefault(eccCtrlBundle)
          next.iStatus := EccCtrlInjStatus.Error
          next.iError := EccCtrlInjError.NotFound
          next.asUInt
        }
      }.otherwise {
        iState := Mux(
          eccCtrlBundle.iTarget === EccCtrlInjTarget.MetaArray,
          InjectFsmState.WriteMeta,
          InjectFsmState.WriteData
        )
      }
    }
    is(InjectFsmState.WriteMeta) {
      when(io.metaWrite.req.fire) {
        iState := InjectFsmState.Idle
        eccCtrl := {
          val next = WireDefault(eccCtrlBundle)
          next.iStatus := EccCtrlInjStatus.Injected
          next.asUInt
        }
      }
    }
    is(InjectFsmState.WriteData) {
      when(io.dataWrite.req.fire) {
        iState := InjectFsmState.Idle
        eccCtrl := {
          val next = WireDefault(eccCtrlBundle)
          next.iStatus := EccCtrlInjStatus.Injected
          next.asUInt
        }
      }
    }
  }

  private val baseAddr = Address.base.U(48.W)
  private val regSpaceBytes = EccIAddrOffset + RegBytes

  private def mergeWrite(oldVal: UInt, newVal: UInt, be: UInt, width: Int): UInt = {
    val nBytes = width / 8
    VecInit((0 until nBytes).map { i =>
      Mux(be(i), newVal(8 * (i + 1) - 1, 8 * i), oldVal(8 * (i + 1) - 1, 8 * i))
    }).asUInt
  }

  private def readEccCtrl(): UInt = {
    val res = WireDefault(eccCtrlBundle)
    res.inject := false.B
    res.asUInt
  }

  private def applyEccCtrlWrite(data: UInt, be: UInt): Unit = {
    val req = mergeWrite(eccCtrl, data, be, RegWidth).asTypeOf(new EccCtrlBundle)
    val next = WireDefault(eccCtrlBundle)
    next.enable := req.enable
    when(req.inject && eccCtrlBundle.iStatus === EccCtrlInjStatus.Idle) {
      when(!req.enable) {
        next.iStatus := EccCtrlInjStatus.Error
        next.iError := EccCtrlInjError.NotEnabled
      }.elsewhen(req.iTarget =/= EccCtrlInjTarget.MetaArray && req.iTarget =/= EccCtrlInjTarget.DataArray) {
        next.iStatus := EccCtrlInjStatus.Error
        next.iError := EccCtrlInjError.TargetInvalid
      }.otherwise {
        next.iStatus := EccCtrlInjStatus.Working
      }
    }
    next.iTarget := req.iTarget
    eccCtrl := next.asUInt
  }

  private def readReg(offset: UInt): UInt = {
    Mux(offset === EccCtrlOffset.U, readEccCtrl(),
      Mux(offset === EccIAddrOffset.U, eccIAddr, 0.U(RegWidth.W)))
  }

  private def applyWrite(offset: UInt, data: UInt, be: UInt): Unit = {
    when(offset === EccCtrlOffset.U) {
      applyEccCtrlWrite(data, be)
    }.elsewhen(offset === EccIAddrOffset.U) {
      eccIAddr := mergeWrite(eccIAddr, data, be, RegWidth)
    }
  }

  private def offsetValid(offset: UInt): Bool = offset < regSpaceBytes.U

  val sIdle :: sSendCompData :: sSendCompDbid :: sWaitWrData :: Nil = Enum(4)
  val state = RegInit(sIdle)
  val latchedTxnId = Reg(UInt(8.W))
  val latchedTgtId = Reg(UInt(8.W))
  val latchedOffset = Reg(UInt(log2Ceil(regSpaceBytes.max(1)).W))

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
        assert(isReadReq || isWriteReq, "ICacheCCHICtrlUnit: unsupported REQ opcode")
        assert(offsetValid(reqOffset), "ICacheCCHICtrlUnit: register offset out of range")
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
        assert(CtrlUnitCCHI.Rx.isWrData(updat.bits.Opcode), "ICacheCCHICtrlUnit: expected NonCopyBackWrData")
        assert(updat.bits.TxnID === latchedTxnId, "ICacheCCHICtrlUnit: write DAT TxnID mismatch")
        applyWrite(latchedOffset, updat.bits.Data, updat.bits.BE)
        state := sIdle
      }
    }
  }

  // Clear iStatus/iError after eccCtrl read completes (same as TL RegReadFn side effect).
  private val completedReadOffset = Mux(state === sSendCompData, latchedOffset, reqOffset)
  when(dndat.fire && completedReadOffset === EccCtrlOffset.U) {
    val next = WireDefault(eccCtrlBundle)
    when(eccCtrlBundle.iStatus === EccCtrlInjStatus.Injected || eccCtrlBundle.iStatus === EccCtrlInjStatus.Error) {
      next.iStatus := EccCtrlInjStatus.Idle
      next.iError := EccCtrlInjError.NotEnabled
    }
    eccCtrl := next.asUInt
  }
}
