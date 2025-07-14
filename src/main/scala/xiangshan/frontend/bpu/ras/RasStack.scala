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
//
// Acknowledgement
//
// This implementation is inspired by several key papers:
// [1] Kevin Skadron, Pritpal S. Ahuja, Margaret Martonosi, and Douglas W. Clark. "[Improving prediction for procedure
// returns with return-address-stack repair mechanisms.](https://doi.org/10.1109/MICRO.1998.742787)" 31st Annual
// ACM/IEEE International Symposium on Microarchitecture (MICRO). 1998.
// [2] Tan Hongze, and Wang Jian. "[A Return Address Predictor Based on Persistent Stack.]
// (https://crad.ict.ac.cn/en/article/doi/10.7544/issn1000-1239.202111274)" Journal of Computer Research and Development
// (CRAD) 60.6: 1337-1345. 2023.

package xiangshan.frontend.bpu.ras

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.CircularQueuePtr
import utility.HasCircularQueuePtrHelper
import utility.RegNextWithEnable
import utility.XSDebug
import utility.XSError
import utility.XSPerfAccumulate
import xiangshan.XSBundle
import xiangshan.XSCoreParamsKey
import xiangshan.XSModule
import xiangshan.frontend.BranchPredictionRedirect
import xiangshan.frontend.BranchPredictionUpdate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.RasSpeculativeInfo

class RasStack(rasSize: Int, rasSpecSize: Int)(implicit p: Parameters) extends RasModule with HasCircularQueuePtrHelper
    with Helpers {
  class RasIO extends Bundle {
    class RasSpecIO extends Bundle {
      val fire:      Bool       = Input(Bool())
      val pushValid: Bool       = Input(Bool())
      val popValid:  Bool       = Input(Bool())
      val pushAddr:  PrunedAddr = Input(PrunedAddr(VAddrBits))
      val popAddr:   PrunedAddr = Output(PrunedAddr(VAddrBits))
    }

    class RasCommitIO extends Bundle {
      val valid:     Bool       = Input(Bool())
      val pushValid: Bool       = Input(Bool())
      val popValid:  Bool       = Input(Bool())
      val pushAddr:  PrunedAddr = Input(PrunedAddr(VAddrBits))
      val metaTOSW:  RasPtr     = Input(new RasPtr)
      // for debug purpose only
      val metaSsp: UInt = Input(UInt(log2Up(RasSize).W))
    }

    class RasRedirectIO extends Bundle {
      val valid:    Bool            = Input(Bool())
      val isCall:   Bool            = Input(Bool())
      val callAddr: PrunedAddr      = Input(PrunedAddr(VAddrBits))
      val isRet:    Bool            = Input(Bool())
      val meta:     RasInternalMeta = Input(new RasInternalMeta)
    }

    val spec     = new RasSpecIO
    val commit   = new RasCommitIO
    val redirect = new RasRedirectIO
    val meta     = Output(new RasInternalMeta)

    val specNearOverflow = Output(Bool())
    val debug            = new RasDebug
  }
  val io: RasIO = IO(new RasIO)

  private val commitStack = RegInit(VecInit(Seq.fill(RasSize)(RasEntry(PrunedAddrInit(0.U(VAddrBits.W)), 0.U))))
  private val specQueue   = RegInit(VecInit(Seq.fill(rasSpecSize)(RasEntry(PrunedAddrInit(0.U(VAddrBits.W)), 0.U))))
  private val specNOS     = RegInit(VecInit(Seq.fill(rasSpecSize)(RasPtr(false.B, 0.U))))

  val nsp = RegInit(0.U(log2Up(rasSize).W))
  val ssp = RegInit(0.U(log2Up(rasSize).W))

  val sctr = RegInit(0.U(RasCtrSize.W))
  val TOSR = RegInit(RasPtr(true.B, (RasSpecSize - 1).U))
  val TOSW = RegInit(RasPtr(false.B, 0.U))
  val BOS  = RegInit(RasPtr(false.B, 0.U))

  val specNearOverflowed = RegInit(false.B)

  val writeBypassEntry = Reg(new RasEntry)
  val writeBypassNos   = Reg(new RasPtr)

  val writeBypassValid     = RegInit(0.B)
  val writeBypassValidWire = Wire(Bool())

  def TOSRinRange(currTOSR: RasPtr, currTOSW: RasPtr): Bool = {
    val inflightValid = WireInit(false.B)
    // if in range, TOSR should be no younger than BOS and strictly younger than TOSW
    when(!isBefore(currTOSR, BOS) && isBefore(currTOSR, currTOSW)) {
      inflightValid := true.B
    }
    inflightValid
  }

  def getCommitTop(currSsp: UInt): RasEntry = commitStack(currSsp)

  def getTopNos(currTOSR: RasPtr, allowBypass: Boolean): RasPtr = {
    val ret = Wire(new RasPtr)
    if (allowBypass) {
      when(writeBypassValid) {
        ret := writeBypassNos
      }.otherwise {
        ret := specNOS(currTOSR.value)
      }
    } else {
      ret := specNOS(currTOSR.value) // invalid when TOSR is not in range
    }
    ret
  }

  def getTop(currSsp: UInt, currSctr: UInt, currTOSR: RasPtr, currTOSW: RasPtr, allowBypass: Boolean): RasEntry = {
    val ret = Wire(new RasEntry)
    if (allowBypass) {
      when(writeBypassValid) {
        ret := writeBypassEntry
      }.elsewhen(TOSRinRange(currTOSR, currTOSW)) {
        ret := specQueue(currTOSR.value)
      }.otherwise {
        ret := getCommitTop(currSsp)
      }
    } else {
      when(TOSRinRange(currTOSR, currTOSW)) {
        ret := specQueue(currTOSR.value)
      }.otherwise {
        ret := getCommitTop(currSsp)
      }
    }
    ret
  }

  def specPush(
      retAddr:  PrunedAddr,
      currSsp:  UInt,
      currSctr: UInt,
      currTOSR: RasPtr,
      currTOSW: RasPtr,
      topEntry: RasEntry
  ): Unit = {
    TOSR := currTOSW
    TOSW := specPtrInc(currTOSW)
    // spec sp and ctr should always be maintained
    when(topEntry.retAddr === retAddr && currSctr < ctrMax) {
      sctr := currSctr + 1.U
    }.otherwise {
      ssp  := ptrInc(currSsp)
      sctr := 0.U
    }
  }

  def specPop(currSsp: UInt, currSctr: UInt, currTOSR: RasPtr, currTOSW: RasPtr, currTopNos: RasPtr): Unit = {
    // TOSR is only maintained when spec queue is not empty
    when(TOSRinRange(currTOSR, currTOSW)) {
      TOSR := currTopNos
    }
    // spec sp and ctr should always be maintained
    when(currSctr > 0.U) {
      sctr := currSctr - 1.U
    }.elsewhen(TOSRinRange(currTopNos, currTOSW)) {
      // in range, use inflight data
      ssp  := ptrDec(currSsp)
      sctr := specQueue(currTopNos.value).ctr
    }.otherwise {
      // NOS not in range, use commit data
      ssp  := ptrDec(currSsp)
      sctr := getCommitTop(ptrDec(currSsp)).ctr
      // in overflow state, we cannot determine the next sctr, sctr here is not accurate
    }
  }

  // it would be unsafe for specPtr manipulation if specSize is not power of 2
  assert(log2Up(RasSpecSize) == log2Floor(RasSpecSize))

  when(io.redirect.valid && io.redirect.isCall) {
    writeBypassValidWire := true.B
    writeBypassValid     := true.B
  }.elsewhen(io.redirect.valid) {
    // clear current top writeBypass if doing redirect
    writeBypassValidWire := false.B
    writeBypassValid     := false.B
  }.elsewhen(io.spec.fire) {
    writeBypassValidWire := io.spec.pushValid
    writeBypassValid     := io.spec.pushValid
  }.otherwise {
    writeBypassValidWire := writeBypassValid
    writeBypassValid     := false.B
  }

  val topEntry = getTop(ssp, sctr, TOSR, TOSW, true)
  val topNos   = getTopNos(TOSR, true)
  val redirectTopEntry =
    getTop(io.redirect.meta.ssp, io.redirect.meta.sctr, io.redirect.meta.TOSR, io.redirect.meta.TOSW, false)
  val redirectTopNos = io.redirect.meta.NOS

  val writeEntry = Wire(new RasEntry)
  val writeNOS   = Wire(new RasPtr)
  writeEntry.retAddr := Mux(io.redirect.valid && io.redirect.isCall, io.redirect.callAddr, io.spec.pushAddr)
  writeEntry.ctr := Mux(
    io.redirect.valid && io.redirect.isCall,
    Mux(
      redirectTopEntry.retAddr === io.redirect.callAddr && redirectTopEntry.ctr < ctrMax,
      io.redirect.meta.sctr + 1.U,
      0.U
    ),
    Mux(topEntry.retAddr === io.spec.pushAddr && topEntry.ctr < ctrMax, sctr + 1.U, 0.U)
  )

  writeNOS := Mux(io.redirect.valid && io.redirect.isCall, io.redirect.meta.TOSR, TOSR)

  when(io.spec.pushValid || (io.redirect.valid && io.redirect.isCall)) {
    writeBypassEntry := writeEntry
    writeBypassNos   := writeNOS
  }

  val realPush       = Wire(Bool())
  val realWriteEntry = Wire(new RasEntry)
  val timingTop      = RegInit(0.U.asTypeOf(new RasEntry))
  val timingNOS      = RegInit(0.U.asTypeOf(new RasPtr))

  when(writeBypassValidWire) {
    when((io.redirect.valid && io.redirect.isCall) || io.spec.pushValid) {
      timingTop := writeEntry
      timingNOS := writeNOS
    }.otherwise {
      timingTop := writeBypassEntry
      timingNOS := writeBypassNos
    }
  }.elsewhen(io.redirect.valid && io.redirect.isRet) {
    // getTop using redirect Nos as TOSR
    val popRedSsp  = Wire(UInt(log2Up(rasSize).W))
    val popRedSctr = Wire(UInt(RasCtrSize.W))
    val popRedTOSR = io.redirect.meta.NOS
    val popRedTOSW = io.redirect.meta.TOSW

    when(io.redirect.meta.sctr > 0.U) {
      popRedSctr := io.redirect.meta.sctr - 1.U
      popRedSsp  := io.redirect.meta.ssp
    }.elsewhen(TOSRinRange(popRedTOSR, TOSW)) {
      popRedSsp  := ptrDec(io.redirect.meta.ssp)
      popRedSctr := specQueue(popRedTOSR.value).ctr
    }.otherwise {
      popRedSsp  := ptrDec(io.redirect.meta.ssp)
      popRedSctr := getCommitTop(ptrDec(io.redirect.meta.ssp)).ctr
    }
    // We are deciding top for the next cycle, no need to use bypass here
    timingTop := getTop(popRedSsp, popRedSctr, popRedTOSR, popRedTOSW, false)
  }.elsewhen(io.redirect.valid) {
    // Neither call nor ret
    val popSsp  = io.redirect.meta.ssp
    val popSctr = io.redirect.meta.sctr
    val popTOSR = io.redirect.meta.TOSR
    val popTOSW = io.redirect.meta.TOSW

    timingTop := getTop(popSsp, popSctr, popTOSR, popTOSW, false)
  }.elsewhen(io.spec.popValid) {
    // getTop using current Nos as TOSR
    val popSsp  = Wire(UInt(log2Up(rasSize).W))
    val popSctr = Wire(UInt(RasCtrSize.W))
    val popTOSR = topNos
    val popTOSW = TOSW

    when(sctr > 0.U) {
      popSctr := sctr - 1.U
      popSsp  := ssp
    }.elsewhen(TOSRinRange(popTOSR, TOSW)) {
      popSsp  := ptrDec(ssp)
      popSctr := specQueue(popTOSR.value).ctr
    }.otherwise {
      popSsp  := ptrDec(ssp)
      popSctr := getCommitTop(ptrDec(ssp)).ctr
    }
    // We are deciding top for the next cycle, no need to use bypass here
    timingTop := getTop(popSsp, popSctr, popTOSR, popTOSW, false)
  }.elsewhen(realPush) {
    // just updating spec queue, cannot read from there
    timingTop := realWriteEntry
  }.otherwise {
    // easy case
    val popSsp  = ssp
    val popSctr = sctr
    val popTOSR = TOSR
    val popTOSW = TOSW
    timingTop := getTop(popSsp, popSctr, popTOSR, popTOSW, false)
  }
  val diffTop = Mux(writeBypassValid, writeBypassEntry.retAddr, topEntry.retAddr)

  XSPerfAccumulate("ras_top_mismatch", diffTop =/= timingTop.retAddr)
  // could diff when more pop than push and a commit stack is updated with inflight info

  realWriteEntry := RegEnable(writeEntry, io.spec.fire || io.redirect.isCall)

  val realWriteAddr = RegEnable(
    Mux(io.redirect.valid && io.redirect.isCall, io.redirect.meta.TOSW, TOSW),
    io.spec.fire || (io.redirect.valid && io.redirect.isCall)
  )

  val realNos = RegEnable(
    Mux(io.redirect.valid && io.redirect.isCall, io.redirect.meta.TOSR, TOSR),
    io.spec.fire || (io.redirect.valid && io.redirect.isCall)
  )

  realPush := RegNext(io.spec.pushValid, init = false.B) || RegNext(
    io.redirect.valid && io.redirect.isCall,
    init = false.B
  )

  when(realPush) {
    specQueue(realWriteAddr.value) := realWriteEntry
    specNOS(realWriteAddr.value)   := realNos
  }

  when(io.spec.pushValid) {
    specPush(io.spec.pushAddr, ssp, sctr, TOSR, TOSW, topEntry)
  }

  when(io.spec.popValid) {
    specPop(ssp, sctr, TOSR, TOSW, topNos)
  }

  io.spec.popAddr := timingTop.retAddr

  io.meta.TOSW := TOSW
  io.meta.TOSR := TOSR
  io.meta.NOS  := topNos
  io.meta.ssp  := ssp
  io.meta.sctr := sctr

  val commitTop = commitStack(nsp)

  when(io.commit.popValid) {

    val nspUpdate = Wire(UInt(log2Up(rasSize).W))
    when(io.commit.metaSsp =/= nsp) {
      // force set nsp to commit ssp to avoid permanent errors
      nspUpdate := io.commit.metaSsp
    }.otherwise {
      nspUpdate := nsp
    }

    // if ctr > 0, --ctr in stack, otherwise --nsp
    when(commitTop.ctr > 0.U) {
      commitStack(nspUpdate).ctr := commitTop.ctr - 1.U
      nsp                        := nspUpdate
    }.otherwise {
      nsp := ptrDec(nspUpdate)
    }
    // XSError(io.commit.metaSsp =/= nsp, "nsp mismatch with expected ssp")
  }

  val commitPushAddr = specQueue(io.commit.metaTOSW.value).retAddr

  when(io.commit.pushValid) {
    val nspUpdate = Wire(UInt(log2Up(rasSize).W))
    when(io.commit.metaSsp =/= nsp) {
      // force set nsp to commit ssp to avoid permanent errors
      nspUpdate := io.commit.metaSsp
    }.otherwise {
      nspUpdate := nsp
    }
    // if ctr < max && topAddr == push addr, ++ctr, otherwise ++nsp
    when(commitTop.ctr < ctrMax && commitTop.retAddr === commitPushAddr) {
      commitStack(nspUpdate).ctr := commitTop.ctr + 1.U
      nsp                        := nspUpdate
    }.otherwise {
      nsp                                    := ptrInc(nspUpdate)
      commitStack(ptrInc(nspUpdate)).retAddr := commitPushAddr
      commitStack(ptrInc(nspUpdate)).ctr     := 0.U
    }

    // XSError(io.commit.metaSsp =/= nsp, "nsp mismatch with expected ssp")
    // XSError(io.commit.pushAddr =/= commitPushAddr, "addr from commit mismatch with addr from spec")
  }

  when(io.commit.pushValid) {
    BOS := io.commit.metaTOSW
  }.elsewhen(io.commit.valid && (distanceBetween(io.commit.metaTOSW, BOS) > 2.U)) {
    BOS := specPtrDec(io.commit.metaTOSW)
  }
  XSError(
    io.commit.valid && (distanceBetween(io.commit.metaTOSW, BOS) > 2.U),
    "The use of inference queue of the RAS module has unexpected situations"
  )

  when(io.redirect.valid) {
    TOSR := io.redirect.meta.TOSR
    TOSW := io.redirect.meta.TOSW
    ssp  := io.redirect.meta.ssp
    sctr := io.redirect.meta.sctr

    when(io.redirect.isCall) {
      specPush(
        io.redirect.callAddr,
        io.redirect.meta.ssp,
        io.redirect.meta.sctr,
        io.redirect.meta.TOSR,
        io.redirect.meta.TOSW,
        redirectTopEntry
      )
    }
    when(io.redirect.isRet) {
      specPop(
        io.redirect.meta.ssp,
        io.redirect.meta.sctr,
        io.redirect.meta.TOSR,
        io.redirect.meta.TOSW,
        redirectTopNos
      )
    }
  }

  when(distanceBetween(TOSW, BOS) > (rasSpecSize - 2).U) {
    specNearOverflowed := true.B
  }.otherwise {
    specNearOverflowed := false.B
  }

  io.specNearOverflow := specNearOverflowed
  XSPerfAccumulate("specNearOverflow", specNearOverflowed)
  io.debug.BOS := BOS
  io.debug.commitStack.zipWithIndex.foreach { case (a, i) => a := commitStack(i) }
  io.debug.specNOS.zipWithIndex.foreach { case (a, i) => a := specNOS(i) }
  io.debug.specQueue.zipWithIndex.foreach { case (a, i) => a := specQueue(i) }
}
