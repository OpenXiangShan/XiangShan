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

package xiangshan.frontend.bpu.ras

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.HasCircularQueuePtrHelper
import utility.XSError
import utility.XSPerfAccumulate
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.GuardedPcInit

class RasStack(implicit p: Parameters) extends RasModule
    with HasCircularQueuePtrHelper
    with Helpers {
  class RasStackIO extends Bundle {
    class RasSpecIO extends Bundle {
      val fire:      Bool      = Input(Bool())
      val pushValid: Bool      = Input(Bool())
      val popValid:  Bool      = Input(Bool())
      val pushAddr:  GuardedPc = Input(GuardedPc())
      val popAddr:   GuardedPc = Output(GuardedPc())
    }

    class RasCommitIO extends Bundle {
      val valid:     Bool      = Input(Bool())
      val pushValid: Bool      = Input(Bool())
      val popValid:  Bool      = Input(Bool())
      val pushAddr:  GuardedPc = Input(GuardedPc())
      val metaTosw:  RasPtr    = Input(new RasPtr)
      // for debug purpose only
      val metaSsp: UInt = Input(UInt(log2Up(CommitStackSize).W))
    }

    class RasRedirectIO extends Bundle {
      val valid:    Bool            = Input(Bool())
      val isCall:   Bool            = Input(Bool())
      val callAddr: GuardedPc       = Input(GuardedPc())
      val isRet:    Bool            = Input(Bool())
      val meta:     RasInternalMeta = Input(new RasInternalMeta)
    }

    val spec:     RasSpecIO       = new RasSpecIO
    val commit:   RasCommitIO     = new RasCommitIO
    val redirect: RasRedirectIO   = new RasRedirectIO
    val meta:     RasInternalMeta = Output(new RasInternalMeta)

    val specNearOverflow: Bool     = Output(Bool())
    val debug:            RasDebug = new RasDebug
  }
  val io: RasStackIO = IO(new RasStackIO)
  class NosEntry(implicit p: Parameters) extends RasBundle {
    val nos:    RasPtr = new RasPtr
    val inSpec: Bool   = Bool()
  }

  private val commitStack =
    RegInit(VecInit(Seq.fill(CommitStackSize)(RasEntry(GuardedPcInit(0.U(GuardedVAddrBits.W)), 0.U))))
  private val specQueue =
    RegInit(VecInit(Seq.fill(SpecQueueSize)(RasEntry(GuardedPcInit(0.U(GuardedVAddrBits.W)), 0.U))))
  private val specNosList = RegInit(VecInit(Seq.fill(SpecQueueSize)(0.U.asTypeOf(new NosEntry))))

  private val nsp = RegInit(0.U(log2Up(CommitStackSize).W))
  private val ssp = RegInit(0.U(log2Up(CommitStackSize).W))

  private val sctr      = RegInit(0.U(StackCounterWidth.W))
  private val tosr      = RegInit(RasPtr(true.B, (SpecQueueSize - 1).U))
  private val tosw      = RegInit(RasPtr(false.B, 0.U))
  private val bos       = RegInit(RasPtr(false.B, 0.U))
  private val topInSpec = RegInit(false.B)

  private val specNearOverflowed = RegInit(false.B)

  private val writeBypassEntry    = Reg(new RasEntry)
  private val writeBypassNosEntry = Reg(new NosEntry)

  private val writeBypassValid     = RegInit(0.B)
  private val writeBypassValidWire = Wire(Bool())

  def tosrInRange(currTosr: RasPtr, currTosw: RasPtr, currInSpec: Bool): Bool = {
    val inflightValid = WireInit(false.B)
    // if in range, tosr should be no younger than bos and strictly younger than tosw
    when(!isBefore(currTosr, bos) && isBefore(currTosr, currTosw) && currInSpec) {
      inflightValid := true.B
    }
    inflightValid
  }

  def getCommitTop(currSsp: UInt): RasEntry = commitStack(currSsp)

  def getTopNos(currTosr: RasPtr, currTosw: RasPtr, currInSpec: Bool, allowBypass: Boolean): NosEntry = {
    val ret = Wire(new NosEntry)
    if (allowBypass) {
      when(writeBypassValid) {
        ret := writeBypassNosEntry
      }.elsewhen(tosrInRange(currTosr, currTosw, currInSpec)) {
        ret := specNosList(currTosr.value)
      }.otherwise {
        ret := 0.U.asTypeOf(new NosEntry)
      }
    } else when(tosrInRange(currTosr, currTosw, currInSpec)) {
      ret := specNosList(currTosr.value)
    }.otherwise {
      ret := 0.U.asTypeOf(new NosEntry)
    }
    ret
  }

  def getTop(
      currSsp:     UInt,
      currSctr:    UInt,
      currTosr:    RasPtr,
      currTosw:    RasPtr,
      allowBypass: Boolean,
      currInSpec:  Bool
  ): RasEntry = {
    val ret = Wire(new RasEntry)
    if (allowBypass) {
      when(writeBypassValid) {
        ret := writeBypassEntry
      }.elsewhen(tosrInRange(currTosr, currTosw, currInSpec)) {
        ret := specQueue(currTosr.value)
      }.otherwise {
        ret := getCommitTop(currSsp)
      }
    } else {
      when(tosrInRange(currTosr, currTosw, currInSpec)) {
        ret := specQueue(currTosr.value)
      }.otherwise {
        ret := getCommitTop(currSsp)
      }
    }
    ret
  }

  def specPush(
      retAddr:  GuardedPc,
      currSsp:  UInt,
      currSctr: UInt,
      currTosr: RasPtr,
      currTosw: RasPtr,
      topEntry: RasEntry
  ): Unit = {
    tosr      := currTosw
    tosw      := specPtrInc(currTosw)
    topInSpec := true.B
    // spec sp and ctr should always be maintained
    when(topEntry.retAddr === retAddr && currSctr < StackCounterMax.U) {
      sctr := currSctr + 1.U
    }.otherwise {
      ssp  := ptrInc(currSsp)
      sctr := 0.U
    }
  }

  def specPop(
      currSsp:       UInt,
      currSctr:      UInt,
      currTosr:      RasPtr,
      currTosw:      RasPtr,
      currTopNos:    RasPtr,
      currTopInSpec: Bool,
      currInSpec:    Bool
  ): Unit = {
    // tosr is only maintained when spec queue is not empty
    when(tosrInRange(currTosr, currTosw, currInSpec)) {
      tosr      := currTopNos
      topInSpec := currTopInSpec
    }.otherwise {
      topInSpec := false.B
    }
    // spec sp and ctr should always be maintained
    when(currSctr > 0.U) {
      sctr := currSctr - 1.U
    }.elsewhen(tosrInRange(currTopNos, currTosw, currTopInSpec)) {
      // in range, use inflight data
      ssp  := ptrDec(currSsp)
      sctr := specQueue(currTopNos.value).ctr
    }.otherwise {
      // Nos not in range, use commit data
      ssp  := ptrDec(currSsp)
      sctr := getCommitTop(ptrDec(currSsp)).ctr
      // in overflow state, we cannot determine the next sctr, sctr here is not accurate
    }
  }

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

  private val topEntry    = getTop(ssp, sctr, tosr, tosw, allowBypass = true, topInSpec)
  private val topNosEntry = getTopNos(tosr, tosw, topInSpec, allowBypass = true)
  private val topNos      = topNosEntry.nos
  private val redirectTopEntry =
    getTop(
      io.redirect.meta.ssp,
      io.redirect.meta.sctr,
      io.redirect.meta.tosr,
      io.redirect.meta.tosw,
      allowBypass = false,
      io.redirect.meta.topInSpec
    )
  private val redirectTopNos = io.redirect.meta.nos

  private val writeEntry    = Wire(new RasEntry)
  private val writeNosEntry = Wire(new NosEntry)
  writeEntry.retAddr := Mux(io.redirect.valid && io.redirect.isCall, io.redirect.callAddr, io.spec.pushAddr)
  writeEntry.ctr := Mux(
    io.redirect.valid && io.redirect.isCall,
    Mux(
      redirectTopEntry.retAddr === io.redirect.callAddr && redirectTopEntry.ctr < StackCounterMax.U,
      io.redirect.meta.sctr + 1.U,
      0.U
    ),
    Mux(topEntry.retAddr === io.spec.pushAddr && topEntry.ctr < StackCounterMax.U, sctr + 1.U, 0.U)
  )
  writeNosEntry.nos := Mux(io.redirect.valid && io.redirect.isCall, io.redirect.meta.tosr, tosr)
  writeNosEntry.inSpec := Mux(
    io.redirect.valid && io.redirect.isCall,
    tosrInRange(io.redirect.meta.tosr, io.redirect.meta.tosw, io.redirect.meta.topInSpec),
    tosrInRange(tosr, tosw, topInSpec)
  )

  when(io.spec.pushValid || (io.redirect.valid && io.redirect.isCall)) {
    writeBypassEntry    := writeEntry
    writeBypassNosEntry := writeNosEntry
  }

  private val realPush       = Wire(Bool())
  private val realWriteEntry = Wire(new RasEntry)
  private val timingTop      = RegInit(0.U.asTypeOf(new RasEntry))

  when(writeBypassValidWire) {
    when((io.redirect.valid && io.redirect.isCall) || io.spec.pushValid) {
      timingTop := writeEntry
    }.otherwise {
      timingTop := writeBypassEntry
    }
  }.elsewhen(io.redirect.valid && io.redirect.isRet) {
    // getTop using redirect Nos as tosr
    val popRedSsp  = Wire(UInt(log2Up(CommitStackSize).W))
    val popRedSctr = Wire(UInt(StackCounterWidth.W))
    val popRedTosr = io.redirect.meta.nos
    val popRedTosw = io.redirect.meta.tosw

    when(io.redirect.meta.sctr > 0.U) {
      popRedSctr := io.redirect.meta.sctr - 1.U
      popRedSsp  := io.redirect.meta.ssp
    }.elsewhen(tosrInRange(popRedTosr, popRedTosw, io.redirect.meta.nosInSpec)) {
      popRedSsp  := ptrDec(io.redirect.meta.ssp)
      popRedSctr := specQueue(popRedTosr.value).ctr
    }.otherwise {
      popRedSsp  := ptrDec(io.redirect.meta.ssp)
      popRedSctr := getCommitTop(ptrDec(io.redirect.meta.ssp)).ctr
    }
    // We are deciding top for the next cycle, no need to use bypass here
    timingTop := getTop(
      popRedSsp,
      popRedSctr,
      popRedTosr,
      popRedTosw,
      allowBypass = false,
      io.redirect.meta.nosInSpec
    )
  }.elsewhen(io.redirect.valid) {
    // Neither call nor ret
    val popSsp  = io.redirect.meta.ssp
    val popSctr = io.redirect.meta.sctr
    val popTosr = io.redirect.meta.tosr
    val popTosw = io.redirect.meta.tosw

    timingTop := getTop(popSsp, popSctr, popTosr, popTosw, allowBypass = false, io.redirect.meta.topInSpec)
  }.elsewhen(io.spec.popValid) {
    // getTop using current Nos as tosr
    val popSsp  = Wire(UInt(log2Up(CommitStackSize).W))
    val popSctr = Wire(UInt(StackCounterWidth.W))
    val popTosr = topNos
    val popTosw = tosw

    when(sctr > 0.U) {
      popSctr := sctr - 1.U
      popSsp  := ssp
    }.elsewhen(tosrInRange(popTosr, popTosw, topNosEntry.inSpec)) {
      popSsp  := ptrDec(ssp)
      popSctr := specQueue(popTosr.value).ctr
    }.otherwise {
      popSsp  := ptrDec(ssp)
      popSctr := getCommitTop(ptrDec(ssp)).ctr
    }
    // We are deciding top for the next cycle, no need to use bypass here
    timingTop := getTop(popSsp, popSctr, popTosr, popTosw, allowBypass = false, topNosEntry.inSpec)
  }.elsewhen(realPush) {
    // just updating spec queue, cannot read from there
    timingTop := realWriteEntry
  }.otherwise {
    // easy case
    val popSsp  = ssp
    val popSctr = sctr
    val popTosr = tosr
    val popTosw = tosw
    timingTop := getTop(popSsp, popSctr, popTosr, popTosw, allowBypass = false, topInSpec)
  }
  private val diffTop = Mux(writeBypassValid, writeBypassEntry.retAddr, topEntry.retAddr)

  XSPerfAccumulate("ras_top_mismatch", diffTop =/= timingTop.retAddr)
  // could diff when more pop than push and a commit stack is updated with inflight info

  realWriteEntry := RegEnable(writeEntry, io.spec.fire || io.redirect.isCall)

  private val realWriteAddr = RegEnable(
    Mux(io.redirect.valid && io.redirect.isCall, io.redirect.meta.tosw, tosw),
    io.spec.fire || (io.redirect.valid && io.redirect.isCall)
  )

  private val realNosEntry = RegEnable(
    writeNosEntry,
    io.spec.fire || (io.redirect.valid && io.redirect.isCall)
  )

  realPush := RegNext(io.spec.pushValid, init = false.B) || RegNext(
    io.redirect.valid && io.redirect.isCall,
    init = false.B
  )

  when(realPush) {
    specQueue(realWriteAddr.value)   := realWriteEntry
    specNosList(realWriteAddr.value) := realNosEntry
  }

  when(io.spec.pushValid) {
    specPush(io.spec.pushAddr, ssp, sctr, tosr, tosw, topEntry)
  }

  when(io.spec.popValid) {
    specPop(ssp, sctr, tosr, tosw, topNos, topNosEntry.inSpec, topInSpec)
  }

  io.spec.popAddr := timingTop.retAddr

  io.meta.tosw      := tosw
  io.meta.tosr      := tosr
  io.meta.nos       := topNos
  io.meta.topInSpec := topInSpec
  io.meta.nosInSpec := topNosEntry.inSpec
  io.meta.ssp       := ssp
  io.meta.sctr      := sctr

  private val commitTop = commitStack(nsp)

  when(io.commit.popValid) {
    val nspUpdate = Wire(UInt(log2Up(CommitStackSize).W))
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

  private val commitPushAddr = specQueue(io.commit.metaTosw.value).retAddr

  when(io.commit.pushValid) {
    val nspUpdate = Wire(UInt(log2Up(CommitStackSize).W))
    when(io.commit.metaSsp =/= nsp) {
      // force set nsp to commit ssp to avoid permanent errors
      nspUpdate := io.commit.metaSsp
    }.otherwise {
      nspUpdate := nsp
    }
    // if ctr < max && topAddr == push addr, ++ctr, otherwise ++nsp
    when(commitTop.ctr < StackCounterMax.U && commitTop.retAddr === commitPushAddr) {
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

  private val tmpWriteTosr  = RegEnable(io.commit.metaTosw, io.commit.pushValid)
  private val tmpWriteValid = RegEnable(io.commit.pushValid, false.B, io.commit.pushValid)
  when(io.commit.pushValid) {
    bos := io.commit.metaTosw
    when(tmpWriteValid) {
      specNosList(tmpWriteTosr.value).inSpec := false.B
    }
  }.elsewhen(io.commit.valid && (distanceBetween(io.commit.metaTosw, bos) > 2.U)) {
    bos := specPtrDec(io.commit.metaTosw)
  }
  // FIXME: Currently this assertion fails. Fix or reconsider it in the future.
//  XSError(
//    io.commit.valid && (distanceBetween(io.commit.metaTosw, bos) > 2.U),
//    "The use of inference queue of the RAS module has unexpected situations"
//  )

  when(io.redirect.valid) {
    tosr      := io.redirect.meta.tosr
    tosw      := io.redirect.meta.tosw
    ssp       := io.redirect.meta.ssp
    sctr      := io.redirect.meta.sctr
    topInSpec := io.redirect.meta.topInSpec

    when(io.redirect.isCall) {
      specPush(
        io.redirect.callAddr,
        io.redirect.meta.ssp,
        io.redirect.meta.sctr,
        io.redirect.meta.tosr,
        io.redirect.meta.tosw,
        redirectTopEntry
      )
    }
    when(io.redirect.isRet) {
      specPop(
        io.redirect.meta.ssp,
        io.redirect.meta.sctr,
        io.redirect.meta.tosr,
        io.redirect.meta.tosw,
        redirectTopNos,
        io.redirect.meta.nosInSpec,
        io.redirect.meta.topInSpec
      )
    }
  }

  // when(distanceBetween(tosw, bos) > (SpecQueueSize - 2).U) {
  //   specNearOverflowed := true.B
  // }.otherwise {
  //   specNearOverflowed := false.B
  // }

  io.specNearOverflow := specNearOverflowed
  XSPerfAccumulate("specNearOverflow", specNearOverflowed)
  io.debug.bos := bos
  io.debug.commitStack.zipWithIndex.foreach { case (a, i) => a := commitStack(i) }
  io.debug.specNos.zipWithIndex.foreach { case (a, i) => a := specNosList(i).nos }
  io.debug.specQueue.zipWithIndex.foreach { case (a, i) => a := specQueue(i) }
}
