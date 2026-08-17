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
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit

class RasStack(implicit p: Parameters) extends RasModule
    with HasCircularQueuePtrHelper
    with Helpers {
  class RasStackIO extends Bundle {
    class RasSpecIO extends Bundle {
      val fire:      Bool       = Input(Bool())
      val pushValid: Bool       = Input(Bool())
      val popValid:  Bool       = Input(Bool())
      val popAndPushValid: Bool = Input(Bool())
      val pushAddr:  PrunedAddr = Input(PrunedAddr(VAddrBits))
      val popAddr:   PrunedAddr = Output(PrunedAddr(VAddrBits))
    }

    class RasCommitIO extends Bundle {
      val valid:     Bool       = Input(Bool())
      val pushValid: Bool       = Input(Bool())
      val popValid:  Bool       = Input(Bool())
      val popAndPushValid: Bool = Input(Bool())
      val pushAddr:  PrunedAddr = Input(PrunedAddr(VAddrBits))
      val metaTosw:  RasPtr     = Input(new RasPtr)
      // for debug purpose only
      val metaSsp: UInt = Input(UInt(log2Up(CommitStackSize).W))
    }

    class RasRedirectIO extends Bundle {
      val valid:    Bool            = Input(Bool())
      val isCall:   Bool            = Input(Bool())
      val callAddr: PrunedAddr      = Input(PrunedAddr(VAddrBits))
      val isRet:    Bool            = Input(Bool())
      val isRetCall: Bool           = Input(Bool())
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

  private val commitStack = RegInit(VecInit(Seq.fill(CommitStackSize)(RasEntry(PrunedAddrInit(0.U(VAddrBits.W)), 0.U))))
  private val specQueue   = RegInit(VecInit(Seq.fill(SpecQueueSize)(RasEntry(PrunedAddrInit(0.U(VAddrBits.W)), 0.U))))
  private val specNos     = RegInit(VecInit(Seq.fill(SpecQueueSize)(RasPtr(false.B, 0.U))))

  private val nsp = RegInit(0.U(log2Up(CommitStackSize).W))
  private val ssp = RegInit(0.U(log2Up(CommitStackSize).W))

  private val tosr = RegInit(RasPtr(true.B, (SpecQueueSize - 1).U))
  private val tosw = RegInit(RasPtr(false.B, 0.U))
  private val bos  = RegInit(RasPtr(false.B, 0.U))

  private val specNearOverflowed = RegInit(false.B)

  // The RAS bypass is added for timing purposes.
  // It targets the RAS write timing, rather than providing the top-of-stack output address.
  // The output path is optimized via TimingTop.
  private val writeBypassValid = RegInit(false.B)
  private val writeBypassEntry = Reg(new RasEntry)
  private val writeBypassNos   = Reg(new RasPtr)

  def tosrInRange(currTosr: RasPtr, currTosw: RasPtr): Bool = {
    val inflightValid = WireInit(false.B)
    // if in range, tosr should be no younger than bos and strictly younger than tosw
    when(!isBefore(currTosr, bos) && isBefore(currTosr, currTosw)) {
      inflightValid := true.B
    }
    inflightValid
  }

  def getCommitTop(currSsp: UInt): RasEntry = commitStack(currSsp)

  def getTopNos(currTosr: RasPtr, allowBypass: Boolean): RasPtr = {
    val ret = Wire(new RasPtr)
    if (allowBypass) {
      when(writeBypassValid) {
        ret := writeBypassNos
      }.otherwise {
        ret := specNos(currTosr.value)
      }
    } else {
      ret := specNos(currTosr.value)
    }
    ret
  }

  def getTop(currSsp: UInt, currTosr: RasPtr, currTosw: RasPtr, allowBypass: Boolean): RasEntry = {
    val ret = Wire(new RasEntry)
    if (allowBypass) {
      when(writeBypassValid) {
        ret := writeBypassEntry
      }.elsewhen(tosrInRange(currTosr, currTosw)) {
        ret := specQueue(currTosr.value)
      }.otherwise {
        ret := getCommitTop(currSsp)
      }
    } else {
      when(tosrInRange(currTosr, currTosw)) {
        ret := specQueue(currTosr.value)
      }.otherwise {
        ret := getCommitTop(currSsp)
      }
    }
    ret
  }

  def specPush(
      retAddr:  PrunedAddr,
      currSsp:  UInt,
      currTosr: RasPtr,
      currTosw: RasPtr,
      topEntry: RasEntry
  ): Unit = {
    tosr := currTosw
    tosw := specPtrInc(currTosw)
    // spec sp and ctr should always be maintained
    ssp  := ptrInc(currSsp)
  }

  def specPop(currSsp: UInt, currTosr: RasPtr, currTosw: RasPtr, currTopNos: RasPtr): Unit = {
    // tosr is only maintained when spec queue is not empty
    when(tosrInRange(currTosr, currTosw)) {
      tosr := currTopNos // 这里需要做一些溢出的操作
    }
    // spec sp should always be maintained
    ssp := ptrDec(currSsp)
  }

  def specPopAndPush(currSsp: UInt, currTosr: RasPtr, currTosw: RasPtr, currTopNos: RasPtr) {
    //
    tosr := currTosw
    tosw := specPtrInc(currTosw)
    ssp  := currSsp
  }

  when(io.redirect.valid && (io.redirect.isCall || io.redirect.isRetCall)) {
    writeBypassValid := true.B
  }.elsewhen(io.redirect.valid) {
    // clear current top writeBypass if doing redirect
    writeBypassValid := false.B
  }.elsewhen(io.spec.fire && (io.spec.pushValid || io.spec.popAndPushValid)) {
    writeBypassValid := true.B
  }.otherwise {
    writeBypassValid := false.B
  }

  private val topEntry = getTop(ssp, tosr, tosw, allowBypass = true)
  private val topNos   = getTopNos(tosr, allowBypass = true)
  private val redirectTopEntry =
    getTop(
      io.redirect.meta.ssp,
      io.redirect.meta.tosr,
      io.redirect.meta.tosw,
      allowBypass = false
    )
  private val redirectTopNos = io.redirect.meta.nos

  private val writeEntry = Wire(new RasEntry)
  private val writeNos   = Wire(new RasPtr)
  private val redirectWriteEntry = Wire(new RasEntry)
  private val specWriteEntry = Wire(new RasEntry)
  redirectWriteEntry.retAddr := io.redirect.callAddr
  specWriteEntry.retAddr := io.spec.pushAddr
  writeEntry.retAddr := Mux(
    io.redirect.valid && (io.redirect.isCall || io.redirect.isRetCall),
    io.redirect.callAddr,
    io.spec.pushAddr
  )

  writeNos := Mux(
    io.redirect.valid,
    Mux(io.redirect.isCall, io.redirect.meta.tosr, io.redirect.meta.nos),
    Mux(io.spec.pushValid, tosr, topNos)
  )

  private val specWriteValid = io.spec.pushValid || io.spec.popAndPushValid
  private val redirectWriteValid = io.redirect.valid && (io.redirect.isCall || io.redirect.isRetCall)
  when(specWriteValid || redirectWriteValid) {
    writeBypassEntry := writeEntry
    writeBypassNos   := writeNos
  }

  private val realWrite      = Wire(Bool())
  private val realWriteEntry = Wire(new RasEntry)
  private val timingTop   = RegInit(0.U.asTypeOf(new RasEntry))

  when(redirectWriteValid) {
    timingTop := redirectWriteEntry
  }.elsewhen(io.redirect.valid && io.redirect.isRet) {
    // getTop using redirect Nos as tosr
    val popReadSsp  = Wire(UInt(log2Up(CommitStackSize).W))
    val popReadTosr = io.redirect.meta.nos
    val popReadTosw = io.redirect.meta.tosw

    popReadSsp := ptrDec(io.redirect.meta.ssp)
    // We are deciding top for the next cycle, no need to use bypass here
    timingTop := getTop(popReadSsp, popReadTosr, popReadTosw, allowBypass = false)
  }.elsewhen(io.redirect.valid) {
    // Neither call nor ret
    val popSsp  = io.redirect.meta.ssp
    val popTosr = io.redirect.meta.tosr
    val popTosw = io.redirect.meta.tosw

    timingTop := getTop(popSsp, popTosr, popTosw, allowBypass = false)
  }.elsewhen(specWriteValid) {
    timingTop := specWriteEntry
  }.elsewhen(io.spec.popValid) {
    val popSsp  = ptrDec(ssp)
    val popTosr = topNos
    val popTosw = tosw

    // We are deciding top for the next cycle, no need to use bypass here
    timingTop := getTop(popSsp, popTosr, popTosw, allowBypass = false)
  }.elsewhen (writeBypassValid) {
    timingTop := writeBypassEntry
  }.otherwise {
    // easy case
    val popSsp  = ssp
    val popTosr = tosr
    val popTosw = tosw
    timingTop := getTop(popSsp, popTosr, popTosw, allowBypass = false)
  }
  private val diffTop = Mux(writeBypassValid, writeBypassEntry.retAddr, topEntry.retAddr)
  XSPerfAccumulate("ras_top_mismatch", diffTop =/= timingTop.retAddr)

  realWriteEntry := RegEnable(writeEntry, specWriteValid || redirectWriteValid)

  private val realWriteTosw = RegEnable(
    Mux(io.redirect.valid, io.redirect.meta.tosw, tosw),
    specWriteValid || redirectWriteValid
  )

  private val realNos = RegEnable(writeNos, specWriteValid || redirectWriteValid)

  // No backpressure at BPU S3 stage; signal holding is not required.
  realWrite := RegNext(specWriteValid, init = false.B) || RegNext(redirectWriteValid, init = false.B)

  when(realWrite) {
    specQueue(realWriteTosw.value) := realWriteEntry
    specNos(realWriteTosw.value)   := realNos
  }

  when(io.spec.pushValid) {
    specPush(io.spec.pushAddr, ssp, tosr, tosw, topEntry)
  }

  when(io.spec.popValid) {
    specPop(ssp, tosr, tosw, topNos)
  }

  io.spec.popAddr := timingTop.retAddr

  io.meta.tosw := tosw
  io.meta.tosr := tosr
  io.meta.nos  := topNos
  io.meta.ssp  := ssp

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
    nsp := ptrDec(nspUpdate)
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

    nsp := ptrInc(nspUpdate)
    commitStack(ptrInc(nspUpdate)).retAddr := commitPushAddr

    // XSError(io.commit.metaSsp =/= nsp, "nsp mismatch with expected ssp")
    // XSError(io.commit.pushAddr =/= commitPushAddr, "addr from commit mismatch with addr from spec")
  }

  when(io.commit.popAndPushValid) {
    val nspUpdate = Wire(UInt(log2Up(CommitStackSize).W))
    when(io.commit.metaSsp =/= nsp) {
      // force set nsp to commit ssp to avoid permanent errors
      nspUpdate := io.commit.metaSsp
    }.otherwise {
      nspUpdate := nsp
    }

    nsp := nspUpdate
    commitStack(nspUpdate).retAddr := commitPushAddr
  }

  when(io.commit.pushValid || io.commit.popAndPushValid) {
    bos := io.commit.metaTosw
  }.elsewhen(io.commit.valid && (distanceBetween(io.commit.metaTosw, bos) > 2.U)) {
    bos := specPtrDec(io.commit.metaTosw)
  }
  // FIXME: Currently this assertion fails. Fix or reconsider it in the future.
//  XSError(
//    io.commit.valid && (distanceBetween(io.commit.metaTosw, bos) > 2.U),
//    "The use of inference queue of the RAS module has unexpected situations"
//  )

  when(io.redirect.valid) {
    tosr := io.redirect.meta.tosr
    tosw := io.redirect.meta.tosw
    ssp  := io.redirect.meta.ssp

    when(io.redirect.isCall) {
      specPush(
        io.redirect.callAddr,
        io.redirect.meta.ssp,
        io.redirect.meta.tosr,
        io.redirect.meta.tosw,
        redirectTopEntry
      )
    }
    when(io.redirect.isRet) {
      specPop(
        io.redirect.meta.ssp,
        io.redirect.meta.tosr,
        io.redirect.meta.tosw,
        redirectTopNos
      )
    }
    when(io.redirect.isRetCall) {
      specPopAndPush(
        io.redirect.meta.ssp,
        io.redirect.meta.tosr,
        io.redirect.meta.tosw,
        redirectTopNos
      )
    }
  }

  when(distanceBetween(tosw, bos) > (SpecQueueSize - 2).U) {
    specNearOverflowed := true.B
  }.otherwise {
    specNearOverflowed := false.B
  }

  io.specNearOverflow := specNearOverflowed
  XSPerfAccumulate("specNearOverflow", specNearOverflowed)
  io.debug.bos := bos
  io.debug.commitStack.zipWithIndex.foreach { case (a, i) => a := commitStack(i) }
  io.debug.specNos.zipWithIndex.foreach { case (a, i) => a := specNos(i) }
  io.debug.specQueue.zipWithIndex.foreach { case (a, i) => a := specQueue(i) }
}
