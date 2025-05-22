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
* [1] Kevin Skadron, Pritpal S. Ahuja, Margaret Martonosi, and Douglas W. Clark. "[Improving prediction for procedure
* returns with return-address-stack repair mechanisms.](https://doi.org/10.1109/MICRO.1998.742787)" 31st Annual
* ACM/IEEE International Symposium on Microarchitecture (MICRO). 1998.
* [2] Tan Hongze, and Wang Jian. "[A Return Address Predictor Based on Persistent Stack.]
* (https://crad.ict.ac.cn/en/article/doi/10.7544/issn1000-1239.202111274)" Journal of Computer Research and Development
* (CRAD) 60.6: 1337-1345. 2023.
***************************************************************************************/
package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.frontend._

class RasEntry()(implicit p: Parameters) extends XSBundle {
  val retAddr = PrunedAddr(VAddrBits)
  val ctr     = UInt(RasCtrSize.W) // layer of nested call functions
  def =/=(that: RasEntry): Bool = this.retAddr =/= that.retAddr || this.ctr =/= that.ctr
}

class RasPtr(implicit p: Parameters) extends CircularQueuePtr[RasPtr](p => p(XSCoreParamsKey).RasSpecSize) {}

object RasPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): RasPtr = {
    val ptr = Wire(new RasPtr)
    ptr.flag  := f
    ptr.value := v
    ptr
  }

  def inverse(ptr: RasPtr)(implicit p: Parameters): RasPtr = apply(!ptr.flag, ptr.value)
}

class RasInternalMeta(implicit p: Parameters) extends XSBundle {
  val ssp  = UInt(log2Up(RasSize).W)
  val sctr = UInt(RasCtrSize.W)
  val TOSW = new RasPtr
  val TOSR = new RasPtr
  val NOS  = new RasPtr
}

object RasInternalMeta {
  def apply(ssp: UInt, sctr: UInt, TOSW: RasPtr, TOSR: RasPtr, NOS: RasPtr)(implicit p: Parameters): RasInternalMeta = {
    val e = Wire(new RasInternalMeta)
    e.ssp  := ssp
    e.TOSW := TOSW
    e.TOSR := TOSR
    e.NOS  := NOS
    e
  }
}

class RasMeta(implicit p: Parameters) extends XSBundle {
  val ssp  = UInt(log2Up(RasSize).W)
  val TOSW = new RasPtr
}

object RasMeta {
  def apply(ssp: UInt, sctr: UInt, TOSW: RasPtr, TOSR: RasPtr, NOS: RasPtr)(implicit p: Parameters): RasMeta = {
    val e = Wire(new RasMeta)
    e.ssp  := ssp
    e.TOSW := TOSW
    e
  }
}

class RasDebug(implicit p: Parameters) extends XSBundle {
  val specQueue   = Output(Vec(RasSpecSize, new RasEntry))
  val specNOS     = Output(Vec(RasSpecSize, new RasPtr))
  val commitStack = Output(Vec(RasSize, new RasEntry))
  val BOS         = Output(new RasPtr)
}

class Ras(implicit p: Parameters) extends BasePredictor with HasCircularQueuePtrHelper {

  object RasEntry {
    def apply(retAddr: PrunedAddr, ctr: UInt): RasEntry = {
      val e = Wire(new RasEntry)
      e.retAddr := retAddr
      e.ctr     := ctr
      e
    }
  }

  class RasStack(rasSize: Int, rasSpecSize: Int) extends XSModule with HasCircularQueuePtrHelper {
    class RasIO extends Bundle {
      class RasSpecIO extends Bundle {
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
      val s3_fire  = Input(Bool())
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

    def TOSRinRange(currentTOSR: RasPtr, currentTOSW: RasPtr) = {
      val inflightValid = WireInit(false.B)
      // if in range, TOSR should be no younger than BOS and strictly younger than TOSW
      when(!isBefore(currentTOSR, BOS) && isBefore(currentTOSR, currentTOSW)) {
        inflightValid := true.B
      }
      inflightValid
    }

    def getCommitTop(currentSsp: UInt): RasEntry = commitStack(currentSsp)

    def getTopNos(currentTOSR: RasPtr, allowBypass: Boolean): RasPtr = {
      val ret = Wire(new RasPtr)
      if (allowBypass) {
        when(writeBypassValid) {
          ret := writeBypassNos
        }.otherwise {
          ret := specNOS(TOSR.value)
        }
      } else {
        ret := specNOS(TOSR.value) // invalid when TOSR is not in range
      }
      ret
    }

    def getTop(
        currentSsp:  UInt,
        currentSctr: UInt,
        currentTOSR: RasPtr,
        currentTOSW: RasPtr,
        allowBypass: Boolean
    ): RasEntry = {
      val ret = Wire(new RasEntry)
      if (allowBypass) {
        when(writeBypassValid) {
          ret := writeBypassEntry
        }.elsewhen(TOSRinRange(currentTOSR, currentTOSW)) {
          ret := specQueue(currentTOSR.value)
        }.otherwise {
          ret := getCommitTop(currentSsp)
        }
      } else {
        when(TOSRinRange(currentTOSR, currentTOSW)) {
          ret := specQueue(currentTOSR.value)
        }.otherwise {
          ret := getCommitTop(currentSsp)
        }
      }

      ret
    }

    // it would be unsafe for specPtr manipulation if specSize is not power of 2
    assert(log2Up(RasSpecSize) == log2Floor(RasSpecSize))
    def ctrMax: UInt = ((1L << RasCtrSize) - 1).U
    def ptrInc(ptr: UInt): UInt = ptr + 1.U
    def ptrDec(ptr: UInt): UInt = ptr - 1.U

    def specPtrInc(ptr: RasPtr): RasPtr = ptr + 1.U
    def specPtrDec(ptr: RasPtr): RasPtr = ptr - 1.U

    when(io.redirect.valid && io.redirect.isCall) {
      writeBypassValidWire := true.B
      writeBypassValid     := true.B
    }.elsewhen(io.redirect.valid) {
      // clear current top writeBypass if doing redirect
      writeBypassValidWire := false.B
      writeBypassValid     := false.B
    }.elsewhen(io.s3_fire) {
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

    realWriteEntry := RegEnable(writeEntry, io.s3_fire || io.redirect.isCall)

    val realWriteAddr = RegEnable(
      Mux(io.redirect.valid && io.redirect.isCall, io.redirect.meta.TOSW, TOSW),
      io.s3_fire || (io.redirect.valid && io.redirect.isCall)
    )

    val realNos = RegEnable(
      Mux(io.redirect.valid && io.redirect.isCall, io.redirect.meta.TOSR, TOSR),
      io.s3_fire || (io.redirect.valid && io.redirect.isCall)
    )

    realPush := RegNext(io.spec.pushValid, init = false.B) || RegNext(
      io.redirect.valid && io.redirect.isCall,
      init = false.B
    )

    when(realPush) {
      specQueue(realWriteAddr.value) := realWriteEntry
      specNOS(realWriteAddr.value)   := realNos
    }

    def specPush(
        retAddr:     PrunedAddr,
        currentSsp:  UInt,
        currentSctr: UInt,
        currentTOSR: RasPtr,
        currentTOSW: RasPtr,
        topEntry:    RasEntry
    ) = {
      TOSR := currentTOSW
      TOSW := specPtrInc(currentTOSW)
      // spec sp and ctr should always be maintained
      when(topEntry.retAddr === retAddr && currentSctr < ctrMax) {
        sctr := currentSctr + 1.U
      }.otherwise {
        ssp  := ptrInc(currentSsp)
        sctr := 0.U
      }
    }

    when(io.spec.pushValid) {
      specPush(io.spec.pushAddr, ssp, sctr, TOSR, TOSW, topEntry)
    }
    def specPop(
        currentSsp:    UInt,
        currentSctr:   UInt,
        currentTOSR:   RasPtr,
        currentTOSW:   RasPtr,
        currentTopNos: RasPtr
    ) = {
      // TOSR is only maintained when spec queue is not empty
      when(TOSRinRange(currentTOSR, currentTOSW)) {
        TOSR := currentTopNos
      }
      // spec sp and ctr should always be maintained
      when(currentSctr > 0.U) {
        sctr := currentSctr - 1.U
      }.elsewhen(TOSRinRange(currentTopNos, currentTOSW)) {
        // in range, use inflight data
        ssp  := ptrDec(currentSsp)
        sctr := specQueue(currentTopNos.value).ctr
      }.otherwise {
        // NOS not in range, use commit data
        ssp  := ptrDec(currentSsp)
        sctr := getCommitTop(ptrDec(currentSsp)).ctr
        // in overflow state, we cannot determine the next sctr, sctr here is not accurate
      }
    }
    when(io.spec.popValid) {
      specPop(ssp, sctr, TOSR, TOSW, topNos)
    }

    // io.spec_pop_addr := Mux(writeBypassValid, writeBypassEntry.retAddr, topEntry.retAddr)

    io.spec.popAddr := timingTop.retAddr

    io.meta.TOSW := TOSW
    io.meta.TOSR := TOSR
    io.meta.NOS  := topNos
    io.meta.ssp  := ssp
    io.meta.sctr := sctr
    // io.meta.nsp            := nsp

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

  val stack = Module(new RasStack(RasSize, RasSpecSize)).io

  val stackNearOverflow = stack.specNearOverflow
  val s3_specPush       = WireInit(false.B)
  val s3_specPop        = WireInit(false.B)
  val s3_full_pred      = io.in.bits.resp_in(0).s3.full_pred

  // when last inst is an rvi call, fall through address would be set to the middle of it, so an addition is needed
  val s3_specNewAddr = s3_full_pred.fallThroughAddr + Mux(s3_full_pred.last_may_be_rvi_call, 2.U, 0.U)
  stack.spec.pushValid := s3_specPush && !stackNearOverflow
  stack.spec.popValid  := s3_specPop && !stackNearOverflow
  stack.spec.pushAddr  := s3_specNewAddr
  stack.s3_fire        := io.s3_fire

  // confirm that the call/ret is the taken cfi
  s3_specPush := io.s3_fire && s3_full_pred.hit_taken_on_call && !io.in.bits.resp_in(0).s3.full_pred.fallThroughErr
  s3_specPop  := io.s3_fire && s3_full_pred.hit_taken_on_ret && !io.in.bits.resp_in(0).s3.full_pred.fallThroughErr

  val s3_isJalr = s3_full_pred.is_jalr && !io.in.bits.resp_in(0).s3.full_pred.fallThroughErr
  val s3_isRet  = s3_full_pred.is_ret && !io.in.bits.resp_in(0).s3.full_pred.fallThroughErr
  val s3_top    = stack.spec.popAddr

  when(s3_isRet && io.ctrl.ras_enable) {
    io.out.s3.full_pred.jalr_target := s3_top
    // FIXME: should use s1 globally
  }
  io.out.s3.full_pred.targets.last := Mux(
    s3_isJalr,
    io.out.s3.full_pred.jalr_target,
    io.in.bits.resp_in(0).s3.full_pred.targets.last
  )

  val s3_meta = Wire(new RasInternalMeta)
  s3_meta.ssp  := stack.meta.ssp
  s3_meta.sctr := stack.meta.sctr
  s3_meta.TOSR := stack.meta.TOSR
  s3_meta.TOSW := stack.meta.TOSW
  s3_meta.NOS  := stack.meta.NOS

  val last_stage_meta = Wire(new RasMeta)
  last_stage_meta.ssp  := s3_meta.ssp
  last_stage_meta.TOSW := s3_meta.TOSW

  io.out.last_stage_spec_info.sctr    := s3_meta.sctr
  io.out.last_stage_spec_info.ssp     := s3_meta.ssp
  io.out.last_stage_spec_info.TOSW    := s3_meta.TOSW
  io.out.last_stage_spec_info.TOSR    := s3_meta.TOSR
  io.out.last_stage_spec_info.NOS     := s3_meta.NOS
  io.out.last_stage_spec_info.topAddr := s3_top
  io.meta.rasMeta                     := last_stage_meta

  val redirect   = RegNextWithEnable(io.redirect)
  val doRecover  = redirect.valid
  val recoverCfi = redirect.bits.cfiUpdate

  val retMissPred  = doRecover && redirect.bits.level === 0.U && recoverCfi.pd.isRet && recoverCfi.pd.valid
  val callMissPred = doRecover && redirect.bits.level === 0.U && recoverCfi.pd.isCall && recoverCfi.pd.valid
  // when we mispredict a call, we must redo a push operation
  // similarly, when we mispredict a return, we should redo a pop
  val stackTOSW    = stack.meta.TOSW
  val redirectTOSW = recoverCfi.TOSW
  stack.redirect.valid     := doRecover && (isBefore(redirectTOSW, stackTOSW) || !stackNearOverflow)
  stack.redirect.isCall    := callMissPred
  stack.redirect.isRet     := retMissPred
  stack.redirect.meta.ssp  := recoverCfi.ssp
  stack.redirect.meta.sctr := recoverCfi.sctr
  stack.redirect.meta.TOSW := recoverCfi.TOSW
  stack.redirect.meta.TOSR := recoverCfi.TOSR
  stack.redirect.meta.NOS  := recoverCfi.NOS

  stack.redirect.callAddr := recoverCfi.pc + Mux(recoverCfi.pd.isRVC, 2.U, 4.U)

  val updateValid = RegNext(io.update.valid, init = false.B)
  val update      = Wire(new BranchPredictionUpdate)
  update := RegEnable(io.update.bits, io.update.valid)

  // The pc register has been moved outside of predictor, pc field of update bundle and other update data are not in the same stage
  // so io.update.bits.pc is used directly here
  val updatePC = io.update.bits.pc

  // full core power down sequence need 'val updateMeta = RegEnable(io.update.bits.meta.asTypeOf(new RASMeta), io.update.valid)' to be
  // 'val updateMeta = RegEnable(io.update.bits.meta.asTypeOf(new RASMeta), io.update.valid && (io.update.bits.is_call || io.update.bits.is_ret))',
  // but the fault-tolerance mechanism of the return stack needs to be updated in time. Using an unexpected old value on reset will cause errors.
  // Only 9 registers have clock gate efficiency affected, so we relaxed the control signals.
  val updateMeta = RegEnable(io.update.bits.meta.rasMeta, io.update.valid)

  stack.commit.valid     := updateValid
  stack.commit.pushValid := updateValid && update.is_call_taken
  stack.commit.popValid  := updateValid && update.is_ret_taken
  stack.commit.pushAddr := update.ftb_entry.getFallThrough(updatePC) + Mux(
    update.ftb_entry.last_may_be_rvi_call,
    2.U,
    0.U
  )
  stack.commit.metaTOSW := updateMeta.TOSW
  stack.commit.metaSsp  := updateMeta.ssp

  XSPerfAccumulate("ras_redirect_recover", redirect.valid)

  val specDebug = stack.debug
  XSDebug(io.s3_fire, "----------------RAS----------------\n")
  XSDebug(io.s3_fire, " TopRegister: 0x%x\n", stack.spec.popAddr.toUInt)
  XSDebug(io.s3_fire, "  index       addr           ctr           nos (spec part)\n")
  for (i <- 0 until RasSpecSize) {
    XSDebug(
      io.s3_fire,
      "  (%d)   0x%x      %d       %d",
      i.U,
      specDebug.specQueue(i).retAddr.toUInt,
      specDebug.specQueue(i).ctr,
      specDebug.specNOS(i).value
    )
    XSDebug(io.s3_fire && i.U === stack.meta.TOSW.value, "   <----TOSW")
    XSDebug(io.s3_fire && i.U === stack.meta.TOSR.value, "   <----TOSR")
    XSDebug(io.s3_fire && i.U === specDebug.BOS.value, "   <----BOS")
    XSDebug(io.s3_fire, "\n")
  }
  XSDebug(io.s3_fire, "  index       addr           ctr   (committed part)\n")
  for (i <- 0 until RasSize) {
    XSDebug(
      io.s3_fire,
      "  (%d)   0x%x      %d",
      i.U,
      specDebug.commitStack(i).retAddr.toUInt,
      specDebug.commitStack(i).ctr
    )

  }

  generatePerfEvent()
}
