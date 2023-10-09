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
package xiangshan.frontend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.frontend._

class RASEntry()(implicit p: Parameters) extends XSBundle {
    val retAddr = UInt(VAddrBits.W)
    val ctr = UInt(8.W) // layer of nested call functions
    def =/=(that: RASEntry) = this.retAddr =/= that.retAddr || this.ctr =/= that.ctr
}

class RASPtr(implicit p: Parameters) extends CircularQueuePtr[RASPtr](
  p => p(XSCoreParamsKey).RasSpecSize
){
}

object RASPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): RASPtr = {
    val ptr = Wire(new RASPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
  def inverse(ptr: RASPtr)(implicit p: Parameters): RASPtr = {
    apply(!ptr.flag, ptr.value)
  }
}

class RASMeta(implicit p: Parameters) extends XSBundle {
  val ssp = UInt(log2Up(RasSize).W)
  val sctr = UInt(log2Up(RasCtrSize).W)
  val TOSW = new RASPtr
  val TOSR = new RASPtr
  val NOS = new RASPtr
}

object RASMeta {
  def apply(ssp: UInt, sctr: UInt, TOSW: RASPtr, TOSR: RASPtr, NOS: RASPtr)(implicit p: Parameters):RASMeta = {
    val e = Wire(new RASMeta)
    e.ssp := ssp
    e.sctr := sctr
    e.TOSW := TOSW
    e.TOSR := TOSR
    e.NOS := NOS
    e
  }
}

class RASDebug(implicit p: Parameters) extends XSBundle {
  val spec_queue = Output(Vec(RasSpecSize, new RASEntry))
  val spec_nos = Output(Vec(RasSpecSize, new RASPtr))
  val commit_stack = Output(Vec(RasSize, new RASEntry))
}

class RAS(implicit p: Parameters) extends BasePredictor {
  override val meta_size = WireInit(0.U.asTypeOf(new RASMeta)).getWidth

  object RASEntry {
    def apply(retAddr: UInt, ctr: UInt): RASEntry = {
      val e = Wire(new RASEntry)
      e.retAddr := retAddr
      e.ctr := ctr
      e
    }
  }


  class RASStack(rasSize: Int, rasSpecSize: Int) extends XSModule with HasCircularQueuePtrHelper {
    val io = IO(new Bundle {
      val spec_push_valid = Input(Bool())
      val spec_pop_valid = Input(Bool())
      val spec_push_addr = Input(UInt(VAddrBits.W))
      // for write bypass between s2 and s3

      val s2_fire = Input(Bool())
      val s3_fire = Input(Bool())
      val s3_cancel = Input(Bool())
      val s3_meta = Input(new RASMeta)
      val s3_missed_pop = Input(Bool())
      val s3_missed_push = Input(Bool())
      val s3_pushAddr = Input(UInt(VAddrBits.W))
      val spec_pop_addr = Output(UInt(VAddrBits.W))

      val commit_push_valid = Input(Bool())
      val commit_pop_valid = Input(Bool())
      val commit_push_addr = Input(UInt(VAddrBits.W))
      val commit_meta_TOSW = Input(new RASPtr)
      val commit_meta_TOSR = Input(new RASPtr)
      // for debug purpose only
      val commit_meta_ssp = Input(UInt(log2Up(RasSize).W))
      val commit_meta_sctr = Input(UInt(log2Up(RasCtrSize).W))

      val redirect_valid = Input(Bool())
      val redirect_isCall = Input(Bool())
      val redirect_isRet = Input(Bool())
      val redirect_meta_ssp = Input(UInt(log2Up(RasSize).W))
      val redirect_meta_sctr = Input(UInt(log2Up(RasCtrSize).W))
      val redirect_meta_TOSW = Input(new RASPtr)
      val redirect_meta_TOSR = Input(new RASPtr)
      val redirect_meta_NOS = Input(new RASPtr)
      val redirect_callAddr = Input(UInt(VAddrBits.W))

      val ssp = Output(UInt(log2Up(RasSize).W))
      val sctr = Output(UInt(log2Up(RasCtrSize).W))
      val nsp = Output(UInt(log2Up(RasSize).W))
      val TOSR = Output(new RASPtr)
      val TOSW = Output(new RASPtr)
      val NOS = Output(new RASPtr)
      val BOS = Output(new RASPtr)

      val debug = new RASDebug
    })

    val commit_stack = RegInit(VecInit(Seq.fill(RasSize)(RASEntry(0.U, 0.U))))
    val spec_queue = RegInit(VecInit(Seq.fill(rasSpecSize)(RASEntry(0.U, 0.U))))
    val spec_nos = RegInit(VecInit(Seq.fill(rasSpecSize)(RASPtr(false.B, 0.U))))

    val nsp = RegInit(0.U(log2Up(rasSize).W))
    val ssp = RegInit(0.U(log2Up(rasSize).W))

    val sctr = RegInit(0.U(RasCtrSize.W))
    val TOSR = RegInit(RASPtr(true.B, (RasSpecSize - 1).U))
    val TOSW = RegInit(RASPtr(false.B, 0.U))
    val BOS = RegInit(RASPtr(false.B, 0.U))

    val spec_overflowed = RegInit(false.B)

    val writeBypassEntry = Reg(new RASEntry)
    val writeBypassNos = Reg(new RASPtr)

    val writeBypassValid = RegInit(0.B)
    val writeBypassValidWire = Wire(Bool())

    def TOSRinRange(currentTOSR: RASPtr, currentTOSW: RASPtr) = {
      val inflightValid = WireInit(false.B)
      // if in range, TOSR should be no younger than BOS and strictly younger than TOSW
      when (!isBefore(currentTOSR, BOS) && isBefore(currentTOSR, currentTOSW)) {
        inflightValid := true.B
      }
      inflightValid
    }

    def getCommitTop(currentSsp: UInt) = {
      commit_stack(currentSsp)
    }

    def getTopNos(currentTOSR: RASPtr, allowBypass: Boolean):RASPtr = {
      val ret = Wire(new RASPtr)
      if (allowBypass){
        when (writeBypassValid) {
          ret := writeBypassNos
        } .otherwise {
          ret := spec_nos(TOSR.value)
        }
      } else {
        ret := spec_nos(TOSR.value) // invalid when TOSR is not in range
      }
      ret
    }

    def getTop(currentSsp: UInt, currentSctr: UInt, currentTOSR: RASPtr, currentTOSW: RASPtr, allowBypass: Boolean):RASEntry = {
      val ret = Wire(new RASEntry)
      if (allowBypass) {
        when (writeBypassValid) {
          ret := writeBypassEntry
        } .elsewhen (TOSRinRange(currentTOSR, currentTOSW)) {
          ret := spec_queue(currentTOSR.value)
        } .otherwise {
          ret := getCommitTop(currentSsp)
        }
      } else {
        when (TOSRinRange(currentTOSR, currentTOSW)) {
          ret := spec_queue(currentTOSR.value)
        } .otherwise {
          ret := getCommitTop(currentSsp)
        }
      }

      ret
    }

    // it would be unsafe for specPtr manipulation if specSize is not power of 2
    assert(log2Up(RasSpecSize) == log2Floor(RasSpecSize))
    def ctrMax = ((1l << RasCtrSize) - 1).U
    def ptrInc(ptr: UInt) = ptr + 1.U
    def ptrDec(ptr: UInt) = ptr - 1.U

    def specPtrInc(ptr: RASPtr) = ptr + 1.U
    def specPtrDec(ptr: RASPtr) = ptr - 1.U






    when (io.redirect_valid && io.redirect_isCall) {
      writeBypassValidWire := true.B
      writeBypassValid := true.B
    } .elsewhen (io.redirect_valid) {
      // clear current top writeBypass if doing redirect
      writeBypassValidWire := false.B
      writeBypassValid := false.B
    } .elsewhen (io.s2_fire) {
      writeBypassValidWire := io.spec_push_valid
      writeBypassValid := io.spec_push_valid
    } .elsewhen (io.s3_fire) {
      writeBypassValidWire := false.B
      writeBypassValid := false.B
    } .otherwise {
      writeBypassValidWire := writeBypassValid
    }



    val topEntry = getTop(ssp, sctr, TOSR, TOSW, true)
    val topNos = getTopNos(TOSR, true)
    val redirectTopEntry = getTop(io.redirect_meta_ssp, io.redirect_meta_sctr, io.redirect_meta_TOSR, io.redirect_meta_TOSW, false)
    val redirectTopNos = io.redirect_meta_NOS
    val s3TopEntry = getTop(io.s3_meta.ssp, io.s3_meta.sctr, io.s3_meta.TOSR, io.s3_meta.TOSW, false)
    val s3TopNos = io.s3_meta.NOS

    val writeEntry = Wire(new RASEntry)
    val writeNos = Wire(new RASPtr)
    writeEntry.retAddr := Mux(io.redirect_valid && io.redirect_isCall,  io.redirect_callAddr, io.spec_push_addr)
    writeEntry.ctr := Mux(io.redirect_valid && io.redirect_isCall,
      Mux(redirectTopEntry.retAddr === io.redirect_callAddr && redirectTopEntry.ctr < ctrMax, io.redirect_meta_sctr + 1.U, 0.U),
      Mux(topEntry.retAddr === io.spec_push_addr && topEntry.ctr < ctrMax, sctr + 1.U, 0.U))

    writeNos := Mux(io.redirect_valid && io.redirect_isCall,
      io.redirect_meta_NOS, TOSR)

    when (io.spec_push_valid || (io.redirect_valid && io.redirect_isCall)) {
      writeBypassEntry := writeEntry
      writeBypassNos := writeNos
    }

    val realPush = Wire(Bool())
    val realWriteEntry = Wire(new RASEntry)
    val timingTop = RegInit(0.U.asTypeOf(new RASEntry))
    val timingNos = RegInit(0.U.asTypeOf(new RASPtr))

    when (writeBypassValidWire) {
      when ((io.redirect_valid && io.redirect_isCall) || io.spec_push_valid) {
        timingTop := writeEntry
        timingNos := writeNos
      } .otherwise {
        timingTop := writeBypassEntry
        timingNos := writeBypassNos
      }

    } .elsewhen (io.redirect_valid && io.redirect_isRet) {
      // getTop using redirect Nos as TOSR
      val popRedSsp = Wire(UInt(log2Up(rasSize).W))
      val popRedSctr = Wire(UInt(log2Up(RasCtrSize).W))
      val popRedTOSR = io.redirect_meta_NOS
      val popRedTOSW = io.redirect_meta_TOSW

      when (io.redirect_meta_sctr > 0.U) {
        popRedSctr := io.redirect_meta_sctr - 1.U
        popRedSsp := io.redirect_meta_ssp
      } .elsewhen (TOSRinRange(popRedTOSR, TOSW)) {
        popRedSsp := ptrDec(io.redirect_meta_ssp)
        popRedSctr := spec_queue(popRedTOSR.value).ctr
      } .otherwise {
        popRedSsp := ptrDec(io.redirect_meta_ssp)
        popRedSctr := getCommitTop(ptrDec(io.redirect_meta_ssp)).ctr
      }
      // We are deciding top for the next cycle, no need to use bypass here
      timingTop := getTop(popRedSsp, popRedSctr, popRedTOSR, popRedTOSW, false)
    } .elsewhen (io.redirect_valid) {
      // Neither call nor ret
      val popSsp = io.redirect_meta_ssp
      val popSctr = io.redirect_meta_sctr
      val popTOSR = io.redirect_meta_TOSR
      val popTOSW = io.redirect_meta_TOSW

      timingTop := getTop(popSsp, popSctr, popTOSR, popTOSW, false)

    } .elsewhen (io.spec_pop_valid) {
      // getTop using current Nos as TOSR
      val popSsp = Wire(UInt(log2Up(rasSize).W))
      val popSctr = Wire(UInt(log2Up(RasCtrSize).W))
      val popTOSR = topNos
      val popTOSW = TOSW

      when (sctr > 0.U) {
        popSctr := sctr - 1.U
        popSsp := ssp
      } .elsewhen (TOSRinRange(popTOSR, TOSW)) {
        popSsp := ptrDec(ssp)
        popSctr := spec_queue(popTOSR.value).ctr
      } .otherwise {
        popSsp := ptrDec(ssp)
        popSctr := getCommitTop(ptrDec(ssp)).ctr
      }
      // We are deciding top for the next cycle, no need to use bypass here
      timingTop := getTop(popSsp, popSctr, popTOSR, popTOSW, false)
    } .elsewhen (realPush) {
      // just updating spec queue, cannot read from there
      timingTop := realWriteEntry
    } .elsewhen (io.s3_cancel) {
      // s3 is different with s2
      timingTop := getTop(io.s3_meta.ssp, io.s3_meta.sctr, io.s3_meta.TOSR, io.s3_meta.TOSW, false)
      when (io.s3_missed_push) {
        val writeEntry_s3 = Wire(new RASEntry)
        timingTop := writeEntry_s3
        writeEntry_s3.retAddr := io.s3_pushAddr
        writeEntry_s3.ctr := Mux(timingTop.retAddr === io.s3_pushAddr && io.s3_meta.sctr < ctrMax, io.s3_meta.sctr + 1.U, 0.U)
      } .elsewhen (io.s3_missed_pop) {
        val popRedSsp_s3 = Wire(UInt(log2Up(rasSize).W))
        val popRedSctr_s3 = Wire(UInt(log2Up(RasCtrSize).W))
        val popRedTOSR_s3 = io.s3_meta.NOS
        val popRedTOSW_s3 = io.s3_meta.TOSW

        when (io.s3_meta.sctr > 0.U) {
          popRedSctr_s3 := io.s3_meta.sctr - 1.U
          popRedSsp_s3 := io.s3_meta.ssp
        } .elsewhen (TOSRinRange(popRedTOSR_s3, popRedTOSW_s3)) {
          popRedSsp_s3 := ptrDec(io.s3_meta.ssp)
          popRedSctr_s3 := spec_queue(popRedTOSR_s3.value).ctr
        } .otherwise {
          popRedSsp_s3 := ptrDec(io.s3_meta.ssp)
          popRedSctr_s3 := getCommitTop(ptrDec(io.s3_meta.ssp)).ctr
        }
        // We are deciding top for the next cycle, no need to use bypass here
        timingTop := getTop(popRedSsp_s3, popRedSctr_s3, popRedTOSR_s3, popRedTOSW_s3, false)
      }
    } .otherwise {
      // easy case
      val popSsp = ssp
      val popSctr = sctr
      val popTOSR = TOSR
      val popTOSW = TOSW
      timingTop := getTop(popSsp, popSctr, popTOSR, popTOSW, false)
    }
    val diffTop = Mux(writeBypassValid, writeBypassEntry.retAddr, topEntry.retAddr)

    XSPerfAccumulate("ras_top_mismatch", diffTop =/= timingTop.retAddr);
    // could diff when more pop than push and a commit stack is updated with inflight info

    val realWriteEntry_next = RegEnable(writeEntry, io.s2_fire || io.redirect_isCall)
    val s3_missPushEntry = Wire(new RASEntry)
    val s3_missPushAddr = Wire(new RASPtr)
    val s3_missPushNos = Wire(new RASPtr)

    s3_missPushEntry.retAddr := io.s3_pushAddr
    s3_missPushEntry.ctr := Mux(s3TopEntry.retAddr === io.s3_pushAddr && s3TopEntry.ctr < ctrMax, io.s3_meta.sctr + 1.U, 0.U)
    s3_missPushAddr := io.s3_meta.TOSW
    s3_missPushNos := io.s3_meta.TOSR



    realWriteEntry := Mux(io.redirect_isCall, realWriteEntry_next,
      Mux(io.s3_missed_push, s3_missPushEntry,
      realWriteEntry_next))

    val realWriteAddr_next = RegEnable(Mux(io.redirect_valid && io.redirect_isCall, io.redirect_meta_TOSW, TOSW), io.s2_fire || (io.redirect_valid && io.redirect_isCall))
    val realWriteAddr = Mux(io.redirect_isCall, realWriteAddr_next,
      Mux(io.s3_missed_push, s3_missPushAddr,
      realWriteAddr_next))
    val realNos_next = RegEnable(Mux(io.redirect_valid && io.redirect_isCall, io.redirect_meta_TOSR, TOSR), io.s2_fire || (io.redirect_valid && io.redirect_isCall))
    val realNos = Mux(io.redirect_isCall, realNos_next,
      Mux(io.s3_missed_push, s3_missPushNos,
      realNos_next))

    realPush := (io.s3_fire && (!io.s3_cancel && RegEnable(io.spec_push_valid, io.s2_fire) || io.s3_missed_push)) || RegNext(io.redirect_valid && io.redirect_isCall)

    when (realPush) {
      spec_queue(realWriteAddr.value) := realWriteEntry
      spec_nos(realWriteAddr.value) := realNos
    }

    def specPush(retAddr: UInt, currentSsp: UInt, currentSctr: UInt, currentTOSR: RASPtr, currentTOSW: RASPtr, topEntry: RASEntry) = {
      TOSR := currentTOSW
      TOSW := specPtrInc(currentTOSW)
      // spec sp and ctr should always be maintained
      when (topEntry.retAddr === retAddr && currentSctr < ctrMax) {
        sctr := currentSctr + 1.U
      } .otherwise {
        ssp := ptrInc(currentSsp)
        sctr := 0.U
      }
      // if we are draining the capacity of spec queue, force move BOS forward
      when (specPtrInc(currentTOSW) === BOS) {
        BOS := specPtrInc(BOS)
        spec_overflowed := true.B;
      }
    }

    when (io.spec_push_valid) {
      specPush(io.spec_push_addr, ssp, sctr, TOSR, TOSW, topEntry)
    }
    def specPop(currentSsp: UInt, currentSctr: UInt, currentTOSR: RASPtr, currentTOSW: RASPtr, currentTopNos: RASPtr) = {
      // TOSR is only maintained when spec queue is not empty
      when (TOSRinRange(currentTOSR, currentTOSW)) {
        TOSR := currentTopNos
      }
      // spec sp and ctr should always be maintained
      when (currentSctr > 0.U) {
        sctr := currentSctr - 1.U
      } .elsewhen (TOSRinRange(currentTopNos, currentTOSW)) {
        // in range, use inflight data
        ssp := ptrDec(currentSsp)
        sctr := spec_queue(currentTopNos.value).ctr
      } .otherwise {
        // NOS not in range, use commit data
        ssp := ptrDec(currentSsp)
        sctr := getCommitTop(ptrDec(currentSsp)).ctr
        // in overflow state, we cannot determine the next sctr, sctr here is not accurate
      }
    }
    when (io.spec_pop_valid) {
      specPop(ssp, sctr, TOSR, TOSW, topNos)
    }

    // io.spec_pop_addr := Mux(writeBypassValid, writeBypassEntry.retAddr, topEntry.retAddr)

    io.spec_pop_addr := timingTop.retAddr
    io.BOS := BOS
    io.TOSW := TOSW
    io.TOSR := TOSR
    io.NOS := topNos
    io.ssp := ssp
    io.sctr := sctr
    io.nsp := nsp

    when (io.s3_cancel) {
      // recovery of all related pointers
      TOSR := io.s3_meta.TOSR
      TOSW := io.s3_meta.TOSW
      ssp := io.s3_meta.ssp
      sctr := io.s3_meta.sctr

      // for missing pop, we also need to do a pop here
      when (io.s3_missed_pop) {
        specPop(io.s3_meta.ssp, io.s3_meta.sctr, io.s3_meta.TOSR, io.s3_meta.TOSW, io.s3_meta.NOS)
      }
      when (io.s3_missed_push) {
        // do not use any bypass from f2
        specPush(io.s3_pushAddr, io.s3_meta.ssp, io.s3_meta.sctr, io.s3_meta.TOSR, io.s3_meta.TOSW, s3TopEntry)
      }
    }

    val commitTop = commit_stack(nsp)

    when (io.commit_pop_valid) {

      val nsp_update = Wire(UInt(log2Up(rasSize).W))
      when (io.commit_meta_ssp =/= nsp) {
        // force set nsp to commit ssp to avoid permanent errors
        nsp_update := io.commit_meta_ssp
      } .otherwise {
        nsp_update := nsp
      }

      // if ctr > 0, --ctr in stack, otherwise --nsp
      when (commitTop.ctr > 0.U) {
        commit_stack(nsp_update).ctr := commitTop.ctr - 1.U
        nsp := nsp_update
      } .otherwise {
        nsp := ptrDec(nsp_update);
      }
      // XSError(io.commit_meta_ssp =/= nsp, "nsp mismatch with expected ssp")
    }

    val commit_push_addr = spec_queue(io.commit_meta_TOSW.value).retAddr



    when (io.commit_push_valid) {
      val nsp_update = Wire(UInt(log2Up(rasSize).W))
      when (io.commit_meta_ssp =/= nsp) {
        // force set nsp to commit ssp to avoid permanent errors
        nsp_update := io.commit_meta_ssp
      } .otherwise {
        nsp_update := nsp
      }
      // if ctr < max && topAddr == push addr, ++ctr, otherwise ++nsp
      when (commitTop.ctr < ctrMax && commitTop.retAddr === commit_push_addr) {
        commit_stack(nsp_update).ctr := commitTop.ctr + 1.U
        nsp := nsp_update
      } .otherwise {
        nsp := ptrInc(nsp_update)
        commit_stack(ptrInc(nsp_update)).retAddr := commit_push_addr
        commit_stack(ptrInc(nsp_update)).ctr := 0.U
      }
      // when overflow, BOS may be forced move forward, do not revert those changes
      when (!spec_overflowed || isAfter(specPtrInc(io.commit_meta_TOSW), BOS)) {
        BOS := specPtrInc(io.commit_meta_TOSW)
        spec_overflowed := false.B
      }

      // XSError(io.commit_meta_ssp =/= nsp, "nsp mismatch with expected ssp")
      // XSError(io.commit_push_addr =/= commit_push_addr, "addr from commit mismatch with addr from spec")
    }

    when (io.redirect_valid) {
      TOSR := io.redirect_meta_TOSR
      TOSW := io.redirect_meta_TOSW
      ssp := io.redirect_meta_ssp
      sctr := io.redirect_meta_sctr

      when (io.redirect_isCall) {
        specPush(io.redirect_callAddr, io.redirect_meta_ssp, io.redirect_meta_sctr, io.redirect_meta_TOSR, io.redirect_meta_TOSW, redirectTopEntry)
      }
      when (io.redirect_isRet) {
        specPop(io.redirect_meta_ssp, io.redirect_meta_sctr, io.redirect_meta_TOSR, io.redirect_meta_TOSW, redirectTopNos)
      }
    }

    io.debug.commit_stack.zipWithIndex.foreach{case (a, i) => a := commit_stack(i)}
    io.debug.spec_nos.zipWithIndex.foreach{case (a, i) => a := spec_nos(i)}
    io.debug.spec_queue.zipWithIndex.foreach{ case (a, i) => a := spec_queue(i)}
  }

  val stack = Module(new RASStack(RasSize, RasSpecSize)).io

  val s2_spec_push = WireInit(false.B)
  val s2_spec_pop = WireInit(false.B)
  val s2_full_pred = io.in.bits.resp_in(0).s2.full_pred(2)
  // when last inst is an rvi call, fall through address would be set to the middle of it, so an addition is needed
  val s2_spec_new_addr = s2_full_pred.fallThroughAddr + Mux(s2_full_pred.last_may_be_rvi_call, 2.U, 0.U)
  stack.spec_push_valid := s2_spec_push
  stack.spec_pop_valid  := s2_spec_pop
  stack.spec_push_addr := s2_spec_new_addr

  // confirm that the call/ret is the taken cfi
  s2_spec_push := io.s2_fire(2) && s2_full_pred.hit_taken_on_call && !io.s3_redirect(2)
  s2_spec_pop  := io.s2_fire(2) && s2_full_pred.hit_taken_on_ret  && !io.s3_redirect(2)

  //val s2_jalr_target = io.out.s2.full_pred.jalr_target
  //val s2_last_target_in = s2_full_pred.targets.last
  // val s2_last_target_out = io.out.s2.full_pred(2).targets.last
  val s2_is_jalr = s2_full_pred.is_jalr
  val s2_is_ret = s2_full_pred.is_ret
  val s2_top = stack.spec_pop_addr
  // assert(is_jalr && is_ret || !is_ret)
  when(s2_is_ret && io.ctrl.ras_enable) {
    io.out.s2.full_pred.map(_.jalr_target).foreach(_ := s2_top)
    // FIXME: should use s1 globally
  }
  //s2_last_target_out := Mux(s2_is_jalr, s2_jalr_target, s2_last_target_in)
  io.out.s2.full_pred.zipWithIndex.foreach{ case (a, i) =>
    a.targets.last := Mux(s2_is_jalr, io.out.s2.full_pred(i).jalr_target, io.in.bits.resp_in(0).s2.full_pred(i).targets.last)
  }

  val s2_meta = Wire(new RASMeta)
  s2_meta.ssp := stack.ssp
  s2_meta.sctr := stack.sctr
  s2_meta.TOSR := stack.TOSR
  s2_meta.TOSW := stack.TOSW
  s2_meta.NOS := stack.NOS

  val s3_top = RegEnable(stack.spec_pop_addr, io.s2_fire(2))
  val s3_spec_new_addr = RegEnable(s2_spec_new_addr, io.s2_fire(2))

  // val s3_jalr_target = io.out.s3.full_pred.jalr_target
  // val s3_last_target_in = io.in.bits.resp_in(0).s3.full_pred(2).targets.last
  // val s3_last_target_out = io.out.s3.full_pred(2).targets.last
  val s3_is_jalr = io.in.bits.resp_in(0).s3.full_pred(2).is_jalr
  val s3_is_ret = io.in.bits.resp_in(0).s3.full_pred(2).is_ret
  // assert(is_jalr && is_ret || !is_ret)
  when(s3_is_ret && io.ctrl.ras_enable) {
    io.out.s3.full_pred.map(_.jalr_target).foreach(_ := s3_top)
    // FIXME: should use s1 globally
  }
  // s3_last_target_out := Mux(s3_is_jalr, s3_jalr_target, s3_last_target_in)
  io.out.s3.full_pred.zipWithIndex.foreach{ case (a, i) =>
    a.targets.last := Mux(s3_is_jalr, io.out.s3.full_pred(i).jalr_target, io.in.bits.resp_in(0).s3.full_pred(i).targets.last)
  }

  val s3_pushed_in_s2 = RegEnable(s2_spec_push, io.s2_fire(2))
  val s3_popped_in_s2 = RegEnable(s2_spec_pop,  io.s2_fire(2))
  val s3_push = io.in.bits.resp_in(0).s3.full_pred(2).hit_taken_on_call
  val s3_pop  = io.in.bits.resp_in(0).s3.full_pred(2).hit_taken_on_ret

  val s3_cancel = io.s3_fire(2) && (s3_pushed_in_s2 =/= s3_push || s3_popped_in_s2 =/= s3_pop)
  stack.s2_fire := io.s2_fire(2)
  stack.s3_fire := io.s3_fire(2)

  stack.s3_cancel := s3_cancel

  val s3_meta = RegEnable(s2_meta, io.s2_fire(2))

  stack.s3_meta := s3_meta
  stack.s3_missed_pop := s3_pop && !s3_popped_in_s2
  stack.s3_missed_push := s3_push && !s3_pushed_in_s2
  stack.s3_pushAddr := s3_spec_new_addr

  // no longer need the top Entry, but TOSR, TOSW, ssp sctr
  // TODO: remove related signals
  io.out.last_stage_spec_info.sctr  := s3_meta.sctr
  io.out.last_stage_spec_info.ssp := s3_meta.ssp
  io.out.last_stage_spec_info.TOSW := s3_meta.TOSW
  io.out.last_stage_spec_info.TOSR := s3_meta.TOSR
  io.out.last_stage_spec_info.NOS := s3_meta.NOS
  io.out.last_stage_spec_info.topAddr := s3_top
  io.out.last_stage_meta := s3_meta.asUInt


  val redirect = RegNext(io.redirect)
  val do_recover = redirect.valid
  val recover_cfi = redirect.bits.cfiUpdate

  val retMissPred  = do_recover && redirect.bits.level === 0.U && recover_cfi.pd.isRet
  val callMissPred = do_recover && redirect.bits.level === 0.U && recover_cfi.pd.isCall
  // when we mispredict a call, we must redo a push operation
  // similarly, when we mispredict a return, we should redo a pop
  stack.redirect_valid := do_recover
  stack.redirect_isCall := callMissPred
  stack.redirect_isRet := retMissPred
  stack.redirect_meta_ssp := recover_cfi.ssp
  stack.redirect_meta_sctr := recover_cfi.sctr
  stack.redirect_meta_TOSW := recover_cfi.TOSW
  stack.redirect_meta_TOSR := recover_cfi.TOSR
  stack.redirect_meta_NOS := recover_cfi.NOS
  stack.redirect_callAddr := recover_cfi.pc + Mux(recover_cfi.pd.isRVC, 2.U, 4.U)

  val update = io.update.bits
  val updateMeta = io.update.bits.meta.asTypeOf(new RASMeta)
  val updateValid = io.update.valid

  stack.commit_push_valid := updateValid && update.is_call_taken
  stack.commit_pop_valid := updateValid && update.is_ret_taken
  stack.commit_push_addr := update.ftb_entry.getFallThrough(update.pc) + Mux(update.ftb_entry.last_may_be_rvi_call, 2.U, 0.U)
  stack.commit_meta_TOSW := updateMeta.TOSW
  stack.commit_meta_TOSR := updateMeta.TOSR
  stack.commit_meta_ssp := updateMeta.ssp
  stack.commit_meta_sctr := updateMeta.sctr


  XSPerfAccumulate("ras_s3_cancel", s3_cancel)
  XSPerfAccumulate("ras_redirect_recover", redirect.valid)
  XSPerfAccumulate("ras_s3_and_redirect_recover_at_the_same_time", s3_cancel && redirect.valid)


  val spec_debug = stack.debug
  XSDebug(io.s2_fire(2), "----------------RAS----------------\n")
  XSDebug(io.s2_fire(2), " TopRegister: 0x%x\n",stack.spec_pop_addr)
  XSDebug(io.s2_fire(2), "  index       addr           ctr           nos (spec part)\n")
  for(i <- 0 until RasSpecSize){
      XSDebug(io.s2_fire(2), "  (%d)   0x%x      %d       %d",i.U,spec_debug.spec_queue(i).retAddr,spec_debug.spec_queue(i).ctr, spec_debug.spec_nos(i).value)
      when(i.U === stack.TOSW.value){XSDebug(io.s2_fire(2), "   <----TOSW")}
      when(i.U === stack.TOSR.value){XSDebug(io.s2_fire(2), "   <----TOSR")}
      when(i.U === stack.BOS.value){XSDebug(io.s2_fire(2), "   <----BOS")}
      XSDebug(io.s2_fire(2), "\n")
  }
  XSDebug(io.s2_fire(2), "  index       addr           ctr   (committed part)\n")
  for(i <- 0 until RasSize){
      XSDebug(io.s2_fire(2), "  (%d)   0x%x      %d",i.U,spec_debug.commit_stack(i).retAddr,spec_debug.commit_stack(i).ctr)
      when(i.U === stack.ssp){XSDebug(io.s2_fire(2), "   <----ssp")}
      when(i.U === stack.nsp){XSDebug(io.s2_fire(2), "   <----nsp")}
      XSDebug(io.s2_fire(2), "\n")
  }
  /*
  XSDebug(s2_spec_push, "s2_spec_push  inAddr: 0x%x  inCtr: %d |  allocNewEntry:%d |   sp:%d \n",
  s2_spec_new_addr,spec_debug.spec_push_entry.ctr,spec_debug.spec_alloc_new,spec_debug.sp.asUInt)
  XSDebug(s2_spec_pop, "s2_spec_pop  outAddr: 0x%x \n",io.out.s2.getTarget)
  val s3_recover_entry = spec_debug.recover_push_entry
  XSDebug(s3_recover && s3_push, "s3_recover_push  inAddr: 0x%x  inCtr: %d |  allocNewEntry:%d |   sp:%d \n",
    s3_recover_entry.retAddr, s3_recover_entry.ctr, spec_debug.recover_alloc_new, s3_sp.asUInt)
  XSDebug(s3_recover && s3_pop, "s3_recover_pop  outAddr: 0x%x \n",io.out.s3.getTarget)
  val redirectUpdate = redirect.bits.cfiUpdate
  XSDebug(do_recover && callMissPred, "redirect_recover_push\n")
  XSDebug(do_recover && retMissPred, "redirect_recover_pop\n")
  XSDebug(do_recover, "redirect_recover(SP:%d retAddr:%x ctr:%d) \n",
      redirectUpdate.rasSp,redirectUpdate.rasEntry.retAddr,redirectUpdate.rasEntry.ctr)
  */

  generatePerfEvent()
}
