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

class Ras(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val in  = Input(new RasTopInput)
    val out = Output(new RasTopOutput)
  })

  val s3_fire = io.in.s3_fire

  object RasEntry {
    def apply(retAddr: PrunedAddr, ctr: UInt): RasEntry = {
      val e = Wire(new RasEntry)
      e.retAddr := retAddr
      e.ctr     := ctr
      e
    }
  }

  val stack = Module(new RasStack(RasSize, RasSpecSize)).io

  val stackNearOverflow = stack.specNearOverflow
  val s3_specPush       = WireInit(false.B)
  val s3_specPop        = WireInit(false.B)
  val fromFtb           = io.in.fromFtb

  // when last inst is an rvi call, fall through address would be set to the middle of it, so an addition is needed
  val s3_specNewAddr = fromFtb.s3_fallThroughAddr + Mux(fromFtb.s3_last_may_be_rvi_call, 2.U, 0.U)
  stack.spec.pushValid := s3_specPush && !stackNearOverflow
  stack.spec.popValid  := s3_specPop && !stackNearOverflow
  stack.spec.pushAddr  := s3_specNewAddr
  stack.s3_fire        := s3_fire

  // confirm that the call/ret is the taken cfi
  s3_specPush := s3_fire && fromFtb.s3_hit_taken_on_call && !fromFtb.s3_fallThroughErr
  s3_specPop  := s3_fire && fromFtb.s3_hit_taken_on_ret && !fromFtb.s3_fallThroughErr

  val s3_isRet = fromFtb.s3_isRet && !fromFtb.s3_fallThroughErr
  val s3_top   = stack.spec.popAddr

  io.out.predictionValid  := io.in.ctrl.ras_enable && s3_isRet
  io.out.s3_returnAddress := s3_top

  val s3_meta = Wire(new RasInternalMeta)
  s3_meta.ssp  := stack.meta.ssp
  s3_meta.sctr := stack.meta.sctr
  s3_meta.TOSR := stack.meta.TOSR
  s3_meta.TOSW := stack.meta.TOSW
  s3_meta.NOS  := stack.meta.NOS

  io.out.s3_rasSpecInfo.sctr    := s3_meta.sctr
  io.out.s3_rasSpecInfo.ssp     := s3_meta.ssp
  io.out.s3_rasSpecInfo.TOSW    := s3_meta.TOSW
  io.out.s3_rasSpecInfo.TOSR    := s3_meta.TOSR
  io.out.s3_rasSpecInfo.NOS     := s3_meta.NOS
  io.out.s3_rasSpecInfo.topAddr := s3_top
  io.out.s3_meta.ssp            := s3_meta.ssp
  io.out.s3_meta.TOSW           := s3_meta.TOSW

  val redirect   = RegNextWithEnable(io.in.redirect)
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

  val updateValid = RegNext(io.in.update.valid, init = false.B)
  val update      = Wire(new BranchPredictionUpdate)
  update := RegEnable(io.in.update.bits, io.in.update.valid)

  // The pc register has been moved outside of predictor, pc field of update bundle and other update data are not in the same stage
  // so io.update.bits.pc is used directly here
  val updatePC = io.in.update.bits.pc

  // full core power down sequence need 'val updateMeta = RegEnable(io.update.bits.meta.asTypeOf(new RASMeta), io.update.valid)' to be
  // 'val updateMeta = RegEnable(io.update.bits.meta.asTypeOf(new RASMeta), io.update.valid && (io.update.bits.is_call || io.update.bits.is_ret))',
  // but the fault-tolerance mechanism of the return stack needs to be updated in time. Using an unexpected old value on reset will cause errors.
  // Only 9 registers have clock gate efficiency affected, so we relaxed the control signals.
  val updateMeta = RegEnable(io.in.update.bits.meta.rasMeta, io.in.update.valid)

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
  XSDebug(s3_fire, "----------------RAS----------------\n")
  XSDebug(s3_fire, " TopRegister: 0x%x\n", stack.spec.popAddr.toUInt)
  XSDebug(s3_fire, "  index       addr           ctr           nos (spec part)\n")
  for (i <- 0 until RasSpecSize) {
    XSDebug(
      s3_fire,
      "  (%d)   0x%x      %d       %d",
      i.U,
      specDebug.specQueue(i).retAddr.toUInt,
      specDebug.specQueue(i).ctr,
      specDebug.specNOS(i).value
    )
    XSDebug(s3_fire && i.U === stack.meta.TOSW.value, "   <----TOSW")
    XSDebug(s3_fire && i.U === stack.meta.TOSR.value, "   <----TOSR")
    XSDebug(s3_fire && i.U === specDebug.BOS.value, "   <----BOS")
    XSDebug(s3_fire, "\n")
  }
  XSDebug(s3_fire, "  index       addr           ctr   (committed part)\n")
  for (i <- 0 until RasSize) {
    XSDebug(
      s3_fire,
      "  (%d)   0x%x      %d",
      i.U,
      specDebug.commitStack(i).retAddr.toUInt,
      specDebug.commitStack(i).ctr
    )

  }
}
