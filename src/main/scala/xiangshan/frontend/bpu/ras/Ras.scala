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
import xiangshan.backend.datapath.DataConfig.VAddrBits
import xiangshan.frontend.BranchPredictionRedirect
import xiangshan.frontend.BranchPredictionUpdate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.RasSpeculativeInfo
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BranchAttribute

class Ras(implicit p: Parameters) extends BasePredictor with HasCircularQueuePtrHelper {
  val io:        RasIO = IO(new RasIO)
  def alignMask: UInt  = (~0.U(VAddrBits.W)) << FetchBlockAlignWidth

  private val stack = Module(new RasStack(RasSize, RasSpecSize)).io
  // Here is an assertion that the same piece of valid data lasts for only one cycle.
  // io.specIn.valid = s3_fire
  private val stackNearOverflow = stack.specNearOverflow
  private val specPush          = io.specIn.valid && (io.specIn.bits.attribute.isCall)
  private val specPop           = io.specIn.valid && (io.specIn.bits.attribute.isReturn)

  private val specIn       = io.specIn.bits
  private val specAlignPc  = specIn.startPc & alignMask
  private val specPushAddr = specAlignPc + specIn.cfiPosition + Mux(specIn.isRvc, 2.U, 4.U)
  stack.spec.pushValid := specPush && !stackNearOverflow
  stack.spec.popValid  := specPop && !stackNearOverflow

  stack.spec.pushAddr := PrunedAddrInit(specPushAddr)
  stack.spec.fire     := io.specIn.valid

  val specMeta = Wire(new RasInternalMeta)
  specMeta.ssp  := stack.meta.ssp
  specMeta.sctr := stack.meta.sctr
  specMeta.TOSR := stack.meta.TOSR
  specMeta.TOSW := stack.meta.TOSW
  specMeta.NOS  := stack.meta.NOS

  io.rasOverride := io.specIn.valid && (io.specIn.bits.target =/= stack.spec.popAddr) && specPop && io.enable
  io.specMeta    := specMeta
  io.topRetAddr  := stack.spec.popAddr

  private val redirect = RegNextWithEnable(io.redirect)
  // when we mispredict a call, we must redo a push operation
  // similarly, when we mispredict a return, we should redo a pop
  private val stackTOSW    = stack.meta.TOSW
  private val redirectTOSW = redirect.bits.meta.TOSW

  stack.redirect.valid    := redirect.valid && (isBefore(redirectTOSW, stackTOSW) || !stackNearOverflow)
  stack.redirect.isCall   := redirect.bits.attribute.isCall && (redirect.bits.level === 0.U)
  stack.redirect.isRet    := redirect.bits.attribute.isReturn && (redirect.bits.level === 0.U)
  stack.redirect.meta     := redirect.bits.meta
  stack.redirect.callAddr := redirect.bits.brPc + Mux(redirect.bits.isRvc, 2.U, 4.U)

  private val commitValid     = RegNext(io.commit.valid, init = false.B)
  private val commitInfo      = RegEnable(io.commit.bits, io.commit.valid)
  private val commitBrAlignPc = commitInfo.startPc & alignMask
  private val commitPushAddr  = commitBrAlignPc + commitInfo.cfiPosition + Mux(commitInfo.isRvc, 2.U, 4.U)
  stack.commit.valid     := commitValid
  stack.commit.pushValid := commitValid && commitInfo.attribute.isCall
  stack.commit.popValid  := commitValid && commitInfo.attribute.isReturn
  stack.commit.pushAddr  := PrunedAddrInit(commitPushAddr)
  stack.commit.metaTOSW  := commitInfo.meta.TOSW
  stack.commit.metaSsp   := commitInfo.meta.ssp

  XSPerfAccumulate("ras_redirect_recover", redirect.valid)

  private val specDebug = stack.debug
  private val specFire  = io.specIn.valid
  XSDebug(specFire, "----------------RAS----------------\n")
  XSDebug(specFire, " TopRegister: 0x%x\n", stack.spec.popAddr.toUInt)
  XSDebug(specFire, "  index       addr           ctr           nos (spec part)\n")
  for (i <- 0 until RasSpecSize) {
    XSDebug(
      specFire,
      "  (%d)   0x%x      %d       %d",
      i.U,
      specDebug.specQueue(i).retAddr.toUInt,
      specDebug.specQueue(i).ctr,
      specDebug.specNOS(i).value
    )
    XSDebug(specFire && i.U === stack.meta.TOSW.value, "   <----TOSW")
    XSDebug(specFire && i.U === stack.meta.TOSR.value, "   <----TOSR")
    XSDebug(specFire && i.U === specDebug.BOS.value, "   <----BOS")
    XSDebug(specFire, "\n")
  }
  XSDebug(specFire, "  index       addr           ctr   (committed part)\n")
  for (i <- 0 until RasSize) {
    XSDebug(
      specFire,
      "  (%d)   0x%x      %d",
      i.U,
      specDebug.commitStack(i).retAddr.toUInt,
      specDebug.commitStack(i).ctr
    )
  }
}
