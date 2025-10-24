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
import utility.HasCircularQueuePtrHelper
import utility.RegNextWithEnable
import utility.XSDebug
import utility.XSPerfAccumulate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO

class Ras(implicit p: Parameters) extends BasePredictor with HasRasParameters with HasCircularQueuePtrHelper {
  class RasIO(implicit p: Parameters) extends BasePredictorIO {
    val specIn:   Valid[RasSpecInfo]     = Flipped(Valid(new RasSpecInfo))
    val commit:   Valid[RasCommitInfo]   = Flipped(Valid(new RasCommitInfo))
    val redirect: Valid[RasRedirectInfo] = Flipped(Valid(new RasRedirectInfo))

    val topRetAddr: PrunedAddr      = Output(PrunedAddr(VAddrBits))
    val specMeta:   RasInternalMeta = Output(new RasInternalMeta)
  }

  val io: RasIO = IO(new RasIO)

  io.resetDone := true.B

  io.train.ready := true.B

  def alignMask: UInt = ((~0.U(VAddrBits.W)) << FetchBlockAlignWidth).asUInt

  private val stack = Module(new RasStack).io
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
  specMeta.tosr := stack.meta.tosr
  specMeta.tosw := stack.meta.tosw
  specMeta.nos  := stack.meta.nos

  io.specMeta   := specMeta
  io.topRetAddr := stack.spec.popAddr

  private val redirect = RegNextWithEnable(io.redirect)
  // when we mispredict a call, we must redo a push operation
  // similarly, when we mispredict a return, we should redo a pop
  private val stackTOSW    = stack.meta.tosw
  private val redirectTOSW = redirect.bits.meta.tosw

  stack.redirect.valid  := redirect.valid && (isBefore(redirectTOSW, stackTOSW) || !stackNearOverflow)
  stack.redirect.isCall := redirect.bits.attribute.isCall && (redirect.bits.level === 0.U)
  stack.redirect.isRet  := redirect.bits.attribute.isReturn && (redirect.bits.level === 0.U)
  stack.redirect.meta   := redirect.bits.meta
  // Redirected branch PC points to end of instruction.
  stack.redirect.callAddr := redirect.bits.brPc + 2.U

  private val commitValid    = RegNext(io.commit.valid, init = false.B)
  private val commitInfo     = RegEnable(io.commit.bits, io.commit.valid)
  private val commitPushAddr = commitInfo.pushAddr
  stack.commit.valid     := commitValid
  stack.commit.pushValid := commitValid && commitInfo.attribute.isCall
  stack.commit.popValid  := commitValid && commitInfo.attribute.isReturn
  stack.commit.pushAddr  := commitPushAddr
  stack.commit.metaTosw  := commitInfo.meta.tosw
  stack.commit.metaSsp   := commitInfo.meta.ssp

  XSPerfAccumulate("ras_redirect_recover", redirect.valid)

  private val specDebug = stack.debug
  private val specFire  = io.specIn.valid
  XSDebug(specFire, "----------------RAS----------------\n")
  XSDebug(specFire, " TopRegister: 0x%x\n", stack.spec.popAddr.toUInt)
  XSDebug(specFire, "  index       addr           ctr           nos (spec part)\n")
  for (i <- 0 until SpecQueueSize) {
    XSDebug(
      specFire,
      "  (%d)   0x%x      %d       %d",
      i.U,
      specDebug.specQueue(i).retAddr.toUInt,
      specDebug.specQueue(i).ctr,
      specDebug.specNos(i).value
    )
    XSDebug(specFire && i.U === stack.meta.tosw.value, "   <----TOSW")
    XSDebug(specFire && i.U === stack.meta.tosr.value, "   <----TOSR")
    XSDebug(specFire && i.U === specDebug.bos.value, "   <----BOS")
    XSDebug(specFire, "\n")
  }
  XSDebug(specFire, "  index       addr           ctr   (committed part)\n")
  for (i <- 0 until CommitStackSize) {
    XSDebug(
      specFire,
      "  (%d)   0x%x      %d",
      i.U,
      specDebug.commitStack(i).retAddr.toUInt,
      specDebug.commitStack(i).ctr
    )
  }
}
