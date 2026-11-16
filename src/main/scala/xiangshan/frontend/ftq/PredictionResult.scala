// Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.frontend.ftq

import chisel3._
import xiangshan.frontend.bpu.Prediction

/** Whether an original stage prediction can be judged from the surviving execution. */
class PredictionResult extends Bundle {
  val known:   Bool = Bool()
  val correct: Bool = Bool()
}

object PredictionResult {
  def apply(meta: PerfMeta, prediction: Prediction): PredictionResult = {
    val result          = Wire(new PredictionResult)
    val finalPrediction = meta.bpuPerf.bpPred
    val actual          = meta.mispredictBranchInfo
    val sameAsFinal = prediction.taken === finalPrediction.taken &&
      prediction.target === finalPrediction.target &&
      (!prediction.taken || prediction.cfiPosition === finalPrediction.cfiPosition &&
        prediction.attribute === meta.cfiAttr(finalPrediction.cfiPosition))
    val sameAsActualTaken = actual.taken && prediction.taken && prediction.cfiPosition === actual.cfiPosition &&
      prediction.attribute === actual.attribute && prediction.target.unGuard === actual.target

    // A not-taken misprediction ends this FTQ entry before the rest of the original prediction
    // window executes. Taken predictions inside that prefix are provably wrong; a fallthrough
    // or a prediction beyond it is unverified. Do not infer its correctness from a later block.
    val knownWrongInPrefix = prediction.taken && prediction.cfiPosition <= actual.cfiPosition
    // The backend checks direction/target, not the BPU's branch-type label. Use the
    // resolved attribute even when the final prediction did not cause a redirect.
    val finalResolved = !finalPrediction.taken || meta.isCfi(finalPrediction.cfiPosition)
    val eligible      = !meta.ifuRedirect.valid && !meta.backendTruncated
    result.known   := eligible && Mux(meta.mispredict, actual.taken || knownWrongInPrefix, finalResolved)
    result.correct := result.known && Mux(meta.mispredict, sameAsActualTaken, sameAsFinal)
    result
  }
}
