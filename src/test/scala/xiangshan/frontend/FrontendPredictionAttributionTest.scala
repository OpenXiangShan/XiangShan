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

package xiangshan.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{TopDownCounters, XSTester}
import xiangshan.frontend.bpu._

class PredictionStageAttributionHarness(implicit p: Parameters) extends FrontendModule {
  val io = IO(new Bundle {
    val meta = Input(new BpuPerfMeta)
    val branch = Input(new BranchInfo)
    val source = Output(BpuPredictionOrigin())
    val reason = Output(UInt(log2Ceil(TopDownCounters.NumStallReasons.id).W))
  })
  io.source := BlameBpuSource(true.B, io.meta, io.branch)
  val reason = Wire(new FrontendTopDownBundle)
  reason.setPredictionReason(io.source)
  io.reason := reason.reason
}

class FrontendPredictionAttributionTest extends XSTester {
  it should "blame S3 after validation, including NT candidates, irrespective of override flags" in {
    simulate(new PredictionStageAttributionHarness) { d =>
      def attribute(a: BranchAttribute, kind: Int = 1, ras: Int = 0): Unit = {
        a.branchType.poke(kind.U); a.rasAction.poke(ras.U)
      }
      val meta = d.io.meta
      meta.mbtbMeta.entries.flatten.foreach { e =>
        e.rawHit.poke(false.B); e.position.poke(0.U); attribute(e.attribute, 0)
      }
      meta.s1Prediction.taken.poke(true.B); meta.s1Prediction.cfiPosition.poke(1.U)
      meta.s2Prediction.taken.poke(true.B); meta.s2Prediction.cfiPosition.poke(2.U)
      meta.bpSource.s1Source.poke(BpuPredictionSource.Stage1.AbtbUtage)
      val pred = meta.s3Prediction
      pred.taken.poke(true.B); pred.cfiPosition.poke(8.U); attribute(pred.attribute)
      val branch = d.io.branch
      branch.taken.poke(false.B); branch.cfiPosition.poke(8.U); attribute(branch.attribute)
      val entry = meta.mbtbMeta.entries(0)(0)
      entry.rawHit.poke(true.B); entry.position.poke(8.U); attribute(entry.attribute)
      meta.bpSource.s3Source.poke(BpuPredictionSource.Stage3.Sc)
      meta.scUsed.poke(1.U); meta.tageUsed.poke(1.U)
      for (s2 <- Seq(false, true); s3 <- Seq(false, true)) {
        meta.bpSource.s2Override.poke(s2.B); meta.bpSource.s3Override.poke(s3.B)
        d.io.reason.expect(TopDownCounters.SCMissBubble.id.U)
      }
      meta.scUsed.poke(0.U); d.io.reason.expect(TopDownCounters.TAGEMissBubble.id.U)
      meta.tageUsed.poke(0.U); d.io.reason.expect(TopDownCounters.BTBMissBubble.id.U)
      // Known false NT: S3 source enum alone says fallthrough; participation masks retain direction responsibility.
      pred.taken.poke(false.B); branch.taken.poke(true.B)
      meta.bpSource.s3Source.poke(BpuPredictionSource.Stage3.Fallthrough)
      meta.tageUsed.poke(1.U); d.io.reason.expect(TopDownCounters.TAGEMissBubble.id.U)
      meta.scUsed.poke(1.U); d.io.reason.expect(TopDownCounters.SCMissBubble.id.U)
      entry.rawHit.poke(false.B); d.io.reason.expect(TopDownCounters.BTBMissBubble.id.U)
      entry.rawHit.poke(true.B)
      // Earlier known NT and later false taken have different direction providers.
      val second = meta.mbtbMeta.entries(0)(1)
      second.rawHit.poke(true.B); second.position.poke(2.U); attribute(second.attribute)
      pred.taken.poke(true.B); branch.cfiPosition.poke(2.U)
      meta.scUsed.poke(1.U); meta.tageUsed.poke(3.U)
      d.io.reason.expect(TopDownCounters.TAGEMissBubble.id.U)
      branch.cfiPosition.poke(12.U); d.io.reason.expect(TopDownCounters.SCMissBubble.id.U)
      // Attribute/position/existence corrections belong to main BTB.
      branch.cfiPosition.poke(8.U); attribute(branch.attribute, 2)
      d.io.reason.expect(TopDownCounters.BTBMissBubble.id.U)
      attribute(branch.attribute, 0); branch.taken.poke(false.B)
      d.io.reason.expect(TopDownCounters.BTBMissBubble.id.U)
      // Same-branch target errors belong to the S3 target provider.
      branch.taken.poke(true.B); attribute(branch.attribute, 3, 1); attribute(pred.attribute, 3, 1)
      meta.bpSource.s3Source.poke(BpuPredictionSource.Stage3.Ras)
      d.io.reason.expect(TopDownCounters.RASMissBubble.id.U)
      meta.bpSource.s3Source.poke(BpuPredictionSource.Stage3.ITTage)
      d.io.reason.expect(TopDownCounters.ITTAGEMissBubble.id.U)
      meta.bpSource.s3Source.poke(BpuPredictionSource.Stage3.Mbtb)
      d.io.reason.expect(TopDownCounters.BTBMissBubble.id.U)
    }
  }
}
