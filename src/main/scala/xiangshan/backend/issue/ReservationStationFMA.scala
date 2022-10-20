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

package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils._

case class FMARSParams()

class FMARSWrapper(modGen: RSMod)(implicit p: Parameters) extends BaseReservationStationWrapper(modGen) {
  params.needScheduledBit = true

  override lazy val module = new FMARSImp(params, this)
}

class FMARSImp(params: RSParams, wrapper: FMARSWrapper) extends BaseReservationStationImp(params, wrapper) {

  extra.fmaMid <> rs.flatMap(_.extra.fmaMid)
}

class FMARS(params: RSParams)(implicit p: Parameters) extends BaseReservationStation(params) {
  for (i <- 0 until params.numDeq) {
    if (params.hasMidState) {
      extra.fmaMid(i).waitForAdd := !s2_all_src_ready(i)
      extra.fmaMid(i).in.valid := !s2_first_issue(i)
      XSPerfAccumulate(s"fma_partial2_issue_$i", io.deq(i).fire && extra.fmaMid(i).waitForAdd)
      XSPerfAccumulate(s"fma_final_issue_$i", io.deq(i).fire && extra.fmaMid(i).in.valid)
    }
  }
  // For FMA instrutions whose third operand is not ready, once they are successfully issued (T0),
  // the FMUL intermediate result will be ready in two clock cycles (T2).
  // If the third operand is ready at T2, this instruction will be selected in T3 and issued at T4.
  // Note that at cycle T4, FMUL finishes as well and it is able to proceed to FADD.
  // Thus, we can set the midState to true two cycles earlier at T0 and forward the result if possible.
  val midFinished2 = extra.fmaMid.zip(io.deq).map(x => x._1.waitForAdd && x._2.fire)
  val updateMid = ParallelMux(midFinished2, s2_issuePtrOH)
  statusArray.io.updateMidState := updateMid

  // FMUL intermediate results are ready in two cycles
  val midFinished2T0 = midFinished2.zip(s2_deq).map{ case (v, deq) =>
    // However, it may be flushed by redirect at T0.
    // If flushed at T0, new instruction enters at T1 and writes the entry at T2.
    // This is a rare case because usually instructions enter RS in-order,
    // unless dispatch2 is blocked.
    v && !deq.bits.uop.robIdx.needFlush(io.redirect)
  }
  val midIssuePtrOHT1 = midFinished2T0.zip(s2_issuePtrOH).map(x => RegEnable(x._2, x._1))
  val midIssuePtrT1 = midFinished2T0.zip(s2_issuePtr).map(x => RegEnable(x._2, x._1))
  val midFinished2T1 = midFinished2T0.map(v => RegNext(v))
  // No flush here: the fma may dequeue at this stage.
  // If cancelled at T1, data written at T2. However, new instruction writes at least at T3.
  val midIssuePtrOHT2 = midFinished2T1.zip(midIssuePtrOHT1).map(x => RegEnable(x._2, x._1))
  val midIssuePtrT2 = midFinished2T1.zip(midIssuePtrT1).map(x => RegEnable(x._2, x._1))
  val midFinished2T2 = midFinished2T1.map(v => RegNext(v))
  for (i <- 0 until params.numDeq) {
    dataArray.io.partialWrite(i).enable := midFinished2T2(i)
    dataArray.io.partialWrite(i).mask := DontCare
    dataArray.io.partialWrite(i).addr := midIssuePtrOHT2(i)
    val writeData = extra.fmaMid(i).out.bits.asUInt
    require(writeData.getWidth <= 2 * params.dataBits, s"why ${writeData.getWidth}???")
    require(writeData.getWidth > params.dataBits, s"why ${writeData.getWidth}???")
    dataArray.io.partialWrite(i).data(0) := writeData(params.dataBits - 1, 0)
    dataArray.io.partialWrite(i).data(1) := writeData(writeData.getWidth - 1, params.dataBits)
    val readData = Cat(io.deq(i).bits.src(1), io.deq(i).bits.src(0))
    extra.fmaMid(i).in.bits := readData.asTypeOf(extra.fmaMid(i).in.bits.cloneType)
  }

  // How to forward intermediate results:
  // (1) T0 issued FMA is selected at T1 and issued at T2: forward from FMUL results
  //     NOTE: In this case, this instruction has been issued and the entry is freed.
  //           Do NOT write data back to data array.
  // (2) T0 issued FMA is selected at T2: RegNext FMUL result at the issue stage
  // Thus, at issue stage:
  // (1.1) If the instruction matches FMA/FMUL two cycles ealier, we issue it and it goes to FADD
  // (1.2) If the instruction matches FMA/FMUL two cycles ealier and it's blocked, we need to hold the result
  // At select stage: (2) bypass FMUL intermediate results from write ports if possible.
  val issuedAtT0 = midFinished2T2.zip(midIssuePtrT2)
  for (i <- 0 until params.numDeq) {
    // cond11: condition (1.1) from different issue ports
    val cond11 = issuedAtT0.map(x => x._1 && x._2 === s2_issuePtr(i))
    for ((c, j) <- cond11.zipWithIndex) {
      when (c) {
        extra.fmaMid(i).in.bits := extra.fmaMid(j).out.bits
        // We should NOT write the intermediate result back to DataArray,
        // when this entry has been selected and arrived at the issue stage.
        // This entry may be allocated for new instructions from dispatch.
        when (io.deq(i).valid) {
          dataArray.io.partialWrite(j).enable := false.B
        }
      }
    }
    val cond11Issued = io.deq(i).fire && extra.fmaMid(i).in.valid && VecInit(cond11).asUInt.orR
    XSPerfAccumulate(s"fma_final_issue_cond11_$i", cond11Issued)
    // cond12: blocked at the issue stage
    val cond12 = cond11.map(_ && io.deq(i).valid && !io.deq(i).ready)
    val hasCond12 = VecInit(cond12).asUInt.orR
    val hasCond12Reg = RegInit(false.B)
    when (hasCond12) {
      hasCond12Reg := true.B
    }.elsewhen (io.deq(i).ready) {
      hasCond12Reg := false.B
    }
    when (hasCond12Reg) {
      // TODO: remove these unnecessary registers (use pipeline registers instead)
      extra.fmaMid(i).in.bits := RegEnable(Mux1H(cond12, extra.fmaMid.map(_.out.bits)), hasCond12)
    }
    val cond12Issued = io.deq(i).fire && extra.fmaMid(i).in.valid && hasCond12Reg
    XSPerfAccumulate(s"fma_final_issue_cond12_$i", cond12Issued)
    // cond2: selected at the select stage
    val cond2 = issuedAtT0.map(x => x._1 && x._2 === s1_issuePtr(i))
    for ((c, j) <- cond2.zipWithIndex) {
      when (c) {
        s1_out(i).bits.src(0) := dataArray.io.partialWrite(j).data(0)
        s1_out(i).bits.src(1) := dataArray.io.partialWrite(j).data(1)
      }
    }
    val cond2Selected = s1_out_fire(i) && VecInit(cond2).asUInt.orR
    XSPerfAccumulate(s"fma_final_selected_cond2_$i", cond2Selected)
  }

  allSrcReady.zip(s1_all_src_ready).map(a => a._1 := a._2)
  allSrcReady1.zip(statusArray.io.update.map(_.data.allSrcReady)).map(a => a._1 := a._2)
  allSrcReadyLast := statusArray.io.allSrcReady.last
}