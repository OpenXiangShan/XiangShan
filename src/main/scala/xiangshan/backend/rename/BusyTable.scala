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

package xiangshan.backend.rename

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.WbConfig.{IntWB, VfWB, NoWB, PregWB}
import xiangshan.backend.issue.SchdBlockParams
import xiangshan.backend.datapath.{DataSource}

class BusyTableReadIO(implicit p: Parameters) extends XSBundle {
  val req = Input(UInt(PhyRegIdxWidth.W))
  val resp = Output(Bool())
  val loadDependency = Vec(LoadPipelineWidth, Output(UInt(LoadDependencyWidth.W)))
}

class BusyTable(numReadPorts: Int, numWritePorts: Int, numPhyPregs: Int, pregWB: PregWB)(implicit p: Parameters, params: SchdBlockParams) extends XSModule with HasPerfEvents {
  val io = IO(new Bundle() {
    // set preg state to busy
    val allocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    // set preg state to ready (write back regfile + rob walk)
    val wbPregs = Vec(numWritePorts, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    // fast wakeup
    val wakeUp: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpInValidBundle)
    // cancelFromDatapath
    val cancel = Vec(backendParams.numExu, Flipped(ValidIO(new CancelSignal)))
    // cancelFromMem
    val ldCancel = Vec(backendParams.LdExuCnt, Flipped(new LoadCancelIO))
    // read preg state
    val read = Vec(numReadPorts, new BusyTableReadIO)
  })

  val loadDependency = RegInit(0.U.asTypeOf(Vec(numPhyPregs, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))))
  val shiftLoadDependency = Wire(Vec(io.wakeUp.size, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W))))
  val tableUpdate = Wire(Vec(numPhyPregs, Bool()))
  val wakeupOHVec = Wire(Vec(numPhyPregs, UInt(io.wakeUp.size.W)))

  def reqVecToMask(rVec: Vec[Valid[UInt]]): UInt = {
    ParallelOR(rVec.map(v => Mux(v.valid, UIntToOH(v.bits), 0.U)))
  }

  shiftLoadDependency.zip(io.wakeUp.map(_.bits.loadDependency)).zip(params.wakeUpInExuSources.map(_.name)).foreach {
    case ((deps, originalDeps), name) => deps.zip(originalDeps).zipWithIndex.foreach {
      case ((dep, originalDep), deqPortIdx) =>
        if (params.backendParam.getLdExuIdx(params.backendParam.allExuParams.find(_.name == name).get) == deqPortIdx)
          dep := 1.U
        else
          dep := originalDep << 1
    }
  }

  wakeupOHVec.zipWithIndex.foreach{ case (wakeupOH, idx) =>
    val tmp = pregWB match {
      case _: IntWB => io.wakeUp.map(x => x.valid && x.bits.rfWen && UIntToOH(x.bits.pdest)(idx) && !LoadShouldCancel(Some(x.bits.loadDependency), io.ldCancel))
      case _: VfWB => io.wakeUp.map(x => x.valid && (x.bits.fpWen || x.bits.vecWen) && UIntToOH(x.bits.pdest)(idx) && !LoadShouldCancel(Some(x.bits.loadDependency), io.ldCancel))
    }
    wakeupOH := (if (io.wakeUp.nonEmpty) VecInit(tmp.toSeq).asUInt else 0.U)
  }
  val wbMask = reqVecToMask(io.wbPregs)
  val allocMask = reqVecToMask(io.allocPregs)
  val wakeUpMask = VecInit(wakeupOHVec.map(_.orR).toSeq).asUInt
  val cancelMask = pregWB match {
    case _: IntWB => io.cancel.map(x => Mux(x.valid && x.bits.rfWen, UIntToOH(x.bits.pdest), 0.U)).fold(0.U)(_ | _)
    case _: VfWB => io.cancel.map(x => Mux(x.valid && (x.bits.fpWen || x.bits.vecWen), UIntToOH(x.bits.pdest), 0.U)).fold(0.U)(_ | _)
    case _: NoWB => throw new IllegalArgumentException("NoWB is not permitted")
  }
  val ldCancelMask = loadDependency.map(x => LoadShouldCancel(Some(x), io.ldCancel))

  loadDependency.zipWithIndex.foreach{ case (ldDp, idx) =>
    when(allocMask(idx) || cancelMask(idx) || wbMask(idx) || ldCancelMask(idx)) {
      ldDp := 0.U.asTypeOf(ldDp)
    }.elsewhen(wakeUpMask(idx)) {
      ldDp := (if (io.wakeUp.nonEmpty) Mux1H(wakeupOHVec(idx), shiftLoadDependency) else 0.U.asTypeOf(ldDp))
    }.elsewhen(ldDp.map(x => x.orR).reduce(_ | _)) {
      ldDp := VecInit(ldDp.map(x => x << 1))
    }
  }

  /*
  we can ensure that the following conditions are mutually exclusive
  wakeUp and cancel (same pdest) may arrive at the same cycle
  for a pdest:
    rename alloc => wakeUp / cancel => ... => wakeUp / cancel => wakeUp
  or
    rename alloc => wbMask  //TODO we still need wbMask because wakeUp signal is partial now
  the bypass state lasts for a maximum of one cycle, cancel(=> busy) or else(=> regFile)
   */
  val table = VecInit((0 until numPhyPregs).zip(tableUpdate).map{ case (idx, update) =>
    RegEnable(update, 0.U(1.W), allocMask(idx) || cancelMask(idx) || ldCancelMask(idx) || wakeUpMask(idx) || wbMask(idx))
  }).asUInt

  tableUpdate.zipWithIndex.foreach{ case (update, idx) =>
    when(allocMask(idx) || cancelMask(idx) || ldCancelMask(idx)) {
      update := true.B                                    //busy
    }.elsewhen(wakeUpMask(idx) || wbMask(idx)) {
      update := false.B                                   //ready
    }.otherwise {
      update := table(idx)
    }
  }

  io.read.foreach{ case res =>
    res.resp := !table(res.req)
    res.loadDependency := loadDependency(res.req)
  }

  val oddTable = table.asBools.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)
  val evenTable = table.asBools.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
  val busyCount = RegNext(RegNext(PopCount(oddTable)) + RegNext(PopCount(evenTable)))

  XSPerfAccumulate("busy_count", PopCount(table))

  val perfEvents = Seq(
    ("std_freelist_1_4_valid", busyCount < (numPhyPregs / 4).U                                      ),
    ("std_freelist_2_4_valid", busyCount > (numPhyPregs / 4).U && busyCount <= (numPhyPregs / 2).U    ),
    ("std_freelist_3_4_valid", busyCount > (numPhyPregs / 2).U && busyCount <= (numPhyPregs * 3 / 4).U),
    ("std_freelist_4_4_valid", busyCount > (numPhyPregs * 3 / 4).U                                  )
  )
  generatePerfEvent()
}
