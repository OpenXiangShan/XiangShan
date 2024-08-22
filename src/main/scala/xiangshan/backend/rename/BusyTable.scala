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
import xiangshan.backend.datapath.WbConfig._
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
    val og0Cancel = Input(ExuVec())
    // cancelFromMem
    val ldCancel = Vec(backendParams.LdExuCnt, Flipped(new LoadCancelIO))
    // read preg state
    val read = Vec(numReadPorts, new BusyTableReadIO)
  })

  val allExuParams = params.backendParam.allExuParams
  val intBusyTableNeedLoadCancel = allExuParams.map(x =>
    x.needLoadDependency && x.writeIntRf && x.iqWakeUpSourcePairs.map(y => y.sink.getExuParam(allExuParams).readIntRf).foldLeft(false)(_ || _)
  ).reduce(_ || _)
  val fpBusyTableNeedLoadCancel = allExuParams.map(x =>
    x.needLoadDependency && x.writeFpRf && x.iqWakeUpSourcePairs.map(y => y.sink.getExuParam(allExuParams).readFpRf).foldLeft(false)(_ || _)
  ).reduce(_ || _)
  val vfBusyTableNeedLoadCancel = allExuParams.map(x =>
    x.needLoadDependency && x.writeVfRf && x.iqWakeUpSourcePairs.map(y => y.sink.getExuParam(allExuParams).readVecRf).foldLeft(false)(_ || _)
  ).reduce(_ || _)
  val v0BusyTableNeedLoadCancel = allExuParams.map(x =>
    x.needLoadDependency && x.writeV0Rf && x.iqWakeUpSourcePairs.map(y => y.sink.getExuParam(allExuParams).readVecRf).foldLeft(false)(_ || _)
  ).reduce(_ || _)
  val vlBusyTableNeedLoadCancel = allExuParams.map(x =>
    x.needLoadDependency && x.writeVlRf && x.iqWakeUpSourcePairs.map(y => y.sink.getExuParam(allExuParams).readVlRf).foldLeft(false)(_ || _)
  ).reduce(_ || _)
  val needLoadCancel = pregWB match {
    case IntWB(_, _) => intBusyTableNeedLoadCancel
    case FpWB(_, _) => fpBusyTableNeedLoadCancel
    case VfWB(_, _) => vfBusyTableNeedLoadCancel
    case V0WB(_, _) => v0BusyTableNeedLoadCancel
    case VlWB(_, _) => vlBusyTableNeedLoadCancel
    case _ => throw new IllegalArgumentException(s"WbConfig ${pregWB} is not permitted")
  }
  if (!needLoadCancel) println(s"[BusyTable]: WbConfig ${pregWB} busyTable don't need loadCancel")
  val loadCancel = if (needLoadCancel) io.ldCancel else 0.U.asTypeOf(io.ldCancel)
  val wakeUpIn = pregWB match {
    case IntWB(_, _) => io.wakeUp.filter(_.bits.params.writeIntRf)
    case FpWB(_, _) => io.wakeUp.filter(_.bits.params.writeFpRf)
    case VfWB(_, _) => io.wakeUp.filter(_.bits.params.writeVfRf)
    case V0WB(_, _) => io.wakeUp.filter(_.bits.params.writeV0Rf)
    case VlWB(_, _) => io.wakeUp.filter(_.bits.params.writeVlRf)
    case _ => throw new IllegalArgumentException(s"WbConfig ${pregWB} is not permitted")
  }
  val loadDependency = RegInit(0.U.asTypeOf(Vec(numPhyPregs, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))))
  val shiftLoadDependency = Wire(Vec(wakeUpIn.size, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W))))
  val tableUpdate = Wire(Vec(numPhyPregs, Bool()))
  val wakeupOHVec = Wire(Vec(numPhyPregs, UInt(wakeUpIn.size.W)))

  def reqVecToMask(rVec: Vec[Valid[UInt]]): UInt = {
    ParallelOR(rVec.map(v => Mux(v.valid, UIntToOH(v.bits), 0.U)))
  }

  shiftLoadDependency.zip(wakeUpIn).map{ case (deps, wakeup) =>
    if (wakeup.bits.params.hasLoadExu) {
      deps.zipWithIndex.map{ case (dep, i) =>
        if (backendParams.getLdExuIdx(wakeup.bits.params) == i) dep := 1.U
        else dep := 0.U
      }
    }
    else {
      deps.zip(wakeup.bits.loadDependency).map{ case (sink, source) =>
        sink := source << 1
      }
    }
  }

  wakeupOHVec.zipWithIndex.foreach{ case (wakeupOH, idx) =>
    val tmp = pregWB match {
      case IntWB(_, _) => wakeUpIn.map(x => x.valid && x.bits.rfWen  && UIntToOH(x.bits.pdest)(idx) && !LoadShouldCancel(Some(x.bits.loadDependency), loadCancel) && !(x.bits.is0Lat && io.og0Cancel(x.bits.params.exuIdx)))
      case FpWB(_, _)  => wakeUpIn.map(x => x.valid && x.bits.fpWen  && UIntToOH(x.bits.pdest)(idx) && !LoadShouldCancel(Some(x.bits.loadDependency), loadCancel) && !(x.bits.is0Lat && io.og0Cancel(x.bits.params.exuIdx)))
      case VfWB(_, _)  => wakeUpIn.map(x => x.valid && x.bits.vecWen && UIntToOH(x.bits.pdest)(idx) && !LoadShouldCancel(Some(x.bits.loadDependency), loadCancel) && !(x.bits.is0Lat && io.og0Cancel(x.bits.params.exuIdx)))
      case V0WB(_, _)  => wakeUpIn.map(x => x.valid && x.bits.v0Wen  && UIntToOH(x.bits.pdest)(idx) && !LoadShouldCancel(Some(x.bits.loadDependency), loadCancel) && !(x.bits.is0Lat && io.og0Cancel(x.bits.params.exuIdx)))
      case VlWB(_, _)  => wakeUpIn.map(x => x.valid && x.bits.vlWen  && UIntToOH(x.bits.pdest)(idx) && !LoadShouldCancel(Some(x.bits.loadDependency), loadCancel) && !(x.bits.is0Lat && io.og0Cancel(x.bits.params.exuIdx)))
      case _ => throw new IllegalArgumentException(s"WbConfig ${pregWB} is not permitted")
    }
    wakeupOH := (if (wakeUpIn.nonEmpty) VecInit(tmp.toSeq).asUInt else 0.U)
  }
  val wbMask = reqVecToMask(io.wbPregs)
  val allocMask = reqVecToMask(io.allocPregs)
  val wakeUpMask = VecInit(wakeupOHVec.map(_.orR).toSeq).asUInt
  val ldCancelMask = loadDependency.map(x => LoadShouldCancel(Some(x), loadCancel))

  loadDependency.zipWithIndex.foreach{ case (ldDp, idx) =>
    when(allocMask(idx) || wbMask(idx) || ldCancelMask(idx)) {
      ldDp := 0.U.asTypeOf(ldDp)
    }.elsewhen(wakeUpMask(idx)) {
      ldDp := (if (wakeUpIn.nonEmpty) Mux1H(wakeupOHVec(idx), shiftLoadDependency) else 0.U.asTypeOf(ldDp))
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
    RegEnable(update, 0.U(1.W), allocMask(idx) || ldCancelMask(idx) || wakeUpMask(idx) || wbMask(idx))
  }).asUInt

  tableUpdate.zipWithIndex.foreach{ case (update, idx) =>
    when(allocMask(idx) || ldCancelMask(idx)) {
      update := true.B                                    //busy
      if (idx == 0 && pregWB.isInstanceOf[IntWB]) {
          // Int RegFile 0 is always ready
          update := false.B
      }
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
    ("bt_std_freelist_1_4_valid", busyCount < (numPhyPregs / 4).U                                      ),
    ("bt_std_freelist_2_4_valid", busyCount > (numPhyPregs / 4).U && busyCount <= (numPhyPregs / 2).U    ),
    ("bt_std_freelist_3_4_valid", busyCount > (numPhyPregs / 2).U && busyCount <= (numPhyPregs * 3 / 4).U),
    ("bt_std_freelist_4_4_valid", busyCount > (numPhyPregs * 3 / 4).U                                  )
  )
  generatePerfEvent()
}
