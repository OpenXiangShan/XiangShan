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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.WbConfig.{IntWB, VfWB, PregWB}
import xiangshan.backend.issue.SchdBlockParams
import xiangshan.backend.datapath.{DataSource}

object RegStatus {
  val busy = "b11".U
  val bypass = "b10".U
  val regFile = "b00".U

  def apply() = UInt(2.W)
}

class BusyTableReadIO(implicit p: Parameters) extends XSBundle {
  val req = Input(UInt(PhyRegIdxWidth.W))
  val resp = Output(Bool())
  val dataSource = Output(DataSource())
  val l1ExuOH = Output(ExuVec())
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
    // read preg state
    val read = Vec(numReadPorts, new BusyTableReadIO)
  })

  val wakeUpReg = Reg(params.genIQWakeUpInValidBundle)
  val table = RegInit(VecInit(Seq.fill(numPhyPregs)(0.U(2.W))))
  val tableUpdate = Wire(Vec(numPhyPregs, RegStatus()))
  val wakeUpFilterLS = io.wakeUp.filter(x => (x.bits.exuIdx != backendParams.getExuIdx("LDU0")) && (x.bits.exuIdx != backendParams.getExuIdx("LDU1")) ) //TODO

  def reqVecToMask(rVec: Vec[Valid[UInt]]): UInt = {
    ParallelOR(rVec.map(v => Mux(v.valid, UIntToOH(v.bits), 0.U)))
  }

  val wbMask = reqVecToMask(io.wbPregs)
  val allocMask = reqVecToMask(io.allocPregs)
  val wakeUpMask = pregWB match {
    case _: IntWB => ParallelOR(wakeUpFilterLS.map(x => Mux(x.valid && x.bits.rfWen && !x.bits.loadDependency.asUInt.orR, UIntToOH(x.bits.pdest), 0.U))) //TODO: dont implement "load -> wakeUp other -> wakeUp BusyTable" now
    case _: VfWB => ParallelOR(wakeUpFilterLS.map(x => Mux(x.valid && (x.bits.fpWen || x.bits.vecWen) && !x.bits.loadDependency.asUInt.orR, UIntToOH(x.bits.pdest), 0.U)))
  }
  val cancelMask = pregWB match {
    case _: IntWB => ParallelOR(io.cancel.map(x => Mux(x.valid && x.bits.rfWen, UIntToOH(x.bits.pdest), 0.U)))
    case _: VfWB => ParallelOR(io.cancel.map(x => Mux(x.valid && (x.bits.fpWen || x.bits.vecWen), UIntToOH(x.bits.pdest), 0.U)))
  }

  /*
  we can ensure that the following conditions are mutually exclusive
  wakeUp and cancel (same pdest) would not arrive at the same cycle
  for a pdest:
    rename alloc => wakeUp => cancel => ... => wakeUp => cancel => wakeUp
  or
    rename alloc => wbMask  //TODO we still need wbMask because wakeUp signal is partial now
  the bypass state lasts for a maximum of one cycle, cancel(=> busy) or else(=> regFile)
   */
  tableUpdate.zipWithIndex.foreach{ case (update, idx) =>
    when(allocMask(idx)) {
      update := RegStatus.busy
    }.elsewhen(wakeUpMask(idx)) {
      update := RegStatus.bypass
    }.elsewhen(cancelMask(idx)) {
      update := RegStatus.busy
    }.elsewhen((table(idx) === RegStatus.bypass) || wbMask(idx)) {
      update := RegStatus.regFile
    }.otherwise {
      update := table(idx)
    }
  }

  io.read.foreach{ case res =>
    res.resp := Mux(!cancelMask(res.req), !table(res.req).andR, false.B)
    res.dataSource.value := Mux(table(res.req) === DataSource.bypass, DataSource.bypass, DataSource.reg)
    val wakeUpExuOHVec = wakeUpReg.map{ case x =>
      val v: Bool = pregWB match {
        case _: IntWB => x.valid && x.bits.rfWen
        case _: VfWB => x.valid && (x.bits.fpWen || x.bits.vecWen)
      }
      val pdestHit = res.req === x.bits.pdest
      val isBypass = table(res.req) === DataSource.bypass
      Mux(v && pdestHit && isBypass, MathUtils.IntToOH(x.bits.exuIdx).U(backendParams.numExu.W), 0.U)
    }
    res.l1ExuOH := Mux(table(res.req) === DataSource.bypass, ParallelOR(wakeUpExuOHVec), 0.U).asBools
  }

  table := tableUpdate
  wakeUpReg := io.wakeUp

  val oddTable = table.zipWithIndex.filter(_._2 % 2 == 1).map(!_._1.orR)
  val evenTable = table.zipWithIndex.filter(_._2 % 2 == 0).map(!_._1.orR)
  val busyCount = RegNext(RegNext(PopCount(oddTable)) + RegNext(PopCount(evenTable)))

  XSPerfAccumulate("busy_count", PopCount(table.map(_.andR)))

  val perfEvents = Seq(
    ("std_freelist_1_4_valid", busyCount < (numPhyPregs / 4).U                                      ),
    ("std_freelist_2_4_valid", busyCount > (numPhyPregs / 4).U && busyCount <= (numPhyPregs / 2).U    ),
    ("std_freelist_3_4_valid", busyCount > (numPhyPregs / 2).U && busyCount <= (numPhyPregs * 3 / 4).U),
    ("std_freelist_4_4_valid", busyCount > (numPhyPregs * 3 / 4).U                                  )
  )
  generatePerfEvent()
}
