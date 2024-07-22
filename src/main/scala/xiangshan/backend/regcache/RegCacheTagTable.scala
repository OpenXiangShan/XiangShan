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

package xiangshan.backend.regcache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.BackendParams
import xiangshan.backend.issue.SchdBlockParams
import freechips.rocketchip.util.SeqToAugmentedSeq

class RegCacheTagTable(numReadPorts: Int)(implicit p: Parameters, schdParams: SchdBlockParams) extends XSModule {

  val io = IO(new RegCacheTagTableIO(numReadPorts))

  println(s"[RegCacheTagTable] readPorts: ${numReadPorts}, " +
    s"writePorts: ${backendParams.getIntExuRCWriteSize} + ${backendParams.getMemExuRCWriteSize}")

  println(s"[RegCacheTagTable] addrWidth: ${RegCacheIdxWidth}, tagWidth: ${schdParams.pregIdxWidth}")

  private val IntRegCacheReadSize = numReadPorts
  private val IntRegCacheWriteSize = backendParams.getIntExuRCWriteSize
  private val MemRegCacheReadSize = numReadPorts
  private val MemRegCacheWriteSize = backendParams.getMemExuRCWriteSize

  val IntRCTagTable = Module(new RegCacheTagModule("IntRCTagTable", IntRegCacheSize, IntRegCacheReadSize, IntRegCacheWriteSize, 
                                                   RegCacheIdxWidth - 1, schdParams.pregIdxWidth))

  val MemRCTagTable = Module(new RegCacheTagModule("MemRCTagTable", MemRegCacheSize, MemRegCacheReadSize, MemRegCacheWriteSize, 
                                                   RegCacheIdxWidth - 1, schdParams.pregIdxWidth))

  // read
  io.readPorts
  .lazyZip(IntRCTagTable.io.readPorts.lazyZip(MemRCTagTable.io.readPorts))
  .foreach{ case (r_in, (r_int, r_mem)) => 
    r_int.tag  := r_in.tag
    r_mem.tag  := r_in.tag
    r_in.valid := r_int.valid || r_mem.valid
    r_in.addr  := Mux(r_int.valid, Cat("b0".U, r_int.addr), Cat("b1".U, r_mem.addr))
  }

  // write
  val wakeupFromIQNeedWriteRC = io.wakeupFromIQ.filter(_.bits.params.needWriteRegCache)
  val shiftLoadDependency = Wire(Vec(wakeupFromIQNeedWriteRC.size, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W))))

  require(wakeupFromIQNeedWriteRC.size == IntRegCacheWriteSize + MemRegCacheWriteSize, "wakeup size should be equal to RC write size")

  shiftLoadDependency.zip(wakeupFromIQNeedWriteRC.map(_.bits.loadDependency)).zip(schdParams.wakeUpInExuSources.map(_.name)).foreach {
    case ((deps, originalDeps), name) => deps.zip(originalDeps).zipWithIndex.foreach {
      case ((dep, originalDep), deqPortIdx) =>
        if (backendParams.getLdExuIdx(backendParams.allExuParams.find(_.name == name).get) == deqPortIdx)
          dep := 1.U
        else
          dep := originalDep << 1
    }
  }

  (IntRCTagTable.io.writePorts ++ MemRCTagTable.io.writePorts).lazyZip(wakeupFromIQNeedWriteRC).lazyZip(shiftLoadDependency)
  .foreach{ case (w, wakeup, ldDp) => 
    w.wen  := wakeup.valid && wakeup.bits.rfWen && !LoadShouldCancel(Some(wakeup.bits.loadDependency), io.ldCancel) && !(wakeup.bits.is0Lat && io.og0Cancel(wakeup.bits.params.exuIdx))
    w.addr := wakeup.bits.rcDest.get(RegCacheIdxWidth - 2, 0)
    w.tag  := wakeup.bits.pdest
    w.loadDependency := ldDp
  }

  // cancel
  val allocVec = (IntRCTagTable.io.tagVec ++ MemRCTagTable.io.tagVec).map{ t => 
    io.allocPregs.map(a => a.valid && a.bits === t).asUInt.orR
  }

  val replaceVec = IntRCTagTable.io.tagVec.map{ t => 
    IntRCTagTable.io.writePorts.map(w => w.wen && w.tag === t).asUInt.orR
  }       ++       MemRCTagTable.io.tagVec.map{ t => 
    MemRCTagTable.io.writePorts.map(w => w.wen && w.tag === t).asUInt.orR
  }

  val ldCancelVec = (IntRCTagTable.io.loadDependencyVec ++ MemRCTagTable.io.loadDependencyVec).map{ ldDp => 
    LoadShouldCancel(Some(ldDp), io.ldCancel)
  }

  val cancelVec = allocVec.lazyZip(replaceVec).lazyZip(ldCancelVec).lazyZip((IntRCTagTable.io.validVec ++ MemRCTagTable.io.validVec))
  .map{ case (alloc, rep, ldCancel, v) => 
    (alloc || rep || ldCancel) && v
  }

  (IntRCTagTable.io.cancelVec ++ MemRCTagTable.io.cancelVec).zip(cancelVec).foreach{ case (cancelIn, cancel) => 
    cancelIn := cancel
  }
}

class RegCacheTagTableIO(numReadPorts: Int)(implicit p: Parameters, schdParams: SchdBlockParams) extends XSBundle {

  val readPorts = Vec(numReadPorts, new RCTagTableReadPort(RegCacheIdxWidth, schdParams.pregIdxWidth))

  val wakeupFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(schdParams.genIQWakeUpInValidBundle)

  // set preg state to invalid
  val allocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))

  // cancelFromDatapath
  val og0Cancel = Input(ExuOH(backendParams.numExu))

  // cancelFromMem
  val ldCancel = Vec(backendParams.LdExuCnt, Flipped(new LoadCancelIO))
}
