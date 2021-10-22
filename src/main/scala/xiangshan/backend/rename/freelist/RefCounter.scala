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

package xiangshan.backend.rename.freelist

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._



class RefCounter(size: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val allocate = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val deallocate = Vec(CommitWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val freeRegs = Vec(CommitWidth, ValidIO(UInt(PhyRegIdxWidth.W)))
  })

  val allocate = RegNext(io.allocate)
  val deallocate = RegNext(io.deallocate)

  // recording referenced times of each physical registers
  // refCounter: increase at rename; decrease at walk/commit
  // Originally 0-31 registers have counters of ones.
  val refCounter = RegInit(VecInit(Seq.fill(32)(1.U(IntRefCounterWidth.W)) ++ Seq.fill(NRPhyRegs - 32)(0.U(IntRefCounterWidth.W))))
  val refCounterNext = WireInit(refCounter)

  /**
    * Deallocation: when refCounter becomes zero, the register can be released to freelist
    */
  for ((de, i) <- deallocate.zipWithIndex) {
    val isNonZero = de.valid && refCounter(de.bits) =/= 0.U
    val hasDuplicate = deallocate.take(i).map(de => de.valid && de.bits === deallocate(i).bits)
    val blockedByDup = if (i == 0) false.B else VecInit(hasDuplicate).asUInt.orR
    val isFreed = refCounter(RegNext(de.bits)) === 0.U
    io.freeRegs(i).valid := RegNext(isNonZero && !blockedByDup) && isFreed
    io.freeRegs(i).bits := RegNext(deallocate(i).bits)
  }

  /**
    * Actually refCounter has nothing to do with freelist (list of free registers)
    * but we write refCounter here because freelist needs to be reset
    * according to the ref counters.
    *
    * Every time the physical register is allocated or deallocated,
    * the counter is increased or decreased by one.
    *
    * refCounter update:
    * (1) rename: increase as move instructions are renamed
    * (2) walk: decrease as physical registers are released (pdest)
    * (3) commit: decrease as physical registers are release (old_pdest)
    *
    * We don't count the number of references for physical register 0.
    * It should never be released to freelist.
    */
  for (i <- 1 until NRPhyRegs) {
    val numAlloc = PopCount(allocate.map(alloc => alloc.valid && alloc.bits === i.U))
    val numDealloc = PopCount(deallocate.map(dealloc => dealloc.valid && dealloc.bits === i.U))
    refCounterNext(i) := refCounter(i) + numAlloc - numDealloc
    XSError(RegNext(refCounter(i) + numAlloc < numDealloc), p"why $i?\n")
    refCounter(i) := refCounterNext(i)
  }

  for (i <- 0 until RobSize) {
    val numCounters = PopCount(refCounter.map(_ === i.U))
    XSPerfAccumulate(s"ref_counter_$i", numCounters)
    val isFreed = io.freeRegs.map(f => f.valid && f.bits === i.U)
    XSPerfAccumulate(s"free_reg_$i", VecInit(isFreed).asUInt.orR)
  }
}

