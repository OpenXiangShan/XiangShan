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
  val refCounter = RegInit(VecInit.fill(size)(0.U(IntRefCounterWidth.W)))
  val refCounterInc = WireInit(refCounter)
  val refCounterDec = WireInit(refCounter)
  val refCounterNext = WireInit(refCounter)

  // One-hot Encoding for allocation and de-allocation
  val allocateOH = allocate.map(alloc => UIntToOH(alloc.bits))
  val deallocateOH = deallocate.map(dealloc => UIntToOH(dealloc.bits))

  /**
    * De-allocation: when refCounter becomes zero, the register can be released to freelist
    */
  for ((de, i) <- deallocate.zipWithIndex) {
    val isNonZero = de.valid && refCounter(de.bits) =/= 0.U
    val hasDuplicate = deallocate.take(i).map(de => de.valid && de.bits === deallocate(i).bits)
    val blockedByDup = if (i == 0) false.B else VecInit(hasDuplicate).asUInt.orR
    val isFreed = refCounter(de.bits) + refCounterInc(de.bits) === refCounterDec(de.bits)
    io.freeRegs(i).valid := RegNext(isNonZero && !blockedByDup) && RegNext(isFreed)
    val isFreed1 = refCounter(RegNext(de.bits)) === 0.U
    XSError(RegNext(isFreed) =/= isFreed1, p"why isFreed ${RegNext(isFreed)} $isFreed1\n")
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
  for (i <- 1 until size) {
    refCounterInc(i) := PopCount(allocate.zip(allocateOH).map(alloc => alloc._1.valid && alloc._2(i)))
    refCounterDec(i) := PopCount(deallocate.zip(deallocateOH).map(dealloc => dealloc._1.valid && dealloc._2(i)))
    val numAlloc1 = PopCount(allocate.map(alloc => alloc.valid && alloc.bits === i.U))
    val numDealloc1 = PopCount(deallocate.map(dealloc => dealloc.valid && dealloc.bits === i.U))
    XSError(refCounterInc(i) =/= numAlloc1, p"why numAlloc ${refCounterInc(i)} $numAlloc1??")
    XSError(refCounterDec(i) =/= numDealloc1, p"why numDealloc ${refCounterDec(i)} $numDealloc1??")
    refCounterNext(i) := refCounter(i) + refCounterInc(i) - refCounterDec(i)
    XSError(RegNext(refCounter(i) + refCounterInc(i) < refCounterDec(i)), p"why $i?\n")
    refCounter(i) := refCounterNext(i)
  }

  for (i <- 0 until RobSize) {
    val numCounters = PopCount(refCounter.map(_ === i.U))
    XSPerfAccumulate(s"ref_counter_$i", numCounters)
  }
  for (i <- 0 until size) {
    val isFreed = io.freeRegs.map(f => f.valid && f.bits === i.U)
    XSPerfAccumulate(s"free_reg_$i", VecInit(isFreed).asUInt.orR)
  }
}

