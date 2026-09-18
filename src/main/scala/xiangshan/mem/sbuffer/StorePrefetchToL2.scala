/***************************************************************************************
* Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import xiangshan._
import xiangshan.cache._

/** Store prefetch request which is forwarded from L1 to L2.
  *
  * The address is the cache block address of the committed store, the mask is the
  * byte mask of the whole cache block (bit i means byte i of the block is written).
  */
class StorePrefetchToL2Req(implicit p: Parameters) extends XSBundle with HasDCacheParameters {
  val addr = UInt(PAddrBits.W)
  val mask = UInt(dcacheParameters.blockBytes.W)
}

/** Generate store prefetch requests when a store is committed into the sbuffer and
  * buffer them in a small queue.
  *
  * The L1 side only forwards these requests to L2 (through the L2 prefetch channel).
  * The merge (a mini store buffer for prefetch) is implemented in L2.
  *
  * Requests of the following stores are not forwarded:
  *  - hardware prefetch stores (they would create a feedback loop)
  *  - cbo.zero (whole line write, no need to prefetch the old data)
  *  - invalid vector elements
  * Uncacheable (NC/MMIO) stores never reach the sbuffer, so they are excluded already.
  */
class StorePrefetchToL2Gen(implicit p: Parameters) extends DCacheModule with HasStorePrefetchHelper {
  val SIZE = 16 // temporary queue, do not lose store events when L2 is busy

  class StoreEnq extends Bundle {
    val valid = Bool()
    val addr = UInt(PAddrBits.W)
    val mask = UInt((VLEN / 8).W)
    val wline = Bool() // cbo.zero
    val prefetch = Bool() // hardware prefetch store
    val vecValid = Bool()
  }

  val io = IO(new Bundle {
    val enq = Flipped(Vec(EnsbufferWidth, new StoreEnq))
    val out = DecoupledIO(new StorePrefetchToL2Req)
  })

  val lineBytes = dcacheParameters.blockBytes
  val storeBytes = VLEN / 8
  val blockOffsetBits = log2Up(lineBytes)
  val storeOffsetBits = log2Up(storeBytes)

  val idxW = log2Up(SIZE)
  val cntW = log2Up(SIZE + 1)

  val entries = Reg(Vec(SIZE, new StorePrefetchToL2Req))
  val valids = RegInit(VecInit(Seq.fill(SIZE)(false.B)))
  val enqPtr = RegInit(0.U(idxW.W))
  val deqPtr = RegInit(0.U(idxW.W))
  val count = RegInit(0.U(cntW.W))

  // generate request: block address + 64B line mask
  val reqValid = Wire(Vec(EnsbufferWidth, Bool()))
  val reqBits = Wire(Vec(EnsbufferWidth, new StorePrefetchToL2Req))
  for (i <- 0 until EnsbufferWidth) {
    val enq = io.enq(i)
    val blockAddr = enq.addr(enq.addr.getWidth - 1, blockOffsetBits)
    val wordIdx = enq.addr(blockOffsetBits - 1, storeOffsetBits)
    val lineMask = (enq.mask << Cat(wordIdx, 0.U(storeOffsetBits.W)))(lineBytes - 1, 0)
    reqValid(i) := enq.valid && enq.vecValid && !enq.prefetch && !enq.wline
    reqBits(i).addr := Cat(blockAddr, 0.U(blockOffsetBits.W))
    reqBits(i).mask := lineMask
  }

  val deqFire = io.out.valid && io.out.ready
  val freeCount = SIZE.U - count + Mux(deqFire, 1.U, 0.U)
  val enqOk = Wire(Vec(EnsbufferWidth, Bool()))
  for (i <- 0 until EnsbufferWidth) {
    enqOk(i) := reqValid(i) && freeCount > i.U
  }
  val enqNum = PopCount(enqOk)

  when(deqFire) {
    valids(deqPtr) := false.B
  }
  for (i <- 0 until EnsbufferWidth) {
    when(enqOk(i)) {
      val pos = (enqPtr + i.U)(idxW - 1, 0)
      entries(pos) := reqBits(i)
      valids(pos) := true.B
    }
  }

  when(deqFire) {
    deqPtr := Mux(deqPtr === (SIZE - 1).U, 0.U, deqPtr + 1.U)
  }
  when(enqNum.orR) {
    enqPtr := enqPtr + enqNum
  }
  count := count + enqNum - Mux(deqFire, 1.U, 0.U)

  io.out.valid := valids(deqPtr)
  io.out.bits := entries(deqPtr)

  XSPerfAccumulate("store_pf_to_l2_enq_valid", PopCount(reqValid))
  XSPerfAccumulate("store_pf_to_l2_enq_fire", enqNum)
  XSPerfAccumulate("store_pf_to_l2_deq", io.out.fire)
  XSPerfAccumulate("store_pf_to_l2_full", count === SIZE.U)
  XSPerfAccumulate("store_pf_to_l2_drop", PopCount(reqValid) > enqNum)
  XSPerfHistogram("store_pf_to_l2_occupancy", count, true.B, 0, SIZE, 1)
}
