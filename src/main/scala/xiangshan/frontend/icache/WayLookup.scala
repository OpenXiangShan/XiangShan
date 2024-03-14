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

package xiangshan.frontend.icache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.tilelink._
import utils._
import xiangshan.cache.mmu._
import xiangshan.frontend._
import xiangshan.backend.fu.{PMPReqBundle, PMPRespBundle}
import huancun.PreferCacheKey
import xiangshan.XSCoreParamsKey
import utility._

class WayLookupRead(implicit p: Parameters) extends ICacheBundle {
  val waymask     = Vec(PortNumber, UInt(nWays.W))
  val ptag        = Vec(PortNumber, UInt(tagBits.W))
  val excp_tlb_af = Vec(PortNumber, Bool())
  val excp_tlb_pf = Vec(PortNumber, Bool())
  // debug
  val vSetIdx     = Vec(PortNumber, UInt(idxBits.W))
}

class WayLookupWrite(implicit p: Parameters) extends ICacheBundle {
  val vSetIdx       = Vec(PortNumber, UInt(idxBits.W))
  val ptag          = Vec(PortNumber, UInt(tagBits.W))
  val waymask       = Vec(PortNumber, UInt(nWays.W))
  val excp_tlb_af   = Vec(PortNumber, Bool())
  val excp_tlb_pf   = Vec(PortNumber, Bool())
}

class WayLookupInterface(implicit p: Parameters) extends ICacheBundle {
  val flush   = Input(Bool())
  val read    = DecoupledIO(new WayLookupRead)
  val write   = Flipped(DecoupledIO(new WayLookupWrite))
  val update  = Flipped(ValidIO(new ICacheMissResp))
}

class WayLookup(implicit p: Parameters) extends ICacheModule {
  val io = IO(new WayLookupInterface)

  // class WayLookupEntry(implicit p: Parameters) {
  //   val first_vSetIdx = RegInit(0.U(idxBits.W))
  //   val ptag          = RegInit(VecInit(Seq.fill(PortNumber)(0.U(tagBits.W))))
  //   val way           = RegInit(VecInit(Seq.fill(PortNumber)(0.U(log2Ceil(nWays).W))))
  //   val excp_tlb_af   = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  //   val excp_tlb_pf   = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))

  //   def vSetIdx       = VecInit(Seq(this.first_vSetIdx, this.first_vSetIdx + 1.U))
  //   def update(info: ICacheMissResp, valid: Bool): Bool = {
  //     val hits = Wire(Vec(PortNumber, Bool()))
  //     (0 until PortNumber).foreach { i => 
  //       val hits(i) = (info.vSetIdx === this.vSetIdx(i)) && (info.ptag === this.ptag(i)) && !info.corrupt && valid
  //       this.way(i) := Mux(hits(i), info.way, this.way(i))
  //     }
  //     hits.reduce(_||_)
  //   }
  // }

  class WayLookupPtr(implicit p: Parameters) extends CircularQueuePtr[WayLookupPtr](nWayLookupSize)

  object WayLookupPtr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): WayLookupPtr = {
      val ptr = Wire(new WayLookupPtr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  class WayLookupEntry(implicit p: Parameters) extends ICacheBundle {
    val vSetIdx       = Vec(PortNumber, UInt(idxBits.W))
    val ptag          = Vec(PortNumber, UInt(tagBits.W))
    val waymask       = Vec(PortNumber, UInt(nWays.W))
    val excp_tlb_af   = Vec(PortNumber, Bool())
    val excp_tlb_pf   = Vec(PortNumber, Bool())
  }

  val entries         = RegInit(VecInit(Seq.fill(nWayLookupSize)(0.U.asTypeOf((new WayLookupEntry).cloneType))))
  val readPtr         = RegInit(WayLookupPtr(false.B, 0.U))
  val writePtr        = RegInit(WayLookupPtr(false.B, 0.U))

  val empty = readPtr === writePtr
  val full  = (readPtr.value === writePtr.value) && (readPtr.flag ^ writePtr.flag)

  when(io.flush) {
    writePtr.value  := 0.U
    writePtr.flag   := false.B
  }.elsewhen(io.write.fire) {
    writePtr := writePtr + 1.U
  }

  when(io.flush) {
    readPtr.value  := 0.U
    readPtr.flag   := false.B
  }.elsewhen(io.read.fire) {
    readPtr := readPtr + 1.U
  }

  /**
    ******************************************************************************
    * update
    ******************************************************************************
    */
  val hits = Wire(Vec(nWayLookupSize, Bool()))
  entries.zip(hits).foreach{case(entry, hit) =>
    val hit_vec = Wire(Vec(PortNumber, Bool()))
    (0 until PortNumber).foreach { i =>
      val vset_same = (io.update.bits.vSetIdx === entry.vSetIdx(i)) && !io.update.bits.corrupt && io.update.valid
      val ptag_same = getPhyTagFromBlk(io.update.bits.blkPaddr) === entry.ptag(i)
      val way_same = io.update.bits.waymask === entry.waymask(i)
      when(vset_same) {
        when(ptag_same) {
          // miss -> hit
          entry.waymask(i) := io.update.bits.waymask
        }.elsewhen(way_same) {
          // data is overwritten: hit -> miss
          entry.waymask(i) := 0.U
        }
      }
      hit_vec(i) := vset_same && (ptag_same || way_same)
    }
    hit := hit_vec.reduce(_||_)
  }

  /**
    ******************************************************************************
    * read
    ******************************************************************************
    */
  // disalbe read when entries(0) update
  io.read.valid             := !empty && !hits(readPtr.value)
  io.read.bits.ptag         := entries(readPtr.value).ptag
  io.read.bits.waymask      := entries(readPtr.value).waymask
  io.read.bits.vSetIdx      := entries(readPtr.value).vSetIdx
  io.read.bits.excp_tlb_af  := entries(readPtr.value).excp_tlb_af
  io.read.bits.excp_tlb_pf  := entries(readPtr.value).excp_tlb_pf
  

  /**
    ******************************************************************************
    * write
    ******************************************************************************
    */
  io.write.ready := !full
  when(io.write.fire) {
    entries(writePtr.value).vSetIdx       := io.write.bits.vSetIdx
    entries(writePtr.value).ptag          := io.write.bits.ptag
    entries(writePtr.value).waymask       := io.write.bits.waymask
    entries(writePtr.value).excp_tlb_af   := io.write.bits.excp_tlb_af
    entries(writePtr.value).excp_tlb_pf   := io.write.bits.excp_tlb_pf
  }
}
