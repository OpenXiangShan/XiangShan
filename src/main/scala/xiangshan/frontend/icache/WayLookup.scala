/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan.cache.mmu.Pbmt
import xiangshan.frontend.ExceptionType

/* WayLookupEntry is for internal storage, while WayLookupInfo is for interface
 * Notes:
 *   1. there must be a flush (caused by guest page fault) after excp_tlb_gpf === true.B,
 *      so, we need only the first excp_tlb_gpf and the corresponding gpaddr.
 *      to save area, we separate those signals from WayLookupEntry and store only once.
 */
class WayLookupEntry(implicit p: Parameters) extends ICacheBundle {
  val vSetIdx:        Vec[UInt] = Vec(PortNumber, UInt(idxBits.W))
  val waymask:        Vec[UInt] = Vec(PortNumber, UInt(nWays.W))
  val ptag:           Vec[UInt] = Vec(PortNumber, UInt(tagBits.W))
  val itlb_exception: Vec[UInt] = Vec(PortNumber, UInt(ExceptionType.width.W))
  val itlb_pbmt:      Vec[UInt] = Vec(PortNumber, UInt(Pbmt.width.W))
  val meta_codes:     Vec[UInt] = Vec(PortNumber, UInt(ICacheMetaCodeBits.W))
}

class WayLookupGPFEntry(implicit p: Parameters) extends ICacheBundle {
  // NOTE: we don't use GPAddrBits here, refer to ICacheMainPipe.scala L43-48 and PR#3795
  val gpaddr:            UInt = UInt(PAddrBitsMax.W)
  val isForVSnonLeafPTE: Bool = Bool()
}

class WayLookupInfo(implicit p: Parameters) extends ICacheBundle {
  val entry = new WayLookupEntry
  val gpf   = new WayLookupGPFEntry

  // for compatibility
  def vSetIdx:           Vec[UInt] = entry.vSetIdx
  def waymask:           Vec[UInt] = entry.waymask
  def ptag:              Vec[UInt] = entry.ptag
  def itlb_exception:    Vec[UInt] = entry.itlb_exception
  def itlb_pbmt:         Vec[UInt] = entry.itlb_pbmt
  def meta_codes:        Vec[UInt] = entry.meta_codes
  def gpaddr:            UInt      = gpf.gpaddr
  def isForVSnonLeafPTE: Bool      = gpf.isForVSnonLeafPTE
}

class WayLookupInterface(implicit p: Parameters) extends ICacheBundle {
  val flush:  Bool                       = Input(Bool())
  val read:   DecoupledIO[WayLookupInfo] = DecoupledIO(new WayLookupInfo)
  val write:  DecoupledIO[WayLookupInfo] = Flipped(DecoupledIO(new WayLookupInfo))
  val update: Valid[ICacheMissResp]      = Flipped(ValidIO(new ICacheMissResp))
}

class WayLookup(implicit p: Parameters) extends ICacheModule with HasICacheECCHelper {
  val io: WayLookupInterface = IO(new WayLookupInterface)

  class WayLookupPtr extends CircularQueuePtr[WayLookupPtr](nWayLookupSize)
  private object WayLookupPtr {
    def apply(f: Bool, v: UInt): WayLookupPtr = {
      val ptr = Wire(new WayLookupPtr)
      ptr.flag  := f
      ptr.value := v
      ptr
    }
  }

  private val entries  = RegInit(VecInit(Seq.fill(nWayLookupSize)(0.U.asTypeOf(new WayLookupEntry))))
  private val readPtr  = RegInit(WayLookupPtr(false.B, 0.U))
  private val writePtr = RegInit(WayLookupPtr(false.B, 0.U))

  private val empty = readPtr === writePtr
  private val full  = (readPtr.value === writePtr.value) && (readPtr.flag ^ writePtr.flag)

  when(io.flush) {
    writePtr.value := 0.U
    writePtr.flag  := false.B
  }.elsewhen(io.write.fire) {
    writePtr := writePtr + 1.U
  }

  when(io.flush) {
    readPtr.value := 0.U
    readPtr.flag  := false.B
  }.elsewhen(io.read.fire) {
    readPtr := readPtr + 1.U
  }

  private val gpf_entry = RegInit(0.U.asTypeOf(Valid(new WayLookupGPFEntry)))
  private val gpfPtr    = RegInit(WayLookupPtr(false.B, 0.U))
  private val gpf_hit   = gpfPtr === readPtr && gpf_entry.valid

  when(io.flush) {
    // we don't need to reset gpfPtr, since the valid is actually gpf_entries.excp_tlb_gpf
    gpf_entry.valid := false.B
    gpf_entry.bits  := 0.U.asTypeOf(new WayLookupGPFEntry)
  }

  /**
    ******************************************************************************
    * update
    ******************************************************************************
    */
  private val hits = Wire(Vec(nWayLookupSize, Bool()))
  entries.zip(hits).foreach { case (entry, hit) =>
    val hit_vec = Wire(Vec(PortNumber, Bool()))
    (0 until PortNumber).foreach { i =>
      val vset_same = (io.update.bits.vSetIdx === entry.vSetIdx(i)) && !io.update.bits.corrupt && io.update.valid
      val ptag_same = getPhyTagFromBlk(io.update.bits.blkPaddr) === entry.ptag(i)
      val way_same  = io.update.bits.waymask === entry.waymask(i)
      when(vset_same) {
        when(ptag_same) {
          // miss -> hit
          entry.waymask(i) := io.update.bits.waymask
          // also update meta_codes
          // NOTE: we have getPhyTagFromBlk(io.update.bits.blkPaddr) === entry.ptag(i),
          //       so we can use entry.ptag(i) for better timing
          entry.meta_codes(i) := encodeMetaECC(entry.ptag(i))
        }.elsewhen(way_same) {
          // data is overwritten: hit -> miss
          entry.waymask(i) := 0.U
          // don't care meta_codes, since it's not used for a missed request
        }
      }
      hit_vec(i) := vset_same && (ptag_same || way_same)
    }
    hit := hit_vec.reduce(_ || _)
  }

  /**
    ******************************************************************************
    * read
    ******************************************************************************
    */
  // if the entry is empty, but there is a valid write, we can bypass it to read port (maybe timing critical)
  private val can_bypass = empty && io.write.valid
  io.read.valid := !empty || io.write.valid
  when(can_bypass) {
    io.read.bits := io.write.bits
  }.otherwise { // can't bypass
    io.read.bits.entry := entries(readPtr.value)
    when(gpf_hit) { // ptr match && entry valid
      io.read.bits.gpf := gpf_entry.bits
      // also clear gpf_entry.valid when it's read, note this will be overridden by write (L175)
      when(io.read.fire) {
        gpf_entry.valid := false.B
      }
    }.otherwise { // gpf not hit
      io.read.bits.gpf := 0.U.asTypeOf(new WayLookupGPFEntry)
    }
  }

  /**
    ******************************************************************************
    * write
    ******************************************************************************
    */
  // if there is a valid gpf to be read, we should stall write
  private val gpf_stall = gpf_entry.valid && !(io.read.fire && gpf_hit)
  io.write.ready := !full && !gpf_stall
  when(io.write.fire) {
    entries(writePtr.value) := io.write.bits.entry
    when(io.write.bits.itlb_exception.map(_ === ExceptionType.gpf).reduce(_ || _)) {
      // if gpf_entry is bypassed, we don't need to save it
      // note this will override the read (L156)
      gpf_entry.valid := !(can_bypass && io.read.fire)
      gpf_entry.bits  := io.write.bits.gpf
      gpfPtr          := writePtr
    }
  }
}
