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
import utility._

/* WayLookupEntry is for internal storage, while WayLookupInfo is for interface
 * Notes:
 *   1. there must be a flush (caused by guest page fault) after excp_tlb_gpf === true.B,
 *      so, we need only the first excp_tlb_gpf and the corresponding gpaddr.
 *      to save area, we separate those signals from WayLookupEntry and store only once.
 */
class WayLookupEntry(implicit p: Parameters) extends ICacheBundle {
  val vSetIdx      : Vec[UInt] = Vec(PortNumber, UInt(idxBits.W))
  val waymask      : Vec[UInt] = Vec(PortNumber, UInt(nWays.W))
  val ptag         : Vec[UInt] = Vec(PortNumber, UInt(tagBits.W))
  val excp_tlb_af  : Vec[Bool] = Vec(PortNumber, Bool())
  val excp_tlb_pf  : Vec[Bool] = Vec(PortNumber, Bool())
  val meta_errors  : Vec[Bool] = Vec(PortNumber, Bool())
}

class WayLookupGPFEntry(implicit p: Parameters) extends ICacheBundle {
  val excp_tlb_gpf : Vec[Bool] = Vec(PortNumber, Bool())
  val gpaddr       : UInt      = UInt(GPAddrBits.W)

  def hasGPF: Bool = excp_tlb_gpf.asUInt.orR
}

class WayLookupInfo(implicit p: Parameters) extends ICacheBundle {
  val entry = new WayLookupEntry
  val gpf   = new WayLookupGPFEntry

  // for compatibility
  def vSetIdx      : Vec[UInt] = entry.vSetIdx
  def waymask      : Vec[UInt] = entry.waymask
  def ptag         : Vec[UInt] = entry.ptag
  def excp_tlb_af  : Vec[Bool] = entry.excp_tlb_af
  def excp_tlb_pf  : Vec[Bool] = entry.excp_tlb_pf
  def meta_errors  : Vec[Bool] = entry.meta_errors
  def excp_tlb_gpf : Vec[Bool] = gpf.excp_tlb_gpf
  def gpaddr       : UInt      = gpf.gpaddr
}


// class WayLookupRead(implicit p: Parameters) extends ICacheBundle {
//   val vSetIdx     = Vec(PortNumber, UInt(idxBits.W))
//   val waymask     = Vec(PortNumber, UInt(nWays.W))
//   val ptag        = Vec(PortNumber, UInt(tagBits.W))
//   val excp_tlb_af = Vec(PortNumber, Bool())
//   val excp_tlb_pf = Vec(PortNumber, Bool())
// }

// class WayLookupWrite(implicit p: Parameters) extends ICacheBundle {
//   val vSetIdx       = Vec(PortNumber, UInt(idxBits.W))
//   val ptag          = Vec(PortNumber, UInt(tagBits.W))
//   val waymask       = Vec(PortNumber, UInt(nWays.W))
//   val excp_tlb_af   = Vec(PortNumber, Bool())
//   val excp_tlb_pf   = Vec(PortNumber, Bool())
// }

class WayLookupInterface(implicit p: Parameters) extends ICacheBundle {
  val flush   = Input(Bool())
  val read    = DecoupledIO(new WayLookupInfo)
  val write   = Flipped(DecoupledIO(new WayLookupInfo))
  val update  = Flipped(ValidIO(new ICacheMissResp))
}

class WayLookup(implicit p: Parameters) extends ICacheModule {
  val io: WayLookupInterface = IO(new WayLookupInterface)

  class WayLookupPtr(implicit p: Parameters) extends CircularQueuePtr[WayLookupPtr](nWayLookupSize)
  private object WayLookupPtr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): WayLookupPtr = {
      val ptr = Wire(new WayLookupPtr)
      ptr.flag := f
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

  private val gpf_entry = RegInit(0.U.asTypeOf(new WayLookupGPFEntry))
  private val gpfPtr    = RegInit(WayLookupPtr(false.B, 0.U))

  when(io.flush) {
    // we don't need to reset gpfPtr, since the valid is actually gpf_entries.excp_tlb_gpf
    gpf_entry := 0.U.asTypeOf(new WayLookupGPFEntry)
  }

  /**
    ******************************************************************************
    * update
    ******************************************************************************
    */
  private val hits = Wire(Vec(nWayLookupSize, Bool()))
  entries.zip(hits).foreach{ case(entry, hit) =>
    val hit_vec = Wire(Vec(PortNumber, Bool()))
    (0 until PortNumber).foreach { i =>
      val vset_same = (io.update.bits.vSetIdx === entry.vSetIdx(i)) && !io.update.bits.corrupt && io.update.valid
      val ptag_same = getPhyTagFromBlk(io.update.bits.blkPaddr) === entry.ptag(i)
      val way_same = io.update.bits.waymask === entry.waymask(i)
      when(vset_same) {
        when(ptag_same) {
          // miss -> hit
          entry.waymask(i) := io.update.bits.waymask
          // also clear previously found errors since data/metaArray is refilled
          entry.meta_errors(i) := false.B
        }.elsewhen(way_same) {
          // data is overwritten: hit -> miss
          entry.waymask(i) := 0.U
          // do not clear previously found errors since way_same might be unreliable when error occurs
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
  io.read.valid := !empty || io.write.valid
  when (empty && io.write.valid) {  // bypass
    io.read.bits := io.write.bits
  }.otherwise {
    io.read.bits.entry := entries(readPtr.value)
    io.read.bits.gpf   := Mux(readPtr === gpfPtr, gpf_entry, 0.U.asTypeOf(new WayLookupGPFEntry))
  }

  /**
    ******************************************************************************
    * write
    ******************************************************************************
    */
  io.write.ready := !full
  when(io.write.fire) {
    entries(writePtr.value) := io.write.bits.entry
    // save gpf iff no gpf is already saved
    when(!gpf_entry.hasGPF && io.write.bits.gpf.hasGPF) {
      gpf_entry := io.write.bits.gpf
      gpfPtr := writePtr
    }
  }
}
