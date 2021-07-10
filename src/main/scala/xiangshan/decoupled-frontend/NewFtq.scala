/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.frontend.newftq

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.{AsyncDataModuleTemplate, CircularQueuePtr, DataModuleTemplate, HasCircularQueuePtrHelper, SRAMTemplate, SyncDataModuleTemplate, XSDebug, XSPerfAccumulate, XSError}
import xiangshan._
import scala.tools.nsc.doc.model.Val

class FtqPtr(implicit p: Parameters) extends CircularQueuePtr[FtqPtr](
  p => p(XSCoreParamsKey).FtqSize
){
  override def cloneType = (new FtqPtr).asInstanceOf[this.type]
}

trait HasFtqHelper { this: XSModule =>
  def GetPcByFtq(ftqPC: UInt, ftqOffset: UInt, hasLastPrev: Bool, lastPacketPC: UInt) = {
    assert(ftqPC.getWidth == VAddrBits)
    assert(lastPacketPC.getWidth == VAddrBits)
    assert(ftqOffset.getWidth == log2Up(PredictWidth))
    val idxBits = ftqPC.head(VAddrBits - ftqOffset.getWidth - instOffsetBits)
    val lastIdxBits = lastPacketPC.head(VAddrBits - ftqOffset.getWidth - instOffsetBits)
    val selLastPacket = hasLastPrev && (ftqOffset === 0.U)
    val packetIdx = Mux(selLastPacket, lastIdxBits, idxBits)
    Cat(
      packetIdx, // packet pc
      Mux(selLastPacket, Fill(ftqOffset.getWidth, 1.U(1.W)), ftqOffset),
      0.U(instOffsetBits.W)
    )
  }
}

class FtqEntry(implicit p: Parameters) extends XSBundle {
  val startAddr = UInt(VAddrBits.W)
  val fallThruAddr = UInt(VAddrBits.W)
  val target = UInt(VAddrBits.W)

  val valids = Vec(16, Bool())
  val isNextMask = Vec(16, Bool())
  val brOffset = Vec(1, UInt(4.W))
  val brMask = Vec(1, Bool())
  val jmpValid = Bool()
  val jmpType = UInt(2.W)

  val meta = UInt()
  val spec_meta = UInt()
}

class FtqRead(implicit val p: Parameters) extends Bundle {
  val ptr = Output(new FtqPtr)
  val entry = Input(new FtqEntry)
}

class FtqToBpuIO(implicit p: Parameters) extends XSBundle {
  val redirect = Valid(new BpuRedirectBundle)
  val update = Valid(new BpuUpdateBundle)
}

class FtqToIfuIO(implicit p: Parameters) extends XSBundle {
  val req = Decoupled(new FetchRequestBundle)
}

class FtqToBackendIO(implicit p: Parameters) extends XSBundle {
  val ftqRead = Vec(1 + 6 + 1 + 1 + 1, Flipped(new FtqRead))
}

class NewFtq(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val fromBpu = Flipped(new BpuToFtqIO)
    val fromIfu = Flipped(new IfuToFtqIO)
    val fromBackend = Flipped(new BackendToFtqIO)
    
    val toBpu = new FtqToBpuIO
    val toIfu = new FtqToIfuIO
    val toBackend = new FtqToBackendIO
  })

  val flush = WireInit(false.B)
  val real_fire = fromBPU.req.fire() && !flush

  val bpuPtr, ifuPtr, commPtr = RegInit(FtqPtr(false.B, 0.U))
  val validEntries = distanceBetween(bpuPtr, commPtr)

  // TODO: remove this
  val numRead = 16
  // TODO: rewrite with SRAM
  val mem = Module(new SyncDataModuleTemplate(new FtqEntry, FtqSize, numRead, 1))
  mem.io.wen(0) := real_fire
  mem.io.waddr(0) := bpuPtr.value
  mem.io.wdata(0).startAddr    := io.fromBpu.req.bits.pc
  mem.io.wdata(0).fallThruAddr := io.fromBpu.req.bits.ftb_entry.pft_addr
  mem.io.wdata(0).target       := io.fromBpu.req.bits.preds.pred_target.bits
  // Leave ifu to writeback
  mem.io.wdata(0).valids := DontCare
  mem.io.wdata(0).isNextMask := DontCare

  mem.io.wdata(0).brOffset := io.fromBpu.req.bits.ftb_entry.br_offset
  mem.io.wdata(0).brMask   := io.fromBpu.req.bits.ftb_entry.br_valids
  mem.io.wdata(0).jmpValid := io.fromBpu.req.bits.ftb_entry.jmp_valid
  mem.io.wdata(0).jmpType  := GetJmpType(io.fromBpu.req.bits.ftb_entry) // TODO: implement this function

  mem.io.wdata(0).meta      := io.fromBpu.req.bits.meta
  mem.io.wdata(0).spec_meta := io.fromBpu.req.bits.spec_meta

  for (i <- 0 until numRead) {
    mem.io.wen
  }
}
