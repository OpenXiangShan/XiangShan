/***************************************************************************************
 * Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 ***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientMetadata
import org.chipsalliance.cde.config.Parameters
import xiangshan.L1CacheErrorInfo

object PBState extends ChiselEnum {
  val invalid, reserved, resident, poison, released, probeLocked, moveLocked = Value
}

class PBMeta(implicit p: Parameters) extends DCacheBundle {
  val state = PBState()
  val paddr = UInt(PAddrBits.W)
  val missEntryId = UInt(log2Up(cfg.nMissEntries).W)
  val vaddr = UInt(VAddrBits.W)
  val coh = new ClientMetadata
  val prefetchSource = UInt(L1PfSourceBits.W)
  val used = Bool()
  val movePending = Bool()
  val dataBad = Bool()
}

class PBAlloc(implicit p: Parameters) extends DCacheBundle {
  val missEntryId = UInt(log2Up(cfg.nMissEntries).W)
  val addr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val prefetchSource = UInt(L1PfSourceBits.W)
}

class PBLoadResp(implicit p: Parameters) extends DCacheBundle {
  val data = UInt(VLEN.W)
  val prefetchSource = UInt(L1PfSourceBits.W)
  val hit = Bool()
  val retry = Bool()
}

class PBRefillReq(implicit p: Parameters) extends DCacheBundle {
  val entryId = UInt(PBIdBits.W)
  val missEntryId = UInt(log2Up(cfg.nMissEntries).W)
  val data = UInt((cfg.blockBytes * 8).W)
  val coh = new ClientMetadata
  val denied = Bool()
  val corrupt = Bool()
}

class PBMoveReq(implicit p: Parameters) extends DCacheBundle {
  val entryId = UInt(PBIdBits.W)
  val paddr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
}

class PBProbeResp(implicit p: Parameters) extends DCacheBundle {
  val locked = Bool()
  val coh = new ClientMetadata
  val entryId = UInt(PBIdBits.W)
}

class PBPipeDataResp(implicit p: Parameters) extends DCacheBundle {
  val retry = Bool()
  val entryId = UInt(PBIdBits.W)
  val data = UInt((cfg.blockBytes * 8).W)
  val coh = new ClientMetadata
  val prefetchSource = UInt(L1PfSourceBits.W)
  val used = Bool()
  val dataBad = Bool()
}

class PBMoveAbort(implicit p: Parameters) extends DCacheBundle {
  val entryId = UInt(PBIdBits.W)
  val dataBad = Bool()
}

class PBLoadIO(implicit p: Parameters) extends DCacheBundle {
  val s1_paddr = Flipped(ValidIO(UInt(PAddrBits.W)))
  val s1_kill = Input(Bool())
  val s1_hit = Output(Bool())
  val s1_retry = Output(Bool())
  val s2_dataResp = Output(ValidIO(new PBLoadResp))
  val s2_use = Input(Bool())
}

class PBMSHRIO(implicit p: Parameters) extends DCacheBundle {
  val allocReq = Flipped(DecoupledIO(new PBAlloc))
  val allocEntryId = Output(UInt(PBIdBits.W))
  val cancelReq = Input(Vec(cfg.nMissEntries, ValidIO(UInt(PBIdBits.W))))
  val refillReq = Flipped(DecoupledIO(new PBRefillReq))
  val status = Output(Vec(PBEntries, ValidIO(UInt(PAddrBits.W))))
  val preAcquire = Input(Vec(cfg.nMissEntries, Bool()))
}

class PBPipeIO(implicit p: Parameters) extends DCacheBundle {
  val s0_probeReq = Flipped(ValidIO(UInt(PAddrBits.W)))
  val s0_moveReq = DecoupledIO(new PBMoveReq)

  val s1_hit = Output(Bool())
  val s1_paddr = Flipped(DecoupledIO(UInt(PAddrBits.W)))
  val s1_alias = Input(UInt(PBAliasBits.W))
  val s1_storeReq = Input(Bool())
  val s1_probeResp = Output(ValidIO(new PBProbeResp))

  val s2_storeResp = Output(ValidIO(Bool()))
  val s2_dataResp = DecoupledIO(new PBPipeDataResp)
  val s2_moveAbort = Flipped(ValidIO(new PBMoveAbort))

  val s3_probeDone = Flipped(ValidIO(UInt(PBIdBits.W)))
  val s3_moveDone = Flipped(ValidIO(UInt(PBIdBits.W)))
}

class PBPowerIO(implicit p: Parameters) extends DCacheBundle {
  val wfiReq = Input(Bool())
  val safe = Output(Bool())
}

class PBErrorIO(implicit p: Parameters) extends DCacheBundle {
  val report = Output(Valid(new L1CacheErrorInfo))
  val fatal = Output(Bool())
}

class PBPerfIO(implicit p: Parameters) extends DCacheBundle {
  val firstUse = Output(Vec(PBEntries, Valid(UInt(L1PfSourceBits.W))))
  val unusedExit = Output(Valid(UInt(L1PfSourceBits.W)))
}

class PBDCacheIO(implicit p: Parameters) extends DCacheBundle {
  val wfi = new PBPowerIO
  val error = new PBErrorIO
  val perf = new PBPerfIO
}
