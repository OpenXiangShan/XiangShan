// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu.ras

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.CircularQueuePtr
import xiangshan.RedirectLevel
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BranchAttribute

class RasEntry(implicit p: Parameters) extends RasBundle {
  val retAddr: PrunedAddr = PrunedAddr(VAddrBits)
  val ctr:     UInt       = UInt(StackCounterWidth.W) // layer of nested call functions
}

object RasEntry {
  def apply(retAddr: PrunedAddr, ctr: UInt)(implicit p: Parameters): RasEntry = {
    val e = Wire(new RasEntry)
    e.retAddr := retAddr
    e.ctr     := ctr
    e
  }
}

class RasPtr(implicit p: Parameters) extends CircularQueuePtr[RasPtr](p =>
      p(XSCoreParamsKey).frontendParameters.bpuParameters.rasParameters.SpecQueueSize
    ) {}

object RasPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): RasPtr = {
    val ptr = Wire(new RasPtr)
    ptr.flag  := f
    ptr.value := v
    ptr
  }

  def inverse(ptr: RasPtr)(implicit p: Parameters): RasPtr = apply(!ptr.flag, ptr.value)
}

class RasInternalMeta(implicit p: Parameters) extends RasBundle {
  val ssp:  UInt   = UInt(log2Up(CommitStackSize).W)
  val sctr: UInt   = UInt(StackCounterWidth.W)
  val tosw: RasPtr = new RasPtr
  val tosr: RasPtr = new RasPtr
  val nos:  RasPtr = new RasPtr
}

object RasInternalMeta {
  def apply(ssp: UInt, sctr: UInt, tosw: RasPtr, tosr: RasPtr, nos: RasPtr)(implicit p: Parameters): RasInternalMeta = {
    val e = Wire(new RasInternalMeta)
    e.ssp  := ssp
    e.sctr := sctr
    e.tosw := tosw
    e.tosr := tosr
    e.nos  := nos
    e
  }
}

class RasMeta(implicit p: Parameters) extends RasBundle {
  val ssp:  UInt   = UInt(log2Up(CommitStackSize).W)
  val tosw: RasPtr = new RasPtr
}

object RasMeta {
  def apply(ssp: UInt, tosw: RasPtr)(implicit p: Parameters): RasMeta = {
    val e = Wire(new RasMeta)
    e.ssp  := ssp
    e.tosw := tosw
    e
  }
}

class RasDebug(implicit p: Parameters) extends RasBundle {
  val specQueue:   Vec[RasEntry] = Output(Vec(SpecQueueSize, new RasEntry))
  val specNos:     Vec[RasPtr]   = Output(Vec(SpecQueueSize, new RasPtr))
  val commitStack: Vec[RasEntry] = Output(Vec(CommitStackSize, new RasEntry))
  val bos:         RasPtr        = Output(new RasPtr)
}

class RasBypass(implicit p: Parameters) extends RasBundle {
  val taken:       Bool            = Bool()
  val cfiPosition: UInt            = UInt(CfiPositionWidth.W)
  val attribute:   BranchAttribute = new BranchAttribute
}

class RasSpecInfo(implicit p: Parameters) extends RasBundle {
  val attribute:   BranchAttribute = new BranchAttribute
  val cfiPosition: UInt            = UInt(CfiPositionWidth.W)
  val startPc:     UInt            = UInt(VAddrBits.W)
}

class RasCommitInfo(implicit p: Parameters) extends RasBundle {
  val attribute: BranchAttribute = new BranchAttribute
  val meta:      RasMeta         = new RasMeta
}

class RasRedirectInfo(implicit p: Parameters) extends RasBundle {
  val attribute: BranchAttribute = new BranchAttribute
  val cfiPc:     PrunedAddr      = PrunedAddr(VAddrBits)
  val meta:      RasInternalMeta = new RasInternalMeta
  val level:     UInt            = RedirectLevel()
}

class RASTrace(implicit p: Parameters) extends RasBundle {
  val redirectPushPc: UInt   = UInt(VAddrBits.W)
  val specPushPc:     UInt   = UInt(VAddrBits.W)
  val topRetAddr:     UInt   = UInt(VAddrBits.W)
  val specPush:       Bool   = Bool()
  val specPop:        Bool   = Bool()
  val normalRedirect: Bool   = Bool()
  val pushRedirect:   Bool   = Bool()
  val popRedirect:    Bool   = Bool()
  val commitPush:     Bool   = Bool()
  val tosw:           RasPtr = new RasPtr
  val tosr:           RasPtr = new RasPtr
  val bos:            RasPtr = new RasPtr
  val ssp:            UInt   = UInt(log2Up(CommitStackSize).W)
  val nsp:            UInt   = UInt(log2Up(CommitStackSize).W)
}
