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
import utility.HasCircularQueuePtrHelper
import xiangshan.RedirectLevel
import xiangshan.XSBundle
import xiangshan.XSCoreParamsKey
import xiangshan.XSModule
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.BranchAttribute

class RasEntry()(implicit p: Parameters) extends RasBundle {
  val retAddr = PrunedAddr(VAddrBits)
  val ctr     = UInt(RasCtrSize.W) // layer of nested call functions
}

object RasEntry {
  def apply(retAddr: PrunedAddr, ctr: UInt)(implicit p: Parameters): RasEntry = {
    val e = Wire(new RasEntry)
    e.retAddr := retAddr
    e.ctr     := ctr
    e
  }
}

class RasPtr(implicit p: Parameters) extends CircularQueuePtr[RasPtr](p => p(XSCoreParamsKey).RasSpecSize) {}

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
  val ssp  = UInt(log2Up(RasSize).W)
  val sctr = UInt(RasCtrSize.W)
  val TOSW = new RasPtr
  val TOSR = new RasPtr
  val NOS  = new RasPtr
}

object RasInternalMeta {
  def apply(ssp: UInt, sctr: UInt, TOSW: RasPtr, TOSR: RasPtr, NOS: RasPtr)(implicit p: Parameters): RasInternalMeta = {
    val e = Wire(new RasInternalMeta)
    e.ssp  := ssp
    e.sctr := sctr
    e.TOSW := TOSW
    e.TOSR := TOSR
    e.NOS  := NOS
    e
  }
}

class RasMeta(implicit p: Parameters) extends RasBundle {
  val ssp  = UInt(log2Up(RasSize).W)
  val TOSW = new RasPtr
}

object RasMeta {
  def apply(ssp: UInt, TOSW: RasPtr)(implicit p: Parameters): RasMeta = {
    val e = Wire(new RasMeta)
    e.ssp  := ssp
    e.TOSW := TOSW
    e
  }
}

class CfiOffset(implicit p: Parameters) extends RasBundle {
  val carry:  Bool = Bool()
  val offset: UInt = UInt(log2Ceil(PredictWidth).W)
}

class RasDebug(implicit p: Parameters) extends RasBundle {
  val specQueue   = Output(Vec(RasSpecSize, new RasEntry))
  val specNOS     = Output(Vec(RasSpecSize, new RasPtr))
  val commitStack = Output(Vec(RasSize, new RasEntry))
  val BOS         = Output(new RasPtr)
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
  val isRvc:       Bool            = Bool()
  val target:      PrunedAddr      = PrunedAddr(VAddrBits)
}

class RasCommitInfo(implicit p: Parameters) extends RasBundle {
  val attribute:   BranchAttribute = new BranchAttribute
  val cfiPosition: UInt            = UInt(CfiPositionWidth.W)
  val startPc:     UInt            = UInt(VAddrBits.W)
  val isRvc:       Bool            = Bool()
  val meta:        RasMeta         = new RasMeta
}

class RasRedirectInfo(implicit p: Parameters) extends RasBundle {
  val attribute: BranchAttribute = new BranchAttribute
  val brPc:      PrunedAddr      = PrunedAddr(VAddrBits)
  val isRvc:     Bool            = Bool()
  val meta:      RasInternalMeta = new RasInternalMeta
  val level = RedirectLevel()
}

class RasIO(implicit p: Parameters) extends BasePredictorIO {
  val specIn:   Valid[RasSpecInfo]     = Flipped(Valid(new RasSpecInfo))
  val commit:   Valid[RasCommitInfo]   = Flipped(Valid(new RasCommitInfo))
  val redirect: Valid[RasRedirectInfo] = Flipped(Valid(new RasRedirectInfo))

  val rasOverride: Bool            = Output(Bool())
  val topRetAddr:  PrunedAddr      = Output(PrunedAddr(VAddrBits))
  val specMeta:    RasInternalMeta = Output(new RasInternalMeta)
}
