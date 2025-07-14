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

class RasEntry()(implicit p: Parameters) extends XSBundle {
  val retAddr = PrunedAddr(VAddrBits)
  val ctr     = UInt(RasCtrSize.W) // layer of nested call functions
  def =/=(that: RasEntry): Bool = this.retAddr =/= that.retAddr || this.ctr =/= that.ctr
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

class RasInternalMeta(implicit p: Parameters) extends XSBundle {
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
    e.TOSW := TOSW
    e.TOSR := TOSR
    e.NOS  := NOS
    e
  }
}

class RasMeta(implicit p: Parameters) extends XSBundle {
  val ssp  = UInt(log2Up(RasSize).W)
  val TOSW = new RasPtr
}

object RasMeta {
  def apply(ssp: UInt, sctr: UInt, TOSW: RasPtr, TOSR: RasPtr, NOS: RasPtr)(implicit p: Parameters): RasMeta = {
    val e = Wire(new RasMeta)
    e.ssp  := ssp
    e.TOSW := TOSW
    e
  }
}

class RasDebug(implicit p: Parameters) extends XSBundle {
  val specQueue   = Output(Vec(RasSpecSize, new RasEntry))
  val specNOS     = Output(Vec(RasSpecSize, new RasPtr))
  val commitStack = Output(Vec(RasSize, new RasEntry))
  val BOS         = Output(new RasPtr)
}

class RasTopInput(implicit p: Parameters) extends XSBundle with HasPredictorCommonSignals {
  val redirect = Valid(new BranchPredictionRedirect)
  val fromFtb  = new FtbToRasBundle
}

class RasTopOutput(implicit p: Parameters) extends XSBundle {
  val predictionValid  = Bool()
  val s3_returnAddress = PrunedAddr(VAddrBits)
  val s3_rasSpecInfo   = new RasSpeculativeInfo
  val s3_meta          = new RasMeta
}