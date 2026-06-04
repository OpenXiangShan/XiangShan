/***************************************************************************************
* Copyright (c) 2021-2026 Beijing Institute of Open Source Chip (BOSC)
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http: // license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/
package xiangshan.cache.mmu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import utility._
import xscache.coupledL2.utils.SplittedSRAM
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import utility.mbist.MbistPipeline
import xiangshan.backend.fu.{PMPReqBundle, PMPRespBundle}

class MptReqBundle(implicit p: Parameters) extends XSBundle with MPTCacheParam {
  // mpt io interface req and resp, id is not used in ptw
  val reqPA   = UInt(ppnLen.W)
  val id      = UInt(mptSourceWidth.W)
  val mptOnly = Bool() // 1bit control logic for L2TLB
}

class MptTlbRespBundle(implicit p: Parameters) extends XSBundle with MPTCacheParam {
  // L2TB return info to L1
  val mptOnly       = Bool()                  // 1bit control logic for L2TLB
  val accessFault   = Bool()
  val mptPerm       = UInt(3.W)
  val mptLevel      = UInt(mptLevelLenUInt.W) // UInt level
  val contigousPerm = Bool()                  // only work for non H l0 pte
  // indicate continous 8 permission. can not compress as l0pte(8bit valididx)
  val permIsNAPOT = Bool()
  def genFakeResp(): Unit = {
    this.accessFault   := false.B
    this.mptPerm       := Fill(3, 1.U(1.W))
    this.contigousPerm := true.B
    this.mptLevel      := 3.U
    this.permIsNAPOT   := true.B
  }
  def applyMptc2TlbResp(childBundle: MptRespBundle): Unit = {
    this.accessFault   := childBundle.accessFault
    this.mptPerm       := childBundle.mptPerm
    this.contigousPerm := childBundle.contigousPerm
    this.mptLevel      := childBundle.mptLevel
    this.permIsNAPOT   := childBundle.permIsNAPOT
    this.mptOnly       := childBundle.mptOnly
  }
}
class MptRespBundle(implicit p: Parameters) extends MptTlbRespBundle with MPTCacheParam { // mptc
  val id    = UInt(mptSourceWidth.W)
  val reqPA = UInt(ppnLen.W) // in req to out req
}

class MptOutputSwitchBoxIO(implicit p: Parameters) extends MMUIOBaseBundle with MPTCacheParam {
  val mergeArb = Flipped(DecoupledIO(new Bundle() {
    val fault = Input(Bool())
  }))
  val mptIn = DecoupledIO()
  val mptOut = Flipped(ValidIO(new MptRespBundle()))
  val l1TLB = DecoupledIO(new Bundle() {
    val outData    = new MptTlbRespBundle()
    val outMptOnly = Bool()
    val reqPA      = UInt(ppnLen.W)
  })
}
class MptOutputSwitchBox(implicit p: Parameters) extends XSModule with MPTCacheParam {
  // All L2TLB address translations must be combined with the MPT response before returning.
  // Therefore, a small state machine is required to demultiplex control signals.
  // Additionally, the MPT output data needs to be stored in a register.

  val io = IO(new MptOutputSwitchBoxIO())

  val mptOutDataWire = Wire(new MptTlbRespBundle())
  mptOutDataWire.applyMptc2TlbResp(io.mptOut.bits)

  val mptOutDataReg = Reg(new MptTlbRespBundle())
  val mptOutMptOnly = RegInit(false.B)
  val mptOutReqPA   = Reg(UInt(ppnLen.W))

  val flush = io.sfence.valid || io.csr.satp.changed || io.csr.vsatp.changed || io.csr.hgatp.changed ||
    io.csr.priv.virt_changed || (if (HasMptCheck) io.csr.mmpt.changed else false.B)

  when(io.mptOut.valid) {
    mptOutDataReg := mptOutDataWire
    mptOutMptOnly := io.mptOut.bits.mptOnly
    mptOutReqPA   := io.mptOut.bits.reqPA
  }
  when(io.mergeArb.bits.fault && io.mergeArb.valid) {
    mptOutMptOnly := false.B
  }

  io.l1TLB.bits.outData := Mux(io.mptOut.valid, mptOutDataWire, mptOutDataReg)
  io.l1TLB.bits.outMptOnly := Mux(io.mptOut.valid, io.mptOut.bits.mptOnly, mptOutMptOnly) &&
    !(io.mergeArb.valid && io.mergeArb.bits.fault)
  io.l1TLB.bits.reqPA := Mux(io.mptOut.valid, io.mptOut.bits.reqPA, mptOutReqPA)

  object MptSwitchState extends ChiselEnum {
    val s_idle, s_send_mpt, s_send_l1_tlb = Value
  }
  import MptSwitchState._
  val curState  = RegInit(s_idle)
  val nextState = WireDefault(s_idle)
  when(flush) {
    curState := s_idle
  }.otherwise {
    curState := nextState // 2 proc FSM
  }
  // fsm start
  io.mergeArb.ready := false.B
  io.mptIn.valid    := false.B
  io.l1TLB.valid    := false.B
  nextState         := curState
  switch(curState) {
    is(s_idle) {
      when(io.mptOut.valid && io.mptOut.bits.mptOnly) { //mptonly mode
        // mpt valid without merge arb first implies that it is mpt only request.try change later
        io.l1TLB.valid := true.B
        when(io.l1TLB.ready) {
          io.mergeArb.ready := true.B
          nextState         := s_idle
        }.otherwise {
          nextState := s_send_l1_tlb
        }
      }.elsewhen(io.mergeArb.valid) { // normal mode, two modes are mutually exclusive
        when(io.mergeArb.bits.fault) {
          io.l1TLB.valid := true.B
          when(io.l1TLB.ready) {
            io.mergeArb.ready := true.B
            nextState         := s_idle
          }.otherwise {
            nextState := s_send_l1_tlb
          }
        }.otherwise {
          io.mptIn.valid := true.B
          when(io.mptIn.ready) {
            nextState := s_send_mpt
          }
        }
      }
    }

    is(s_send_mpt) { // delay+1+mptclk
      io.mptIn.valid := false.B
      when(io.mptOut.valid) {
        io.l1TLB.valid := true.B
        when(io.l1TLB.ready) {
          io.mergeArb.ready := true.B
          nextState         := s_idle
        }.otherwise {
          nextState := s_send_l1_tlb
        }
      }
    }

    is(s_send_l1_tlb) {
      io.l1TLB.valid := true.B
      when(io.l1TLB.ready) {
        io.mergeArb.ready := true.B
        nextState         := s_idle
      }
    }
  }
  // fsm end
}

class PLRUOH(logWays: Int, isTop: Boolean = true) extends Module {
  val wayNum = 1 << logWays
  val io = IO(new Bundle {
    val access   = Flipped(ValidIO(UInt(wayNum.W)))
    val replace  = Output(UInt(wayNum.W))
    val upperCom = Option.when(!isTop)(Output(Bool()))
  })
  if (logWays == 0) {
    io.replace := 1.U // OH 1 is b0

  } else if (logWays == 1) { // delay 1 gate
    val changed = io.access.bits(1) || io.access.bits(0)
    // 01 will let state points to right 10 to left entry, 00 will disable state input,if input freezes,
    // i.e. same access value with valid for more than 1 clk, the state will not change, great
    if (!isTop) { io.upperCom.get := changed }
    val state = RegEnable(io.access.bits(0), false.B, io.access.valid && changed)
    // OH last bit indicates the next state 01 state 1, 10 state 0
    io.replace := Cat(state, ~state) // replace state 1 : 10, state 0 01 opposite of the direction iof input
  } else {
    val top      = wayNum
    val mid      = 1 << (logWays - 1)
    val plruleft = Module(new PLRUOH(logWays - 1, false)) // gen left and right entry
    plruleft.io.access.bits  := io.access.bits(top - 1, mid)
    plruleft.io.access.valid := io.access.valid

    val plruright = Module(new PLRUOH(logWays - 1, false))
    plruright.io.access.bits  := io.access.bits(mid - 1, 0)
    plruright.io.access.valid := io.access.valid

    val changed = plruleft.io.upperCom.get || plruright.io.upperCom.get
    val state =
      RegEnable(
        plruright.io.upperCom.get,
        false.B,
        io.access.valid && changed
      ) // OH last bit indicates the next state 01 state 1, 10 state 0
    if (!isTop) { io.upperCom.get := changed }
    val leftreplace  = Fill(plruleft.wayNum, state) & plruleft.io.replace
    val rightreplace = Fill(plruleft.wayNum, !state) & plruright.io.replace // replace state 1 : 10, state 0 01
    io.replace := Cat(leftreplace, rightreplace)
  }
}
class PLRUOHSet(setsLog2: Int, logWays: Int) extends Module {
  val wayNum = 1 << logWays
  val setNum = 1 << setsLog2
  val io = IO(new Bundle {
    val access  = Flipped(ValidIO(UInt(wayNum.W)))
    val replace = Output(UInt(wayNum.W))
    val idx     = Input(UInt(setsLog2.W))
  })

  val plruSet     = Array.fill(setNum)(Module(new PLRUOH(logWays)).io)
  val outputArray = Wire(Vec(setNum, UInt(wayNum.W)))
  val hitArray    = Wire(Vec(setNum, Bool()))

  for (i <- 0 until setNum) {
    val Idxhit = i.U === io.idx
    hitArray(i)             := Idxhit
    plruSet(i).access.bits  := io.access.bits
    plruSet(i).access.valid := io.access.valid & Idxhit
    outputArray(i)          := plruSet(i).replace
  }
  io.replace := Mux1H(hitArray, outputArray) // better readablity, select replace based on hit idx
}

class MptData(implicit p: Parameters) extends XSBundle with MPTCacheParam {
  val data = UInt(perms16Len.W)
  def apply(data: UInt): Unit = this.data := data // zero extended
  def getPPN: UInt = this.data(ppnLen - 1, 0)
  def getAddr(offset: UInt): UInt = Cat(this.getPPN, Cat(offset, 0.U(3.W))) // 2|3 byte = 64bit
  def extractPerm(select: UInt): (UInt) = (this.data >> (select * 3.U))(2, 0) // extract XWR using 4bit offset
  // cal start end and extract
}

class MptEntry(implicit p: Parameters) extends XSBundle with MPTCacheParam {
  val N    = Bool()
  val data = new MptData()
  val L    = Bool()
  val V    = Bool()
  def apply(sMEMResp: UInt): Unit = {
    this.V := (sMEMResp(0) === 1.U)
    this.L := (sMEMResp(1) === 1.U)
    this.N := (sMEMResp(63) === 1.U)
    this.data.apply(sMEMResp(57, 10)) // xiangshan only support 48bit PA, so PPN only needs 36
  }
  def isValid: Bool =
    this.V

  def isLeaf: Bool =
    this.L

  def getAddr(offset: UInt): UInt =
    this.data.getAddr(offset)

  def genFake(level: UInt): Unit = {
    this.N         := false.B
    this.data.data := "h802F4".U
    this.L         := false.B
    this.V         := true.B
    switch(level) {
      is("b1000".U) {
        this.N         := false.B
        this.data.data := "h802F4".U
        this.L         := false.B
        this.V         := true.B
      }
      is("b0100".U) {
        this.N         := false.B
        this.data.data := "h802F5".U
        this.L         := false.B
        this.V         := true.B
      }
      is("b0010".U) {
        if (HasMptCheckDefault4k) {
          this.N         := false.B
          this.data.data := "h802F6".U
          this.L         := false.B
          this.V         := true.B
        } else {
          this.N         := false.B
          this.data.data := "hFFFFFFFFFFFF".U
          this.L         := true.B
          this.V         := true.B
        }
      }
      is("b0001".U) {
        this.N         := false.B
        this.data.data := "hFFFFFFFFFFFF".U
        this.L         := true.B
        this.V         := true.B
      }
    }
  }
}

class MptCacheTag(tagLen: Int, isSp: Boolean = false)(implicit p: Parameters) extends XSBundle with MPTCacheParam {
  val tag   = UInt(tagLen.W)
  val level = Option.when(isSp)(UInt((mptLevelLenOH - 1).W)) // sp can not be l0
  def hit(ppn: UInt): Bool =
    tag === ppn(ppnLen - 1, ppnLen - tagLen) // tag = 5, (47,43)
  def hitSp(ppn: UInt): Bool = {
    val hitL3 = this.tag(tagLen - 1, tagLen - mptL3TagLen) === ppn(ppnLen - 1, ppnLen - mptL3TagLen) // tag = 5, (47,43)
    val hitL2 = this.tag(tagLen - 1, tagLen - mptL2TagLen) === ppn(ppnLen - 1, ppnLen - mptL2TagLen)
    val hitL1 = this.tag === ppn(ppnLen - 1, ppnLen - tagLen)
    val hotVal = Mux1H(Seq(
      this.level.get(2) -> hitL3,
      this.level.get(1) -> hitL2,
      this.level.get(0) -> hitL1
    )) // it is a tuple scala> 1 -> 2 res0: (Int, Int) = (1,2)
    hotVal
  }
}
class MptCacheData(isPerms: Boolean = false)(implicit p: Parameters) extends XSBundle with MPTCacheParam {
  val data = if (isPerms) UInt(perms16Len.W) else UInt(ppnLen.W) // 36.W
  def extractPerm(select: UInt): (UInt) = { // extract XWR using 4bit offset
    // cal start end and extract
    require(isPerms, "extractPerm is only valid when isPerms is true")
    (this.data >> (select * 3.U))(
      2,
      0
    ) // not quite sure what kind of crap will be synthesized. I meant it to be a binary mux
  }
}

class MptCacheL0(implicit p: Parameters) extends XSBundle with MPTCacheParam {
  val cacheData = new MptCacheData(isPerms = true)
  val tag       = new MptCacheTag(tagLen = mptL0TagLen) // , isL0 = true )
}

class MptCacheReq(implicit p: Parameters) extends XSBundle with MPTCacheParam {
  val mptOnly = Bool()
  val reqPA   = UInt(ppnLen.W)
  val source  = UInt(mptSourceWidth.W)
}

// create pipe with reset
class MPTPipe(implicit p: Parameters) extends MptCacheReq {
  val dataValid  = Bool()
  val flushCache = Bool()

  def applySplitData(MPTPipeControl: MPTPipeControl, MPTPipeData: MPTPipeData): Unit = {
    this.dataValid  := MPTPipeControl.dataValid
    this.flushCache := MPTPipeControl.flushCache
    this.mptOnly    := MPTPipeControl.mptOnly
    this.reqPA      := MPTPipeData.reqPA
    this.source     := MPTPipeData.source
  }

  def createSplitData(): (MPTPipeControl, MPTPipeData) = {
    val mptPipeControl = Wire(new MPTPipeControl) // evil defination of hardware type :-(
    val mptPipeData    = Wire(new MPTPipeData)
    mptPipeData.reqPA  := this.reqPA
    mptPipeData.source := this.source

    mptPipeControl.dataValid  := this.dataValid
    mptPipeControl.flushCache := this.flushCache
    mptPipeControl.mptOnly    := this.mptOnly
    (mptPipeControl, mptPipeData)
  }
}

class MPTPipeControl(implicit p: Parameters) extends XSBundle with MPTCacheParam {
  val dataValid  = Bool()
  val flushCache = Bool()
  val mptOnly    = Bool()
  def applyPipeData(mptPipe: MPTPipe): Unit = {
    this.dataValid  := mptPipe.dataValid
    this.flushCache := mptPipe.flushCache
    this.mptOnly    := mptPipe.mptOnly
  }
}

class MPTPipeData(implicit p: Parameters) extends XSBundle with MPTCacheParam {
  val reqPA  = UInt(ppnLen.W)
  val source = UInt(mptSourceWidth.W)
  def applyPipeData(MPTPipe: MPTPipe): Unit = {
    this.reqPA  := MPTPipe.reqPA
    this.source := MPTPipe.source
  }
}

object MPTPipeWithReset {
  def apply(enqValid: Bool, enqBits: MPTPipe, reset: Bool, latency: Int): Valid[MPTPipe] = {
    require(latency >= 0, "Pipe latency must be greater than or equal to zero!")

    if (latency == 0) {
      val out = Wire(Valid(chiselTypeOf(enqBits)))
      out.valid := enqValid
      out.bits  := enqBits
      out
    } else {
      val v = RegNext(enqValid, false.B) // valid has reset
      val (mptPipeControlIn, mptPipeDataIn) = enqBits.createSplitData() // split input as data and control
      val mptPipeData = RegEnable(mptPipeDataIn, (mptPipeControlIn.dataValid || mptPipeControlIn.flushCache) && enqValid)
      // data has no reset
      val mptPipeControl = RegEnable(mptPipeControlIn, 0.U.asTypeOf(mptPipeControlIn), enqValid) // control has reset
      val b              = Wire(chiselTypeOf(enqBits))
      when(reset) {
        mptPipeControl := 0.U.asTypeOf(mptPipeControlIn)
        v := false.B
      }
      b.applySplitData(mptPipeControl, mptPipeData) // merge pipe control and data signal
      apply(v, b, reset, latency - 1)
    }
  }
}

class RefillBundle(implicit p: Parameters) extends XSBundle with MPTCacheParam {
  val level      = UInt(mptLevelLenOH.W)
  val pa         = UInt((PAddrBits - mptOff).W)
  val refillData = new MptData()
  val isAf       = Bool()
  val isLeafMpte = Bool() // is leaf? decide what cache is refilled
}
class MPTCacheIO(implicit p: Parameters) extends MMUIOBaseBundle with MPTCacheParam {
  val req = Flipped(DecoupledIO(new MptCacheReq()))

  val respHit = ValidIO(new Bundle { // source is waiting for cache to resp
    val accessFault      = Bool()
    val perm             = UInt(3.W)
    val tlbContigousPerm = Bool()
    val permIsNAPOT      = Bool()
    val source           = UInt(mptSourceWidth.W)
    val mptLevel         = UInt(log2Up(mptLevelLenOH).W)
    val mptOnly          = Bool()
    val reqPA            = UInt(ppnLen.W)
  })

  val respMiss = DecoupledIO(new Bundle {
    val hitLevel = UInt(mptLevelLenOH.W)
    val ppn      = UInt(ppnLen.W) // minsize is 4k,dont need 12bits offset
    val source   = UInt(mptSourceWidth.W)
    val pa       = UInt(ppnLen.W)
    val mptOnly  = Bool()
  })

  val refill = Flipped(ValidIO(new RefillBundle()))

}

class MPTCache(implicit p: Parameters) extends XSModule with MPTCacheParam {
  val io = IO(new MPTCacheIO)
  // // mfence signal
  val mfenceActive = WireInit(false.B)
  val fencePA      = WireInit(false.B)
  val mfencevalid  = io.sfence.valid && io.sfence.bits.mfence.get
  // This MPT design supports partial cache flushing by PA. When flushing, in addition to leaf nodes, intermediate nodes are also invalidated
  switch(Cat(io.sfence.bits.rs2, io.sfence.bits.rs1).asUInt) {
    is("b11".U) {
      fencePA := (io.sfence.bits.id === io.csr.mmpt.sdid) && mfencevalid // delay of about 10 gates
    }
    is("b01".U) {
      fencePA := mfencevalid
    }
    is("b10".U) {
      mfenceActive := (io.sfence.bits.id === io.csr.mmpt.sdid) && mfencevalid
    }
    is("b00".U) {
      mfenceActive := mfencevalid
    }
  }
  val flushAll      = mfenceActive || io.csr.mmpt.changed

  val pipeFlowEn = Wire(Bool())
  val refilling     = Wire(Bool())
  val refillCounter = RegInit(0.U(4.W))
  when(io.refill.valid) { // && ! io.refill.bits.isAf) { // refill valid and not access fault
    refillCounter := "b1000".U
  }.elsewhen(io.sfence.valid) {
    refillCounter := 0.U
  }.otherwise {
    refillCounter := refillCounter >> 1.U
  }

  refilling := refillCounter > 0.U // is refiill state when counter ! = 0 for 4 rounds
  val respHitRegTmp = Wire(Bool()) // resphitreg is defined later
  pipeFlowEn := (io.respMiss.ready || respHitRegTmp) || refilling || fencePA
  // Without respHitReg, if the final stage hits and stalls,
  // the hit control signal will get stuck at a high level, causing the entire MMU to fail.
  // Switch the pipeline input based on whether it is an mfence-PA operation or a refill operation.
  val paFenceInputs = Wire(new MPTPipe)
  paFenceInputs.reqPA      := io.sfence.bits.addr(47, 12)
  paFenceInputs.source     := io.req.bits.source
  paFenceInputs.dataValid  := false.B
  paFenceInputs.flushCache := true.B
  paFenceInputs.mptOnly    := false.B

  val ioInputs = Wire(new MPTPipe)
  ioInputs.reqPA      := io.req.bits.reqPA
  ioInputs.source     := io.req.bits.source
  ioInputs.dataValid  := io.req.fire
  ioInputs.flushCache := false.B
  ioInputs.mptOnly    := io.req.bits.mptOnly

  val pipeInputs = Wire(new MPTPipe)

  val stageReq     = MPTPipeWithReset(pipeFlowEn, pipeInputs, flushAll, 1)
  val stageDelayin = stageReq.bits
  val stageDelay   = MPTPipeWithReset(pipeFlowEn, stageDelayin, flushAll, 1)
  val stageCheckin = stageDelay.bits
  val stageCheck   = MPTPipeWithReset(pipeFlowEn, stageCheckin, flushAll, 1)
  val stageRespin  = stageCheck.bits
  val stageResp    = MPTPipeWithReset(pipeFlowEn, stageRespin, flushAll, 1)

  // priority
  // 1. fence PA
  // 2. refill and last stage valid
  // 3. normal request

  pipeInputs := Mux(fencePA, paFenceInputs, Mux(refilling && stageResp.bits.dataValid, stageResp.bits, ioInputs))
  when(io.sfence.valid) {

    pipeInputs.dataValid := false.B
    pipeInputs.mptOnly := false.B

    stageDelayin.dataValid := false.B
    stageDelayin.mptOnly := false.B

    stageCheckin.dataValid := false.B
    stageCheckin.mptOnly := false.B

    stageRespin.dataValid := false.B
    stageRespin.mptOnly := false.B
  }
  io.req.ready := ((io.respMiss.ready && !refilling) || (refilling && !stageResp.bits.dataValid)) && !fencePA
  // init cache tag
  val l3Tag   = Reg(Vec(l3Size, new MptCacheTag(tagLen = mptL3TagLen)))
  val l3Valid = RegInit(Vec(l3Size, Bool()), 0.U.asTypeOf(Vec(l3Size, Bool())))

  val l2Tag   = Reg(Vec(l2Size, new MptCacheTag(tagLen = mptL2TagLen)))
  val l2Valid = RegInit(Vec(l2Size, Bool()), 0.U.asTypeOf(Vec(l2Size, Bool())))

  val l1Tag   = Reg(Vec(l1Size, new MptCacheTag(tagLen = mptL1TagLen)))
  val l1Valid = RegInit(Vec(l1Size, Bool()), 0.U.asTypeOf(Vec(l1Size, Bool())))

  val spTag   = Reg(Vec(spSize, new MptCacheTag(tagLen = mptspTagLen, isSp = true)))
  val spValid = RegInit(Vec(spSize, Bool()), 0.U.asTypeOf(Vec(spSize, Bool())))

  val l3Data = Reg(Vec(l3Size, new MptCacheData()))
  val l2Data = Reg(Vec(l2Size, new MptCacheData()))
  val l1Data = Reg(Vec(l1Size, new MptCacheData()))
  val spData = Reg(Vec(spSize, new MptCacheData(isPerms = true)))
  val l0Data = Module(new SplittedSRAM(
    new MptCacheL0(),
    set = l0nSets,
    way = l0nWays,
    setSplit = 1,
    waySplit = 2,
    dataSplit = 1,
    singlePort = sramSinglePort,
    readMCP2 = false,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl
  )) // 1clk delay from req to resp
  val l0Valid = RegInit(Vec(l0nSets, Vec(l0nWays, Bool())), 0.U.asTypeOf(Vec(l0nSets, Vec(l0nWays, Bool()))))

  val mptCacheL3Replace = Module(new PLRUOH(logWays = log2Up(l3Size))).io
  val mptCacheL2Replace = Module(new PLRUOH(logWays = log2Up(l2Size))).io
  val mptCacheL1Replace = Module(new PLRUOH(logWays = log2Up(l1Size))).io
  val MptCacheL0Replace = Module(new PLRUOHSet(setsLog2 = log2Up(l0nSets), logWays = log2Up(l0nWays))).io
  val mptCacheSpReplace = Module(new PLRUOH(logWays = log2Up(spSize))).io
  // alloc replacement policy,use PLRU with Onehot in/out

  val (l3Hit, l3hitPPN) = {
    val hitVecTemp = l3Tag.zip(l3Valid).map { case (x, v) =>
      x.hit(stageReq.bits.reqPA) && v
    } // hit when valid and tag equal stagereq
    when(stageReq.bits.flushCache) { // clean fence valid if hit tag
      hitVecTemp.zip(l3Valid).map { case (x, v) =>
        when(x)(v := false.B)
      }
    }
    val hitVec = RegEnable(VecInit(hitVecTemp), stageReq.bits.dataValid)
    // ready at stage check, use datavalid instead of stageReq.valid
    val hitData = RegEnable(Mux1H(hitVecTemp, l3Data), stageReq.bits.dataValid)
    // we can use onehot mux, should be faster.
    val hit =RegEnable(hitVecTemp.reduce(_ || _), stageReq.bits.dataValid)
    // 1 bit hit, avaliable at stage delay after 2 or gates
    mptCacheL3Replace.access.bits := hitVec.asUInt
    // assign hitVec to plru to update plru state, miss(hitVec = h0) will not change the plru state
    mptCacheL3Replace.access.valid := stageDelay.bits.dataValid // ready at stage check
    (hit, hitData)
  }

  val (l2Hit, l2hitPPN) = {
    val hitVecTemp = l2Tag.zip(l2Valid).map { case (x, v) =>
      x.hit(stageReq.bits.reqPA) && v
    }                                // hit when valid and tag equal stagereq
    when(stageReq.bits.flushCache) { // clear fence valid
      hitVecTemp.zip(l2Valid).map { case (x, v) =>
        when(x)(v := false.B)
      }
    }
    val hitVec = RegEnable(VecInit(hitVecTemp), stageReq.bits.dataValid) // ready at stage check
    // val hitData = ParallelPriorityMux(hitVec zip l3Data)
    val hitData = RegEnable(Mux1H(hitVecTemp, l2Data), stageReq.bits.dataValid)
    // we can use onehot mux, should be faster.
    val hit = RegEnable(hitVecTemp.reduce(_ || _), stageReq.bits.dataValid)
    // 1 bit hit, avaliable at stage delay after 2 or gates
    mptCacheL2Replace.access.bits := hitVec.asUInt
    // assign hitVec to plru to update plru state, miss(hitVec = h0) will not change the plru state
    mptCacheL2Replace.access.valid := stageDelay.bits.dataValid // ready at stage check
    (hit, hitData)
  }

  val (l1Hit, l1hitPPN) = {
    val hitVecTemp = l1Tag.zip(l1Valid).map { case (x, v) =>
      x.hit(stageReq.bits.reqPA) && v
    }                                // hit when valid and tag equal stagereq
    when(stageReq.bits.flushCache) { // clear fence valid
      hitVecTemp.zip(l1Valid).map { case (x, v) =>
        when(x)(v := false.B)
      }
    }
    val hitVec = RegEnable(VecInit(hitVecTemp), stageReq.bits.dataValid) // ready at stage check
    // val hitData = ParallelPriorityMux(hitVec zip l3Data)
    val hitData = RegEnable(Mux1H(hitVecTemp, l1Data), stageReq.bits.dataValid)
    // we can use onehot mux, should be faster.
    val hit = RegEnable(hitVecTemp.reduce(_ || _), stageReq.bits.dataValid)
    // 1 bit hit, avaliable at stage delay after 2 or gates

    mptCacheL1Replace.access.bits := hitVec.asUInt
    // assign hitVec to plru to update plru state, miss(hitVec = h0) will not change the plru state
    mptCacheL1Replace.access.valid := stageDelay.bits.dataValid // ready at stage check
    (hit, hitData)

  }
  // // // // // // gen addr hit(l3-l1) at stage check // // // // // //
  val missLevel = Mux(io.csr.mmpt.mode === 2.U, "b1000".U, "b0100".U)
    // enablesmmpt52 = true, 0 delay since io.csr.mmpt.mode will not change during cache read
  val hitAddrLevelTemp = Mux(l1Hit, "b0001".U, Mux(l2Hit, "b0010".U, Mux(l3Hit, "b0100".U, missLevel)))
  val hitAddrDataTemp = Mux(l1Hit, l1hitPPN.data, Mux(l2Hit, l2hitPPN.data,
    Mux(l3Hit, l3hitPPN.data, io.csr.mmpt.ppn(ppnLen - 1, 0))))
  val hitAddrData  = RegEnable(hitAddrDataTemp, stageDelay.bits.dataValid)
  val hitAddrLevel = RegEnable(hitAddrLevelTemp, stageDelay.bits.dataValid)

  // // // // // // // // // // // // // /

  val (l0Hit, l0HitPerms, l0PermTlbCompress, l0PermIs64kNAPOT) = {
    val idx = geL0Set(pipeInputs.reqPA) //

    l0Data.io.r.req.bits.apply(setIdx = idx) // .. 0 delay stagereq reg get valid at the same time
    l0Data.io.r.req.valid := pipeInputs.dataValid || pipeInputs.flushCache // read and write at the same time will not cause error, but read is invalid

    val l0validReg = RegEnable(
      l0Valid(geL0Set(stageReq.bits.reqPA)),
      0.U.asTypeOf(Vec(l0nWays, Bool())),
      stageReq.bits.dataValid || stageReq.bits.flushCache
    )
    when(flushAll) {
      l0validReg := 0.U.asTypeOf(Vec(l0nWays, Bool()))
    }
    val dataResp = RegEnable(l0Data.io.r.resp.data, stageReq.bits.dataValid || stageReq.bits.flushCache)
    // data avaliable at stage delay
    val setTag  = dataResp.map(_.tag)
    val setData = dataResp.map(_.cacheData) // 4 entry+tag
    // some wire
    val hitVecTemp = setTag.zip(l0validReg).map { case (x, v) =>
      x.hit(stageDelay.bits.reqPA) && v
    } // hit when valid and tag equal

    // delay (29 bit === ):(1xnor + 5*and), (&& valid):(1 and) total 7
    // MfencePA
    when(stageDelay.bits.flushCache) { // clear fence valid
      hitVecTemp.zipWithIndex.map { case (x, i) =>
        when(x)(l0Valid(geL0Set(stageDelay.bits.reqPA))(i) := false.B)
      }
    }
    //
    val hitVec = RegEnable(VecInit(hitVecTemp), stageDelay.bits.dataValid) // valid at stage check
    val hitData = Mux1H(hitVecTemp, setData)
    // we use onehot mux, should be faster than ParallelPriorityMux. delay:log2(4)*2 = 4
    val hitDataReg = RegEnable(hitData, stageDelay.bits.dataValid) // valid at stage check, total delay 11 gates
    val hit = RegEnable(hitVecTemp.reduce(_ || _), stageDelay.bits.dataValid) // 4-> 1 bit hit

    val hitPermsTemp = hitDataReg.extractPerm(stageCheck.bits.reqPA(3, 0)) // always 15:12 delay log2(16)*3 = 12 gates

    MptCacheL0Replace.access.bits := hitVec.asUInt // assign hitVec to plru to update plru state, miss(hitVec = h0) will not change the plru state
    MptCacheL0Replace.access.valid := stageCheck.bits.dataValid // processing at stage check, ready at stage resp
    MptCacheL0Replace.idx          := geL0Set(stageCheck.bits.reqPA)

    val permsAsVec = Wire(Vec(16, UInt(3.W))) // perm xwr bits, total 16 xwrs in one mpte
    for (i <- 0 until 16) {
      permsAsVec(i) := hitDataReg.data(2 + i * 3, i * 3)
    }
    val permsEqual = Wire(Vec(16 - 1, Bool()))
    for (i <- 0 until 16 - 1) {
      permsEqual(i) := permsAsVec(i + 1) === permsAsVec(i)
    } // delay 1XNOR + 2 and gates = 3
    val low8PermsAllEqual  = permsEqual.slice(0, 7).reduce(_ && _)                    // = permsEqual(6,0), delay 3
    val high8PermsAllEqual = permsEqual.slice(8, 15).reduce(_ && _)                   // = permsEqual(14,8)
    val allEqual           = low8PermsAllEqual && high8PermsAllEqual && permsEqual(7) // Delay 2
    val tlbCompress = Mux(stageCheck.bits.reqPA(3, 0) < 8.U, low8PermsAllEqual, high8PermsAllEqual) // delay 3

    (
      hit,
      RegEnable(hitPermsTemp, stageCheck.bits.dataValid),
      RegEnable(tlbCompress, stageCheck.bits.dataValid),
      RegEnable(allEqual, stageCheck.bits.dataValid)
    ) // ready at stage resp,hit reaady at stage check
  }

  // val (sphit,spHitPerms,spPermIsNAPOT,splevel) = {
  val (sphit, spHitPerms, splevel) = {
    val hitVecTemp = spTag.zip(spValid).map { case (x, v) =>
      x.hitSp(stageReq.bits.reqPA) && v
    }                                // hit when valid and tag equal delay 7 + mux1h delay 4 gates
    when(stageReq.bits.flushCache) { // clear fence valid
      hitVecTemp.zip(spValid).map { case (x, v) =>
        when(x)(v := false.B)
      }
    }

    val hitVec = RegEnable(VecInit(hitVecTemp), stageReq.bits.dataValid) // ready at stage delay
    // // // //
    val levelVec  = spTag.map(x => x.level.get)
    val level     = Mux1H(hitVec, levelVec)       // ready at stage delay, require cache to be consistent
    val levelReg  = RegEnable(level, stageDelay.bits.dataValid)
    val levelUInt = Wire(UInt(mptLevelLenUInt.W)) // 4levels len 2
    levelUInt := OHToUInt(Cat(levelReg, 0.U(1.W)))

    val extractOffset = Mux1H(Seq(
      level(2) -> stageDelay.bits.reqPA(3 + 9 * 3, 9 * 3),
      level(1) -> stageDelay.bits.reqPA(3 + 9 * 2, 9 * 2),
      level(0) -> stageDelay.bits.reqPA(3 + 9, 9)
    ))

    val extractOffsetReg = RegEnable(extractOffset, stageDelay.bits.dataValid) // ready at stage check

    // val hitData = ParallelPriorityMux(hitVec zip l3Data)
    val hitData      = Mux1H(hitVec, spData)                         // we can use onehot mux, should be faster.
    val hitDataReg   = RegEnable(hitData, stageDelay.bits.dataValid) // valid at stage check, total delay 11 gates
    val hitPermsTemp = hitDataReg.extractPerm(extractOffsetReg)      // always 15:12 delay log2(16)*3 = 12 gates
    val hit = RegEnable(hitVec.reduce(_ || _), stageDelay.bits.dataValid) // 1 bit hit

    mptCacheSpReplace.access.bits := hitVec.asUInt // assign hitVec to plru to update plru state, miss(hitVec = h0) will not change the plru state
    mptCacheSpReplace.access.valid := stageDelay.bits.dataValid // ready at stage check

    (
      hit,
      RegEnable(hitPermsTemp, stageCheck.bits.dataValid),
      RegEnable(levelUInt, stageCheck.bits.dataValid)
    ) // ready at stage resp,hit reaady at stage check

  }
  // gen perms hit l0 sp at stage check

  val hitPerms = sphit || l0Hit
  val respHitReg = RegEnable(hitPerms & stageCheck.bits.dataValid, false.B, pipeFlowEn)
  // Data is latched regardless of dataValid. If dataValid is low, the hit signal is also considered invalid.
  // In this case, hitPerms actually holds the result from the previous pipeline stage.

  respHitRegTmp    := respHitReg
  io.respHit.valid := respHitReg && !refilling && !fencePA
  val (sphitReg, l0hitReg) = (RegEnable(sphit, pipeFlowEn), RegEnable(l0Hit, pipeFlowEn))
  io.respHit.bits.perm := Mux1H(
    Seq(sphitReg, l0hitReg),
    Seq(spHitPerms, l0HitPerms)
  ) // 1 mux at output, 2 gates, should be fine
  io.respHit.bits.source           := stageResp.bits.source
  io.respHit.bits.mptOnly          := stageResp.bits.mptOnly
  io.respHit.bits.reqPA            := stageResp.bits.reqPA
  io.respHit.bits.tlbContigousPerm := l0hitReg && l0PermTlbCompress
  io.respHit.bits.permIsNAPOT := l0hitReg && l0PermIs64kNAPOT
  io.respHit.bits.accessFault := (!io.respHit.bits.perm(0)) && io.respHit.bits.perm(
    1
  ) // not read but write // false.B // entry in mpt cache is always valid
  io.respHit.bits.mptLevel := Mux1H(
    Seq(sphitReg, l0hitReg),
    Seq(splevel, 0.U(mptLevelLenUInt.W))
  ) // splevel is converted to binary for l1/l2tlb level compare with s1pte and s2pte

  val respMissReg = RegEnable(!hitPerms & stageCheck.bits.dataValid, false.B, pipeFlowEn)
  // read regardless of dataValid
  io.respMiss.valid := respMissReg && !refilling && !fencePA
  io.respMiss.bits.hitLevel := RegEnable(hitAddrLevel, pipeFlowEn)
  io.respMiss.bits.ppn     := RegEnable(hitAddrData, pipeFlowEn)
  io.respMiss.bits.source  := stageResp.bits.source
  io.respMiss.bits.mptOnly := stageResp.bits.mptOnly
  io.respMiss.bits.pa      := stageResp.bits.reqPA
  // read logic end
  // refill write logic start
  /* If a circular pipe is used to resolve refill conflicts, the cache will be accessed twice with the same tag during the loop.
  For TLRU, this is not an issue as the LRU queue remains unchanged. But what about PLRU? It should probably be fine as well.
  If TLRU is used, when the cache is empty, the LRU pointer should point to an empty entry because empty entries have never been accessed.
  Therefore, we can normally use the entry pointed to by the LRU for refilling.
  If PLRU is used, when the cache is empty, accessing neighbors of an empty entry may cause the LRU to stop pointing to that empty entry.
  For example, with a 4-way PLRU sequence ABACAD, B would be replaced by D, while the empty entry next to A would not be written to.
  We could fill the cache from top to bottom regardless of the PLRU state when it is not full.
  However, due to tight timing constraints, let's try using only PLRU first to see how much waste/inefficiency it causes.*/

  val l3RefillEn =
    io.refill.bits.level(3).asBool & (!io.refill.bits.isLeafMpte) & (io.refill.valid && !io.refill.bits.isAf)
  val rfl3Tag     = io.refill.bits.pa(PAddrBits - mptOff - 1, PAddrBits - mptOff - mptL3TagLen)
  val l3VictimWay = mptCacheL3Replace.replace // ready after idx is set, OH encoding

  val l2RefillEn =
    io.refill.bits.level(2).asBool & (!io.refill.bits.isLeafMpte) & (io.refill.valid && !io.refill.bits.isAf)
  val rfl2Tag     = io.refill.bits.pa(PAddrBits - mptOff - 1, PAddrBits - mptOff - mptL2TagLen)
  val l2VictimWay = mptCacheL2Replace.replace // ready after idx is set, OH encoding

  val l1RefillEn =
    io.refill.bits.level(1).asBool & (!io.refill.bits.isLeafMpte) & (io.refill.valid && !io.refill.bits.isAf)
  val rfl1Tag     = io.refill.bits.pa(PAddrBits - mptOff - 1, PAddrBits - mptOff - mptL1TagLen)
  val l1VictimWay = mptCacheL1Replace.replace // ready after idx is set, OH encoding

  val l0RefillEn =
    io.refill.bits.level(0).asBool & (io.refill.bits.isLeafMpte) & (io.refill.valid && !io.refill.bits.isAf)
  val rfl0Tag     = io.refill.bits.pa(PAddrBits - mptOff - 1, PAddrBits - mptOff - mptL0TagLen)
  val rfl0SetIdx  = io.refill.bits.pa(PAddrBits - mptOff - mptL0TagLen - 1, 0)
  val l0VictimWay = MptCacheL0Replace.replace // ready after idx is set, OH encoding

  val spRefillEn =
    (!io.refill.bits.level(0).asBool) & io.refill.bits.isLeafMpte & (io.refill.valid && !io.refill.bits.isAf)
  val rfspTag     = io.refill.bits.pa(PAddrBits - mptOff - 1, PAddrBits - mptOff - mptspTagLen)
  val spVictimWay = mptCacheSpReplace.replace //

  // /write data into cache 1 cycle
  val l0Wdata = Wire(new MptCacheL0()) // 0 delay, wire signal assignment
  l0Wdata.tag.tag        := rfl0Tag
  l0Wdata.cacheData.data := io.refill.bits.refillData.data
  l0Data.io.w.req <> DontCare // default val for write channel is invalid
  l0Data.io.w.req.valid := false.B
  when(l0RefillEn) {
    l0Data.io.w.apply(
      valid = true.B, // valid when refill
      setIdx = rfl0SetIdx,
      data = l0Wdata,
      waymask = l0VictimWay
    )
    for (j <- 0 until l0nSets) {
      for (i <- 0 until l0nWays) {
        when(l0VictimWay(i) === 1.U && rfl0SetIdx === j.U) {
          l0Valid(j)(i) := true.B
        }
      }
    }

    MptCacheL0Replace.idx := rfl0SetIdx
    /* During refill, switch the PLRU input to the refill data and update immediately.
    Timing breakdown: 4-way update delay (2 gates), 32-set index switching (3 gates),
    l0 refillEn (1 gate), mux for switching refillEn input (3 gates).*/
    MptCacheL0Replace.access.bits  := l0VictimWay
    MptCacheL0Replace.access.valid := true.B
  }

  when(l3RefillEn) {
    for (i <- 0 until l3Size) {
      when(l3VictimWay(i) === 1.U) {
        l3Tag(i).tag   := rfl3Tag
        l3Valid(i)     := true.B
        l3Data(i).data := io.refill.bits.refillData.getPPN
      }
    }
    mptCacheL3Replace.access.bits  := l3VictimWay
    mptCacheL3Replace.access.valid := true.B
  }

  when(l2RefillEn) {
    for (i <- 0 until l2Size) {
      when(l2VictimWay(i) === 1.U) {
        l2Tag(i).tag   := rfl2Tag
        l2Valid(i)     := true.B
        l2Data(i).data := io.refill.bits.refillData.getPPN
      }
    }
    mptCacheL2Replace.access.bits  := l2VictimWay
    mptCacheL2Replace.access.valid := true.B
  }
  when(l1RefillEn) {
    for (i <- 0 until l1Size) {
      when(l1VictimWay(i) === 1.U) {
        l1Tag(i).tag   := rfl1Tag
        l1Valid(i)     := true.B
        l1Data(i).data := io.refill.bits.refillData.getPPN
      }
    }
    mptCacheL1Replace.access.bits  := l1VictimWay
    mptCacheL1Replace.access.valid := true.B
  }
  when(spRefillEn) {
    /*spVictimWay.zipWithIndex.map{case(en,i) => // update cache content
              when(en) {*/
    for (i <- 0 until spSize) {
      when(spVictimWay(i) === 1.U) {
        spTag(i).tag       := rfspTag
        spValid(i)         := true.B
        spTag(i).level.get := io.refill.bits.level(3, 1)
        spData(i).data     := io.refill.bits.refillData.data
      }
    }
    mptCacheSpReplace.access.bits  := spVictimWay
    mptCacheSpReplace.access.valid := true.B
  }

  when(flushAll) {
    refillCounter := 0.U
    l3Valid := 0.U.asTypeOf(Vec(l3Size, Bool()))
    l2Valid := 0.U.asTypeOf(Vec(l2Size, Bool()))
    l1Valid := 0.U.asTypeOf(Vec(l1Size, Bool()))
    spValid := 0.U.asTypeOf(Vec(spSize, Bool()))
    l0Valid := 0.U.asTypeOf(Vec(l0nSets, Vec(l0nWays, Bool())))
    respHitReg := false.B
    respMissReg := false.B
  }
}
// // // MptMissQueue START // // //

class MptMissQueueToTWReqBundle(implicit p: Parameters) extends XSBundle with MPTCacheParam {
  val hitAddr  = UInt(ppnLen.W)
  val reqPA    = UInt((PAddrBits - mptOff).W)
  val hitLevel = UInt(mptLevelLenOH.W)
}

class MissCacheBundle(implicit p: Parameters) extends XSBundle with MPTCacheParam {
  val hitLevel = UInt(mptLevelLenOH.W)
  val hitAddr  = UInt(ppnLen.W) // hit addr
  val source   = UInt(mptSourceWidth.W)
  val pa       = UInt(ppnLen.W) // req pa minsize is 4k,dont need 12bits offset
  val mptOnly  = Bool()         // 1bit control signal for tlb, does basically nothing in mptc
}

class MptMissQueueIO(implicit p: Parameters) extends MMUIOBaseBundle with MPTCacheParam {
  val missCache = Flipped(DecoupledIO(new MissCacheBundle()))

  val twReq = DecoupledIO(new MptMissQueueToTWReqBundle())

  val refill = Flipped(ValidIO(new RefillBundle()))
  val resp = ValidIO(new Bundle { // source is waiting for cache to resp
    val accessFault     = Bool()
    val mptLevel        = UInt(mptLevelLenOH.W)
    val perm            = UInt(3.W)
    val PermTlbCompress = Bool()
    val permIsNAPOT     = Bool()
    val mptOnly         = Bool()
    val reqPA           = UInt(ppnLen.W)
    val source          = UInt(mptSourceWidth.W)
  })
}

class MptMissQueue(implicit p: Parameters) extends XSModule with MPTCacheParam {
  val io = IO(new MptMissQueueIO)

  val flush = io.sfence.valid || io.csr.mmpt.changed
  val reqFIFO = Module(new Queue(new MissCacheBundle(), entries = 4, pipe = true, hasFlush = true)).io
  // FIFO queue,record offset
  val fifoNotEmpty = reqFIFO.deq.valid
  val fifoNotFull  = reqFIFO.enq.ready
  val reqAltInput = Wire(new MissCacheBundle()) // alternative input for refill
  val refillReg = RegEnable(io.refill.bits, 0.U.asTypeOf(new RefillBundle()), io.refill.valid)
  // refill input, valid when refill is valid

  val refilling     = Wire(Bool())
  val refillCounter = RegInit(0.U(4.W))
  when(io.refill.valid) { // refill valid and not access fault
    refillCounter := "b1000".U
  }.otherwise {
    refillCounter := refillCounter >> 1.U
  }

  refilling := refillCounter > 0.U // is refiill state when counter ! = 0 4 clk

  val l3Hit = reqFIFO.deq.bits.pa(ppnLen - 1, ppnLen - mptL3TagLen) === refillReg.pa(
    PAddrBits - mptOff - 1,
    PAddrBits - mptOff - mptL3TagLen
  ) // 35:31 31:27
  val l2Hit = reqFIFO.deq.bits.pa(ppnLen - 1, ppnLen - mptL2TagLen) === refillReg.pa(
    PAddrBits - mptOff - 1,
    PAddrBits - mptOff - mptL2TagLen
  ) // 35:22 31:18
  val l1Hit = reqFIFO.deq.bits.pa(ppnLen - 1, ppnLen - mptL1TagLen) === refillReg.pa(
    PAddrBits - mptOff - 1,
    PAddrBits - mptOff - mptL1TagLen
  ) // 35:13 31:9
  val l0Hit = reqFIFO.deq.bits.pa(ppnLen - 1, mptOff - offLen) === refillReg.pa // 35:4 31:0
  val hitFIFO = Mux1H(Seq(
    refillReg.level(3) -> l3Hit,
    refillReg.level(2) -> l2Hit,
    refillReg.level(1) -> l1Hit,
    refillReg.level(0) -> l0Hit // 4k addr len can not repeat
  )) //

  io.missCache.ready := fifoNotFull && !refilling

  reqFIFO.enq.valid := (fifoNotFull && io.missCache.valid && !refilling) ||
    (refilling && fifoNotEmpty && !refillReg.isLeafMpte && !refillReg.isAf) ||
    (refilling && fifoNotEmpty && !hitFIFO && (refillReg.isLeafMpte || refillReg.isAf))
  // Entries are enqueued under three conditions: 1. Normal miss cache request; 2. Refill of a non-leaf MPTE;
  // 3. Refill of a leaf MPTE or access fault, provided it is not a hit.
  reqFIFO.enq.bits := Mux(refilling, reqAltInput, io.missCache.bits)

  reqFIFO.deq.ready := (fifoNotEmpty && refilling)

  reqAltInput := reqFIFO.deq.bits

  when(hitFIFO && refilling) {
    reqAltInput.hitLevel := refillReg.level
    reqAltInput.hitAddr  := refillReg.refillData.getPPN
  }

  val retPermOffset = Mux1H(Seq( // different level use different offset vpnnLen
    refillReg.level(3) -> reqFIFO.deq.bits.pa(mptOff - offLen - 1 + vpnnLen * 3, 0 + vpnnLen * 3), // 30:27
    refillReg.level(2) -> reqFIFO.deq.bits.pa(mptOff - offLen - 1 + vpnnLen * 2, 0 + vpnnLen * 2), // 21:18
    refillReg.level(1) -> reqFIFO.deq.bits.pa(mptOff - offLen - 1 + vpnnLen, 0 + vpnnLen),         // 12:9
    refillReg.level(0) -> reqFIFO.deq.bits.pa(mptOff - offLen - 1, 0)
  ) // 3:0
  )
  io.resp.bits.perm := refillReg.refillData.extractPerm(retPermOffset)
  // extractPerm is a function in MptData, extract perm from refillData, offset is the offset of the perm in refillData.data
  io.resp.valid := fifoNotEmpty && hitFIFO && refilling && (refillReg.isAf || refillReg.isLeafMpte)
  // refillCounter.orR means we are refilling, FIFO not empty means we have a request to respond
  io.resp.bits.accessFault := refillReg.isAf || ((!io.resp.bits.perm(0)) && io.resp.bits.perm(1))
  // not read but write is af // access fault, no need to refill, just return the fault
  io.resp.bits.mptLevel := OHToUInt(refillReg.level) // mptLevelLenOH.W
  io.resp.bits.reqPA := reqFIFO.deq.bits.pa // reqPA is the PA of the request, used to generate the refill address
  io.resp.bits.source  := reqFIFO.deq.bits.source // source is the source
  io.resp.bits.mptOnly := reqFIFO.deq.bits.mptOnly && io.resp.valid

  val permsAsVec = Wire(Vec(16, UInt(3.W))) // perm xwr bits, total 16 xwrs in one mpte
  for (i <- 0 until 16) { permsAsVec(i) := refillReg.refillData.data(2 + i * 3, i * 3) }
  val permsEqual = Wire(Vec(16 - 1, Bool()))
  for (i <- 0 until 16 - 1) { permsEqual(i) := permsAsVec(i + 1) === permsAsVec(i) } // delay 1XNOR + 2 and gates = 3
  val leftPermsAllEqual  = permsEqual.slice(0, 7).reduce(_ && _)  // = permsEqual(6,0), delay 3
  val rightPermsAllEqual = permsEqual.slice(8, 15).reduce(_ && _) // = permsEqual(14,8)
  // OH (refillReg.level === 1.U(4.W)) is (refillReg.level(0))
  io.resp.bits.permIsNAPOT := (refillReg.level(0)) && leftPermsAllEqual && rightPermsAllEqual && permsEqual(
    7
  ) // Delay 2
  io.resp.bits.PermTlbCompress := (refillReg.level(0)) && Mux(
    reqFIFO.deq.bits.pa(mptOff - offLen - 1, 0) < 8.U,
    leftPermsAllEqual,
    rightPermsAllEqual
  ) // delay 3

  io.twReq.bits.hitAddr  := reqFIFO.deq.bits.hitAddr
  io.twReq.bits.reqPA    := reqFIFO.deq.bits.pa(ppnLen - 1, mptOff - offLen)
  io.twReq.bits.hitLevel := reqFIFO.deq.bits.hitLevel
  io.twReq.valid         := fifoNotEmpty && !refilling && io.twReq.ready

  reqFIFO.flush.get := flush
  when(flush) {
    refillReg := 0.U.asTypeOf(new RefillBundle())
    refillCounter := 0.U(4.W)
  }
}

class MptTableWalkerIO(implicit p: Parameters) extends MMUIOBaseBundle with MPTCacheParam {
  val req = Flipped(DecoupledIO(new MptMissQueueToTWReqBundle()))

  // val resp = DecoupledIO(new TWtoMptMissQueueRespBundle())

  val mem = new Bundle {
    val req  = DecoupledIO(new Bundle { val addr = UInt(PAddrBits.W) })
    val resp = Flipped(ValidIO(UInt(XLEN.W)))
    // val mask = Input(Bool()) dont need？
  }

  val pmp = new Bundle {
    val req  = ValidIO(new PMPReqBundle())
    val resp = Flipped(new PMPRespBundle())
  }
  val refill = ValidIO(new RefillBundle())
}

class MPTTableWalker(implicit p: Parameters) extends XSModule with MPTCacheParam {
  val io  = IO(new MptTableWalkerIO)
  val mem = io.mem

  io.pmp.req.bits.size := 3.U
  io.pmp.req.bits.cmd  := TlbCmd.read

  val flush = io.sfence.valid || io.csr.mmpt.changed

  val pa = RegEnable(io.req.bits.reqPA, 0.U, io.req.fire)
  // Store the received request PA in a register, used to generate the PN1/2/3 offsets for synthesizing the table walk address.
  io.refill.bits.pa := pa
  // define level reg
  val setLevel          = Wire(Bool())
  val setPmpCheckLevel  = Wire(Bool())
  val nextLevel         = Wire(UInt(mptLevelLenOH.W))
  val nextPmpCheckLevel = Wire(UInt(mptLevelLenOH.W))

  val level         = RegEnable(nextLevel, "b1000".U(mptLevelLenOH.W), setLevel)
  val pmpCheckLevel = RegEnable(nextPmpCheckLevel, "b1000".U(mptLevelLenOH.W), setPmpCheckLevel)

  val mpteResp = Wire(new MptEntry())
  if (HasMptCheckDefault) {
    mpteResp.genFake(level)
  } else {
    mpteResp.apply(mem.resp.bits) // mem mpte mpteData
  }

  val mpteData = Reg(new MptData())
  // Stores the returned permissions/lower-level address, or the incoming request address entry;
  // used to return permissions or synthesize the lower-level page table address with PA.
  io.refill.bits.refillData := mpteData // output alloc
  val isLeafMpte = RegEnable(mpteResp.isLeaf, false.B, io.mem.resp.valid)
  io.refill.bits.isLeafMpte := isLeafMpte // tell cache if the current refill is leaf node
  val mpteInvalid = RegEnable(!mpteResp.isValid, false.B, io.mem.resp.valid) // 1 level not on top of mem.resp
  val rsvZeroError0 = RegEnable(mem.resp.bits(9, 2).orR, false.B, io.mem.resp.valid)
  // max 3 level or gate on top of mem.resp，NON ZERO error of mtpe
  val rsvZeroError1 = RegEnable(mem.resp.bits(62, 58).orR, false.B, io.mem.resp.valid)
  val rsvZeroError2 = false.B
  when(io.req.fire) {
    mpteData.apply(io.req.bits.hitAddr)
  }.elsewhen(io.mem.resp.valid) {
    mpteData := mpteResp.data
  }

  val pn = Wire(UInt(9.W))
  pn := Mux1H(Seq(
    level(3) -> Cat(0.U(4.W), pa(47 - mptOff, 43 - mptOff)),
    level(2) -> pa(42 - mptOff, 34 - mptOff),
    level(1) -> pa(33 - mptOff, 25 - mptOff),
    level(0) -> pa(24 - mptOff, 16 - mptOff)
  ))

  // 3 layers of gate logic select the coresponding PN[i] based on cur level, just a onehot mux
  val memAddr = mpteData.getAddr(pn) // gen addr 0 delay
  io.mem.req.bits.addr := memAddr
  io.pmp.req.valid := DontCare
  io.pmp.req.bits.addr := Mux(io.mem.resp.valid, mpteResp.getAddr(pn), memAddr)
  // should be safer than just := memAddr

  // accessFault logic
  val pmpFail = if (HasMptCheckDefault) false.B else (!isLeafMpte) && (io.pmp.resp.ld || io.pmp.resp.mmio)
  // PMP delay unknown
  val entryError = if (HasMptCheckDefault) false.B else
    mpteInvalid || rsvZeroError0 || rsvZeroError1 || rsvZeroError2 || ((!isLeafMpte) && level === 1.U)
  // level == 0 non leaf, zero = / = 0,pmp fail,invalid casue accessFault
  val accessFault = entryError || pmpFail // pmp fail also cause accessFault
  io.refill.bits.level := Mux(pmpFail, pmpCheckLevel, level) // pmpFail return next level,else cur level
  io.refill.bits.isAf  := accessFault
  // pmp fail will not be recorded(if the root addr+ pn[i] cause pmp fail does not necessarily mean that
  // the root addr + other offset will cause pmp fail, so entry will be refilled as a normal intermidiate node)

  // // // FSM // // //
  object mystate extends ChiselEnum {
    val s_idle, s_mem_req, s_mem_resp, s_addr_proc = Value
  }
  import mystate._
  val curState  = RegInit(s_idle)
  val nextState = WireDefault(s_idle)
  curState := nextState // 2 proc FSM, no need to specify the flush, it is global
  // fsm start
  mem.req.valid     := false.B
  io.req.ready      := false.B
  nextState         := curState
  setLevel          := false.B
  setPmpCheckLevel  := false.B
  nextLevel         := level >> 1.U // onehotcounter-1 no aflevel
  nextPmpCheckLevel := pmpCheckLevel >> 1.U
  io.refill.valid   := false.B
  // default val
  switch(curState) {
    is(s_idle) {
      io.req.ready := true.B
      when(io.req.fire) {
        setPmpCheckLevel  := true.B
        nextLevel         := io.req.bits.hitLevel
        nextPmpCheckLevel := io.req.bits.hitLevel
        nextState         := s_mem_req // to mem req if fire
        setLevel          := true.B
      }
    }
    is(s_mem_req) {
      mem.req.valid := true.B // req valid when not fire

      when(io.mem.req.fire) { // just waiting, timing safe
        nextState := s_mem_resp // to wait resp
      }.otherwise {}
    }
    is(s_mem_resp) {             // unknown in delay,timing?
      when(io.mem.resp.valid) { // do nothing,delay one cycle OPTPOINT*
        nextState        := s_addr_proc
        setPmpCheckLevel := true.B
      }
    }
    is(s_addr_proc) { // known delay
      // process return
      when(accessFault || isLeafMpte) { // out delay unknown
        // when(isLeafMpte) {io.refill.valid := true.B}
        io.refill.valid := true.B
        nextState       := s_idle // OPTPOINT*
      }.otherwise {
        io.refill.valid := true.B   // start refill
        setLevel        := true.B
        nextState       := s_mem_req // OPTPOINT*
      }
    }

  }
  when(flush){
    pa := 0.U
    level := "b1000".U(mptLevelLenOH.W)
    pmpCheckLevel := "b1000".U(mptLevelLenOH.W)
    isLeafMpte := false.B
    mpteInvalid := false.B
    rsvZeroError0 := false.B
    rsvZeroError1 := false.B
    curState := s_idle
  }
  // fsm end
}

class MptCheckerIO(implicit p: Parameters) extends MMUIOBaseBundle with HasPtwConst {
  val mem = new Bundle {
    val req  = DecoupledIO(new L2TlbMemReqBundle())
    val resp = Flipped(ValidIO(UInt(XLEN.W)))
    val mask = Input(Bool()) // mask bit
  }
  val req  = Flipped(DecoupledIO(new MptReqBundle()))
  val resp = ValidIO(new MptRespBundle())

  val pmp = new Bundle {
    val req  = ValidIO(new PMPReqBundle())
    val resp = Flipped(new PMPRespBundle())
  }
}

class MptChecker(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new MptCheckerIO)
  io.mem.req.bits.hptw_bypassed := true.B // never refill to page cache
  io.mem.req.bits.id            := mptcMemReqID.U(bMemID.W)
  val mptCacheInst     = Module(new MPTCache()).io
  val mptTWInst        = Module(new MPTTableWalker()).io
  val mptMissQueueInst = Module(new MptMissQueue()).io

  mptCacheInst.csr <> io.csr
  mptCacheInst.sfence <> io.sfence

  mptTWInst.csr <> io.csr
  mptTWInst.sfence <> io.sfence

  mptMissQueueInst.csr <> io.csr
  mptMissQueueInst.sfence <> io.sfence

  mptCacheInst.req.bits.mptOnly := io.req.bits.mptOnly // need some fix
  mptCacheInst.req.bits.reqPA   := io.req.bits.reqPA
  mptCacheInst.req.bits.source  := io.req.bits.id
  mptCacheInst.req.valid        := io.req.valid
  io.req.ready                  := mptCacheInst.req.ready

  val mptReturn = Wire(new MptRespBundle())
  mptReturn.mptPerm := Mux(mptMissQueueInst.resp.valid, mptMissQueueInst.resp.bits.perm, mptCacheInst.respHit.bits.perm)
  mptReturn.contigousPerm := Mux(
    mptMissQueueInst.resp.valid,
    mptMissQueueInst.resp.bits.PermTlbCompress,
    mptCacheInst.respHit.bits.tlbContigousPerm
  )
  mptReturn.id := Mux(mptMissQueueInst.resp.valid, mptMissQueueInst.resp.bits.source, mptCacheInst.respHit.bits.source)
  mptReturn.mptLevel := Mux(
    mptMissQueueInst.resp.valid,
    mptMissQueueInst.resp.bits.mptLevel,
    mptCacheInst.respHit.bits.mptLevel
  )
  mptReturn.mptOnly := Mux(
    mptMissQueueInst.resp.valid,
    mptMissQueueInst.resp.bits.mptOnly,
    mptCacheInst.respHit.bits.mptOnly
  )
  mptReturn.reqPA := Mux(mptMissQueueInst.resp.valid, mptMissQueueInst.resp.bits.reqPA, mptCacheInst.respHit.bits.reqPA)
  mptReturn.accessFault := Mux(
    mptMissQueueInst.resp.valid,
    mptMissQueueInst.resp.bits.accessFault,
    mptCacheInst.respHit.bits.accessFault
  )
  mptReturn.permIsNAPOT := Mux(
    mptMissQueueInst.resp.valid,
    mptMissQueueInst.resp.bits.permIsNAPOT,
    mptCacheInst.respHit.bits.permIsNAPOT
  )

  io.resp.valid := mptMissQueueInst.resp.valid || mptCacheInst.respHit.valid
  io.resp.bits <> mptReturn

  mptMissQueueInst.refill <> mptTWInst.refill
  // cache miss send MptMissQueue
  mptMissQueueInst.missCache.bits.mptOnly  := mptCacheInst.respMiss.bits.mptOnly
  mptMissQueueInst.missCache.bits.hitLevel := mptCacheInst.respMiss.bits.hitLevel
  mptMissQueueInst.missCache.bits.hitAddr  := mptCacheInst.respMiss.bits.ppn
  mptMissQueueInst.missCache.bits.source   := mptCacheInst.respMiss.bits.source
  mptMissQueueInst.missCache.bits.pa       := mptCacheInst.respMiss.bits.pa
  mptMissQueueInst.missCache.valid         := mptCacheInst.respMiss.valid
  mptCacheInst.respMiss.ready              := mptMissQueueInst.missCache.ready
  // cache refill io
  mptCacheInst.refill <> mptTWInst.refill
  // MptMissQueue-twio
  mptTWInst.req <> mptMissQueueInst.twReq
  // tw-MptMissQueueio
  // table walk read ram
  io.mem.req.bits.addr    := mptTWInst.mem.req.bits.addr
  io.mem.req.valid        := mptTWInst.mem.req.valid
  mptTWInst.mem.req.ready := io.mem.req.ready

  mptTWInst.mem.resp.bits  := io.mem.resp.bits
  mptTWInst.mem.resp.valid := io.mem.resp.valid
  // mptTWInst.mem.resp.ready := true.B
  // PMP IO
  mptTWInst.pmp.resp <> io.pmp.resp
  io.pmp.req <> mptTWInst.pmp.req

  val mptDiabledFakedRespValid = RegInit(false.B)
  val mptDiabledFakedMptOnly   = RegInit(false.B)
  when(io.csr.mmpt.mode === 0.U) {
    mptCacheInst.req.valid := false.B // mptmode  return 111

    io.req.ready := true.B
    when(io.req.fire) {
      mptDiabledFakedRespValid := true.B
      mptDiabledFakedMptOnly   := io.req.bits.mptOnly
    }
    io.resp.valid        := mptDiabledFakedRespValid
    io.resp.bits.mptOnly := mptDiabledFakedMptOnly
    when(io.resp.fire) {
      mptDiabledFakedRespValid := false.B
      mptDiabledFakedMptOnly   := false.B
    }
  }
}