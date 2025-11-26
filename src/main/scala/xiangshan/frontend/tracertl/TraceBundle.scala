/** *************************************************************************************
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
 * ************************************************************************************* */
package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{XSBundle, XSModule}
import xiangshan.cache.HasL1CacheParameters

class TraceBundle(implicit p: Parameters) extends XSBundle
class TraceModule(implicit p: Parameters) extends XSModule

class TraceInstrInnerBundle(implicit p: Parameters) extends Bundle {
  def trtl = p(TraceRTLParamKey)

  val pcVA = UInt(trtl.TraceVAddrWidth.W)
  val pcPA = UInt(trtl.TracePAddrWidth.W)
  val memoryAddrVA = UInt(trtl.TraceVAddrWidth.W)
  val memoryAddrPA = UInt(trtl.TracePAddrWidth.W)
  val target = UInt(trtl.TraceVAddrWidth.W)
  val inst = UInt(trtl.TraceInstCodeWidth.W)
  val memoryType = UInt(4.W)
  val memorySize = UInt(4.W)
  val branchType = UInt(8.W)
  val branchTaken = UInt(8.W)
  val exception = UInt(8.W)

  val fastSimulation = UInt(8.W)
  val InstID = UInt(trtl.TraceInstIDWidth.W)
  val sbID = if (trtl.TraceRTLOnFPGA) Some(new SmallBufferPtr(trtl.TraceFpgaSmallBufferSize)) else None

  def isFastSim = fastSimulation(0) === 1.U
  def arthiSrc0 = memoryAddrVA
  def arthiSrc1 = memoryAddrPA
  def arthiSrcAt(i: Int): UInt = Seq(arthiSrc0, arthiSrc1)(i)
  def isTaken = branchTaken(0) === 1.U
  def isException = exception =/= 0.U
  // def isBranch = branchType =/= 0.U

  def seqPC = pcVA + Mux(inst(1, 0) === 0x3.U, 4.U, 2.U)
  def targeEqSeq = target === seqPC
  def nextPC = Mux((exception =/= 0.U) || branchTaken(0), target,
               Mux(inst(1,0) === 3.U, pcVA + 4.U, pcVA + 2.U))
  def nextEqSeq = nextPC === seqPC
}


object TraceInstrInnerBundle {
  def apply(pcVA: UInt, pcPA: UInt, memoryAddrVA: UInt, memoryAddrPA: UInt,
    target: UInt, inst: UInt, memoryType: UInt, memorySize: UInt,
    branchType: UInt, branchTaken: UInt,
    InstID: UInt)(implicit p: Parameters): TraceInstrInnerBundle = {

    def trtl = p(TraceRTLParamKey)

    val bundle = Wire(new TraceInstrInnerBundle)
    bundle.pcVA := pcVA
    bundle.pcPA := pcPA
    bundle.memoryAddrVA := memoryAddrVA
    bundle.memoryAddrPA := memoryAddrPA
    bundle.target := target
    bundle.inst := inst
    bundle.memoryType := memoryType
    bundle.memorySize := memorySize
    bundle.branchType := branchType
    bundle.branchTaken := branchTaken
    bundle.InstID := InstID
    bundle.sbID.map(_ := DontCare)
    bundle
  }

  def readRaw(raw: UInt)(implicit p: Parameters): TraceInstrInnerBundle = {
    val m = Wire(new TraceInstrInnerBundle)
    var offset = 0
    // m.getElements.reverse.foreach( elt => {
    m.getElements.foreach( elt => {
      val width = elt.getWidth
      elt := raw(offset + width - 1, offset)
      offset += width
    })
    assert(offset == raw.getWidth, s"ERROR in TraceInstrInnerBundle, fromOuterRaw offset not match, expect ${raw.getWidth}, got ${offset}")

    m
  }
}

object TraceInstType {
  def Compute   = "b000".U
  def Branch    = "b001".U
  def Load      = "b010".U
  def Store     = "b011".U
  def Exception = "b100".U
  def Other     = "b101".U

  def len     = 3
  def apply() = UInt(len.W)
}

class TraceInstrFpgaBundle(implicit p: Parameters) extends Bundle {
  def trtl = p(TraceRTLParamKey)

  val traceType = UInt(TraceInstType.len.W)
  val pcVA = UInt(trtl.TraceVAddrWidth.W)
  val pcPAWoOff = UInt((trtl.TracePAddrWidth-12).W)
  // memory addr va, target
  val op1 = UInt(trtl.TraceVAddrWidth.W)
  // memory addr pa, taken, exception
  val op2 = UInt((trtl.TracePAddrWidth-12).W)
  // TODO: move inst to memory to save bus width
  val inst = UInt(trtl.TraceInstCodeWidth.W)

  def pcPA = Cat(pcPAWoOff, pcVA(11,0))
  def memoryAddrVA = op1
  def memoryAddrPA = Cat(op2, memoryAddrVA(11,0))
  def target = op1
  def branchTaken = op2(0)
  def exception = Mux(traceType === TraceInstType.Exception, op2(7,0), 0.U)

  def isCompute = traceType === TraceInstType.Compute
  def isBranch = traceType === TraceInstType.Branch
  def isLoad = traceType === TraceInstType.Load
  def isStore = traceType === TraceInstType.Store
  def isException = traceType === TraceInstType.Exception
  def isUnknownType = traceType === TraceInstType.Other

  def nextPC = Mux(isException || (isBranch && branchTaken.asBool), target,
               Mux(inst(1,0) === 3.U, pcVA + 4.U, pcVA + 2.U))
  def toInnerBundle(instID: UInt): TraceInstrInnerBundle = {
    val inner = Wire(new TraceInstrInnerBundle)
    inner.pcVA := pcVA
    inner.pcPA := pcPA
    inner.memoryAddrVA := op1
    inner.memoryAddrPA := Cat(op2, memoryAddrVA(11,0))
    inner.target := target
    inner.inst := inst
    inner.memoryType := Mux1H(Seq(
      isLoad -> 1.U,
      isStore -> 2.U,
    ))
    inner.memorySize := 0.U
    inner.branchType := Mux(isBranch, 1.U, 0.U)
    inner.branchTaken := branchTaken && isBranch
    inner.exception := exception
    inner.fastSimulation := 0.U
    inner.InstID := instID
    inner.sbID.map(_ := DontCare)

    inner
  }
}

object TraceInstrFpgaBundle {

  def upAlign(num: Int, align: Int): Int = {
    ((num + align - 1) / align) * align
  }
  def alignWidth()(implicit p: Parameters) = upAlign((new TraceInstrFpgaBundle).getWidth, 8)

  def readRaw(rawBits: UInt, reverse: Boolean)(implicit p: Parameters): TraceInstrFpgaBundle = {
    val m = Wire(new TraceInstrFpgaBundle)
    var offset = 0
    val elements = if (reverse) m.getElements.reverse else m.getElements
    elements.foreach( elt => {
      val width = elt.getWidth
      elt := rawBits(offset + width - 1, offset)
      offset += width
    })
    assert(offset == rawBits.getWidth, s"ERROR in TraceInstrFpgaBundle, fromOuterRaw offset not match, expect ${rawBits.getWidth}, got ${offset}")

    m
  }
}


class TraceRecvInfo(implicit p: Parameters) extends TraceBundle {
  val instNum = UInt(log2Ceil(PredictWidth + 1).W)
}

class TraceDynaInfo(implicit p: Parameters) extends TraceBundle {
  val eliminateOoO = Bool() // no need OoO execution
}
class TraceRTLAXISIO(DATA_WIDTH: Int) extends Bundle {
  val tdata = Output(UInt(DATA_WIDTH.W))
  val tkeep = Output(UInt((DATA_WIDTH / 8).W))
  val tlast = Output(Bool())
  val tvalid = Output(Bool())
  val tready = Input(Bool())
}

// size must be multiple of 8bit(byte)
class TraceFPGACollectBundle(implicit p: Parameters) extends Bundle {
  val instNum = UInt(8.W)
  val pcVA = UInt(p(TraceRTLParamKey).TraceVAddrWidth.W)
  val padding = UInt(6.W)
}

object TraceFPGACollectBundle {
  def upAlign(num: Int, align: Int): Int = {
    ((num + align - 1) / align) * align
  }

  def alignWidth()(implicit p: Parameters) = upAlign((new TraceFPGACollectBundle).getWidth, 8)

  def apply(instNum: UInt, pcVA: UInt)(implicit p: Parameters): TraceFPGACollectBundle = {
    val bundle = Wire(new TraceFPGACollectBundle)
    bundle.instNum := instNum
    bundle.pcVA := pcVA
    bundle
  }
}