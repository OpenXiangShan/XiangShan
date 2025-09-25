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

trait TraceParams {
  val TraceInstrWidth = 32
  // val blockOffBits = log2Ceil(64)
}

class TraceBundle(implicit p: Parameters) extends XSBundle with TraceParams
class TraceModule(implicit p: Parameters) extends XSModule with TraceParams

class TraceInstrOuterBundle extends Bundle {
  val pcVA = UInt(64.W)
  val pcPA = UInt(64.W)
  val memoryAddrVA = UInt(64.W)
  val memoryAddrPA = UInt(64.W)
  val target = UInt(64.W)
  val inst = UInt(32.W)
  val memoryType = UInt(4.W)
  val memorySize = UInt(4.W)
  val branchType = UInt(8.W)
  val branchTaken = UInt(8.W)
  val exception = UInt(8.W)
  val InstID = UInt(64.W)


  def toInnerBundle: TraceInstrInnerBundle = {
    val inner = Wire(new TraceInstrInnerBundle)
    inner.elements.foreach { case (name, elt) =>
      require(this.elements.contains(name), s"[TraceInstrOuterBundle] Error: field $name not found in outer bundle")
      elt := this.elements(name)
    }
    inner
  }

  def nextPC = Mux((exception =/= 0.U) || branchTaken(0), target,
               Mux(inst(1,0) === 3.U, pcVA + 4.U, pcVA + 2.U))
}

object TraceInstrOuterBundle {

  // FIXME: asTypeOf would use the reverse order of fields
  def readRaw(outer: UInt): TraceInstrOuterBundle = {
    val m = Wire(new TraceInstrOuterBundle)
    var offset = 0
    m.getElements.reverse.foreach( elt => {
      val width = elt.getWidth
      elt := outer(offset + width - 1, offset)
      offset += width
    })
    assert(offset == outer.getWidth, s"ERROR in TraceInstrOuterBundle, fromOuterRaw offset not match, expect ${outer.getWidth}, got ${offset}")

    m
  }
}

class TraceInstrInnerBundle extends Bundle {
  def VAddrBits = 39
  def PAddrBits = 48
  def XLEN = 64
  def TraceInstrWidth = 32

  val pcVA = UInt(VAddrBits.W)
  val pcPA = UInt(PAddrBits.W)
  val memoryAddrVA = UInt(XLEN.W)
  val memoryAddrPA = UInt(XLEN.W)
  val target = UInt(VAddrBits.W)
  val inst = UInt(TraceInstrWidth.W)
  val memoryType = UInt(4.W)
  val memorySize = UInt(4.W)
  val branchType = UInt(8.W)
  val branchTaken = UInt(8.W)
  val exception = UInt(8.W)

  val InstID = UInt(64.W)

  def arthiSrc0 = memoryAddrVA
  def arthiSrc1 = memoryAddrPA
  def arthiSrcAt(i: Int): UInt = Seq(arthiSrc0, arthiSrc1)(i)
  def nextPC = Mux((exception =/= 0.U) || branchTaken(0), target,
               Mux(inst(1,0) === 3.U, pcVA + 4.U, pcVA + 2.U))
}


object TraceInstrInnerBundle {
  def apply(pcVA: UInt, pcPA: UInt, memoryAddrVA: UInt, memoryAddrPA: UInt,
    target: UInt, inst: UInt, memoryType: UInt, memorySize: UInt,
    branchType: UInt, branchTaken: UInt,
    InstID: UInt): TraceInstrInnerBundle = {

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
    bundle
  }

  def readRaw(raw: UInt): TraceInstrInnerBundle = {
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