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

/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
*
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
* FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package utils

import chisel3._
import chisel3.util._

class SRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt) = {
    this.setIdx := setIdx
    this
  }
}

class SRAMBundleAW[T <: Data](private val gen: T, set: Int, val way: Int = 1) extends SRAMBundleA(set) {
  val data = Output(Vec(way, gen))
  val waymask = if (way > 1) Some(Output(UInt(way.W))) else None

  def apply(data: Vec[T], setIdx: UInt, waymask: UInt): SRAMBundleAW[T] = {
    super.apply(setIdx)
    this.data := data
    this.waymask.map(_ := waymask)
    this
  }
  // this could only be used when waymask is onehot or nway is 1
  def apply(data: T, setIdx: UInt, waymask: UInt): SRAMBundleAW[T] = {
    apply(VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }
}

class SRAMBundleR[T <: Data](private val gen: T, val way: Int = 1) extends Bundle {
  val data = Output(Vec(way, gen))
}

class SRAMReadBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleA(set))
  val resp = Flipped(new SRAMBundleR(gen, way))

  def apply(valid: Bool, setIdx: UInt) = {
    this.req.bits.apply(setIdx)
    this.req.valid := valid
    this
  }
}

class SRAMWriteBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleAW(gen, set, way))

  def apply(valid: Bool, data: Vec[T], setIdx: UInt, waymask: UInt): SRAMWriteBus[T] = {
    this.req.bits.apply(data = data, setIdx = setIdx, waymask = waymask)
    this.req.valid := valid
    this
  }
  def apply(valid: Bool, data: T, setIdx: UInt, waymask: UInt): SRAMWriteBus[T] = {
    apply(valid, VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }
}

class SRAMRawIO[T <: Data](gen: T, set: Int, way: Int) extends Bundle {
  // WRITE
  val wen   = Input(Bool())
  val waddr = Input(UInt(log2Up(set).W))
  val wmask = Input(UInt(way.W))
  val wdata = Input(Vec(way, UInt(gen.getWidth.W)))
  // READ
  val ren   = Input(Bool())
  val raddr = Input(waddr.cloneType)
  val rdata = Output(wdata.cloneType)
}

class SRAMFuncIO[T <: Data](gen: T, set: Int, way: Int) extends Bundle {
  val r = Flipped(new SRAMReadBus(gen, set, way))
  val w = Flipped(new SRAMWriteBus(gen, set, way))
}

class SRAMFuncTemplate[T <: Data](
  gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false,
  bypassWrite: Boolean = false, debugHazardRdata: String = "rand+lastcycle") extends Module {
  val io = IO(new SRAMFuncIO(gen, set, way))
  val sram = IO(Flipped(new SRAMRawIO(gen, set, way)))

  val wordType = sram.wdata.head.cloneType

  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdata = Mux(resetState, 0.U.asTypeOf(Vec(way, gen)), io.w.req.bits.data)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  sram.wen   := wen
  sram.waddr := setIdx
  sram.wmask := waymask
  sram.wdata := VecInit(wdata.map(_.asTypeOf(wordType)))
  sram.ren   := ren
  sram.raddr := io.r.req.bits.setIdx
  val raw_rdata = sram.rdata

  // bypass for dual-port SRAMs
  require(!bypassWrite || bypassWrite && !singlePort)
  def need_bypass(wen: Bool, waddr: UInt, wmask: UInt, ren: Bool, raddr: UInt) : UInt = {
    val need_check = RegNext(ren && wen)
    val waddr_reg = RegNext(waddr)
    val raddr_reg = RegNext(raddr)
    require(wmask.getWidth == way)
    val bypass = Fill(way, need_check && waddr_reg === raddr_reg) & RegNext(wmask)
    bypass.asTypeOf(UInt(way.W))
  }
  val bypass_wdata = if (bypassWrite) VecInit(RegNext(io.w.req.bits.data).map(_.asTypeOf(wordType)))
    else VecInit((0 until way).map(_ => LFSR64().asTypeOf(wordType)))
  val bypass_mask = need_bypass(io.w.req.valid, io.w.req.bits.setIdx, io.w.req.bits.waymask.getOrElse("b1".U), io.r.req.valid, io.r.req.bits.setIdx)
  val debug_hazard_rdata = debugHazardRdata match {
    case "rand" => VecInit((0 until way).map(_ => LFSR64().asTypeOf(wordType)))
    case "lastcycle" => RegNext(raw_rdata)
    //"rand+lastcycle"
    case _ => Mux(LFSR64()(0), VecInit((0 until way).map(_ => LFSR64().asTypeOf(wordType))), RegNext(raw_rdata))
  }
  val mem_rdata = {
    if (singlePort) Mux(RegNext(io.w.req.valid, false.B), debug_hazard_rdata, raw_rdata)
    else VecInit(bypass_mask.asBools.zip(raw_rdata).zip(bypass_wdata).map {
      case ((m, r), w) => Mux(m, w, r)
    })
  }

  // hold read data for SRAMs
  val rdata = (if (holdRead) HoldUnless(mem_rdata, RegNext(realRen))
              else mem_rdata).map(_.asTypeOf(gen))

  io.r.resp.data := VecInit(rdata)
  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

}

class FoldedSRAMTemplate[T <: Data](gen: T, set: Int, width: Int = 4, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false, bypassWrite: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })
  //   |<----- setIdx ----->|
  //   | ridx | width | way |

  require(width > 0 && isPow2(width))
  require(way > 0 && isPow2(way))
  require(set % width == 0)

  val nRows = set / width

  val array = Module(new SRAMFuncTemplate(gen, set=nRows, way=width*way, shouldReset=shouldReset, holdRead=holdRead, singlePort=singlePort))
  val sram = Module(new SRAMWrapper(gen, nRows, width*way, singlePort))
  array.sram <> sram.io

  io.r.req.ready := array.io.r.req.ready
  io.w.req.ready := array.io.w.req.ready

  val raddr = io.r.req.bits.setIdx >> log2Ceil(width)
  val ridx = RegNext(if (width != 1) io.r.req.bits.setIdx(log2Ceil(width)-1, 0) else 0.U(1.W))
  val ren  = io.r.req.valid

  array.io.r.req.valid := ren
  array.io.r.req.bits.setIdx := raddr

  val rdata = array.io.r.resp.data
  for (w <- 0 until way) {
    val wayData = VecInit(rdata.indices.filter(_ % way == w).map(rdata(_)))
    io.r.resp.data(w) := Mux1H(UIntToOH(ridx, width), wayData)
  }

  val wen = io.w.req.valid
  val wdata = VecInit(Seq.fill(width)(io.w.req.bits.data).flatten)
  val waddr = io.w.req.bits.setIdx >> log2Ceil(width)
  val widthIdx = if (width != 1) io.w.req.bits.setIdx(log2Ceil(width)-1, 0) else 0.U
  val wmask = if (width*way != 1) VecInit(Seq.tabulate(width*way)(n => (n / way).U === widthIdx)).asUInt else 1.U(1.W)

  array.io.w.apply(wen, wdata, waddr, wmask)
}

class SRAMWrapper[T <: Data](gen: T, set: Int, way: Int = 1, singlePort: Boolean = false) extends Module {
  val io = IO(new SRAMRawIO(gen, set, way))

  val wordType = UInt(gen.getWidth.W)
  val array = SyncReadMem(set, Vec(way, wordType))
  array.suggestName("sram")
  when (io.wen) { array.write(io.waddr, io.wdata, io.wmask.asBools) }

  val ren = if (singlePort) io.ren && !io.wen else io.ren
  io.rdata := array.read(io.raddr, ren)

}

class SRAMTemplate[T <: Data](
  gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false,
  bypassWrite: Boolean = false, debugHazardRdata: String = "rand+lastcycle") extends Module {
  val io = IO(new SRAMFuncIO(gen, set, way))

  val func_if = Module(new SRAMFuncTemplate(gen, set, way, shouldReset, holdRead, singlePort, bypassWrite, debugHazardRdata))
  val sram = Module(new SRAMWrapper(gen, set, way, singlePort))

  io.r <> func_if.io.r
  io.w <> func_if.io.w
  func_if.sram <> sram.io
}

class SRAMTemplateWithMBIST[T <: Data](
  gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false,
  bypassWrite: Boolean = false, debugHazardRdata: String = "rand+lastcycle")
  extends SRAMTemplate(gen, set, way, shouldReset, holdRead, singlePort, bypassWrite, debugHazardRdata)
    with HasMBISTInterface {

  val mbist_if = Module(new MBIST2SRAM(gen, set, way, singlePort))
  override val mbistSlaves = Seq(mbist_if)
  connectMBIST()

  mbist_if.sram := DontCare
  // To simplify the design hierarchy, we instantiate the MUXes here.
  when (mbist_if.mbist.get.ack) {
    mbist_if.sram <> sram.io
  }
}

class FoldedSRAMTemplateWithMBIST[T <: Data](gen: T, set: Int, width: Int = 4, way: Int = 1,
                                    shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false, bypassWrite: Boolean = false)
  extends FoldedSRAMTemplate(gen, set, width, way, shouldReset, holdRead, singlePort, bypassWrite)
  with HasMBISTInterface {

  val mbist_if = Module(new MBIST2SRAM(gen, nRows, width*way, singlePort))
  override val mbistSlaves = Seq(mbist_if)
  connectMBIST()

  mbist_if.sram := DontCare
  // To simplify the design hierarchy, we instantiate the MUXes here.
  when (mbist_if.mbist.get.ack) {
    mbist_if.sram <> sram.io
  }
}