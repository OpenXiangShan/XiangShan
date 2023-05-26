/****************************************************************************************
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
 ****************************************************************************************
 */


package xiangshan.backend.fu.vector

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import top.{ArgParser, BaseConfig, DefaultConfig}
import xiangshan._
import xiangshan.backend.fu.vector.Bundles.{VSew, Vl}
import xiangshan.backend.fu.vector.Utils.VecDataToMaskDataVec
import yunsuan.vector._

class Mgu(vlen: Int)(implicit p: Parameters) extends  Module {
  private val numBytes = vlen / 8
  private val byteWidth = log2Up(numBytes)

  val io = IO(new MguIO(vlen))

  val in = io.in
  val out = io.out
  val info = in.info

  private val maskTailGen = Module(new ByteMaskTailGen(vlen))

  private val eewOH = SewOH(info.eew).oneHot

  private val vstartMapVdIdx = elemIdxMapVdIdx(info.vstart)(2, 0) // 3bits 0~7
  private val vlMapVdIdx = elemIdxMapVdIdx(info.vl)(3, 0)         // 4bits 0~8
  private val uvlMax = numBytes.U >> info.eew
  private val maskDataVec: Vec[UInt] = VecDataToMaskDataVec(in.mask, info.eew)
  private val maskUsed = maskDataVec(info.vdIdx)

  maskTailGen.io.in.begin := Mux1H(Seq(
    (vstartMapVdIdx < info.vdIdx) -> 0.U,
    (vstartMapVdIdx === info.vdIdx) -> elemIdxMapUElemIdx(info.vstart),
    (vstartMapVdIdx > info.vdIdx) -> uvlMax,
  ))
  maskTailGen.io.in.end := Mux1H(Seq(
    (vlMapVdIdx < info.vdIdx) -> 0.U,
    (vlMapVdIdx === info.vdIdx) -> elemIdxMapUElemIdx(info.vl),
    (vlMapVdIdx > info.vdIdx) -> uvlMax,
  ))
  maskTailGen.io.in.vma := info.ma
  maskTailGen.io.in.vta := info.ta
  maskTailGen.io.in.vsew := info.eew
  maskTailGen.io.in.maskUsed := maskUsed

  private val keepEn = maskTailGen.io.out.keepEn
  private val agnosticEn = maskTailGen.io.out.agnosticEn

  private val byte1s: UInt = (~0.U(8.W)).asUInt

  private val resVec = Wire(Vec(numBytes, UInt(8.W)))
  private val vdVec = io.in.vd.asTypeOf(resVec)
  private val oldVdVec = io.in.oldVd.asTypeOf(resVec)

  for (i <- 0 until numBytes) {
    resVec(i) := MuxCase(oldVdVec(i), Seq(
      keepEn(i) -> vdVec(i),
      agnosticEn(i) -> byte1s,
    ))
  }

  io.out.vd := resVec.asUInt

  io.debugOnly.vstartMapVdIdx := vstartMapVdIdx
  io.debugOnly.vlMapVdIdx := vlMapVdIdx
  io.debugOnly.begin := maskTailGen.io.in.begin
  io.debugOnly.end := maskTailGen.io.in.end
  io.debugOnly.keepEn := keepEn
  io.debugOnly.agnosticEn := agnosticEn
  def elemIdxMapVdIdx(elemIdx: UInt) = {
    require(elemIdx.getWidth >= log2Up(vlen))
    // 3 = log2(8)
    Mux1H(eewOH, Seq.tabulate(eewOH.getWidth)(x => elemIdx(byteWidth - x + 3, byteWidth - x)))
  }

  def elemIdxMapUElemIdx(elemIdx: UInt) = {
    Mux1H(eewOH, Seq.tabulate(eewOH.getWidth)(x => elemIdx(byteWidth - x - 1, 0)))
  }
}


class MguIO(vlen: Int)(implicit p: Parameters) extends Bundle {
  val in = new Bundle {
    val vd = Input(UInt(vlen.W))
    val oldVd = Input(UInt(vlen.W))
    val mask = Input(UInt(vlen.W))
    val info = Input(new VecInfo)
  }
  val out = new Bundle {
    val vd = Output(UInt(vlen.W))
  }
  val debugOnly = Output(new Bundle {
    val vstartMapVdIdx = UInt()
    val vlMapVdIdx = UInt()
    val begin = UInt()
    val end = UInt()
    val keepEn = UInt()
    val agnosticEn = UInt()
  })
}

class VecInfo(implicit p: Parameters) extends Bundle {
  val ta = Bool()
  val ma = Bool()
  val vl = Vl()
  val vstart = Vl()
  val eew = VSew()
  val vdIdx = UInt(3.W) // 0~7
}

object VerilogMgu extends App {
  println("Generating the Mgu hardware")
  val (config, firrtlOpts, firrtlComplier, firtoolOpts) = ArgParser.parse(args)
  val p = config.alterPartial({case XSCoreParamsKey => config(XSTileKey).head})

  emitVerilog(new Mgu(128)(p), Array("--target-dir", "build/vifu"))
}

class MguTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  val defaultConfig = (new DefaultConfig).alterPartial({
    case XSCoreParamsKey => XSCoreParameters()
  })

  println("test start")

  behavior of "Mgu"
  it should "run" in {
    test(new Mgu(128)(defaultConfig)).withAnnotations(Seq(VerilatorBackendAnnotation)) {
      m: Mgu =>
        m.io.in.vd.poke("h8765_4321_8765_4321_8765_4321_8765_4321".U)
        m.io.in.oldVd.poke("h7777_7777_7777_7777_7777_7777_7777_7777".U)
        m.io.in.mask.poke("h0000_0000_0000_0000_0000_0000_ffff_0000".U)
        m.io.in.info.ta.poke(true.B)
        m.io.in.info.ma.poke(false.B)
        m.io.in.info.vl.poke((16 + 7).U)
        m.io.in.info.vstart.poke((16 + 2).U)
        m.io.in.info.eew.poke(VSew.e8)
        m.io.in.info.vdIdx.poke(1.U)

        println("out.vd: " + m.io.out.vd.peek().litValue.toString(16))
        println("debugOnly.vstartMapVdIdx: " + m.io.debugOnly.vstartMapVdIdx.peek().litValue.toString(16))
        println("debugOnly.vlMapVdIdx: "     + m.io.debugOnly.vlMapVdIdx.peek().litValue.toString(16))
        println("debugOnly.begin: "          + m.io.debugOnly.begin.peek().litValue)
        println("debugOnly.end: "            + m.io.debugOnly.end.peek().litValue)
        println("debugOnly.keepEn: "         + m.io.debugOnly.keepEn.peek().litValue.toString(2))
        println("debugOnly.agnosticEn: "     + m.io.debugOnly.agnosticEn.peek().litValue.toString(2))
    }
    println("test done")
  }
}