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

import org.chipsalliance.cde.config.Parameters
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
  val vd = in.vd
  val oldVd = in.oldVd
  val narrow = io.in.info.narrow

  private val vdIdx = Mux(narrow, info.vdIdx(2, 1), info.vdIdx)

  private val maskTailGen = Module(new ByteMaskTailGen(vlen))

  private val eewOH = SewOH(info.eew).oneHot

  private val vstartMapVdIdx = elemIdxMapVdIdx(info.vstart)(2, 0) // 3bits 0~7
  private val vlMapVdIdx = elemIdxMapVdIdx(info.vl)(3, 0)         // 4bits 0~8
  private val uvlMax = numBytes.U >> info.eew
  private val uvlMaxForAssert = numBytes.U >> info.vsew
  private val vlMaxForAssert = Mux(io.in.info.vlmul(2), uvlMaxForAssert >> (-io.in.info.vlmul), uvlMaxForAssert << io.in.info.vlmul).asUInt

  private val maskDataVec: Vec[UInt] = VecDataToMaskDataVec(in.mask, info.eew)
  private val maskUsed = maskDataVec(vdIdx)

  maskTailGen.io.in.begin := Mux1H(Seq(
    (vstartMapVdIdx < vdIdx) -> 0.U,
    (vstartMapVdIdx === vdIdx) -> elemIdxMapUElemIdx(info.vstart),
    (vstartMapVdIdx > vdIdx) -> uvlMax,
  ))
  maskTailGen.io.in.end := Mux1H(Seq(
    (vlMapVdIdx < vdIdx) -> 0.U,
    (vlMapVdIdx === vdIdx) -> elemIdxMapUElemIdx(info.vl),
    (vlMapVdIdx > vdIdx) -> uvlMax,
  ))
  maskTailGen.io.in.vma := info.ma
  maskTailGen.io.in.vta := info.ta
  maskTailGen.io.in.vsew := info.eew
  maskTailGen.io.in.maskUsed := maskUsed

  private val keepEn = maskTailGen.io.out.keepEn
  private val agnosticEn = maskTailGen.io.out.agnosticEn

  // the result of normal inst and narrow inst which does not need concat
  private val byte1s: UInt = (~0.U(8.W)).asUInt

  private val resVecByte = Wire(Vec(numBytes, UInt(8.W)))
  private val vdVecByte = vd.asTypeOf(resVecByte)
  private val oldVdVecByte = oldVd.asTypeOf(resVecByte)

  for (i <- 0 until numBytes) {
    resVecByte(i) := MuxCase(oldVdVecByte(i), Seq(
      keepEn(i) -> vdVecByte(i),
      agnosticEn(i) -> byte1s,
    ))
  }

  // the result of narrow inst which needs concat
  private val narrowNeedCat = info.vdIdx(0).asBool & narrow
  private val narrowResCat = Cat(resVecByte.asUInt(vlen / 2 - 1, 0), oldVd(vlen / 2 - 1, 0))

  // the result of mask-generating inst
  private val maxVdIdx = 8
  private val meaningfulBitsSeq = Seq(16, 8, 4, 2)
  private val allPossibleResBit = Wire(Vec(4, Vec(maxVdIdx, UInt(vlen.W))))
  private val catData = Mux(info.ta, ~0.U(vlen.W), oldVd)

  for (sew <- 0 to 3) {
    if (sew == 0) {
      allPossibleResBit(sew)(maxVdIdx - 1) := Cat(vd(meaningfulBitsSeq(sew) - 1, 0),
        oldVd(meaningfulBitsSeq(sew) * (maxVdIdx - 1) - 1, 0))
    } else {
      allPossibleResBit(sew)(maxVdIdx - 1) := Cat(catData(vlen - 1, meaningfulBitsSeq(sew) * maxVdIdx),
        vd(meaningfulBitsSeq(sew) - 1, 0), oldVd(meaningfulBitsSeq(sew) * (maxVdIdx - 1) - 1, 0))
    }
    for (i <- 1 until maxVdIdx - 1) {
      allPossibleResBit(sew)(i) := Cat(catData(vlen - 1, meaningfulBitsSeq(sew) * (i + 1)),
        vd(meaningfulBitsSeq(sew) - 1, 0), oldVd(meaningfulBitsSeq(sew) * i - 1, 0))
    }
    allPossibleResBit(sew)(0) := Cat(catData(vlen - 1, meaningfulBitsSeq(sew)), vd(meaningfulBitsSeq(sew) - 1, 0))
  }

  private val resVecBit = allPossibleResBit(info.eew)(vdIdx)

  io.out.vd := MuxCase(resVecByte.asUInt, Seq(
    info.dstMask -> resVecBit.asUInt,
    narrowNeedCat -> narrowResCat,
  ))
  io.out.keep := keepEn
  io.out.illegal := (info.vl > vlMaxForAssert) && info.valid

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
    val keep = Output(UInt((vlen / 8).W))
    val illegal = Output(Bool())
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
  val vsew = VSew()
  val vdIdx = UInt(3.W) // 0~7
  val vlmul = UInt(3.W)
  val valid = Bool()
  val narrow = Bool()
  val dstMask = Bool()
}

object VerilogMgu extends App {
  println("Generating the Mgu hardware")
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)
  val p = config.alterPartial({case XSCoreParamsKey => config(XSTileKey).head})

  emitVerilog(new Mgu(128)(p), Array("--target-dir", "build/vifu", "--full-stacktrace"))
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