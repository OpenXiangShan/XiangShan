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

import chisel3._
import chisel3.util._
import yunsuan.vector._

class Mgu extends  Module {
  val io = IO(new MguIO)

  val in = io.in
  val out = io.out
  val info = in.info

  val eewOH = SewOH(info.eew)
  val tail = TailGen(info.vl, info.uopIdx, eewOH, false.B)
  val prestart = PrestartGen(info.vstart, info.uopIdx, eewOH, false.B)
  val mask = MaskExtract(in.mask, info.uopIdx, eewOH)
  val vstart_gte_vl = info.vstart >= info.vl
  val tailReorg = MaskReorg.splash(tail, eewOH)
  val prestartReorg = MaskReorg.splash(prestart, eewOH)
  val maskReorg = MaskReorg.splash(mask, eewOH)
  val updateType = Wire(Vec(16, UInt(2.W))) // 00: keep result  10: old_vd  11: write 1s
  for(i <- 0 until 16) {
    when(prestartReorg(i) || vstart_gte_vl) {
      updateType(i) := 2.U
    }.elsewhen(tailReorg(i)) {
      updateType(i) := Mux(info.ta, 3.U, 2.U)
    }.elsewhen(!info.vm && !maskReorg(i)) {
      updateType(i) := Mux(info.ma, 3.U, 2.U)
    }.otherwise {
      updateType(i) := 0.U
    }
  }
  val bitsKeep = Cat(updateType.map(x => Mux(x(1), 0.U(8.W), ~0.U(8.W))).reverse)
  val bitsReplace = Cat(updateType.zipWithIndex.map({ case (x, i) =>
    Mux(!x(1), 0.U(8.W), Mux(x(0), ~0.U(8.W), UIntSplit(in.old_src, 8)(i)))
  }).reverse)


  out.vd := in.src & bitsKeep | bitsReplace

}


class MguIO extends Bundle {
  val in = new Bundle {
    val src = Input(UInt(128.W))
    val old_src = Input(UInt(128.W))
    val mask = Input(UInt(128.W))
    val isSegment = Input(Bool())
    val info = Input(new VecInfo)
  }
  val out = new Bundle {
    val vd = Output(UInt(128.W))
  }
}

class VecInfo extends Bundle {
  val vm = Bool()
  val ta = Bool()
  val ma = Bool()
  val vl = UInt(8.W)
  val vstart = UInt(7.W)
  val eew = UInt(4.W)
  val uopIdx = UInt(5.W)                //TODO: uopIdx width need to be paramterized, be consistent with Bundles.DecodeInst.uopIdx
}

object VerilogMgu extends App {
  println("Generating the Mgu hardware")
  emitVerilog(new Mgu(), Array("--target-dir", "build/vifu"))
}