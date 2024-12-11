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

package device

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.RegField
import utils.{HasTLDump, XSDebug}

class TLTimer(address: Seq[AddressSet], sim: Boolean, numCores: Int)(implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice("clint", Seq("XiangShan", "clint"))
  val node = TLRegisterNode(address, device, beatBytes = 8)

  lazy val module = new LazyModuleImp(this) with HasTLDump {
    val io = IO(new Bundle() {
      val mtip = Output(Vec(numCores, Bool()))
      val msip = Output(Vec(numCores, Bool()))
    })

    val mtime = RegInit(0.U(64.W))  // unit: us
    val mtimecmp = Seq.fill(numCores)(RegInit(0.U(64.W)))
    val msip = Seq.fill(numCores)(RegInit(0.U(32.W)))

    val clk = (if (!sim) 1000000 /* 40MHz / 1000000 */ else 100)
    val freq = RegInit(clk.U(64.W))
    val inc = RegInit(1.U(64.W))

    val cnt = RegInit(0.U(64.W))
    val nextCnt = cnt + 1.U
    cnt := Mux(nextCnt < freq, nextCnt, 0.U)
    val tick = (nextCnt === freq)
    when (tick) { mtime := mtime + inc }

    var clintMapping = Seq(
      0x8000 -> RegField.bytes(freq),
      0x8008 -> RegField.bytes(inc),
      0xbff8 -> RegField.bytes(mtime))

    for (i <- 0 until numCores) {
      clintMapping = clintMapping ++ Seq(
        0x0000 + i*4 -> RegField.bytes(msip(i)),
        0x4000 + i*8 -> RegField.bytes(mtimecmp(i))
      )
    }

    node.regmap( mapping = clintMapping:_* )

    val in = node.in.head._1
    when(in.a.valid){
      XSDebug("[A] channel valid ready=%d ", in.a.ready)
      in.a.bits.dump
    }

    for (i <- 0 until numCores) {
      io.mtip(i) := RegNext(mtime >= mtimecmp(i))
      io.msip(i) := RegNext(msip(i) =/= 0.U)
    }
  }
}
