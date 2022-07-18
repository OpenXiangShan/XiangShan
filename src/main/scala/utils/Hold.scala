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

package utils

import chisel3._
import chisel3.util._

object HoldUnless {
  def apply[T <: Data](x: T, en: Bool): T = Mux(en, x, RegEnable(x, 0.U.asTypeOf(x), en))
}

object ReadAndHold {
  def apply[T <: Data](x: Mem[T], addr: UInt, en: Bool): T = HoldUnless(x.read(addr), en)
  def apply[T <: Data](x: SyncReadMem[T], addr: UInt, en: Bool): T = HoldUnless(x.read(addr, en), RegNext(en))
}

/*
 * Hold the in fire unless out fire or flush happens
 * similar to BoolStopWatch
 */
object ValidHold {
  def apply(infire: Bool, outfire: Bool, flush: Bool = false.B ) = {
    val valid = RegInit(false.B)
    when (outfire) { valid := false.B }
    when (infire) { valid := true.B }
    when (flush) { valid := false.B } // NOTE: the flush will flush in & out, is that ok?
    valid
  }
}


/*
 * Hold the 'fire' for only one cycle unless new fire comes in
 */
object OneCycleValid {
  def apply(fire: Bool, flush: Bool = false.B) = {
    val valid = RegInit(false.B)
    when (valid) { valid := false.B }
    when (fire) { valid := true.B }
    when (flush) { valid := false.B }
    valid
  }
}

/*
 * Hold the data when it is valid and bypass latest data
 */
object DataHoldBypass {
  def apply[T <: Data](data: T, valid: Bool): T = {
    Mux(valid, data, RegEnable(data, valid))
  }
}

/*
 * Data change or not
 */
object DataChanged {
  def apply(data: UInt): UInt = {
    data =/= RegNext(data)
  }
}

/**
  * Delay the data for N cycles
  */
class DelayN[T <: Data](gen: T, n: Int) extends Module {
  val io = IO(new Bundle() {
    val in = Input(gen)
    val out = Output(gen)
  })
  var out = io.in
  for (i <- 0 until n) {
    out = RegNext(out)
  }
  io.out := out
}

object DelayN {
  def apply[T <: Data](in: T, n: Int): T = {
    val delay = Module(new DelayN(in.cloneType, n))
    delay.io.in := in
    delay.io.out
  }
}
