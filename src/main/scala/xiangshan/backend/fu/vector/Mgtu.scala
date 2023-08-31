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
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import top.{ArgParser, BaseConfig, DefaultConfig}
import xiangshan._
import xiangshan.backend.fu.vector.Bundles.{Vl}
import yunsuan.vector._

class Mgtu(vlen: Int)(implicit p: Parameters) extends  Module {
  val io = IO(new MgtuIO(vlen))

  val in = io.in
  val vd = in.vd
  val vl = in.vl

  /**
    * Mask destination tail elements are always treated as tail-agnostic, regardless of the setting of vta
    * 1. 1.U(vlen.W) << vl get a bit 1 followed by vl bits 0 (100...000)
    * 2. 1.U(vlen.W) << vl - 1.U(vlen.W) get a bits with vl bits 1 (11...111)
    * 3. ~((1.U(vlen.W) << vl) - 1.U(vlen.W)) get a bits with (vlen - vl) bits 1 followed by vl bits 0 (11...1100...000)
    * 4. vd | tailBit set the high (vlen - vl) bits of vd to 1 (11...11xx...xx)
    */
  private val tailBit = ~((1.U(vlen.W) << vl) - 1.U(vlen.W))

  io.out.vd := vd | tailBit
}


class MgtuIO(vlen: Int)(implicit p: Parameters) extends Bundle {
  val in = new Bundle {
    val vd = Input(UInt(vlen.W))
    val vl = Input(Vl())
  }
  val out = new Bundle {
    val vd = Output(UInt(vlen.W))
  }
}
