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

  /*
   * Mask destination tail elements are always treated as tail-agnostic, regardless of the setting of vta
   */
  private val vdWithTail = Wire(Vec(vlen, UInt(1.W)))
  vdWithTail.zipWithIndex.foreach{ case (bit, idx) =>
    bit := Mux(idx.U < vl, vd(idx), 1.U)
  }

  io.out.vd := vdWithTail.asUInt
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
