// Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package utils

import chisel3._

/** Utility class to create a duplicated signal. Used to control fan-out.
 *
 * @example {{{
 * class GCD extends Module {
 *   val io = IO(new Bundle {
 *     val value1        = Input(UInt(16.W))
 *     val value2        = Input(UInt(16.W))
 *     val loadingValues = Input(Bool())
 *     val outputGCD     = Output(Duplicate(2, UInt(16.W)))
 *     val outputValid   = Output(Bool())
 *   })
 *
 *   // create a duplicated register, 1 duplicate for inner logic, 2 duplicates for output
 *   val x  = Reg(Duplicate(Map("inner" -> 1, "output" -> 2), UInt(16.W)))
 *   val y  = Reg(UInt())
 *
 *   when(x.head > y) {
 *     x := x.head - y // use overloaded `:=` to broadcast result to all duplicates
 *   }.otherwise {
 *     y := y - x.head
 *   }
 *   // here, x.head is equivalent to:
 *   // - x(0)
 *   // - x.get("inner_0")
 *
 *   when(io.loadingValues) {
 *     x := io.value1
 *     y := io.value2
 *   }
 *
 *   io.outputGCD := x.group("output")
 *   // here, x.group("output") is equivalent to:
 *   // - x(1, 2)
 *   // - x.get("output_0", "output_1")
 *   // - x.tail
 *   io.outputValid := y === 0.U
 * }
 * /** verilog
 *  * module GCD(
 *  *   input         clock,
 *  *                 reset,
 *  *   input  [15:0] io_value1,
 *  *                 io_value2,
 *  *   input         io_loadingValues,
 *  *   output [15:0] io_outputGCD_dup_0,
 *  *                 io_outputGCD_dup_1,
 *  *   output        io_outputValid
 *  * );
 *  *
 *  *   reg [15:0] x_dup_0;
 *  *   reg [15:0] x_dup_1;
 *  *   reg [15:0] x_dup_2;
 *  *   reg [15:0] y;
 *  *   always @(posedge clock) begin
 *  *     if (io_loadingValues) begin
 *  *       x_dup_0 <= io_value1;
 *  *       x_dup_1 <= io_value1;
 *  *       x_dup_2 <= io_value1;
 *  *       y <= io_value2;
 *  *     end
 *  *     else if (x_dup_0 > y) begin
 *  *       automatic logic [15:0] _GEN = x_dup_0 - y;
 *  *       x_dup_0 <= _GEN;
 *  *       x_dup_1 <= _GEN;
 *  *       x_dup_2 <= _GEN;
 *  *     end
 *  *     else
 *  *       y <= y - x_dup_0;
 *  *   end // always @(posedge)
 *  *   assign io_outputGCD_dup_0 = x_dup_1;
 *  *   assign io_outputGCD_dup_1 = x_dup_2;
 *  *   assign io_outputValid = y == 16'h0;
 *  * endmodule
 *  */
 * }}}
 */
class Duplicate[T <: Data](
    names: Seq[String],
    gen:   T
) extends Bundle {
  private val n = names.length
  require(n > 0, "Duplicate should not be empty")

  val dup: Vec[T] = Vec(n, gen)

  private var next: Int = 0

  def apply(index: Int): T = this.dup(index)

  def apply(indexes: Int*): Duplicate[T] = {
    val dups  = indexes.map(i => this.dup(i))
    val names = indexes.map(i => this.names(i))
    DuplicateInit(names, dups)
  }

  def get(name: String): T = {
    val index = this.names.indexOf(name)
    require(index >= 0, s"Name $name not found in names: ${this.names.mkString(", ")}")
    apply(index)
  }

  def get(names: String*): Duplicate[T] = {
    val dups = names.map(get) // call get(name: String)
    DuplicateInit(names, dups)
  }

  def getNext: T = {
    val ret = dup(next)
    next = (next + 1) % n
    ret
  }

  def group(name: String): Duplicate[T] = {
    val selected = (this.dup zip this.names).map { case (d, n) =>
      if (n.startsWith(name)) (d, n) else null
    }.filter(_ != null)
    val dups  = selected.map(_._1)
    val names = selected.map(_._2)
    require(names.nonEmpty, s"Group $name not found in names: ${this.names.mkString(", ")}")
    DuplicateInit(names, dups)
  }

  def head: T = dup.head

  def tail: Duplicate[T] = DuplicateInit(names.tail, this.dup.tail)

  def init: Duplicate[T] = DuplicateInit(names.init, this.dup.init)

  def last: T = dup.last

  def :=(source: T): Unit = dup.foreach(_ := source)
}

object Duplicate {
  def apply[T <: Data](
      n:    Int,
      data: T
  ): Duplicate[T] = {
    val names = (0 until n).map(i => s"dup$i")
    new Duplicate[T](names, data)
  }

  def apply[T <: Data](
      names: Seq[String],
      data:  T
  ): Duplicate[T] =
    new Duplicate[T](names, data)

  def apply[T <: Data](
      groups: Map[String, Int],
      data:   T
  ): Duplicate[T] = {
    val names = groups.keys.flatMap { name =>
      val n = groups(name)
      (0 until n).map(i => s"${name}_${i}")
    }.toSeq
    new Duplicate[T](names, data)
  }
}

object DuplicateInit {
  def apply[T <: Data](
      n:    Int,
      data: T
  ): Duplicate[T] = {
    val dup = Wire(Duplicate[T](n, data.cloneType))
    dup := data
    dup
  }

  def apply[T <: Data](
      names: Seq[String],
      data:  T
  ): Duplicate[T] = {
    val dup = Wire(Duplicate[T](names, data.cloneType))
    dup := data
    dup
  }

  def apply[T <: Data](
      groups: Map[String, Int],
      data:   T
  ): Duplicate[T] = {
    val dup = Wire(Duplicate[T](groups, data.cloneType))
    dup := data
    dup
  }

  def apply[T <: Data](
      datas: Seq[T]
  ): Duplicate[T] = {
    val dup = Wire(Duplicate[T](datas.length, datas.head.cloneType))
    (dup.dup zip datas).foreach { case (dup, source) =>
      dup := source
    }
    dup
  }

  def apply[T <: Data](
      names: Seq[String],
      datas: Seq[T]
  ): Duplicate[T] = {
    require(names.length == datas.length, "names and datas should have the same length")
    val dup = Wire(Duplicate[T](names, datas.head.cloneType))
    (dup.dup zip datas).foreach { case (dup, source) =>
      dup := source
    }
    dup
  }
}
