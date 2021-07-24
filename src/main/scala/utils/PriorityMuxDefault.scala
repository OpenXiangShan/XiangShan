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

object PriorityMuxDefault {
  def apply[T <: Data](in: Seq[(Bool, T)], default: T): T = {
    in.size match {
      case 1=>
        Mux(in.head._1, in.head._2, default)
      case _ =>
        Mux(in.head._1, in.head._2, PriorityMuxDefault(in.tail, default))
    }
  }
}

object PriorityEncoderDefault {
  def apply(in: Seq[Bool], default: UInt): UInt = {
    PriorityMuxDefault(in.zipWithIndex.map(x => x._1 -> x._2.U), default)
  }
}

object PriorityMuxWithFlag {
  def apply[T <: Data](in: Seq[(Bool, T)]): (T, Bool) = {
    in.size match {
      case 1 =>
        (in.head._2, in.head._1)
      case _ =>
        val (d_tail, f_tail) = PriorityMuxWithFlag(in.tail)
        val d_head = in.head._2
        val f_head = in.head._1
        (Mux(f_head, d_head, d_tail), f_head || f_tail)
    }
  }
}

object PriorityEncoderWithFlag {
  def apply(in: Seq[Bool]): (UInt, Bool) = {
    PriorityMuxWithFlag(in.zipWithIndex.map(x => x._1 -> x._2.U))
  }
  def apply(in: Bits): (UInt, Bool) = {
    PriorityEncoderWithFlag(in.asBools())
  }
}