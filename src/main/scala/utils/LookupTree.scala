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

object LookupTree {
  def apply[T <: Data](key: UInt, mapping: Iterable[(UInt, T)]): T =
    Mux1H(mapping.map(p => (p._1 === key, p._2)))
}

object LookupTreeDefault {
  def apply[T <: Data](key: UInt, default: T, mapping: Iterable[(UInt, T)]): T =
    MuxLookup(key, default, mapping.toSeq)
}


object MuxT {
  def apply[T <: Data, U <: Data](cond: Bool, con: (T, U), alt: (T, U)): (T, U) =
    (Mux(cond, con._1, alt._1), Mux(cond, con._2, alt._2))

  def apply[T <: Data, U <: Data, W <: Data](cond: Bool, con: (T, U, W), alt: (T, U, W)): (T, U, W) =
    (Mux(cond, con._1, alt._1), Mux(cond, con._2, alt._2), Mux(cond, con._3, alt._3))

  def apply[T <: Data, U <: Data, W <: Data, X <: Data](cond: Bool, con: (T, U, W, X), alt: (T, U, W, X)): (T, U, W, X) =
    (Mux(cond, con._1, alt._1), Mux(cond, con._2, alt._2), Mux(cond, con._3, alt._3), Mux(cond, con._4, alt._4))
}

/** Creates a cascade of n MuxTs to search for a key value. */
object MuxTLookup {
  def apply[S <: UInt, T <: Data, U <: Data](key: S, default: (T, U), mapping: Seq[(S, (T, U))]): (T, U) = {
    var res = default
    for ((k, v) <- mapping.reverse)
      res = MuxT(k === key, v, res)
    res
  }

  def apply[S <: UInt, T <: Data, U <: Data, W <: Data](key: S, default: (T, U, W), mapping: Seq[(S, (T, U, W))]): (T, U, W) = {
    var res = default
    for ((k, v) <- mapping.reverse)
      res = MuxT(k === key, v, res)
    res
  }

  // in case you really need to search for a 4-tuple
  def apply[S <: UInt, T <: Data, U <: Data, W <: Data, X <: Data](key: S, default: (T, U, W, X), mapping: Seq[(S, (T, U, W, X))]): (T, U, W, X) = {
    var res = default
    for ((k, v) <- mapping.reverse)
      res = MuxT(k === key, v, res)
    res
  }
}
