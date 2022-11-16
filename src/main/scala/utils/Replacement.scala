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

// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package utils

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import freechips.rocketchip.util._
import freechips.rocketchip.util.property.cover
import xiangshan.{XSCoreParameters}

object ReplacementPolicy {
  //for fully associative mapping
  def fromString(s: Option[String],n_ways: Int): ReplacementPolicy = fromString(s.getOrElse("none"),n_ways)
  def fromString(s: String, n_ways: Int): ReplacementPolicy = s.toLowerCase match {
    case "random" => new RandomReplacement(n_ways)
    case "lru"    => new TrueLRU(n_ways)
    case "plru"   => new PseudoLRU(n_ways)
    case t => throw new IllegalArgumentException(s"unknown Replacement Policy type $t")
  }
  //for set associative mapping
  def fromString(s: Option[String], n_ways: Int, n_sets: Int): SetAssocReplacementPolicy = fromString(s.getOrElse("none"),n_ways,n_sets )
  def fromString(s: String, n_ways: Int, n_sets: Int): SetAssocReplacementPolicy = s.toLowerCase match {
    case "random"    => new SetAssocRandom(n_sets, n_ways)
    case "setlru"    => new SetAssocLRU(n_sets, n_ways, "lru")
    case "setplru"   => new SetAssocLRU(n_sets, n_ways, "plru")
    case t => throw new IllegalArgumentException(s"unknown Replacement Policy type $t")
  }
}

class SetAssocRandom(n_sets : Int, n_ways: Int) extends SetAssocReplacementPolicy {
  val random = new RandomReplacement(n_ways)

  def miss(set: UInt) =  random.miss
  def way(set: UInt) = random.way

  def access(set: UInt, touch_way: UInt) = random.access(touch_way)
  def access(sets: Seq[UInt], touch_ways: Seq[Valid[UInt]]) = random.access(touch_ways)

}