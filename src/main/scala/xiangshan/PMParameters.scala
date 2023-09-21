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

package xiangshan

import chisel3.util.log2Ceil
import chipsalliance.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.tile.XLen
import system.SoCParamsKey
import xiangshan.backend.fu.{MMPMAConfig, MMPMAMethod}

case object PMParameKey extends Field[PMParameters]

case class PMParameters
(
  NumPMP: Int = 16,
  NumPMA: Int = 16,

  PlatformGrain: Int = log2Ceil(4*1024), // 4KB, a normal page
  mmpma: MMPMAConfig = MMPMAConfig(
    address = 0x38021000,
    mask = 0xfff,
    lgMaxSize = 3,
    sameCycle = true,
    num = 2
  )
)
//trait HasPMParameters extends PMParameters
trait HasPMParameters {
  implicit val p: Parameters

  val PMPAddrBits = p(SoCParamsKey).PAddrBits
  val PMXLEN = p(XLen)
  val pmParams = p(PMParameKey)
  val NumPMP = pmParams.NumPMP
  val NumPMA = pmParams.NumPMA

  val PlatformGrain = pmParams.PlatformGrain
  val mmpma = pmParams.mmpma
}