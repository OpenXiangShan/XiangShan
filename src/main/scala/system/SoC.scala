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

package system

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import xiangshan.{DebugOptionsKey, HasXSParameter, XSBundle, XSCore, XSCoreParameters}
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, BusErrors, L1BusErrors}
import huancun.{CacheParameters, HCCacheParameters}

case object SoCParamsKey extends Field[SoCParameters]

case class SoCParameters
(
  cores: List[XSCoreParameters],
  EnableILA: Boolean = false,
  extIntrs: Int = 150,
  L3NBanks: Int = 4,
  L3CacheParams: HCCacheParameters = HCCacheParameters(
    name = "l3",
    level = 3,
    ways = 8,
    sets = 2048 // 1MB per bank
  ),
  useFakeL3Cache: Boolean = false,
){
  val PAddrBits = cores.map(_.PAddrBits).reduce((x, y) => if(x > y) x else y)
  // L3 configurations
  val L3InnerBusWidth = 256
  val L3BlockSize = 64
  // on chip network configurations
  val L3OuterBusWidth = 256
}

trait HasSoCParameter {
  implicit val p: Parameters

  val soc = p(SoCParamsKey)
  val debugOpts = p(DebugOptionsKey)
  val NumCores = soc.cores.size
  val EnableILA = soc.EnableILA

  // L3 configurations
  val useFakeL3Cache = soc.useFakeL3Cache
  val L3InnerBusWidth = soc.L3InnerBusWidth
  val L3BlockSize = soc.L3BlockSize
  val L3NBanks = soc.L3NBanks

  // on chip network configurations
  val L3OuterBusWidth = soc.L3OuterBusWidth

  val NrExtIntr = soc.extIntrs
}

class ILABundle extends Bundle {}


class L1CacheErrorInfo(implicit val p: Parameters) extends Bundle with HasSoCParameter {
  val paddr = Valid(UInt(soc.PAddrBits.W))
  // for now, we only detect ecc
  val ecc_error = Valid(Bool())
}

class XSL1BusErrors(val nCores: Int)(implicit val p: Parameters) extends BusErrors {
  val icache = Vec(nCores, new L1CacheErrorInfo)
  val l1plus = Vec(nCores, new L1CacheErrorInfo)
  val dcache = Vec(nCores, new L1CacheErrorInfo)

  override def toErrorList: List[Option[(ValidIO[UInt], String, String)]] =
    List.tabulate(nCores){i =>
      List(
        Some(icache(i).paddr, s"IBUS_$i", s"Icache_$i bus error"),
        Some(icache(i).ecc_error, s"I_ECC_$i", s"Icache_$i ecc error"),
        Some(l1plus(i).paddr, s"L1PLUS_$i", s"L1PLUS_$i bus error"),
        Some(l1plus(i).ecc_error, s"L1PLUS_ECC_$i", s"L1PLUS_$i ecc error"),
        Some(dcache(i).paddr, s"DBUS_$i", s"Dcache_$i bus error"),
        Some(dcache(i).ecc_error, s"D_ECC_$i", s"Dcache_$i ecc error")
      )
    }.flatten
}
