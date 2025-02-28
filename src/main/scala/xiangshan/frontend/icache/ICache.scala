/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] Glenn Reinman, Brad Calder, and Todd Austin. "[Fetch directed instruction prefetching.]
* (https://doi.org/10.1109/MICRO.1999.809439)" 32nd Annual ACM/IEEE International Symposium on Microarchitecture
* (MICRO). 1999.
***************************************************************************************/

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.diplomacy.IdRange
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleFieldBase
import huancun.AliasField
import huancun.PrefetchField
import org.chipsalliance.cde.config.Parameters
import utility._
import utility.mbist.MbistPipeline
import utility.sram.SRAMReadBus
import utility.sram.SRAMTemplate
import utility.sram.SRAMWriteBus
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.frontend._

class ICache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {
  override def shouldBeInlined: Boolean = false

  val clientParameters: TLMasterPortParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "icache",
      sourceId = IdRange(0, cacheParams.nFetchMshr + cacheParams.nPrefetchMshr + 1)
    )),
    requestFields = cacheParams.reqFields,
    echoFields = cacheParams.echoFields
  )

  val clientNode: TLClientNode = TLClientNode(Seq(clientParameters))

  val ctrlUnitOpt: Option[ICacheCtrlUnit] = ctrlUnitParamsOpt.map(params => LazyModule(new ICacheCtrlUnit(params)))

  lazy val module: ICacheImp = new ICacheImp(this)
}
