// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
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
//
//
// Acknowledgement
//
// This implementation is inspired by several key papers:
// [1] Glenn Reinman, Brad Calder, and Todd Austin. "[Fetch directed instruction prefetching.]
// (https://doi.org/10.1109/MICRO.1999.809439)" 32nd Annual ACM/IEEE International Symposium on Microarchitecture
// (MICRO). 1999.

package xiangshan.frontend.icache

import freechips.rocketchip.diplomacy.LazyModule
import org.chipsalliance.cde.config.Parameters

class ICache()(implicit p: Parameters) extends LazyModule with HasICacheParameters {
  override def shouldBeInlined: Boolean = false

  // currently, L2 cache supports only 2 bits alias tag
  require(
    AliasTagBits.isEmpty || AliasTagBits.get <= 2,
    s"L2 cache supports only 2bits alias tag, ICache with ${nSets}sets * ${blockBytes}B need ${AliasTagBits.get}bits"
  )

  val ctrlUnitOpt: Option[ICacheCtrlUnit] = Option.when(EnableCtrlUnit)(LazyModule(new ICacheCtrlUnit))

  lazy val module: ICacheImp = new ICacheImp(this)
}
