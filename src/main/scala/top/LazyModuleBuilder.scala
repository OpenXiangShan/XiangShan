/***************************************************************************************
* Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
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

package top

import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import org.chipsalliance.cde.config.Parameters

trait HasLazyModuleBuilder {
  def buildWithName[T <: LazyModule](name: String)
      (constructor: Parameters => T)(params: Parameters): T =
  {
    constructor(params)
  }

  def buildLazyModuleWithName[T <: LazyModule](name: String)
      (constructor: Parameters => T)(params: Parameters): T =
  {
    implicit val valName: ValName = ValName(name)
    LazyModule(buildWithName(name)(constructor)(params))
  }
}
