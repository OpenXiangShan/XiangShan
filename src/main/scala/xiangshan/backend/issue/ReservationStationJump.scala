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

package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

case class JumpRSParams()

trait JumpRSMod extends RSSubMod {
  override def rsGen: (RSParams, Parameters) => JumpRS =
    (a: RSParams, b: Parameters) => new JumpRS(a)(b)
  override def rsIOGen: (RSParams, Parameters) => JumpRSIO =
    (a: RSParams, b: Parameters) => new JumpRSIO(a)(b)
}

class JumpRSWrapper(implicit p: Parameters) extends BaseReservationStationWrapper {
  override lazy val module = new JumpRSImp(params, this)
}

class JumpRSIO(params: RSParams)(implicit p: Parameters) extends BaseReservationStationIO(params)

class JumpRSImp(params: RSParams, wrapper: JumpRSWrapper) extends BaseReservationStationImp(params, wrapper) {
}

class JumpRS(params: RSParams)(implicit p: Parameters) extends BaseReservationStation(params)