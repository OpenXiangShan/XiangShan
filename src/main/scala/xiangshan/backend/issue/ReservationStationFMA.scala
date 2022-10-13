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

case class FMARSParams()

trait FMARSMod extends RSSubMod {
  override def rsGen: (RSParams, Parameters) => FMARS =
    (a: RSParams, b: Parameters) => new FMARS(a)(b)
  override def rsIOGen: (RSParams, Parameters) => FMARSIO =
    (a: RSParams, b: Parameters) => new FMARSIO(a)(b)
}

class FMARSWrapper(implicit p: Parameters) extends BaseReservationStationWrapper {
  override lazy val module = new FMARSImp(params, this)
}

class FMARSIO(params: RSParams)(implicit p: Parameters) extends BaseReservationStationIO(params)

class FMARSImp(params: RSParams, wrapper: FMARSWrapper) extends BaseReservationStationImp(params, wrapper) {
}

class FMARS(params: RSParams)(implicit p: Parameters) extends BaseReservationStation(params)