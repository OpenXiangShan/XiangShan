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
import xiangshan._

case class StdRSParams()

class StdRSWrapper(modGen: RSMod)(implicit p: Parameters) extends BaseReservationStationWrapper(modGen) {
  params.exuCfg = Some(StdExeUnitCfg)
  override lazy val module = new StdRSImp(params, this)
}

class StdRSImp(params: RSParams, wrapper: StdRSWrapper) extends BaseReservationStationImp(params, wrapper) {
  // extra.fpRegValue <> rs.flatMap(_.extra.fpRegValue)
}

class StdRS(params: RSParams)(implicit p: Parameters) extends BaseReservationStation(params)
  with RSDropNotOnRedirect {
  for ((uop, i) <- s1_payloadUops.zipWithIndex) {
    s1_deqRfDataSel(i)(0) := Mux(uop.needRfRPort(0, true, true), readFpRf_asyn(i).data, readIntRf_asyn(i).data)
  }
}