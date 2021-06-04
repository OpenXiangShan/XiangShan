/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

import system.SoCParameters
import xiangshan.{EnviromentParameters, XSCoreParameters}

case class Parameters
(
  coreParameters: XSCoreParameters = XSCoreParameters(),
  socParameters: SoCParameters = SoCParameters(),
  envParameters: EnviromentParameters = EnviromentParameters()
){
  require(
    !(envParameters.FPGAPlatform && envParameters.EnableDebug),
    "Enable debug(display log) is only supported in simulation enviroment!"
  )
  require(
    !(socParameters.EnableILA && !envParameters.FPGAPlatform),
    "ILA is only supported in FPGA platform!"
  )
}

object Parameters {
  val dualCoreParameters = Parameters(socParameters = SoCParameters(NumCores = 2))
  val simParameters = Parameters(envParameters = EnviromentParameters(FPGAPlatform = false)) // sim only, disable log
  val debugParameters = Parameters(envParameters = simParameters.envParameters.copy(EnableDebug = true, EnablePerfDebug = true)) // open log

  val simDualCoreParameters = Parameters(socParameters = SoCParameters(NumCores = 2), envParameters = EnviromentParameters(FPGAPlatform = false, DualCore = true))
  val debugDualCoreParameters = Parameters(socParameters = SoCParameters(NumCores = 2), envParameters = simDualCoreParameters.envParameters.copy(EnableDebug = true))

  private var parameters = Parameters() // a default parameter, can be updated before use
  def get: Parameters = parameters
  def set(p: Parameters): Unit = {
    parameters = p
  }
}
