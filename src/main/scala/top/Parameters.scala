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
