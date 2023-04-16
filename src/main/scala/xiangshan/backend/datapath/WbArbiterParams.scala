package xiangshan.backend.datapath

import chipsalliance.rocketchip.config.Parameters
import chisel3.Output
import chisel3.util.{DecoupledIO, MixedVec, ValidIO, log2Up}
import xiangshan.backend.Bundles.WriteBackBundle
import xiangshan.backend.datapath.DataConfig.{FpData, IntData, VecData}
import xiangshan.backend.datapath.WbConfig.{FpWB, IntWB, VecWB, WbConfig}
import xiangshan.backend.regfile.PregParams

case class WbArbiterParams(
  wbCfgs    : Seq[WbConfig],
  pregParams: PregParams,
) {

  def numIn = wbCfgs.length

  def numOut = pregParams.numWrite

  def dataWidth = pregParams.dataCfg.dataWidth

  def addrWidth = log2Up(pregParams.numEntries)

  def genInput(implicit p: Parameters) = {
    MixedVec(wbCfgs.map(x => DecoupledIO(new WriteBackBundle(x))))
  }

  def genOutput(implicit p: Parameters): MixedVec[ValidIO[WriteBackBundle]] = {
    Output(MixedVec(Seq.tabulate(numOut) {
      x =>
        ValidIO(new WriteBackBundle(
          wbCfgs.head.dataCfg match {
            case IntData() => IntWB(port = x)
            case FpData() => FpWB(port = x)
            case VecData() => VecWB(port = x)
            case _ => ???
          }
        )
        )
    }
    )
    )
  }
}
