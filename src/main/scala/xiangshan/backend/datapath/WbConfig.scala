package xiangshan.backend.datapath

import chisel3.util.log2Up
import xiangshan.backend.BackendParams
import xiangshan.backend.datapath.DataConfig._

object WbConfig {
  sealed abstract class WbConfig() {
    val port: Int
    def dataCfg: DataConfig
    def dataWidth: Int = dataCfg.dataWidth

    def writeInt = dataCfg == IntData()
    def writeFp = dataCfg == FpData()
    def writeVec = dataCfg == VecData()

    override def toString: String = {
      var res = this match {
        case _: IntWB => "INT"
        case _: VfWB => "VF"
        case _: NoWB => "NO"
        case _ => "??"
      }
      res += s"($port)"
      res
    }
  }

  sealed abstract class ExuWB extends WbConfig

  sealed abstract class PregWB extends ExuWB {
    val priority: Int

    def numPreg(backendParams: BackendParams): Int

    def pregIdxWidth(backendParams: BackendParams) = log2Up(numPreg(backendParams))
  }

  case class IntWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {

    def dataCfg: DataConfig = IntData()

    def numPreg(backendParams: BackendParams): Int = backendParams.getPregParams(IntData()).numEntries
  }

  case class VfWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {

    def dataCfg: DataConfig = VecData()

    def numPreg(backendParams: BackendParams): Int = backendParams.getPregParams(VecData()).numEntries
  }

  case class NoWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {

    override def dataCfg: DataConfig = NoData()

    override def numPreg(backendParams: BackendParams): Int = 0
  }

  case class CtrlWB(
    port: Int = -1,
  ) extends WbConfig {
    val priority: Int = Int.MaxValue
    override def dataCfg: DataConfig = NoData()
  }
}

