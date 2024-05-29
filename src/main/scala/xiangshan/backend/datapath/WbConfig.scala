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
        case _: FpWB => "FP"
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

  case class FpWB(
    port: Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {

    def dataCfg: DataConfig = FpData()

    def numPreg(backendParams: BackendParams): Int = backendParams.getPregParams(FpData()).numEntries
  }

  case class VfWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {

    def dataCfg: DataConfig = VecData()

    def numPreg(backendParams: BackendParams): Int = backendParams.getPregParams(VecData()).numEntries
  }

  case class V0WB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {

    def dataCfg: DataConfig = V0Data()

    def numPreg(backendParams: BackendParams): Int = backendParams.getPregParams(V0Data()).numEntries
  }

  case class VlWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {

    def dataCfg: DataConfig = VlData()

    def numPreg(backendParams: BackendParams): Int = backendParams.getPregParams(VlData()).numEntries
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

  case class FakeIntWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {

    def dataCfg: DataConfig = FakeIntData()

    def numPreg(backendParams: BackendParams): Int = backendParams.getPregParams(FakeIntData()).numEntries
  }
}
