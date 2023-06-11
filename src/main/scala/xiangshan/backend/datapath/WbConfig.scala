package xiangshan.backend.datapath

import chisel3.util.log2Up
import xiangshan.backend.datapath.DataConfig._

object WbConfig {
  sealed abstract class WbConfig() {
    val port: Int
    def dataCfg: DataConfig
    def numPreg: Int = 0
    def dataWidth: Int = dataCfg.dataWidth

    def pregIdxWidth = log2Up(numPreg)

    def writeInt = dataCfg == IntData()
    def writeFp = dataCfg == FpData()
    def writeVec = dataCfg == VecData()
  }

  sealed abstract class ExuWB extends WbConfig

  sealed abstract class PregWB extends ExuWB {
    val priority: Int
  }

  case class IntWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {
    def dataCfg: DataConfig = IntData()
    override def numPreg: Int = 160
  }

  case class VfWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {
    def dataCfg: DataConfig = VecData()
    override def numPreg: Int = 160
  }

  case class VecWB(
    port: Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {
    def dataCfg: DataConfig = VecData()

    override def numPreg: Int = 160
  }

  case class CtrlWB(
    port: Int = -1,
  ) extends WbConfig {
    val priority: Int = Int.MaxValue
    override def dataCfg: DataConfig = NoData()
  }

  // Todo: use it
  sealed trait WBSource
  case class WBFromInt() extends WBSource
  case class WBFromMem() extends WBSource
  case class WBFromVec() extends WBSource
  case class WBFromFp()  extends WBSource
}

