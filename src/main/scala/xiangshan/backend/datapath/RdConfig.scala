package xiangshan.backend.datapath

import xiangshan.backend.datapath.DataConfig.{DataConfig, IntData, NoData, VecData}

object RdConfig {
  sealed abstract class RdConfig() {
    val port: Int
    val priority: Int

    def getDataConfig: DataConfig
  }

  case class IntRD(port: Int = -1, priority: Int = Int.MaxValue) extends RdConfig() {
    override def getDataConfig = IntData()
  }

  case class VfRD(port: Int = -1, priority: Int = Int.MaxValue) extends RdConfig() {
    override def getDataConfig = VecData()
  }

  case class NoRD() extends RdConfig() {
    override val port: Int = -1

    override val priority: Int = Int.MaxValue

    override def getDataConfig: DataConfig = NoData()
  }
}

