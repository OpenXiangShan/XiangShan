package xiangshan.backend.datapath

object RdConfig {
  sealed abstract class RdConfig() {
    val port: Int
    val priority: Int
  }

  case class IntRD(port: Int, priority: Int) extends RdConfig()

  case class VfRD(port: Int, priority: Int) extends RdConfig()
}

