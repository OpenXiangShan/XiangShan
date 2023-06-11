package xiangshan.backend.datapath

object RdConfig {
  sealed abstract class RdConfig() {
    val port: Int
    val priority: Int
  }

  case class IntRD(port: Int = -1, priority: Int = Int.MaxValue) extends RdConfig()

  case class VfRD(port: Int = -1, priority: Int = Int.MaxValue) extends RdConfig()
}

