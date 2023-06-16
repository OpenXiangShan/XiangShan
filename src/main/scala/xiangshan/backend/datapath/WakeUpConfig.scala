package xiangshan.backend.datapath

import xiangshan.backend.exu.ExeUnitParams

case class WakeUpConfig (source: String, sink: String) {
  def getSourceExuParam(exus: Seq[ExeUnitParams]) : ExeUnitParams = {
    val sourceExus = exus.filter(_.name == source)
    require(sourceExus.size == 1)
    sourceExus.head
  }

  def getSinkExuParam(exus: Seq[ExeUnitParams]) : ExeUnitParams = {
    val sinkExus = exus.filter(_.name == sink)
    require(sinkExus.size == 1)
    sinkExus.head
  }
}
