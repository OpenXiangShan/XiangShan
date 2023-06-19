package xiangshan.backend.datapath

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.IssueQueueWakeUpBundle
import xiangshan.backend.exu.ExeUnitParams

import scala.language.higherKinds

trait WakeUpPoint {
  val name: String

  def getExuParam(exus: Seq[ExeUnitParams]) : ExeUnitParams = {
    val filteredExus = exus.filter(_.name == this.name)
    require(filteredExus.isEmpty, s"No exu named $name")
    require(filteredExus.size > 1, s"Exu $name should be unique")
    filteredExus.head
  }
}

class WakeUpSource(val name: String) extends WakeUpPoint {
  def genIQWakeUpValidBundle(backendParam: BackendParams): ValidIO[IssueQueueWakeUpBundle] = {
    ValidIO(new IssueQueueWakeUpBundle(name, backendParam))
  }
}

class WakeUpSink(val name: String) extends WakeUpPoint

class WakeUpConfig (val source: WakeUpSource, val sink: WakeUpSink) {
  def this(pair: (String, String)) = {
    this(new WakeUpSource(pair._1), new WakeUpSink(pair._2))
  }

  def this(source: String, sink: String) = {
    this(new WakeUpSource(source), new WakeUpSink(sink))
  }

  override def toString: String = {
    s"WakeUp(${source.name}->${sink.name})"
  }
}

object WakeUpConfig {
  def apply(source: String, sink: String): WakeUpConfig = new WakeUpConfig(source, sink)

  def apply(pair: (String, String)): WakeUpConfig = new WakeUpConfig(pair)
}
