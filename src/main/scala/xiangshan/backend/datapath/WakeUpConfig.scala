package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3.util._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.IssueQueueIQWakeUpBundle
import xiangshan.backend.exu.ExeUnitParams

import scala.language.higherKinds

trait WakeUpPoint {
  val name: String

  def getExuParam(exus: Seq[ExeUnitParams]) : ExeUnitParams = {
    val filteredExus = exus.filter(_.name == this.name)
    require(filteredExus.nonEmpty, s"No exu named $name")
    require(filteredExus.size == 1, s"Exu $name should be unique")
    filteredExus.head
  }
}

class WakeUpSource(val name: String) extends WakeUpPoint {
  def genIQWakeUpValidBundle(backendParam: BackendParams)(implicit p: Parameters): ValidIO[IssueQueueIQWakeUpBundle] = {
    ValidIO(new IssueQueueIQWakeUpBundle(backendParam.getExuIdx(name), backendParam))
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
  def apply(pair: (Seq[String], Seq[String])): Seq[WakeUpConfig] = for {
    source <- pair._1
    sink <- pair._2
  } yield new WakeUpConfig(source, sink)
}
