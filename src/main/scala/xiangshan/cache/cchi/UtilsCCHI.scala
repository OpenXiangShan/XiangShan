package xiangshan.cache

import chisel3._
import chisel3.util._

object CCHICheckTypeEquality {
  def apply(x: Data, y: Data): Boolean = {
    val kx = portClass(x)
    val ky = portClass(y)
    kx.nonEmpty && kx == ky
  }

  private def portClass(d: Data): Option[Class[_]] = d match {
    case _: CCHIType1Port => Some(classOf[CCHIType1Port])
    case _: CCHIType3Port => Some(classOf[CCHIType3Port])
    case _: CCHIType4Port => Some(classOf[CCHIType4Port])
    case _                => None
  }
}

object CCHIBuffer {
  // upper = requester / inner; down = completer / outer.
  // up* : upper -> Queue -> down;  dn* : down -> Queue -> upper.
  def apply[T <: Data](upper: T, down: T, nStages: Int = 1): Unit = {
    require(CCHICheckTypeEquality(upper, down), "CCHI port types must match")
    require(nStages >= 1, "nStages must be >= 1")
    val upperChans = channels(upper)
    val downChans = channels(down)
    upperChans.foreach { case (name, u) =>
      val d = downChans(name)
      if (name.startsWith("up")) {
        d <> QueueBuffer(u, nStages)
      } else if (name.startsWith("dn")) {
        u <> QueueBuffer(d, nStages)
      } else {
        require(false, s"CCHI channel must be up* or dn*: $name")
      }
    }
  }

  private def channels(p: Data): collection.Map[String, DecoupledIO[Data]] = {
    p.asInstanceOf[Record].elements.map { case (name, ch) =>
      require(ch.isInstanceOf[DecoupledIO[_]], s"CCHI field $name is not DecoupledIO")
      name -> ch.asInstanceOf[DecoupledIO[Data]]
    }
  }

  private def QueueBuffer[T <: Data](x: DecoupledIO[T], nStages: Int): DecoupledIO[T] = {
    (0 until nStages).foldLeft(x)((ch, _) => Queue(ch, 2))
  }
}
