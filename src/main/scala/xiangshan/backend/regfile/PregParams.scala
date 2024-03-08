package xiangshan.backend.regfile

import chisel3.util.log2Up
import xiangshan.backend.datapath.DataConfig._

abstract class PregParams {
  val numEntries: Int
  val numRead: Option[Int]
  val numWrite: Option[Int]
  val dataCfg: DataConfig
  val isFake: Boolean

  def addrWidth = log2Up(numEntries)
}

case class IntPregParams(
  numEntries: Int,
  numRead   : Option[Int],
  numWrite  : Option[Int],
) extends PregParams {

  val dataCfg: DataConfig = IntData()
  val isFake: Boolean = false
}

case class VfPregParams(
  numEntries: Int,
  numRead   : Option[Int],
  numWrite  : Option[Int],
) extends PregParams {

  val dataCfg: DataConfig = VecData()
  val isFake: Boolean = false
}

case class NoPregParams() extends PregParams {
  val numEntries: Int = 0
  val numRead   : Option[Int] = None
  val numWrite  : Option[Int] = None

  val dataCfg: DataConfig = NoData()
  val isFake: Boolean = false
}

case class FakeIntPregParams(
  numEntries: Int,
  numRead   : Option[Int],
  numWrite  : Option[Int],
) extends PregParams {

  val dataCfg: DataConfig = FakeIntData()
  val isFake: Boolean = true
}
