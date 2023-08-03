package xiangshan.backend.regfile

import chisel3.util.log2Up
import xiangshan.backend.datapath.DataConfig._

abstract class PregParams {
  val numEntries: Int
  val numRead: Option[Int]
  val numWrite: Option[Int]
  val dataCfg: DataConfig

  def addrWidth = log2Up(numEntries)
}

case class IntPregParams(
  numEntries: Int,
  numRead   : Option[Int],
  numWrite  : Option[Int],
) extends PregParams {

  override val dataCfg: DataConfig = IntData()
}

case class VfPregParams(
  numEntries: Int,
  numRead   : Option[Int],
  numWrite  : Option[Int],
) extends PregParams {

  override val dataCfg: DataConfig = VecData()
}

case class NoPregParams() extends PregParams {
  val numEntries: Int = 0
  val numRead   : Option[Int] = None
  val numWrite  : Option[Int] = None

  override val dataCfg: DataConfig = NoData()
}
