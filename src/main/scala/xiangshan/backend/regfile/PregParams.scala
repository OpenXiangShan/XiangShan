package xiangshan.backend.regfile

import chisel3.util.log2Up
import xiangshan.backend.datapath.DataConfig._

abstract class PregParams {
  val numEntries: Int
  val numRead: Int
  val numWrite: Int
  val dataCfg: DataConfig

  def addrWidth = log2Up(numEntries)
}

case class IntPregParams(
  numEntries: Int,
  numRead   : Int,
  numWrite  : Int,
) extends PregParams {
  override val dataCfg: DataConfig = IntData()
}

case class VfPregParams(
  numEntries: Int,
  numRead   : Int,
  numWrite  : Int,
) extends PregParams {
  override val dataCfg: DataConfig = VecData()
}

