package xiangshan.backend.regfile

import chisel3._
import chisel3.util._
import xiangshan.backend.BackendParams
import xiangshan.backend.datapath.DataConfig._

/** Physical RF shape and interleaved-bank address decomposition.
  * Low address bits select a power-of-two bank; remaining high bits address its
  * row. Optional port counts are overrides consumed by backend datapath builders.
  */
abstract class PregParams {
  val numEntries: Int
  val numBank   : Int
  val numRead   : Option[Int]
  val numWrite  : Option[Int]
  val dataCfg   : DataConfig
  val isFake    : Boolean

  def addrWidth = log2Up(numEntries)
  // addr for datas after read each bank
  def bankRaddrWidth = log2Ceil(numBank)
  // addr for read each bank
  def arbiterAddrWidth = addrWidth - bankRaddrWidth

  /** Select low bank bits; a single-bank RF uses constant bank zero. */
  def bankIndex(addr: UInt): UInt = {
    require(numBank > 0 && isPow2(numBank), s"numBank must be a positive power of two, but got $numBank")
    if (numBank == 1) 0.U else addr(bankRaddrWidth - 1, 0)
  }

  /** Remove bank-select bits while retaining the row's original high bits. */
  def bankAddr(addr: UInt): UInt = {
    require(numBank > 0 && isPow2(numBank), s"numBank must be a positive power of two, but got $numBank")
    addr(addrWidth - 1, bankRaddrWidth)
  }

  /** Query the assembled backend's write count; fake/absent RF descriptors are unsupported. */
  def getNumWrite(backendParams: BackendParams): Int = {
    this match {
      case _: FpPregParams => backendParams.getFpRfWriteSize
      case _: IntPregParams => backendParams.getIntRfWriteSize
      case _: V0PregParams => backendParams.getV0RfWriteSize
      case _: VfPregParams => backendParams.getVfRfWriteSize
      case _: VlPregParams => backendParams.getVlRfWriteSize
      case _ => throw new RuntimeException(s"the type[${this.getClass}] is not permitted to call getNumWrite method")
    }
  }
}

case class IntPregParams(
  numEntries: Int,
  numBank   : Int,
  numRead   : Option[Int],
  numWrite  : Option[Int],
) extends PregParams {

  val dataCfg: DataConfig = IntData()
  val isFake: Boolean = false
}

case class FpPregParams(
                          numEntries: Int,
                          numBank   : Int,
                          numRead   : Option[Int],
                          numWrite  : Option[Int],
                        ) extends PregParams {

  val dataCfg: DataConfig = FpData()
  val isFake: Boolean = false
}

case class VfPregParams(
  numEntries: Int,
  numBank   : Int,
  numRead   : Option[Int],
  numWrite  : Option[Int],
) extends PregParams {

  val dataCfg: DataConfig = VecData()
  val isFake: Boolean = false
}

case class V0PregParams(
  numEntries: Int,
  numBank: Int,
  numRead   : Option[Int],
  numWrite  : Option[Int],
) extends PregParams {

  val dataCfg: DataConfig = V0Data()
  val isFake: Boolean = false
}

case class VlPregParams(
  numEntries: Int,
  numBank   : Int,
  numRead   : Option[Int],
  numWrite  : Option[Int],
) extends PregParams {

  val dataCfg: DataConfig = VlData()
  val isFake: Boolean = false
}

case class NoPregParams() extends PregParams {
  val numEntries: Int = 0
  val numBank   : Int = 0
  val numRead   : Option[Int] = None
  val numWrite  : Option[Int] = None

  val dataCfg: DataConfig = NoData()
  val isFake: Boolean = false
}

case class FakeIntPregParams(
  numEntries: Int,
  numBank   : Int,
  numRead   : Option[Int],
  numWrite  : Option[Int],
) extends PregParams {

  val dataCfg: DataConfig = FakeIntData()
  val isFake: Boolean = true
}
