package xiangshan.backend.datapath

import chisel3.util.log2Up
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey

object DataConfig {
  private var vlen: Int = 128
  private var vlWidth: Int = log2Up(vlen) + 1

  def setVLen(newVLen: Int): Unit = {
    vlen = newVLen
    vlWidth = log2Up(newVLen) + 1
  }

  def VLEN: Int = vlen
  def VlWidth: Int = vlWidth

  sealed abstract class DataConfig (
    val name: String,
  ) {
    def dataWidth: Int
    override def toString: String = name
  }

  case class IntData() extends DataConfig("int") {
    override def dataWidth: Int = 64
  }
  case class FpData() extends DataConfig("fp") {
    override def dataWidth: Int = 64
  }
  case class VecData() extends DataConfig("vec") {
    override def dataWidth: Int = VLEN
  }
  case class ImmData(len: Int) extends DataConfig("int") {
    override def dataWidth: Int = len
  }
  case class VAddrData()(implicit p: Parameters) extends DataConfig("vaddr") { // Todo: associate it with the width of vaddr
    override def dataWidth: Int = 48 + 2
  }
  case class V0Data() extends DataConfig("v0") {
    override def dataWidth: Int = VLEN
  }
  case class VlData() extends DataConfig("vl") {
    override def dataWidth: Int = VlWidth
  }
  case class FakeIntData() extends DataConfig("fakeint") {
    override def dataWidth: Int = 64
  }
  case class NoData() extends DataConfig("nodata") {
    override def dataWidth: Int = 0
  }

  def RegSrcDataSet   : Set[DataConfig] = Set(IntData(), FpData(), VecData(), V0Data(), VlData())
  def IntRegSrcDataSet: Set[DataConfig] = Set(IntData())
  def FpRegSrcDataSet : Set[DataConfig] = Set(FpData())
  def VecRegSrcDataSet : Set[DataConfig] = Set(VecData())
  def V0RegSrcDataSet : Set[DataConfig] = Set(V0Data())
  def VlRegSrcDataSet : Set[DataConfig] = Set(VlData())


  def RegDataMaxWidth : Int = RegSrcDataSet.map(_.dataWidth).max

  def VAddrBits(implicit p: Parameters): Int = {
    def coreParams = p(XSCoreParamsKey)
    if (coreParams.HasHExtension) {
      if (coreParams.EnableSv48)
        coreParams.GPAddrBitsSv48x4
      else
        coreParams.GPAddrBitsSv39x4
    } else {
      if (coreParams.EnableSv48)
        coreParams.VAddrBitsSv48
      else
        coreParams.VAddrBitsSv39
    }
    // VAddrBits is Virtual Memory addr bits
  }
}
