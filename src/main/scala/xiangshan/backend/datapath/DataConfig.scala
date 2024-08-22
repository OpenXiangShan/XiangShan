package xiangshan.backend.datapath

import chisel3.util.log2Up
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey

object DataConfig {
  sealed abstract class DataConfig (
    val name: String,
    val dataWidth: Int,
  ) {
    override def toString: String = name
  }

  case class IntData() extends DataConfig("int", 64)
  case class FpData() extends DataConfig("fp", 64)
  case class VecData() extends DataConfig("vec", 128)
  case class ImmData(len: Int) extends DataConfig("int", len)
  case class VAddrData()(implicit p: Parameters) extends DataConfig("vaddr", 48 + 2) // Todo: associate it with the width of vaddr
  case class V0Data() extends DataConfig("v0", 128)
  case class VlData() extends DataConfig("vl", log2Up(VecData().dataWidth) + 1 ) // 8
  case class FakeIntData() extends DataConfig("fakeint", 64)
  case class NoData() extends DataConfig("nodata", 0)

  def RegSrcDataSet   : Set[DataConfig] = Set(IntData(), FpData(), VecData(), V0Data(), VlData())
  def IntRegSrcDataSet: Set[DataConfig] = Set(IntData())
  def FpRegSrcDataSet : Set[DataConfig] = Set(FpData())
  def VecRegSrcDataSet : Set[DataConfig] = Set(VecData())
  def V0RegSrcDataSet : Set[DataConfig] = Set(V0Data())
  def VlRegSrcDataSet : Set[DataConfig] = Set(VlData())


  def RegDataMaxWidth : Int = RegSrcDataSet.map(_.dataWidth).max

  def VAddrBits(implicit p: Parameters): Int = {
    def coreParams = p(XSCoreParamsKey)
    def HasHExtension = coreParams.HasHExtension
    if(HasHExtension){
      coreParams.GPAddrBits
    }else{
      coreParams.VAddrBits
    }
    // VAddrBits is Virtual Memory addr bits
  }
}
