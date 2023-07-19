package xiangshan.backend.datapath

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
  case class VAddrData() extends DataConfig("vaddr", 39) // Todo: associate it with the width of vaddr
  case class MaskSrcData() extends DataConfig("masksrc", VecData().dataWidth) // 128
  case class MaskDstData() extends DataConfig("maskdst", VecData().dataWidth / 8) // 16
  case class VConfigData() extends DataConfig("vconfig", VecData().dataWidth) // Todo: use 16 bit instead
  case class NoData() extends DataConfig("nodata", 0)

  def RegSrcDataSet   : Set[DataConfig] = Set(IntData(), FpData(), VecData(), MaskSrcData(), VConfigData())
  def IntRegSrcDataSet: Set[DataConfig] = Set(IntData())
  def FpRegSrcDataSet : Set[DataConfig] = Set(FpData())
  def VecRegSrcDataSet: Set[DataConfig] = Set(VecData(), MaskSrcData(), VConfigData())
  def VfRegSrcDataSet : Set[DataConfig] = Set(FpData(), VecData(), MaskSrcData(), VConfigData())

  def RegDataMaxWidth : Int = RegSrcDataSet.map(_.dataWidth).max
}
