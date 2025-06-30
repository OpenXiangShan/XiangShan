package xiangshan.frontend.ifu

import chisel3.util._
import xiangshan.HasXSParameter
import xiangshan.frontend.icache.HasICacheParameters

trait HasIfuParameters extends HasICacheParameters {
  def PreDecodeWidth: Int = PredictWidth + 4
  def FetchPorts:     Int = 2
  def ICacheLineSize: Int = blockBytes
  def FetchInstrPort: Int = blockBytes / 2
  // equal lower_result overflow bit
  def PcCutPoint:      Int = (VAddrBits / 4) - 1
  def ICacheLineBytes: Int = 64
}
