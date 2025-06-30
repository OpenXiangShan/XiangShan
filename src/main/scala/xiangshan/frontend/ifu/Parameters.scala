package xiangshan.frontend.ifu

import chisel3.util._
import xiangshan.HasXSParameter
import xiangshan.frontend.icache.HasICacheParameters

trait HasIfuParameters extends HasICacheParameters {
  def FetchPorts:       Int = 2
  def ICacheLineBytes:  Int = 64
  // equal lower_result overflow bit
  def PcCutPoint:       Int = (VAddrBits / 4) - 1
}
