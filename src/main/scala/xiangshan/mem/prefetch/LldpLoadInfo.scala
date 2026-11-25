package xiangshan.mem.prefetch

import chisel3._
import chisel3.util.Valid
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSBundle
import xiangshan.backend.rob.RobPtr
import xiangshan.cache.DCacheBundle
import xiangshan.mem.LqPtr
import xiangshan.mem.prefetch.temporal.L1TemporalPrefetcher

case class L1LldpParams() extends PrefetcherParams {
  override def name: String = "L1LLDP"
}

private[xiangshan] class LldpLoadInfo(implicit p: Parameters) extends XSBundle {
  val producer = Valid(new LldpDependence(VAddrBits))
  val producerRobIdx = new RobPtr
  val producerLqIdx = new LqPtr
  val dataType = new LldpDataType
  val pc = UInt(VAddrBits.W)
}

private[xiangshan] class LldpRefill(implicit p: Parameters) extends DCacheBundle {
  val mshr = UInt(chisel3.util.log2Up(cfg.nMissEntries).W)
  val paddr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val way = UInt(nWays.W)
  val data = UInt(blockBits.W)
  val chainHit = Valid(new LldpTrigger)
  val readable = Bool()
}

private[xiangshan] class LldpTrigger(implicit p: Parameters) extends DCacheBundle {
  val pc = UInt(VAddrBits.W)
  val context = UInt(L1TemporalPrefetcher.ContextBits.W)
  val offset = UInt(blockOffBits.W)
  val dataType = new LldpDataType
  val source = UInt(LldpValueSource.getWidth.W)
  val depth = Bool()
  val addressRef = Valid(new LldpAddressRef)
}

private[xiangshan] class LldpPrefetchResult(implicit p: Parameters) extends DCacheBundle {
  val trigger = new LldpTrigger
  val mshr = UInt(chisel3.util.log2Up(cfg.nMissEntries).W)
  val allocated = Bool()
  val merged = Bool()
  val fromLldp = Bool()
  val dataHit = Bool()
  val paddr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
}

private[xiangshan] class LldpAddressTrain(implicit p: Parameters) extends DCacheBundle {
  val producer = UInt(VAddrBits.W)
  val consumer = UInt(VAddrBits.W)
  val producerAddress = UInt(PAddrBits.W)
  val consumerAddress = UInt(PAddrBits.W)
  val consumerVaddr = UInt(VAddrBits.W)
  val robIdx = new RobPtr
}
