package xiangshan.mem.prefetch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import xiangshan._
import utility._
import xiangshan.cache.HasDCacheParameters

// parameter case
case class NextLineParams() extends PrefetcherParams {
  override def name = "NextLine"
}

// trait
trait HasNextLineHelper extends HasDCacheParameters{
  def param = p(XSCoreParamsKey).prefetcher.find{
    case p: NextLineParams => true
    case _ => false
  }.get.asInstanceOf[NextLineParams]
}

// module
class NextLinePrefetcher(implicit p: Parameters) extends BasePrefecher with HasNextLineHelper {
  val trainFilter = Module(new TrainFilter(param.TRAIN_FILTER_SIZE, param.name))
  trainFilter.io.enable := io.enable
  trainFilter.io.flush := false.B
  trainFilter.io.ld_in := io.ld_in
  trainFilter.io.train_req.ready := true.B
  val trainValid = trainFilter.io.train_req.valid
  val train = trainFilter.io.train_req.bits

  val vaddrPf = (get_block(train.vaddr) + 1.U) << blockOffBits
  val paddrPf = (get_block(train.paddr) + 1.U) << blockOffBits
  // not cross page, so no tlb
  val isCrossPage = get_phy_tag(train.paddr) =/= get_phy_tag(paddrPf)

  io.l1_req.valid := trainValid && !isCrossPage && io.enable
  io.l1_req.bits.paddr := paddrPf
  io.l1_req.bits.alias := get_alias(vaddrPf)
  io.l1_req.bits.confidence := 1.U
  io.l1_req.bits.is_store := false.B
  io.l1_req.bits.pf_source.value := L1_HW_PREFETCH_NEXTLINE
  XSPerfAccumulate("train", trainValid)
  XSPerfAccumulate("req_sent", io.l1_req.valid)
  XSPerfAccumulate("req_drop_crosspage", trainValid && isCrossPage)
}
