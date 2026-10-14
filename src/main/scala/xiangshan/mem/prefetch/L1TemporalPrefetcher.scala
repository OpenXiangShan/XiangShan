package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import xiangshan._
import xiangshan.mem.prefetch.temporal.TemporalPrefetchEngine

/** L1 port of coupledL2's on-chip temporal prefetcher. Table sizes match the L2 implementation. */
case class TemporalPrefetcherParams(
  tpTableEntries: Int = 16384,
  tpTableAssoc: Int = 16,
  tpTrainQueueDepth: Int = 8,
  dataReadQueueDepth: Int = 8,
  dataWriteQueueDepth: Int = 4,
  tpDataQueueDepth: Int = 16,
  tpMetaWQueueDepth: Int = 8,
  tpMetaResetQueueDepth: Int = 8,
  throttleCycles: Int = 4,
  replacementPolicy: String = "plru",
  samplerFilterEntries: Int = 256,
  samplerFilterAssoc: Int = 8,
  samplerFilterReplacementPolicy: String = "plru",
  pcHashWidth: Int = 22,
  trainQueueDepth: Int = 100,
  samplerTableEntries: Int = 16384,
  samplerTableAssoc: Int = 16,
  samplerTableMatchCntWidth: Int = 3,
  samplerTableReplacementPolicy: String = "plru",
  recorderTableEntries: Int = 1024,
  recorderTableAssoc: Int = 8,
  recorderTableReplacementPolicy: String = "plru",
  confTableEntries: Int = 512,
  confTableAssoc: Int = 8,
  confTableReplacementPolicy: String = "plru",
  confReqQueueDepth: Int = 8,
  debug: Boolean = false
) extends PrefetcherParams {
  override def name = "Temporal"
  override def TRAIN_FILTER_SIZE = 16

  private def checkTable(entries: Int, ways: Int): Unit = {
    require(ways >= 2 && isPow2(ways))
    require(entries >= 2 * ways && entries % ways == 0 && isPow2(entries / ways))
  }
  checkTable(tpTableEntries, tpTableAssoc)
  checkTable(samplerFilterEntries, samplerFilterAssoc)
  checkTable(samplerTableEntries, samplerTableAssoc)
  checkTable(recorderTableEntries, recorderTableAssoc)
  checkTable(confTableEntries, confTableAssoc)
  require(dataReadQueueDepth >= 4 && confReqQueueDepth >= 4)
  require(Seq(tpTrainQueueDepth, dataWriteQueueDepth, tpDataQueueDepth,
    tpMetaWQueueDepth, tpMetaResetQueueDepth, trainQueueDepth).forall(_ >= 4))
  require(pcHashWidth >= 14 && pcHashWidth > log2Ceil(recorderTableEntries / recorderTableAssoc))
  require(throttleCycles >= 0 && throttleCycles < 16)
}

/** Uses the same training, request arbiter and LoadUnit path as the other L1 prefetchers.
  * Each recorded token contains a physical cache line and its virtual alias. No address
  * translation is necessary on replay; the inherited TLB interface remains inactive.
  */
class L1TemporalPrefetcher(implicit p: Parameters) extends BasePrefecher {
  private val params = prefetcherSeq.collectFirst { case t: TemporalPrefetcherParams => t }.get
  private val trainFilter = Module(new NewTrainFilter(params.TRAIN_FILTER_SIZE, "temporal"))
  private val temporal = Module(new TemporalPrefetchEngine)

  trainFilter.io.enable := io.enable
  trainFilter.io.flush := !io.enable
  trainFilter.io.ldTrainOpt.get <> io.ld_in
  temporal.io.enable := io.enable

  val train = trainFilter.io.trainReq
  // Consume training and its confidence feedback atomically so their PCs stay aligned.
  val needsFeedback = isFromTemporal(train.bits.metaSource)
  temporal.io.train.valid := train.valid && (!needsFeedback || temporal.io.feedBack.ready)
  temporal.io.train.bits.addr := Cat(train.bits.vaddr(13, 12), train.bits.paddr)
  temporal.io.train.bits.vaddr := train.bits.vaddr
  temporal.io.train.bits.pc := train.bits.pc
  temporal.io.train.bits.hit := !train.bits.miss
  temporal.io.train.bits.prefetched := needsFeedback
  temporal.io.feedBack.valid := train.valid && needsFeedback && temporal.io.train.ready
  temporal.io.feedBack.bits.pc := train.bits.pc
  temporal.io.feedBack.bits.hit := !train.bits.miss
  temporal.io.feedBack.bits.late := train.bits.miss
  train.ready := temporal.io.train.ready && (!needsFeedback || temporal.io.feedBack.ready)

  val pfPaddr = temporal.io.req.bits.addr(PAddrBits - 1, 0)
  val inPmem = PmemRanges.map(_.cover(pfPaddr)).reduce(_ || _)
  io.l1_req.valid := temporal.io.req.valid && inPmem && io.enable
  io.l1_req.bits.paddr := pfPaddr
  io.l1_req.bits.alias := temporal.io.req.bits.addr(PAddrBits + 1, PAddrBits)
  io.l1_req.bits.confidence := 0.U // use idle LoadUnit capacity, like low-confidence stride
  io.l1_req.bits.is_store := false.B
  io.l1_req.bits.pf_source.value := L1_HW_PREFETCH_TEMPORAL
  temporal.io.req.ready := io.l1_req.ready || !inPmem || !io.enable

  XSPerfAccumulate("temporal_train", train.fire)
  XSPerfAccumulate("temporal_feedback", temporal.io.feedBack.fire)
  XSPerfAccumulate("temporal_sent", io.l1_req.fire)
  XSPerfAccumulate("temporal_blocked", io.l1_req.valid && !io.l1_req.ready)
  XSPerfAccumulate("temporal_drop_non_pmem", temporal.io.req.fire && !inPmem)
}
