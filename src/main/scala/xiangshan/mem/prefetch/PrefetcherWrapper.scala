package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import utils._
import xiangshan._
import xiangshan.backend.Bundles.DynInst
import xiangshan.backend.fu.{FuType, PMPRespBundle}
import xiangshan.cache.{DCacheLineReq, HasDCacheParameters, LoadPfDbBundle}
import xiangshan.cache.mmu._
import xiangshan.mem._
import xiangshan.mem.Bundles.LsPrefetchTrainBundle
import xiangshan.mem.trace._
import freechips.rocketchip.diplomacy.BufferParams.default
import utility.mbist.{MbistInterface, MbistPipeline}

trait PrefetcherParams

trait HasPrefetcherParams extends HasXSParameter with PrefetcherParams{
  def LD_TRAIN_WIDTH = backendParams.LdExuCnt
  def ST_TRAIN_WIDTH = backendParams.StaExuCnt

  // TODO: change fixed number to option
  val PF_NUM = 2
  val StrideIdx = 0
  val SMSIdx = 1

  val L1_PF_SOURCE_NUM = 1
  val L1_PF_REG_CNT = 1
  val L2_PF_SOURCE_NUM = 2
  val L2_PF_REG_CNT = 2
  val L3_PF_SOURCE_NUM = 1
  val L3_PF_REG_CNT = 4
}

abstract class PrefetchModule(implicit p: Parameters) extends XSModule
  with HasPrefetcherParams with HasDCacheParameters

abstract class PrefetchBundle(implicit p: Parameters) extends XSBundle
  with HasPrefetcherParams with HasDCacheParameters


class PrefetchCtrlFromTile(implicit p: Parameters) extends PrefetchBundle {
  val l2PfqBusy = Bool()
}

class OOOToPrefetchIO(implicit p: Parameters) extends PrefetchBundle {
  // for s1 and only FirstIssue
  // Because of the pointer chase, the PC can only determine whose it is in S1
  val s1_loadPc = Vec(LD_TRAIN_WIDTH, Output(UInt(VAddrBits.W)))
  val s1_storePc = Vec(ST_TRAIN_WIDTH, Output(UInt(VAddrBits.W)))
}

class DCacheToPrefetchIO(implicit p: Parameters) extends PrefetchBundle {
  val sms_agt_evict_req = DecoupledIO(new AGTEvictReq())
}

class TrainSourceIO(implicit p: Parameters) extends PrefetchBundle {
  // load: ldu + hyu
  val s1_loadFireHint = Output(Vec(LD_TRAIN_WIDTH, Bool()))
  val s2_loadFireHint = Output(Vec(LD_TRAIN_WIDTH, Bool()))
  val s3_load = Output(Vec(LD_TRAIN_WIDTH, ValidIO(new LsPrefetchTrainBundle())))
  val s3_ptrChasing = Output(Vec(LD_TRAIN_WIDTH, Bool()))

  // store: stu + hyu
  val s1_storeFireHint = Output(Vec(ST_TRAIN_WIDTH, Bool()))
  val s2_storeFireHint = Output(Vec(ST_TRAIN_WIDTH, Bool()))
  val s3_store = Output(Vec(ST_TRAIN_WIDTH, ValidIO(new LsPrefetchTrainBundle())))
}

class PrefetcherWrapper(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new Bundle() {
    // prefetch control
    val pfCtrlFromTile = Input(new PrefetchCtrlFromTile)
    val pfCtrlFromOOO = Flipped(new PrefetchCtrl)
    val pfCtrlFromDCache = Flipped(new PrefetchControlBundle)
    // replenish information
    val fromDCache = Flipped(new DCacheToPrefetchIO)
    val fromOOO = Flipped(new OOOToPrefetchIO)
    // train
    val trainSource = Flipped(new TrainSourceIO)
    // tlb
    val tlb_req = Vec(PF_NUM, new TlbRequestIO(nRespDups = 2))
    val pmp_resp = Vec(PF_NUM, Flipped(new PMPRespBundle()))
    // prefetch req sender
    val l1_pf_to_l1 = DecoupledIO(new L1PrefetchReq())
    val l1_pf_to_l2 = Output(new coupledL2.PrefetchRecv())
    val l1_pf_to_l3 = Output(new huancun.PrefetchRecv())
  })

  def isLoadAccess(uop: DynInst): Bool = FuType.isLoad(uop.fuType) || FuType.isVLoad(uop.fuType)
  def isStoreAccess(uop: DynInst): Bool = FuType.isStore(uop.fuType)

  val hartId = p(XSCoreParamsKey).HartId
  val l1D_pf_enable = RegNextN(io.pfCtrlFromOOO.l1D_pf_enable, 2, Some(false.B))
  val pf_train_on_hit = RegNextN(io.pfCtrlFromOOO.l1D_pf_train_on_hit, 2, Some(true.B))
  val s2_loadPcVec = (0 until LD_TRAIN_WIDTH).map{ i=>
    RegEnable(io.fromOOO.s1_loadPc(i), io.trainSource.s1_loadFireHint(i))
  }
  val s3_loadPcVec = (0 until LD_TRAIN_WIDTH).map { i =>
    RegEnable(s2_loadPcVec(i), io.trainSource.s2_loadFireHint(i))
  }
  val s2_storePcVec = (0 until ST_TRAIN_WIDTH).map{ i=>
    RegEnable(io.fromOOO.s1_storePc(i), io.trainSource.s1_storeFireHint(i))
  }
  val s3_storePcVec = (0 until ST_TRAIN_WIDTH).map { i =>
    RegEnable(s2_storePcVec(i), io.trainSource.s2_storeFireHint(i))
  }

  /** Prefetchor
    * L1: Stride
    * L2: Stride, SMS
    * L3: Stride
    */
  val smsOpt: Option[SMSPrefetcher] = if(hasSMS) Some(Module(new SMSPrefetcher())) else None
  smsOpt.foreach (pf => {
    // sms.io.enable := Constantin.createRecord(s"enableSMS$hartId", initValue = true)
    val enableSMS = Constantin.createRecord(s"enableSMS$hartId", initValue = true)
    // constantinCtrl && master switch csrCtrl && single switch csrCtrl
    pf.io.enable := enableSMS && l1D_pf_enable &&
        GatedRegNextN(io.pfCtrlFromOOO.l2_pf_recv_enable, 2, Some(false.B))
    pf.io_agt_en := GatedRegNextN(io.pfCtrlFromOOO.l1D_pf_enable_agt, 2, Some(false.B))
    pf.io_pht_en := GatedRegNextN(io.pfCtrlFromOOO.l1D_pf_enable_pht, 2, Some(false.B))
    pf.io_act_threshold := GatedRegNextN(io.pfCtrlFromOOO.l1D_pf_active_threshold, 2, Some(12.U))
    pf.io_act_stride := GatedRegNextN(io.pfCtrlFromOOO.l1D_pf_active_stride, 2, Some(30.U))
    pf.io_stride_en := false.B
    pf.io_dcache_evict <> io.fromDCache.sms_agt_evict_req
    pf.io.l1_req.ready := false.B
    val mbistSmsPl = MbistPipeline.PlaceMbistPipeline(1, "MbistPipeSms", hasMbist)

    for (i <- 0 until LD_TRAIN_WIDTH) {
      val source = io.trainSource.s3_load(i)
      val primaryValid = source.valid && (!source.bits.tlbMiss || source.bits.is_from_hw_pf)
      pf.io.ld_in(i).valid := Mux(
        pf_train_on_hit,
        primaryValid,
        primaryValid && source.bits.isFirstIssue && source.bits.miss
      ) // && isLoadAccess(source.bits.uop)
      pf.io.ld_in(i).bits := source.bits
      pf.io.ld_in(i).bits.uop.pc := Mux(
        io.trainSource.s3_ptrChasing(i),
        s2_loadPcVec(i),
        s3_loadPcVec(i)
      )
    }

    for (i <- 0 until ST_TRAIN_WIDTH) {
      val source = io.trainSource.s3_store(i)
      val primaryValid = source.valid && (!source.bits.tlbMiss || source.bits.is_from_hw_pf)
      pf.io.st_in(i).valid := Mux(
        pf_train_on_hit,
        primaryValid,
        primaryValid && source.bits.isFirstIssue && source.bits.miss
      ) // && isStoreAccess(source.bits.uop)
      pf.io.st_in(i).bits := source.bits
      pf.io.st_in(i).bits.uop.pc := s3_storePcVec(i)
    }

    io.tlb_req(SMSIdx) <> pf.io.tlb_req
    pf.io.pmp_resp := io.pmp_resp(SMSIdx)
  })

  val strideOpt: Option[L1Prefetcher] = if(hasStreamStride) Some(Module(new L1Prefetcher())) else None
  strideOpt.foreach(pf => {
    val enableL1StreamPrefetcher = Constantin.createRecord(s"enableL1StreamPrefetcher$hartId", initValue = true)
      // constantinCtrl && master switch csrCtrl && single switch csrCtrl
    pf.io.enable := enableL1StreamPrefetcher && l1D_pf_enable &&
      GatedRegNextN(io.pfCtrlFromOOO.l1D_pf_enable_stride, 2, Some(false.B))
    pf.pf_ctrl <> io.pfCtrlFromDCache
    pf.l2PfqBusy := io.pfCtrlFromTile.l2PfqBusy

    // stride will train on miss or prefetch hit
    for(i <- 0 until LD_TRAIN_WIDTH){
      val source = io.trainSource.s3_load(i)
      pf.stride_train(i).valid := source.valid && source.bits.isFirstIssue && (
        source.bits.miss || isFromStride(source.bits.meta_prefetch)
      ) // && isLoadAccess(source.bits.uop)
      pf.stride_train(i).bits := source.bits
      pf.stride_train(i).bits.uop.pc := Mux(
        io.trainSource.s3_ptrChasing(i),
        s2_loadPcVec(i),
        s3_loadPcVec(i)
      )
      pf.io.ld_in(i).valid := source.valid && source.bits.isFirstIssue && isLoadAccess(source.bits.uop)
      pf.io.ld_in(i).bits := source.bits
    }

    for (i <- 0 until ST_TRAIN_WIDTH) {
      pf.io.st_in(i).valid := false.B
      pf.io.st_in(i).bits := DontCare
    }

    io.tlb_req(StrideIdx) <> pf.io.tlb_req
    pf.io.pmp_resp := io.pmp_resp(StrideIdx)
  })

  /**
   * load prefetch to l1 Dcache
   * stride
   */
  val l1_pf_arb = Module(new RRArbiterInit(new L1PrefetchReq, L1_PF_SOURCE_NUM))
  if(hasStreamStride) {
    l1_pf_arb.io.in(0) <> strideOpt.get.io.l1_req
  } else {
    l1_pf_arb.io.in(0).valid := false.B
    l1_pf_arb.io.in(0).bits := DontCare
  }
  io.l1_pf_to_l1 <> Pipeline(in = l1_pf_arb.io.out, depth = L1_PF_REG_CNT, pipe = false, name = Some("pf_to_ldu_reg"))

  /** load/store prefetch to l2 cache
   *  stride, sms
   */
  val l2_pf_req = Wire(Decoupled(new L2PrefetchReq()))
  val l2_pf_arb = Module(new RRArbiterInit(new L2PrefetchReq, L2_PF_SOURCE_NUM))
  if(hasStreamStride) {
    l2_pf_arb.io.in(0).valid := strideOpt.get.io.l2_req.valid
    l2_pf_arb.io.in(0).bits := strideOpt.get.io.l2_req.bits
  } else {
    l2_pf_arb.io.in(0).valid := false.B
    l2_pf_arb.io.in(0).bits := DontCare
  }
  if(hasSMS) {
    l2_pf_arb.io.in(1).valid := smsOpt.get.io.l2_req.valid
    l2_pf_arb.io.in(1).bits := smsOpt.get.io.l2_req.bits
  } else {
    l2_pf_arb.io.in(1).valid := false.B
    l2_pf_arb.io.in(1).bits := DontCare
  }
  l2_pf_req <> Pipeline(in = l2_pf_arb.io.out, depth = L2_PF_REG_CNT, pipe = false, name = Some("pf_to_l2cache_reg"))
  l2_pf_req.ready := true.B

  // load/store prefetch to l3 cache
  val l3_pf_req = Wire(Decoupled(new L3PrefetchReq()))
  val l3_pf_arb = Module(new RRArbiterInit(new L3PrefetchReq, L3_PF_SOURCE_NUM))
  if(hasStreamStride) {
    l3_pf_arb.io.in(0).valid := strideOpt.get.io.l3_req.valid
    l3_pf_arb.io.in(0).bits := strideOpt.get.io.l3_req.bits
  } else {
    l3_pf_arb.io.in(0).valid := false.B
    l3_pf_arb.io.in(0).bits := DontCare
  }
  l3_pf_req <> Pipeline(in = l3_pf_arb.io.out, depth = L3_PF_REG_CNT, pipe = false, name = Some("pf_to_l3cache_reg"))
  l3_pf_req.ready := true.B

  io.fromDCache.sms_agt_evict_req.ready := false.B
  if (!hasSMS) {
    io.tlb_req(SMSIdx).req.valid := false.B
    io.tlb_req(SMSIdx).req.bits := false.B
    io.tlb_req(SMSIdx).resp.ready := true.B
  }
  if (!hasStreamStride) {
    io.tlb_req(StrideIdx).req.valid := false.B
    io.tlb_req(StrideIdx).req.bits := false.B
    io.tlb_req(StrideIdx).resp.ready := true.B
  }
  io.l1_pf_to_l2.addr_valid := l2_pf_req.valid
  io.l1_pf_to_l2.addr := l2_pf_req.bits.addr
  io.l1_pf_to_l2.pf_source := l2_pf_req.bits.source
  io.l1_pf_to_l2.l2_pf_en := RegNextN(io.pfCtrlFromOOO.l2_pf_enable, L2_PF_REG_CNT, Some(true.B))
  io.l1_pf_to_l3.addr_valid := l3_pf_req.valid
  io.l1_pf_to_l3.addr := l3_pf_req.bits.addr
  io.l1_pf_to_l3.l2_pf_en := RegNextN(io.pfCtrlFromOOO.l2_pf_enable, L3_PF_REG_CNT, Some(true.B))

  val l2_trace = Wire(new LoadPfDbBundle)
  l2_trace.paddr := l2_pf_req.bits.addr
  val l2_trace_table = ChiselDB.createTable(s"L2PrefetchTrace$hartId", new LoadPfDbBundle, basicDB = false)
  when(l2_pf_req.bits.source === MemReqSource.Prefetch2L2Stream.id.U || l2_pf_req.bits.source === MemReqSource.Prefetch2L2Stride.id.U) {
    l2_trace_table.log(l2_trace, l2_pf_req.valid, "L2StreamStride", clock, reset) 
  }.elsewhen(l2_pf_req.bits.source === MemReqSource.Prefetch2L2SMS.id.U) {
    l2_trace_table.log(l2_trace, l2_pf_req.valid, "L2SMS", clock, reset)
  }.otherwise {
    l2_trace_table.log(l2_trace, l2_pf_req.valid, "L2Unknown", clock, reset)
  }

  val l3_trace = Wire(new LoadPfDbBundle)
  l3_trace.paddr := l3_pf_req.bits.addr
  val l3_trace_table = ChiselDB.createTable(s"L3PrefetchTrace$hartId", new LoadPfDbBundle, basicDB = false)
  when(l3_pf_req.bits.source === MemReqSource.Prefetch2L3Stream.id.U || l3_pf_req.bits.source === MemReqSource.Prefetch2L3Stride.id.U) {
    l3_trace_table.log(l3_trace, l3_pf_req.valid, "L3StreamStride", clock, reset)
  }.otherwise {
    l3_trace_table.log(l3_trace, l3_pf_req.valid, "L3Unknown", clock, reset)
  }

  // FIXME move to Memblock
  // XSPerfAccumulate("prefetch_fire_l2", outer.l2_pf_sender_opt.get.out.head._1.addr_valid)
  // XSPerfAccumulate("prefetch_fire_l3", outer.l3_pf_sender_opt.map(_.out.head._1.addr_valid).getOrElse(false.B))
  // XSPerfAccumulate("l1pf_fire_l2", l1_pf_to_l2.valid)
  // XSPerfAccumulate("sms_fire_l2", !l1_pf_to_l2.valid && sms_pf_to_l2.valid)
  // XSPerfAccumulate("sms_block_by_l1pf", l1_pf_to_l2.valid && sms_pf_to_l2.valid)

}