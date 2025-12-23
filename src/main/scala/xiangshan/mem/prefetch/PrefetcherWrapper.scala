package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import utility.mbist.MbistPipeline
import utils._
import xiangshan._
import xiangshan.backend.Bundles.DynInst
import xiangshan.backend.fu.{FuType, PMPRespBundle}
import xiangshan.cache.mmu._
import xiangshan.cache.{HasDCacheParameters, LoadPfDbBundle}
import xiangshan.mem.Bundles.LsPrefetchTrainBundle
import xiangshan.mem._
import xiangshan.mem.trace._

object TLBPlace extends Enumeration{
  /* now only support dtlb_ld and dtlb_st */
  // val none = Value("none")
  // val itlb = Value("itlb")
  val dtlb_ld = Value("dtlb_ld")
  // val dtlb_st = Value("dtlb_st")
  val dtlb_pf = Value("dtlb_pf")
  // val l2tlb = Value("l2tlb")
}

trait PrefetcherParams {
  def name: String
  def tlbPlace = TLBPlace.dtlb_pf
  def TRAIN_FILTER_SIZE = 6
  def PREFETCH_FILTER_SIZE = 16
}

trait HasPrefetcherParams extends HasXSParameter{
  def LD_TRAIN_WIDTH = backendParams.LdExuCnt
  def ST_TRAIN_WIDTH = backendParams.StaExuCnt

  // You can set the unified interface to N, and the invalid that you don't need can be set to 0
  val L1_PF_REG_CNT = 1
  val L2_PF_REG_CNT = 2
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
  val refillTrain = ValidIO(new TrainReqBundle)
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
  println(s"PrefetcherWrapper:")
  prefetcherSeq.zipWithIndex.foreach { case (pf, idx) =>
    println(f"  Prefetcher #$idx%-2d => ${pf.name}")
  }
  val io = IO(new Bundle() {
    // prefetch control
    val pfCtrlFromTile = Input(new PrefetchCtrlFromTile)
    val pfCtrlFromCSR = Flipped(new PrefetchCtrl)
    val pfCtrlFromDCache = Input(Vec(L1PrefetcherNum, new PrefetchControlBundle))
    // replenish information
    val fromDCache = Flipped(new DCacheToPrefetchIO)
    val fromOOO = Flipped(new OOOToPrefetchIO)
    // train
    val trainSource = Flipped(new TrainSourceIO)
    // tlb
    val tlb_req = Vec(prefetcherNum, new TlbRequestIO(nRespDups = 2))
    val pmp_resp = Vec(prefetcherNum, Flipped(new PMPRespBundle()))
    // prefetch req sender
    val l1_pf_to_l1 = DecoupledIO(new L1PrefetchReq())
    val l1_pf_to_l2 = Output(new coupledL2.PrefetchRecv())
    val l1_pf_to_l3 = Output(new huancun.PrefetchRecv())
  })

  def isLoadAccess(uop: DynInst): Bool = FuType.isLoad(uop.fuType) || FuType.isVLoad(uop.fuType)
  def isStoreAccess(uop: DynInst): Bool = FuType.isStore(uop.fuType)

  val hartId = p(XSCoreParamsKey).HartId
  val l1D_pf_enable = RegNextN(io.pfCtrlFromCSR.l1D_pf_enable, 2, Some(true.B))
  val pf_train_on_hit = RegNextN(io.pfCtrlFromCSR.l1D_pf_train_on_hit, 2, Some(true.B))
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
  /* prefetch arbiter */
  val l1_pf_arb = Module(new Arbiter(new L1PrefetchReq, prefetcherNum))
  val l2_pf_req = Wire(Decoupled(new L2PrefetchReq()))
  val l2_pf_arb = Module(new Arbiter(new L2PrefetchReq, prefetcherNum))
  val l3_pf_req = Wire(Decoupled(new L3PrefetchReq()))
  val l3_pf_arb = Module(new Arbiter(new L3PrefetchReq, prefetcherNum))

  // init
  io.tlb_req.foreach{ x =>
    x.req.valid := false.B
    x.req_kill := false.B
    x.req.bits := DontCare
    x.resp.ready := true.B
  }
  l1_pf_arb.io.in.foreach{ x =>
    x.valid := false.B
    x.bits := DontCare
  }
  l2_pf_arb.io.in.foreach{ x =>
    x.valid := false.B
    x.bits := DontCare
  }
  l3_pf_arb.io.in.foreach{ x =>
    x.valid := false.B
    x.bits := DontCare
  }


  /** Prefetchor
    * L1: Stride, Berti
    * L2: Stride, SMS, Berti
    * L3: Stride, Berti
    */
  val HasSMS = prefetcherSeq.exists(_.isInstanceOf[SMSParams])
  val IdxSMS = prefetcherSeq.indexWhere(_.isInstanceOf[SMSParams])
  val HasStreamStride = prefetcherSeq.exists(_.isInstanceOf[StreamStrideParams])
  val IdxStreamStride = prefetcherSeq.indexWhere(_.isInstanceOf[StreamStrideParams])
  val HasBerti = prefetcherSeq.exists(_.isInstanceOf[BertiParams])
  val IdxBerti = prefetcherSeq.indexWhere(_.isInstanceOf[BertiParams])

  private val Seq(bothOff, strideOnBertiOff, strideOffBertiOn, bothOn) = Seq(0, 1, 2, 3)
  val modeStrideBerti = Constantin.createRecord(s"pf_modeStrideBerti$hartId", initValue = strideOffBertiOn)
  val strideModeEnable = modeStrideBerti =/= bothOff.U && !(modeStrideBerti === strideOffBertiOn.U && HasBerti.B)
  val bertiModeEnable = modeStrideBerti =/= bothOff.U && !(modeStrideBerti === strideOnBertiOff.U && HasStreamStride.B)

  val smsOpt: Option[SMSPrefetcher] = if(HasSMS) Some(Module(new SMSPrefetcher())) else None
  smsOpt.foreach (pf => {
    val enableSMS = Constantin.createRecord(s"pf_enableSMS$hartId", initValue = true)
    // constantinCtrl && master switch csrCtrl && single switch csrCtrl
    pf.io.enable := enableSMS && l1D_pf_enable &&
      GatedRegNextN(io.pfCtrlFromCSR.l2_pf_recv_enable, 2, Some(false.B))
    pf.io_agt_en := GatedRegNextN(io.pfCtrlFromCSR.l1D_pf_enable_agt, 2, Some(false.B))
    pf.io_pht_en := GatedRegNextN(io.pfCtrlFromCSR.l1D_pf_enable_pht, 2, Some(false.B))
    pf.io_act_threshold := GatedRegNextN(io.pfCtrlFromCSR.l1D_pf_active_threshold, 2, Some(12.U))
    pf.io_act_stride := GatedRegNextN(io.pfCtrlFromCSR.l1D_pf_active_stride, 2, Some(30.U))
    pf.io_stride_en := false.B
    pf.io_dcache_evict <> io.fromDCache.sms_agt_evict_req
    val mbistSmsPl = MbistPipeline.PlaceMbistPipeline(1, "MbistPipeSms", hasMbist)

    for (i <- 0 until LD_TRAIN_WIDTH) {
      val source = io.trainSource.s3_load(i)
      val primaryValid = source.valid && !source.bits.tlbMiss && !source.bits.is_from_hw_pf
      pf.io.ld_in(i).valid := Mux(
        pf_train_on_hit,
        primaryValid,
        primaryValid && source.bits.isFirstIssue && (source.bits.miss || isFromL1Prefetch(source.bits.meta_prefetch))
      )
      pf.io.ld_in(i).bits := source.bits
      pf.io.ld_in(i).bits.uop.pc := Mux(
        io.trainSource.s3_ptrChasing(i),
        s2_loadPcVec(i),
        s3_loadPcVec(i)
      )
    }

    for (i <- 0 until ST_TRAIN_WIDTH) {
      val source = io.trainSource.s3_store(i)
      val primaryValid = source.valid && !source.bits.tlbMiss && !source.bits.is_from_hw_pf
      pf.io.st_in(i).valid := Mux(
        pf_train_on_hit,
        primaryValid,
        primaryValid && source.bits.isFirstIssue && (source.bits.miss || isFromL1Prefetch(source.bits.meta_prefetch))
      )
      pf.io.st_in(i).bits := source.bits
      pf.io.st_in(i).bits.uop.pc := s3_storePcVec(i)
    }

    io.tlb_req(IdxSMS) <> pf.io.tlb_req
    pf.io.pmp_resp := io.pmp_resp(IdxSMS)

    l2_pf_arb.io.in(IdxSMS) <> pf.io.l2_req
    pf.io.l1_req.ready := false.B
    pf.io.l3_req.ready := false.B
  })

  val strideOpt: Option[L1Prefetcher] = if(HasStreamStride) Some(Module(new L1Prefetcher())) else None
  strideOpt.foreach(pf => {
    val enableL1StreamPrefetcher = Constantin.createRecord(s"pf_enableL1StreamPrefetcher$hartId", initValue = true)
    // constantinCtrl && master switch csrCtrl && single switch csrCtrl
    pf.io.enable := enableL1StreamPrefetcher && l1D_pf_enable &&
      GatedRegNextN(io.pfCtrlFromCSR.l1D_pf_enable_stride, 2, Some(false.B))

    pf.pf_ctrl <> io.pfCtrlFromDCache
    pf.l2PfqBusy := io.pfCtrlFromTile.l2PfqBusy
    pf.strideEnable := strideModeEnable

    // stride will train on miss or prefetch hit
    for(i <- 0 until LD_TRAIN_WIDTH){
      // for stride
      val source = io.trainSource.s3_load(i)
      pf.stride_train(i).valid := source.valid && source.bits.isFirstIssue && (
        source.bits.miss || isFromL1Prefetch(source.bits.meta_prefetch)
      ) && !source.bits.is_from_hw_pf
      pf.stride_train(i).bits := source.bits
      pf.stride_train(i).bits.uop.pc := Mux(
        io.trainSource.s3_ptrChasing(i),
        s2_loadPcVec(i),
        s3_loadPcVec(i)
      )
      // for stream
      pf.io.ld_in(i).valid := source.valid && source.bits.isFirstIssue && !source.bits.is_from_hw_pf
      pf.io.ld_in(i).bits := source.bits
    }

    for (i <- 0 until ST_TRAIN_WIDTH) {
      pf.io.st_in(i).valid := false.B
      pf.io.st_in(i).bits := DontCare
    }

    io.tlb_req(IdxStreamStride) <> pf.io.tlb_req
    pf.io.pmp_resp := io.pmp_resp(IdxStreamStride)

    l1_pf_arb.io.in(IdxStreamStride) <> pf.io.l1_req
    l2_pf_arb.io.in(IdxStreamStride) <> pf.io.l2_req
    l3_pf_arb.io.in(IdxStreamStride) <> pf.io.l3_req
  })

  val bertiOpt: Option[BertiPrefetcher] = if(HasBerti) Some(Module(new BertiPrefetcher())) else None
  bertiOpt.foreach(pf => {
    val enableBerti = Constantin.createRecord(s"pf_enableBerti$hartId", initValue = true)
    // constantinCtrl && master switch csrCtrl && single switch csrCtrl
    pf.io.enable := enableBerti && l1D_pf_enable &&
      GatedRegNextN(io.pfCtrlFromCSR.berti_enable, 2, Some(false.B)) &&
      bertiModeEnable

    for(i <- 0 until LD_TRAIN_WIDTH){
      val source = io.trainSource.s3_load(i)
      pf.io.ld_in(i).valid := source.valid && source.bits.isFirstIssue && (
        source.bits.miss || isFromL1Prefetch(source.bits.meta_prefetch)
      ) && !source.bits.is_from_hw_pf
      pf.io.ld_in(i).bits := source.bits
      pf.io.ld_in(i).bits.uop.pc := Mux(
        io.trainSource.s3_ptrChasing(i),
        s2_loadPcVec(i),
        s3_loadPcVec(i)
      )
    }

    for (i <- 0 until ST_TRAIN_WIDTH) {
      pf.io.st_in(i).valid := false.B
      pf.io.st_in(i).bits := DontCare
      // val source = io.trainSource.s3_store(i)
      // pf.io.st_in(i).valid := source.valid && source.bits.isFirstIssue && (
      //  source.bits.miss || isFromBerti(source.bits.meta_prefetch)
      // )
      // pf.io.st_in(i).bits := source.bits
      // pf.io.st_in(i).bits.uop.pc := s3_storePcVec(i)
    }

    pf.io.refillTrain := io.fromDCache.refillTrain

    io.tlb_req(IdxBerti) <> pf.io.tlb_req
    pf.io.pmp_resp := io.pmp_resp(IdxBerti)

    l1_pf_arb.io.in(IdxBerti) <> pf.io.l1_req
    l2_pf_arb.io.in(IdxBerti) <> pf.io.l2_req
    l3_pf_arb.io.in(IdxBerti) <> pf.io.l3_req

  })

  /**
   * load prefetch to l1 Dcache
   * stride
   */
  io.l1_pf_to_l1 <> Pipeline(in = l1_pf_arb.io.out, depth = L1_PF_REG_CNT, name = Some("pf_to_ldu_reg"))

  /** load/store prefetch to l2 cache
   *  stride, sms
   */
  l2_pf_req <> Pipeline(in = l2_pf_arb.io.out, depth = L2_PF_REG_CNT, name = Some("pf_to_l2cache_reg"))
  l2_pf_req.ready := true.B

  // load/store prefetch to l3 cache
  l3_pf_req <> Pipeline(in = l3_pf_arb.io.out, depth = L3_PF_REG_CNT, name = Some("pf_to_l3cache_reg"))
  l3_pf_req.ready := true.B

  io.fromDCache.sms_agt_evict_req.ready := false.B

  io.l1_pf_to_l2.addr_valid := l2_pf_req.valid
  io.l1_pf_to_l2.addr := l2_pf_req.bits.addr
  io.l1_pf_to_l2.pf_source := l2_pf_req.bits.source
  io.l1_pf_to_l2.l2_pf_en := RegNextN(io.pfCtrlFromCSR.l2_pf_enable, L2_PF_REG_CNT, Some(true.B))
  io.l1_pf_to_l3.addr_valid := l3_pf_req.valid
  io.l1_pf_to_l3.addr := l3_pf_req.bits.addr
  io.l1_pf_to_l3.l2_pf_en := RegNextN(io.pfCtrlFromCSR.l2_pf_enable, L3_PF_REG_CNT, Some(true.B))

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

  val arb_seq = Seq(l1_pf_arb, l2_pf_arb, l3_pf_arb)
  arb_seq.zipWithIndex.foreach { case (arb, level) =>
    prefetcherSeq.zipWithIndex.foreach { case (pf, idx) =>
      XSPerfAccumulate(s"${pf.name}_fire_l${level+1}", arb.io.in(idx).fire)
      XSPerfAccumulate(s"${pf.name}_block_l${level+1}", arb.io.in(idx).valid && !arb.io.in(idx).ready)
    }
  }

}
