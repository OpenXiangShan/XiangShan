package xiangshan.mem.prefetch.temporal

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import xiangshan.Redirect
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.rob.RobPtr
import xiangshan.cache.mmu.{TlbCmd, TlbRequestIO}
import xiangshan.mem.{L1PrefetchReq, LqPtr}
import xiangshan.mem.prefetch.{PrefetchBundle, PrefetchModule, PrefetcherParams}

case class L1TemporalParams(
  samplerSets: Int = 128,
  samplerWays: Int = 16,
  recorderSets: Int = 128,
  confWays: Int = 8
) extends PrefetcherParams {
  override def name: String = "L1Temporal"
}

class L1TemporalLoad(implicit p: Parameters) extends PrefetchBundle {
  val robIdx = new RobPtr
  val lqIdx = new LqPtr
  val pc = UInt(VAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
  val miss = Bool()
  val kind = UInt(2.W)
  val mshr = Option.when(hasL1Lldp)(UInt(log2Up(cfg.nMissEntries).W))
  val missAccepted = Option.when(hasL1Lldp)(Bool())
  val way = Option.when(hasL1Lldp)(UInt(nWays.W))
}

object L1TemporalPrefetcher {
  val ContextBits = 50
}

class L1TemporalPrefetcher(params: L1TemporalParams)(implicit p: Parameters) extends PrefetchModule {
  import L1TemporalPrefetcher.ContextBits

  private val lineBits = PAddrBits - blockOffBits
  private val aliasBits = cacheParams.aliasBitsOpt.getOrElse(1)
  private val candidateType = new TemporalCandidate(lineBits, aliasBits, ContextBits)

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val context = Input(UInt(ContextBits.W))
    val contextChanged = Input(Bool())
    val redirect = Flipped(Valid(new Redirect))
    val allocate = Flipped(Vec(LSQEnqWidth, Valid(new LqPtr)))
    val load = Flipped(Vec(LD_TRAIN_WIDTH, Valid(new L1TemporalLoad)))
    val role = Flipped(Decoupled(new TemporalRole(VAddrBits, ContextBits)))
    val feedback = Flipped(Decoupled(new TemporalFeedback(ContextBits)))
    val tlb = new TlbRequestIO(nRespDups = 2)
    val pmp = Flipped(new PMPRespBundle)
    val request = Decoupled(new L1PrefetchReq)
  })

  private val engine = Module(new TemporalEngine(
    lineBits, aliasBits, VAddrBits, ContextBits,
    params.samplerSets, params.samplerWays, params.recorderSets, params.confWays
  ))
  engine.io.role <> io.role
  engine.io.feedback <> io.feedback
  engine.io.generationWrap.ready := true.B

  // Clearing on LQ allocation also covers a redirect that reuses the same wrapped pointer.
  private val seen = RegInit(VecInit.fill(VirtualLoadQueueSize)(false.B))
  private val samples = Seq.fill(LD_TRAIN_WIDTH)(Module(new Queue(new L1TemporalLoad, 16, hasFlush = true)))
  private val sampleArb = Module(new Arbiter(new L1TemporalLoad, LD_TRAIN_WIDTH))
  private val flushSamples = io.redirect.valid || io.contextChanged || !io.enable

  for (alloc <- io.allocate) {
    when(alloc.valid) {
      seen(alloc.bits.value) := false.B
    }
  }
  for ((queue, lane) <- samples.zipWithIndex) {
    val load = io.load(lane)
    val alreadyQueued = samples.take(lane).zip(io.load.take(lane)).map { case (older, other) =>
      older.io.enq.fire && other.bits.lqIdx.asUInt === load.bits.lqIdx.asUInt
    }.foldLeft(false.B)(_ || _)
    queue.io.flush.get := flushSamples
    queue.io.enq.valid := load.valid && !flushSamples && !seen(load.bits.lqIdx.value) && !alreadyQueued
    queue.io.enq.bits := load.bits
    sampleArb.io.in(lane) <> queue.io.deq
    when(queue.io.enq.fire) {
      seen(load.bits.lqIdx.value) := true.B
    }
    XSPerfAccumulate(s"tp_load_queue_full_$lane", queue.io.enq.valid && !queue.io.enq.ready)
  }

  engine.io.load.valid := sampleArb.io.out.valid && !flushSamples
  engine.io.load.bits.pc := sampleArb.io.out.bits.pc
  engine.io.load.bits.context := io.context
  engine.io.load.bits.kind := sampleArb.io.out.bits.kind
  engine.io.load.bits.target.line := sampleArb.io.out.bits.paddr >> blockOffBits
  engine.io.load.bits.target.alias := get_alias(sampleArb.io.out.bits.vaddr)
  sampleArb.io.out.ready := engine.io.load.ready && !flushSamples

  private val candidates = Module(new Queue(candidateType, 16))
  candidates.io.enq <> engine.io.candidate
  private val candidate = candidates.io.deq
  private val pending = Reg(candidateType)
  private val idle :: waitTlb :: waitPmp :: send :: Nil = Enum(4)
  private val state = RegInit(idle)
  private val killed = RegInit(false.B)
  private val paddr = Cat(pending.target.line, 0.U(blockOffBits.W))
  private val vaddr = if (cacheParams.aliasBitsOpt.isDefined) {
    Cat(0.U((VAddrBits - pgIdxBits - aliasBits).W), pending.target.alias, paddr(pgIdxBits - 1, 0))
  } else {
    paddr.pad(VAddrBits)
  }
  private val candidatePaddr = Cat(candidate.bits.target.line, 0.U(blockOffBits.W))
  private val inMemory = PmemRanges.map(_.cover(candidatePaddr)).reduce(_ || _)
  private val active = io.enable && !io.contextChanged
  private val discard = !active || !inMemory || candidate.bits.owner.context =/= io.context
  private val cancel = killed || !active || pending.owner.context =/= io.context

  io.tlb.req.valid := state === idle && candidate.valid && !discard
  io.tlb.req.bits := 0.U.asTypeOf(io.tlb.req.bits)
  io.tlb.req.bits.cmd := TlbCmd.read
  io.tlb.req.bits.size := 3.U
  io.tlb.req.bits.isPrefetch := true.B
  io.tlb.req.bits.no_translate := io.tlb.req.valid
  io.tlb.req.bits.pmp_addr := paddr
  io.tlb.req_kill := false.B
  io.tlb.resp.ready := true.B
  candidate.ready := state === idle && (discard || io.tlb.req.ready)
  io.request.valid := state === send && !cancel
  io.request.bits.paddr := paddr
  io.request.bits.vaddr := vaddr
  io.request.bits.confidence := 0.U
  io.request.bits.is_store := false.B
  io.request.bits.pf_source.value := L1_HW_PREFETCH_TEMPORAL
  io.request.bits.temporalOwner.get := pending.owner
  io.request.bits.lldpTrigger.foreach(trigger => trigger := 0.U.asTypeOf(trigger))

  when(state =/= idle && !active) {
    killed := true.B
  }
  when(io.tlb.req.fire) {
    pending := candidate.bits
    killed := false.B
    state := waitTlb
  }
  when(state === waitTlb && io.tlb.resp.fire) {
    val excp = io.tlb.resp.bits.excp.head
    val denied = io.tlb.resp.bits.miss || excp.pf.ld || excp.af.ld || excp.gpf.ld
    state := Mux(denied || cancel, idle, waitPmp)
  }
  when(state === waitPmp) {
    state := Mux(io.pmp.ld || io.pmp.mmio || cancel, idle, send)
  }
  when(state === send && (io.request.fire || cancel)) {
    state := idle
  }

  XSPerfAccumulate("tp_observe", engine.io.load.fire)
  XSPerfAccumulate("tp_pair", engine.io.trainPair)
  XSPerfAccumulate("tp_install", engine.io.install)
  XSPerfAccumulate("tp_meta_hit", engine.io.metaHit)
  XSPerfAccumulate("tp_bypass", engine.io.bypass)
  XSPerfAccumulate("tp_candidate", engine.io.candidate.fire)
  XSPerfAccumulate("tp_candidate_drop", candidate.fire && discard)
  XSPerfAccumulate("tp_pmp_drop", state === waitPmp && (io.pmp.ld || io.pmp.mmio))
  XSPerfAccumulate("tp_l1_fire", io.request.fire)
}
