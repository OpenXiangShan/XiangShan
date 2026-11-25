package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import xiangshan.Redirect
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.cache.MissTrackInvalidate
import xiangshan.cache.mmu.{Pbmt, TlbCmd, TlbRequestIO}
import xiangshan.mem.L1PrefetchReq
import xiangshan.mem.prefetch.temporal.{L1TemporalLoad, L1TemporalPrefetcher, TemporalRole}

private[prefetch] class L1LldpPrefetcher(implicit p: Parameters) extends PrefetchModule {
  import L1TemporalPrefetcher.ContextBits

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val context = Input(UInt(ContextBits.W))
    val contextChanged = Input(Bool())
    val train = Flipped(Vec(LD_TRAIN_WIDTH, Valid(new LldpDependence(VAddrBits))))
    val demand = Flipped(Vec(LD_TRAIN_WIDTH, Valid(new Bundle {
      val load = new L1TemporalLoad
      val info = new LldpLoadInfo
    })))
    val refill = Flipped(Valid(new LldpRefill))
    val prefetch = Flipped(Valid(new LldpPrefetchResult))
    val invalidate = Flipped(Valid(new MissTrackInvalidate))
    val addressTrain = Flipped(Vec(LD_TRAIN_WIDTH, Valid(new LldpAddressTrain)))
    val redirect = Flipped(Valid(new Redirect))
    val role = Decoupled(new TemporalRole(VAddrBits, ContextBits))
    val tlb = new TlbRequestIO(nRespDups = 2)
    val pmp = Flipped(new PMPRespBundle)
    val request = Decoupled(new L1PrefetchReq)
  })

  private val active = io.enable && !io.contextChanged
  private val table = Module(new LldpTable(VAddrBits, ContextBits, blockOffBits))
  private val aliasBits = cacheParams.aliasBitsOpt.getOrElse(1)
  private val address = Module(new LldpAddressPredictor(VAddrBits, PAddrBits, ContextBits, aliasBits))
  private val owners = Module(new LldpAddressOwners)
  owners.io.context := io.context
  owners.io.prefetch := io.prefetch
  owners.io.refill.bits := io.refill.bits
  owners.io.refill.valid := io.refill.valid && !io.refill.bits.chainHit.valid
  owners.io.invalidate := io.invalidate
  for ((load, demand) <- owners.io.demand.zip(io.demand)) {
    load.valid := demand.valid && active
    load.bits := demand.bits.load
  }
  address.io.feedback <> owners.io.feedback
  private val trainArb = Module(new Arbiter(new LldpDependence(VAddrBits), LD_TRAIN_WIDTH))
  for ((input, sample) <- trainArb.io.in.zip(io.train)) {
    input.valid := sample.valid && active
    input.bits := sample.bits
  }
  table.io.train.valid := trainArb.io.out.valid
  table.io.train.bits.dependence := trainArb.io.out.bits
  table.io.train.bits.context := io.context
  trainArb.io.out.ready := table.io.train.ready
  io.role <> table.io.role

  private val demands = VecInit(io.demand.map(_.valid))
  private val demandMisses = VecInit(io.demand.map(demand => demand.valid && demand.bits.load.missAccepted.get))
  private val lookupMask = Mux(demandMisses.asUInt.orR, demandMisses, demands)
  private val lookup = PriorityMux(lookupMask, io.demand.map(_.bits))
  private val prefetch = io.prefetch.bits
  private val refill = io.refill.bits
  private val chainHit = io.refill.valid && refill.chainHit.valid && refill.chainHit.bits.context === io.context
  private val prefetchLookup = io.prefetch.valid && !prefetch.dataHit && prefetch.trigger.context === io.context
  private val demandLookup = !chainHit && (demandMisses.asUInt.orR || (demands.asUInt.orR && !prefetchLookup))
  private val trigger = Mux(chainHit, refill.chainHit.bits, prefetch.trigger)
  table.io.lookup.valid := active && (demandLookup || prefetchLookup || chainHit)
  table.io.lookup.bits.pc := Mux(demandLookup, lookup.info.pc, trigger.pc)
  table.io.lookup.bits.context := io.context
  table.io.lookup.bits.dataType := Mux(demandLookup, lookup.info.dataType, trigger.dataType)
  table.io.lookup.bits.offset := Mux(demandLookup, lookup.load.paddr(blockOffBits - 1, 0), trigger.offset)
  table.io.lookup.bits.source := Mux(demandLookup, LldpValueSource.Demand,
    Mux(chainHit || prefetch.fromLldp, LldpValueSource.Chain, LldpValueSource.Spatial))
  table.io.lookup.bits.depth := !demandLookup && trigger.depth

  private val lookupMshr = Mux(demandLookup, lookup.load.mshr.get, prefetch.mshr)
  private val lookupBlock = get_block_addr(Mux(demandLookup, lookup.load.paddr, Mux(chainHit, refill.paddr, prefetch.paddr)))
  private val missForHint = !chainHit && Mux(demandLookup, lookup.load.missAccepted.get, prefetch.allocated)
  private val addressRule = table.io.addressRule
  address.io.lookup.valid := addressRule.valid
  address.io.lookup.bits.producer := table.io.lookup.bits.pc
  address.io.lookup.bits.consumer := addressRule.bits.consumer
  address.io.lookup.bits.context := io.context
  address.io.lookup.bits.producerAddress := lookupBlock | table.io.lookup.bits.offset
  address.io.lookup.bits.ruleGeneration := addressRule.bits.generation
  address.io.observe := PopCount(demands.asUInt) +& prefetchLookup +& chainHit

  private val addressArb = Module(new Arbiter(new LldpAddressTrain, LD_TRAIN_WIDTH))
  for ((port, sample) <- addressArb.io.in.zip(io.addressTrain)) {
    port.valid := active && sample.valid
    port.bits := sample.bits
  }
  private val addressQueue = Module(new Queue(new Bundle {
    val train = new LldpAddressTrain
    val retries = UInt(7.W)
  }, 64, pipe = true, hasFlush = true))
  addressQueue.io.flush.get := !active || io.redirect.valid
  private val sample = addressQueue.io.deq.bits
  table.io.addressTrainKey.producer := sample.train.producer
  table.io.addressTrainKey.consumer := sample.train.consumer
  table.io.addressTrainKey.context := io.context
  private val generation = table.io.addressTrainGeneration
  private val retrySample = addressQueue.io.deq.valid && !generation.valid && sample.retries < 68.U
  addressQueue.io.enq.valid := !addressQueue.io.flush.get && (retrySample || addressArb.io.out.valid)
  addressQueue.io.enq.bits.train := Mux(retrySample, sample.train, addressArb.io.out.bits)
  addressQueue.io.enq.bits.retries := Mux(retrySample, sample.retries + 1.U, 0.U)
  addressArb.io.out.ready := addressQueue.io.enq.ready && !retrySample && !addressQueue.io.flush.get
  addressQueue.io.deq.ready := !generation.valid || address.io.train.ready
  address.io.train.valid := addressQueue.io.deq.valid && generation.valid && !addressQueue.io.flush.get
  address.io.train.bits.key.producer := sample.train.producer
  address.io.train.bits.key.consumer := sample.train.consumer
  address.io.train.bits.key.context := io.context
  address.io.train.bits.key.producerAddress := sample.train.producerAddress
  address.io.train.bits.key.ruleGeneration := generation.bits
  address.io.train.bits.target.paddr := sample.train.consumerAddress
  address.io.train.bits.target.alias := get_alias(sample.train.consumerVaddr)

  private class Candidate extends Bundle {
    val vaddr = UInt(64.W)
    val paddr = UInt(PAddrBits.W)
    val context = UInt(ContextBits.W)
    val consumer = UInt(VAddrBits.W)
    val dataType = new LldpDataType
    val source = LldpValueSource()
    val addressRef = Valid(new LldpAddressRef)
  }
  private val candidates = Module(new Queue(new Candidate, 16))
  private val physical = Module(new Queue(new Candidate, 8))
  private val addressSource = RegEnable(table.io.lookup.bits.source, address.io.lookup.fire)
  private val addressConsumer = RegEnable(addressRule.bits, address.io.lookup.fire)
  private val addressContext = RegEnable(io.context, address.io.lookup.fire)
  private val physicalTarget = address.io.candidate.bits.target
  physical.io.enq.valid := address.io.candidate.valid && active && addressContext === io.context
  physical.io.enq.bits.paddr := physicalTarget.paddr
  physical.io.enq.bits.vaddr := (if (cacheParams.aliasBitsOpt.isDefined) {
    Cat(physicalTarget.alias, physicalTarget.paddr(pgIdxBits - 1, 0))
  } else {
    physicalTarget.paddr
  })
  physical.io.enq.bits.context := addressContext
  physical.io.enq.bits.consumer := addressConsumer.consumer
  physical.io.enq.bits.dataType := addressConsumer.dataType
  physical.io.enq.bits.source := addressSource
  physical.io.enq.bits.addressRef.valid := true.B
  physical.io.enq.bits.addressRef.bits := address.io.candidate.bits.ref
  address.io.admit := physical.io.enq.fire

  candidates.io.enq.valid := table.io.candidate.valid
  table.io.candidate.ready := candidates.io.enq.ready
  candidates.io.enq.bits.vaddr := table.io.candidate.bits.vaddr
  candidates.io.enq.bits.paddr := DontCare
  candidates.io.enq.bits.context := table.io.candidate.bits.context
  candidates.io.enq.bits.consumer := table.io.candidate.bits.consumer
  candidates.io.enq.bits.dataType := table.io.candidate.bits.dataType
  candidates.io.enq.bits.source := table.io.candidate.bits.source
  candidates.io.enq.bits.addressRef.valid := false.B
  candidates.io.enq.bits.addressRef.bits := DontCare

  // One triggering demand per MSHR. The block check prevents a recycled MSHR
  // from consuming a hint that belongs to a different refill.
  private val valid = RegInit(VecInit.fill(cfg.nMissEntries)(false.B))
  private val hints = Reg(Vec(cfg.nMissEntries, chiselTypeOf(table.io.hint.bits)))
  private val blocks = Reg(Vec(cfg.nMissEntries, UInt((PAddrBits - blockOffBits).W)))
  private val contexts = Reg(Vec(cfg.nMissEntries, UInt(ContextBits.W)))
  private val fillValid = io.refill.valid && !refill.chainHit.valid
  private val nextHint = RegEnable(table.io.hint.bits, table.io.hint.valid && missForHint)
  private val nextMshr = RegEnable(lookupMshr, table.io.hint.valid && missForHint)
  private val nextBlock = RegEnable(lookupBlock >> blockOffBits, table.io.hint.valid && missForHint)
  private val nextContext = RegEnable(io.context, table.io.hint.valid && missForHint)
  private val hintEligible = RegNext(table.io.hint.valid && missForHint, false.B) && !physical.io.enq.fire &&
    nextContext === io.context && !valid(nextMshr)
  private val bypassHint = hintEligible && nextMshr === refill.mshr && nextBlock === (refill.paddr >> blockOffBits)
  private val refillMatches = bypassHint || (valid(refill.mshr) &&
    blocks(refill.mshr) === (refill.paddr >> blockOffBits) && contexts(refill.mshr) === io.context
  )
  private val hint = Mux(bypassHint, nextHint, hints(refill.mshr))
  private val returned = Module(new Queue(chiselTypeOf(table.io.returned.bits), 4, hasFlush = true))
  returned.io.flush.get := !active
  returned.io.enq.valid := active && fillValid && refill.readable && refillMatches
  returned.io.enq.bits.hint := hint
  returned.io.enq.bits.bytes := (refill.data >> Cat(hint.offset, 0.U(3.W)))(63, 0)
  private val hitLookup = table.io.hint.valid && chainHit
  private val hitReturns = Module(new Queue(chiselTypeOf(table.io.returned.bits), 4, hasFlush = true))
  hitReturns.io.flush.get := !active
  hitReturns.io.enq.valid := active && RegNext(hitLookup, false.B) && !physical.io.enq.fire
  hitReturns.io.enq.bits.hint := RegEnable(table.io.hint.bits, hitLookup)
  hitReturns.io.enq.bits.bytes := RegEnable((refill.data >> Cat(trigger.offset, 0.U(3.W)))(63, 0), hitLookup)

  private val returnArb = Module(new RRArbiter(chiselTypeOf(table.io.returned.bits), 2))
  returnArb.io.in(0) <> returned.io.deq
  returnArb.io.in(1) <> hitReturns.io.deq
  table.io.returned <> returnArb.io.out

  when(fillValid) {
    valid(refill.mshr) := false.B
  }
  when(hintEligible && !(fillValid && nextMshr === refill.mshr)) {
    valid(nextMshr) := true.B
    hints(nextMshr) := nextHint
    blocks(nextMshr) := nextBlock
    contexts(nextMshr) := nextContext
  }
  when(!active) {
    valid.foreach(_ := false.B)
  }

  private val arbitrate = Module(new RRArbiter(new Candidate, 2))
  arbitrate.io.in(0) <> candidates.io.deq
  arbitrate.io.in(1) <> physical.io.deq
  private val candidate = arbitrate.io.out
  private val pending = Reg(chiselTypeOf(candidate.bits))
  private val paddr = Reg(UInt(PAddrBits.W))
  private val retries = RegInit(0.U(6.W))
  private val killed = RegInit(false.B)
  private val idle :: translate :: waitTlb :: waitPmp :: retry :: send :: Nil = Enum(6)
  private val state = RegInit(idle)
  private val cancel = killed || !active || pending.context =/= io.context
  private val inMemory = PmemRanges.map(_.cover(paddr)).reduce(_ || _)
  private val staleCandidate = !active || candidate.bits.context =/= io.context
  private val failed = WireDefault(false.B)

  candidate.ready := state === idle
  owners.io.dropped(0).valid := candidate.fire && staleCandidate && candidate.bits.addressRef.valid
  owners.io.dropped(0).bits := candidate.bits.addressRef.bits
  owners.io.dropped(1).valid := failed && pending.addressRef.valid
  owners.io.dropped(1).bits := pending.addressRef.bits
  when(candidate.fire && !staleCandidate) {
    pending := candidate.bits
    retries := 0.U
    killed := false.B
    state := translate
  }
  when(state =/= idle && !active) {
    killed := true.B
  }

  io.tlb.req.valid := state === translate && !cancel
  io.tlb.req.bits := 0.U.asTypeOf(io.tlb.req.bits)
  io.tlb.req.bits.fullva := pending.vaddr
  io.tlb.req.bits.vaddr := pending.vaddr(VAddrBits - 1, 0)
  io.tlb.req.bits.checkfullva := !pending.addressRef.valid
  io.tlb.req.bits.cmd := TlbCmd.read
  io.tlb.req.bits.size := pending.dataType.size
  io.tlb.req.bits.isPrefetch := true.B
  io.tlb.req.bits.no_translate := io.tlb.req.valid && pending.addressRef.valid
  io.tlb.req.bits.pmp_addr := pending.paddr
  io.tlb.req_kill := false.B
  io.tlb.resp.ready := true.B
  when(state === translate) {
    when(cancel) {
      failed := true.B
      state := idle
    }.elsewhen(io.tlb.req.fire) {
      state := waitTlb
    }
  }
  when(state === waitTlb && io.tlb.resp.fire) {
    val response = io.tlb.resp.bits
    val excp = response.excp.head
    val badPage = excp.pf.ld || excp.af.ld || excp.gpf.ld || Pbmt.isUncache(response.pbmt.head)
    when(cancel) {
      failed := true.B
      state := idle
    }.elsewhen(response.miss) {
      failed := retries.andR
      state := Mux(retries.andR, idle, retry)
      retries := retries + 1.U
    }.elsewhen(badPage) {
      failed := true.B
      state := idle
    }.otherwise {
      paddr := Mux(pending.addressRef.valid, pending.paddr, response.paddr.head)
      state := waitPmp
    }
  }
  when(state === retry) {
    failed := cancel
    state := Mux(cancel, idle, translate)
  }
  when(state === waitPmp) {
    failed := io.pmp.ld || io.pmp.mmio || !inMemory || cancel
    state := Mux(failed, idle, send)
  }
  io.request.valid := state === send && !cancel
  io.request.bits.paddr := get_block_addr(paddr)
  io.request.bits.vaddr := get_block_addr(pending.vaddr(VAddrBits - 1, 0))
  io.request.bits.confidence := 0.U
  io.request.bits.is_store := false.B
  io.request.bits.pf_source.value := L1_HW_PREFETCH_LLDP
  io.request.bits.temporalOwner.foreach(_ := DontCare)
  private val nextTrigger = io.request.bits.lldpTrigger.get
  nextTrigger.valid := true.B
  nextTrigger.bits.pc := pending.consumer
  nextTrigger.bits.context := pending.context
  nextTrigger.bits.offset := paddr(blockOffBits - 1, 0)
  nextTrigger.bits.dataType := pending.dataType
  nextTrigger.bits.source := pending.source.asUInt
  nextTrigger.bits.depth := pending.source === LldpValueSource.Chain
  nextTrigger.bits.addressRef := pending.addressRef
  when(state === send && (io.request.fire || cancel)) {
    failed := cancel
    state := idle
  }

  XSPerfAccumulate("lldp_dependence", table.io.train.fire)
  XSPerfAccumulate("lldp_dependence_drop", PopCount(io.train.map(_.valid)) - table.io.train.fire)
  XSPerfAccumulate("lldp_hint", table.io.hint.valid)
  XSPerfAccumulate("lldp_refill_return", returned.io.enq.fire)
  XSPerfAccumulate("lldp_chain_hit_return", hitReturns.io.enq.fire)
  XSPerfAccumulate("lldp_chain_hit_drop", hitReturns.io.enq.valid && !hitReturns.io.enq.ready)
  XSPerfAccumulate("lldp_refill_drop", returned.io.enq.valid && !returned.io.enq.ready)
  XSPerfAccumulate("lldp_tlb_retry", state === waitTlb && io.tlb.resp.fire && io.tlb.resp.bits.miss)
  XSPerfAccumulate("lldp_l1_fire", io.request.fire)
  XSPerfAccumulate("lldp_lookup_contention", demandLookup && prefetchLookup)
  XSPerfAccumulate("lldp_address_train", address.io.train.fire)
  XSPerfAccumulate("lldp_address_train_drop", PopCount(io.addressTrain.map(_.valid)) - addressArb.io.out.fire)
  XSPerfAccumulate("lldp_address_promoted", address.io.promoted)
  XSPerfAccumulate("lldp_address_blocked", address.io.blocked)
  for ((source, name) <- Seq(
    LldpValueSource.Demand -> "demand",
    LldpValueSource.Spatial -> "spatial",
    LldpValueSource.Chain -> "chain"
  )) {
    XSPerfAccumulate(s"lldp_l1_fire_$name", io.request.fire && pending.source === source && !pending.addressRef.valid)
    XSPerfAccumulate(s"lldp_l1_address_fire_$name", io.request.fire && pending.source === source && pending.addressRef.valid)
    XSPerfAccumulate(s"lldp_l1_alloc_$name", io.prefetch.valid && prefetch.allocated && prefetch.trigger.source === source.asUInt)
    XSPerfAccumulate(s"lldp_l1_merge_$name", io.prefetch.valid && prefetch.merged && prefetch.trigger.source === source.asUInt)
  }
}
