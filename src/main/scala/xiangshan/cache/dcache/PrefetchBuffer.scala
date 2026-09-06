/***************************************************************************************
 * Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 ***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates}
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan.L1CacheErrorInfo

object PBState extends ChiselEnum {
  val invalid, reserved, resident, poison, claimed = Value
}

object PBOp extends ChiselEnum {
  val promote, probe, evict = Value
}

object PBResult extends ChiselEnum {
  val published, poisoned, denied = Value
}

class PBToken(implicit p: Parameters) extends DCacheBundle {
  val id = UInt(PBIdBits.W)
  val gen = UInt(2.W)
  val addr = UInt(PAddrBits.W)
  val alias = UInt(PBAliasBits.W)
}

class PBAlloc(implicit p: Parameters) extends DCacheBundle {
  val mid = UInt(log2Up(cfg.nMissEntries).W)
  val addr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val src = UInt(L1PfSourceBits.W)
}

class PBRes(implicit p: Parameters) extends DCacheBundle {
  val token = new PBToken
  val mid = UInt(log2Up(cfg.nMissEntries).W)
}

class PBFill(implicit p: Parameters) extends PBRes {
  val data = UInt((cfg.blockBytes * 8).W)
  val coh = new ClientMetadata
  val denied = Bool()
  val corrupt = Bool()
}

class PBFillResp(implicit p: Parameters) extends PBRes {
  val result = PBResult()
}

class PBMissIO(nReq: Int)(implicit p: Parameters) extends DCacheBundle {
  val alloc = Output(Vec(nReq, Valid(new PBAlloc)))
  val slot = Input(Vec(nReq, Valid(new PBToken)))
  val take = Output(Vec(nReq, Bool()))
  val free = Output(Vec(nReq + cfg.nMissEntries, Valid(new PBRes)))
  val status = Input(Vec(PBEntries, new PBDir))
  val done = Input(Valid(new PBFillResp))
  val pub = Input(Valid(new PBRes))
  val preA = Output(Vec(cfg.nMissEntries, Bool()))
  val owners = Output(Vec(cfg.nMissEntries, Valid(UInt(PAddrBits.W))))
}

class PBDir(implicit p: Parameters) extends DCacheBundle {
  val hit = Bool()
  val reserved = Bool()
  val readable = Bool()
  val busy = Bool()
  val token = new PBToken
  val vaddr = UInt(VAddrBits.W)
  val coh = new ClientMetadata
  val src = UInt(L1PfSourceBits.W)
  val used = Bool()
  val origin = Valid(UInt(PBCreditBits.W))
}

class PBLoadReq(implicit p: Parameters) extends DCacheBundle {
  val valid = Bool()
  val addr = UInt(PAddrBits.W)
  val kill = Bool()
  val dcHit = Bool()
  val tagErr = Bool()
}

class PBLoadResp(implicit p: Parameters) extends DCacheBundle {
  val hit = Bool()
  val retry = Bool()
  val token = new PBToken
  val data = UInt(VLEN.W)
  val src = UInt(L1PfSourceBits.W)
  val used = Bool()
}

class PBLoadIO(implicit p: Parameters) extends DCacheBundle {
  val s0 = Flipped(Valid(UInt(VAddrBits.W)))
  val s1 = Input(new PBLoadReq)
  val resp = Output(new PBLoadResp)
  val s2 = Output(Valid(new PBLoadResp))
  val use = Flipped(Valid(new PBToken))
}

class PBMaint(implicit p: Parameters) extends DCacheBundle {
  val token = new PBToken
  val op = PBOp()
  val vaddr = UInt(VAddrBits.W)
}

class PBClaim(implicit p: Parameters) extends PBMaint {
  val origin = Valid(UInt(PBCreditBits.W))
}

class PBLine(implicit p: Parameters) extends DCacheBundle {
  val token = new PBToken
  val data = UInt((cfg.blockBytes * 8).W)
  val coh = new ClientMetadata
  val src = UInt(L1PfSourceBits.W)
  val used = Bool()
  val bad = Bool()
}

class PBAbort(implicit p: Parameters) extends DCacheBundle {
  val token = new PBToken
  val bad = Bool()
}

class PBDispatch(implicit p: Parameters) extends DCacheBundle {
  val token = new PBToken
  val claimed = Bool()
}

class PBMainIO(implicit p: Parameters) extends DCacheBundle {
  val addr = Output(UInt(PAddrBits.W))
  val dir = Input(new PBDir)
  val status = Input(Vec(PBEntries, new PBDir))
  val owners = Input(Vec(cfg.nMissEntries, Valid(UInt(PAddrBits.W))))
  val claim = Vec(2, Decoupled(new PBClaim))
  val op = Input(Vec(2, PBOp()))
  val read = Decoupled(new PBToken)
  val line = Flipped(Decoupled(new PBLine))
  val abort = Output(Valid(new PBAbort))
  val finish = Output(Valid(new PBMaint))
  val replay = Input(Valid(UInt(PBCreditBits.W)))
  val maint = Flipped(Decoupled(new PBMaint))
  val cancel = Input(Bool())
  val dispatch = Output(Valid(new PBDispatch))
  val assist = Output(Valid(new PBToken))
}

class PBMeta(implicit p: Parameters) extends DCacheBundle {
  val state = PBState()
  val token = new PBToken
  val mid = UInt(log2Up(cfg.nMissEntries).W)
  val vaddr = UInt(VAddrBits.W)
  val coh = new ClientMetadata
  val src = UInt(L1PfSourceBits.W)
  val used = Bool()
  val pending = Bool()
  val assist = Bool()
  val origin = Valid(UInt(PBCreditBits.W))
  val op = PBOp()
  val bad = Bool()
}

class PrefetchBuffer(nReq: Int)(implicit p: Parameters) extends DCacheModule {
  require(PBEntries > 0)
  require(cfg.blockBytes * 8 % VLEN == 0)

  val io = IO(new Bundle {
    val load = Vec(LoadPipelineWidth, new PBLoadIO)
    val addr = Input(UInt(PAddrBits.W))
    val dir = Output(new PBDir)
    val status = Output(Vec(PBEntries, new PBDir))

    val alloc = Input(Vec(nReq, Valid(new PBAlloc)))
    val slot = Output(Vec(nReq, Valid(new PBToken)))
    val take = Input(Vec(nReq, Bool()))
    val free = Input(Vec(nReq + cfg.nMissEntries, Valid(new PBRes)))
    val fill = Flipped(Decoupled(new PBFill))
    val fillDone = Output(Valid(new PBFillResp))
    val pub = Output(Valid(new PBRes))

    // The older S1 claim is port 0; a new S0 Probe is port 1.
    val claim = Vec(2, Flipped(Decoupled(new PBClaim)))
    val op = Output(Vec(2, PBOp()))
    val read = Flipped(Decoupled(new PBToken))
    val line = Decoupled(new PBLine)
    val abort = Input(Valid(new PBAbort))
    val finish = Input(Valid(new PBMaint))
    val replay = Output(Valid(UInt(PBCreditBits.W)))

    val maint = Decoupled(new PBMaint)
    val cancel = Output(Bool())
    val dispatch = Input(Valid(new PBDispatch))
    val assist = Input(Valid(new PBToken))
    val wfi = Input(Bool())
    val preA = Input(Vec(cfg.nMissEntries, Bool()))
    val safe = Output(Bool())
    val error = Output(Valid(new L1CacheErrorInfo))
    val fatal = Output(Bool())
  })

  val n = PBEntries
  val rows = cfg.blockBytes * 8 / DCacheSRAMRowBits
  val winRows = VLEN / DCacheSRAMRowBits
  val meta = RegInit(VecInit(Seq.fill(n)(0.U.asTypeOf(new PBMeta))))
  val next = WireInit(meta)
  val parity = RegInit(VecInit(Seq.fill(n)(false.B)))
  val data = Reg(Vec(n, Vec(rows, UInt(encDataBits.W))))
  val fatal = RegInit(false.B)
  val pressure = RegInit(false.B)

  def block(addr: UInt): UInt = addr(PAddrBits - 1, blockOffBits)
  def aligned(addr: UInt): UInt = Cat(block(addr), 0.U(blockOffBits.W))
  def alias(addr: UInt): UInt = get_alias(addr)
  def same(a: PBToken, b: PBToken): Bool = a.asUInt === b.asUInt
  def clean(coh: ClientMetadata): Bool = coh.state === ClientStates.Branch || coh.state === ClientStates.Trunk
  def decode(enc: UInt): (UInt, Bool) = {
    if (EnableDataEcc) {
      val dec = cfg.dataCode.decode(enc)
      (dec.corrected, dec.uncorrectable)
    } else (enc, false.B)
  }
  def window(line: Vec[UInt], idx: UInt): UInt = {
    VecInit(line.grouped(winRows).map(x => VecInit(x).asUInt).toSeq)(idx)
  }

  val dirErr = VecInit(meta.indices.map(i => parity(i) =/= meta(i).asUInt.xorR))
  val badDir = dirErr.asUInt.orR
  when (badDir) { fatal := true.B }
  io.fatal := fatal || badDir

  val reqValid = RegInit(false.B)
  val reqSent = RegInit(false.B)
  val req = Reg(new PBMaint)
  val pinned = VecInit(meta.indices.map(i => (reqValid || reqSent) && req.token.id === i.U))
  val freeOH = VecInit(meta.indices.map(i => meta(i).state === PBState.invalid && !pinned(i))).asUInt

  for (i <- meta.indices) {
    val m = meta(i)
    val out = io.status(i)
    out.hit := m.state =/= PBState.invalid
    out.reserved := m.state === PBState.reserved
    out.readable := m.state === PBState.resident && !m.origin.valid && !m.bad && clean(m.coh) && !io.fatal
    out.busy := out.hit && !out.reserved && !out.readable
    out.token := m.token
    out.vaddr := m.vaddr
    out.coh := m.coh
    out.src := m.src
    out.used := m.used
    out.origin := m.origin
  }

  val dirOH = VecInit(meta.indices.map(i => io.status(i).hit && block(meta(i).token.addr) === block(io.addr)))
  io.dir := Mux1H(dirOH, io.status)
  io.dir.hit := dirOH.asUInt.orR
  assert(PopCount(dirOH) <= 1.U)

  val use = VecInit(meta.indices.map(i => io.load.map(ld => ld.use.valid && same(ld.use.bits, meta(i).token)).reduce(_ || _)))
  val claims = Wire(Vec(n, Bool()))
  val owner = Wire(Vec(n, new PBClaim))
  val loadErr = Wire(Vec(LoadPipelineWidth, Vec(n, Bool())))
  val queryHit = Wire(Vec(LoadPipelineWidth, Bool()))

  for (c <- io.claim.indices) {
    val port = io.claim(c)
    val idx = port.bits.token.id
    val m = meta(idx)
    val prior = io.claim.take(c).map(x => x.fire && x.bits.token.id === idx).foldLeft(false.B)(_ || _)
    port.ready := idx < n.U && same(port.bits.token, m.token) && !io.fatal && !prior &&
      (m.state === PBState.resident || m.state === PBState.poison) &&
      (!port.bits.origin.valid || !m.origin.valid)
    io.op(c) := Mux(port.bits.op === PBOp.evict && !m.bad &&
      (m.pending || m.assist || m.origin.valid || use(idx)), PBOp.promote, port.bits.op)
  }
  for (i <- meta.indices) {
    val hits = io.claim.map(c => c.fire && c.bits.token.id === i.U)
    claims(i) := hits.reduce(_ || _)
    owner(i) := Mux1H(hits, io.claim.map(_.bits))
    owner(i).op := Mux1H(hits, io.op)
    assert(PopCount(VecInit(hits)) <= 1.U)
  }

  for ((ld, lane) <- io.load.zipWithIndex) {
    val s0Win = ld.s0.bits(blockOffBits - 1, log2Up(VLEN / 8))
    val s1Win = ld.s1.addr(blockOffBits - 1, log2Up(VLEN / 8))
    val win = RegEnable(s0Win, ld.s0.valid)
    val snap = RegEnable(VecInit(data.map(d => window(d, s0Win))), ld.s0.valid)
    val gens = RegEnable(VecInit(meta.map(_.token.gen)), ld.s0.valid)
    val valid = RegEnable(VecInit(meta.map(_.state === PBState.resident)), ld.s0.valid)
    val seen = RegNext(ld.s0.valid, false.B)
    val hitOH = VecInit(meta.indices.map(i => io.status(i).hit && block(meta(i).token.addr) === block(ld.s1.addr)))
    queryHit(lane) := ld.s1.valid && seen && !ld.s1.kill && hitOH.asUInt.orR
    val words = Wire(Vec(n, UInt(VLEN.W)))
    val auth = Wire(Vec(n, Bool()))
    for (i <- meta.indices) {
      val enc = Mux(win === s1Win, snap(i), window(data(i), s1Win))
      val dec = (0 until winRows).map(r => decode(enc((r + 1) * encDataBits - 1, r * encDataBits)))
      val check = ld.s1.valid && seen && !ld.s1.kill && !ld.s1.dcHit && !ld.s1.tagErr &&
        hitOH(i) && valid(i) && gens(i) === meta(i).token.gen && io.status(i).readable
      loadErr(lane)(i) := check && VecInit(dec.map(_._2)).asUInt.orR
      words(i) := VecInit(dec.map(_._1)).asUInt
      auth(i) := check && !loadErr(lane)(i) && !claims(i)
    }
    val owned = hitOH.zip(meta).map { case (h, m) => h && m.state =/= PBState.reserved }.reduce(_ || _)
    when (ld.s1.valid && seen && !ld.s1.kill && !ld.s1.tagErr) {
      assert(!(ld.s1.dcHit && owned), "Load found independent PB and DCache owners")
    }
    ld.resp.hit := auth.asUInt.orR
    ld.resp.retry := ld.s1.valid && seen && !ld.s1.kill && !ld.s1.dcHit && owned && !ld.resp.hit
    ld.resp.token := Mux1H(hitOH, meta.map(_.token))
    ld.resp.data := Mux1H(auth, words)
    ld.resp.src := Mux1H(hitOH, meta.map(_.src))
    ld.resp.used := Mux1H(hitOH, meta.map(_.used))
    ld.s2.valid := RegNext(ld.s1.valid && seen && !ld.s1.kill, false.B)
    ld.s2.bits := RegEnable(ld.resp, ld.s1.valid && seen)
    assert(!ld.s1.valid || seen)
    assert(PopCount(hitOH) <= 1.U)
    for (i <- meta.indices) { assert(!(auth(i) && claims(i))) }
  }

  var available = freeOH
  for (r <- io.alloc.indices) {
    val select = PriorityEncoderOH(available)
    val idx = OHToUInt(select)
    io.slot(r).valid := io.alloc(r).valid && available.orR && !io.fatal && !io.wfi
    io.slot(r).bits.id := idx
    io.slot(r).bits.gen := meta(idx).token.gen + 1.U
    io.slot(r).bits.addr := aligned(io.alloc(r).bits.addr)
    io.slot(r).bits.alias := alias(io.alloc(r).bits.vaddr)
    available = available & ~Mux(io.slot(r).valid, select, 0.U(n.W))
    assert(!io.take(r) || io.slot(r).valid)
  }
  val full = io.alloc.zip(io.slot).map { case (a, s) => a.valid && !s.valid && !io.wfi }.reduce(_ || _)
  when (freeOH.orR) { pressure := false.B }
  when (full) { pressure := true.B }

  val fillIdx = io.fill.bits.token.id
  val fillMeta = meta(fillIdx)
  val fillOK = fillIdx < n.U && fillMeta.state === PBState.reserved &&
    same(fillMeta.token, io.fill.bits.token) && fillMeta.mid === io.fill.bits.mid
  io.fill.ready := fillOK && !io.fatal
  assert(!io.fill.valid || fillOK)
  assert(!io.fill.valid || io.fill.bits.denied || clean(io.fill.bits.coh))
  io.pub.valid := io.fill.fire
  io.pub.bits.token := io.fill.bits.token
  io.pub.bits.mid := io.fill.bits.mid
  io.fillDone.valid := RegNext(io.fill.fire, false.B)
  io.fillDone.bits.token := RegEnable(io.fill.bits.token, io.fill.fire)
  io.fillDone.bits.mid := RegEnable(io.fill.bits.mid, io.fill.fire)
  io.fillDone.bits.result := RegEnable(Mux(io.fill.bits.denied, PBResult.denied,
    Mux(io.fill.bits.corrupt, PBResult.poisoned, PBResult.published)), io.fill.fire)

  val lineValid = RegInit(false.B)
  val line = Reg(new PBLine)
  io.line.valid := lineValid
  io.line.bits := line
  io.read.ready := (!lineValid || io.line.ready) && !io.fatal
  when (io.line.fire) { lineValid := false.B }
  when (io.read.fire) {
    val idx = io.read.bits.id
    val m = meta(idx)
    val dec = data(idx).map(decode)
    assert(idx < n.U && same(io.read.bits, m.token))
    assert(m.state === PBState.claimed || claims(idx))
    lineValid := true.B
    line.token := m.token
    line.data := VecInit(dec.map(_._1)).asUInt
    line.coh := m.coh
    line.src := m.src
    line.used := m.used || (use(idx) && claims(idx) && owner(idx).op === PBOp.promote)
    line.bad := m.bad || VecInit(dec.map(_._2)).asUInt.orR
  }

  val arb = Module(new RRArbiter(new PBMaint, n))
  for (i <- meta.indices) {
    val m = meta(i)
    val pending = m.pending || m.assist || m.origin.valid
    arb.io.in(i).valid := !io.fatal && (!io.wfi || m.origin.valid) &&
      (m.state === PBState.poison || m.state === PBState.resident && (pending || pressure))
    arb.io.in(i).bits.token := m.token
    arb.io.in(i).bits.vaddr := m.vaddr
    arb.io.in(i).bits.op := Mux(m.bad || !pending, PBOp.evict, PBOp.promote)
  }
  arb.io.out.ready := !reqValid && !reqSent
  when (arb.io.out.fire) {
    req := arb.io.out.bits
    reqValid := true.B
  }
  val reqMeta = meta(req.token.id)
  io.maint.valid := reqValid && !io.fatal
  io.maint.bits := req
  io.cancel := reqValid && (!same(req.token, reqMeta.token) || io.fatal ||
    (reqMeta.state =/= PBState.resident && reqMeta.state =/= PBState.poison) || io.wfi && !reqMeta.origin.valid)
  when (io.cancel) { reqValid := false.B }
  when (io.maint.fire) {
    assert(!io.cancel)
    reqValid := false.B
    reqSent := true.B
  }
  when (io.dispatch.valid) {
    assert(reqSent && same(req.token, io.dispatch.bits.token))
    reqSent := false.B
  }

  io.replay.valid := io.finish.valid && meta(io.finish.bits.token.id).origin.valid
  io.replay.bits := meta(io.finish.bits.token.id).origin.bits
  for (i <- meta.indices) {
    val m = meta(i)
    val n = next(i)
    val err = loadErr.map(_(i)).reduce(_ || _)
    val finish = io.finish.valid && io.finish.bits.token.id === i.U
    val abort = io.abort.valid && io.abort.bits.token.id === i.U
    val fill = io.fill.fire && fillIdx === i.U
    val frees = io.free.map(f => f.valid && f.bits.token.id === i.U)
    val free = frees.reduce(_ || _)
    val takes = io.take.zip(io.slot).map { case (t, s) => t && s.bits.id === i.U }
    val take = takes.reduce(_ || _)
    when (m.state === PBState.resident && use(i) && !m.origin.valid) {
      n.pending := true.B
      n.used := true.B
    }
    when (io.assist.valid && same(io.assist.bits, m.token) && m.state === PBState.resident) {
      n.assist := true.B
    }
    when (err) {
      n.bad := true.B
      n.state := PBState.poison
    }
    when (claims(i)) {
      n.state := PBState.claimed
      n.op := owner(i).op
      n.pending := owner(i).op === PBOp.promote
      n.used := m.used || (use(i) && owner(i).op === PBOp.promote)
      when (owner(i).origin.valid) { n.origin := owner(i).origin }
    }
    when (abort) {
      assert(m.state === PBState.claimed && same(io.abort.bits.token, m.token))
      assert(m.op =/= PBOp.probe)
      n.bad := m.bad || io.abort.bits.bad
      n.state := Mux(m.bad || io.abort.bits.bad, PBState.poison, PBState.resident)
    }
    when (finish) {
      assert(m.state === PBState.claimed && same(io.finish.bits.token, m.token))
      assert(io.finish.bits.op === m.op)
      n.state := PBState.invalid
      n.pending := false.B
      n.assist := false.B
      n.used := false.B
      n.origin.valid := false.B
      n.bad := false.B
    }
    when (free) {
      assert(m.state === PBState.reserved && !fill)
      for ((f, hit) <- io.free.zip(frees)) {
        when (hit) { assert(same(f.bits.token, m.token) && f.bits.mid === m.mid) }
      }
      n.state := PBState.invalid
    }
    when (fill) {
      n.state := Mux(io.fill.bits.denied, PBState.invalid,
        Mux(io.fill.bits.corrupt, PBState.poison, PBState.resident))
      n.coh := io.fill.bits.coh
      n.bad := io.fill.bits.corrupt && !io.fill.bits.denied
      for (r <- 0 until rows) {
        val raw = io.fill.bits.data((r + 1) * DCacheSRAMRowBits - 1, r * DCacheSRAMRowBits)
        data(i)(r) := (if (EnableDataEcc) cfg.dataCode.encode(raw) else raw)
      }
    }
    when (take) {
      assert(m.state === PBState.invalid && !pinned(i))
      n := 0.U.asTypeOf(new PBMeta)
      n.state := PBState.reserved
      n.token := Mux1H(takes, io.slot.map(_.bits))
      n.mid := Mux1H(takes, io.alloc.map(_.bits.mid))
      n.vaddr := Mux1H(takes, io.alloc.map(_.bits.vaddr))
      n.src := Mux1H(takes, io.alloc.map(_.bits.src))
    }
    assert(PopCount(VecInit(takes)) <= 1.U)
    assert(PopCount(VecInit(frees)) <= 1.U)
    assert(!(finish && abort))
    assert(!io.status(i).readable || clean(m.coh))
    for (j <- 0 until i) {
      assert(!(io.status(i).hit && io.status(j).hit && block(m.token.addr) === block(meta(j).token.addr)))
    }
    when (!io.fatal) {
      meta(i) := n
      parity(i) := n.asUInt.xorR
    }
  }

  val errOH = VecInit(meta.indices.map(i => loadErr.map(_(i)).reduce(_ || _)))
  val loadBad = errOH.asUInt.orR
  val readBad = io.abort.valid && io.abort.bits.bad
  val fillBad = io.fill.fire && (io.fill.bits.denied || io.fill.bits.corrupt)
  io.error := 0.U.asTypeOf(io.error)
  io.error.valid := badDir || loadBad || readBad || fillBad
  io.error.bits.paddr := Mux(badDir, 0.U, Mux(loadBad,
    meta(PriorityEncoder(errOH)).token.addr, Mux(readBad, io.abort.bits.token.addr, io.fill.bits.token.addr)))
  io.error.bits.source.tag := badDir
  io.error.bits.source.data := !badDir && (loadBad || readBad)
  io.error.bits.source.l2 := !badDir && !loadBad && !readBad && fillBad
  io.error.bits.opType.load := !badDir && loadBad
  io.error.bits.opType.release := !badDir && !loadBad && readBad
  io.error.bits.report_to_beu := badDir || loadBad || readBad
  val active = meta.map(m => m.state === PBState.claimed || m.origin.valid ||
    m.state === PBState.reserved && !io.preA(m.mid)).reduce(_ || _)
  io.safe := !active && !reqValid && !reqSent && !lineValid && !io.fillDone.valid &&
    !io.claim.map(_.fire).reduce(_ || _) && !io.read.fire && !io.fill.fire &&
    !io.load.map(ld => ld.s0.valid || ld.s1.valid || ld.s2.valid).reduce(_ || _) && !io.fatal

  XSPerfAccumulate("fill", io.fill.fire && !io.fill.bits.denied && !io.fill.bits.corrupt)
  XSPerfAccumulate("query_hit", PopCount(queryHit))
  XSPerfAccumulate("hit", PopCount(io.load.map(_.s2.bits.hit).zip(io.load.map(_.s2.valid)).map { case (h, v) => h && v }))
  XSPerfAccumulate("use", PopCount(use.zip(meta).map { case (u, m) => u && m.state === PBState.resident }))
  XSPerfAccumulate("lane_use", PopCount(io.load.map(_.use.valid)))
  XSPerfAccumulate("first_use", PopCount(meta.indices.map(i => use(i) && !meta(i).used &&
    meta(i).state === PBState.resident && !meta(i).origin.valid &&
    (!claims(i) || owner(i).op === PBOp.promote))))
  XSPerfAccumulate("retry", PopCount(io.load.map(ld => ld.s2.valid && ld.s2.bits.retry)))
  XSPerfAccumulate("promote", io.finish.valid && io.finish.bits.op === PBOp.promote)
  XSPerfAccumulate("probe", io.finish.valid && io.finish.bits.op === PBOp.probe)
  XSPerfAccumulate("evict", io.finish.valid && io.finish.bits.op === PBOp.evict)
  XSPerfAccumulate("full", full)
}
