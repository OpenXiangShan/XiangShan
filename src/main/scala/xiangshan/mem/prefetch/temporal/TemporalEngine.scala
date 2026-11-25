package xiangshan.mem.prefetch.temporal

import chisel3._
import chisel3.util._

private[xiangshan] class TemporalOwner(contextBits: Int) extends Bundle {
  val pc = UInt(TemporalAddress.PcBits.W)
  val context = UInt(contextBits.W)
  val record = new TemporalRecordRef
  val wrapEpoch = UInt(32.W)
}

private[xiangshan] class TemporalFeedback(contextBits: Int) extends Bundle {
  val owner = new TemporalOwner(contextBits)
  val sent = Bool()
  val useful = Bool()
  val timely = Bool()
  val legacyUseful = Bool()
  val legacyTimely = Bool()
  val adjustRecord = Bool()
  val priority = UInt(2.W)
}

private[prefetch] class TemporalRole(pcBits: Int, contextBits: Int) extends Bundle {
  val pc = UInt(pcBits.W)
  val context = UInt(contextBits.W)
  val roles = UInt(2.W)
}

private[prefetch] class TemporalCandidate(lineBits: Int, aliasBits: Int, contextBits: Int) extends Bundle {
  val target = new TemporalLine(lineBits, aliasBits)
  val owner = new TemporalOwner(contextBits)
}

private[prefetch] class TemporalEngine(
  lineBits: Int,
  aliasBits: Int,
  virtualPcBits: Int,
  contextBits: Int,
  samplerSets: Int,
  samplerWays: Int,
  recorderSets: Int,
  confWays: Int,
  metaSets: Int = 1536,
  metaWays: Int = 16,
  compactSlots: Int = 18432,
  fullSlots: Int = 4608
) extends Module {
  import TemporalAddress._

  val io = IO(new Bundle {
    val load = Flipped(Decoupled(new Bundle {
      val pc = UInt(virtualPcBits.W)
      val context = UInt(contextBits.W)
      val kind = UInt(2.W)
      val target = new TemporalLine(lineBits, aliasBits)
    }))
    val role = Flipped(Decoupled(new TemporalRole(virtualPcBits, contextBits)))
    val feedback = Flipped(Decoupled(new TemporalFeedback(contextBits)))
    val candidate = Decoupled(new TemporalCandidate(lineBits, aliasBits, contextBits))
    val generationWrap = Decoupled(UInt(15.W))
    val trainPair = Output(Bool())
    val metaHit = Output(Bool())
    val install = Output(Bool())
    val bypass = Output(Bool())
    val staleFeedback = Output(Bool())
  })

  private val training = Module(new TemporalTraining(lineBits, aliasBits, contextBits, samplerSets, samplerWays))
  private val recorder = Module(new TemporalRecorder(recorderSets, lineBits, aliasBits, contextBits))
  private val meta = Module(new TemporalMeta(lineBits, aliasBits, contextBits, metaSets, metaWays, compactSlots, fullSlots))
  private val conf = Module(new TemporalConfidence(confWays, contextBits))

  private val Seq(idle, readConf, confResult, accessMeta, metaResult, touch, touched,
    replay, charge, charged, train) = Enum(11)
  private val state = RegInit(idle)
  private val load :: mark :: install :: feedback :: Nil = Enum(4)
  private val work = Reg(UInt(2.W))
  private val active = Reg(new TemporalRecord(lineBits, aliasBits, contextBits, PcBits))
  private val role = Reg(UInt(2.W))
  private val kind = Reg(UInt(2.W))
  private val target = Reg(new TemporalLine(lineBits, aliasBits))
  private val effects = Reg(chiselTypeOf(io.feedback.bits))
  private val bypass = Reg(Bool())
  private val replayAllowed = Reg(Bool())
  private val lowIssue = Reg(Bool())
  private val issueProbe = RegInit(0.U(6.W))
  private val metaProbe = RegInit(0.U(6.W))
  private val replayRecord = Reg(chiselTypeOf(meta.io.response.bits.record))
  private val replayRef = Reg(new TemporalRecordRef)
  private val wrapEpoch = RegInit(0.U(32.W))
  private val replayIndex = Reg(UInt(log2Ceil(RecordLength).W))
  private val queued = Reg(UInt(log2Ceil(RecordLength + 1).W))

  io.generationWrap <> meta.io.generationWrap
  when(io.generationWrap.fire) {
    wrapEpoch := wrapEpoch + 1.U
  }
  recorder.io.pair <> training.io.pair
  io.trainPair := training.io.pair.fire
  io.bypass := training.io.bypass
  io.metaHit := state === metaResult && work === load && meta.io.response.fire && meta.io.response.bits.hit
  io.install := state === metaResult && work === install && meta.io.response.fire && meta.io.response.bits.success
  io.staleFeedback := state === metaResult && work === feedback &&
    meta.io.response.fire && !meta.io.response.bits.success

  io.feedback.ready := state === idle
  recorder.io.record.ready := state === idle && !io.feedback.valid
  io.role.ready := recorder.io.record.ready && !recorder.io.record.valid
  io.load.ready := io.role.ready && !io.role.valid

  when(state === idle) {
    when(io.feedback.fire) {
      effects := io.feedback.bits
      active.pc := io.feedback.bits.owner.pc
      active.context := io.feedback.bits.owner.context
      work := feedback
      state := readConf
    }.elsewhen(recorder.io.record.fire) {
      active := recorder.io.record.bits
      work := install
      state := readConf
    }.elsewhen(io.role.fire) {
      active.pc := pcHash(io.role.bits.pc)
      active.context := io.role.bits.context
      role := io.role.bits.roles
      work := mark
      state := readConf
    }.elsewhen(io.load.fire) {
      active.pc := pcHash(io.load.bits.pc)
      active.context := io.load.bits.context
      active.trigger := io.load.bits.target.line
      target := io.load.bits.target
      kind := io.load.bits.kind
      work := load
      state := readConf
    }
  }

  conf.io.request.valid := state === readConf || state === touch || state === charge
  conf.io.request.bits := 0.U.asTypeOf(conf.io.request.bits)
  conf.io.request.bits.pc := active.pc
  conf.io.request.bits.context := active.context
  conf.io.request.bits.op := MuxCase(TemporalConfOp.Lookup, Seq(
    (state === touch || (state === readConf && work === install)) -> TemporalConfOp.Touch,
    (state === charge) -> TemporalConfOp.Issued,
    (work === mark) -> TemporalConfOp.Mark,
    (work === feedback) -> TemporalConfOp.Feedback
  ))
  conf.io.request.bits.roles := role
  conf.io.request.bits.issued := queued
  conf.io.request.bits.legacyUseful := effects.legacyUseful
  conf.io.request.bits.legacyTimely := effects.legacyTimely
  conf.io.request.bits.sent := effects.sent
  conf.io.request.bits.useful := effects.useful
  conf.io.request.bits.timely := effects.timely
  conf.io.response.ready := state === confResult || state === touched || state === charged

  when(conf.io.request.fire) {
    state := Mux(state === readConf, confResult, Mux(state === touch, touched, charged))
  }
  when(state === confResult && conf.io.response.fire) {
    val result = conf.io.response.bits
    switch(work) {
      is(load) {
        role := result.roles
        bypass := result.canBypassSampler
        lowIssue := result.issue < 32.U
        replayAllowed := result.issue >= 32.U || issueProbe.andR
        state := accessMeta
      }
      is(install) {
        val low = result.meta < 32.U
        when(low) {
          metaProbe := metaProbe + 1.U
        }
        state := Mux(!low || metaProbe.andR, accessMeta, idle)
      }
      is(mark) {
        state := idle
      }
      is(feedback) {
        state := Mux(effects.adjustRecord && effects.owner.wrapEpoch === wrapEpoch, accessMeta, idle)
      }
    }
  }

  meta.io.request.valid := state === accessMeta
  meta.io.request.bits.op := Mux(work === load, TemporalMetaOp.Query,
    Mux(work === install, TemporalMetaOp.Install, TemporalMetaOp.Feedback))
  meta.io.request.bits.record := active
  when(work === load) {
    meta.io.request.bits.record.roles := role
  }
  meta.io.request.bits.ref := effects.owner.record
  meta.io.request.bits.priority := effects.priority
  meta.io.request.bits.allowReplay := replayAllowed
  meta.io.response.ready := state === metaResult

  when(meta.io.request.fire) {
    state := metaResult
  }
  when(meta.io.response.fire) {
    when(work === load) {
      when(meta.io.response.bits.hit && lowIssue) {
        issueProbe := issueProbe + 1.U
      }
      when(meta.io.response.bits.success) {
        replayRecord := meta.io.response.bits.record
        replayRef := meta.io.response.bits.ref
      }
      state := Mux(meta.io.response.bits.hit, touch, train)
    }.otherwise {
      state := idle
    }
  }

  private val candidate = replayRecord.targets(replayIndex)
  private val skip = candidate.line === active.trigger
  io.candidate.valid := state === replay && !skip
  io.candidate.bits.target := candidate
  io.candidate.bits.owner.pc := active.pc
  io.candidate.bits.owner.context := active.context
  io.candidate.bits.owner.record := replayRef
  io.candidate.bits.owner.wrapEpoch := wrapEpoch

  when(state === touched && conf.io.response.fire) {
    // Touch is reached only on a Meta hit. The saved probe decision determines replay.
    replayIndex := 0.U
    queued := 0.U
    state := Mux(replayAllowed, replay, train)
  }
  when(state === replay && (skip || io.candidate.fire)) {
    val count = queued + io.candidate.fire
    queued := count
    when((replayIndex +& 1.U) === replayRecord.length) {
      state := Mux(count.orR, charge, train)
    }.otherwise {
      replayIndex := replayIndex + 1.U
    }
  }
  when(state === charged && conf.io.response.fire) {
    state := train
  }
  training.io.load.valid := state === train
  training.io.load.bits.pc := active.pc
  training.io.load.bits.context := active.context
  training.io.load.bits.roles := role
  training.io.load.bits.kind := kind
  training.io.load.bits.target := target
  training.io.load.bits.bypassSampler := bypass
  when(training.io.load.fire) {
    state := idle
  }
}
