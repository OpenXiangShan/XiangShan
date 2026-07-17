/***************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 ***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{XORFold, XSPerfAccumulate, XSPerfHistogram}
import xiangshan.mem.HasL1PrefetchSourceParameter

case class L1DBPParams(
  sampleBits: Int = 2,
  pcPredictorEntries: Int = 8192
) {
  require(pcPredictorEntries >= 2 && isPow2(pcPredictorEntries),
    "L1DBP PC predictor entries must be a power of two and at least two")
  def pcIndexWidth: Int = log2Ceil(pcPredictorEntries)
}

object L1DBPOrigin {
  val width = 2
  val demand = 0.U(width.W)
  val stream = 1.U(width.W)
  val stride = 2.U(width.W)
}

trait HasL1DBPParameters extends HasDCacheParameters {
  val l1dbpParams: L1DBPParams

  require(l1dbpParams.sampleBits >= 1 && l1dbpParams.sampleBits < idxBits,
    s"L1DBP sampleBits must be in [1, ${idxBits - 1}]")

  final def l1dbpPcIndexWidth: Int = l1dbpParams.pcIndexWidth
  final def l1dbpNumSampleSets: Int = nSets >> l1dbpParams.sampleBits

  final def isL1DBPSampleSet(set: UInt): Bool = {
    val n = l1dbpParams.sampleBits
    set(idxBits - 1, idxBits - n) === set(n - 1, 0)
  }

  final def getL1DBPSampleIndex(set: UInt): UInt =
    set(idxBits - l1dbpParams.sampleBits - 1, 0)
}

class L1DBPOriginInfo(implicit p: Parameters) extends DCacheBundle {
  val pc = UInt(VAddrBits.W)
  val pfSource = UInt(L1PfSourceBits.W)
  val isPrefetch = Bool()
}

class L1DBPPrediction(params: L1DBPParams)(implicit p: Parameters) extends DCacheBundle {
  val valid = Bool()
  val origin = UInt(L1DBPOrigin.width.W)
  val payload = UInt(params.pcIndexWidth.W)
  val predictedDead = Bool()
}

class L1DBPSampleEntry(params: L1DBPParams) extends Bundle {
  val valid = Bool()
  val payload = UInt(params.pcIndexWidth.W)
}

class L1DBPDeadEntry extends Bundle {
  val valid = Bool()
  val origin = UInt(L1DBPOrigin.width.W)
  val predictedDead = Bool()
}

class L1DBPRefill(params: L1DBPParams)(implicit p: Parameters) extends DCacheBundle {
  val set = UInt(idxBits.W)
  val wayEn = UInt(nWays.W)
  val prediction = new L1DBPPrediction(params)
}

class L1DBPTerminate(params: L1DBPParams)(implicit p: Parameters) extends DCacheBundle {
  val set = UInt(idxBits.W)
  val wayEn = UInt(nWays.W)
  val sample = new L1DBPSampleEntry(params)
  val dead = new L1DBPDeadEntry
  val pfSource = UInt(L1PfSourceBits.W)
  val accessed = Bool()
  val fromProbe = Bool()
}

class L1DBPResult(params: L1DBPParams)(implicit p: Parameters) extends DCacheBundle {
  val sampled = Bool()
  val origin = UInt(L1DBPOrigin.width.W)
  val predictorIndex = UInt(params.pcIndexWidth.W)
  val predictedDead = Bool()
  val actualDead = Bool()
  val fromProbe = Bool()
}

class L1DBPMonitor(params: L1DBPParams)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val result = Flipped(ValidIO(new L1DBPResult(params)))
  })

  val result = io.result.bits
  val correct = result.predictedDead === result.actualDead
  val isDemand = result.origin === L1DBPOrigin.demand
  val isStream = result.origin === L1DBPOrigin.stream
  val isStride = result.origin === L1DBPOrigin.stride

  XSPerfAccumulate("l1dbp_prediction_total", io.result.valid)
  XSPerfAccumulate("l1dbp_prediction_correct", io.result.valid && correct)
  XSPerfAccumulate("l1dbp_pred_dead_actual_dead", io.result.valid && result.predictedDead && result.actualDead)
  XSPerfAccumulate("l1dbp_pred_dead_actual_reused", io.result.valid && result.predictedDead && !result.actualDead)
  XSPerfAccumulate("l1dbp_pred_reused_actual_dead", io.result.valid && !result.predictedDead && result.actualDead)
  XSPerfAccumulate("l1dbp_pred_reused_actual_reused", io.result.valid && !result.predictedDead && !result.actualDead)
  Seq(
    "demand" -> isDemand,
    "stream" -> isStream,
    "stride" -> isStride
  ).foreach { case (name, selectedOrigin) =>
    val selected = io.result.valid && selectedOrigin
    XSPerfAccumulate(s"l1dbp_${name}_total", selected)
    XSPerfAccumulate(s"l1dbp_${name}_correct", selected && correct)
    XSPerfAccumulate(s"l1dbp_${name}_pred_dead_actual_dead",
      selected && result.predictedDead && result.actualDead)
    XSPerfAccumulate(s"l1dbp_${name}_pred_dead_actual_reused",
      selected && result.predictedDead && !result.actualDead)
    XSPerfAccumulate(s"l1dbp_${name}_pred_reused_actual_dead",
      selected && !result.predictedDead && result.actualDead)
    XSPerfAccumulate(s"l1dbp_${name}_pred_reused_actual_reused",
      selected && !result.predictedDead && !result.actualDead)
  }
  XSPerfAccumulate("l1dbp_probe_termination", io.result.valid && result.fromProbe)

  val pcEntrySelected = io.result.valid && result.sampled && isDemand
  XSPerfHistogram(
    "l1dbp_pc_entry_total",
    result.predictorIndex,
    pcEntrySelected,
    start = 0,
    stop = params.pcPredictorEntries,
    step = 1
  )
  XSPerfHistogram(
    "l1dbp_pc_entry_correct",
    result.predictorIndex,
    pcEntrySelected && correct,
    start = 0,
    stop = params.pcPredictorEntries,
    step = 1
  )
  for (i <- 0 until 2) {
    val origin = if (i == 0) L1DBPOrigin.stream else L1DBPOrigin.stride
    val selected = io.result.valid && result.sampled && result.origin === origin
    XSPerfAccumulate(s"l1dbp_pf_entry_${i}_total", selected)
    XSPerfAccumulate(s"l1dbp_pf_entry_${i}_correct", selected && correct)
  }
}

class L1DBP(val l1dbpParams: L1DBPParams)(implicit p: Parameters)
  extends DCacheModule with HasL1DBPParameters with HasL1PrefetchSourceParameter {
  val io = IO(new Bundle {
    val read = Flipped(ValidIO(UInt(idxBits.W)))
    val sampleResp = Output(Vec(nWays, new L1DBPSampleEntry(l1dbpParams)))
    val deadResp = Output(Vec(nWays, new L1DBPDeadEntry))

    val query = Flipped(ValidIO(new L1DBPOriginInfo))
    val queryResp = Output(new L1DBPPrediction(l1dbpParams))

    val refill = Flipped(ValidIO(new L1DBPRefill(l1dbpParams)))
    val terminate = Flipped(ValidIO(new L1DBPTerminate(l1dbpParams)))
  })

  val pcCounters = RegInit(VecInit(Seq.fill(l1dbpParams.pcPredictorEntries)(2.U(2.W))))
  val pfCounters = RegInit(VecInit(Seq.fill(2)(2.U(2.W))))

  val sampleArray = Module(new L1DBPSampleArray(l1dbpParams))
  sampleArray.io.read.valid := io.read.valid && isL1DBPSampleSet(io.read.bits)
  sampleArray.io.read.bits := getL1DBPSampleIndex(io.read.bits)
  sampleArray.io.write.valid := io.refill.valid && isL1DBPSampleSet(io.refill.bits.set)
  sampleArray.io.write.bits.set := getL1DBPSampleIndex(io.refill.bits.set)
  sampleArray.io.write.bits.wayEn := io.refill.bits.wayEn
  sampleArray.io.write.bits.entry.valid := io.refill.bits.prediction.valid
  sampleArray.io.write.bits.entry.payload := io.refill.bits.prediction.payload
  io.sampleResp := sampleArray.io.resp

  val deadTable = RegInit(VecInit(Seq.fill(nSets)(
    VecInit(Seq.fill(nWays)(0.U.asTypeOf(new L1DBPDeadEntry)))
  )))
  val deadReadSet = RegEnable(io.read.bits, io.read.valid)
  io.deadResp := deadTable(deadReadSet)
  when(io.refill.valid) {
    io.refill.bits.wayEn.asBools.zipWithIndex.foreach { case (wen, way) =>
      when(wen) {
        deadTable(io.refill.bits.set)(way).valid := io.refill.bits.prediction.valid
        deadTable(io.refill.bits.set)(way).origin := io.refill.bits.prediction.origin
        deadTable(io.refill.bits.set)(way).predictedDead := io.refill.bits.prediction.predictedDead
      }
    }
  }.elsewhen(io.terminate.valid) {
    io.terminate.bits.wayEn.asBools.zipWithIndex.foreach { case (wen, way) =>
      when(wen) {
        deadTable(io.terminate.bits.set)(way).valid := false.B
      }
    }
  }

  // pfSource is mutable: a demand hit changes a prefetched line to CLEAR. Use the
  // immutable refill origin so training remains attributed to the allocating request.
  val terminateOrigin = io.terminate.bits.dead.origin
  val terminateWasPrefetch = terminateOrigin === L1DBPOrigin.stream ||
    terminateOrigin === L1DBPOrigin.stride
  val terminateWasDemand = terminateOrigin === L1DBPOrigin.demand
  val trainValid = io.terminate.valid && isL1DBPSampleSet(io.terminate.bits.set) &&
    io.terminate.bits.sample.valid && io.terminate.bits.dead.valid &&
    (terminateWasDemand || terminateWasPrefetch)
  val trainIndex = io.terminate.bits.sample.payload
  val trainPfIndex = terminateOrigin === L1DBPOrigin.stride
  val rawTrainCounter = Mux(terminateWasPrefetch, pfCounters(trainPfIndex), pcCounters(trainIndex))
  val trainedCounter = Mux(
    io.terminate.bits.accessed,
    3.U(2.W),
    Mux(rawTrainCounter === 0.U, 0.U, rawTrainCounter - 1.U)
  )

  when(trainValid) {
    when(terminateWasPrefetch) {
      pfCounters(trainPfIndex) := trainedCounter
    }.otherwise {
      pcCounters(trainIndex) := trainedCounter
    }
  }

  def pcHash: (UInt, Int) => UInt = (pc, width) => XORFold(pc(VAddrBits - 1, 1), width)
  val queryPcIndex = pcHash(io.query.bits.pc, l1dbpPcIndexWidth)
  require(queryPcIndex.getWidth == l1dbpPcIndexWidth,
    s"L1DBP hash width ${queryPcIndex.getWidth} != predictor index width $l1dbpPcIndexWidth")
  val queryIsStream = io.query.bits.pfSource === L1_HW_PREFETCH_STREAM
  val queryIsStride = io.query.bits.pfSource === L1_HW_PREFETCH_STRIDE
  val queryPfSupported = queryIsStream || queryIsStride
  val queryPfIndex = Wire(UInt(1.W))
  queryPfIndex := queryIsStride
  val querySupported = io.query.valid && (!io.query.bits.isPrefetch || queryPfSupported)
  val queryPayload = Mux(io.query.bits.isPrefetch, queryPfIndex.pad(l1dbpPcIndexWidth), queryPcIndex)
  val rawQueryCounter = Mux(io.query.bits.isPrefetch, pfCounters(queryPfIndex), pcCounters(queryPcIndex))
  val queryHitsTraining = trainValid && terminateWasPrefetch === io.query.bits.isPrefetch &&
    Mux(terminateWasPrefetch, trainPfIndex === queryPfIndex, trainIndex === queryPcIndex)
  val queryCounter = Mux(queryHitsTraining, trainedCounter, rawQueryCounter)

  io.queryResp.valid := querySupported
  io.queryResp.origin := Mux(
    io.query.bits.isPrefetch,
    Mux(queryIsStream, L1DBPOrigin.stream, L1DBPOrigin.stride),
    L1DBPOrigin.demand
  )
  io.queryResp.payload := queryPayload
  io.queryResp.predictedDead := queryCounter === 0.U

  val monitor = Module(new L1DBPMonitor(l1dbpParams))
  monitor.io.result.valid := io.terminate.valid && io.terminate.bits.dead.valid
  monitor.io.result.bits.sampled := isL1DBPSampleSet(io.terminate.bits.set)
  monitor.io.result.bits.origin := io.terminate.bits.dead.origin
  monitor.io.result.bits.predictorIndex := io.terminate.bits.sample.payload
  monitor.io.result.bits.predictedDead := io.terminate.bits.dead.predictedDead
  monitor.io.result.bits.actualDead := !io.terminate.bits.accessed
  monitor.io.result.bits.fromProbe := io.terminate.bits.fromProbe

  XSPerfAccumulate("l1dbp_sample_alloc", io.refill.valid && isL1DBPSampleSet(io.refill.bits.set) &&
    io.refill.bits.prediction.valid)
  XSPerfAccumulate("l1dbp_sample_train", trainValid)
  XSPerfAccumulate("l1dbp_unsupported_refill", io.refill.valid && !io.refill.bits.prediction.valid)
  XSPerfAccumulate("l1dbp_probe_to_n_train", trainValid && io.terminate.bits.fromProbe)
  XSPerfAccumulate("l1dbp_replacement_train", trainValid && !io.terminate.bits.fromProbe)
  XSPerfAccumulate("l1dbp_non_sample_termination", io.terminate.valid && !isL1DBPSampleSet(io.terminate.bits.set))
  XSPerfAccumulate("l1dbp_sample_invalid_termination", io.terminate.valid &&
    isL1DBPSampleSet(io.terminate.bits.set) && !io.terminate.bits.sample.valid)

  assert(!(sampleArray.io.read.valid && sampleArray.io.write.valid),
    "L1DBP Sample SRAM read/write conflict must be covered by the tag SRAM stall")
  when(trainValid && terminateWasPrefetch) {
    assert(trainIndex === trainPfIndex,
      "L1DBP prefetch sample payload must match its immutable origin")
  }
  when(io.terminate.valid && io.terminate.bits.sample.valid && isL1DBPSampleSet(io.terminate.bits.set)) {
    assert(io.terminate.bits.dead.valid,
      "L1DBP valid sample must contain a valid immutable origin")
    assert(terminateWasDemand || terminateWasPrefetch,
      "L1DBP valid sample must be Demand, Stream, or Stride")
  }
}
