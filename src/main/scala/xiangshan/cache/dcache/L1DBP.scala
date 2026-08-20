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
  pcPredictorEntries: Int = 8192,
  debugMode: Boolean = false,
  enablePrefetchPrediction: Boolean = false,
  pcHash: (UInt, Int) => UInt = (pc, width) => XORFold(pc >> 1, width)
) {
  require(sampleBits >= 1, "L1DBP sampleBits must be positive")
  require(pcPredictorEntries >= 2 && isPow2(pcPredictorEntries),
    "L1DBP PC predictor entries must be a power of two and at least two")
}

object L1DBPOrigin {
  val width = 2
  val demand = 0.U(width.W)
  val stream = 1.U(width.W)
  val stride = 2.U(width.W)
}

class L1DBPRead(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(l1dbpPcIndexWidth.W)

  def pfIdx = idx(0).asUInt
}

class L1DBPResp(implicit p: Parameters) extends DCacheBundle {
  val dead = Bool()
}

class L1DBPUpdate(implicit p: Parameters) extends DCacheBundle {
  val origin = UInt(L1DBPOrigin.width.W)
  val idx = UInt(l1dbpPcIndexWidth.W)
  val accessed = Bool()

  def pfIdx = idx(0).asUInt
}

class L1DBP(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val demandQuery = Flipped(Vec(LduCnt, ValidIO(new L1DBPRead)))
    val demandResp = Output(Vec(LduCnt, ValidIO(new L1DBPResp)))
    val pftQuery = Flipped(Vec(1, ValidIO(new L1DBPRead)))
    val pftResp = Output(Vec(1, ValidIO(new L1DBPResp)))
    val update = Flipped(ValidIO(new L1DBPUpdate))
  })

  def scUpdate(state: UInt, accessed: Bool): UInt = {
    val accessedUpdate = Mux(state === 3.U, 3.U, state + 1.U)
    val notAccessedUpdate = Mux(state(1) === 0.U, 0.U, state - 1.U)
    Mux(accessed, accessedUpdate, notAccessedUpdate)
  }

  val s1Upd = WireInit(0.U.asTypeOf(io.update))
  s1Upd.valid := RegNext(io.update.valid, false.B)
  s1Upd.bits := RegEnable(io.update.bits, io.update.valid)

  val pcSCArray = RegInit(VecInit(Seq.fill(l1dbpPcPredictorEntries)(3.U(2.W))))
  val pfSCArray = RegInit(VecInit(Seq.fill(2)(3.U(2.W))))
  val forceDead = l1dbpParams.debugMode.B
  val enablePrefetchPrediction = l1dbpParams.enablePrefetchPrediction.B

  io.pftQuery.foreach { query =>
    when (query.valid) {
      assert(query.bits.idx <= 1.U, "L1DBP prefetch query index must be Stream(0) or Stride(1)")
    }
  }
  when (io.update.valid) {
    assert(io.update.bits.origin === L1DBPOrigin.demand ||
      io.update.bits.origin === L1DBPOrigin.stream ||
      io.update.bits.origin === L1DBPOrigin.stride,
      "L1DBP update origin must be Demand, Stream, or Stride")
  }
  when (io.update.valid && io.update.bits.origin =/= L1DBPOrigin.demand) {
    assert(io.update.bits.idx <= 1.U, "L1DBP prefetch update index must be Stream(0) or Stride(1)")
  }

  io.demandQuery.zip(io.demandResp).foreach { case (query, resp) =>
    val s1Query = WireInit(0.U.asTypeOf(Valid(new L1DBPRead)))
    s1Query.valid := RegNext(query.valid, false.B)
    s1Query.bits := RegEnable(query.bits, query.valid)
    val s0BypassEn = io.update.valid && io.update.bits.idx === s1Query.bits.idx &&
      io.update.bits.origin === L1DBPOrigin.demand
    val s1BypassEn = s1Upd.valid && s1Upd.bits.idx === s1Query.bits.idx &&
      s1Upd.bits.origin === L1DBPOrigin.demand
    val s1PCArrayRead = pcSCArray(s1Query.bits.idx)
    val bypassByS1 = Mux(s1BypassEn, scUpdate(s1PCArrayRead, s1Upd.bits.accessed), s1PCArrayRead)
    val bypassByS0 = Mux(s0BypassEn, scUpdate(bypassByS1, io.update.bits.accessed), bypassByS1)
    resp.valid := s1Query.valid
    resp.bits.dead := forceDead || bypassByS0 === 0.U
  }

  io.pftQuery.zip(io.pftResp).foreach { case (query, resp) =>
    val s1Query = WireInit(0.U.asTypeOf(Valid(new L1DBPRead)))
    s1Query.valid := RegNext(query.valid, false.B)
    s1Query.bits := RegEnable(query.bits, query.valid)
    val s0BypassEn = io.update.valid && io.update.bits.pfIdx === s1Query.bits.pfIdx &&
      io.update.bits.origin =/= L1DBPOrigin.demand
    val s1BypassEn = s1Upd.valid && s1Upd.bits.pfIdx === s1Query.bits.pfIdx &&
      s1Upd.bits.origin =/= L1DBPOrigin.demand
    val s1PfArrayRead = if (l1dbpParams.enablePrefetchPrediction) {
      pfSCArray(s1Query.bits.pfIdx)
    } else {
      1.U(2.W)
    }
    val bypassByS1 = Mux(s1BypassEn, scUpdate(s1PfArrayRead, s1Upd.bits.accessed), s1PfArrayRead)
    val bypassByS0 = Mux(s0BypassEn, scUpdate(bypassByS1, io.update.bits.accessed), bypassByS1)
    resp.valid := s1Query.valid
    resp.bits.dead := enablePrefetchPrediction && (forceDead || bypassByS0 === 0.U)
  }

  val pcWen = s1Upd.valid && s1Upd.bits.origin === L1DBPOrigin.demand
  val pcWOH = UIntToOH(s1Upd.bits.idx, l1dbpPcPredictorEntries)
  (0 until l1dbpPcPredictorEntries).foreach { i =>
    when(pcWen && pcWOH(i)) {
      pcSCArray(i) := scUpdate(pcSCArray(i), s1Upd.bits.accessed)
    }
  }

  val pfWen = if (l1dbpParams.enablePrefetchPrediction) {
    s1Upd.valid && s1Upd.bits.origin =/= L1DBPOrigin.demand
  } else {
    false.B
  }
  val pfWOH = UIntToOH(s1Upd.bits.pfIdx, 2)
  (0 until 2).foreach { i =>
    when(pfWen && pfWOH(i)) {
      pfSCArray(i) := scUpdate(pfSCArray(i), s1Upd.bits.accessed)
    }
  }
}
