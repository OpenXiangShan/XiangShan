/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  ***************************************************************************************/

package xiangshan.cache.mmu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.experimental.chiselName
import freechips.rocketchip.util.SRAMAnnotation
import xiangshan._
import utils._

@chiselName
class TLBFA(
  sameCycle: Boolean,
  ports: Int,
  nSets: Int,
  nWays: Int,
  sramSinglePort: Boolean,
  normalPage: Boolean,
  superPage: Boolean
)(implicit p: Parameters) extends TlbModule{

  val io = IO(new TlbStorageIO(nSets, nWays, ports))
  io.r.req.map(_.ready := true.B)

  val v = RegInit(VecInit(Seq.fill(nWays)(false.B)))
  val entries = Reg(Vec(nWays, new TlbEntry(normalPage, superPage)))
  val g = entries.map(_.perm.g)

  for (i <- 0 until ports) {
    val req = io.r.req(i)
    val resp = io.r.resp(i)
    val access = io.access(i)

    val vpn = req.bits.vpn
    val vpn_reg = if (sameCycle) vpn else RegEnable(vpn, req.fire())

    val refill_mask = if (sameCycle) 0.U(nWays.W) else Mux(io.w.valid, UIntToOH(io.w.bits.wayIdx), 0.U(nWays.W))
    val hitVec = VecInit((entries.zipWithIndex).zip(v zip refill_mask.asBools).map{case (e, m) => e._1.hit(vpn, io.csr.satp.asid) && m._1 && !m._2 })

    hitVec.suggestName("hitVec")

    val hitVecReg = if (sameCycle) hitVec else RegEnable(hitVec, req.fire())

    resp.valid := { if (sameCycle) req.valid else RegNext(req.valid) }
    resp.bits.hit := Cat(hitVecReg).orR
    resp.bits.ppn := ParallelMux(hitVecReg zip entries.map(_.genPPN(vpn_reg)))
    resp.bits.perm := ParallelMux(hitVecReg zip entries.map(_.perm))
    io.r.resp_hit_sameCycle(i) := Cat(hitVec).orR

    access.sets := get_set_idx(vpn_reg, nSets) // no use
    access.touch_ways.valid := resp.valid && Cat(hitVecReg).orR
    access.touch_ways.bits := OHToUInt(hitVecReg)

    resp.bits.hit.suggestName("hit")
    resp.bits.ppn.suggestName("ppn")
    resp.bits.perm.suggestName("perm")
  }

  when (io.w.valid) {
    v(io.w.bits.wayIdx) := true.B
    entries(io.w.bits.wayIdx).apply(io.w.bits.data, io.csr.satp.asid)
  }

  val refill_vpn_reg = RegNext(io.w.bits.data.entry.tag)
  val refill_wayIdx_reg = RegNext(io.w.bits.wayIdx)
  when (RegNext(io.w.valid)) {
    io.access.map { access =>
      access.sets := get_set_idx(refill_vpn_reg, nSets)
      access.touch_ways.valid := true.B
      access.touch_ways.bits := refill_wayIdx_reg
    }
  }

  val sfence = io.sfence
  val sfence_vpn = sfence.bits.addr.asTypeOf(new VaBundle().cloneType).vpn
  val sfenceHit = entries.map(_.hit(sfence_vpn, sfence.bits.asid))
  val sfenceHit_noasid = entries.map(_.hit(sfence_vpn, sfence.bits.asid, ignoreAsid = true))
  when (io.sfence.valid) {
    when (sfence.bits.rs1) { // virtual address *.rs1 <- (rs1===0.U)
      when (sfence.bits.rs2) { // asid, but i do not want to support asid, *.rs2 <- (rs2===0.U)
        // all addr and all asid
        v.map(_ := false.B)
      }.otherwise {
        // all addr but specific asid
        v.zipWithIndex.map{ case (a,i) => a := a & (g(i) | !(entries(i).asid === sfence.bits.asid)) }
      }
    }.otherwise {
      when (sfence.bits.rs2) {
        // specific addr but all asid
        v.zipWithIndex.map{ case (a,i) => a := a & !sfenceHit_noasid(i) }
      }.otherwise {
        // specific addr and specific asid
        v.zipWithIndex.map{ case (a,i) => a := a & !(sfenceHit(i) && !g(i)) }
      }
    }
  }

  val victim_idx = io.w.bits.wayIdx
  io.victim.out.valid := v(victim_idx) && io.w.valid && entries(victim_idx).level.getOrElse(3.U) === 2.U
  io.victim.out.bits.entry := ns_to_n(entries(victim_idx))

  def ns_to_n(ns: TlbEntry): TlbEntry = {
    val n = Wire(new TlbEntry(pageNormal = true, pageSuper = false))
    n.perm := ns.perm
    n.ppn := ns.ppn
    n.tag := ns.tag
    n.asid := ns.asid
    n
  }

  XSPerfAccumulate(s"access", io.r.resp.map(_.valid.asUInt()).fold(0.U)(_ + _))
  XSPerfAccumulate(s"hit", io.r.resp.map(a => a.valid && a.bits.hit).fold(0.U)(_.asUInt() + _.asUInt()))

  for (i <- 0 until nWays) {
    XSPerfAccumulate(s"access${i}", io.r.resp.zip(io.access.map(acc => UIntToOH(acc.touch_ways.bits))).map{ case (a, b) =>
      a.valid && a.bits.hit && b(i)}.fold(0.U)(_.asUInt() + _.asUInt()))
  }
  for (i <- 0 until nWays) {
    XSPerfAccumulate(s"refill${i}", io.w.valid && io.w.bits.wayIdx === i.U)
  }

  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(2))
  })
  val perfEvents = Seq(
    ("tlbstore_access            ", io.r.resp.map(_.valid.asUInt()).fold(0.U)(_ + _)                            ),
    ("tlbstore_hit               ", io.r.resp.map(a => a.valid && a.bits.hit).fold(0.U)(_.asUInt() + _.asUInt())),
  )

  for (((perf_out,(perf_name,perf)),i) <- perfinfo.perfEvents.perf_events.zip(perfEvents).zipWithIndex) {
    perf_out.incr_step := RegNext(perf)
  }

  println(s"tlb_fa: nSets${nSets} nWays:${nWays}")
}

@chiselName
class TLBSA(
  sameCycle: Boolean,
  ports: Int,
  nSets: Int,
  nWays: Int,
  sramSinglePort: Boolean,
  normalPage: Boolean,
  superPage: Boolean
)(implicit p: Parameters) extends TlbModule {
  require(!superPage, "super page should use reg/fa")
  require(!sameCycle, "sram needs next cycle")

  val io = IO(new TlbStorageIO(nSets, nWays, ports))

  io.r.req.map(_.ready := { if (sramSinglePort) !io.w.valid else true.B })
  val v = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(false.B)))))

  for (i <- 0 until ports) { // duplicate sram
    val entries = Module(new SRAMTemplate(
      new TlbEntry(normalPage, superPage),
      set = nSets,
      way = nWays,
      singlePort = sramSinglePort
    ))

    val req = io.r.req(i)
    val resp = io.r.resp(i)
    val access = io.access(i)

    val vpn = req.bits.vpn
    val vpn_reg = RegEnable(vpn, req.fire())

    val ridx = get_set_idx(vpn, nSets)
    val vidx = RegNext(Mux(req.fire(), v(ridx), VecInit(Seq.fill(nWays)(false.B))))
    entries.io.r.req.valid := req.valid
    entries.io.r.req.bits.apply(setIdx = ridx)

    val data = entries.io.r.resp.data
    val hitVec = VecInit(data.zip(vidx).map { case (e, vi) => e.hit(vpn_reg, io.csr.satp.asid) && vi })
    resp.bits.hit := Cat(hitVec).orR && RegNext(req.ready, init = false.B)
    resp.bits.ppn := ParallelMux(hitVec zip data.map(_.genPPN(vpn_reg)))
    resp.bits.perm := ParallelMux(hitVec zip data.map(_.perm))
    io.r.resp_hit_sameCycle(i) := DontCare

    resp.valid := {
      if (sramSinglePort) RegNext(req.fire()) else RegNext(req.valid)
    }
    resp.bits.hit.suggestName("hit")
    resp.bits.ppn.suggestName("ppn")
    resp.bits.perm.suggestName("perm")

    access.sets := get_set_idx(vpn_reg, nSets) // no use
    access.touch_ways.valid := resp.valid && Cat(hitVec).orR
    access.touch_ways.bits := OHToUInt(hitVec)

    entries.io.w.apply(
      valid = io.w.valid || io.victim.in.valid,
      setIdx = Mux(io.w.valid, get_set_idx(io.w.bits.data.entry.tag, nSets), get_set_idx(io.victim.in.bits.entry.tag, nSets)),
      data = Mux(io.w.valid, (Wire(new TlbEntry(normalPage, superPage)).apply(io.w.bits.data, io.csr.satp.asid)), io.victim.in.bits.entry),
      waymask = UIntToOH(io.w.bits.wayIdx)
    )
  }

  when (io.victim.in.valid) {
    v(get_set_idx(io.victim.in.bits.entry.tag, nSets))(io.w.bits.wayIdx) := true.B
  }
  // w has higher priority than victim
  when (io.w.valid) {
    v(get_set_idx(io.w.bits.data.entry.tag, nSets))(io.w.bits.wayIdx) := true.B
  }

  val refill_vpn_reg = RegNext(Mux(io.victim.in.valid, io.victim.in.bits.entry.tag, io.w.bits.data.entry.tag))
  val refill_wayIdx_reg = RegNext(io.w.bits.wayIdx)
  when (RegNext(io.w.valid || io.victim.in.valid)) {
    io.access.map { access =>
      access.sets := get_set_idx(refill_vpn_reg, nSets)
      access.touch_ways.valid := true.B
      access.touch_ways.bits := refill_wayIdx_reg
    }
  }

  val sfence = io.sfence
  val sfence_vpn = sfence.bits.addr.asTypeOf(new VaBundle().cloneType).vpn
  when (io.sfence.valid) {
    when (sfence.bits.rs1) { // virtual address *.rs1 <- (rs1===0.U)
        v.map(a => a.map(b => b := false.B))
    }.otherwise {
        // specific addr but all asid
        v(get_set_idx(sfence_vpn, nSets)).map(_ := false.B)
    }
  }

  io.victim.out := DontCare

  XSPerfAccumulate(s"access", io.r.req.map(_.valid.asUInt()).fold(0.U)(_ + _))
  XSPerfAccumulate(s"hit", io.r.resp.map(a => a.valid && a.bits.hit).fold(0.U)(_.asUInt() + _.asUInt()))

  for (i <- 0 until nSets) {
    for (j <- 0 until nWays) {
      XSPerfAccumulate(s"refill${i}_${j}", (io.w.valid || io.victim.in.valid) &&
        (Mux(io.w.valid, get_set_idx(io.w.bits.data.entry.tag, nSets), get_set_idx(io.victim.in.bits.entry.tag, nSets)) === i.U) &&
        (j.U === io.w.bits.wayIdx)
      )
    }
  }

  for (i <- 0 until nSets) {
    for (j <- 0 until nWays) {
      XSPerfAccumulate(s"hit${i}_${j}", io.r.resp.map(_.valid)
        .zip(io.access.map(a => UIntToOH(a.touch_ways.bits)(j)))
        .map{case(vi, hi) => vi && hi }
        .zip(io.r.req.map(a => RegNext(get_set_idx(a.bits.vpn, nSets)) === i.U))
        .map{a => (a._1 && a._2).asUInt()}
        .fold(0.U)(_ + _)
      )
    }
  }

  for (i <- 0 until nSets) {
    XSPerfAccumulate(s"access${i}", io.r.resp.map(_.valid)
      .zip(io.r.req.map(a => RegNext(get_set_idx(a.bits.vpn, nSets)) === i.U))
      .map{a => (a._1 && a._2).asUInt()}
      .fold(0.U)(_ + _)
    )
  }

  println(s"tlb_sa: nSets:${nSets} nWays:${nWays}")
}

object TlbStorage {
  def apply
  (
    name: String,
    associative: String,
    sameCycle: Boolean,
    ports: Int,
    nSets: Int,
    nWays: Int,
    sramSinglePort: Boolean,
    normalPage: Boolean,
    superPage: Boolean
  )(implicit p: Parameters) = {
    if (associative == "fa") {
       val storage = Module(new TLBFA(sameCycle, ports, nSets, nWays, sramSinglePort, normalPage, superPage))
       storage.suggestName(s"tlb_${name}_fa")
       storage.io
    } else {
       val storage = Module(new TLBSA(sameCycle, ports, nSets, nWays, sramSinglePort, normalPage, superPage))
       storage.suggestName(s"tlb_${name}_sa")
       storage.io
    }
  }
}
