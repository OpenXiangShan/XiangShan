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
  superPage: Boolean,
  asvictim: Boolean
)(implicit p: Parameters) extends TlbModule{

  val io = IO(new TlbStorageIO(nSets, nWays, ports))
  io.r.req.map(_.ready := true.B)

  val v = RegInit(VecInit(Seq.fill(nWays)(false.B)))
  val entries = Reg(Vec(nWays, new TlbEntry(normalPage, superPage)))
  val g = entries.map(_.perm.g)

  for (i <- 0 until ports) {
    val req = io.r.req(i)
    val resp = io.r.resp(i)

    val vpn = req.bits.vpn
    val vpn_reg = if (sameCycle) vpn else RegEnable(vpn, req.fire())

    val refill_mask = if (sameCycle) 0.U(nWays.W)
                      else if (asvictim) Mux(io.w.valid || io.victim.in.valid, UIntToOH(io.w.bits.wayIdx), 0.U(nWays.W))
                      else Mux(io.w.valid, UIntToOH(io.w.bits.wayIdx), 0.U(nWays.W))
    val hitVec = VecInit(entries.zip(v zip refill_mask.asBools).map{case (e, m) => e.hit(vpn) && m._1 && !m._2})

    hitVec.suggestName("hitVec")

    val hitVecReg = if (sameCycle) hitVec else RegEnable(hitVec, req.fire())

    resp.valid := { if (sameCycle) req.valid else RegNext(req.valid) }
    resp.bits.hit := Cat(hitVecReg).orR
    resp.bits.ppn := ParallelMux(hitVecReg zip entries.map(_.genPPN(vpn_reg)))
    resp.bits.perm := ParallelMux(hitVecReg zip entries.map(_.perm))
    resp.bits.hitVec := hitVecReg.asUInt

    resp.bits.hit.suggestName("hit")
    resp.bits.ppn.suggestName("ppn")
    resp.bits.perm.suggestName("perm")
    resp.bits.hitVec.suggestName("hitVec")
  }

  // victim input has lower priority than ptw resp
  // fa may write two entries at same time, but we only have one wayIdx
  if (asvictim) {
    when (io.victim.in.valid) {
      v(io.w.bits.wayIdx) := true.B
      entries(io.w.bits.wayIdx) := { if(superPage) n_to_ns(io.victim.in.bits) else io.victim.in.bits }
    }
  }

  when (io.w.valid) {
    v(io.w.bits.wayIdx) := true.B
    entries(io.w.bits.wayIdx) := Wire(new TlbEntry(normalPage, superPage)).apply(io.w.bits.data)
  }

  val sfence = io.sfence
  val sfence_vpn = sfence.bits.addr.asTypeOf(new VaBundle().cloneType).vpn
  val sfenceHit = entries.map(_.hit(sfence_vpn))
  when (io.sfence.valid) {
    when (sfence.bits.rs1) { // virtual address *.rs1 <- (rs1===0.U)
      when (sfence.bits.rs2) { // asid, but i do not want to support asid, *.rs2 <- (rs2===0.U)
        // all addr and all asid
        v.map(_ := false.B)
      }.otherwise {
        // all addr but specific asid
        v.zipWithIndex.map{ case (a,i) => a := a & g(i) }
      }
    }.otherwise {
      when (sfence.bits.rs2) {
        // specific addr but all asid
        v.zipWithIndex.map{ case (a,i) => a := a & !sfenceHit(i) }
      }.otherwise {
        // specific addr and specific asid
        v.zipWithIndex.map{ case (a,i) => a := a & !(sfenceHit(i) && !g(i)) }
      }
    }
  }

  if (!asvictim) {
    val victim_idx = io.w.bits.wayIdx
    io.victim.out.valid := v(victim_idx) && io.w.valid && entries(victim_idx).level.getOrElse(3.U) === 2.U
    io.victim.out.bits := ns_to_n(entries(victim_idx))
  } else {
    io.victim.out := DontCare
  }


  def ns_to_n(ns: TlbEntry): TlbEntry = {
    val n = Wire(new TlbEntry(pageNormal = true, pageSuper = false))
    n.perm := ns.perm
    n.ppn := ns.ppn
    n.tag := ns.tag
    n
  }
  def n_to_ns(n: TlbEntry): TlbEntry = {
    val ns = Wire(new TlbEntry(pageNormal = true, pageSuper = true))
    ns.level.map(_ := 2.U)
    ns.perm := n.perm
    ns.ppn := n.ppn
    ns.tag := n.tag
    ns
  }
  XSPerfAccumulate(s"access", io.r.resp.map(_.valid.asUInt()).fold(0.U)(_ + _))
  XSPerfAccumulate(s"hit", io.r.resp.map(a => a.valid && a.bits.hit).fold(0.U)(_.asUInt() + _.asUInt()))

  for (i <- 0 until nWays) {
    XSPerfAccumulate(s"access${i}", io.r.resp.map(a => a.valid && a.bits.hit && a.bits.hitVec(i)).fold(0.U)(_.asUInt
    () + _.asUInt()))
  }
  for (i <- 0 until nWays) {
    XSPerfAccumulate(s"refill${i}", io.w.valid && io.w.bits.wayIdx === i.U)
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
  superPage: Boolean,
  asvictim: Boolean
)(implicit p: Parameters) extends TlbModule {
  require(!superPage, "super page should use reg/fa")
  require(!sameCycle, "sram needs next cycle")

  val io = IO(new TlbStorageIO(nSets, nWays, ports))

  io.r.req.map(_.ready := { if (sramSinglePort) !io.w.valid else true.B })
  val v = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(false.B)))))

  for (i <- 0 until ports) { // duplicate sram
//    val entries = Module(new SRAMTemplate(
    val entries = Module(new SRAMTemplateTemp(
      new TlbEntry(normalPage, superPage),
      set = nSets,
      way = nWays,
      singlePort = sramSinglePort
    ))

    val req = io.r.req(i)
    val resp = io.r.resp(i)

    val vpn = req.bits.vpn
    val vpn_reg = RegEnable(vpn, req.fire())

    val ridx = get_idx(vpn, nSets)
    val vidx = RegNext(Mux(req.fire(), v(ridx), VecInit(Seq.fill(nWays)(false.B))))
    entries.io.r.req.valid := req.valid
    entries.r_apply(setIdx = ridx)

    val data = entries.io.r.resp.data
    val hitVec = VecInit(data.zip(vidx).map{ case (e, vi) => e.hit(vpn_reg) && vi})
    resp.bits.hit := Cat(hitVec).orR
    resp.bits.ppn := ParallelMux(hitVec zip data.map(_.genPPN(vpn_reg)))
    resp.bits.perm := ParallelMux(hitVec zip data.map(_.perm))
    resp.bits.hitVec := hitVec.asUInt

    resp.valid := { if (sramSinglePort) RegNext(req.fire()) else RegNext(req.valid) }
    resp.bits.hit.suggestName("hit")
    resp.bits.ppn.suggestName("ppn")
    resp.bits.perm.suggestName("perm")
    resp.bits.hitVec.suggestName("hitVec")

    if (asvictim) {
      entries.w_apply(
        valid = io.w.valid || io.victim.in.valid,
        setIdx = Mux(io.w.valid, get_idx(io.w.bits.data.entry.tag, nSets), get_idx(io.victim.in.bits.tag, nSets)),
        data = Mux(io.w.valid, (Wire(new TlbEntry(normalPage, superPage)).apply(io.w.bits.data)), io.victim.in.bits),
        waymask = UIntToOH(io.w.bits.wayIdx)
      )
    } else {
      entries.w_apply(
        valid = io.w.valid,
        setIdx = get_idx(io.w.bits.data.entry.tag, nSets),
        data = (Wire(new TlbEntry(normalPage, superPage)).apply(io.w.bits.data)),
        waymask = UIntToOH(io.w.bits.wayIdx)
      )
    }

    entries.io.r_asy.req.setIdx := get_idx(io.w.bits.data.entry.tag, nSets)
    if (i == 0) {
      if (!asvictim) {
        io.victim.out.valid := v(get_idx(io.w.bits.data.entry.tag, nSets))(io.w.bits.wayIdx) && io.w.valid
        io.victim.out.bits := entries.io.r_asy.resp.data(io.w.bits.wayIdx)
      } else {
        io.victim.out := DontCare
      }
    }
  }

  when (io.w.valid) {
    v(get_idx(io.w.bits.data.entry.tag, nSets))(io.w.bits.wayIdx) := true.B
  }
  if (asvictim) {
    when (io.victim.in.valid) {
      v(get_idx(io.victim.in.bits.tag, nSets))(io.w.bits.wayIdx) := true.B
    }
  }


  val sfence = io.sfence
  val sfence_vpn = sfence.bits.addr.asTypeOf(new VaBundle().cloneType).vpn
  when (io.sfence.valid) {
    when (sfence.bits.rs1) { // virtual address *.rs1 <- (rs1===0.U)
      when (sfence.bits.rs2) { // asid, but i do not want to support asid, *.rs2 <- (rs2===0.U)
        // all addr and all asid
        v.map(a => a.map(b => b := false.B))
      }.otherwise {
        // all addr but specific asid
        // v.zipWithIndex.map{ case (a,i) => a := a & g(i) }
        v.map(a => a.map(b => b := false.B)) // TODO: handle g
      }
    }.otherwise {
      when (sfence.bits.rs2) {
        // specific addr but all asid
        v(get_idx(sfence_vpn, nSets)).map(_ := false.B)
      }.otherwise {
        // specific addr and specific asid
        v(get_idx(sfence_vpn, nSets)).map(_ := false.B)
      }
    }
  }

  XSPerfAccumulate(s"access", io.r.req.map(_.valid.asUInt()).fold(0.U)(_ + _))
  XSPerfAccumulate(s"hit", io.r.resp.map(a => a.valid && a.bits.hit).fold(0.U)(_.asUInt() + _.asUInt()))

  for (i <- 0 until nSets) {
    for (j <- 0 until nWays) {
      XSPerfAccumulate(s"refill${i}_${j}", (io.w.valid || io.victim.in.valid) &&
        (Mux(io.w.valid, get_idx(io.w.bits.data.entry.tag, nSets), get_idx(io.victim.in.bits.tag, nSets)) === i.U) &&
        (j.U === io.w.bits.wayIdx)
      )
    }
  }

  for (i <- 0 until nSets) {
    for (j <- 0 until nWays) {
      XSPerfAccumulate(s"hit${i}_${j}", io.r.resp.map(_.valid)
        .zip(io.r.resp.map(_.bits.hitVec(j)))
        .map{case(vi, hi) => vi && hi }
        .zip(io.r.req.map(a => RegNext(get_idx(a.bits.vpn, nSets)) === i.U))
        .map{a => (a._1 && a._2).asUInt()}
        .fold(0.U)(_ + _)
      )
    }
  }

  for (i <- 0 until nSets) {
    XSPerfAccumulate(s"access${i}", io.r.resp.map(_.valid)
      .zip(io.r.req.map(a => RegNext(get_idx(a.bits.vpn, nSets)) === i.U))
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
    superPage: Boolean,
    asVictim: Boolean
  )(implicit p: Parameters) = {
    if (associative == "fa") {
       val storage = Module(new TLBFA(sameCycle, ports, nSets, nWays, sramSinglePort, normalPage, superPage, asVictim))
       storage.suggestName(s"tlb_${name}_fa")
       storage.io
    } else {
       val storage = Module(new TLBSA(sameCycle, ports, nSets, nWays, sramSinglePort, normalPage, superPage, asVictim))
       storage.suggestName(s"tlb_${name}_sa")
       storage.io
    }
  }
}

class SRAMTemplateTemp[T <: Data](gen: T, set: Int, way: Int, singlePort: Boolean = false)(implicit p: Parameters)
  extends 
  XSModule {
  val io = IO(new Bundle {
    val r = new Bundle {
      val req = Flipped(Decoupled(new Bundle {
        val setIdx = Output(UInt(log2Up(set).W))
      }))
      val resp = new Bundle {
        val data = Output(Vec(way, gen))
      }
    }
    val r_asy = new Bundle {
      val req = Flipped(new Bundle {
        val setIdx = Output(UInt(log2Up(set).W))
      })
      val resp = new Bundle {
        val data = Output(Vec(way, gen))
      }
    }
    val w = Flipped(Decoupled(new Bundle {
      val setIdx = Output(UInt(log2Up(set).W))
      val data = Output(gen)
      val wayMask = Output(UInt(way.W))
    })

    )
  })
  def r_apply(setIdx: UInt) = {
    this.io.r.req.bits.setIdx := setIdx
  }
  def w_apply(valid: Bool, setIdx: UInt, data: T, waymask: UInt) = {
    this.io.w.valid := valid
    this.io.w.bits.setIdx := setIdx
    this.io.w.bits.data := data
    this.io.w.bits.wayMask := waymask
  }
  val entries = Reg(Vec(set, Vec(way, gen)))
  when (io.w.valid) {
    val wayIdx = OHToUInt(io.w.bits.wayMask)
    entries(io.w.bits.setIdx)(wayIdx) := io.w.bits.data
  }
  io.w.ready := true.B
  
  io.r.req.ready := !io.w.valid
  io.r.resp.data := RegNext(entries(io.r.req.bits.setIdx))
  
  io.r_asy.resp.data := entries(io.r_asy.req.setIdx)
}