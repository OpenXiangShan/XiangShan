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
import chisel3.experimental.chiselName
import chisel3.util._
import utils._
import freechips.rocketchip.formal.PropertyClass

import scala.math.min

@chiselName
class TLBFA(
  parentName: String,
  ports: Int,
  nSets: Int,
  nWays: Int,
  saveLevel: Boolean = false,
  normalPage: Boolean,
  superPage: Boolean
)(implicit p: Parameters) extends TlbModule with HasPerfEvents {

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
    val vpn_reg = RegEnable(vpn, req.fire())
    val vpn_gen_ppn = if(saveLevel) vpn else vpn_reg

    val refill_mask = Mux(io.w.valid, UIntToOH(io.w.bits.wayIdx), 0.U(nWays.W))
    val hitVec = VecInit((entries.zipWithIndex).zip(v zip refill_mask.asBools).map{case (e, m) => e._1.hit(vpn, io.csr.satp.asid) && m._1 && !m._2 })

    hitVec.suggestName("hitVec")

    val hitVecReg = RegEnable(hitVec, req.fire())
    assert(!resp.valid || (PopCount(hitVecReg) === 0.U || PopCount(hitVecReg) === 1.U), s"${parentName} fa port${i} multi-hit")

    resp.valid := RegNext(req.valid)
    resp.bits.hit := Cat(hitVecReg).orR
    if (nWays == 1) {
      resp.bits.ppn := entries(0).genPPN(saveLevel, req.valid)(vpn_gen_ppn)
      resp.bits.perm := entries(0).perm
    } else {
      resp.bits.ppn := ParallelMux(hitVecReg zip entries.map(_.genPPN(saveLevel, req.valid)(vpn_gen_ppn)))
      resp.bits.perm := ParallelMux(hitVecReg zip entries.map(_.perm))
    }

    access.sets := get_set_idx(vpn_reg, nSets) // no use
    access.touch_ways.valid := resp.valid && Cat(hitVecReg).orR
    access.touch_ways.bits := OHToUInt(hitVecReg)

    resp.bits.hit.suggestName("hit")
    resp.bits.ppn.suggestName("ppn")
    resp.bits.perm.suggestName("perm")
  }

  when (io.w.valid) {
    v(io.w.bits.wayIdx) := true.B
    entries(io.w.bits.wayIdx).apply(io.w.bits.data, io.csr.satp.asid, io.w.bits.data_replenish)
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
  io.victim.out.valid := v(victim_idx) && io.w.valid && entries(victim_idx).is_normalentry()
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

  val perfEvents = Seq(
    ("tlbstore_access", io.r.resp.map(_.valid.asUInt()).fold(0.U)(_ + _)                            ),
    ("tlbstore_hit   ", io.r.resp.map(a => a.valid && a.bits.hit).fold(0.U)(_.asUInt() + _.asUInt())),
  )
  generatePerfEvent()

  println(s"${parentName} tlb_fa: nSets${nSets} nWays:${nWays}")
}

@chiselName
class TLBSA(
  parentName: String,
  ports: Int,
  nSets: Int,
  nWays: Int,
  normalPage: Boolean,
  superPage: Boolean
)(implicit p: Parameters) extends TlbModule {
  require(!superPage, "super page should use reg/fa")
  require(nWays == 1, "nWays larger than 1 causes bad timing")

  // timing optimization to divide v select into two cycles.
  val VPRE_SELECT = min(8, nSets)
  val VPOST_SELECT = nSets / VPRE_SELECT

  val io = IO(new TlbStorageIO(nSets, nWays, ports))

  io.r.req.map(_.ready :=  true.B)
  val v = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(false.B)))))
  val entries = Module(new SyncDataModuleTemplate(new TlbEntry(normalPage, superPage), nSets, ports, 1))

  for (i <- 0 until ports) { // duplicate sram
    val req = io.r.req(i)
    val resp = io.r.resp(i)
    val access = io.access(i)

    val vpn = req.bits.vpn
    val vpn_reg = RegEnable(vpn, req.fire())

    val ridx = get_set_idx(vpn, nSets)
    val v_resize = v.asTypeOf(Vec(VPRE_SELECT, Vec(VPOST_SELECT, UInt(nWays.W))))
    val vidx_resize = RegNext(v_resize(get_set_idx(drop_set_idx(vpn, VPOST_SELECT), VPRE_SELECT)))
    val vidx = vidx_resize(get_set_idx(vpn_reg, VPOST_SELECT)).asBools.map(_ && RegNext(req.fire()))
    val vidx_bypass = RegNext((entries.io.waddr(0) === ridx) && entries.io.wen(0))
    entries.io.raddr(i) := ridx

    val data = entries.io.rdata(i)
    val hit = data.hit(vpn_reg, io.csr.satp.asid, nSets) && (vidx(0) || vidx_bypass)
    resp.bits.hit := hit
    resp.bits.ppn := data.genPPN()(vpn_reg)
    resp.bits.perm := data.perm

    resp.valid := { RegNext(req.valid) }
    resp.bits.hit.suggestName("hit")
    resp.bits.ppn.suggestName("ppn")
    resp.bits.perm.suggestName("perm")

    access.sets := get_set_idx(vpn_reg, nSets) // no use
    access.touch_ways.valid := resp.valid && hit
    access.touch_ways.bits := 1.U // TODO: set-assoc need no replacer when nset is 1
  }

  // W ports should be 1, or, check at above will be wrong.
  entries.io.wen(0) := io.w.valid || io.victim.in.valid
  entries.io.waddr(0) := Mux(io.w.valid,
    get_set_idx(io.w.bits.data.entry.tag, nSets),
    get_set_idx(io.victim.in.bits.entry.tag, nSets))
  entries.io.wdata(0) := Mux(io.w.valid,
    (Wire(new TlbEntry(normalPage, superPage)).apply(io.w.bits.data, io.csr.satp.asid, io.w.bits.data_replenish)),
    io.victim.in.bits.entry)

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
  io.victim.out.valid := false.B

  XSPerfAccumulate(s"access", io.r.req.map(_.valid.asUInt()).fold(0.U)(_ + _))
  XSPerfAccumulate(s"hit", io.r.resp.map(a => a.valid && a.bits.hit).fold(0.U)(_.asUInt() + _.asUInt()))

  for (i <- 0 until nSets) {
    XSPerfAccumulate(s"refill${i}", (io.w.valid || io.victim.in.valid) &&
        (Mux(io.w.valid, get_set_idx(io.w.bits.data.entry.tag, nSets), get_set_idx(io.victim.in.bits.entry.tag, nSets)) === i.U)
      )
  }

  for (i <- 0 until nSets) {
    XSPerfAccumulate(s"hit${i}", io.r.resp.map(a => a.valid & a.bits.hit)
      .zip(io.r.req.map(a => RegNext(get_set_idx(a.bits.vpn, nSets)) === i.U))
      .map{a => (a._1 && a._2).asUInt()}
      .fold(0.U)(_ + _)
    )
  }

  for (i <- 0 until nSets) {
    XSPerfAccumulate(s"access${i}", io.r.resp.map(_.valid)
      .zip(io.r.req.map(a => RegNext(get_set_idx(a.bits.vpn, nSets)) === i.U))
      .map{a => (a._1 && a._2).asUInt()}
      .fold(0.U)(_ + _)
    )
  }

  println(s"${parentName} tlb_sa: nSets:${nSets} nWays:${nWays}")
}

object TlbStorage {
  def apply
  (
    parentName: String,
    associative: String,
    ports: Int,
    nSets: Int,
    nWays: Int,
    saveLevel: Boolean = false,
    normalPage: Boolean,
    superPage: Boolean
  )(implicit p: Parameters) = {
    if (associative == "fa") {
       val storage = Module(new TLBFA(parentName, ports, nSets, nWays, saveLevel, normalPage, superPage))
       storage.suggestName(s"${parentName}_fa")
       storage.io
    } else {
       val storage = Module(new TLBSA(parentName, ports, nSets, nWays, normalPage, superPage))
       storage.suggestName(s"${parentName}_sa")
       storage.io
    }
  }
}

class TlbStorageWrapper(ports: Int, q: TLBParameters)(implicit p: Parameters) extends TlbModule {
  val io = IO(new TlbStorageWrapperIO(ports, q))

// TODO: wrap Normal page and super page together, wrap the declare & refill dirty codes
  val normalPage = TlbStorage(
    parentName = q.name + "_storage",
    associative = q.normalAssociative,
    ports = ports,
    nSets = q.normalNSets,
    nWays = q.normalNWays,
    saveLevel = q.saveLevel,
    normalPage = true,
    superPage = false
  )
  val superPage = TlbStorage(
    parentName = q.name + "_storage",
    associative = q.superAssociative,
    ports = ports,
    nSets = q.superNSets,
    nWays = q.superNWays,
    normalPage = q.normalAsVictim,
    superPage = true,
  )

  for (i <- 0 until ports) {
    normalPage.r_req_apply(
      valid = io.r.req(i).valid,
      vpn = io.r.req(i).bits.vpn,
      i = i
    )
    superPage.r_req_apply(
      valid = io.r.req(i).valid,
      vpn = io.r.req(i).bits.vpn,
      i = i
    )
  }

  for (i <- 0 until ports) {
    val nq = normalPage.r.req(i)
    val np = normalPage.r.resp(i)
    val sq = superPage.r.req(i)
    val sp = superPage.r.resp(i)
    val rq = io.r.req(i)
    val rp = io.r.resp(i)
    rq.ready := nq.ready && sq.ready // actually, not used
    rp.valid := np.valid && sp.valid // actually, not used
    rp.bits.hit := np.bits.hit || sp.bits.hit
    rp.bits.ppn := Mux(sp.bits.hit, sp.bits.ppn, np.bits.ppn)
    rp.bits.perm := Mux(sp.bits.hit, sp.bits.perm, np.bits.perm)
    rp.bits.super_hit := sp.bits.hit
    rp.bits.super_ppn := sp.bits.ppn
    rp.bits.spm := np.bits.perm.pm
    assert(!np.bits.hit || !sp.bits.hit || !rp.valid, s"${q.name} storage ports${i} normal and super multi-hit")
  }

  normalPage.victim.in <> superPage.victim.out
  normalPage.victim.out <> superPage.victim.in
  normalPage.sfence <> io.sfence
  superPage.sfence <> io.sfence
  normalPage.csr <> io.csr
  superPage.csr <> io.csr

  val normal_refill_idx = if (q.outReplace) {
    io.replace.normalPage.access <> normalPage.access
    io.replace.normalPage.chosen_set := get_set_idx(io.w.bits.data.entry.tag, q.normalNSets)
    io.replace.normalPage.refillIdx
  } else if (q.normalAssociative == "fa") {
    val re = ReplacementPolicy.fromString(q.normalReplacer, q.normalNWays)
    re.access(normalPage.access.map(_.touch_ways)) // normalhitVecVec.zipWithIndex.map{ case (hv, i) => get_access(hv, validRegVec(i))})
    re.way
  } else { // set-acco && plru
    val re = ReplacementPolicy.fromString(q.normalReplacer, q.normalNSets, q.normalNWays)
    re.access(normalPage.access.map(_.sets), normalPage.access.map(_.touch_ways))
    re.way(get_set_idx(io.w.bits.data.entry.tag, q.normalNSets))
  }

  val super_refill_idx = if (q.outReplace) {
    io.replace.superPage.access <> superPage.access
    io.replace.superPage.chosen_set := DontCare
    io.replace.superPage.refillIdx
  } else {
    val re = ReplacementPolicy.fromString(q.superReplacer, q.superNWays)
    re.access(superPage.access.map(_.touch_ways))
    re.way
  }

  normalPage.w_apply(
    valid = { if (q.normalAsVictim) false.B
    else io.w.valid && io.w.bits.data.entry.level.get === 2.U },
    wayIdx = normal_refill_idx,
    data = io.w.bits.data,
    data_replenish = io.w.bits.data_replenish
  )
  superPage.w_apply(
    valid = { if (q.normalAsVictim) io.w.valid
    else io.w.valid && io.w.bits.data.entry.level.get =/= 2.U },
    wayIdx = super_refill_idx,
    data = io.w.bits.data,
    data_replenish = io.w.bits.data_replenish
  )

    // replacement
  def get_access(one_hot: UInt, valid: Bool): Valid[UInt] = {
    val res = Wire(Valid(UInt(log2Up(one_hot.getWidth).W)))
    res.valid := Cat(one_hot).orR && valid
    res.bits := OHToUInt(one_hot)
    res
  }
}
