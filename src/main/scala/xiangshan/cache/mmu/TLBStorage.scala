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
import chisel3.experimental.{ExtModule, chiselName}
import chisel3.util._
import utils._
import utility._
import freechips.rocketchip.formal.PropertyClass
import xiangshan.backend.fu.util.HasCSRConst

import scala.math.min

class BankedAsyncDataModuleTemplateWithDup[T <: Data](
  gen: T,
  numEntries: Int,
  numRead: Int,
  numDup: Int,
  numBanks: Int
) extends Module {
  val io = IO(new Bundle {
    val raddr = Vec(numRead, Input(UInt(log2Ceil(numEntries).W)))
    val rdata = Vec(numRead, Vec(numDup, Output(gen)))
    val wen   = Input(Bool())
    val waddr = Input(UInt(log2Ceil(numEntries).W))
    val wdata = Input(gen)
  })
  require(numBanks > 1)
  require(numEntries > numBanks)

  val numBankEntries = numEntries / numBanks
  def bankOffset(address: UInt): UInt = {
    address(log2Ceil(numBankEntries) - 1, 0)
  }

  def bankIndex(address: UInt): UInt = {
    address(log2Ceil(numEntries) - 1, log2Ceil(numBankEntries))
  }

  val dataBanks = Seq.tabulate(numBanks)(i => {
    val bankEntries = if (i < numBanks - 1) numBankEntries else (numEntries - (i * numBankEntries))
    Mem(bankEntries, gen)
  })

  // async read, but regnext
  for (i <- 0 until numRead) {
    val data_read = Reg(Vec(numDup, Vec(numBanks, gen)))
    val bank_index = Reg(Vec(numDup, UInt(numBanks.W)))
    for (j <- 0 until numDup) {
      bank_index(j) := UIntToOH(bankIndex(io.raddr(i)))
      for (k <- 0 until numBanks) {
        data_read(j)(k) := Mux(io.wen && (io.waddr === io.raddr(i)),
          io.wdata, dataBanks(k)(bankOffset(io.raddr(i))))
      }
    }
    // next cycle
    for (j <- 0 until numDup) {
      io.rdata(i)(j) := Mux1H(bank_index(j), data_read(j))
    }
  }

  // write
  for (i <- 0 until numBanks) {
    when (io.wen && (bankIndex(io.waddr) === i.U)) {
      dataBanks(i)(bankOffset(io.waddr)) := io.wdata
    }
  }
}

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
  val entries = Reg(Vec(nWays, new TlbSectorEntry(normalPage, superPage)))
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
    // Sector tlb may trigger multi-hit, see def "wbhit"
    XSPerfAccumulate(s"port${i}_multi_hit", !(!resp.valid || (PopCount(hitVecReg) === 0.U || PopCount(hitVecReg) === 1.U)))
    // assert(!resp.valid || (PopCount(hitVecReg) === 0.U || PopCount(hitVecReg) === 1.U), s"${parentName} fa port${i} multi-hit")

    resp.valid := RegNext(req.valid)
    resp.bits.hit := Cat(hitVecReg).orR
    if (nWays == 1) {
      resp.bits.ppn(0) := entries(0).genPPN(saveLevel, req.valid)(vpn_gen_ppn)
      resp.bits.perm(0) := entries(0).perm
    } else {
      resp.bits.ppn(0) := ParallelMux(hitVecReg zip entries.map(_.genPPN(saveLevel, req.valid)(vpn_gen_ppn)))
      resp.bits.perm(0) := ParallelMux(hitVecReg zip entries.map(_.perm))
    }

    access.sets := get_set_idx(vpn_reg(vpn_reg.getWidth - 1, sectortlbwidth), nSets) // no use
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
  // write assert, should not duplicate with the existing entries
  val w_hit_vec = VecInit(entries.zip(v).map{case (e, vi) => e.wbhit(io.w.bits.data, io.csr.satp.asid) && vi })
  XSError(io.w.valid && Cat(w_hit_vec).orR, s"${parentName} refill, duplicate with existing entries")

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
  // Sfence will flush all sectors of an entry when hit
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

  def ns_to_n(ns: TlbSectorEntry): TlbEntry = {
    val n = Wire(new TlbEntry(pageNormal = true, pageSuper = false))
    n.perm.af := ns.perm.af
    n.perm.pf := ns.perm.pf
    n.perm.d := ns.perm.d
    n.perm.a := ns.perm.a
    n.perm.g := ns.perm.g
    n.perm.u := ns.perm.u
    n.perm.x := ns.perm.x
    n.perm.w := ns.perm.w
    n.perm.r := ns.perm.r
    n.perm.pm := ns.perm.pm(OHToUInt(ns.pteidx))
    n.ppn := Cat(ns.ppn, ns.ppn_low(OHToUInt(ns.pteidx)))
    n.tag := Cat(ns.tag, OHToUInt(ns.pteidx))
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
  nDups: Int,
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
  val nBanks = 8

  val io = IO(new TlbStorageIO(nSets, nWays, ports, nDups))

  io.r.req.map(_.ready :=  true.B)
  val v = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(false.B)))))
  val entries = Module(new BankedAsyncDataModuleTemplateWithDup(new TlbEntry(normalPage, superPage), nSets, ports, nDups, nBanks))

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
    val vidx_bypass = RegNext((entries.io.waddr === ridx) && entries.io.wen)
    entries.io.raddr(i) := ridx

    val data = entries.io.rdata(i)
    val hit = data(0).hit(vpn_reg, io.csr.satp.asid, nSets) && (vidx(0) || vidx_bypass)
    resp.bits.hit := hit
    for (d <- 0 until nDups) {
      resp.bits.ppn(d) := data(d).genPPN()(vpn_reg)
      resp.bits.perm(d).pf := data(d).perm.pf
      resp.bits.perm(d).af := data(d).perm.af
      resp.bits.perm(d).d := data(d).perm.d
      resp.bits.perm(d).a := data(d).perm.a
      resp.bits.perm(d).g := data(d).perm.g
      resp.bits.perm(d).u := data(d).perm.u
      resp.bits.perm(d).x := data(d).perm.x
      resp.bits.perm(d).w := data(d).perm.w
      resp.bits.perm(d).r := data(d).perm.r
      for (i <- 0 until tlbcontiguous) {
        resp.bits.perm(d).pm(i) := data(d).perm.pm
      }
    }

    resp.valid := { RegNext(req.valid) }
    resp.bits.hit.suggestName("hit")
    resp.bits.ppn.suggestName("ppn")
    resp.bits.perm.suggestName("perm")

    access.sets := get_set_idx(vpn_reg, nSets) // no use
    access.touch_ways.valid := resp.valid && hit
    access.touch_ways.bits := 1.U // TODO: set-assoc need no replacer when nset is 1
  }

  // W ports should be 1, or, check at above will be wrong.
  entries.io.wen := io.w.valid || io.victim.in.valid
  entries.io.waddr := Mux(io.w.valid,
    get_set_idx(io.w.bits.data.entry.tag, nSets),
    get_set_idx(io.victim.in.bits.entry.tag, nSets))
  entries.io.wdata := Mux(io.w.valid,
    (Wire(new TlbEntry(normalPage, superPage)).apply(io.w.bits.data, io.csr.satp.asid, io.w.bits.data_replenish(OHToUInt(io.w.bits.data.pteidx)))),
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

@chiselName
class TLBFakeSP(
             ports: Int,
             nSets: Int,
             nWays: Int,
             useDmode: Boolean = false
           )(implicit p: Parameters) extends TlbModule with HasCSRConst{

  val io = IO(new TlbStorageIO(nSets, nWays, ports))
  io.r.req.map(_.ready := true.B)
  val mode = if (useDmode) io.csr.priv.dmode else io.csr.priv.imode
  val vmEnable = if (EnbaleTlbDebug) (io.csr.satp.mode === 8.U)
    else (io.csr.satp.mode === 8.U && (mode < ModeM))

  for (i <- 0 until ports) {
    val req = io.r.req(i)
    val resp = io.r.resp(i)

    val helper = Module(new PTEHelper())
    helper.clock := clock
    helper.satp := io.csr.satp.ppn
    helper.enable := req.fire && vmEnable
    helper.vpn := req.bits.vpn

    val pte = helper.pte.asTypeOf(new PteBundle)
    val ppn = pte.ppn
    val vpn_reg = RegNext(req.bits.vpn)
    val pf = helper.pf
    val level = helper.level

    resp.valid := RegNext(req.valid)
    resp.bits.hit := true.B
    resp.bits.perm(0).pf := pf
    resp.bits.perm(0).af := false.B
    resp.bits.perm(0).d := pte.perm.d
    resp.bits.perm(0).a := pte.perm.a
    resp.bits.perm(0).g := pte.perm.g
    resp.bits.perm(0).u := pte.perm.u
    resp.bits.perm(0).x := pte.perm.x
    resp.bits.perm(0).w := pte.perm.w
    resp.bits.perm(0).r := pte.perm.r
    resp.bits.perm(0).pm := DontCare

    resp.bits.ppn(0) := MuxLookup(level, 0.U, Seq(
      0.U -> Cat(ppn(ppn.getWidth-1, vpnnLen*2), vpn_reg(vpnnLen*2-1, 0)),
      1.U -> Cat(ppn(ppn.getWidth-1, vpnnLen), vpn_reg(vpnnLen-1, 0)),
      2.U -> ppn)
    )
  }

  io.access := DontCare
  io.victim.out := DontCare

}

@chiselName
class TLBFakeNP(
             ports: Int,
             nDups: Int,
             nSets: Int,
             nWays: Int
           )(implicit p: Parameters) extends TlbModule {

  val io = IO(new TlbStorageIO(nSets, nWays, ports, nDups))

  io.r.req.map(_.ready :=  true.B)
  io.r.resp := DontCare
  io.access := DontCare
  io.victim.out := DontCare
}

object TlbStorage {
  def apply
  (
    parentName: String,
    associative: String,
    ports: Int,
    nDups: Int = 1,
    nSets: Int,
    nWays: Int,
    saveLevel: Boolean = false,
    normalPage: Boolean,
    superPage: Boolean,
    useDmode: Boolean,
    SoftTLB: Boolean
  )(implicit p: Parameters) = {
    if (SoftTLB) {
      if (superPage == true) {
        val storage = Module(new TLBFakeSP(ports, nSets, nWays, useDmode))
        storage.suggestName(s"${parentName}_fakesp")
        storage.io
      } else {
        val storage = Module(new TLBFakeNP(ports, nDups, nSets, nWays))
        storage.suggestName(s"${parentName}_fakenp")
        storage.io
      }
    } else {
      if (associative == "fa") {
        val storage = Module(new TLBFA(parentName, ports, nSets, nWays, saveLevel, normalPage, superPage))
        storage.suggestName(s"${parentName}_fa")
        storage.io
      } else {
        val storage = Module(new TLBSA(parentName, ports, nDups, nSets, nWays, normalPage, superPage))
        storage.suggestName(s"${parentName}_sa")
        storage.io
      }
    }
  }
}

class TlbStorageWrapper(ports: Int, q: TLBParameters, nDups: Int = 1)(implicit p: Parameters) extends TlbModule {
  val io = IO(new TlbStorageWrapperIO(ports, q, nDups))

// TODO: wrap Normal page and super page together, wrap the declare & refill dirty codes
  val normalPage = TlbStorage(
    parentName = q.name + "_np_storage",
    associative = q.normalAssociative,
    ports = ports,
    nDups = nDups,
    nSets = q.normalNSets,
    nWays = q.normalNWays,
    saveLevel = q.saveLevel,
    normalPage = true,
    superPage = false,
    useDmode = q.useDmode,
    SoftTLB = coreParams.softTLB
  )
  val superPage = TlbStorage(
    parentName = q.name + "_sp_storage",
    associative = q.superAssociative,
    ports = ports,
    nSets = q.superNSets,
    nWays = q.superNWays,
    normalPage = q.normalAsVictim,
    superPage = true,
    useDmode = q.useDmode,
    SoftTLB = coreParams.softTLB
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
    for (d <- 0 until nDups) {
      rp.bits.ppn(d) := Mux(sp.bits.hit, sp.bits.ppn(0), np.bits.ppn(d))
      rp.bits.perm(d).pf := Mux(sp.bits.hit, sp.bits.perm(0).pf, np.bits.perm(d).pf)
      rp.bits.perm(d).af := Mux(sp.bits.hit, sp.bits.perm(0).af, np.bits.perm(d).af)
      rp.bits.perm(d).d := Mux(sp.bits.hit, sp.bits.perm(0).d, np.bits.perm(d).d)
      rp.bits.perm(d).a := Mux(sp.bits.hit, sp.bits.perm(0).a, np.bits.perm(d).a)
      rp.bits.perm(d).g := Mux(sp.bits.hit, sp.bits.perm(0).g, np.bits.perm(d).g)
      rp.bits.perm(d).u := Mux(sp.bits.hit, sp.bits.perm(0).u, np.bits.perm(d).u)
      rp.bits.perm(d).x := Mux(sp.bits.hit, sp.bits.perm(0).x, np.bits.perm(d).x)
      rp.bits.perm(d).w := Mux(sp.bits.hit, sp.bits.perm(0).w, np.bits.perm(d).w)
      rp.bits.perm(d).r := Mux(sp.bits.hit, sp.bits.perm(0).r, np.bits.perm(d).r)
      rp.bits.perm(d).pm := DontCare
    }
    rp.bits.super_hit := sp.bits.hit
    rp.bits.super_ppn := sp.bits.ppn(0)
    rp.bits.spm := np.bits.perm(0).pm(0)
    // Sector tlb may trigger multi-hit, see def "wbhit"
    XSPerfAccumulate(s"port${i}_np_sp_multi_hit", !(!np.bits.hit || !sp.bits.hit || !rp.valid))
    //assert(!np.bits.hit || !sp.bits.hit || !rp.valid, s"${q.name} storage ports${i} normal and super multi-hit")
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
