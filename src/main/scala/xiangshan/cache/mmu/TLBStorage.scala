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
import utils._
import utility._
import freechips.rocketchip.formal.PropertyClass
import xiangshan.backend.fu.util.HasCSRConst

import scala.math.min

// For Direct-map TLBs, we do not use it now
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

class TLBFA(
  parentName: String,
  ports: Int,
  nDups: Int,
  nSets: Int,
  nWays: Int,
  saveLevel: Boolean = false,
  normalPage: Boolean,
  superPage: Boolean
)(implicit p: Parameters) extends TlbModule with HasPerfEvents {

  val io = IO(new TlbStorageIO(nSets, nWays, ports, nDups))
  io.r.req.map(_.ready := true.B)

  val v = RegInit(VecInit(Seq.fill(nWays)(false.B)))
  val entries = Reg(Vec(nWays, new TlbSectorEntry(normalPage, superPage)))
  val g = entries.map(_.perm.g)

  for (i <- 0 until ports) {
    val req = io.r.req(i)
    val resp = io.r.resp(i)
    val access = io.access(i)

    val vpn = req.bits.vpn
    val vpn_reg = RegEnable(vpn, req.fire)

    val refill_mask = Mux(io.w.valid, UIntToOH(io.w.bits.wayIdx), 0.U(nWays.W))
    val hitVec = VecInit((entries.zipWithIndex).zip(v zip refill_mask.asBools).map{case (e, m) => e._1.hit(vpn, io.csr.satp.asid) && m._1 && !m._2 })

    hitVec.suggestName("hitVec")

    val hitVecReg = RegEnable(hitVec, req.fire)
    // Sector tlb may trigger multi-hit, see def "wbhit"
    XSPerfAccumulate(s"port${i}_multi_hit", !(!resp.valid || (PopCount(hitVecReg) === 0.U || PopCount(hitVecReg) === 1.U)))

    resp.valid := RegNext(req.valid)
    resp.bits.hit := Cat(hitVecReg).orR
    if (nWays == 1) {
      for (d <- 0 until nDups) {
        resp.bits.ppn(d) := RegEnable(entries(0).genPPN(saveLevel, req.valid)(vpn), req.fire)
        resp.bits.perm(d) := RegEnable(entries(0).perm, req.fire)
      }
    } else {
      for (d <- 0 until nDups) {
        resp.bits.ppn(d) := RegEnable(ParallelMux(hitVec zip entries.map(_.genPPN(saveLevel, req.valid)(vpn))), req.fire)
        resp.bits.perm(d) := RegEnable(ParallelMux(hitVec zip entries.map(_.perm)), req.fire)
      }
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
    entries(io.w.bits.wayIdx).apply(io.w.bits.data, io.csr.satp.asid)
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

  XSPerfAccumulate(s"access", io.r.resp.map(_.valid.asUInt).fold(0.U)(_ + _))
  XSPerfAccumulate(s"hit", io.r.resp.map(a => a.valid && a.bits.hit).fold(0.U)(_.asUInt + _.asUInt))

  for (i <- 0 until nWays) {
    XSPerfAccumulate(s"access${i}", io.r.resp.zip(io.access.map(acc => UIntToOH(acc.touch_ways.bits))).map{ case (a, b) =>
      a.valid && a.bits.hit && b(i)}.fold(0.U)(_.asUInt + _.asUInt))
  }
  for (i <- 0 until nWays) {
    XSPerfAccumulate(s"refill${i}", io.w.valid && io.w.bits.wayIdx === i.U)
  }

  val perfEvents = Seq(
    ("tlbstore_access", io.r.resp.map(_.valid.asUInt).fold(0.U)(_ + _)                            ),
    ("tlbstore_hit   ", io.r.resp.map(a => a.valid && a.bits.hit).fold(0.U)(_.asUInt + _.asUInt)),
  )
  generatePerfEvent()

  println(s"${parentName} tlb_fa: nSets${nSets} nWays:${nWays}")
}

class TLBFakeFA(
             ports: Int,
             nDups: Int,
             nSets: Int,
             nWays: Int,
             useDmode: Boolean = false
           )(implicit p: Parameters) extends TlbModule with HasCSRConst{

  val io = IO(new TlbStorageIO(nSets, nWays, ports, nDups))
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
    for (d <- 0 until nDups) {
      resp.bits.perm(d).pf := pf
      resp.bits.perm(d).af := false.B
      resp.bits.perm(d).d := pte.perm.d
      resp.bits.perm(d).a := pte.perm.a
      resp.bits.perm(d).g := pte.perm.g
      resp.bits.perm(d).u := pte.perm.u
      resp.bits.perm(d).x := pte.perm.x
      resp.bits.perm(d).w := pte.perm.w
      resp.bits.perm(d).r := pte.perm.r

      resp.bits.ppn(d) := MuxLookup(level, 0.U, Seq(
        0.U -> Cat(ppn(ppn.getWidth-1, vpnnLen*2), vpn_reg(vpnnLen*2-1, 0)),
        1.U -> Cat(ppn(ppn.getWidth-1, vpnnLen), vpn_reg(vpnnLen-1, 0)),
        2.U -> ppn)
      )
    }
  }

  io.access := DontCare
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
      val storage = Module(new TLBFakeFA(ports, nDups, nSets, nWays, useDmode))
      storage.suggestName(s"${parentName}_fake_fa")
      storage.io
    } else {
       val storage = Module(new TLBFA(parentName, ports, nDups, nSets, nWays, saveLevel, normalPage, superPage))
       storage.suggestName(s"${parentName}_fa")
       storage.io
    }
  }
}

class TlbStorageWrapper(ports: Int, q: TLBParameters, nDups: Int = 1)(implicit p: Parameters) extends TlbModule {
  val io = IO(new TlbStorageWrapperIO(ports, q, nDups))

  val page = TlbStorage(
    parentName = q.name + "_storage",
    associative = q.Associative,
    ports = ports,
    nDups = nDups,
    nSets = q.NSets,
    nWays = q.NWays,
    normalPage = true,
    superPage = true,
    useDmode = q.useDmode,
    SoftTLB = coreParams.softTLB
  )

  for (i <- 0 until ports) {
    page.r_req_apply(
      valid = io.r.req(i).valid,
      vpn = io.r.req(i).bits.vpn,
      i = i
    )
  }

  for (i <- 0 until ports) {
    val q = page.r.req(i)
    val p = page.r.resp(i)
    val rq = io.r.req(i)
    val rp = io.r.resp(i)
    rq.ready := q.ready // actually, not used
    rp.valid := p.valid // actually, not used
    rp.bits.hit := p.bits.hit
    for (d <- 0 until nDups) {
      rp.bits.ppn(d) := p.bits.ppn(d)
      rp.bits.perm(d).pf := p.bits.perm(d).pf
      rp.bits.perm(d).af := p.bits.perm(d).af
      rp.bits.perm(d).d := p.bits.perm(d).d
      rp.bits.perm(d).a := p.bits.perm(d).a
      rp.bits.perm(d).g := p.bits.perm(d).g
      rp.bits.perm(d).u := p.bits.perm(d).u
      rp.bits.perm(d).x := p.bits.perm(d).x
      rp.bits.perm(d).w := p.bits.perm(d).w
      rp.bits.perm(d).r := p.bits.perm(d).r
    }
  }

  page.sfence <> io.sfence
  page.csr <> io.csr

  val refill_idx = if (q.outReplace) {
    io.replace.page.access <> page.access
    io.replace.page.chosen_set := DontCare
    io.replace.page.refillIdx
  } else {
    val re = ReplacementPolicy.fromString(q.Replacer, q.NWays)
    re.access(page.access.map(_.touch_ways))
    re.way
  }

  page.w_apply(
    valid = io.w.valid,
    wayIdx = refill_idx,
    data = io.w.bits.data
  )

    // replacement
  def get_access(one_hot: UInt, valid: Bool): Valid[UInt] = {
    val res = Wire(Valid(UInt(log2Up(one_hot.getWidth).W)))
    res.valid := Cat(one_hot).orR && valid
    res.bits := OHToUInt(one_hot)
    res
  }
}
