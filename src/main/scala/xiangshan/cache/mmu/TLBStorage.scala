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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan.backend.fu.util.HasCSRConst

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
    val hasS2xlate = req.bits.s2xlate =/= noS2xlate
    val OnlyS2 = req.bits.s2xlate === onlyStage2
    val OnlyS1 = req.bits.s2xlate === onlyStage1
    val refill_mask = Mux(io.w.valid, UIntToOH(io.w.bits.wayIdx), 0.U(nWays.W))
    val hitVec = VecInit(entries.zip(v zip refill_mask.asBools).map {
      case (entry, (valid, refilling)) =>
        val stageMatch = entry.s2xlate === req.bits.s2xlate
        val hit = entry.hit(vpn, Mux(hasS2xlate, io.csr.vsatp.asid, io.csr.satp.asid),
          vmid = io.csr.hgatp.vmid, hasS2xlate = hasS2xlate, onlyS2 = OnlyS2, onlyS1 = OnlyS1)
        stageMatch && hit && valid && !refilling
    })

    hitVec.suggestName("hitVec")

    val hitVecReg = RegEnable(hitVec, req.fire)
    // Sector tlb may trigger multi-hit, see def "wbhit"
    XSPerfAccumulate(s"port${i}_multi_hit", !(!resp.valid || (PopCount(hitVecReg) === 0.U || PopCount(hitVecReg) === 1.U)))

    resp.valid := RegNext(req.valid)
    resp.bits.hit := Cat(hitVecReg).orR
    val reqVpn   = RegNext(vpn)
    def selectHit[T <: Data](values: Seq[T]): T = {
      if (nWays == 1) values.head else Mux1H(hitVecReg zip values)
    }

    for (d <- 0 until nDups) {
      resp.bits.ppn(d) := selectHit(entries.map(_.genPPN(saveLevel, resp.valid)(reqVpn)))
      resp.bits.pbmt(d) := selectHit(entries.map(_.pbmt))
      resp.bits.g_pbmt(d) := selectHit(entries.map(_.g_pbmt))
      resp.bits.perm(d) := selectHit(entries.map(_.perm))
      resp.bits.g_perm(d) := selectHit(entries.map(_.g_perm))
      resp.bits.s2xlate(d) := selectHit(entries.map(_.s2xlate))
      if (HasMptCheck) {
        val mptPerm = selectHit(entries.map(_.mptperm.get))
        resp.bits.mptperm.get(d).x := mptPerm.x
        resp.bits.mptperm.get(d).w := mptPerm.w
        resp.bits.mptperm.get(d).r := mptPerm.r
        resp.bits.mptperm.get(d).af.get := false.B
      }
    }

    access.sets := get_set_idx(vpn_reg(vpn_reg.getWidth - 1, sectortlbwidth), nSets) // no use
    access.touch_ways.valid := resp.valid && Cat(hitVecReg).orR
    access.touch_ways.bits := OHToUInt(hitVecReg)

    resp.bits.hit.suggestName("hit")
    resp.bits.ppn.suggestName("ppn")
    resp.bits.pbmt.suggestName("pbmt")
    resp.bits.g_pbmt.suggestName("g_pbmt")
    resp.bits.perm.suggestName("perm")
    resp.bits.g_perm.suggestName("g_perm")
  }

  when (io.w.valid) {
    v(io.w.bits.wayIdx) := true.B
    entries(io.w.bits.wayIdx).apply(io.w.bits.data)
  }
  // write assert, should not duplicate with the existing entries
  val w_hit_vec = VecInit(entries.zip(v).map{case (e, vi) => e.wbhit(io.w.bits.data, Mux(io.w.bits.data.s2xlate =/= noS2xlate, io.csr.vsatp.asid, io.csr.satp.asid), io.csr.hgatp.vmid, s2xlate = io.w.bits.data.s2xlate) && vi })
  XSError(io.w.valid && Cat(w_hit_vec).orR, s"${parentName} refill, duplicate with existing entries")

  val refill_vpn_reg = RegEnable(io.w.bits.data.s1.entry.tag, io.w.valid)
  val refill_wayIdx_reg = RegEnable(io.w.bits.wayIdx, io.w.valid)
  when (RegNext(io.w.valid)) {
    io.access.map { access =>
      access.sets := get_set_idx(refill_vpn_reg, nSets)
      access.touch_ways.valid := true.B
      access.touch_ways.bits := refill_wayIdx_reg
    }
  }

  val sfence = io.sfence
  val sfence_valid = sfence.valid && !sfence.bits.hg && !sfence.bits.hv && (if (HasMptCheck) !(sfence.bits.mfence.get) else true.B)
  val sfence_vpn = sfence.bits.addr(VAddrBits - 1, offLen)
  val sfenceHit = entries.map(_.hit(sfence_vpn, sfence.bits.id, vmid = io.csr.hgatp.vmid, hasS2xlate = io.csr.priv.fenceVirt))
  val sfenceHit_noasid = entries.map(_.hit(sfence_vpn, sfence.bits.id, ignoreAsid = true, vmid = io.csr.hgatp.vmid, hasS2xlate = io.csr.priv.fenceVirt))
  // Sfence will flush all sectors of an entry when hit
  when (sfence_valid) {
    when (sfence.bits.rs1 || io.csr.priv.fenceVirt || (if (HasBitmapCheck) (io.csr.mbmc.BME === 1.U && io.csr.mbmc.CMODE === 0.U) else false.B)) { // virtual address *.rs1 <- (rs1===0.U)
      // Note: when virt=1, always flush all addr. See hfence.vvma comment.
      when (sfence.bits.rs2) { // asid, but i do not want to support asid, *.rs2 <- (rs2===0.U)
        // all addr and all asid
        v.zipWithIndex.map{ case(a, i) => a := a && !((io.csr.priv.fenceVirt === false.B && entries(i).s2xlate === noS2xlate) ||
          (io.csr.priv.fenceVirt && entries(i).s2xlate =/= noS2xlate && entries(i).vmid === io.csr.hgatp.vmid))}
      }.otherwise {
        // all addr but specific asid
        v.zipWithIndex.map{ case (a, i) => a := a && !(!g(i) && ((!io.csr.priv.fenceVirt && entries(i).s2xlate === noS2xlate && entries(i).asid === sfence.bits.id) ||
          (io.csr.priv.fenceVirt && entries(i).s2xlate =/= noS2xlate && entries(i).asid === sfence.bits.id && entries(i).vmid === io.csr.hgatp.vmid)))}
      }
    }.otherwise {
      when (sfence.bits.rs2) {
        // specific addr but all asid
        v.zipWithIndex.map{ case (a, i) => a := a & !sfenceHit_noasid(i) }
      }.otherwise {
        // specific addr and specific asid
        v.zipWithIndex.map{ case (a, i) => a := a & !(sfenceHit(i) && !g(i)) }
      }
    }
  }

  val hfencev_valid = sfence.valid && sfence.bits.hv
  val hfenceg_valid = sfence.valid && sfence.bits.hg
  val hfencev = io.sfence
  // val hfencev_vpn = sfence_vpn
  // val hfencevHit = entries.map(_.hit(hfencev_vpn, hfencev.bits.id, vmid = io.csr.hgatp.vmid, hasS2xlate = true.B))
  // val hfencevHit_noasid = entries.map(_.hit(hfencev_vpn, 0.U, ignoreAsid = true, vmid = io.csr.hgatp.vmid, hasS2xlate = true.B))
  when (hfencev_valid) {
    when (hfencev.bits.rs2) {
      // all addr and all asid
      v.zipWithIndex.map { case (a, i) => a := a && !(entries(i).s2xlate =/= noS2xlate && entries(i).vmid === io.csr.hgatp.vmid)}
    }.otherwise {
      // all addr but specific asid
      v.zipWithIndex.map { case (a, i) => a := a && !(!g(i) && (entries(i).s2xlate =/= noS2xlate && entries(i).asid === sfence.bits.id && entries(i).vmid === io.csr.hgatp.vmid))
      }
    }
    /***
     * Current design cannot handle addr match in L1TLB properly, when two-stage address translation is enabled.
     * So we disable the address match function. Now hfence.vvma won't consider addr and will flush all entries.
     *
     * Currently, both VS-stage and G-stage are merged into a single L1TLB entry, with address matching controlled by the smaller page size.
     * Consider the following scenario:
     *                 VS-stage Page    G-stage Page
     *                  Large Page       Small Page
     *                   +--------+
     *                   |        |
     *                 +=|========|=====+========+=+
     *  L1TLB Entry->  | |########|     |########| |
     *                 +=|========|=====+========+=+
     *                   |        |
     *  sfence addr ---> |        |
     *  try to flush     |        |
     *                   +--------+
     *
     * In this case, the VS-stage is a large page, while the G-stage is a small page. L1TLB stores them as a small page.
     * When hfence.vvma comes with an address in that VS large page but outside the small page, it should flush the VS page.
     * However, since L1TLB always treats this entry as a small page, it cannot match this address, thus cannot flush this entry.
     *
    ***/
    // when (hfencev.bits.rs1) {
    //   // all addr
    // }.otherwise {
    //   when (hfencev.bits.rs2) {
    //     // specific addr but all asid
    //     v.zipWithIndex.map{ case (a, i) => a := a && !hfencevHit_noasid(i) }
    //   }.otherwise {
    //     // specific addr and specific asid
    //     v.zipWithIndex.map{ case (a, i) => a := a && !(hfencevHit(i) && !g(i)) }
    //   }
    // }
  }


  val hfenceg = io.sfence
  val hfenceg_gvpn = (sfence.bits.addr << 2)(VAddrBits - 1, offLen)
  when (hfenceg_valid) {
    when(hfenceg.bits.rs2) {
      v.zipWithIndex.map { case (a, i) => a := a && !(entries(i).s2xlate =/= noS2xlate) }
    }.otherwise {
      v.zipWithIndex.map { case (a, i) => a := a && !(entries(i).s2xlate =/= noS2xlate && entries(i).vmid === sfence.bits.id) }
    }
  }
  if (HasMptCheck) {
    when(sfence.valid && sfence.bits.mfence.get) {
      v.zipWithIndex.map { case (a, i) => a := false.B } // mfence reset all
    }
    val modechange = DataChanged(io.csr.satp.mode).asBool || DataChanged(io.csr.vsatp.mode).asBool ||
      DataChanged(io.csr.hgatp.mode).asBool
    when(modechange) {
      v.zipWithIndex.map { case (a, i) => a := false.B } // mptonly to ptw reset all
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

// TLBFakeFA: Software TLB implementation using DPI-C PteHelper for debug/simulation
// This module bypasses hardware page table walk and uses C++ reference model instead
class TLBFakeFA(
  ports: Int,
  nDups: Int,
  nSets: Int,
  nWays: Int,
  useDmode: Boolean = false
)(implicit p: Parameters) extends TlbModule with HasCSRConst {

  val io = IO(new TlbStorageIO(nSets, nWays, ports, nDups))
  io.r.req.map(_.ready := true.B)

  val mode = if (useDmode) io.csr.priv.dmode else io.csr.priv.imode
  val sv39Enable = io.csr.satp.mode === Sv39
  val sv48Enable = io.csr.satp.mode === Sv48
  val sv39vsEnable = io.csr.vsatp.mode === Sv39
  val sv48vsEnable = io.csr.vsatp.mode === Sv48
  val sv39x4Enable = io.csr.hgatp.mode === Sv39x4
  val sv48x4Enable = io.csr.hgatp.mode === Sv48x4

  val vmEnable = if (EnableTlbDebug) (sv39Enable || sv48Enable)
    else ((sv39Enable || sv48Enable) && (mode < ModeM))
  val s2xlateEnable = if (EnableTlbDebug) {
    sv39vsEnable || sv48vsEnable || sv39x4Enable || sv48x4Enable
  } else {
    (sv39vsEnable || sv48vsEnable || sv39x4Enable || sv48x4Enable) && (mode < ModeM)
  }

  for (i <- 0 until ports) {
    val req = io.r.req(i)
    val resp = io.r.resp(i)

    val helper = Module(new PteHelper())
    helper.clock := clock
    helper.connectCsr(io.csr)

    val helperEnable = req.fire &&
      Mux(req.bits.s2xlate === noS2xlate, vmEnable, s2xlateEnable) &&
      !reset.asBool
    helper.connectReq(req.bits.vpn, req.bits.s2xlate, helperEnable)

    val helperResult = PteHelperResult.fromHelper(helper, helperEnable)
    val pte = helperResult.pte
    val s1Pte = helperResult.s1.pte
    val s2Pte = helperResult.s2.pte

    val vpnReg = RegEnable(req.bits.vpn, req.valid)
    val s2xlateReg = RegEnable(req.bits.s2xlate, req.valid)
    val isAllStage = s2xlateReg === allStage
    val finalPPN = helperResult.finalPPN(vpnReg, s2xlateReg)
    val s1RespPte = helperResult.s1RespPte(s2xlateReg)
    val s2RespPte = helperResult.s2RespPte(s2xlateReg)

    resp.valid := RegNext(req.valid)
    resp.bits.hit := RegNext(helperEnable, false.B)

    for (d <- 0 until nDups) {
      resp.bits.perm(d)(s1RespPte, helperResult.hasPf, false.B, Mux(isAllStage, s1Pte.perm.v, helperResult.hasNoPf))
      resp.bits.pbmt(d) := s1RespPte.pbmt
      resp.bits.ppn(d) := finalPPN
      resp.bits.g_perm(d)(s2RespPte, helperResult.hasGpf, false.B, Mux(isAllStage, s2Pte.perm.v, helperResult.hasNoPf))
      resp.bits.g_pbmt(d) := s2RespPte.pbmt
      resp.bits.s2xlate(d) := s2xlateReg
    }
  }

  io.access := DontCare
}

object TlbStorage {
  def apply
  (
    parentName: String,
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
    ports = ports,
    nDups = nDups,
    nSets = q.NSets,
    nWays = q.NWays,
    normalPage = true,
    superPage = true,
    useDmode = q.useDmode,
    SoftTLB = coreParams.softTLB
  )

  page.r.req <> io.r.req
  io.r.resp := page.r.resp

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
}
