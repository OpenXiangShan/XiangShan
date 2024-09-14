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
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import utility._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import xiangshan.backend.fu.{PMPReqBundle, PMPRespBundle}

/** Page Table Walk is divided into two parts
  * One,   PTW: page walk for pde, except for leaf entries, one by one
  * Two, LLPTW: page walk for pte, only the leaf entries(4KB), in parallel
  */


/** PTW : page table walker
  * a finite state machine
  * only take 1GB and 2MB page walks
  * or in other words, except the last level(leaf)
  **/
class PTWIO()(implicit p: Parameters) extends MMUIOBaseBundle with HasPtwConst {
  val req = Flipped(DecoupledIO(new Bundle {
    val req_info = new L2TlbInnerBundle()
    val l3Hit = if (EnableSv48) Some(new Bool()) else None
    val l2Hit = Bool()
    val ppn = UInt(ptePPNLen.W)
    val stage1Hit = Bool()
    val stage1 = new PtwMergeResp
  }))
  val resp = DecoupledIO(new Bundle {
    val source = UInt(bSourceWidth.W)
    val s2xlate = UInt(2.W)
    val resp = new PtwMergeResp
    val h_resp = new HptwResp
  })

  val llptw = DecoupledIO(new LLPTWInBundle())
  // NOTE: llptw change from "connect to llptw" to "connect to page cache"
  // to avoid corner case that caused duplicate entries

  val hptw = new Bundle {
    val req = DecoupledIO(new Bundle {
      val source = UInt(bSourceWidth.W)
      val id = UInt(log2Up(l2tlbParams.llptwsize).W)
      val gvpn = UInt(ptePPNLen.W)
    })
    val resp = Flipped(Valid(new Bundle {
      val h_resp = Output(new HptwResp)
    }))
  }
  val mem = new Bundle {
    val req = DecoupledIO(new L2TlbMemReqBundle())
    val resp = Flipped(ValidIO(UInt(XLEN.W)))
    val mask = Input(Bool())
  }
  val pmp = new Bundle {
    val req = ValidIO(new PMPReqBundle())
    val resp = Flipped(new PMPRespBundle())
  }

  val refill = Output(new Bundle {
    val req_info = new L2TlbInnerBundle()
    val level = UInt(log2Up(Level + 1).W)
  })
}

class PTW()(implicit p: Parameters) extends XSModule with HasPtwConst with HasPerfEvents {
  val io = IO(new PTWIO)
  val sfence = io.sfence
  val mem = io.mem
  val req_s2xlate = Reg(UInt(2.W))
  val enableS2xlate = req_s2xlate =/= noS2xlate
  val onlyS1xlate = req_s2xlate === onlyStage1
  val onlyS2xlate = req_s2xlate === onlyStage2
  val satp = Wire(new TlbSatpBundle())
  when (io.req.fire) {
    satp := Mux(io.req.bits.req_info.s2xlate =/= noS2xlate, io.csr.vsatp, io.csr.satp)
  } .otherwise {
    satp := Mux(enableS2xlate, io.csr.vsatp, io.csr.satp)
  }
  val s1Pbmte = Mux(req_s2xlate =/= noS2xlate, io.csr.hPBMTE, io.csr.mPBMTE)

  val mode = satp.mode
  val hgatp = io.csr.hgatp
  val flush = io.sfence.valid || io.csr.satp.changed || io.csr.vsatp.changed || io.csr.hgatp.changed
  val s2xlate = enableS2xlate && !onlyS1xlate
  val level = RegInit(3.U(log2Up(Level + 1).W))
  val af_level = RegInit(3.U(log2Up(Level + 1).W)) // access fault return this level
  val gpf_level = RegInit(3.U(log2Up(Level + 1).W))
  val ppn = Reg(UInt(ptePPNLen.W))
  val vpn = Reg(UInt(vpnLen.W)) // vpn or gvpn(onlyS2xlate)
  val levelNext = level - 1.U
  val l3Hit = Reg(Bool())
  val l2Hit = Reg(Bool())
  val pte = mem.resp.bits.asTypeOf(new PteBundle())

  // s/w register
  val s_pmp_check = RegInit(true.B)
  val s_mem_req = RegInit(true.B)
  val s_llptw_req = RegInit(true.B)
  val w_mem_resp = RegInit(true.B)
  val s_hptw_req = RegInit(true.B)
  val w_hptw_resp = RegInit(true.B)
  val s_last_hptw_req = RegInit(true.B)
  val w_last_hptw_resp = RegInit(true.B)
  // for updating "level"
  val mem_addr_update = RegInit(false.B)

  val idle = RegInit(true.B)
  val finish = WireInit(false.B)
  val sent_to_pmp = idle === false.B && (s_pmp_check === false.B || mem_addr_update) && !finish

  val pageFault = pte.isPf(level, s1Pbmte)
  val accessFault = RegEnable(io.pmp.resp.ld || io.pmp.resp.mmio, false.B, sent_to_pmp)

  val hptw_pageFault = RegInit(false.B)
  val hptw_accessFault = RegInit(false.B)
  val last_s2xlate = RegInit(false.B)
  val stage1Hit = RegEnable(io.req.bits.stage1Hit, io.req.fire)
  val stage1 = RegEnable(io.req.bits.stage1, io.req.fire)
  val hptw_resp_stage2 = Reg(Bool())

  val ppn_af = Mux(enableS2xlate, Mux(onlyS1xlate, pte.isAf(), false.B), pte.isAf()) // In two-stage address translation, stage 1 ppn is a vpn for host, so don't need to check ppn_high
  val find_pte = pte.isLeaf() || ppn_af || pageFault
  val to_find_pte = level === 1.U && find_pte === false.B
  val source = RegEnable(io.req.bits.req_info.source, io.req.fire)

  val l3addr = Wire(UInt(PAddrBits.W))
  val l2addr = Wire(UInt(PAddrBits.W))
  val l1addr = Wire(UInt(PAddrBits.W))
  val mem_addr = Wire(UInt(PAddrBits.W))

  l3addr := MakeAddr(satp.ppn, getVpnn(vpn, 3))
  if (EnableSv48) {
    when (mode === Sv48) {
      l2addr := MakeAddr(Mux(l3Hit, ppn, pte.getPPN()), getVpnn(vpn, 2))
    } .otherwise {
      l2addr := MakeAddr(satp.ppn, getVpnn(vpn, 2))
    }
  } else {
    l2addr := MakeAddr(satp.ppn, getVpnn(vpn, 2))
  }
  l1addr := MakeAddr(Mux(l2Hit, ppn, pte.getPPN()), getVpnn(vpn, 1))
  mem_addr := Mux(af_level === 3.U, l3addr, Mux(af_level === 2.U, l2addr, l1addr))

  val hptw_resp = Reg(new HptwResp)
  val gpaddr = MuxCase(mem_addr, Seq(
    stage1Hit -> Cat(stage1.genPPN(), 0.U(offLen.W)),
    onlyS2xlate -> Cat(vpn, 0.U(offLen.W)),
    !s_last_hptw_req -> Cat(MuxLookup(level, pte.getPPN())(Seq(
      3.U -> Cat(pte.getPPN()(ptePPNLen - 1, vpnnLen * 3), vpn(vpnnLen * 3 - 1, 0)),
      2.U -> Cat(pte.getPPN()(ptePPNLen - 1, vpnnLen * 2), vpn(vpnnLen * 2 - 1, 0)),
      1.U -> Cat(pte.getPPN()(ptePPNLen - 1, vpnnLen), vpn(vpnnLen - 1, 0)
    ))),
    0.U(offLen.W))
  ))
  val gvpn_gpf = Mux(s2xlate && io.csr.hgatp.mode === Sv39x4, gpaddr(gpaddr.getWidth - 1, GPAddrBitsSv39x4) =/= 0.U, Mux(s2xlate && io.csr.hgatp.mode === Sv48x4, gpaddr(gpaddr.getWidth - 1, GPAddrBitsSv48x4) =/= 0.U, false.B))
  val guestFault = hptw_pageFault || hptw_accessFault || gvpn_gpf
  val hpaddr = Cat(hptw_resp.genPPNS2(get_pn(gpaddr)), get_off(gpaddr))
  val fake_h_resp = 0.U.asTypeOf(new HptwResp)
  fake_h_resp.entry.tag := get_pn(gpaddr)
  fake_h_resp.entry.vmid.map(_ := io.csr.hgatp.vmid)
  fake_h_resp.gpf := true.B

  val pte_valid = RegInit(false.B)  // avoid l1tlb pf from stage1 when gpf happens in the first s2xlate in PTW
  val fake_pte = 0.U.asTypeOf(new PteBundle())
  fake_pte.perm.v := false.B // tell L1TLB this is fake pte
  fake_pte.perm.r := true.B
  fake_pte.perm.w := true.B
  fake_pte.perm.x := true.B
  fake_pte.perm.a := true.B
  fake_pte.perm.d := true.B
  fake_pte.ppn := ppn(ppnLen - 1, 0)
  fake_pte.ppn_high := ppn(ptePPNLen - 1, ppnLen)

  io.req.ready := idle
  val ptw_resp = Wire(new PtwMergeResp)
  ptw_resp.apply(Mux(pte_valid, pageFault && !accessFault, false.B), accessFault || (ppn_af && !(pte_valid && (pageFault || guestFault))), Mux(accessFault, af_level, Mux(guestFault, gpf_level, level)), Mux(pte_valid, pte, fake_pte), vpn, satp.asid, hgatp.vmid, vpn(sectortlbwidth - 1, 0), not_super = false, not_merge = false)

  val normal_resp = idle === false.B && mem_addr_update && !last_s2xlate && (guestFault || (w_mem_resp && find_pte) || (s_pmp_check && accessFault) || onlyS2xlate )
  val stageHit_resp = idle === false.B && hptw_resp_stage2
  io.resp.valid := Mux(stage1Hit, stageHit_resp, normal_resp)
  io.resp.bits.source := source
  io.resp.bits.resp := Mux(stage1Hit || (l3Hit || l2Hit) && guestFault && !pte_valid, stage1, ptw_resp)
  io.resp.bits.h_resp := Mux(gvpn_gpf, fake_h_resp, hptw_resp)
  io.resp.bits.s2xlate := req_s2xlate

  io.llptw.valid := s_llptw_req === false.B && to_find_pte && !accessFault && !guestFault
  io.llptw.bits.req_info.source := source
  io.llptw.bits.req_info.vpn := vpn
  io.llptw.bits.req_info.s2xlate := req_s2xlate
  io.llptw.bits.ppn := DontCare

  io.pmp.req.valid := DontCare // samecycle, do not use valid
  io.pmp.req.bits.addr := Mux(s2xlate, hpaddr, mem_addr)
  io.pmp.req.bits.size := 3.U // TODO: fix it
  io.pmp.req.bits.cmd := TlbCmd.read

  mem.req.valid := s_mem_req === false.B && !mem.mask && !accessFault && s_pmp_check
  mem.req.bits.addr := Mux(s2xlate, hpaddr, mem_addr)
  mem.req.bits.id := FsmReqID.U(bMemID.W)
  mem.req.bits.hptw_bypassed := false.B

  io.refill.req_info.s2xlate := req_s2xlate
  io.refill.req_info.vpn := vpn
  io.refill.level := level
  io.refill.req_info.source := source

  io.hptw.req.valid := !s_hptw_req || !s_last_hptw_req
  io.hptw.req.bits.id := FsmReqID.U(bMemID.W)
  io.hptw.req.bits.gvpn := get_pn(gpaddr)
  io.hptw.req.bits.source := source

  when (io.req.fire && io.req.bits.stage1Hit){
    idle := false.B
    req_s2xlate := io.req.bits.req_info.s2xlate
    s_last_hptw_req := false.B
    hptw_resp_stage2 := false.B
    last_s2xlate := false.B
    hptw_pageFault := false.B
    hptw_accessFault := false.B
  }

  when (io.resp.fire && stage1Hit){
    idle := true.B
  }

  when (io.req.fire && !io.req.bits.stage1Hit){
    val req = io.req.bits
    if (EnableSv48) {
      when (mode === Sv48) {
        level := Mux(req.l2Hit, 1.U, Mux(req.l3Hit.get, 2.U, 3.U))
        af_level := Mux(req.l2Hit, 1.U, Mux(req.l3Hit.get, 2.U, 3.U))
        gpf_level := Mux(req.l2Hit, 2.U, Mux(req.l3Hit.get, 3.U, 0.U))
        ppn := Mux(req.l2Hit || req.l3Hit.get, io.req.bits.ppn, satp.ppn)
        l3Hit := req.l3Hit.get
      } .otherwise {
        level := Mux(req.l2Hit, 1.U, 2.U)
        af_level := Mux(req.l2Hit, 1.U, 2.U)
        gpf_level := 0.U
        ppn := Mux(req.l2Hit, io.req.bits.ppn, satp.ppn)
        l3Hit := false.B
      }
    } else {
      level := Mux(req.l2Hit, 1.U, 2.U)
      af_level := Mux(req.l2Hit, 1.U, 2.U)
      gpf_level := 0.U
      ppn := Mux(req.l2Hit, io.req.bits.ppn, satp.ppn)
      l3Hit := false.B
    }
    vpn := io.req.bits.req_info.vpn
    l2Hit := req.l2Hit
    accessFault := false.B
    idle := false.B
    hptw_pageFault := false.B
    hptw_accessFault := false.B
    pte_valid := false.B
    req_s2xlate := io.req.bits.req_info.s2xlate
    when(io.req.bits.req_info.s2xlate === onlyStage2){
      val onlys2_gpaddr = Cat(io.req.bits.req_info.vpn, 0.U(offLen.W)) // is 50 bits, don't need to check high bits when sv48x4 is enabled
      val check_gpa_high_fail = Mux(io.req.bits.req_info.s2xlate === onlyStage2 && io.csr.hgatp.mode === Sv39x4, onlys2_gpaddr(onlys2_gpaddr.getWidth - 1, GPAddrBitsSv39x4) =/= 0.U, false.B)
      last_s2xlate := false.B
      when(check_gpa_high_fail){
        mem_addr_update := true.B
      }.otherwise{
        s_last_hptw_req := false.B
      }
    }.elsewhen(io.req.bits.req_info.s2xlate === allStage){
      last_s2xlate := true.B
      s_hptw_req := false.B
    }.otherwise {
      last_s2xlate := false.B
      s_pmp_check := false.B
    }
  }

  when(io.hptw.req.fire && s_hptw_req === false.B){
    s_hptw_req := true.B
    w_hptw_resp := false.B
  }

  when(io.hptw.resp.fire && w_hptw_resp === false.B) {
    w_hptw_resp := true.B
    val g_perm_fail = !io.hptw.resp.bits.h_resp.gaf && (!io.hptw.resp.bits.h_resp.entry.perm.get.r && !(io.csr.priv.mxr && io.hptw.resp.bits.h_resp.entry.perm.get.x))
    hptw_pageFault := io.hptw.resp.bits.h_resp.gpf || g_perm_fail
    hptw_accessFault := io.hptw.resp.bits.h_resp.gaf
    hptw_resp := io.hptw.resp.bits.h_resp
    hptw_resp.gpf := io.hptw.resp.bits.h_resp.gpf || g_perm_fail
    when(!(g_perm_fail || io.hptw.resp.bits.h_resp.gpf || io.hptw.resp.bits.h_resp.gaf)) {
      s_pmp_check := false.B
    }.otherwise {
      mem_addr_update := true.B
      last_s2xlate := false.B
    }
  }
  
  when(io.hptw.req.fire && s_last_hptw_req === false.B) {
    w_last_hptw_resp := false.B
    s_last_hptw_req := true.B
  }

  when (io.hptw.resp.fire && w_last_hptw_resp === false.B && stage1Hit){
    w_last_hptw_resp := true.B
    hptw_resp_stage2 := true.B
    hptw_resp := io.hptw.resp.bits.h_resp
  }

  when(io.hptw.resp.fire && w_last_hptw_resp === false.B && !stage1Hit){
    hptw_pageFault := io.hptw.resp.bits.h_resp.gpf
    hptw_accessFault := io.hptw.resp.bits.h_resp.gaf
    hptw_resp := io.hptw.resp.bits.h_resp
    w_last_hptw_resp := true.B
    mem_addr_update := true.B
    last_s2xlate := false.B
  }

  when(sent_to_pmp && mem_addr_update === false.B){
    s_mem_req := false.B
    s_pmp_check := true.B
  }

  when(accessFault && idle === false.B){
    s_pmp_check := true.B
    s_mem_req := true.B
    w_mem_resp := true.B
    s_llptw_req := true.B
    s_hptw_req := true.B
    w_hptw_resp := true.B
    s_last_hptw_req := true.B
    w_last_hptw_resp := true.B
    mem_addr_update := true.B
    last_s2xlate := false.B
  }

  when(guestFault && idle === false.B){
    s_pmp_check := true.B
    s_mem_req := true.B
    w_mem_resp := true.B
    s_llptw_req := true.B
    s_hptw_req := true.B
    w_hptw_resp := true.B
    s_last_hptw_req := true.B
    w_last_hptw_resp := true.B
    mem_addr_update := true.B
    last_s2xlate := false.B
  }

  when (mem.req.fire){
    s_mem_req := true.B
    w_mem_resp := false.B
  }

  when(mem.resp.fire && w_mem_resp === false.B){
    w_mem_resp := true.B
    af_level := af_level - 1.U
    s_llptw_req := false.B
    mem_addr_update := true.B
    gpf_level := Mux(mode === Sv39 && !pte_valid && !(l3Hit || l2Hit), gpf_level - 2.U, gpf_level - 1.U)
    pte_valid := true.B
  }

  when(mem_addr_update){
    when(level >= 2.U && !onlyS2xlate && !(guestFault || find_pte || accessFault)) {
      level := levelNext
      when(s2xlate){
        s_hptw_req := false.B
      }.otherwise{
        s_mem_req := false.B
      }
      s_llptw_req := true.B
      mem_addr_update := false.B
    }.elsewhen(io.llptw.valid){
      when(io.llptw.fire) {
        idle := true.B
        s_llptw_req := true.B
        mem_addr_update := false.B
        last_s2xlate := false.B
      }
      finish := true.B
    }.elsewhen(s2xlate && last_s2xlate === true.B) {
      when(accessFault || pageFault || ppn_af){
        last_s2xlate := false.B
      }.otherwise{
        s_last_hptw_req := false.B
        mem_addr_update := false.B
      }
    }.elsewhen(io.resp.valid){
      when(io.resp.fire) {
        idle := true.B
        s_llptw_req := true.B
        mem_addr_update := false.B
        accessFault := false.B
      }
      finish := true.B
    }
  }


  when (flush) {
    idle := true.B
    s_pmp_check := true.B
    s_mem_req := true.B
    s_llptw_req := true.B
    w_mem_resp := true.B
    accessFault := false.B
    mem_addr_update := false.B
    s_hptw_req := true.B
    w_hptw_resp := true.B
    s_last_hptw_req := true.B
    w_last_hptw_resp := true.B
  }


  XSDebug(p"[ptw] level:${level} notFound:${pageFault}\n")

  // perf
  XSPerfAccumulate("fsm_count", io.req.fire)
  for (i <- 0 until PtwWidth) {
    XSPerfAccumulate(s"fsm_count_source${i}", io.req.fire && io.req.bits.req_info.source === i.U)
  }
  XSPerfAccumulate("fsm_busy", !idle)
  XSPerfAccumulate("fsm_idle", idle)
  XSPerfAccumulate("resp_blocked", io.resp.valid && !io.resp.ready)
  XSPerfAccumulate("ptw_ppn_af", io.resp.fire && ppn_af)
  XSPerfAccumulate("mem_count", mem.req.fire)
  XSPerfAccumulate("mem_cycle", BoolStopWatch(mem.req.fire, mem.resp.fire, true))
  XSPerfAccumulate("mem_blocked", mem.req.valid && !mem.req.ready)

  TimeOutAssert(!idle, timeOutThreshold, "page table walker time out")

  val perfEvents = Seq(
    ("fsm_count         ", io.req.fire                                     ),
    ("fsm_busy          ", !idle                                             ),
    ("fsm_idle          ", idle                                              ),
    ("resp_blocked      ", io.resp.valid && !io.resp.ready                   ),
    ("mem_count         ", mem.req.fire                                    ),
    ("mem_cycle         ", BoolStopWatch(mem.req.fire, mem.resp.fire, true)),
    ("mem_blocked       ", mem.req.valid && !mem.req.ready                   ),
  )
  generatePerfEvent()
}

/*========================= LLPTW ==============================*/

/** LLPTW : Last Level Page Table Walker
  * the page walker that only takes 4KB(last level) page walk.
  **/

class LLPTWInBundle(implicit p: Parameters) extends XSBundle with HasPtwConst {
  val req_info = Output(new L2TlbInnerBundle())
  val ppn = Output(UInt(ptePPNLen.W))
}

class LLPTWIO(implicit p: Parameters) extends MMUIOBaseBundle with HasPtwConst {
  val in = Flipped(DecoupledIO(new LLPTWInBundle()))
  val out = DecoupledIO(new Bundle {
    val req_info = Output(new L2TlbInnerBundle())
    val id = Output(UInt(bMemID.W))
    val h_resp = Output(new HptwResp)
    val first_s2xlate_fault = Output(Bool()) // Whether the first stage 2 translation occurs pf/af
    val af = Output(Bool())
  })
  val mem = new Bundle {
    val req = DecoupledIO(new L2TlbMemReqBundle())
    val resp = Flipped(Valid(new Bundle {
      val id = Output(UInt(log2Up(l2tlbParams.llptwsize).W))
      val value = Output(UInt(blockBits.W))
    }))
    val enq_ptr = Output(UInt(log2Ceil(l2tlbParams.llptwsize).W))
    val buffer_it = Output(Vec(l2tlbParams.llptwsize, Bool()))
    val refill = Output(new L2TlbInnerBundle())
    val req_mask = Input(Vec(l2tlbParams.llptwsize, Bool()))
    val flush_latch = Input(Vec(l2tlbParams.llptwsize, Bool()))
  }
  val cache = DecoupledIO(new L2TlbInnerBundle())
  val pmp = new Bundle {
    val req = Valid(new PMPReqBundle())
    val resp = Flipped(new PMPRespBundle())
  }
  val hptw = new Bundle {
    val req = DecoupledIO(new Bundle{
      val source = UInt(bSourceWidth.W)
      val id = UInt(log2Up(l2tlbParams.llptwsize).W)
      val gvpn = UInt(ptePPNLen.W)
    })
    val resp = Flipped(Valid(new Bundle {
      val id = Output(UInt(log2Up(l2tlbParams.llptwsize).W))
      val h_resp = Output(new HptwResp)
    }))
  }
}

class LLPTWEntry(implicit p: Parameters) extends XSBundle with HasPtwConst {
  val req_info = new L2TlbInnerBundle()
  val ppn = UInt(ptePPNLen.W)
  val wait_id = UInt(log2Up(l2tlbParams.llptwsize).W)
  val af = Bool()
  val hptw_resp = new HptwResp()
  val first_s2xlate_fault = Output(Bool())
}


class LLPTW(implicit p: Parameters) extends XSModule with HasPtwConst with HasPerfEvents {
  val io = IO(new LLPTWIO())
  val enableS2xlate = io.in.bits.req_info.s2xlate =/= noS2xlate
  val satp = Mux(enableS2xlate, io.csr.vsatp, io.csr.satp)
  val s1Pbmte = Mux(enableS2xlate, io.csr.hPBMTE, io.csr.mPBMTE)

  val flush = io.sfence.valid || io.csr.satp.changed || io.csr.vsatp.changed || io.csr.hgatp.changed
  val entries = RegInit(VecInit(Seq.fill(l2tlbParams.llptwsize)(0.U.asTypeOf(new LLPTWEntry()))))
  val state_idle :: state_hptw_req :: state_hptw_resp :: state_addr_check :: state_mem_req :: state_mem_waiting :: state_mem_out :: state_last_hptw_req :: state_last_hptw_resp :: state_cache :: Nil = Enum(10)
  val state = RegInit(VecInit(Seq.fill(l2tlbParams.llptwsize)(state_idle)))

  val is_emptys = state.map(_ === state_idle)
  val is_mems = state.map(_ === state_mem_req)
  val is_waiting = state.map(_ === state_mem_waiting)
  val is_having = state.map(_ === state_mem_out)
  val is_cache = state.map(_ === state_cache)
  val is_hptw_req = state.map(_ === state_hptw_req)
  val is_last_hptw_req = state.map(_ === state_last_hptw_req)
  val is_hptw_resp = state.map(_ === state_hptw_resp)
  val is_last_hptw_resp = state.map(_ === state_last_hptw_resp)

  val full = !ParallelOR(is_emptys).asBool
  val enq_ptr = ParallelPriorityEncoder(is_emptys)

  val mem_ptr = ParallelPriorityEncoder(is_having) // TODO: optimize timing, bad: entries -> ptr -> entry
  val mem_arb = Module(new RRArbiterInit(new LLPTWEntry(), l2tlbParams.llptwsize))
  for (i <- 0 until l2tlbParams.llptwsize) {
    mem_arb.io.in(i).bits := entries(i)
    mem_arb.io.in(i).valid := is_mems(i) && !io.mem.req_mask(i)
  }

  // process hptw requests in serial
  val hyper_arb1 = Module(new RRArbiterInit(new LLPTWEntry(), l2tlbParams.llptwsize))
  for (i <- 0 until l2tlbParams.llptwsize) {
    hyper_arb1.io.in(i).bits := entries(i)
    hyper_arb1.io.in(i).valid := is_hptw_req(i) && !(Cat(is_hptw_resp).orR) && !(Cat(is_last_hptw_resp).orR)
  }
  val hyper_arb2 = Module(new RRArbiterInit(new LLPTWEntry(), l2tlbParams.llptwsize))
  for(i <- 0 until l2tlbParams.llptwsize) {
    hyper_arb2.io.in(i).bits := entries(i)
    hyper_arb2.io.in(i).valid := is_last_hptw_req(i) && !(Cat(is_hptw_resp).orR) && !(Cat(is_last_hptw_resp).orR)
  }

  val cache_ptr = ParallelMux(is_cache, (0 until l2tlbParams.llptwsize).map(_.U(log2Up(l2tlbParams.llptwsize).W)))

  // duplicate req
  // to_wait: wait for the last to access mem, set to mem_resp
  // to_cache: the last is back just right now, set to mem_cache
  val dup_vec = state.indices.map(i =>
    dup(io.in.bits.req_info.vpn, entries(i).req_info.vpn) && io.in.bits.req_info.s2xlate === entries(i).req_info.s2xlate
  )
  val dup_req_fire = mem_arb.io.out.fire && dup(io.in.bits.req_info.vpn, mem_arb.io.out.bits.req_info.vpn) && io.in.bits.req_info.s2xlate === mem_arb.io.out.bits.req_info.s2xlate // dup with the req fire entry
  val dup_vec_wait = dup_vec.zip(is_waiting).map{case (d, w) => d && w} // dup with "mem_waiting" entries, sending mem req already
  val dup_vec_having = dup_vec.zipWithIndex.map{case (d, i) => d && is_having(i)} // dup with the "mem_out" entry recv the data just now
  val dup_vec_last_hptw = dup_vec.zipWithIndex.map{case (d, i) => d && (is_last_hptw_req(i) || is_last_hptw_resp(i))}
  val wait_id = Mux(dup_req_fire, mem_arb.io.chosen, ParallelMux(dup_vec_wait zip entries.map(_.wait_id)))
  val dup_wait_resp = io.mem.resp.fire && VecInit(dup_vec_wait)(io.mem.resp.bits.id) && !io.mem.flush_latch(io.mem.resp.bits.id) // dup with the entry that data coming next cycle
  val to_wait = Cat(dup_vec_wait).orR || dup_req_fire
  val to_mem_out = dup_wait_resp && ((entries(io.mem.resp.bits.id).req_info.s2xlate === noS2xlate) || (entries(io.mem.resp.bits.id).req_info.s2xlate === onlyStage1))
  val to_cache = Cat(dup_vec_having).orR || Cat(dup_vec_last_hptw).orR
  val to_hptw_req = io.in.bits.req_info.s2xlate === allStage
  val to_last_hptw_req = dup_wait_resp && entries(io.mem.resp.bits.id).req_info.s2xlate === allStage
  val last_hptw_req_id = io.mem.resp.bits.id
  val req_paddr = MakeAddr(io.in.bits.ppn(ppnLen-1, 0), getVpnn(io.in.bits.req_info.vpn, 0))
  val req_hpaddr = MakeAddr(entries(last_hptw_req_id).hptw_resp.genPPNS2(get_pn(req_paddr)), getVpnn(io.in.bits.req_info.vpn, 0))
  val index =  Mux(entries(last_hptw_req_id).req_info.s2xlate === allStage, req_hpaddr, req_paddr)(log2Up(l2tlbParams.blockBytes)-1, log2Up(XLEN/8))
  val last_hptw_req_ppn = io.mem.resp.bits.value.asTypeOf(Vec(blockBits / XLEN, new PteBundle()))(index).getPPN()
  XSError(RegNext(dup_req_fire && Cat(dup_vec_wait).orR, init = false.B), "mem req but some entries already waiting, should not happed")

  XSError(io.in.fire && ((to_mem_out && to_cache) || (to_wait && to_cache)), "llptw enq, to cache conflict with to mem")
  val mem_resp_hit = RegInit(VecInit(Seq.fill(l2tlbParams.llptwsize)(false.B)))
  val enq_state_normal = MuxCase(state_addr_check, Seq(
    to_mem_out -> state_mem_out, // same to the blew, but the mem resp now
    to_last_hptw_req -> state_last_hptw_req,
    to_wait -> state_mem_waiting,
    to_cache -> state_cache,
    to_hptw_req -> state_hptw_req
  ))
  val enq_state = Mux(from_pre(io.in.bits.req_info.source) && enq_state_normal =/= state_addr_check, state_idle, enq_state_normal)
  when (io.in.fire) {
    // if prefetch req does not need mem access, just give it up.
    // so there will be at most 1 + FilterSize entries that needs re-access page cache
    // so 2 + FilterSize is enough to avoid dead-lock
    state(enq_ptr) := enq_state
    entries(enq_ptr).req_info := io.in.bits.req_info
    entries(enq_ptr).ppn := Mux(to_last_hptw_req, last_hptw_req_ppn, io.in.bits.ppn)
    entries(enq_ptr).wait_id := Mux(to_wait, wait_id, enq_ptr)
    entries(enq_ptr).af := false.B
    entries(enq_ptr).hptw_resp := Mux(to_last_hptw_req, entries(last_hptw_req_id).hptw_resp, Mux(to_wait, entries(wait_id).hptw_resp, entries(enq_ptr).hptw_resp))
    entries(enq_ptr).first_s2xlate_fault := false.B
    mem_resp_hit(enq_ptr) := to_mem_out || to_last_hptw_req
  }

  val enq_ptr_reg = RegNext(enq_ptr)
  val need_addr_check = GatedValidRegNext(enq_state === state_addr_check && io.in.fire && !flush)

  val hasHptwResp = ParallelOR(state.map(_ === state_hptw_resp)).asBool
  val hptw_resp_ptr_reg = RegNext(io.hptw.resp.bits.id)
  val hptw_need_addr_check = RegNext(hasHptwResp && io.hptw.resp.fire && !flush) && state(hptw_resp_ptr_reg) === state_addr_check

  val ptes = io.mem.resp.bits.value.asTypeOf(Vec(blockBits / XLEN, new PteBundle()))
  val gpaddr = MakeGPAddr(entries(hptw_resp_ptr_reg).ppn, getVpnn(entries(hptw_resp_ptr_reg).req_info.vpn, 0))
  val hptw_resp = entries(hptw_resp_ptr_reg).hptw_resp
  val hpaddr = Cat(hptw_resp.genPPNS2(get_pn(gpaddr)), get_off(gpaddr))
  val addr = RegEnable(MakeAddr(io.in.bits.ppn(ppnLen - 1, 0), getVpnn(io.in.bits.req_info.vpn, 0)), io.in.fire)
  io.pmp.req.valid := need_addr_check || hptw_need_addr_check
  io.pmp.req.bits.addr := Mux(hptw_need_addr_check, hpaddr, addr)
  io.pmp.req.bits.cmd := TlbCmd.read
  io.pmp.req.bits.size := 3.U // TODO: fix it
  val pmp_resp_valid = io.pmp.req.valid // same cycle
  when (pmp_resp_valid) {
    // NOTE: when pmp resp but state is not addr check, then the entry is dup with other entry, the state was changed before
    //       when dup with the req-ing entry, set to mem_waiting (above codes), and the ld must be false, so dontcare
    val ptr = Mux(hptw_need_addr_check, hptw_resp_ptr_reg, enq_ptr_reg);
    val accessFault = io.pmp.resp.ld || io.pmp.resp.mmio
    entries(ptr).af := accessFault
    state(ptr) := Mux(accessFault, state_mem_out, state_mem_req)
  }

  when (mem_arb.io.out.fire) {
    for (i <- state.indices) {
      when (state(i) =/= state_idle && state(i) =/= state_mem_out && state(i) =/= state_last_hptw_req && state(i) =/= state_last_hptw_resp
      && entries(i).req_info.s2xlate === mem_arb.io.out.bits.req_info.s2xlate
      && dup(entries(i).req_info.vpn, mem_arb.io.out.bits.req_info.vpn)) {
        // NOTE: "dup enq set state to mem_wait" -> "sending req set other dup entries to mem_wait"
        state(i) := state_mem_waiting
        entries(i).hptw_resp := entries(mem_arb.io.chosen).hptw_resp
        entries(i).wait_id := mem_arb.io.chosen
      }
    }
  }
  when (io.mem.resp.fire) {
    state.indices.map{i =>
      when (state(i) === state_mem_waiting && io.mem.resp.bits.id === entries(i).wait_id) {
        val req_paddr = MakeAddr(entries(i).ppn, getVpnn(entries(i).req_info.vpn, 0))
        val req_hpaddr = MakeAddr(entries(i).hptw_resp.genPPNS2(get_pn(req_paddr)), getVpnn(entries(i).req_info.vpn, 0))
        val index =  Mux(entries(i).req_info.s2xlate === allStage, req_hpaddr, req_paddr)(log2Up(l2tlbParams.blockBytes)-1, log2Up(XLEN/8))
        state(i) := Mux(entries(i).req_info.s2xlate === allStage && !(ptes(index).isPf(0.U, s1Pbmte) || !ptes(index).isLeaf() || ptes(index).isAf() || ptes(index).isStage1Gpf(io.csr.vsatp.mode))
                , state_last_hptw_req, state_mem_out)
        mem_resp_hit(i) := true.B
        entries(i).ppn := ptes(index).getPPN() // for last stage 2 translation
        entries(i).hptw_resp.gpf := Mux(entries(i).req_info.s2xlate === allStage, ptes(index).isStage1Gpf(io.csr.vsatp.mode), false.B)
      }
    }
  }

  when (hyper_arb1.io.out.fire) {
    for (i <- state.indices) {
      when (state(i) === state_hptw_req && entries(i).ppn === hyper_arb1.io.out.bits.ppn && entries(i).req_info.s2xlate === allStage && hyper_arb1.io.chosen === i.U) {
        state(i) := state_hptw_resp
        entries(i).wait_id := hyper_arb1.io.chosen
      }
    }
  }

  when (hyper_arb2.io.out.fire) {
    for (i <- state.indices) {
      when (state(i) === state_last_hptw_req && entries(i).ppn === hyper_arb2.io.out.bits.ppn && entries(i).req_info.s2xlate === allStage && hyper_arb2.io.chosen === i.U) {
        state(i) := state_last_hptw_resp
        entries(i).wait_id := hyper_arb2.io.chosen
      }
    }
  }

  when (io.hptw.resp.fire) {
    for (i <- state.indices) {
      when (state(i) === state_hptw_resp && io.hptw.resp.bits.id === entries(i).wait_id && io.hptw.resp.bits.h_resp.entry.tag === entries(i).ppn) {
        val check_g_perm_fail = !io.hptw.resp.bits.h_resp.gaf && (!io.hptw.resp.bits.h_resp.entry.perm.get.r && !(io.csr.priv.mxr && io.hptw.resp.bits.h_resp.entry.perm.get.x))
        when (check_g_perm_fail || io.hptw.resp.bits.h_resp.gaf || io.hptw.resp.bits.h_resp.gpf) {
          state(i) := state_mem_out
          entries(i).hptw_resp := io.hptw.resp.bits.h_resp
          entries(i).hptw_resp.gpf := io.hptw.resp.bits.h_resp.gpf || check_g_perm_fail
          entries(i).first_s2xlate_fault := io.hptw.resp.bits.h_resp.gaf || io.hptw.resp.bits.h_resp.gpf
        }.otherwise{ // change the entry that is waiting hptw resp
          val need_to_waiting_vec = state.indices.map(i => state(i) === state_mem_waiting && dup(entries(i).req_info.vpn, entries(io.hptw.resp.bits.id).req_info.vpn))
          val waiting_index = ParallelMux(need_to_waiting_vec zip entries.map(_.wait_id))
          state(i) := Mux(Cat(need_to_waiting_vec).orR, state_mem_waiting, state_addr_check)
          entries(i).hptw_resp := io.hptw.resp.bits.h_resp
          entries(i).wait_id := Mux(Cat(need_to_waiting_vec).orR, waiting_index, entries(i).wait_id)
          //To do: change the entry that is having the same hptw req
        }
      }
      when (state(i) === state_last_hptw_resp && io.hptw.resp.bits.id === entries(i).wait_id && io.hptw.resp.bits.h_resp.entry.tag === entries(i).ppn) {
        state(i) := state_mem_out
        entries(i).hptw_resp := io.hptw.resp.bits.h_resp
        //To do: change the entry that is having the same hptw req
      }
    }
  }
  when (io.out.fire) {
    assert(state(mem_ptr) === state_mem_out)
    state(mem_ptr) := state_idle
  }
  mem_resp_hit.map(a => when (a) { a := false.B } )

  when (io.cache.fire) {
    state(cache_ptr) := state_idle
  }
  XSError(io.out.fire && io.cache.fire && (mem_ptr === cache_ptr), "mem resp and cache fire at the same time at same entry")

  when (flush) {
    state.map(_ := state_idle)
  }

  io.in.ready := !full

  io.out.valid := ParallelOR(is_having).asBool
  io.out.bits.req_info := entries(mem_ptr).req_info
  io.out.bits.id := mem_ptr
  io.out.bits.af := entries(mem_ptr).af
  io.out.bits.h_resp := entries(mem_ptr).hptw_resp
  io.out.bits.first_s2xlate_fault := entries(mem_ptr).first_s2xlate_fault

  val hptw_req_arb = Module(new Arbiter(new Bundle{
      val source = UInt(bSourceWidth.W)
      val id = UInt(log2Up(l2tlbParams.llptwsize).W)
      val ppn = UInt(ptePPNLen.W)
    } , 2))
  // first stage 2 translation
  hptw_req_arb.io.in(0).valid := hyper_arb1.io.out.valid
  hptw_req_arb.io.in(0).bits.source := hyper_arb1.io.out.bits.req_info.source
  hptw_req_arb.io.in(0).bits.ppn := hyper_arb1.io.out.bits.ppn
  hptw_req_arb.io.in(0).bits.id := hyper_arb1.io.chosen
  hyper_arb1.io.out.ready := hptw_req_arb.io.in(0).ready
  // last stage 2 translation
  hptw_req_arb.io.in(1).valid := hyper_arb2.io.out.valid
  hptw_req_arb.io.in(1).bits.source := hyper_arb2.io.out.bits.req_info.source
  hptw_req_arb.io.in(1).bits.ppn := hyper_arb2.io.out.bits.ppn
  hptw_req_arb.io.in(1).bits.id := hyper_arb2.io.chosen
  hyper_arb2.io.out.ready := hptw_req_arb.io.in(1).ready
  hptw_req_arb.io.out.ready := io.hptw.req.ready
  io.hptw.req.valid := hptw_req_arb.io.out.fire && !flush
  io.hptw.req.bits.gvpn := hptw_req_arb.io.out.bits.ppn
  io.hptw.req.bits.id := hptw_req_arb.io.out.bits.id
  io.hptw.req.bits.source := hptw_req_arb.io.out.bits.source

  io.mem.req.valid := mem_arb.io.out.valid && !flush
  val mem_paddr = MakeAddr(mem_arb.io.out.bits.ppn, getVpnn(mem_arb.io.out.bits.req_info.vpn, 0))
  val mem_hpaddr = MakeAddr(mem_arb.io.out.bits.hptw_resp.genPPNS2(get_pn(mem_paddr)), getVpnn(mem_arb.io.out.bits.req_info.vpn, 0))
  io.mem.req.bits.addr := Mux(mem_arb.io.out.bits.req_info.s2xlate === allStage, mem_hpaddr, mem_paddr)
  io.mem.req.bits.id := mem_arb.io.chosen
  io.mem.req.bits.hptw_bypassed := false.B
  mem_arb.io.out.ready := io.mem.req.ready
  val mem_refill_id = RegNext(io.mem.resp.bits.id(log2Up(l2tlbParams.llptwsize)-1, 0))
  io.mem.refill := entries(mem_refill_id).req_info
  io.mem.refill.s2xlate := entries(mem_refill_id).req_info.s2xlate
  io.mem.buffer_it := mem_resp_hit
  io.mem.enq_ptr := enq_ptr

  io.cache.valid := Cat(is_cache).orR
  io.cache.bits := ParallelMux(is_cache, entries.map(_.req_info))

  XSPerfAccumulate("llptw_in_count", io.in.fire)
  XSPerfAccumulate("llptw_in_block", io.in.valid && !io.in.ready)
  for (i <- 0 until 7) {
    XSPerfAccumulate(s"enq_state${i}", io.in.fire && enq_state === i.U)
  }
  for (i <- 0 until (l2tlbParams.llptwsize + 1)) {
    XSPerfAccumulate(s"util${i}", PopCount(is_emptys.map(!_)) === i.U)
    XSPerfAccumulate(s"mem_util${i}", PopCount(is_mems) === i.U)
    XSPerfAccumulate(s"waiting_util${i}", PopCount(is_waiting) === i.U)
  }
  XSPerfAccumulate("mem_count", io.mem.req.fire)
  XSPerfAccumulate("mem_cycle", PopCount(is_waiting) =/= 0.U)
  XSPerfAccumulate("blocked_in", io.in.valid && !io.in.ready)

  for (i <- 0 until l2tlbParams.llptwsize) {
    TimeOutAssert(state(i) =/= state_idle, timeOutThreshold, s"missqueue time out no out ${i}")
  }

  val perfEvents = Seq(
    ("tlbllptw_incount           ", io.in.fire               ),
    ("tlbllptw_inblock           ", io.in.valid && !io.in.ready),
    ("tlbllptw_memcount          ", io.mem.req.fire          ),
    ("tlbllptw_memcycle          ", PopCount(is_waiting)       ),
  )
  generatePerfEvent()
}

/*========================= HPTW ==============================*/

/** HPTW : Hypervisor Page Table Walker
  * the page walker take the virtual machine's page walk.
  * guest physical address translation, guest physical address -> host physical address
  **/
class HPTWIO()(implicit p: Parameters) extends MMUIOBaseBundle with HasPtwConst {
  val req = Flipped(DecoupledIO(new Bundle {
    val source = UInt(bSourceWidth.W)
    val id = UInt(log2Up(l2tlbParams.llptwsize).W)
    val gvpn = UInt(gvpnLen.W)
    val ppn = UInt(ppnLen.W)
    val l3Hit = if (EnableSv48) Some(new Bool()) else None
    val l2Hit = Bool()
    val l1Hit = Bool()
    val bypassed = Bool() // if bypass, don't refill
  }))
  val resp = DecoupledIO(new Bundle {
    val source = UInt(bSourceWidth.W)
    val resp = Output(new HptwResp())
    val id = Output(UInt(bMemID.W))
  })

  val mem = new Bundle {
    val req = DecoupledIO(new L2TlbMemReqBundle())
    val resp = Flipped(ValidIO(UInt(XLEN.W)))
    val mask = Input(Bool())
  }
  val refill = Output(new Bundle {
    val req_info = new L2TlbInnerBundle()
    val level = UInt(log2Up(Level + 1).W)
  })
  val pmp = new Bundle {
    val req = ValidIO(new PMPReqBundle())
    val resp = Flipped(new PMPRespBundle())
  }
}

class HPTW()(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new HPTWIO)
  val hgatp = io.csr.hgatp
  val mpbmte = io.csr.mPBMTE
  val sfence = io.sfence
  val flush = sfence.valid || hgatp.changed || io.csr.satp.changed || io.csr.vsatp.changed
  val mode = hgatp.mode

  val level = RegInit(3.U(log2Up(Level + 1).W))
  val af_level = RegInit(3.U(log2Up(Level + 1).W)) // access fault return this level
  val gpaddr = Reg(UInt(GPAddrBits.W))
  val req_ppn = Reg(UInt(ppnLen.W))
  val vpn = gpaddr(GPAddrBits-1, offLen)
  val levelNext = level - 1.U
  val l3Hit = Reg(Bool())
  val l2Hit = Reg(Bool())
  val l1Hit = Reg(Bool())
  val bypassed = Reg(Bool())
//  val pte = io.mem.resp.bits.MergeRespToPte()
  val pte = io.mem.resp.bits.asTypeOf(new PteBundle().cloneType)
  val ppn_l3 = Mux(l3Hit, req_ppn, pte.ppn)
  val ppn_l2 = Mux(l2Hit, req_ppn, pte.ppn)
  val ppn_l1 = Mux(l1Hit, req_ppn, pte.ppn)
  val ppn = Wire(UInt(PAddrBits.W))
  val p_pte = MakeAddr(ppn, getVpnn(vpn, level))
  val pg_base = Wire(UInt(PAddrBits.W))
  val mem_addr = Wire(UInt(PAddrBits.W))
  if (EnableSv48) {
    when (mode === Sv48) {
      ppn := Mux(af_level === 2.U, ppn_l3, Mux(af_level === 1.U, ppn_l2, ppn_l1)) // for l2, l1 and l3
      pg_base := MakeGPAddr(hgatp.ppn, getGVpnn(vpn, 3.U, mode = Sv48)) // for l3
      mem_addr := Mux(af_level === 3.U, pg_base, p_pte)
    } .otherwise {
      ppn := Mux(af_level === 1.U, ppn_l2, ppn_l1) //for l1 and l2
      pg_base := MakeGPAddr(hgatp.ppn, getGVpnn(vpn, 2.U, mode = Sv39))
      mem_addr := Mux(af_level === 2.U, pg_base, p_pte)
    }
  } else {
    ppn := Mux(af_level === 1.U, ppn_l2, ppn_l1) //for l1 and l2
    pg_base := MakeGPAddr(hgatp.ppn, getGVpnn(vpn, 2.U, mode = Sv39))
    mem_addr := Mux(af_level === 2.U, pg_base, p_pte)
  }

  //s/w register
  val s_pmp_check = RegInit(true.B)
  val s_mem_req = RegInit(true.B)
  val w_mem_resp = RegInit(true.B)
  val idle = RegInit(true.B)
  val mem_addr_update = RegInit(false.B)
  val finish = WireInit(false.B)

  val sent_to_pmp = !idle && (!s_pmp_check || mem_addr_update) && !finish
  val pageFault = pte.isGpf(level, mpbmte) || (!pte.isLeaf() && level === 0.U)
  val accessFault = RegEnable(io.pmp.resp.ld || io.pmp.resp.mmio, sent_to_pmp)

  val ppn_af = pte.isAf()
  val find_pte = pte.isLeaf() || ppn_af || pageFault

  val resp_valid = !idle && mem_addr_update && ((w_mem_resp && find_pte) || (s_pmp_check && accessFault))
  val id = Reg(UInt(log2Up(l2tlbParams.llptwsize).W))
  val source = RegEnable(io.req.bits.source, io.req.fire)

  io.req.ready := idle
  val resp = Wire(new HptwResp())
  // accessFault > pageFault > ppn_af
  resp.apply(
    gpf = pageFault && !accessFault,
    gaf = accessFault || (ppn_af && !pageFault),
    level = Mux(accessFault, af_level, level),
    pte = pte, 
    vpn = vpn, 
    vmid = hgatp.vmid
  )
  io.resp.valid := resp_valid
  io.resp.bits.id := id
  io.resp.bits.resp := resp
  io.resp.bits.source := source

  io.pmp.req.valid := DontCare
  io.pmp.req.bits.addr := mem_addr
  io.pmp.req.bits.size := 3.U
  io.pmp.req.bits.cmd := TlbCmd.read

  io.mem.req.valid := !s_mem_req && !io.mem.mask && !accessFault && s_pmp_check
  io.mem.req.bits.addr := mem_addr
  io.mem.req.bits.id := HptwReqId.U(bMemID.W)
  io.mem.req.bits.hptw_bypassed := bypassed

  io.refill.req_info.vpn := vpn
  io.refill.level := level
  io.refill.req_info.source := source
  io.refill.req_info.s2xlate := onlyStage2
  when (idle){
    when(io.req.fire){
      bypassed := io.req.bits.bypassed
      idle := false.B
      gpaddr := Cat(io.req.bits.gvpn, 0.U(offLen.W))
      accessFault := false.B
      s_pmp_check := false.B
      id := io.req.bits.id
      req_ppn := io.req.bits.ppn
      if (EnableSv48) {
        when (mode === Sv48) {
          level := Mux(io.req.bits.l1Hit, 0.U, Mux(io.req.bits.l2Hit, 1.U, Mux(io.req.bits.l3Hit.get, 2.U, 3.U)))
          af_level := Mux(io.req.bits.l1Hit, 0.U, Mux(io.req.bits.l2Hit, 1.U, Mux(io.req.bits.l3Hit.get, 2.U, 3.U)))
          l3Hit := io.req.bits.l3Hit.get
        } .otherwise {
          level := Mux(io.req.bits.l1Hit, 0.U, Mux(io.req.bits.l2Hit, 1.U, 2.U))
          af_level := Mux(io.req.bits.l1Hit, 0.U, Mux(io.req.bits.l2Hit, 1.U, 2.U))
          l3Hit := false.B
        }
      } else {
        level := Mux(io.req.bits.l1Hit, 0.U, Mux(io.req.bits.l2Hit, 1.U, 2.U))
        af_level := Mux(io.req.bits.l1Hit, 0.U, Mux(io.req.bits.l2Hit, 1.U, 2.U))
        l3Hit := false.B
      }
      l2Hit := io.req.bits.l2Hit
      l1Hit := io.req.bits.l1Hit
    }
  }

  when(sent_to_pmp && !mem_addr_update){
    s_mem_req := false.B
    s_pmp_check := true.B
  }

  when(accessFault && !idle){
    s_pmp_check := true.B
    s_mem_req := true.B
    w_mem_resp := true.B
    mem_addr_update := true.B
  }

  when(io.mem.req.fire){
    s_mem_req := true.B
    w_mem_resp := false.B
  }

  when(io.mem.resp.fire && !w_mem_resp){
    w_mem_resp := true.B
    af_level := af_level - 1.U
    mem_addr_update := true.B
  }

  when(mem_addr_update){
    when(!(find_pte || accessFault)){
      level := levelNext
      s_mem_req := false.B
      mem_addr_update := false.B
    }.elsewhen(resp_valid){
      when(io.resp.fire){
        idle := true.B
        mem_addr_update := false.B
        accessFault := false.B
      }
      finish := true.B
    }
  }
   when (flush) {
    idle := true.B
    s_pmp_check := true.B
    s_mem_req := true.B
    w_mem_resp := true.B
    accessFault := false.B
    mem_addr_update := false.B
  }
}
