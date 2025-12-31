/***************************************************************************************
* Copyright (c) 2021-2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
* Copyright (c) 2024-2025 Institute of Information Engineering, Chinese Academy of Sciences
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

class NewPtwReqBundle(implicit p: Parameters) extends PtwBundle {
  val vpn = UInt(vpnLen.W)
  val doUpdateDBit = Bool()
  // TODO: req source for debug
  // val source = UInt(bSourceWidth.W)
}

class NewPtwPpnSector(implicit p: Parameters) extends PtwBundle {
  val valid = Vec(tlbcontiguous, Bool())
  val ppnHigh = UInt(sectorppnLen.W)
  val ppnLow = Vec(tlbcontiguous, UInt(sectortlbwidth.W))
  val gppnHigh = UInt(sectorgvpnLen.W)
  val gppnLow = Vec(tlbcontiguous, UInt(sectortlbwidth.W))

  def getPpn(idx: UInt): (Bool, UInt) = {
    (valid(idx), Cat(ppnHigh, ppnLow(idx)))
  }

  def getGppn(idx: UInt): (Bool, UInt) = {
    (valid(idx), Cat(gppnHigh, gppnLow(idx)))
  }
}

class NewPtwGranularity(implicit p: Parameters) extends PtwBundle {
  val level = UInt(log2Up(Level + 1).W)
  val napot = Bool()
  // Reserved for future extension
  // val napotBits = UInt(log2Up(vpnnLen + 1).W)
}

class NewPteInfoBundle(implicit p: Parameters) extends PtwBundle {
  val v = Bool()
  val r = Bool()
  val w = Bool()
  val x = Bool()
  val u = Bool()
  val g = Bool()
  val a = Bool()
  val d = Bool()
  val pbmt = UInt(ptePbmtLen.W)
}

class NewPtwExceptionBundle(implicit p: Parameters) extends PtwBundle {
  val pageFault = Bool()
  val guestPageFault = Bool()
  val accessFault = Bool()
  val hardwareError = Bool()

  val forSPte = Bool()        // for GPF, AF, HE
  val forGPte = Bool()        // for AF, HE
}

class NewPtwRespBundle(implicit p: Parameters) extends PtwBundle {
  val vpn = UInt(vpnLen.W)
  val ppnSector = new NewPtwPpnSector()
  val sStageGranularity = new NewPtwGranularity()
  val gStageGranularity = new NewPtwGranularity()
  // val pmpGranularity = new NewPtwGranularity()
  val sStagePteInfo = new NewPteInfoBundle()
  val gStagePteInfo = new NewPteInfoBundle()
  // val pmpCfgInfo = new NewPteInfoBundle()
  val exception = new NewPtwExceptionBundle()
}

class PageTableWalkerIO()(implicit p: Parameters) extends TlbBundle with HasPtwConst {
  // Global Status
  val sfence = Input(new SfenceBundle)
  val csr = Input(new TlbCsrBundle)

  // PTW req and resp
  val req = Flipped(DecoupledIO(new NewPtwReqBundle()))
  val resp = DecoupledIO(new NewPtwRespBundle())

  // Mem req and resp
  // TODO
}

class NewPageTableWalker()(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new PTWIO)

  private def nPtwFsmState: Int = 5
  private object PtwFsmState extends EnumUInt(nPtwFsmState) {
    def Idle:                     UInt = 0.U(width.W)
    def PageCacheReq:             UInt = 1.U(width.W)
    def PageCacheWait:            UInt = 2.U(width.W)
    def CalculatePteAddr:         UInt = 3.U(width.W)
    def PteAddrGStageTranslate:   UInt = 4.U(width.W)
    def PtePaddrProtectReq:       UInt = 5.U(width.W)
    def PtePaddrProtectWait:      UInt = 6.U(width.W)
    def MemoryReadReq:            UInt = 7.U(width.W)
    def MemoryReadWait:           UInt = 8.U(width.W)
    def HandlePte:                UInt = 9.U(width.W)
    def AddrGStageTranslate:      UInt = 10.U(width.W)
    def PaddrProtectReq:          UInt = 11.U(width.W)
    def PaddrProtectWait:         UInt = 12.U(width.W)
    def DoResp:                   UInt = 13.U(width.W)
  }

  val originReqInfo = Reg(new L2TlbInnerBundle())

  // Main State
  private val state     = RegInit(PtwFsmState.Idle)
  private val nextState = WireDefault(state)
  state := nextState

  // Additional State
  private val level = Reg(UInt(log2Up(Level).W))

  // Idle
  when (state === PtwFsmState.Idle) {
    when (io.req.fire) {
      nextState := PtwFsmState.PageCacheReq
      level := Level.U
    }
  }

}

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
    val bitmapCheck = Option.when(HasBitmapCheck)(new Bundle {
      val jmp_bitmap_check = Bool() // super page in PtwCache ptw hit, but need bitmap check
      val pte = UInt(XLEN.W) // Page Table Entry
      val cfs = Vec(tlbcontiguous, Bool()) // Bitmap Check Failed Vector
      val SPlevel = UInt(log2Up(Level).W)
    })
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
  val bitmap = Option.when(HasBitmapCheck)(new Bundle {
      val req = DecoupledIO(new bitmapReqBundle())
      val resp = Flipped(DecoupledIO(new bitmapRespBundle()))
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

  // mbmc:bitmap csr
  val mbmc = io.csr.mbmc
  val bitmap_enable = (if (HasBitmapCheck) true.B else false.B) && mbmc.BME === 1.U && mbmc.CMODE === 0.U

  // when req.fire, should use current_satp
  val current_satp = Mux(io.req.bits.req_info.s2xlate =/= noS2xlate, io.csr.vsatp, io.csr.satp)
  val satp = Mux(enableS2xlate, io.csr.vsatp, io.csr.satp)
  val s1Pbmte = Mux(req_s2xlate =/= noS2xlate, io.csr.hPBMTE, io.csr.mPBMTE)

  val current_mode = current_satp.mode
  val mode = satp.mode
  val hgatp = io.csr.hgatp
  val flush = io.sfence.valid || io.csr.satp.changed || io.csr.vsatp.changed || io.csr.hgatp.changed || io.csr.priv.virt_changed
  val s2xlate = enableS2xlate && !onlyS1xlate
  val level = RegInit(3.U(log2Up(Level + 1).W))
  val af_level = RegInit(3.U(log2Up(Level + 1).W)) // access fault return this level
  val gpf_level = RegInit(3.U(log2Up(Level + 1).W))
  val ppn = Reg(UInt(ptePPNLen.W))
  val vpn = Reg(UInt(vpnLen.W)) // vpn or gvpn(onlyS2xlate)
  val levelNext = level - 1.U
  val l3Hit = Reg(Bool())
  val l2Hit = Reg(Bool())
  val jmp_bitmap_check_w = if (HasBitmapCheck) { io.req.bits.bitmapCheck.get.jmp_bitmap_check && io.req.bits.req_info.s2xlate =/= onlyStage2 } else { false.B }
  val jmp_bitmap_check_r = if (HasBitmapCheck) { RegEnable(jmp_bitmap_check_w, io.req.fire) } else { false.B }
  val cache_pte = Option.when(HasBitmapCheck)(RegEnable(io.req.bits.bitmapCheck.get.pte.asTypeOf(new PteBundle().cloneType), io.req.fire))
  val pte = if (HasBitmapCheck) { Mux(jmp_bitmap_check_r, cache_pte.get, io.mem.resp.bits.asTypeOf(new PteBundle().cloneType)) } else { mem.resp.bits.asTypeOf(new PteBundle()) }

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

  val s_bitmap_check = RegInit(true.B)
  val w_bitmap_resp = RegInit(true.B)
  val whether_need_bitmap_check = RegInit(false.B)
  val bitmap_checkfailed = RegInit(false.B)

  val idle = RegInit(true.B)
  val finish = WireInit(false.B)
  dontTouch(finish)
  val vs_finish = WireInit(false.B) // need to wait for G-stage translate, should not do pmp check
  dontTouch(vs_finish)

  val hptw_pageFault = RegInit(false.B)
  val hptw_accessFault = RegInit(false.B)
  val need_last_s2xlate = RegInit(false.B)
  val stage1Hit = RegEnable(io.req.bits.stage1Hit, io.req.fire)
  val stage1 = RegEnable(io.req.bits.stage1, io.req.fire)
  val hptw_resp_stage2 = Reg(Bool())
  val first_gvpn_check_fail = RegInit(false.B)

  // use accessfault repersent bitmap check failed
  val pte_isAf = Mux(bitmap_enable, pte.isAf() || bitmap_checkfailed, pte.isAf())
  val ppn_af = if (HasBitmapCheck) {
    Mux(enableS2xlate, Mux(onlyS1xlate, pte_isAf, false.B), pte_isAf) // In two-stage address translation, stage 1 ppn is a vpn for host, so don't need to check ppn_high
  } else {
    Mux(enableS2xlate, Mux(onlyS1xlate, pte.isAf(), false.B), pte.isAf()) // In two-stage address translation, stage 1 ppn is a vpn for host, so don't need to check ppn_high
  }
  val pte_valid = RegInit(false.B)  // avoid l1tlb pf from stage1 when gpf happens in the first s2xlate in PTW

  val pageFault = pte.isPf(level, s1Pbmte)
  val find_pte = pte.isLeaf() || ppn_af || pageFault
  val to_find_pte = level === 1.U && find_pte === false.B
  val source = RegEnable(io.req.bits.req_info.source, io.req.fire)

  val sent_to_pmp = idle === false.B && (s_pmp_check === false.B || mem_addr_update) && !finish && !vs_finish && !first_gvpn_check_fail && !(find_pte && pte_valid)
  val accessFault = RegEnable(io.pmp.resp.ld || io.pmp.resp.mmio, false.B, sent_to_pmp)

  val l3addr = Wire(UInt(ptePaddrLen.W))
  val l2addr = Wire(UInt(ptePaddrLen.W))
  val l1addr = Wire(UInt(ptePaddrLen.W))
  val hptw_addr = Wire(UInt(ptePaddrLen.W))
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
  hptw_addr := Mux(af_level === 3.U, l3addr, Mux(af_level === 2.U, l2addr, l1addr))
  mem_addr := hptw_addr(PAddrBits - 1, 0)

  val hptw_resp = Reg(new HptwResp)

  val update_full_gvpn_mem_resp = RegInit(false.B)
  val full_gvpn_reg = Reg(UInt(ptePPNLen.W))
  val full_gvpn_wire = pte.getPPN()
  val full_gvpn = Mux(update_full_gvpn_mem_resp, full_gvpn_wire, full_gvpn_reg)

  val gpaddr = MuxCase(hptw_addr, Seq(
    (stage1Hit || onlyS2xlate) -> Cat(full_gvpn, 0.U(offLen.W)),
    !s_last_hptw_req -> Cat(MuxLookup(level, pte.getPPN())(Seq(
      3.U -> Cat(pte.getPPN()(ptePPNLen - 1, vpnnLen * 3), vpn(vpnnLen * 3 - 1, 0)),
      2.U -> Cat(pte.getPPN()(ptePPNLen - 1, vpnnLen * 2), vpn(vpnnLen * 2 - 1, 0)),
      1.U -> Cat(pte.getPPN()(ptePPNLen - 1, vpnnLen), vpn(vpnnLen - 1, 0)
    ))),
    0.U(offLen.W))
  ))
  val gvpn_gpf =
    (!(hptw_pageFault || hptw_accessFault || ((pageFault || ppn_af) && pte_valid)) &&
    Mux(
      s2xlate && io.csr.hgatp.mode === Sv39x4,
      full_gvpn(ptePPNLen - 1, GPAddrBitsSv39x4 - offLen) =/= 0.U,
      Mux(
        s2xlate && io.csr.hgatp.mode === Sv48x4,
        full_gvpn(ptePPNLen - 1, GPAddrBitsSv48x4 - offLen) =/= 0.U,
        false.B
      )
    )) || first_gvpn_check_fail

  val guestFault = hptw_pageFault || hptw_accessFault || gvpn_gpf
  val hpaddr = Cat(hptw_resp.genPPNS2(get_pn(gpaddr)), get_off(gpaddr))
  val fake_h_resp = WireInit(0.U.asTypeOf(new HptwResp))
  fake_h_resp.entry.tag := get_pn(gpaddr)
  fake_h_resp.entry.vmid.map(_ := io.csr.hgatp.vmid)
  fake_h_resp.gpf := true.B

  val fake_pte = WireInit(0.U.asTypeOf(new PteBundle()))
  fake_pte.perm.v := false.B // tell L1TLB this is fake pte
  fake_pte.ppn := ppn(ppnLen - 1, 0)
  fake_pte.ppn_high := ppn(ptePPNLen - 1, ppnLen)

  io.req.ready := idle
  val ptw_resp = Wire(new PtwMergeResp)
  // pageFault is always valid when pte_valid
  val resp_pf = pte_valid && pageFault
  // when (pte_valid && (pageFault || guestFault), should not report accessFault or ppn_af
  val resp_af = (accessFault || ppn_af) && !((pte_valid && pageFault) || guestFault)
  // should use af_level when accessFault && !((pte_valid && pageFault) || guestFault)
  val resp_level = Mux(accessFault && resp_af, af_level, Mux(guestFault, gpf_level, level))
  // when ptw do not really send a memory request, should use fake_pte
  val resp_pte = Mux(pte_valid, pte, fake_pte)
  ptw_resp.apply(resp_pf, resp_af, resp_level, resp_pte, vpn, satp.asid, hgatp.vmid, vpn(sectortlbwidth - 1, 0), not_super = false, not_merge = false, bitmap_checkfailed.asBool)

  val normal_resp = mem_addr_update && !need_last_s2xlate && (guestFault || (w_mem_resp && find_pte) || (s_pmp_check && accessFault) || onlyS2xlate )
  val stageHit_resp = hptw_resp_stage2
  io.resp.valid := !idle && Mux(stage1Hit, stageHit_resp, normal_resp)
  io.resp.bits.source := source
  io.resp.bits.resp := Mux(stage1Hit || (l3Hit || l2Hit) && guestFault && !pte_valid, stage1, ptw_resp)
  io.resp.bits.h_resp := Mux(gvpn_gpf, fake_h_resp, hptw_resp)
  io.resp.bits.s2xlate := req_s2xlate

  io.llptw.valid := s_llptw_req === false.B && to_find_pte && !accessFault && !guestFault
  io.llptw.bits.req_info.source := source
  io.llptw.bits.req_info.vpn := vpn
  io.llptw.bits.req_info.s2xlate := req_s2xlate
  io.llptw.bits.ppn := DontCare
  if (HasBitmapCheck) {
    io.llptw.bits.bitmapCheck.get.jmp_bitmap_check := DontCare
    io.llptw.bits.bitmapCheck.get.ptes := DontCare
    io.llptw.bits.bitmapCheck.get.cfs := DontCare
    io.llptw.bits.bitmapCheck.get.hitway := DontCare
  }

  io.pmp.req.valid := DontCare // samecycle, do not use valid
  io.pmp.req.bits.addr := Mux(s2xlate, hpaddr, mem_addr)
  io.pmp.req.bits.size := 3.U // TODO: fix it
  io.pmp.req.bits.cmd := TlbCmd.read

  if (HasBitmapCheck) {
    val cache_level = RegEnable(io.req.bits.bitmapCheck.get.SPlevel, io.req.fire)
    io.bitmap.get.req.valid := !s_bitmap_check
    io.bitmap.get.req.bits.bmppn := pte.ppn
    io.bitmap.get.req.bits.id := FsmReqID.U(bMemID.W)
    io.bitmap.get.req.bits.vpn := vpn
    io.bitmap.get.req.bits.level := Mux(jmp_bitmap_check_r, cache_level, level)
    io.bitmap.get.req.bits.way_info := DontCare
    io.bitmap.get.req.bits.hptw_bypassed := false.B
    io.bitmap.get.req.bits.n := pte.n
    io.bitmap.get.req.bits.s2xlate := req_s2xlate
    io.bitmap.get.resp.ready := !w_bitmap_resp
  }
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

  if (HasBitmapCheck) {
    when (io.req.fire && jmp_bitmap_check_w) {
      idle := false.B
      req_s2xlate := io.req.bits.req_info.s2xlate
      vpn := io.req.bits.req_info.vpn
      s_bitmap_check := false.B
      need_last_s2xlate := false.B
      hptw_pageFault := false.B
      hptw_accessFault := false.B
      level := io.req.bits.bitmapCheck.get.SPlevel
      pte_valid := true.B
      accessFault := false.B
    }
  }

  when (io.req.fire && io.req.bits.stage1Hit && (if (HasBitmapCheck) !jmp_bitmap_check_w else true.B)) {
    idle := false.B
    req_s2xlate := io.req.bits.req_info.s2xlate
    s_last_hptw_req := false.B
    hptw_resp_stage2 := false.B
    need_last_s2xlate := false.B
    hptw_pageFault := false.B
    hptw_accessFault := false.B
    full_gvpn_reg := io.req.bits.stage1.genPPN()
  }

  when (io.resp.fire && stage1Hit){
    idle := true.B
  }

  when (io.req.fire && !io.req.bits.stage1Hit && (if (HasBitmapCheck) !jmp_bitmap_check_w else true.B)) {
    val req = io.req.bits
    val gvpn_wire = Wire(UInt(ptePPNLen.W))
    if (EnableSv48) {
      when (current_mode === Sv48) {
        level := Mux(req.l2Hit, 1.U, Mux(req.l3Hit.get, 2.U, 3.U))
        af_level := Mux(req.l2Hit, 1.U, Mux(req.l3Hit.get, 2.U, 3.U))
        gpf_level := Mux(req.l2Hit, 2.U, Mux(req.l3Hit.get, 3.U, 0.U))
        ppn := Mux(req.l2Hit || req.l3Hit.get, io.req.bits.ppn, current_satp.ppn)
        l3Hit := req.l3Hit.get
        gvpn_wire := Mux(req.l2Hit || req.l3Hit.get, io.req.bits.ppn, current_satp.ppn)
      } .otherwise {
        level := Mux(req.l2Hit, 1.U, 2.U)
        af_level := Mux(req.l2Hit, 1.U, 2.U)
        gpf_level := Mux(req.l2Hit, 2.U, 0.U)
        ppn := Mux(req.l2Hit, io.req.bits.ppn, current_satp.ppn)
        l3Hit := false.B
        gvpn_wire := Mux(req.l2Hit, io.req.bits.ppn, current_satp.ppn)
      }
    } else {
      level := Mux(req.l2Hit, 1.U, 2.U)
      af_level := Mux(req.l2Hit, 1.U, 2.U)
      gpf_level := Mux(req.l2Hit, 2.U, 0.U)
      ppn := Mux(req.l2Hit, io.req.bits.ppn, current_satp.ppn)
      l3Hit := false.B
      gvpn_wire := Mux(req.l2Hit, io.req.bits.ppn, current_satp.ppn)
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
      full_gvpn_reg := io.req.bits.req_info.vpn
      val onlys2_gpaddr = Cat(io.req.bits.req_info.vpn, 0.U(offLen.W)) // is 50 bits, don't need to check high bits when sv48x4 is enabled
      val check_gpa_high_fail = Mux(io.req.bits.req_info.s2xlate === onlyStage2 && io.csr.hgatp.mode === Sv39x4, onlys2_gpaddr(onlys2_gpaddr.getWidth - 1, GPAddrBitsSv39x4) =/= 0.U, false.B)
      need_last_s2xlate := false.B
      when(check_gpa_high_fail){
        mem_addr_update := true.B
        first_gvpn_check_fail := true.B
      }.otherwise{
        s_last_hptw_req := false.B
      }
    }.elsewhen(io.req.bits.req_info.s2xlate === allStage){
      full_gvpn_reg := 0.U
      val allstage_gpaddr = Cat(gvpn_wire, 0.U(offLen.W))
      val check_gpa_high_fail = Mux(io.csr.hgatp.mode === Sv39x4, allstage_gpaddr(allstage_gpaddr.getWidth - 1, GPAddrBitsSv39x4) =/= 0.U, Mux(io.csr.hgatp.mode === Sv48x4, allstage_gpaddr(allstage_gpaddr.getWidth - 1, GPAddrBitsSv48x4) =/= 0.U, false.B))
      when(check_gpa_high_fail){
        mem_addr_update := true.B
        first_gvpn_check_fail := true.B
      }.otherwise{
        need_last_s2xlate := true.B
        s_hptw_req := false.B
      }
    }.otherwise {
      full_gvpn_reg := 0.U
      need_last_s2xlate := false.B
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
      need_last_s2xlate := false.B
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
    need_last_s2xlate := false.B
    if (HasBitmapCheck) {
      s_bitmap_check := true.B
      w_bitmap_resp := true.B
      whether_need_bitmap_check := false.B
      bitmap_checkfailed := false.B
    }
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
    need_last_s2xlate := false.B
    if (HasBitmapCheck) {
      s_bitmap_check := true.B
      w_bitmap_resp := true.B
      whether_need_bitmap_check := false.B
      bitmap_checkfailed := false.B
    }
  }

  when (mem.req.fire){
    s_mem_req := true.B
    w_mem_resp := false.B
  }

  when(mem.resp.fire && w_mem_resp === false.B){
    w_mem_resp := true.B
    af_level := af_level - 1.U
    gpf_level := Mux(mode === Sv39 && !pte_valid && !l2Hit, gpf_level - 2.U, gpf_level - 1.U)
    pte_valid := true.B
    update_full_gvpn_mem_resp := true.B
    if (HasBitmapCheck) {
      when (bitmap_enable) {
        whether_need_bitmap_check := true.B
      } .otherwise {
        s_llptw_req := false.B
        mem_addr_update := true.B
        whether_need_bitmap_check := false.B
      }
    } else {
      s_llptw_req := false.B
      mem_addr_update := true.B
    }
  }

  when(update_full_gvpn_mem_resp) {
    update_full_gvpn_mem_resp := false.B
    full_gvpn_reg := pte.getPPN()
  }

  if (HasBitmapCheck) {
    when (whether_need_bitmap_check) {
      when (bitmap_enable && (!enableS2xlate || onlyS1xlate) && pte.isLeaf() && !pageFault) {
        s_bitmap_check := false.B
        whether_need_bitmap_check := false.B
      } .otherwise {
        mem_addr_update := true.B
        s_llptw_req := false.B
        whether_need_bitmap_check := false.B
      }
    }
    // bitmapcheck
    when (io.bitmap.get.req.fire) {
      s_bitmap_check := true.B
      w_bitmap_resp := false.B
    }
    when (io.bitmap.get.resp.fire) {
      w_bitmap_resp := true.B
      mem_addr_update := true.B
      bitmap_checkfailed := io.bitmap.get.resp.bits.cf
    }
  }

  when(mem_addr_update){
    when(level >= 2.U && !onlyS2xlate && !(guestFault || find_pte || accessFault)) {
      level := levelNext
      when(s2xlate){
        s_hptw_req := false.B
        vs_finish := true.B
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
        need_last_s2xlate := false.B
      }
      finish := true.B
    }.elsewhen(s2xlate && need_last_s2xlate === true.B) {
      need_last_s2xlate := false.B
      when(!(guestFault || accessFault || pageFault || ppn_af)){
        s_last_hptw_req := false.B
        mem_addr_update := false.B
      }
    }.elsewhen(io.resp.valid){
      when(io.resp.fire) {
        idle := true.B
        s_llptw_req := true.B
        mem_addr_update := false.B
        accessFault := false.B
        first_gvpn_check_fail := false.B
        if (HasBitmapCheck) {
          bitmap_checkfailed := false.B
        }
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
    first_gvpn_check_fail := false.B
    s_hptw_req := true.B
    w_hptw_resp := true.B
    s_last_hptw_req := true.B
    w_last_hptw_resp := true.B
    if (HasBitmapCheck) {
      s_bitmap_check := true.B
      w_bitmap_resp := true.B
      whether_need_bitmap_check := false.B
      bitmap_checkfailed := false.B
    }
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

  val perfEvents = Seq(
    ("fsm_count         ", io.req.fire                                     ),
    ("fsm_busy          ", !idle                                           ),
    ("fsm_idle          ", idle                                            ),
    ("resp_blocked      ", io.resp.valid && !io.resp.ready                 ),
    ("mem_count         ", mem.req.fire                                    ),
    ("mem_cycle         ", BoolStopWatch(mem.req.fire, mem.resp.fire, true)),
    ("mem_blocked       ", mem.req.valid && !mem.req.ready                 ),
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
  val bitmapCheck = Option.when(HasBitmapCheck)(new Bundle {
    val jmp_bitmap_check = Bool() // find pte in l0 or sp, but need bitmap check
    val ptes = Vec(tlbcontiguous, UInt(XLEN.W)) // Page Table Entry Vector
    val cfs = Vec(tlbcontiguous, Bool()) // Bitmap Check Failed Vector
    val hitway = UInt(l2tlbParams.l0nWays.W)
  })
}

class LLPTWIO(implicit p: Parameters) extends MMUIOBaseBundle with HasPtwConst {
  val in = Flipped(DecoupledIO(new LLPTWInBundle()))
  val out = DecoupledIO(new Bundle {
    val req_info = Output(new L2TlbInnerBundle())
    val id = Output(UInt(bMemID.W))
    val h_resp = Output(new HptwResp)
    val first_s2xlate_fault = Output(Bool()) // Whether the first stage 2 translation occurs pf/af
    val af = Output(Bool())
    val bitmapCheck = Option.when(HasBitmapCheck)(new Bundle {
      val jmp_bitmap_check = Bool() // find pte in l0 or sp, but need bitmap check
      val ptes = Vec(tlbcontiguous, UInt(XLEN.W)) // Page Table Entry Vector
      val cfs = Vec(tlbcontiguous, Bool()) // Bitmap Check Failed Vector
    })
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
  val pmp = Vec(2, new Bundle {
    val req  = Valid(new PMPReqBundle())
    val resp = Flipped(new PMPRespBundle())
  })
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
  val bitmap = Option.when(HasBitmapCheck)(new Bundle {
      val req = DecoupledIO(new bitmapReqBundle())
      val resp = Flipped(DecoupledIO(new bitmapRespBundle()))
  })

  val l0_way_info = Option.when(HasBitmapCheck)(Input(UInt(l2tlbParams.l0nWays.W)))
}

class LLPTWEntry(implicit p: Parameters) extends XSBundle with HasPtwConst {
  val req_info = new L2TlbInnerBundle()
  val ppn = UInt(ptePPNLen.W)
  val wait_id = UInt(log2Up(l2tlbParams.llptwsize).W)
  val af = Bool()
  val hptw_resp = new HptwResp()
  val first_s2xlate_fault = Output(Bool())
  val cf = Bool()
  val from_l0 = Bool()
  val way_info = UInt(l2tlbParams.l0nWays.W)
  val jmp_bitmap_check = Bool()
  val n = Bool()
  val ptes = Vec(tlbcontiguous, UInt(XLEN.W))
  val cfs = Vec(tlbcontiguous, Bool())
}


class LLPTW(implicit p: Parameters) extends XSModule with HasPtwConst with HasPerfEvents {
  val io = IO(new LLPTWIO())

  // mbmc:bitmap csr
  val mbmc = io.csr.mbmc
  val bitmap_enable = (if (HasBitmapCheck) true.B else false.B) && mbmc.BME === 1.U && mbmc.CMODE === 0.U

  val flush = io.sfence.valid || io.csr.satp.changed || io.csr.vsatp.changed || io.csr.hgatp.changed || io.csr.priv.virt_changed
  val entries = RegInit(VecInit(Seq.fill(l2tlbParams.llptwsize)(0.U.asTypeOf(new LLPTWEntry()))))
  val state_idle :: state_hptw_req :: state_hptw_resp :: state_addr_check :: state_mem_req :: state_mem_waiting :: state_mem_out :: state_last_hptw_req :: state_last_hptw_resp :: state_cache :: state_bitmap_check :: state_bitmap_resp :: Nil = Enum(12)
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
  val is_bitmap_req = state.map(_ === state_bitmap_check)
  val is_bitmap_resp = state.map(_ === state_bitmap_resp)

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


  val bitmap_arb = Option.when(HasBitmapCheck)(Module(new RRArbiterInit(new bitmapReqBundle(), l2tlbParams.llptwsize)))
  val way_info = Option.when(HasBitmapCheck)(Wire(Vec(l2tlbParams.llptwsize, UInt(l2tlbParams.l0nWays.W))))
  if (HasBitmapCheck) {
    for (i <- 0 until l2tlbParams.llptwsize) {
      bitmap_arb.get.io.in(i).valid := is_bitmap_req(i)
      bitmap_arb.get.io.in(i).bits.bmppn  := entries(i).ppn
      bitmap_arb.get.io.in(i).bits.vpn := entries(i).req_info.vpn
      bitmap_arb.get.io.in(i).bits.id := i.U
      bitmap_arb.get.io.in(i).bits.level := 0.U // last level
      bitmap_arb.get.io.in(i).bits.way_info := Mux(entries(i).from_l0, entries(i).way_info, way_info.get(i))
      bitmap_arb.get.io.in(i).bits.hptw_bypassed := false.B
      bitmap_arb.get.io.in(i).bits.n := entries(i).n
      bitmap_arb.get.io.in(i).bits.s2xlate := entries(i).req_info.s2xlate
    }
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
  val dup_vec_bitmap = dup_vec.zipWithIndex.map{case (d, i) => d && (is_bitmap_req(i) || is_bitmap_resp(i))}
  val dup_vec_last_hptw = dup_vec.zipWithIndex.map{case (d, i) => d && (is_last_hptw_req(i) || is_last_hptw_resp(i))}
  val wait_id = Mux(dup_req_fire, mem_arb.io.chosen, ParallelMux(dup_vec_wait zip entries.map(_.wait_id)))
  val dup_wait_resp = io.mem.resp.fire && VecInit(dup_vec_wait)(io.mem.resp.bits.id) && !io.mem.flush_latch(io.mem.resp.bits.id) // dup with the entry that data coming next cycle
  val to_wait = Cat(dup_vec_wait).orR || dup_req_fire

  val last_hptw_req_id = io.mem.resp.bits.id
  val req_paddr = MakeAddr(io.in.bits.ppn(ppnLen-1, 0), getVpnn(io.in.bits.req_info.vpn, 0))
  val req_hpaddr = MakeAddr(entries(last_hptw_req_id).hptw_resp.genPPNS2(get_pn(req_paddr)), getVpnn(io.in.bits.req_info.vpn, 0))
  val index =  Mux(entries(last_hptw_req_id).req_info.s2xlate === allStage, req_hpaddr, req_paddr)(log2Up(l2tlbParams.blockBytes)-1, log2Up(XLEN/8))
  val last_hptw_req_pte = io.mem.resp.bits.value.asTypeOf(Vec(blockBits / XLEN, new PteBundle()))(index)
  val last_hptw_req_ppn = Mux(last_hptw_req_pte.n === 0.U, last_hptw_req_pte.getPPN(), Cat(last_hptw_req_pte.getPPN()(ptePPNLen - 1, pteNapotBits), io.in.bits.req_info.vpn(pteNapotBits - 1, 0)))
  // in `to_last_hptw_req`, we have already judged whether s2xlate === allStage
  val last_hptw_vsStagePf = last_hptw_req_pte.isPf(0.U, io.csr.hPBMTE) || !last_hptw_req_pte.isLeaf()
  val last_hptw_gStagePf = last_hptw_req_pte.isStage1Gpf(io.csr.hgatp.mode) && !last_hptw_vsStagePf

  // (noS2xlate || onlyStage1 || allStage) but exception; do not need bitmap check
  val mem_resp_enableS2xlate = entries(io.mem.resp.bits.id).req_info.s2xlate =/= noS2xlate
  val mem_resp_s1Pbmte = Mux(mem_resp_enableS2xlate, io.csr.hPBMTE, io.csr.mPBMTE)
  val mem_resp_Pf = last_hptw_req_pte.isPf(0.U, mem_resp_s1Pbmte) || !last_hptw_req_pte.isLeaf()
  val mem_resp_gStagePf = entries(io.mem.resp.bits.id).req_info.s2xlate === allStage && last_hptw_req_pte.isStage1Gpf(io.csr.hgatp.mode) && !mem_resp_Pf

  // noS2xlate || onlyStage1 || allStage but exception; do not need Stage2 translate
  val noStage2 = ((entries(io.mem.resp.bits.id).req_info.s2xlate === noS2xlate) || (entries(io.mem.resp.bits.id).req_info.s2xlate === onlyStage1)) ||
    (entries(io.mem.resp.bits.id).req_info.s2xlate === allStage && (last_hptw_vsStagePf || last_hptw_gStagePf))
  val to_mem_out = dup_wait_resp && noStage2 && (!bitmap_enable || mem_resp_Pf || mem_resp_gStagePf)
  val to_bitmap_req = (if (HasBitmapCheck) true.B else false.B) && dup_wait_resp && noStage2 && bitmap_enable && !(mem_resp_Pf || mem_resp_gStagePf)
  val to_cache = if (HasBitmapCheck) Cat(dup_vec_bitmap).orR || Cat(dup_vec_having).orR || Cat(dup_vec_last_hptw).orR
                 else Cat(dup_vec_having).orR || Cat(dup_vec_last_hptw).orR
  val to_hptw_req = io.in.bits.req_info.s2xlate === allStage
  val to_last_hptw_req = dup_wait_resp && entries(io.mem.resp.bits.id).req_info.s2xlate === allStage && !(last_hptw_vsStagePf || last_hptw_gStagePf)
  val last_hptw_excp = dup_wait_resp && entries(io.mem.resp.bits.id).req_info.s2xlate === allStage && (last_hptw_vsStagePf || last_hptw_gStagePf)

  XSError(RegNext(dup_req_fire && Cat(dup_vec_wait).orR, init = false.B), "mem req but some entries already waiting, should not happed")

  XSError(io.in.fire && ((to_mem_out && to_cache) || (to_wait && to_cache)), "llptw enq, to cache conflict with to mem")
  val mem_resp_hit = RegInit(VecInit(Seq.fill(l2tlbParams.llptwsize)(false.B)))
  val enq_state_normal = MuxCase(state_addr_check, Seq(
    to_mem_out -> state_mem_out, // same to the blew, but the mem resp now
    to_bitmap_req -> state_bitmap_check,
    to_last_hptw_req -> state_last_hptw_req,
    to_wait -> state_mem_waiting,
    to_cache -> state_cache,
    to_hptw_req -> state_hptw_req
  ))
  val enq_state = Mux(from_pre(io.in.bits.req_info.source) && enq_state_normal =/= state_addr_check, state_idle, enq_state_normal)
  when (io.in.fire && (if (HasBitmapCheck) !io.in.bits.bitmapCheck.get.jmp_bitmap_check else true.B)) {
    // if prefetch req does not need mem access, just give it up.
    // so there will be at most 1 + FilterSize entries that needs re-access page cache
    // so 2 + FilterSize is enough to avoid dead-lock
    state(enq_ptr) := enq_state
    entries(enq_ptr).req_info := io.in.bits.req_info
    entries(enq_ptr).ppn := Mux(to_bitmap_req || to_last_hptw_req || last_hptw_excp, last_hptw_req_ppn, io.in.bits.ppn)
    entries(enq_ptr).wait_id := Mux(to_wait, wait_id, enq_ptr)
    entries(enq_ptr).af := false.B
    if (HasBitmapCheck) {
      entries(enq_ptr).cf := false.B
      entries(enq_ptr).from_l0 := false.B
      entries(enq_ptr).way_info := 0.U
      entries(enq_ptr).jmp_bitmap_check := false.B
      for (i <- 0 until tlbcontiguous) {
        entries(enq_ptr).ptes(i) := 0.U
      }
      entries(enq_ptr).cfs := io.in.bits.bitmapCheck.get.cfs
      entries(enq_ptr).n := last_hptw_req_pte.n
    }
    entries(enq_ptr).hptw_resp := Mux(to_last_hptw_req, entries(last_hptw_req_id).hptw_resp, Mux(to_wait, entries(wait_id).hptw_resp, entries(enq_ptr).hptw_resp))
    entries(enq_ptr).hptw_resp.gpf := Mux(last_hptw_excp, last_hptw_gStagePf, false.B)
    entries(enq_ptr).first_s2xlate_fault := false.B
    mem_resp_hit(enq_ptr) := to_bitmap_req || to_mem_out || to_last_hptw_req
  }

  if (HasBitmapCheck) {
    when (io.in.bits.bitmapCheck.get.jmp_bitmap_check && io.in.fire) {
      state(enq_ptr) := state_bitmap_check
      entries(enq_ptr).req_info := io.in.bits.req_info
      entries(enq_ptr).ppn := io.in.bits.bitmapCheck.get.ptes(io.in.bits.req_info.vpn(sectortlbwidth - 1, 0)).asTypeOf(new PteBundle().cloneType).ppn
      entries(enq_ptr).wait_id := enq_ptr
      entries(enq_ptr).af := false.B
      entries(enq_ptr).cf := false.B
      entries(enq_ptr).from_l0 := true.B
      entries(enq_ptr).way_info := io.in.bits.bitmapCheck.get.hitway
      entries(enq_ptr).jmp_bitmap_check := io.in.bits.bitmapCheck.get.jmp_bitmap_check
      entries(enq_ptr).n := io.in.bits.bitmapCheck.get.ptes(io.in.bits.req_info.vpn(sectortlbwidth - 1, 0)).asTypeOf(new PteBundle().cloneType).n
      entries(enq_ptr).ptes := io.in.bits.bitmapCheck.get.ptes
      entries(enq_ptr).cfs := io.in.bits.bitmapCheck.get.cfs
      entries(enq_ptr).first_s2xlate_fault := false.B
      mem_resp_hit(enq_ptr) := false.B
    }
  }

  val enq_ptr_reg = RegNext(enq_ptr)
  val need_addr_check = GatedValidRegNext(enq_state === state_addr_check && io.in.fire && !flush && (if (HasBitmapCheck) !io.in.bits.bitmapCheck.get.jmp_bitmap_check else true.B))

  val hasHptwResp = ParallelOR(state.map(_ === state_hptw_resp)).asBool
  val hptw_resp_ptr_reg = RegNext(io.hptw.resp.bits.id)
  val hptw_need_addr_check = RegNext(hasHptwResp && io.hptw.resp.fire && !flush) && state(hptw_resp_ptr_reg) === state_addr_check

  val ptes = io.mem.resp.bits.value.asTypeOf(Vec(blockBits / XLEN, new PteBundle()))
  val gpaddr = MakeGPAddr(entries(io.hptw.resp.bits.id).ppn, getVpnn(entries(io.hptw.resp.bits.id).req_info.vpn, 0))
  val hptw_resp = io.hptw.resp.bits.h_resp
  val hpaddr = RegEnable(Cat(hptw_resp.genPPNS2(get_pn(gpaddr)), get_off(gpaddr)), io.hptw.resp.fire)
  val addr = RegEnable(MakeAddr(io.in.bits.ppn(ppnLen - 1, 0), getVpnn(io.in.bits.req_info.vpn, 0)), io.in.fire)

  when (hyper_arb1.io.out.fire) {
    for (i <- state.indices) {
      when (state(i) === state_hptw_req && entries(i).ppn === hyper_arb1.io.out.bits.ppn && entries(i).req_info.s2xlate === allStage && hyper_arb1.io.chosen === i.U) {
        state(i) := state_hptw_resp
        entries(i).wait_id := hyper_arb1.io.chosen
      }
    }
  }

  io.pmp(0).req.valid := need_addr_check
  io.pmp(0).req.bits.addr := addr
  io.pmp(0).req.bits.cmd := TlbCmd.read
  io.pmp(0).req.bits.size := 3.U // TODO: fix it
  when (io.pmp(0).req.valid) {  // same cycle
    val ptr = enq_ptr_reg
    val accessFault = io.pmp(0).resp.ld || io.pmp(0).resp.mmio
    entries(ptr).af := accessFault
    state(ptr) := Mux(accessFault, state_mem_out, state_mem_req)
  }

  io.pmp(1).req.valid := hptw_need_addr_check
  io.pmp(1).req.bits.addr := hpaddr
  io.pmp(1).req.bits.cmd := TlbCmd.read
  io.pmp(1).req.bits.size := 3.U // TODO: fix it
  when (io.pmp(1).req.valid) {  // same cycle
    val ptr = hptw_resp_ptr_reg
    val accessFault = io.pmp(1).resp.ld || io.pmp(1).resp.mmio
    entries(ptr).af := accessFault
    state(ptr) := Mux(accessFault, state_mem_out, state_mem_req)
  }

  when (mem_arb.io.out.fire) {
    for (i <- state.indices) {
      when (state(i) =/= state_idle && state(i) =/= state_mem_out && state(i) =/= state_last_hptw_req && state(i) =/= state_last_hptw_resp
      && (if (HasBitmapCheck) state(i) =/= state_bitmap_check && state(i) =/= state_bitmap_resp else true.B)
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
        val enableS2xlate = entries(i).req_info.s2xlate =/= noS2xlate
        val s1Pbmte = Mux(enableS2xlate, io.csr.hPBMTE, io.csr.mPBMTE)
        val vsStagePf = ptes(index).isPf(0.U, s1Pbmte) || !ptes(index).isLeaf() // Pagefault in vs-Stage
        // Pagefault in g-Stage; when vsStagePf valid, should not check gStagepf
        val gStagePf = ptes(index).isStage1Gpf(io.csr.hgatp.mode) && !vsStagePf
        state(i) := Mux(entries(i).req_info.s2xlate === allStage && !(vsStagePf || gStagePf),
                        state_last_hptw_req,
                        Mux(bitmap_enable && !(vsStagePf || (entries(i).req_info.s2xlate === allStage && gStagePf)), state_bitmap_check, state_mem_out))
        mem_resp_hit(i) := true.B
        entries(i).ppn := Mux(ptes(index).n === 0.U, ptes(index).getPPN(), Cat(ptes(index).getPPN()(ptePPNLen - 1, pteNapotBits), entries(i).req_info.vpn(pteNapotBits - 1, 0))) // for last stage 2 translation
        // af will be judged in L2 TLB `contiguous_pte_to_merge_ptwResp`
        entries(i).hptw_resp.gpf := Mux(entries(i).req_info.s2xlate === allStage, gStagePf, false.B)
        if (HasBitmapCheck) {
          entries(i).n := ptes(index).n
        }
      }
    }
  }

  if (HasBitmapCheck) {
    for (i <- 0 until l2tlbParams.llptwsize) {
      way_info.get(i) := DataHoldBypass(io.l0_way_info.get, mem_resp_hit(i))
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

  if (HasBitmapCheck) {
    when (bitmap_arb.get.io.out.fire) {
      for (i <- state.indices) {
        when (is_bitmap_req(i) && bitmap_arb.get.io.out.bits.bmppn === entries(i).ppn(ppnLen - 1, 0)) {
          state(i) := state_bitmap_resp
          entries(i).wait_id := bitmap_arb.get.io.chosen
        }
      }
    }
  }

  if (HasBitmapCheck) {
    when (io.bitmap.get.resp.fire) {
      for (i <- state.indices) {
        when (is_bitmap_resp(i) && io.bitmap.get.resp.bits.id === entries(i).wait_id) {
          entries(i).cfs := io.bitmap.get.resp.bits.cfs
          entries(i).cf := io.bitmap.get.resp.bits.cf
          state(i) := state_mem_out
        }
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
          entries(i).first_s2xlate_fault := io.hptw.resp.bits.h_resp.gaf || io.hptw.resp.bits.h_resp.gpf || check_g_perm_fail
        }.otherwise{ // change the entry that is waiting hptw resp
          val need_to_waiting_vec = state.indices.map(i => state(i) === state_mem_waiting &&
            dup(entries(i).req_info.vpn, entries(io.hptw.resp.bits.id).req_info.vpn) &&
            entries(i).req_info.s2xlate === entries(io.hptw.resp.bits.id).req_info.s2xlate)
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
  if (HasBitmapCheck) {
    io.out.bits.af := Mux(bitmap_enable, entries(mem_ptr).af || entries(mem_ptr).cf, entries(mem_ptr).af)
    io.out.bits.bitmapCheck.get.jmp_bitmap_check := entries(mem_ptr).jmp_bitmap_check
    io.out.bits.bitmapCheck.get.ptes := entries(mem_ptr).ptes
    io.out.bits.bitmapCheck.get.cfs := entries(mem_ptr).cfs
  } else {
    io.out.bits.af := entries(mem_ptr).af
  }

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

  val has_bitmap_resp = ParallelOR(is_bitmap_resp).asBool
  if (HasBitmapCheck) {
    io.bitmap.get.req.valid := bitmap_arb.get.io.out.valid && !flush
    io.bitmap.get.req.bits.bmppn := bitmap_arb.get.io.out.bits.bmppn
    io.bitmap.get.req.bits.id := bitmap_arb.get.io.chosen
    io.bitmap.get.req.bits.vpn := bitmap_arb.get.io.out.bits.vpn
    io.bitmap.get.req.bits.level := 0.U
    io.bitmap.get.req.bits.way_info := bitmap_arb.get.io.out.bits.way_info
    io.bitmap.get.req.bits.hptw_bypassed := bitmap_arb.get.io.out.bits.hptw_bypassed
    io.bitmap.get.req.bits.s2xlate := bitmap_arb.get.io.out.bits.s2xlate
    io.bitmap.get.req.bits.n := bitmap_arb.get.io.out.bits.n
    bitmap_arb.get.io.out.ready := io.bitmap.get.req.ready
    io.bitmap.get.resp.ready := has_bitmap_resp
  }

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
    val bitmapCheck = Option.when(HasBitmapCheck)(new Bundle {
      val jmp_bitmap_check = Bool() // find pte in l0 or sp, but need bitmap check
      val pte = UInt(XLEN.W) // Page Table Entry
      val ptes = Vec(tlbcontiguous, UInt(XLEN.W)) // Page Table Entry Vector
      val cfs = Vec(tlbcontiguous, Bool()) // Bitmap Check Failed Vector
      val hitway = UInt(l2tlbParams.l0nWays.W)
      val fromSP = Bool()
      val SPlevel = UInt(log2Up(Level).W)
    })
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
  val bitmap = Option.when(HasBitmapCheck)(new Bundle {
      val req = DecoupledIO(new bitmapReqBundle())
      val resp = Flipped(DecoupledIO(new bitmapRespBundle()))
  })

  val l0_way_info = Option.when(HasBitmapCheck)(Input(UInt(l2tlbParams.l0nWays.W)))
}

class HPTW()(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new HPTWIO)
  val hgatp = io.csr.hgatp
  val mpbmte = io.csr.mPBMTE
  val sfence = io.sfence
  val flush = sfence.valid || hgatp.changed || io.csr.satp.changed || io.csr.vsatp.changed || io.csr.priv.virt_changed
  val mode = hgatp.mode

  // mbmc:bitmap csr
  val mbmc = io.csr.mbmc
  val bitmap_enable = (if (HasBitmapCheck) true.B else false.B) && mbmc.BME === 1.U && mbmc.CMODE === 0.U

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
  val jmp_bitmap_check = if (HasBitmapCheck) RegEnable(io.req.bits.bitmapCheck.get.jmp_bitmap_check, io.req.fire) else false.B
  val fromSP = if (HasBitmapCheck) RegEnable(io.req.bits.bitmapCheck.get.fromSP, io.req.fire) else false.B
  val cache_pte = Option.when(HasBitmapCheck)(RegEnable(Mux(io.req.bits.bitmapCheck.get.fromSP, io.req.bits.bitmapCheck.get.pte.asTypeOf(new PteBundle().cloneType), io.req.bits.bitmapCheck.get.ptes(io.req.bits.gvpn(sectortlbwidth - 1, 0)).asTypeOf(new PteBundle().cloneType)), io.req.fire))
  val pte = if (HasBitmapCheck) Mux(jmp_bitmap_check, cache_pte.get, io.mem.resp.bits.asTypeOf(new PteBundle().cloneType)) else io.mem.resp.bits.asTypeOf(new PteBundle().cloneType)
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
  val s_bitmap_check = RegInit(true.B)
  val w_bitmap_resp = RegInit(true.B)
  val whether_need_bitmap_check = RegInit(false.B)
  val bitmap_checkfailed = RegInit(false.B)

  val sent_to_pmp = !idle && (!s_pmp_check || mem_addr_update) && !finish
  val pageFault = pte.isGpf(level, mpbmte) || (!pte.isLeaf() && level === 0.U)
  val accessFault = RegEnable(io.pmp.resp.ld || io.pmp.resp.mmio, sent_to_pmp)

  // use access fault when bitmap check failed
  val ppn_af = if (HasBitmapCheck) {
    Mux(bitmap_enable, pte.isAf() || bitmap_checkfailed, pte.isAf())
  } else {
    pte.isAf()
  }
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

  if (HasBitmapCheck) {
    val way_info = DataHoldBypass(io.l0_way_info.get, RegNext(io.mem.resp.fire, init=false.B))
    val cache_hitway = RegEnable(io.req.bits.bitmapCheck.get.hitway, io.req.fire)
    val cache_level = RegEnable(io.req.bits.bitmapCheck.get.SPlevel, io.req.fire)
    io.bitmap.get.req.valid := !s_bitmap_check
    io.bitmap.get.req.bits.bmppn := pte.ppn
    io.bitmap.get.req.bits.id := HptwReqId.U(bMemID.W)
    io.bitmap.get.req.bits.vpn := vpn
    io.bitmap.get.req.bits.level := Mux(jmp_bitmap_check, Mux(fromSP,cache_level,0.U), level)
    io.bitmap.get.req.bits.way_info := Mux(jmp_bitmap_check, cache_hitway, way_info)
    io.bitmap.get.req.bits.hptw_bypassed := bypassed
    io.bitmap.get.req.bits.n := pte.n
    io.bitmap.get.req.bits.s2xlate := onlyStage2
    io.bitmap.get.resp.ready := !w_bitmap_resp
  }

  io.mem.req.valid := !s_mem_req && !io.mem.mask && !accessFault && s_pmp_check
  io.mem.req.bits.addr := mem_addr
  io.mem.req.bits.id := HptwReqId.U(bMemID.W)
  io.mem.req.bits.hptw_bypassed := bypassed

  io.refill.req_info.vpn := vpn
  io.refill.level := level
  io.refill.req_info.source := source
  io.refill.req_info.s2xlate := onlyStage2

  when (idle){
    if (HasBitmapCheck) {
      when (io.req.bits.bitmapCheck.get.jmp_bitmap_check && io.req.fire) {
        idle := false.B
        gpaddr := Cat(io.req.bits.gvpn, 0.U(offLen.W))
        s_bitmap_check := false.B
        id := io.req.bits.id
        level := Mux(io.req.bits.bitmapCheck.get.fromSP, io.req.bits.bitmapCheck.get.SPlevel, 0.U)
      }
    }
    when (io.req.fire && (if (HasBitmapCheck) !io.req.bits.bitmapCheck.get.jmp_bitmap_check else true.B)) {
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
    if (HasBitmapCheck) {
      s_bitmap_check := true.B
      w_bitmap_resp := true.B
      whether_need_bitmap_check := false.B
      bitmap_checkfailed := false.B
    }
  }

  when(io.mem.req.fire){
    s_mem_req := true.B
    w_mem_resp := false.B
  }

  when(io.mem.resp.fire && !w_mem_resp){
    w_mem_resp := true.B
    af_level := af_level - 1.U
    if (HasBitmapCheck) {
      when (bitmap_enable) {
        whether_need_bitmap_check := true.B
      } .otherwise {
        mem_addr_update := true.B
        whether_need_bitmap_check := false.B
      }
    } else {
      mem_addr_update := true.B
    }
  }

  if (HasBitmapCheck) {
    when (whether_need_bitmap_check) {
      when (bitmap_enable && pte.isLeaf() && !pageFault) {
        s_bitmap_check := false.B
        whether_need_bitmap_check := false.B
      } .otherwise {
        mem_addr_update := true.B
        whether_need_bitmap_check := false.B
      }
    }
    // bitmapcheck
    when (io.bitmap.get.req.fire) {
      s_bitmap_check := true.B
      w_bitmap_resp := false.B
    }
    when (io.bitmap.get.resp.fire) {
      w_bitmap_resp := true.B
      mem_addr_update := true.B
      bitmap_checkfailed := io.bitmap.get.resp.bits.cf
    }
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
        if (HasBitmapCheck) {
          bitmap_checkfailed := false.B
        }
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
    if (HasBitmapCheck) {
      s_bitmap_check := true.B
      w_bitmap_resp := true.B
      whether_need_bitmap_check := false.B
      bitmap_checkfailed := false.B
    }
  }
}
