/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2

* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] Binh Pham, Viswanathan Vaidyanathan, Aamer Jaleel, and Abhishek Bhattacharjee. "[Colt: Coalesced large-reach
* tlbs.](https://doi.org/10.1109/MICRO.2012.32)" 45th Annual IEEE/ACM International Symposium on Microarchitecture
* (MICRO). 2012.
***************************************************************************************/

package xiangshan.cache.mmu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import xiangshan._
import utils._
import utility._
import xiangshan.backend.fu.{PMPChecker, PMPReqBundle, PMPConfig => XSPMPConfig}
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.fu.util.HasCSRConst
import freechips.rocketchip.rocket.PMPConfig

/** TLB module
  * support block request and non-block request io at the same time
  * return paddr at next cycle, then go for pmp/pma check
  * @param Width: The number of requestors
  * @param Block: Blocked or not for each requestor ports
  * @param q: TLB Parameters, like entry number, each TLB has its own parameters
  * @param p: XiangShan Paramemters, like XLEN
  */

class TLB(Width: Int, nRespDups: Int = 1, Block: Seq[Boolean], q: TLBParameters)(implicit p: Parameters) extends TlbModule
  with HasCSRConst
  with HasPerfEvents
{
  val io = IO(new TlbIO(Width, nRespDups, q))

  val req = io.requestor.map(_.req)
  val resp = io.requestor.map(_.resp)
  val ptw = io.ptw
  val pmp = io.pmp
  val refill_to_mem = io.refill_to_mem

  /** Sfence.vma & Svinval
    * Sfence.vma will 1. flush old entries 2. flush inflight 3. flush pipe
    * Svinval will 1. flush old entries 2. flush inflight
    * So, Svinval will not flush pipe, which means
    * it should not drop reqs from pipe and should return right resp
    */
  val sfence = DelayN(io.sfence, q.fenceDelay)
  val csr = DelayN(io.csr, q.fenceDelay)

  val flush_mmu = sfence.valid || csr.satp.changed || csr.vsatp.changed || csr.hgatp.changed || csr.priv.virt_changed
  val mmu_flush_pipe = sfence.valid && sfence.bits.flushPipe // for svinval, won't flush pipe
  val flush_pipe = io.flushPipe
  val redirect = io.redirect
  val EffectiveVa = Wire(Vec(Width, UInt(XLEN.W)))
  val req_in = req
  val req_out = Reg(Vec(Width, new TlbReq))
  for (i <- 0 until Width) {
    when (req(i).fire) {
      req_out(i) := req(i).bits
      req_out(i).fullva := EffectiveVa(i)
    }
  }
  val req_out_v = (0 until Width).map(i => ValidHold(req_in(i).fire && !req_in(i).bits.kill, resp(i).fire, flush_pipe(i)))

  val isHyperInst = (0 until Width).map(i => req_out_v(i) && req_out(i).hyperinst)

  // ATTENTION: csr and flush from backend are delayed. csr should not be later than flush.
  // because, csr will influence tlb behavior.
  val ifetch = if (q.fetchi) true.B else false.B
  val mode_tmp = if (q.useDmode) csr.priv.dmode else csr.priv.imode
  val mode = (0 until Width).map(i => Mux(isHyperInst(i), csr.priv.spvp, mode_tmp))
  val virt_in = csr.priv.virt
  val virt_out = req.map(a => RegEnable(csr.priv.virt, a.fire))
  val sum = (0 until Width).map(i => Mux(virt_out(i) || isHyperInst(i), csr.priv.vsum, csr.priv.sum))
  val mxr = (0 until Width).map(i => Mux(virt_out(i) || isHyperInst(i), csr.priv.vmxr || csr.priv.mxr, csr.priv.mxr))
  val req_in_s2xlate = (0 until Width).map(i => MuxCase(noS2xlate, Seq(
      (!(virt_in || req_in(i).bits.hyperinst)) -> noS2xlate,
      (csr.vsatp.mode =/= 0.U && csr.hgatp.mode =/= 0.U) -> allStage,
      (csr.vsatp.mode === 0.U) -> onlyStage2,
      (csr.hgatp.mode === 0.U) -> onlyStage1
    )))
  val req_out_s2xlate = (0 until Width).map(i => MuxCase(noS2xlate, Seq(
    (!(virt_out(i) || isHyperInst(i))) -> noS2xlate,
    (csr.vsatp.mode =/= 0.U && csr.hgatp.mode =/= 0.U) -> allStage,
    (csr.vsatp.mode === 0.U) -> onlyStage2,
    (csr.hgatp.mode === 0.U) -> onlyStage1
  )))
  val resp_s1_level = RegInit(0.U(log2Up(Level + 1).W))
  val resp_s1_isLeaf = RegInit(false.B)
  val resp_s1_isFakePte = RegInit(false.B)
  val hasGpf = Wire(Vec(Width, Bool()))

  val Sv39Enable = csr.satp.mode === 8.U
  val Sv48Enable = csr.satp.mode === 9.U
  val Sv39vsEnable = csr.vsatp.mode === 8.U
  val Sv48vsEnable = csr.vsatp.mode === 9.U
  val Sv39x4Enable = csr.hgatp.mode === 8.U
  val Sv48x4Enable = csr.hgatp.mode === 9.U

  val vmEnable = (0 until Width).map(i => !(isHyperInst(i) || virt_out(i)) && (
    if (EnbaleTlbDebug) (Sv39Enable || Sv48Enable)
    else (Sv39Enable || Sv48Enable) && (mode(i) < ModeM))
  )
  val s2xlateEnable = (0 until Width).map(i =>
    (isHyperInst(i) || virt_out(i)) &&
    (Sv39vsEnable || Sv48vsEnable || Sv39x4Enable || Sv48x4Enable) &&
    (mode(i) < ModeM)
  )

  val useReqS1Paddr = (0 until Width).map(i => RegNext(req(i).bits.no_translate))
  val privNeedTranslate = (0 until Width).map(i => (vmEnable(i) || s2xlateEnable(i)))
  val portTranslateEnable = (0 until Width).map(i => privNeedTranslate(i) && !useReqS1Paddr(i))

  // pre fault: check fault before real do translate
  val prepf = WireInit(VecInit(Seq.fill(Width)(false.B)))
  val pregpf = WireInit(VecInit(Seq.fill(Width)(false.B)))
  val preaf = WireInit(VecInit(Seq.fill(Width)(false.B)))
  val premode = (0 until Width).map(i => Mux(req_in(i).bits.hyperinst, csr.priv.spvp, mode_tmp))
  for (i <- 0 until Width) {
    resp(i).bits.fullva := RegEnable(EffectiveVa(i), req(i).valid)
  }
  val prevmEnable = (0 until Width).map(i => !(virt_in || req_in(i).bits.hyperinst) && (
    if (EnbaleTlbDebug) (Sv39Enable || Sv48Enable)
    else (Sv39Enable || Sv48Enable) && (premode(i) < ModeM))
  )
  val pres2xlateEnable = (0 until Width).map(i =>
    (virt_in || req_in(i).bits.hyperinst) &&
    (Sv39vsEnable || Sv48vsEnable || Sv39x4Enable || Sv48x4Enable) &&
    (premode(i) < ModeM)
  )

  (0 until Width).foreach{i =>

    val pmm = WireInit(0.U(2.W))

    when (ifetch || req(i).bits.hlvx) {
      pmm := 0.U
    } .elsewhen (premode(i) === ModeM) {
      pmm := csr.pmm.mseccfg
    } .elsewhen (Mux(virt_in || req_in(i).bits.hyperinst, csr.priv.vmxr || csr.priv.mxr, csr.priv.mxr)) {
      pmm := 0.U
    } .elsewhen (!(virt_in || req_in(i).bits.hyperinst) && premode(i) === ModeS) {
      pmm := csr.pmm.menvcfg
    } .elsewhen ((virt_in || req_in(i).bits.hyperinst) && premode(i) === ModeS) {
      pmm := csr.pmm.henvcfg
    } .elsewhen (req_in(i).bits.hyperinst && csr.priv.imode === ModeU) {
      pmm := csr.pmm.hstatus
    } .elsewhen (premode(i) === ModeU) {
      pmm := csr.pmm.senvcfg
    }

    when (prevmEnable(i) || (pres2xlateEnable(i) && csr.vsatp.mode =/= 0.U)) {
      when (pmm === PMLEN7) {
        EffectiveVa(i) := SignExt(req_in(i).bits.fullva(56, 0), XLEN)
      } .elsewhen (pmm === PMLEN16) {
        EffectiveVa(i) := SignExt(req_in(i).bits.fullva(47, 0), XLEN)
      } .otherwise {
        EffectiveVa(i) := req_in(i).bits.fullva
      }
    } .otherwise {
      when (pmm === PMLEN7) {
        EffectiveVa(i) := ZeroExt(req_in(i).bits.fullva(56, 0), XLEN)
      } .elsewhen (pmm === PMLEN16) {
        EffectiveVa(i) := ZeroExt(req_in(i).bits.fullva(47, 0), XLEN)
      } .otherwise {
        EffectiveVa(i) := req_in(i).bits.fullva
      }
    }

    val pf48 = SignExt(EffectiveVa(i)(47, 0), XLEN) =/= EffectiveVa(i)
    val pf39 = SignExt(EffectiveVa(i)(38, 0), XLEN) =/= EffectiveVa(i)
    val gpf48 = EffectiveVa(i)(XLEN - 1, 48 + 2) =/= 0.U
    val gpf39 = EffectiveVa(i)(XLEN - 1, 39 + 2) =/= 0.U
    val af = EffectiveVa(i)(XLEN - 1, PAddrBits) =/= 0.U
    when (req(i).valid && req(i).bits.checkfullva) {
      when (prevmEnable(i) || pres2xlateEnable(i)) {
        when (req_in_s2xlate(i) === onlyStage2) {
          when (Sv48x4Enable) {
            pregpf(i) := gpf48
          } .elsewhen (Sv39x4Enable) {
            pregpf(i) := gpf39
          }
        } .elsewhen (req_in_s2xlate(i) === onlyStage1 || req_in_s2xlate(i) === allStage) {
          when (Sv48vsEnable) {
            prepf(i) := pf48
          } .elsewhen (Sv39vsEnable) {
            prepf(i) := pf39
          }
        } .otherwise { // noS2xlate
          when (Sv48Enable) {
            prepf(i) := pf48
          } .elsewhen (Sv39Enable) {
            prepf(i) := pf39
          }
        }
      } .otherwise {
        preaf(i) := af
      }
    }
  }

  val refill = ptw.resp.fire && !flush_mmu

  refill_to_mem := DontCare
  val entries = Module(new TlbStorageWrapper(Width, q, nRespDups))
  entries.io.base_connect(sfence, csr, csr.satp)
  if (q.outReplace) { io.replace <> entries.io.replace }
  for (i <- 0 until Width) {
    entries.io.r_req_apply(io.requestor(i).req.valid, get_pn(req_in(i).bits.vaddr), i, req_in_s2xlate(i))
    entries.io.w_apply(refill, ptw.resp.bits, io.ptw_replenish)
    // TODO: RegNext enable:req.valid
    resp(i).bits.debug.isFirstIssue := RegEnable(req(i).bits.debug.isFirstIssue, req(i).valid)
    resp(i).bits.debug.robIdx := RegEnable(req(i).bits.debug.robIdx, req(i).valid)
  }

  // read TLB, get hit/miss, paddr, perm bits
  val readResult = (0 until Width).map(TLBRead(_))
  val hitVec = readResult.map(_._1)
  val missVec = readResult.map(_._2)
  val pmp_addr = readResult.map(_._3)
  val perm = readResult.map(_._4)
  val g_perm = readResult.map(_._5)
  val pbmt = readResult.map(_._6)
  val g_pbmt = readResult.map(_._7)
  val pm_check = readResult.map(_._8)
  // check pmp use paddr (for timing optization, use pmp_addr here)
  // check permisson
  (0 until Width).foreach{i =>
    val noTranslateReg = RegNext(req(i).bits.no_translate)
    val addr = Mux(noTranslateReg, req(i).bits.pmp_addr, pmp_addr(i))
    pmp_check(addr, req_out(i).size, req_out(i).cmd, noTranslateReg, i)
    for (d <- 0 until nRespDups) {
      pbmt_check(i, d, pbmt(i)(d), g_pbmt(i)(d), req_out_s2xlate(i))
      perm_check(perm(i)(d), req_out(i).cmd, i, d, g_perm(i)(d), req_out(i).hlvx, req_out_s2xlate(i), prepf(i), pregpf(i), preaf(i))
    }
    hasGpf(i) := hitVec(i) && (resp(i).bits.excp(0).gpf.ld || resp(i).bits.excp(0).gpf.st || resp(i).bits.excp(0).gpf.instr)
  }

  // handle block or non-block io
  // for non-block io, just return the above result, send miss to ptw
  // for block io, hold the request, send miss to ptw,
  //   when ptw back, return the result
  (0 until Width) foreach {i =>
    handle_nonblock(i)
  }
  io.ptw.resp.ready := true.B

  /************************  main body above | method/log/perf below ****************************/
  def TLBRead(i: Int) = {
    val (e_hit, e_ppn, e_gvpns, e_perm, e_g_perm, e_s2xlate, e_pbmt, e_g_pbmt, e_pm) = entries.io.r_resp_apply(i)
    val (p_hit, p_ppn, p_pbmt, p_perm, p_gvpn, p_g_pbmt, p_g_perm, p_s2xlate, p_s1_level, p_s1_isLeaf, p_s1_isFakePte, p_hit_fast, p_pm) = ptw_resp_bypass(get_pn(req_in(i).bits.vaddr), req_in_s2xlate(i))
    val enable = portTranslateEnable(i)
    val isitlb = TlbCmd.isExec(req_out(i).cmd)

    val hit = e_hit || p_hit
    val miss = !hit && enable
    hit.suggestName(s"hit_read_${i}")
    miss.suggestName(s"miss_read_${i}")

    val vaddr = SignExt(req_out(i).vaddr, PAddrBits)
    val notTranslatePaddr = Mux(useReqS1Paddr(i), req_in(i).bits.pmp_addr, SignExt(req_out(i).vaddr, PAddrBits))
    resp(i).bits.miss := miss
    resp(i).bits.ptwBack := ptw.resp.fire
    resp(i).bits.memidx := RegEnable(req_in(i).bits.memidx, req_in(i).valid)
    resp(i).bits.fastMiss := !hit && enable

    val ppn = WireInit(VecInit(Seq.fill(nRespDups)(0.U(ppnLen.W))))
    val pbmt = WireInit(VecInit(Seq.fill(nRespDups)(0.U(ptePbmtLen.W))))
    val perm = WireInit(VecInit(Seq.fill(nRespDups)(0.U.asTypeOf(new TlbPermBundle))))
    val gvpn = WireInit(VecInit(Seq.fill(nRespDups)(0.U(ptePPNLen.W))))
    val level = WireInit(VecInit(Seq.fill(nRespDups)(0.U(log2Up(Level + 1).W))))
    val isLeaf = WireInit(VecInit(Seq.fill(nRespDups)(false.B)))
    val isFakePte = WireInit(VecInit(Seq.fill(nRespDups)(false.B)))
    val g_pbmt = WireInit(VecInit(Seq.fill(nRespDups)(0.U(ptePbmtLen.W))))
    val g_perm = WireInit(VecInit(Seq.fill(nRespDups)(0.U.asTypeOf(new TlbPermBundle))))
    val r_s2xlate = WireInit(VecInit(Seq.fill(nRespDups)(0.U(2.W))))
    val pm = Mux(p_hit, p_pm, e_pm)

    for (d <- 0 until nRespDups) {
      val e_s1_isLeaf = (e_perm(d).r || e_perm(d).w || e_perm(d).x) && e_perm(d).v
      val e_s1_isFakePte = !e_perm(d).pf && !e_perm(d).v && !e_perm(d).af

      ppn(d) := Mux(p_hit, p_ppn, e_ppn(d))
      pbmt(d) := Mux(p_hit, p_pbmt, e_pbmt(d))
      perm(d) := Mux(p_hit, p_perm, e_perm(d))
      gvpn(d) :=  Mux(p_hit, p_gvpn, e_gvpns(d).gvpn)
      level(d) := Mux(p_hit, p_s1_level, e_gvpns(d).s1_level)
      isLeaf(d) := Mux(p_hit, p_s1_isLeaf, e_s1_isLeaf)
      isFakePte(d) := Mux(p_hit, p_s1_isFakePte, e_s1_isFakePte)
      g_pbmt(d) := Mux(p_hit, p_g_pbmt, e_g_pbmt(d))
      g_perm(d) := Mux(p_hit, p_g_perm, e_g_perm(d))
      r_s2xlate(d) := Mux(p_hit, p_s2xlate, e_s2xlate(d))
      val paddr = Cat(ppn(d), get_off(req_out(i).vaddr))
      val vpn_idx = Mux1H(Seq(
        (isFakePte(d) && csr.vsatp.mode === Sv39) -> 2.U,
        (isFakePte(d) && csr.vsatp.mode === Sv48) -> 3.U,
        (!isFakePte(d)) -> (level(d) - 1.U),
      ))
      // We use `fullva` here when `isLeaf`, in order to cope with the situation of an unaligned load/store cross page
      // for example, a `ld` instruction on address 0x81000ffb will be splited into two loads
      // 1. ld 0x81000ff8. vaddr = 0x81000ff8, fullva = 0x80000ffb
      // 2. ld 0x81001000. vaddr = 0x81001000, fullva = 0x80000ffb
      // When load 1 trigger a guest page fault, we should use offset of fullva when generate gpaddr
      // and when load 2 trigger a guest page fault, we should just use offset of vaddr(all zero).
      // Also, when onlyS2, if crosspage, gpaddr = vaddr(start address of a new page), else gpaddr = fullva(original vaddr)
      // By the way, frontend handles the cross page instruction fetch by itself, so TLB doesn't need to do anything extra.
      // Also, the fullva of iTLB is not used and always zero. crossPageVaddr should never use fullva in iTLB.
      val crossPageVaddr = Mux(isitlb || req_out(i).fullva(12) =/= vaddr(12), req_out(i).vaddr, req_out(i).fullva)
      val gpaddr_offset = Mux(isLeaf(d), get_off(crossPageVaddr), Cat(getVpnn(get_pn(crossPageVaddr), vpn_idx), 0.U(log2Up(XLEN/8).W)))
      val gpaddr = Cat(gvpn(d), gpaddr_offset)
      resp(i).bits.paddr(d) := Mux(enable, paddr, notTranslatePaddr)
      resp(i).bits.gpaddr(d) := Mux(r_s2xlate(d) === onlyStage2, crossPageVaddr, gpaddr)
    }

    XSDebug(req_out_v(i), p"(${i.U}) hit:${hit} miss:${miss} ppn:${Hexadecimal(ppn(0))} perm:${perm(0)}\n")

    val pmp_paddr = resp(i).bits.paddr(0)

    (hit, miss, pmp_paddr, perm, g_perm, pbmt, g_pbmt, pm)
  }

  def getVpnn(vpn: UInt, idx: UInt): UInt = {
    MuxLookup(idx, 0.U)(Seq(
      0.U -> vpn(vpnnLen - 1, 0),
      1.U -> vpn(vpnnLen * 2 - 1, vpnnLen),
      2.U -> vpn(vpnnLen * 3 - 1, vpnnLen * 2),
      3.U -> vpn(vpnnLen * 4 - 1, vpnnLen * 3))
    )
  }

  def pmp_check(addr: UInt, size: UInt, cmd: UInt, noTranslate: Bool, idx: Int): Unit = {
    pmp(idx).valid := resp(idx).valid || noTranslate
    pmp(idx).bits.addr := addr
    pmp(idx).bits.size := size
    pmp(idx).bits.cmd := cmd
  }

  def pbmt_check(idx: Int, d: Int, pbmt: UInt, g_pbmt: UInt, s2xlate: UInt):Unit = {
    val onlyS1 = s2xlate === onlyStage1 || s2xlate === noS2xlate
    val pbmtRes = pbmt
    val gpbmtRes = g_pbmt
    val res = MuxLookup(s2xlate, 0.U)(Seq(
      onlyStage1 -> pbmtRes,
      onlyStage2 -> gpbmtRes,
      allStage -> Mux(pbmtRes =/= 0.U, pbmtRes, gpbmtRes),
      noS2xlate -> pbmtRes
    ))
    resp(idx).bits.pbmt(d) := Mux(portTranslateEnable(idx), res, 0.U)
  }

  // for timing optimization, pmp check is divided into dynamic and static
  def perm_check(perm: TlbPermBundle, cmd: UInt, idx: Int, nDups: Int, g_perm: TlbPermBundle, hlvx: Bool, s2xlate: UInt, prepf: Bool = false.B, pregpf: Bool = false.B, preaf: Bool = false.B) = {
    // dynamic: superpage (or full-connected reg entries) -> check pmp when translation done
    // static: 4K pages (or sram entries) -> check pmp with pre-checked results
    val hasS2xlate = s2xlate =/= noS2xlate
    val onlyS1 = s2xlate === onlyStage1
    val onlyS2 = s2xlate === onlyStage2
    val allS2xlate = s2xlate === allStage
    // noS2xlate || onlyS1 -> perm.af
    // onlyS2 -> g_perm.af
    // allS2xlate -> perm.af || g_perm.af
    val af = (!onlyS2 && perm.af) || ((onlyS2 || allS2xlate) && g_perm.af)

    // Stage 1 perm check
    val pf = perm.pf
    val isLd = TlbCmd.isRead(cmd) && !TlbCmd.isAmo(cmd)
    val isSt = TlbCmd.isWrite(cmd) || TlbCmd.isAmo(cmd)
    val isInst = TlbCmd.isExec(cmd)
    val ldUpdate = !perm.a && isLd // update A/D through exception
    val stUpdate = (!perm.a || !perm.d) && isSt // update A/D through exception
    val instrUpdate = !perm.a && isInst // update A/D through exception
    val modeCheck = !(mode(idx) === ModeU && !perm.u || mode(idx) === ModeS && perm.u && (!sum(idx) || ifetch))
    val ldPermFail = !(modeCheck && Mux(hlvx, perm.x, perm.r || mxr(idx) && perm.x))
    val stPermFail = !(modeCheck && perm.w)
    val instrPermFail = !(modeCheck && perm.x)
    val ldPf = (ldPermFail || pf) && isLd
    val stPf = (stPermFail || pf) && isSt
    val instrPf = (instrPermFail || pf) && isInst
    val isFakePte = !perm.v && !perm.pf && !perm.af && !onlyS2
    val isNonLeaf = !(perm.r || perm.w || perm.x) && perm.v && !perm.pf && !perm.af
    val s1_valid = portTranslateEnable(idx) && !onlyS2

    // Stage 2 perm check
    val gpf = g_perm.pf
    val g_ldUpdate = !g_perm.a && isLd
    val g_stUpdate = (!g_perm.a || !g_perm.d) && isSt
    val g_instrUpdate = !g_perm.a && isInst
    val g_ldPermFail = !Mux(hlvx, g_perm.x, (g_perm.r || csr.priv.mxr && g_perm.x))
    val g_stPermFail = !g_perm.w
    val g_instrPermFail = !g_perm.x
    val ldGpf = (g_ldPermFail || gpf) && isLd
    val stGpf = (g_stPermFail || gpf) && isSt
    val instrGpf = (g_instrPermFail || gpf) && isInst
    val s2_valid = portTranslateEnable(idx) && (onlyS2 || allS2xlate)

    val fault_valid = s1_valid || s2_valid

    // when pf and gpf can't happens simultaneously
    val hasPf = (ldPf || ldUpdate || stPf || stUpdate || instrPf || instrUpdate) && s1_valid && !af && !isFakePte && !isNonLeaf
    // Only lsu need check related to high address truncation
    when (RegNext(prepf || pregpf || preaf)) {
      resp(idx).bits.isForVSnonLeafPTE := false.B
      resp(idx).bits.excp(nDups).pf.ld := RegNext(prepf) && isLd
      resp(idx).bits.excp(nDups).pf.st := RegNext(prepf) && isSt
      resp(idx).bits.excp(nDups).pf.instr := false.B

      resp(idx).bits.excp(nDups).gpf.ld := RegNext(pregpf) && isLd
      resp(idx).bits.excp(nDups).gpf.st := RegNext(pregpf) && isSt
      resp(idx).bits.excp(nDups).gpf.instr := false.B

      resp(idx).bits.excp(nDups).af.ld := RegNext(preaf) && TlbCmd.isRead(cmd)
      resp(idx).bits.excp(nDups).af.st := RegNext(preaf) && TlbCmd.isWrite(cmd)
      resp(idx).bits.excp(nDups).af.instr := false.B

      resp(idx).bits.excp(nDups).vaNeedExt := false.B

      // when pre-fault happens, should not use pma check results
      resp(idx).bits.pma(nDups).cacheable := false.B
      resp(idx).bits.pma(nDups).atomic := false.B

      // overwrite miss & gpaddr when exception related to high address truncation happens
      resp(idx).bits.miss := false.B
      resp(idx).bits.gpaddr(nDups) := req_out(idx).fullva
    } .otherwise {
      // isForVSnonLeafPTE is used only when gpf happens and it caused by a G-stage translation which supports VS-stage translation
      // it will be sent to CSR in order to modify the m/htinst.
      // Ref: The RISC-V Instruction Set Manual: Volume II: Privileged Architecture - 19.6.3. Transformed Instruction or Pseudoinstruction for mtinst or htinst
      val isForVSnonLeafPTE = isNonLeaf || isFakePte
      resp(idx).bits.isForVSnonLeafPTE := isForVSnonLeafPTE
      resp(idx).bits.excp(nDups).pf.ld := (ldPf || ldUpdate) && s1_valid && !af && !isFakePte && !isNonLeaf
      resp(idx).bits.excp(nDups).pf.st := (stPf || stUpdate) && s1_valid && !af && !isFakePte && !isNonLeaf
      resp(idx).bits.excp(nDups).pf.instr := (instrPf || instrUpdate) && s1_valid && !af && !isFakePte && !isNonLeaf
      // NOTE: pf need && with !af, page fault has higher priority than access fault
      // but ptw may also have access fault, then af happens, the translation is wrong.
      // In this case, pf has lower priority than af

      resp(idx).bits.excp(nDups).gpf.ld := (ldGpf || g_ldUpdate) && s2_valid && !af && !hasPf
      resp(idx).bits.excp(nDups).gpf.st := (stGpf || g_stUpdate) && s2_valid && !af && !hasPf
      resp(idx).bits.excp(nDups).gpf.instr := (instrGpf || g_instrUpdate) && s2_valid && !af && !hasPf

      val pf_gpf_ld = resp(idx).bits.excp(nDups).pf.ld || resp(idx).bits.excp(nDups).gpf.ld
      val pf_gpf_st = resp(idx).bits.excp(nDups).pf.st || resp(idx).bits.excp(nDups).gpf.st
      val pf_gpf_instr = resp(idx).bits.excp(nDups).pf.instr || resp(idx).bits.excp(nDups).gpf.instr

      val pm_valid = portTranslateEnable(idx)

      resp(idx).bits.excp(nDups).af.ld    := (af || pm_valid && !pm_check(idx).r && !pf_gpf_ld) && TlbCmd.isRead(cmd) && fault_valid
      resp(idx).bits.excp(nDups).af.st    := (af || pm_valid && !pm_check(idx).w && !pf_gpf_st) && TlbCmd.isWrite(cmd) && fault_valid
      resp(idx).bits.excp(nDups).af.instr := (af || pm_valid && !pm_check(idx).x && !pf_gpf_instr) && TlbCmd.isExec(cmd) && fault_valid

      resp(idx).bits.excp(nDups).vaNeedExt := true.B

      resp(idx).bits.pma(nDups).cacheable := pm_check(idx).c
      resp(idx).bits.pma(nDups).atomic := pm_check(idx).atomic
    }

    resp(idx).bits.excp(nDups).isHyper := isHyperInst(idx)
  }

  def handle_nonblock(idx: Int): Unit = {
    io.requestor(idx).resp.valid := req_out_v(idx)
    io.requestor(idx).req.ready := io.requestor(idx).resp.ready // should always be true
    XSError(!io.requestor(idx).resp.ready, s"${q.name} port ${idx} is non-block, resp.ready must be true.B")

    val req_s2xlate = Wire(UInt(2.W))
    req_s2xlate := MuxCase(noS2xlate, Seq(
      (!(virt_out(idx) || req_out(idx).hyperinst)) -> noS2xlate,
      (csr.vsatp.mode =/= 0.U && csr.hgatp.mode =/= 0.U) -> allStage,
      (csr.vsatp.mode === 0.U) -> onlyStage2,
      (csr.hgatp.mode === 0.U) -> onlyStage1
    ))

    val ptw_just_back = ptw.resp.fire && req_s2xlate === ptw.resp.bits.s2xlate && ptw.resp.bits.hit(get_pn(req_out(idx).vaddr), csr.satp.asid, csr.vsatp.asid, csr.hgatp.vmid, allType = true)
    // TODO: RegNext enable: ptw.resp.valid ? req.valid
    val ptw_resp_bits_reg = RegEnable(ptw.resp.bits, ptw.resp.valid)
    val ptw_already_back = GatedValidRegNext(ptw.resp.fire) && req_s2xlate === ptw_resp_bits_reg.s2xlate && ptw_resp_bits_reg.hit(get_pn(req_out(idx).vaddr), csr.satp.asid, csr.vsatp.asid, csr.hgatp.vmid, allType = true)

    io.ptw.req(idx).valid := false.B;
    io.tlbreplay(idx) := false.B;

    when (req_out_v(idx) && missVec(idx)) {
      // NOTE: for an miss tlb request: either send a ptw request, or ask for a replay
      when (ptw_just_back || ptw_already_back) {
        io.tlbreplay(idx) := true.B;
      } .otherwise {
        io.ptw.req(idx).valid := true.B;
      }
    }

    when (io.requestor(idx).req_kill && GatedValidRegNext(io.requestor(idx).req.fire)) {
      io.ptw.req(idx).valid := false.B
      io.tlbreplay(idx) := true.B
    }

    io.ptw.req(idx).bits.vpn := get_pn(req_out(idx).vaddr)
    io.ptw.req(idx).bits.s2xlate := req_s2xlate
    io.ptw.req(idx).bits.memidx := req_out(idx).memidx
  }

  // when ptw resp, tlb at refill_idx maybe set to miss by force.
  // Bypass ptw resp to check.
  def ptw_resp_bypass(vpn: UInt, s2xlate: UInt) = {
    // TODO: RegNext enable: ptw.resp.valid
    val hasS2xlate = s2xlate =/= noS2xlate
    val onlyS2 = s2xlate === onlyStage2
    val onlyS1 = s2xlate === onlyStage1
    val s2xlate_hit = s2xlate === ptw.resp.bits.s2xlate
    val resp_hit = ptw.resp.bits.hit(vpn, csr.satp.asid, csr.vsatp.asid, csr.hgatp.vmid, allType = true)
    val p_hit_fast = resp_hit && io.ptw.resp.fire && s2xlate_hit    // valid in the same cycle as tlb_req and ptw_resp
    val p_hit = GatedValidRegNext(p_hit_fast)                       // valid in the next cycle after tlb_req and ptw_resp
    val ppn_s1 = ptw.resp.bits.s1.genPPN(vpn)(ppnLen - 1, 0)
    val gvpn = Mux(onlyS2, vpn, ppn_s1)
    val ppn_s2 = ptw.resp.bits.s2.genPPNS2(gvpn)(ppnLen - 1, 0)
    val p_ppn = RegEnable(Mux(s2xlate === onlyStage2 || s2xlate === allStage, ppn_s2, ppn_s1), io.ptw.resp.fire)
    val p_pbmt = RegEnable(ptw.resp.bits.s1.entry.pbmt,io.ptw.resp.fire)
    val p_perm = RegEnable(ptwresp_to_tlbperm(ptw.resp.bits.s1), io.ptw.resp.fire)
    val p_gvpn = RegEnable(Mux(onlyS2, ptw.resp.bits.s2.entry.tag, ptw.resp.bits.s1.genGVPN(vpn)), io.ptw.resp.fire)
    val p_g_pbmt = RegEnable(ptw.resp.bits.s2.entry.pbmt,io.ptw.resp.fire)
    val p_g_perm = RegEnable(hptwresp_to_tlbperm(ptw.resp.bits.s2), io.ptw.resp.fire)
    val p_s2xlate = RegEnable(ptw.resp.bits.s2xlate, io.ptw.resp.fire)
    val p_s1_level = RegEnable(ptw.resp.bits.s1.entry.level.get, io.ptw.resp.fire)
    val p_s1_isLeaf = RegEnable(ptw.resp.bits.s1.isLeaf(), io.ptw.resp.fire)
    val p_s1_isFakePte = RegEnable(ptw.resp.bits.s1.isFakePte(), io.ptw.resp.fire)

    // do static pmp & pma check
    val hitVec_pm = io.ptw_replenish.map(_.hit)
    val selected_pm = ParallelPriorityMux(hitVec_pm, io.ptw_replenish.map(_.cfg))
    val assigned_pm = Wire(new TlbPMBundle)
    assigned_pm.assign_ap(selected_pm)

    val p_pm = RegEnable(assigned_pm, io.ptw.resp.fire)

    (p_hit, p_ppn, p_pbmt, p_perm, p_gvpn, p_g_pbmt, p_g_perm, p_s2xlate, p_s1_level, p_s1_isLeaf, p_s1_isFakePte, p_hit_fast, p_pm)
  }

  // perf event
  val result_ok = req_in.map(a => GatedValidRegNext(a.fire))
  val perfEvents =
    Seq(
      ("access", PopCount((0 until Width).map{i => if (Block(i)) io.requestor(i).req.fire else portTranslateEnable(i) && result_ok(i) })),
      ("miss  ", PopCount((0 until Width).map{i => if (Block(i)) portTranslateEnable(i) && result_ok(i) && missVec(i) else ptw.req(i).fire })),
    )
  generatePerfEvent()

  // perf log
  for (i <- 0 until Width) {
    if (Block(i)) {
      XSPerfAccumulate(s"access${i}",result_ok(i) && portTranslateEnable(i))
      XSPerfAccumulate(s"miss${i}", result_ok(i) && missVec(i))
    } else {
      XSPerfAccumulate("first_access" + Integer.toString(i, 10), result_ok(i) && portTranslateEnable(i) && RegEnable(req(i).bits.debug.isFirstIssue, req(i).valid))
      XSPerfAccumulate("access" + Integer.toString(i, 10), result_ok(i) && portTranslateEnable(i))
      XSPerfAccumulate("first_miss" + Integer.toString(i, 10), result_ok(i) && portTranslateEnable(i) && missVec(i) && RegEnable(req(i).bits.debug.isFirstIssue, req(i).valid))
      XSPerfAccumulate("miss" + Integer.toString(i, 10), result_ok(i) && portTranslateEnable(i) && missVec(i))
    }
  }
  XSPerfAccumulate("ptw_resp_count", ptw.resp.fire)
  XSPerfAccumulate("ptw_resp_pf_count", ptw.resp.fire && ptw.resp.bits.s1.pf)

  // Log
  for(i <- 0 until Width) {
    XSDebug(req(i).valid, p"req(${i.U}): (${req(i).valid} ${req(i).ready}) ${req(i).bits}\n")
    XSDebug(resp(i).valid, p"resp(${i.U}): (${resp(i).valid} ${resp(i).ready}) ${resp(i).bits}\n")
  }

  XSDebug(io.sfence.valid, p"Sfence: ${io.sfence}\n")
  XSDebug(ParallelOR(req_out_v) || ptw.resp.valid, p"vmEnable:${vmEnable} hit:${Binary(VecInit(hitVec).asUInt)} miss:${Binary(VecInit(missVec).asUInt)}\n")
  for (i <- ptw.req.indices) {
    XSDebug(ptw.req(i).fire, p"L2TLB req:${ptw.req(i).bits}\n")
  }
  XSDebug(ptw.resp.valid, p"L2TLB resp:${ptw.resp.bits} (v:${ptw.resp.valid}r:${ptw.resp.ready}) \n")

  println(s"${q.name}: page: ${q.NWays} ${q.Associative} ${q.Replacer.get}")

  if (env.EnableDifftest) {
    for (i <- 0 until Width) {
      val pf = io.requestor(i).resp.bits.excp(0).pf.instr || io.requestor(i).resp.bits.excp(0).pf.st || io.requestor(i).resp.bits.excp(0).pf.ld
      val gpf = io.requestor(i).resp.bits.excp(0).gpf.instr || io.requestor(i).resp.bits.excp(0).gpf.st || io.requestor(i).resp.bits.excp(0).gpf.ld
      val af = io.requestor(i).resp.bits.excp(0).af.instr || io.requestor(i).resp.bits.excp(0).af.st || io.requestor(i).resp.bits.excp(0).af.ld
      val difftest = DifftestModule(new DiffL1TLBEvent)
      difftest.coreid := io.hartId
      difftest.valid := RegNext(io.requestor(i).req.fire) && !io.requestor(i).req_kill && io.requestor(i).resp.fire && !io.requestor(i).resp.bits.miss && !pf && !af && !gpf && portTranslateEnable(i)
      if (!Seq("itlb", "ldtlb", "sttlb").contains(q.name)) {
        difftest.valid := false.B
      }
      difftest.index := TLBDiffId(p(XSCoreParamsKey).HartId).U
      difftest.vpn := RegEnable(get_pn(req_in(i).bits.vaddr), req_in(i).valid)
      difftest.ppn := get_pn(io.requestor(i).resp.bits.paddr(0))
      difftest.satp := Cat(csr.satp.mode, csr.satp.asid, csr.satp.ppn)
      difftest.vsatp := Cat(csr.vsatp.mode, csr.vsatp.asid, csr.vsatp.ppn)
      difftest.hgatp := Cat(csr.hgatp.mode, csr.hgatp.vmid, csr.hgatp.ppn)
      val req_need_gpa = gpf
      val req_s2xlate = Wire(UInt(2.W))
      req_s2xlate := MuxCase(noS2xlate, Seq(
        (!RegNext(virt_in || req_in(i).bits.hyperinst)) -> noS2xlate,
        (csr.vsatp.mode =/= 0.U && csr.hgatp.mode =/= 0.U) -> allStage,
        (csr.vsatp.mode === 0.U) -> onlyStage2,
        (csr.hgatp.mode === 0.U) -> onlyStage1
      ))
      difftest.s2xlate := req_s2xlate
    }
  }
}

object TLBDiffId {
  var i: Int = 0
  var lastHartId: Int = -1
  def apply(hartId: Int): Int = {
    if (lastHartId != hartId) {
      i = 0
      lastHartId = hartId
    }
    i += 1
    i - 1
  }
}

class TLBNonBlock(Width: Int, nRespDups: Int = 1, q: TLBParameters)(implicit p: Parameters) extends TLB(Width, nRespDups, Seq.fill(Width)(false), q)
class TLBBLock(Width: Int, nRespDups: Int = 1, q: TLBParameters)(implicit p: Parameters) extends TLB(Width, nRespDups, Seq.fill(Width)(true), q)

class TlbReplace(Width: Int, q: TLBParameters)(implicit p: Parameters) extends TlbModule {
  val io = IO(new TlbReplaceIO(Width, q))

  if (q.Associative == "fa") {
    val re = ReplacementPolicy.fromString(q.Replacer, q.NWays)
    re.access(io.page.access.map(_.touch_ways))
    io.page.refillIdx := re.way
  } else { // set-acco && plru
    val re = ReplacementPolicy.fromString(q.Replacer, q.NSets, q.NWays)
    re.access(io.page.access.map(_.sets), io.page.access.map(_.touch_ways))
    io.page.refillIdx := { if (q.NWays == 1) 0.U else re.way(io.page.chosen_set) }
  }
}
