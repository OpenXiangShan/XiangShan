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
***************************************************************************************/

package xiangshan.cache.mmu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.util.SRAMAnnotation
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
  val csr = io.csr
  val satp = DelayN(io.csr.satp, q.fenceDelay)
  val vsatp = DelayN(io.csr.vsatp, q.fenceDelay)
  val hgatp = DelayN(io.csr.hgatp, q.fenceDelay)

  val flush_mmu = DelayN(sfence.valid || csr.satp.changed || csr.vsatp.changed || csr.hgatp.changed, q.fenceDelay)
  val mmu_flush_pipe = DelayN(sfence.valid && sfence.bits.flushPipe, q.fenceDelay) // for svinval, won't flush pipe
  val flush_pipe = io.flushPipe
  val redirect = io.redirect
  val req_in = req
  val req_out = req.map(a => RegEnable(a.bits, a.fire))
  val req_out_v = (0 until Width).map(i => ValidHold(req_in(i).fire && !req_in(i).bits.kill, resp(i).fire, flush_pipe(i)))

  val isHyperInst = (0 until Width).map(i => req_out_v(i) && req_out(i).hyperinst)

  // ATTENTION: csr and flush from backend are delayed. csr should not be later than flush.
  // because, csr will influence tlb behavior.
  val ifecth = if (q.fetchi) true.B else false.B
  val mode_tmp = if (q.useDmode) csr.priv.dmode else csr.priv.imode
  val mode = (0 until Width).map(i => Mux(isHyperInst(i), csr.priv.spvp, mode_tmp))
  val virt_in = csr.priv.virt
  val virt_out = req.map(a => RegEnable(csr.priv.virt, a.fire))
  val sum = (0 until Width).map(i => Mux(virt_out(i) || isHyperInst(i), io.csr.priv.vsum, io.csr.priv.sum))
  val mxr = (0 until Width).map(i => Mux(virt_out(i) || isHyperInst(i), io.csr.priv.vmxr || io.csr.priv.mxr, io.csr.priv.mxr))
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
  val need_gpa = RegInit(false.B)
  val need_gpa_robidx = Reg(new RobPtr)
  val need_gpa_vpn = Reg(UInt(vpnLen.W))
  val resp_gpa_gvpn = Reg(UInt(ptePPNLen.W))
  val resp_gpa_refill = RegInit(false.B)
  val resp_s1_level = RegInit(0.U(log2Up(Level + 1).W))
  val resp_s1_isLeaf = RegInit(false.B)
  val resp_s1_isFakePte = RegInit(false.B)
  val hasGpf = Wire(Vec(Width, Bool()))

  val Sv39Enable = satp.mode === 8.U
  val Sv48Enable = satp.mode === 9.U
  val Sv39x4Enable = vsatp.mode === 8.U || hgatp.mode === 8.U
  val Sv48x4Enable = vsatp.mode === 9.U || hgatp.mode === 9.U
  val vmEnable = (0 until Width).map(i => !(isHyperInst(i) || virt_out(i)) && (
    if (EnbaleTlbDebug) (Sv39Enable || Sv48Enable)
    else (Sv39Enable || Sv48Enable) && (mode(i) < ModeM))
  )
  val s2xlateEnable = (0 until Width).map(i => (isHyperInst(i) || virt_out(i)) && (Sv39x4Enable || Sv48x4Enable) && (mode(i) < ModeM))
  val portTranslateEnable = (0 until Width).map(i => (vmEnable(i) || s2xlateEnable(i)) && RegEnable(!req(i).bits.no_translate, req(i).valid))


  val refill = ptw.resp.fire && !(ptw.resp.bits.getGpa) && !flush_mmu
  refill_to_mem := DontCare
  val entries = Module(new TlbStorageWrapper(Width, q, nRespDups))
  entries.io.base_connect(sfence, csr, satp)
  if (q.outReplace) { io.replace <> entries.io.replace }
  for (i <- 0 until Width) {
    entries.io.r_req_apply(io.requestor(i).req.valid, get_pn(req_in(i).bits.vaddr), i, req_in_s2xlate(i))
    entries.io.w_apply(refill, ptw.resp.bits)
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
  // check pmp use paddr (for timing optization, use pmp_addr here)
  // check permisson
  (0 until Width).foreach{i =>
    when (RegNext(req(i).bits.no_translate)) {
      pmp_check(req(i).bits.pmp_addr, req_out(i).size, req_out(i).cmd, i)
    } .otherwise {
      pmp_check(pmp_addr(i), req_out(i).size, req_out(i).cmd, i)
    }
    for (d <- 0 until nRespDups) {
      pbmt_check(i, d, pbmt(i)(d), g_pbmt(i)(d), req_out_s2xlate(i))
      perm_check(perm(i)(d), req_out(i).cmd, i, d, g_perm(i)(d), req_out(i).hlvx, req_out_s2xlate(i))
    }
    hasGpf(i) := resp(i).bits.excp(0).gpf.ld || resp(i).bits.excp(0).gpf.st || resp(i).bits.excp(0).gpf.instr
  }

  // handle block or non-block io
  // for non-block io, just return the above result, send miss to ptw
  // for block io, hold the request, send miss to ptw,
  //   when ptw back, return the result
  (0 until Width) foreach {i =>
    if (Block(i)) handle_block(i)
    else handle_nonblock(i)
  }
  io.ptw.resp.ready := true.B

  /************************  main body above | method/log/perf below ****************************/
  def TLBRead(i: Int) = {
    val (e_hit, e_ppn, e_perm, e_g_perm, e_s2xlate, e_pbmt, e_g_pbmt) = entries.io.r_resp_apply(i)
    val (p_hit, p_ppn, p_pbmt, p_perm, p_gvpn, p_g_pbmt, p_g_perm, p_s2xlate, p_s1_level, p_s1_isLeaf, p_s1_isFakePte) = ptw_resp_bypass(get_pn(req_in(i).bits.vaddr), req_in_s2xlate(i))
    val enable = portTranslateEnable(i)
    val isOnlys2xlate = req_out_s2xlate(i) === onlyStage2
    val need_gpa_vpn_hit = need_gpa_vpn === get_pn(req_out(i).vaddr)
    val isitlb = TlbCmd.isExec(req_out(i).cmd)

    when (!isitlb && need_gpa_robidx.needFlush(redirect) || isitlb && flush_pipe(i)){
      need_gpa := false.B
      resp_gpa_refill := false.B
      need_gpa_vpn := 0.U
    }.elsewhen (req_out_v(i) && !p_hit && !(resp_gpa_refill && need_gpa_vpn_hit) && !isOnlys2xlate && hasGpf(i) && need_gpa === false.B && !io.requestor(i).req_kill) {
      need_gpa := true.B
      need_gpa_vpn := get_pn(req_out(i).vaddr)
      resp_gpa_refill := false.B
      need_gpa_robidx := req_out(i).debug.robIdx
    }.elsewhen (ptw.resp.fire && need_gpa && need_gpa_vpn === ptw.resp.bits.getVpn(need_gpa_vpn)) {
      resp_gpa_gvpn := ptw.resp.bits.s1.genPPN(need_gpa_vpn)
      resp_s1_level := ptw.resp.bits.s1.entry.level.get
      resp_s1_isLeaf := ptw.resp.bits.s1.isLeaf()
      resp_s1_isFakePte := ptw.resp.bits.s1.isFakePte()
      resp_gpa_refill := true.B
    }

    when (req_out_v(i) && hasGpf(i) && resp_gpa_refill && need_gpa_vpn_hit ){
      need_gpa := false.B
    }

    TimeOutAssert(need_gpa && !resp_gpa_refill, timeOutThreshold, s"port${i} need gpa long time not refill.")

    val hit = e_hit || p_hit
    val miss = (!hit && enable) || hasGpf(i) && !p_hit && !(resp_gpa_refill && need_gpa_vpn_hit) && !isOnlys2xlate
    hit.suggestName(s"hit_read_${i}")
    miss.suggestName(s"miss_read_${i}")

    val vaddr = SignExt(req_out(i).vaddr, PAddrBits)
    resp(i).bits.miss := miss
    resp(i).bits.ptwBack := ptw.resp.fire
    resp(i).bits.memidx := RegEnable(req_in(i).bits.memidx, req_in(i).valid)

    val ppn = WireInit(VecInit(Seq.fill(nRespDups)(0.U(ppnLen.W))))
    val pbmt = WireInit(VecInit(Seq.fill(nRespDups)(0.U(ptePbmtLen.W))))
    val perm = WireInit(VecInit(Seq.fill(nRespDups)(0.U.asTypeOf(new TlbPermBundle))))
    val gvpn = WireInit(VecInit(Seq.fill(nRespDups)(0.U(vpnLen.W))))
    val level = WireInit(VecInit(Seq.fill(nRespDups)(0.U(log2Up(Level + 1).W))))
    val isLeaf = WireInit(VecInit(Seq.fill(nRespDups)(false.B)))
    val isFakePte = WireInit(VecInit(Seq.fill(nRespDups)(false.B)))
    val g_pbmt = WireInit(VecInit(Seq.fill(nRespDups)(0.U(ptePbmtLen.W))))
    val g_perm = WireInit(VecInit(Seq.fill(nRespDups)(0.U.asTypeOf(new TlbPermBundle))))
    val r_s2xlate = WireInit(VecInit(Seq.fill(nRespDups)(0.U(2.W))))
    for (d <- 0 until nRespDups) {
      ppn(d) := Mux(p_hit, p_ppn, e_ppn(d))
      pbmt(d) := Mux(p_hit, p_pbmt, e_pbmt(d))
      perm(d) := Mux(p_hit, p_perm, e_perm(d))
      gvpn(d) :=  Mux(p_hit, p_gvpn, resp_gpa_gvpn)
      level(d) := Mux(p_hit, p_s1_level, resp_s1_level)
      isLeaf(d) := Mux(p_hit, p_s1_isLeaf, resp_s1_isLeaf)
      isFakePte(d) := Mux(p_hit, p_s1_isFakePte, resp_s1_isFakePte)
      g_pbmt(d) := Mux(p_hit, p_g_pbmt, e_g_pbmt(d))
      g_perm(d) := Mux(p_hit, p_g_perm, e_g_perm(d))
      r_s2xlate(d) := Mux(p_hit, p_s2xlate, e_s2xlate(d))
      val paddr = Cat(ppn(d), get_off(req_out(i).vaddr))
      val vpn_idx = Mux1H(Seq(
        (isFakePte(d) && vsatp.mode === Sv39) -> 2.U,
        (isFakePte(d) && vsatp.mode === Sv48) -> 3.U,
        (!isFakePte(d)) -> (level(d) - 1.U),
      ))
      val gpaddr_offset = Mux(isLeaf(d), get_off(req_out(i).vaddr), Cat(getVpnn(get_pn(req_out(i).vaddr), vpn_idx),  0.U(log2Up(XLEN/8).W)))
      val gpaddr = Cat(gvpn(d), gpaddr_offset)
      resp(i).bits.paddr(d) := Mux(enable, paddr, vaddr)
      resp(i).bits.gpaddr(d) := Mux(r_s2xlate(d) === onlyStage2, vaddr, gpaddr)
    }

    XSDebug(req_out_v(i), p"(${i.U}) hit:${hit} miss:${miss} ppn:${Hexadecimal(ppn(0))} perm:${perm(0)}\n")

    val pmp_paddr = resp(i).bits.paddr(0)

    (hit, miss, pmp_paddr, perm, g_perm, pbmt, g_pbmt)
  }

  def getVpnn(vpn: UInt, idx: UInt): UInt = {
    MuxLookup(idx, 0.U)(Seq(
      0.U -> vpn(vpnnLen - 1, 0),
      1.U -> vpn(vpnnLen * 2 - 1, vpnnLen),
      2.U -> vpn(vpnnLen * 3 - 1, vpnnLen * 2),
      3.U -> vpn(vpnnLen * 4 - 1, vpnnLen * 3))
    )
  }

  def pmp_check(addr: UInt, size: UInt, cmd: UInt, idx: Int): Unit = {
    pmp(idx).valid := resp(idx).valid
    pmp(idx).bits.addr := addr
    pmp(idx).bits.size := size
    pmp(idx).bits.cmd := cmd
  }

  def pbmt_check(idx: Int, d: Int, pbmt: UInt, g_pbmt: UInt, s2xlate: UInt):Unit = {
    val onlyS1 = s2xlate === onlyStage1 || s2xlate === noS2xlate
    resp(idx).bits.pbmt(d) := Mux(
      portTranslateEnable(idx),
      Mux(onlyS1, pbmt, g_pbmt),
      0.U
    )
  }

  // for timing optimization, pmp check is divided into dynamic and static
  def perm_check(perm: TlbPermBundle, cmd: UInt, idx: Int, nDups: Int, g_perm: TlbPermBundle, hlvx: Bool, s2xlate: UInt) = {
    // dynamic: superpage (or full-connected reg entries) -> check pmp when translation done
    // static: 4K pages (or sram entries) -> check pmp with pre-checked results
    val hasS2xlate = s2xlate =/= noS2xlate
    val onlyS1 = s2xlate === onlyStage1
    val onlyS2 = s2xlate === onlyStage2
    val af = perm.af || (hasS2xlate && g_perm.af)

    // Stage 1 perm check
    val pf = perm.pf
    val ldUpdate = !perm.a && TlbCmd.isRead(cmd) && !TlbCmd.isAmo(cmd) // update A/D through exception
    val stUpdate = (!perm.a || !perm.d) && (TlbCmd.isWrite(cmd) || TlbCmd.isAmo(cmd)) // update A/D through exception
    val instrUpdate = !perm.a && TlbCmd.isExec(cmd) // update A/D through exception
    val modeCheck = !(mode(idx) === ModeU && !perm.u || mode(idx) === ModeS && perm.u && (!sum(idx) || ifecth))
    val ldPermFail = !(modeCheck && Mux(hlvx, perm.x, perm.r || mxr(idx) && perm.x))
    val stPermFail = !(modeCheck && perm.w)
    val instrPermFail = !(modeCheck && perm.x)
    val ldPf = (ldPermFail || pf) && (TlbCmd.isRead(cmd) && !TlbCmd.isAmo(cmd))
    val stPf = (stPermFail || pf) && (TlbCmd.isWrite(cmd) || TlbCmd.isAmo(cmd))
    val instrPf = (instrPermFail || pf) && TlbCmd.isExec(cmd)
    val s1_valid = portTranslateEnable(idx) && !onlyS2

    // Stage 2 perm check
    val gpf = g_perm.pf
    val g_ldUpdate = !g_perm.a && TlbCmd.isRead(cmd) && !TlbCmd.isAmo(cmd)
    val g_stUpdate = (!g_perm.a || !g_perm.d) && (TlbCmd.isWrite(cmd) || TlbCmd.isAmo(cmd))
    val g_instrUpdate = !g_perm.a && TlbCmd.isExec(cmd)
    val g_ldPermFail = !Mux(hlvx, g_perm.x, (g_perm.r || io.csr.priv.mxr && g_perm.x))
    val g_stPermFail = !g_perm.w
    val g_instrPermFail = !g_perm.x
    val ldGpf = (g_ldPermFail || gpf) && (TlbCmd.isRead(cmd) && !TlbCmd.isAmo(cmd))
    val stGpf = (g_stPermFail || gpf) && (TlbCmd.isWrite(cmd) || TlbCmd.isAmo(cmd))
    val instrGpf = (g_instrPermFail || gpf) && TlbCmd.isExec(cmd)
    val s2_valid = hasS2xlate && !onlyS1 && portTranslateEnable(idx)

    val fault_valid = s1_valid || s2_valid

    // when pf and gpf can't happens simultaneously
    val hasPf = (ldPf || ldUpdate || stPf || stUpdate || instrPf || instrUpdate) && s1_valid && !af
    resp(idx).bits.excp(nDups).pf.ld := (ldPf || ldUpdate) && s1_valid && !af
    resp(idx).bits.excp(nDups).pf.st := (stPf || stUpdate) && s1_valid && !af
    resp(idx).bits.excp(nDups).pf.instr := (instrPf || instrUpdate) && s1_valid && !af
    // NOTE: pf need && with !af, page fault has higher priority than access fault
    // but ptw may also have access fault, then af happens, the translation is wrong.
    // In this case, pf has lower priority than af

    resp(idx).bits.excp(nDups).gpf.ld := (ldGpf || g_ldUpdate) && s2_valid && !af && !hasPf
    resp(idx).bits.excp(nDups).gpf.st := (stGpf || g_stUpdate) && s2_valid && !af && !hasPf
    resp(idx).bits.excp(nDups).gpf.instr := (instrGpf || g_instrUpdate) && s2_valid && !af && !hasPf

    resp(idx).bits.excp(nDups).af.ld    := af && TlbCmd.isRead(cmd) && fault_valid
    resp(idx).bits.excp(nDups).af.st    := af && TlbCmd.isWrite(cmd) && fault_valid
    resp(idx).bits.excp(nDups).af.instr := af && TlbCmd.isExec(cmd) && fault_valid


  }

  def handle_nonblock(idx: Int): Unit = {
    io.requestor(idx).resp.valid := req_out_v(idx)
    io.requestor(idx).req.ready := io.requestor(idx).resp.ready // should always be true
    XSError(!io.requestor(idx).resp.ready, s"${q.name} port ${idx} is non-block, resp.ready must be true.B")

    val req_need_gpa = hasGpf(idx)
    val req_s2xlate = Wire(UInt(2.W))
    req_s2xlate := MuxCase(noS2xlate, Seq(
      (!(virt_out(idx) || req_out(idx).hyperinst)) -> noS2xlate,
      (csr.vsatp.mode =/= 0.U && csr.hgatp.mode =/= 0.U) -> allStage,
      (csr.vsatp.mode === 0.U) -> onlyStage2,
      (csr.hgatp.mode === 0.U || req_need_gpa) -> onlyStage1
    ))
   
    val ptw_just_back = ptw.resp.fire && req_s2xlate === ptw.resp.bits.s2xlate && ptw.resp.bits.hit(get_pn(req_out(idx).vaddr), io.csr.satp.asid, io.csr.vsatp.asid, io.csr.hgatp.vmid, true, false)
    // TODO: RegNext enable: ptw.resp.valid ? req.valid
    val ptw_resp_bits_reg = RegEnable(ptw.resp.bits, ptw.resp.valid)
    val ptw_already_back = GatedValidRegNext(ptw.resp.fire) && req_s2xlate === ptw_resp_bits_reg.s2xlate && ptw_resp_bits_reg.hit(get_pn(req_out(idx).vaddr), io.csr.satp.asid, io.csr.vsatp.asid, io.csr.hgatp.vmid, allType = true)
    io.ptw.req(idx).valid := req_out_v(idx) && missVec(idx) && !(ptw_just_back || ptw_already_back) // TODO: remove the regnext, timing
    io.tlbreplay(idx) := req_out_v(idx) && missVec(idx) && (ptw_just_back || ptw_already_back)
    when (io.requestor(idx).req_kill && GatedValidRegNext(io.requestor(idx).req.fire)) {
      io.ptw.req(idx).valid := false.B
      io.tlbreplay(idx) := true.B
    }
    io.ptw.req(idx).bits.vpn := get_pn(req_out(idx).vaddr)
    io.ptw.req(idx).bits.s2xlate := req_s2xlate
    io.ptw.req(idx).bits.getGpa := req_need_gpa && hitVec(idx)
    io.ptw.req(idx).bits.memidx := req_out(idx).memidx
  }

  def handle_block(idx: Int): Unit = {
    // three valid: 1.if exist a entry; 2.if sent to ptw; 3.unset resp.valid
    io.requestor(idx).req.ready := !req_out_v(idx) || io.requestor(idx).resp.fire
    // req_out_v for if there is a request, may long latency, fixme

    // miss request entries
    val req_need_gpa = hasGpf(idx)
    val miss_req_vpn = get_pn(req_out(idx).vaddr)
    val miss_req_memidx = req_out(idx).memidx
    val miss_req_s2xlate = Wire(UInt(2.W))
    miss_req_s2xlate := MuxCase(noS2xlate, Seq(
      (!(virt_out(idx) || req_out(idx).hyperinst)) -> noS2xlate,
      (csr.vsatp.mode =/= 0.U && csr.hgatp.mode =/= 0.U) -> allStage,
      (csr.vsatp.mode === 0.U) -> onlyStage2,
      (csr.hgatp.mode === 0.U || req_need_gpa) -> onlyStage1
    ))
    val miss_req_s2xlate_reg = RegEnable(miss_req_s2xlate, io.ptw.req(idx).fire)
    val hasS2xlate = miss_req_s2xlate_reg =/= noS2xlate
    val onlyS2 = miss_req_s2xlate_reg === onlyStage2
    val hit_s1 = io.ptw.resp.bits.s1.hit(miss_req_vpn, Mux(hasS2xlate, io.csr.vsatp.asid, io.csr.satp.asid), io.csr.hgatp.vmid, allType = true, false, hasS2xlate)
    val hit_s2 = io.ptw.resp.bits.s2.hit(miss_req_vpn, io.csr.hgatp.vmid)
    val hit = Mux(onlyS2, hit_s2, hit_s1) && io.ptw.resp.valid && miss_req_s2xlate_reg === io.ptw.resp.bits.s2xlate

    val new_coming_valid = WireInit(false.B)
    new_coming_valid := req_in(idx).fire && !req_in(idx).bits.kill && !flush_pipe(idx)
    val new_coming = GatedValidRegNext(new_coming_valid)
    val miss_wire = new_coming && missVec(idx)
    val miss_v = ValidHoldBypass(miss_wire, resp(idx).fire, flush_pipe(idx))
    val miss_req_v = ValidHoldBypass(miss_wire || (miss_v && flush_mmu && !mmu_flush_pipe),
      io.ptw.req(idx).fire || resp(idx).fire, flush_pipe(idx))

    // when ptw resp, check if hit, reset miss_v, resp to lsu/ifu
    resp(idx).valid := req_out_v(idx) && !(miss_v && portTranslateEnable(idx))
    when (io.ptw.resp.fire && hit && req_out_v(idx) && portTranslateEnable(idx)) {
      val stage1 = io.ptw.resp.bits.s1
      val stage2 = io.ptw.resp.bits.s2
      val s2xlate = io.ptw.resp.bits.s2xlate
      resp(idx).valid := true.B
      resp(idx).bits.miss := false.B
      val s1_paddr = Cat(stage1.genPPN(get_pn(req_out(idx).vaddr)), get_off(req_out(idx).vaddr))
      val s2_paddr = Cat(stage2.genPPNS2(get_pn(req_out(idx).vaddr)), get_off(req_out(idx).vaddr))
      for (d <- 0 until nRespDups) {
        resp(idx).bits.paddr(d) := Mux(s2xlate =/= noS2xlate, s2_paddr, s1_paddr)
        resp(idx).bits.gpaddr(d) := s1_paddr
        pbmt_check(idx, d, io.ptw.resp.bits.s1.entry.pbmt, io.ptw.resp.bits.s2.entry.pbmt, s2xlate)
        perm_check(stage1, req_out(idx).cmd, idx, d, stage2, req_out(idx).hlvx, s2xlate)
      }
      pmp_check(resp(idx).bits.paddr(0), req_out(idx).size, req_out(idx).cmd, idx)

      // NOTE: the unfiltered req would be handled by Repeater
    }
    assert(RegNext(!resp(idx).valid || resp(idx).ready, true.B), "when tlb resp valid, ready should be true, must")
    assert(RegNext(req_out_v(idx) || !(miss_v || miss_req_v), true.B), "when not req_out_v, should not set miss_v/miss_req_v")

    val ptw_req = io.ptw.req(idx)
    ptw_req.valid := miss_req_v
    ptw_req.bits.vpn := miss_req_vpn
    ptw_req.bits.s2xlate := miss_req_s2xlate
    ptw_req.bits.getGpa := req_need_gpa && hitVec(idx)
    ptw_req.bits.memidx := miss_req_memidx

    io.tlbreplay(idx) := false.B

    // NOTE: when flush pipe, tlb should abandon last req
    // however, some outside modules like icache, dont care flushPipe, and still waiting for tlb resp
    // just resp valid and raise page fault to go through. The pipe(ifu) will abandon it.
    if (!q.outsideRecvFlush) {
      when (req_out_v(idx) && flush_pipe(idx) && portTranslateEnable(idx)) {
        resp(idx).valid := true.B
        for (d <- 0 until nRespDups) {
          resp(idx).bits.pbmt(d) := 0.U
          resp(idx).bits.excp(d).pf.ld := true.B // sfence happened, pf for not to use this addr
          resp(idx).bits.excp(d).pf.st := true.B
          resp(idx).bits.excp(d).pf.instr := true.B
        }
      }
    }
  }

  // when ptw resp, tlb at refill_idx maybe set to miss by force.
  // Bypass ptw resp to check.
  def ptw_resp_bypass(vpn: UInt, s2xlate: UInt) = {
    // TODO: RegNext enable: ptw.resp.valid
    val hasS2xlate = s2xlate =/= noS2xlate
    val onlyS2 = s2xlate === onlyStage2
    val onlyS1 = s2xlate === onlyStage1
    val s2xlate_hit = s2xlate === ptw.resp.bits.s2xlate
    val resp_hit = ptw.resp.bits.hit(vpn, io.csr.satp.asid, io.csr.vsatp.asid, io.csr.hgatp.vmid, true, false)
    val p_hit = GatedValidRegNext(resp_hit && io.ptw.resp.fire && s2xlate_hit)
    val ppn_s1 = ptw.resp.bits.s1.genPPN(vpn)
    val gvpn = Mux(onlyS2, vpn, ppn_s1)
    val ppn_s2 = ptw.resp.bits.s2.genPPNS2(gvpn)
    val p_ppn = RegEnable(Mux(hasS2xlate, ppn_s2, ppn_s1), io.ptw.resp.fire)
    val p_pbmt = RegEnable(ptw.resp.bits.s1.entry.pbmt,io.ptw.resp.fire)
    val p_perm = RegEnable(ptwresp_to_tlbperm(ptw.resp.bits.s1), io.ptw.resp.fire)
    val p_gvpn = RegEnable(Mux(onlyS2, ptw.resp.bits.s2.entry.tag, ppn_s1), io.ptw.resp.fire)
    val p_g_pbmt = RegEnable(ptw.resp.bits.s2.entry.pbmt,io.ptw.resp.fire)
    val p_g_perm = RegEnable(hptwresp_to_tlbperm(ptw.resp.bits.s2), io.ptw.resp.fire)
    val p_s2xlate = RegEnable(ptw.resp.bits.s2xlate, io.ptw.resp.fire)
    val p_s1_level = RegEnable(ptw.resp.bits.s1.entry.level.get, io.ptw.resp.fire)
    val p_s1_isLeaf = RegEnable(ptw.resp.bits.s1.isLeaf(), io.ptw.resp.fire)
    val p_s1_isFakePte = RegEnable(ptw.resp.bits.s1.isFakePte(), io.ptw.resp.fire)
    (p_hit, p_ppn, p_pbmt, p_perm, p_gvpn, p_g_pbmt, p_g_perm, p_s2xlate, p_s1_level, p_s1_isLeaf, p_s1_isFakePte)
  }

  // assert
  for(i <- 0 until Width) {
    TimeOutAssert(req_out_v(i) && !resp(i).valid, timeOutThreshold, s"{q.name} port{i} long time no resp valid.")
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
      difftest.satp := Cat(io.csr.satp.mode, io.csr.satp.asid, io.csr.satp.ppn)
      difftest.vsatp := Cat(io.csr.vsatp.mode, io.csr.vsatp.asid, io.csr.vsatp.ppn)
      difftest.hgatp := Cat(io.csr.hgatp.mode, io.csr.hgatp.vmid, io.csr.hgatp.ppn)
      val req_need_gpa = gpf
      val req_s2xlate = Wire(UInt(2.W))
      req_s2xlate := MuxCase(noS2xlate, Seq(
        (!RegNext(virt_in || req_in(i).bits.hyperinst)) -> noS2xlate,
        (vsatp.mode =/= 0.U && hgatp.mode =/= 0.U) -> allStage,
        (vsatp.mode === 0.U) -> onlyStage2,
        (hgatp.mode === 0.U || req_need_gpa) -> onlyStage1
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
