package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.util._
import xiangshan.backend.fu.{FuConfig, FuncUnit}
import device._
import system.HasSoCParameter
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles.TrapInstInfo
import xiangshan.backend.decode.Imm_Z
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState
import xiangshan.backend.fu.NewCSR.CSRDefines.PrivMode
import xiangshan.frontend.FtqPtr

class CSR(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg)
  with HasCircularQueuePtrHelper
{
  val csrIn = io.csrio.get
  val csrOut = io.csrio.get
  val csrToDecode = io.csrToDecode.get

  val setFsDirty = csrIn.fpu.dirty_fs
  val setFflags = csrIn.fpu.fflags

  val setVsDirty = csrIn.vpu.dirty_vs
  val setVstart = csrIn.vpu.set_vstart
  val setVtype = csrIn.vpu.set_vtype
  val setVxsat = csrIn.vpu.set_vxsat
  val vlFromPreg = csrIn.vpu.vl

  val flushPipe = Wire(Bool())
  val flush = io.flush.valid

  /** Alias of input signals */
  val (valid, src1, imm, func) = (
    io.in.valid,
    io.in.bits.data.src(0),
    io.in.bits.data.imm(Imm_Z().len - 1, 0),
    io.in.bits.ctrl.fuOpType
  )

  // split imm/src1/rd from IMM_Z: src1/rd for tval
  val addr = Imm_Z().getCSRAddr(imm)
  val rd   = Imm_Z().getRD(imm)
  val rs1  = Imm_Z().getRS1(imm)
  val imm5 = Imm_Z().getImm5(imm)
  val csri = ZeroExt(imm5, XLEN)

  import CSRConst._

  private val isEcall  = CSROpType.isSystemOp(func) && addr === privEcall
  private val isEbreak = CSROpType.isSystemOp(func) && addr === privEbreak
  private val isMNret  = CSROpType.isSystemOp(func) && addr === privMNret
  private val isMret   = CSROpType.isSystemOp(func) && addr === privMret
  private val isSret   = CSROpType.isSystemOp(func) && addr === privSret
  private val isDret   = CSROpType.isSystemOp(func) && addr === privDret
  private val isWfi    = CSROpType.isWfi(func)
  private val isCSRAcc = CSROpType.isCsrAccess(func)

  val csrMod = Module(new NewCSR)
  val trapInstMod = Module(new TrapInstMod)
  val trapTvalMod = Module(new TrapTvalMod)

  private val privState = csrMod.io.status.privState
  // The real reg value in CSR, with no read mask
  private val regOut = csrMod.io.out.bits.regOut
  private val src = Mux(CSROpType.needImm(func), csri, src1)
  private val wdata = LookupTree(func, Seq(
    CSROpType.wrt  -> src1,
    CSROpType.set  -> (regOut | src1),
    CSROpType.clr  -> (regOut & (~src1).asUInt),
    CSROpType.wrti -> csri,
    CSROpType.seti -> (regOut | csri),
    CSROpType.clri -> (regOut & (~csri).asUInt),
  ))

  private val csrAccess = valid && CSROpType.isCsrAccess(func)
  private val csrWen = valid && (
    CSROpType.isCSRRW(func) ||
    CSROpType.isCSRRSorRC(func) && rs1 =/= 0.U
  )
  private val csrRen = valid && (
    CSROpType.isCSRRW(func) && rd =/= 0.U ||
    CSROpType.isCSRRSorRC(func)
  )

  csrMod.io.in match {
    case in =>
      in.valid := valid
      in.bits.wen := csrWen
      in.bits.ren := csrRen
      in.bits.op  := CSROpType.getCSROp(func)
      in.bits.addr := addr
      in.bits.src := src
      in.bits.wdata := wdata
      in.bits.mret := isMret
      in.bits.mnret := isMNret
      in.bits.sret := isSret
      in.bits.dret := isDret
  }
  csrMod.io.trapInst := trapInstMod.io.currentTrapInst
  csrMod.io.fetchMalTval := trapTvalMod.io.tval
  csrMod.io.fromMem.excpVA  := csrIn.memExceptionVAddr
  csrMod.io.fromMem.excpGPA := csrIn.memExceptionGPAddr
  csrMod.io.fromMem.excpIsForVSnonLeafPTE := csrIn.memExceptionIsForVSnonLeafPTE

  csrMod.io.fromRob.trap.valid := csrIn.exception.valid
  csrMod.io.fromRob.trap.bits.pc := csrIn.exception.bits.pc
  csrMod.io.fromRob.trap.bits.instr := csrIn.exception.bits.instr
  csrMod.io.fromRob.trap.bits.pcGPA := csrIn.exception.bits.gpaddr
  // Todo: shrink the width of trap vector.
  // We use 64bits trap vector in CSR, and 24 bits exceptionVec in exception bundle.
  csrMod.io.fromRob.trap.bits.trapVec := csrIn.exception.bits.exceptionVec.asUInt
  csrMod.io.fromRob.trap.bits.singleStep := csrIn.exception.bits.singleStep
  csrMod.io.fromRob.trap.bits.crossPageIPFFix := csrIn.exception.bits.crossPageIPFFix
  csrMod.io.fromRob.trap.bits.isInterrupt := csrIn.exception.bits.isInterrupt
  csrMod.io.fromRob.trap.bits.trigger := csrIn.exception.bits.trigger
  csrMod.io.fromRob.trap.bits.isHls := csrIn.exception.bits.isHls
  csrMod.io.fromRob.trap.bits.isFetchMalAddr := csrIn.exception.bits.isFetchMalAddr
  csrMod.io.fromRob.trap.bits.isForVSnonLeafPTE := csrIn.exception.bits.isForVSnonLeafPTE

  csrMod.io.fromRob.commit.fflags := setFflags
  csrMod.io.fromRob.commit.fsDirty := setFsDirty
  csrMod.io.fromRob.commit.vxsat.valid := setVxsat.valid
  csrMod.io.fromRob.commit.vxsat.bits := setVxsat.bits
  csrMod.io.fromRob.commit.vsDirty := setVsDirty
  csrMod.io.fromRob.commit.vstart := setVstart
  csrMod.io.fromRob.commit.vl := vlFromPreg
  // Todo: correct vtype
  csrMod.io.fromRob.commit.vtype.valid := setVtype.valid
  csrMod.io.fromRob.commit.vtype.bits.VILL := setVtype.bits(XLEN - 1)
  csrMod.io.fromRob.commit.vtype.bits.VMA := setVtype.bits(7)
  csrMod.io.fromRob.commit.vtype.bits.VTA := setVtype.bits(6)
  csrMod.io.fromRob.commit.vtype.bits.VSEW := setVtype.bits(5, 3)
  csrMod.io.fromRob.commit.vtype.bits.VLMUL := setVtype.bits(2, 0)

  csrMod.io.fromRob.commit.instNum.valid := true.B  // Todo: valid control signal
  csrMod.io.fromRob.commit.instNum.bits  := csrIn.perf.retiredInstr

  csrMod.io.fromRob.robDeqPtr := csrIn.robDeqPtr

  csrMod.io.fromVecExcpMod.busy := io.csrin.get.fromVecExcpMod.busy

  csrMod.io.perf  := csrIn.perf

  csrMod.platformIRP.MEIP := csrIn.externalInterrupt.meip
  csrMod.platformIRP.MTIP := csrIn.externalInterrupt.mtip
  csrMod.platformIRP.MSIP := csrIn.externalInterrupt.msip
  csrMod.platformIRP.SEIP := csrIn.externalInterrupt.seip
  csrMod.platformIRP.STIP := false.B
  csrMod.platformIRP.VSEIP := false.B // Todo
  csrMod.platformIRP.VSTIP := false.B // Todo
  csrMod.platformIRP.debugIP := csrIn.externalInterrupt.debug
  csrMod.nonMaskableIRP.NMI_43 := csrIn.externalInterrupt.nmi.nmi_43
  csrMod.nonMaskableIRP.NMI_31 := csrIn.externalInterrupt.nmi.nmi_31

  csrMod.io.fromTop.hartId := io.csrin.get.hartId
  csrMod.io.fromTop.clintTime := io.csrin.get.clintTime
  private val csrModOutValid = csrMod.io.out.valid
  private val csrModOut      = csrMod.io.out.bits

  trapInstMod.io.fromDecode.trapInstInfo := RegNextWithEnable(io.csrin.get.trapInstInfo, hasInit = true)
  trapInstMod.io.fromRob.flush.valid := io.flush.valid
  trapInstMod.io.fromRob.flush.bits.ftqPtr := io.flush.bits.ftqIdx
  trapInstMod.io.fromRob.flush.bits.ftqOffset := io.flush.bits.ftqOffset
  trapInstMod.io.faultCsrUop.valid         := csrMod.io.out.valid && (csrMod.io.out.bits.EX_II || csrMod.io.out.bits.EX_VI)
  trapInstMod.io.faultCsrUop.bits.fuOpType := DataHoldBypass(io.in.bits.ctrl.fuOpType, io.in.fire)
  trapInstMod.io.faultCsrUop.bits.imm      := DataHoldBypass(io.in.bits.data.imm, io.in.fire)
  trapInstMod.io.faultCsrUop.bits.ftqInfo.ftqPtr    := DataHoldBypass(io.in.bits.ctrl.ftqIdx.get, io.in.fire)
  trapInstMod.io.faultCsrUop.bits.ftqInfo.ftqOffset := DataHoldBypass(io.in.bits.ctrl.ftqOffset.get, io.in.fire)
  // Clear trap instruction when instruction fault trap(EX_II, EX_VI) occurs.
  trapInstMod.io.readClear := (csrMod.io.fromRob.trap match {
    case t =>
      t.valid && !t.bits.isInterrupt && (t.bits.trapVec(EX_II) || t.bits.trapVec(EX_VI))
  })

  trapTvalMod.io.targetPc.valid := csrMod.io.out.bits.targetPcUpdate
  trapTvalMod.io.targetPc.bits := csrMod.io.out.bits.targetPc
  trapTvalMod.io.clear := csrIn.exception.valid && csrIn.exception.bits.isFetchMalAddr
  trapTvalMod.io.fromCtrlBlock.flush := io.flush
  trapTvalMod.io.fromCtrlBlock.robDeqPtr := io.csrio.get.robDeqPtr

  private val imsic = Module(new IMSIC(NumVSIRFiles = 5, NumHart = 1, XLEN = 64, NumIRSrc = 256))
  imsic.i.hartId := io.csrin.get.hartId
  imsic.i.msiInfo := io.csrin.get.msiInfo
  imsic.i.csr.addr.valid := csrMod.toAIA.addr.valid
  imsic.i.csr.addr.bits.addr := csrMod.toAIA.addr.bits.addr
  imsic.i.csr.addr.bits.prvm := csrMod.toAIA.addr.bits.prvm.asUInt
  imsic.i.csr.addr.bits.v := csrMod.toAIA.addr.bits.v.asUInt
  imsic.i.csr.vgein := csrMod.toAIA.vgein
  imsic.i.csr.mClaim := csrMod.toAIA.mClaim
  imsic.i.csr.sClaim := csrMod.toAIA.sClaim
  imsic.i.csr.vsClaim := csrMod.toAIA.vsClaim
  imsic.i.csr.wdata.valid := csrMod.toAIA.wdata.valid
  imsic.i.csr.wdata.bits.op := csrMod.toAIA.wdata.bits.op
  imsic.i.csr.wdata.bits.data := csrMod.toAIA.wdata.bits.data

  csrMod.fromAIA.rdata.valid        := imsic.o.csr.rdata.valid
  csrMod.fromAIA.rdata.bits.data    := imsic.o.csr.rdata.bits.rdata
  csrMod.fromAIA.rdata.bits.illegal := imsic.o.csr.rdata.bits.illegal
  csrMod.fromAIA.meip    := imsic.o.meip
  csrMod.fromAIA.seip    := imsic.o.seip
  csrMod.fromAIA.vseip   := imsic.o.vseip
  csrMod.fromAIA.mtopei  := imsic.o.mtopei
  csrMod.fromAIA.stopei  := imsic.o.stopei
  csrMod.fromAIA.vstopei := imsic.o.vstopei

  private val exceptionVec = WireInit(0.U.asTypeOf(ExceptionVec())) // Todo:

  exceptionVec(EX_BP    ) := DataHoldBypass(isEbreak, false.B, io.in.fire)
  exceptionVec(EX_MCALL ) := DataHoldBypass(isEcall && privState.isModeM, false.B, io.in.fire)
  exceptionVec(EX_HSCALL) := DataHoldBypass(isEcall && privState.isModeHS, false.B, io.in.fire)
  exceptionVec(EX_VSCALL) := DataHoldBypass(isEcall && privState.isModeVS, false.B, io.in.fire)
  exceptionVec(EX_UCALL ) := DataHoldBypass(isEcall && privState.isModeHUorVU, false.B, io.in.fire)
  exceptionVec(EX_II    ) := csrMod.io.out.bits.EX_II
  exceptionVec(EX_VI    ) := csrMod.io.out.bits.EX_VI

  val isXRet = valid && func === CSROpType.jmp && !isEcall && !isEbreak

  // ctrl block will use theses later for flush // Todo: optimize isXRetFlag's DelayN
  val isXRetFlag = RegInit(false.B)
  isXRetFlag := Mux1H(Seq(
    DelayN(flush, 5) -> false.B,
    isXRet -> true.B,
  ))

  flushPipe := csrMod.io.out.bits.flushPipe

  // tlb
  val tlb = Wire(new TlbCsrBundle)
  tlb.satp.changed  := csrMod.io.tlb.satpASIDChanged
  tlb.satp.mode     := csrMod.io.tlb.satp.MODE.asUInt
  tlb.satp.asid     := csrMod.io.tlb.satp.ASID.asUInt
  tlb.satp.ppn      := csrMod.io.tlb.satp.PPN.asUInt
  tlb.vsatp.changed := csrMod.io.tlb.vsatpASIDChanged
  tlb.vsatp.mode    := csrMod.io.tlb.vsatp.MODE.asUInt
  tlb.vsatp.asid    := csrMod.io.tlb.vsatp.ASID.asUInt
  tlb.vsatp.ppn     := csrMod.io.tlb.vsatp.PPN.asUInt
  tlb.hgatp.changed := csrMod.io.tlb.hgatpVMIDChanged
  tlb.hgatp.mode    := csrMod.io.tlb.hgatp.MODE.asUInt
  tlb.hgatp.vmid    := csrMod.io.tlb.hgatp.VMID.asUInt
  tlb.hgatp.ppn     := csrMod.io.tlb.hgatp.PPN.asUInt

  // expose several csr bits for tlb
  tlb.priv.mxr := csrMod.io.tlb.mxr
  tlb.priv.sum := csrMod.io.tlb.sum
  tlb.priv.vmxr := csrMod.io.tlb.vmxr
  tlb.priv.vsum := csrMod.io.tlb.vsum
  tlb.priv.spvp := csrMod.io.tlb.spvp
  tlb.priv.virt := csrMod.io.tlb.dvirt
  tlb.priv.imode := csrMod.io.tlb.imode
  tlb.priv.dmode := csrMod.io.tlb.dmode

  // Svpbmt extension enable
  tlb.mPBMTE := csrMod.io.tlb.mPBMTE
  tlb.hPBMTE := csrMod.io.tlb.hPBMTE

  /** Since some CSR read instructions are allowed to be pipelined, ready/valid signals should be modified */
  io.in.ready := csrMod.io.in.ready // Todo: Async read imsic may block CSR
  io.out.valid := csrModOutValid
  io.out.bits.ctrl.exceptionVec.get := exceptionVec
  io.out.bits.ctrl.flushPipe.get := flushPipe
  io.out.bits.res.data := csrMod.io.out.bits.rData

  /** initialize NewCSR's io_out_ready from wrapper's io */
  csrMod.io.out.ready := io.out.ready

  io.out.bits.res.redirect.get.valid := io.out.valid && DataHoldBypass(isXRet, false.B, io.in.fire)
  val redirect = io.out.bits.res.redirect.get.bits
  redirect := 0.U.asTypeOf(redirect)
  redirect.level := RedirectLevel.flushAfter
  redirect.robIdx := io.in.bits.ctrl.robIdx
  redirect.ftqIdx := io.in.bits.ctrl.ftqIdx.get
  redirect.ftqOffset := io.in.bits.ctrl.ftqOffset.get
  redirect.cfiUpdate.predTaken := true.B
  redirect.cfiUpdate.taken := true.B
  redirect.cfiUpdate.target := csrMod.io.out.bits.targetPc.pc
  redirect.cfiUpdate.backendIPF := csrMod.io.out.bits.targetPc.raiseIPF
  redirect.cfiUpdate.backendIAF := csrMod.io.out.bits.targetPc.raiseIAF
  redirect.cfiUpdate.backendIGPF := csrMod.io.out.bits.targetPc.raiseIGPF
  // Only mispred will send redirect to frontend
  redirect.cfiUpdate.isMisPred := true.B

  connectNonPipedCtrlSingalForCSR

  // Todo: summerize all difftest skip condition
  csrOut.isPerfCnt  := io.out.valid && csrMod.io.out.bits.isPerfCnt && DataHoldBypass(func =/= CSROpType.jmp, false.B, io.in.fire)
  csrOut.fpu.frm    := csrMod.io.status.fpState.frm.asUInt
  csrOut.vpu.vstart := csrMod.io.status.vecState.vstart.asUInt
  csrOut.vpu.vxrm   := csrMod.io.status.vecState.vxrm.asUInt

  csrOut.isXRet := isXRetFlag

  csrOut.trapTarget := csrMod.io.out.bits.targetPc
  csrOut.interrupt := csrMod.io.status.interrupt
  csrOut.wfi_event := csrMod.io.status.wfiEvent

  csrOut.tlb := tlb

  csrOut.debugMode := csrMod.io.status.debugMode

  csrOut.customCtrl match {
    case custom =>
      custom.l1I_pf_enable            := csrMod.io.status.custom.l1I_pf_enable
      custom.l2_pf_enable             := csrMod.io.status.custom.l2_pf_enable
      custom.l1D_pf_enable            := csrMod.io.status.custom.l1D_pf_enable
      custom.l1D_pf_train_on_hit      := csrMod.io.status.custom.l1D_pf_train_on_hit
      custom.l1D_pf_enable_agt        := csrMod.io.status.custom.l1D_pf_enable_agt
      custom.l1D_pf_enable_pht        := csrMod.io.status.custom.l1D_pf_enable_pht
      custom.l1D_pf_active_threshold  := csrMod.io.status.custom.l1D_pf_active_threshold
      custom.l1D_pf_active_stride     := csrMod.io.status.custom.l1D_pf_active_stride
      custom.l1D_pf_enable_stride     := csrMod.io.status.custom.l1D_pf_enable_stride
      custom.l2_pf_store_only         := csrMod.io.status.custom.l2_pf_store_only
      // ICache
      custom.icache_parity_enable     := csrMod.io.status.custom.icache_parity_enable
      // Load violation predictor
      custom.lvpred_disable           := csrMod.io.status.custom.lvpred_disable
      custom.no_spec_load             := csrMod.io.status.custom.no_spec_load
      custom.storeset_wait_store      := csrMod.io.status.custom.storeset_wait_store
      custom.storeset_no_fast_wakeup  := csrMod.io.status.custom.storeset_no_fast_wakeup
      custom.lvpred_timeout           := csrMod.io.status.custom.lvpred_timeout
      // Branch predictor
      custom.bp_ctrl                  := csrMod.io.status.custom.bp_ctrl
      // Memory Block
      custom.sbuffer_threshold                := csrMod.io.status.custom.sbuffer_threshold
      custom.ldld_vio_check_enable            := csrMod.io.status.custom.ldld_vio_check_enable
      custom.soft_prefetch_enable             := csrMod.io.status.custom.soft_prefetch_enable
      custom.cache_error_enable               := csrMod.io.status.custom.cache_error_enable
      custom.uncache_write_outstanding_enable := csrMod.io.status.custom.uncache_write_outstanding_enable
      custom.hd_misalign_st_enable            := csrMod.io.status.custom.hd_misalign_st_enable
      custom.hd_misalign_ld_enable            := csrMod.io.status.custom.hd_misalign_ld_enable
      // Rename
      custom.fusion_enable            := csrMod.io.status.custom.fusion_enable
      custom.wfi_enable               := csrMod.io.status.custom.wfi_enable
      // distribute csr write signal
      // write to frontend and memory
      custom.distribute_csr.w.valid := csrMod.io.distributedWenLegal
      custom.distribute_csr.w.bits.addr := addr
      custom.distribute_csr.w.bits.data := wdata
      // rename single step
      custom.singlestep := csrMod.io.status.singleStepFlag
      // trigger
      custom.frontend_trigger := csrMod.io.status.frontendTrigger
      custom.mem_trigger      := csrMod.io.status.memTrigger
      // virtual mode
      custom.virtMode := csrMod.io.status.privState.V.asBool
  }

  csrOut.instrAddrTransType := csrMod.io.status.instrAddrTransType

  csrToDecode := csrMod.io.toDecode
}

class CSRInput(implicit p: Parameters) extends XSBundle with HasSoCParameter{
  val hartId = Input(UInt(8.W))
  val msiInfo = Input(ValidIO(new MsiInfoBundle))
  val clintTime = Input(ValidIO(UInt(64.W)))
  val trapInstInfo = Input(ValidIO(new TrapInstInfo))
  val fromVecExcpMod = Input(new Bundle {
    val busy = Bool()
  })
}

class CSRToDecode(implicit p: Parameters) extends XSBundle {
  val illegalInst = new Bundle {
    /**
     * illegal sfence.vma, sinval.vma
     * raise EX_II when isModeHS && mstatus.TVM=1 || isModeHU
     */
    val sfenceVMA = Bool()

    /**
     * illegal sfence.w.inval sfence.inval.ir
     * raise EX_II when isModeHU
     */
    val sfencePart = Bool()

    /**
     * illegal hfence.gvma, hinval.gvma
     * raise EX_II when isModeHS && mstatus.TVM=1 || isModeHU
     * the condition is the same as sfenceVMA
     */
    val hfenceGVMA = Bool()

    /**
     * illegal hfence.vvma, hinval.vvma
     * raise EX_II when isModeHU
     */
    val hfenceVVMA = Bool()

    /**
     * illegal hlv, hlvx, and hsv
     * raise EX_II when isModeHU && hstatus.HU=0
     */
    val hlsv = Bool()

    /**
     * decode all fp inst or all vecfp inst
     * raise EX_II when FS=Off
     */
    val fsIsOff = Bool()

    /**
     * decode all vec inst
     * raise EX_II when VS=Off
     */
    val vsIsOff = Bool()

    /**
     * illegal wfi
     * raise EX_II when isModeHU || !isModeM && mstatus.TW=1
     */
    val wfi = Bool()

    /**
     * frm reserved
     * raise EX_II when frm.data > 4
     */
    val frm = Bool()

    /**
     * illegal CBO.ZERO
     * raise [[EX_II]] when !isModeM && !MEnvCfg.CBZE || isModeHU && !SEnvCfg.CBZE
     */
    val cboZ = Bool()

    /**
     * illegal CBO.CLEAN/FLUSH
     * raise [[EX_II]] when !isModeM && !MEnvCfg.CBCFE || isModeHU && !SEnvCfg.CBCFE
     */
    val cboCF = Bool()

    /**
     * illegal CBO.INVAL
     * raise [[EX_II]] when !isModeM && MEnvCfg.CBIE = EnvCBIE.Off || isModeHU && SEnvCfg.CBIE = EnvCBIE.Off
     */
    val cboI = Bool()
  }

  val virtualInst = new Bundle {
    /**
     * illegal sfence.vma, svinval.vma
     * raise EX_VI when isModeVS && hstatus.VTVM=1 || isModeVU
     */
    val sfenceVMA = Bool()

    /**
     * illegal sfence.w.inval sfence.inval.ir
     * raise EX_VI when isModeVU
     */
    val sfencePart = Bool()

    /**
     * illegal hfence.gvma, hinval.gvma, hfence.vvma, hinval.vvma
     * raise EX_VI when isModeVS || isModeVU
     */
    val hfence = Bool()

    /**
     * illegal hlv, hlvx, and hsv
     * raise EX_VI when isModeVS || isModeVU
     */
    val hlsv = Bool()

    /**
     * illegal wfi
     * raise EX_VI when isModeVU && mstatus.TW=0 || isModeVS && mstatus.TW=0 && hstatus.VTW=1
     */
    val wfi = Bool()

    /**
     * illegal CBO.ZERO
     * raise [[EX_VI]] when MEnvCfg.CBZE && (isModeVS && !HEnvCfg.CBZE || isModeVU && (!HEnvCfg.CBZE || !SEnvCfg.CBZE))
     */
    val cboZ = Bool()

    /**
     * illegal CBO.CLEAN/FLUSH
     * raise [[EX_VI]] when MEnvCfg.CBZE && (isModeVS && !HEnvCfg.CBCFE || isModeVU && (!HEnvCfg.CBCFE || !SEnvCfg.CBCFE))
     */
    val cboCF = Bool()

    /**
     * illegal CBO.INVAL <br/>
     * raise [[EX_VI]] when MEnvCfg.CBIE =/= EnvCBIE.Off && ( <br/>
     *   isModeVS && HEnvCfg.CBIE === EnvCBIE.Off || <br/>
     *   isModeVU && (HEnvCfg.CBIE === EnvCBIE.Off || SEnvCfg.CBIE === EnvCBIE.Off) <br/>
     * ) <br/>
     */
    val cboI = Bool()
  }

  val special = new Bundle {
    /**
     * execute CBO.INVAL and perform flush operation when <br/>
     * isModeHS && MEnvCfg.CBIE === EnvCBIE.Flush || <br/>
     * isModeHU && (MEnvCfg.CBIE === EnvCBIE.Flush || SEnvCfg.CBIE === EnvCBIE.Flush) <br/>
     * isModeVS && (MEnvCfg.CBIE === EnvCBIE.Flush || HEnvCfg.CBIE === EnvCBIE.Flush) <br/>
     * isModeVU && (MEnvCfg.CBIE === EnvCBIE.Flush || HEnvCfg.CBIE === EnvCBIE.Flush || SEnvCfg.CBIE === EnvCBIE.Flush) <br/>
     */
    val cboI2F = Bool()
  }
}