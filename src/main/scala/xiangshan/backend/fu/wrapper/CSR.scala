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
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState
import xiangshan.backend.fu.NewCSR.CSRDefines.PrivMode

class CSR(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg)
{
  val csrIn = io.csrio.get
  val csrOut = io.csrio.get
  val csrToDecode = io.csrToDecode.get

  val setFsDirty = csrIn.fpu.dirty_fs
  val setFflags = csrIn.fpu.fflags
  val setVsDirty = csrIn.vpu.dirty_vs
  val setVxsat = csrIn.vpu.vxsat
  val setVstart = csrIn.vpu.set_vstart
  val setVl = csrIn.vpu.set_vl
  val setVtype = 0.U.asTypeOf(csrIn.vpu.set_vtype)

  val flushPipe = Wire(Bool())
  val flush = io.flush.valid

  val (valid, src1, src2, func) = (
    io.in.valid,
    io.in.bits.data.src(0),
    io.in.bits.data.imm,
    io.in.bits.ctrl.fuOpType
  )

  // split imm from IMM_Z
  val addr = src2(11, 0)
  val csri = ZeroExt(src2(16, 12), XLEN)

  import CSRConst._

  private val isEcall  = CSROpType.isSystemOp(func) && addr === privEcall
  private val isEbreak = CSROpType.isSystemOp(func) && addr === privEbreak
  private val isMret   = CSROpType.isSystemOp(func) && addr === privMret
  private val isSret   = CSROpType.isSystemOp(func) && addr === privSret
  private val isDret   = CSROpType.isSystemOp(func) && addr === privDret
  private val isWfi    = CSROpType.isWfi(func)
  private val isCSRAcc = CSROpType.isCsrAccess(func)

  val csrMod = Module(new NewCSR)

  private val privState = csrMod.io.out.privState
  // The real reg value in CSR, with no read mask
  private val regOut = csrMod.io.out.regOut
  // The read data with read mask
  private val rdata = csrMod.io.out.rData
  private val wdata = LookupTree(func, Seq(
    CSROpType.wrt  -> src1,
    CSROpType.set  -> (regOut | src1),
    CSROpType.clr  -> (regOut & (~src1).asUInt),
    CSROpType.wrti -> csri,
    CSROpType.seti -> (regOut | csri),
    CSROpType.clri -> (regOut & (~csri).asUInt),
  ))

  private val csrAccess = valid && CSROpType.isCsrAccess(func)
  private val csrWen = valid && CSROpType.notReadOnly(func)

  csrMod.io.in match {
    case in =>
      in.wen := csrWen
      in.ren := csrAccess
      in.addr := addr
      in.wdata := wdata
  }
  csrMod.io.fromMem.excpVA  := csrIn.memExceptionVAddr
  csrMod.io.fromMem.excpGPA := csrIn.memExceptionGPAddr

  csrMod.io.fromRob.trap.valid := csrIn.exception.valid
  csrMod.io.fromRob.trap.bits.pc := csrIn.exception.bits.pc
  csrMod.io.fromRob.trap.bits.instr := csrIn.exception.bits.instr
  // Todo: shrink the width of trap vector.
  // We use 64bits trap vector in CSR, and 24 bits exceptionVec in exception bundle.
  csrMod.io.fromRob.trap.bits.trapVec := csrIn.exception.bits.exceptionVec.asUInt
  csrMod.io.fromRob.trap.bits.singleStep := csrIn.exception.bits.singleStep
  csrMod.io.fromRob.trap.bits.crossPageIPFFix := csrIn.exception.bits.crossPageIPFFix
  csrMod.io.fromRob.trap.bits.isInterrupt := csrIn.exception.bits.isInterrupt
  csrMod.io.fromRob.trap.bits.triggerCf := csrIn.exception.bits.trigger

  csrMod.io.fromRob.commit.fflags := setFflags
  csrMod.io.fromRob.commit.fsDirty := setFsDirty
  csrMod.io.fromRob.commit.vxsat.valid := setVxsat
  csrMod.io.fromRob.commit.vxsat.bits := setVxsat
  csrMod.io.fromRob.commit.vsDirty := setVsDirty
  csrMod.io.fromRob.commit.vstart := setVstart
  csrMod.io.fromRob.commit.vl := setVl
  // Todo: correct vtype
  csrMod.io.fromRob.commit.vtype.valid := setVtype.valid
  csrMod.io.fromRob.commit.vtype.bits.VILL := setVtype.bits(XLEN - 1)
  csrMod.io.fromRob.commit.vtype.bits.VMA := setVtype.bits(7)
  csrMod.io.fromRob.commit.vtype.bits.VTA := setVtype.bits(6)
  csrMod.io.fromRob.commit.vtype.bits.VSEW := setVtype.bits(5, 3)
  csrMod.io.fromRob.commit.vtype.bits.VLMUL := setVtype.bits(2, 0)

  csrMod.io.fromRob.commit.instNum.valid := true.B  // Todo: valid control signal
  csrMod.io.fromRob.commit.instNum.bits  := csrIn.perf.retiredInstr

  csrMod.io.mret := isMret && valid
  csrMod.io.sret := isSret && valid
  csrMod.io.dret := isDret && valid
  csrMod.io.wfi  := isWfi  && valid
  csrMod.io.ebreak := isEbreak && valid

  csrMod.platformIRP.MEIP := csrIn.externalInterrupt.meip
  csrMod.platformIRP.MTIP := csrIn.externalInterrupt.mtip
  csrMod.platformIRP.MSIP := csrIn.externalInterrupt.msip
  csrMod.platformIRP.SEIP := csrIn.externalInterrupt.seip
  csrMod.platformIRP.VSEIP := false.B // Todo
  csrMod.platformIRP.VSTIP := false.B // Todo
  csrMod.platformIRP.debugIP := csrIn.externalInterrupt.debug

  csrMod.io.fromTop.hartId := io.csrin.get.hartId

  private val imsic = Module(new IMSIC)
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
  imsic.i.csr.wdata.bits.data := csrMod.toAIA.wdata.bits.data

  csrMod.fromAIA.rdata.valid := imsic.o.csr.rdata.valid
  csrMod.fromAIA.rdata.bits.data := imsic.o.csr.rdata.bits.rdata
  csrMod.fromAIA.rdata.bits.illegal := imsic.o.csr.rdata.bits.illegal
  csrMod.fromAIA.mtopei.valid := imsic.o.mtopei.valid
  csrMod.fromAIA.stopei.valid := imsic.o.stopei.valid
  csrMod.fromAIA.vstopei.valid := imsic.o.vstopei.valid
  csrMod.fromAIA.mtopei.bits := imsic.o.mtopei.bits
  csrMod.fromAIA.stopei.bits := imsic.o.stopei.bits
  csrMod.fromAIA.vstopei.bits := imsic.o.vstopei.bits

  private val exceptionVec = WireInit(0.U.asTypeOf(ExceptionVec())) // Todo:
  import ExceptionNO._
  exceptionVec(EX_BP    ) := isEbreak
  exceptionVec(EX_MCALL ) := isEcall && privState.isModeM
  exceptionVec(EX_HSCALL) := isEcall && privState.isModeHS
  exceptionVec(EX_VSCALL) := isEcall && privState.isModeVS
  exceptionVec(EX_UCALL ) := isEcall && privState.isModeHUorVU
  exceptionVec(EX_II    ) := csrMod.io.out.EX_II
  exceptionVec(EX_VI    ) := csrMod.io.out.EX_VI

  val isXRet = valid && func === CSROpType.jmp && !isEcall && !isEbreak

  // ctrl block will use theses later for flush // Todo: optimize isXRetFlag's DelayN
  val isXRetFlag = RegInit(false.B)
  isXRetFlag := Mux1H(Seq(
    DelayN(flush, 5) -> false.B,
    isXRet -> true.B,
  ))

  flushPipe := csrMod.io.out.flushPipe

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
  tlb.hgatp.asid    := csrMod.io.tlb.hgatp.VMID.asUInt
  tlb.hgatp.ppn     := csrMod.io.tlb.hgatp.PPN.asUInt

  // expose several csr bits for tlb
  tlb.priv.mxr := csrMod.io.tlb.mxr
  tlb.priv.sum := csrMod.io.tlb.sum
  tlb.priv.vmxr := csrMod.io.tlb.vmxr
  tlb.priv.vsum := csrMod.io.tlb.vsum
  tlb.priv.spvp := csrMod.io.tlb.spvp
  tlb.priv.virt := csrMod.io.out.privState.V.asBool
  tlb.priv.imode := csrMod.io.tlb.imode
  tlb.priv.dmode := csrMod.io.tlb.dmode

  io.in.ready := true.B // Todo: Async read imsic may block CSR
  io.out.valid := valid
  io.out.bits.ctrl.exceptionVec.get := exceptionVec
  io.out.bits.ctrl.flushPipe.get := flushPipe
  io.out.bits.res.data := csrMod.io.out.rData

  io.out.bits.res.redirect.get.valid := isXRet
  val redirect = io.out.bits.res.redirect.get.bits
  redirect := 0.U.asTypeOf(redirect)
  redirect.level := RedirectLevel.flushAfter
  redirect.robIdx := io.in.bits.ctrl.robIdx
  redirect.ftqIdx := io.in.bits.ctrl.ftqIdx.get
  redirect.ftqOffset := io.in.bits.ctrl.ftqOffset.get
  redirect.cfiUpdate.predTaken := true.B
  redirect.cfiUpdate.taken := true.B
  redirect.cfiUpdate.target := csrMod.io.out.targetPc
  // Only mispred will send redirect to frontend
  redirect.cfiUpdate.isMisPred := true.B

  connect0LatencyCtrlSingal

  // Todo: summerize all difftest skip condition
  csrOut.isPerfCnt  := csrMod.io.out.isPerfCnt && valid && func =/= CSROpType.jmp
  csrOut.fpu.frm    := csrMod.io.out.fpState.frm.asUInt
  csrOut.vpu.vstart := csrMod.io.out.vecState.vstart.asUInt
  csrOut.vpu.vxsat  := csrMod.io.out.vecState.vxsat.asUInt
  csrOut.vpu.vxrm   := csrMod.io.out.vecState.vxrm.asUInt
  csrOut.vpu.vcsr   := csrMod.io.out.vecState.vcsr.asUInt
  csrOut.vpu.vl     := csrMod.io.out.vecState.vl.asUInt
  csrOut.vpu.vtype  := csrMod.io.out.vecState.vtype.asUInt
  csrOut.vpu.vlenb  := csrMod.io.out.vecState.vlenb.asUInt
  csrOut.vpu.vill   := csrMod.io.out.vecState.vtype.asTypeOf(new CSRVTypeBundle).VILL.asUInt
  csrOut.vpu.vma    := csrMod.io.out.vecState.vtype.asTypeOf(new CSRVTypeBundle).VMA.asUInt
  csrOut.vpu.vta    := csrMod.io.out.vecState.vtype.asTypeOf(new CSRVTypeBundle).VTA.asUInt
  csrOut.vpu.vsew   := csrMod.io.out.vecState.vtype.asTypeOf(new CSRVTypeBundle).VSEW.asUInt
  csrOut.vpu.vlmul  := csrMod.io.out.vecState.vtype.asTypeOf(new CSRVTypeBundle).VLMUL.asUInt

  csrOut.isXRet := isXRetFlag

  csrOut.trapTarget := csrMod.io.out.targetPc
  csrOut.interrupt := csrMod.io.out.interrupt
  csrOut.wfi_event := csrMod.io.out.wfiEvent

  csrOut.tlb := tlb

  csrOut.debugMode := csrMod.io.out.debugMode

  // Todo: this bundle should be used in decode.
  // Todo: check permission in decode stage, pass tvm and vtvm only
  csrOut.disableSfence := Mux(
    csrMod.io.out.tvm,
    csrMod.io.out.privState < PrivState.ModeM,
    csrMod.io.out.privState.PRVM < PrivMode.S
  )
  csrOut.disableHfencev := DontCare // Todo
  csrOut.disableHfenceg := DontCare // Todo

  csrOut.customCtrl match {
    case custom =>
      custom.l1I_pf_enable            := csrMod.io.out.custom.l1I_pf_enable
      custom.l2_pf_enable             := csrMod.io.out.custom.l2_pf_enable
      custom.l1D_pf_enable            := csrMod.io.out.custom.l1D_pf_enable
      custom.l1D_pf_train_on_hit      := csrMod.io.out.custom.l1D_pf_train_on_hit
      custom.l1D_pf_enable_agt        := csrMod.io.out.custom.l1D_pf_enable_agt
      custom.l1D_pf_enable_pht        := csrMod.io.out.custom.l1D_pf_enable_pht
      custom.l1D_pf_active_threshold  := csrMod.io.out.custom.l1D_pf_active_threshold
      custom.l1D_pf_active_stride     := csrMod.io.out.custom.l1D_pf_active_stride
      custom.l1D_pf_enable_stride     := csrMod.io.out.custom.l1D_pf_enable_stride
      custom.l2_pf_store_only         := csrMod.io.out.custom.l2_pf_store_only
      // ICache
      custom.icache_parity_enable     := csrMod.io.out.custom.icache_parity_enable
      // Load violation predictor
      custom.lvpred_disable           := csrMod.io.out.custom.lvpred_disable
      custom.no_spec_load             := csrMod.io.out.custom.no_spec_load
      custom.storeset_wait_store      := csrMod.io.out.custom.storeset_wait_store
      custom.storeset_no_fast_wakeup  := csrMod.io.out.custom.storeset_no_fast_wakeup
      custom.lvpred_timeout           := csrMod.io.out.custom.lvpred_timeout
      // Branch predictor
      custom.bp_ctrl                  := csrMod.io.out.custom.bp_ctrl
      // Memory Block
      custom.sbuffer_threshold                := csrMod.io.out.custom.sbuffer_threshold
      custom.ldld_vio_check_enable            := csrMod.io.out.custom.ldld_vio_check_enable
      custom.soft_prefetch_enable             := csrMod.io.out.custom.soft_prefetch_enable
      custom.cache_error_enable               := csrMod.io.out.custom.cache_error_enable
      custom.uncache_write_outstanding_enable := csrMod.io.out.custom.uncache_write_outstanding_enable
      // Rename
      custom.fusion_enable            := csrMod.io.out.custom.fusion_enable
      custom.wfi_enable               := csrMod.io.out.custom.wfi_enable
      // distribute csr write signal
      // write to frontend and memory
      custom.distribute_csr.w.valid := csrWen
      custom.distribute_csr.w.bits.addr := addr
      custom.distribute_csr.w.bits.data := wdata
      // rename single step
      custom.singlestep := csrMod.io.out.singleStepFlag
      // trigger
      custom.frontend_trigger.tUpdate.valid       := csrMod.io.out.frontendTrigger.tUpdate.valid
      custom.frontend_trigger.tUpdate.bits.addr   := csrMod.io.out.frontendTrigger.tUpdate.bits.addr
      custom.frontend_trigger.tUpdate.bits.tdata  := csrMod.io.out.frontendTrigger.tUpdate.bits.tdata
      custom.frontend_trigger.tEnableVec          := csrMod.io.out.frontendTrigger.tEnableVec
      custom.mem_trigger.tUpdate.valid            := csrMod.io.out.memTrigger.tUpdate.valid
      custom.mem_trigger.tUpdate.bits.addr        := csrMod.io.out.memTrigger.tUpdate.bits.addr
      custom.mem_trigger.tUpdate.bits.tdata       := csrMod.io.out.memTrigger.tUpdate.bits.tdata
      custom.mem_trigger.tEnableVec               := csrMod.io.out.memTrigger.tEnableVec
      // virtual mode
      custom.virtMode := csrMod.io.out.privState.V.asBool
  }

  csrToDecode := csrMod.io.toDecode
}

class CSRInput(implicit p: Parameters) extends XSBundle with HasSoCParameter{
  val hartId = Input(UInt(8.W))
  val msiInfo = Input(ValidIO(new MsiInfoBundle))
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
     * decode all fp inst
     * raise EX_II when FS=Off
     */
    val fsIsOff = Bool()

    /**
     * decode all vec inst
     * raise EX_II when VS=Off
     */
    val vsIsOff = Bool()
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
  }
}