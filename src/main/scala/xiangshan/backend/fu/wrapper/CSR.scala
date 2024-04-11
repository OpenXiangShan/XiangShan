package xiangshan.backend.fu.wrapper

import chisel3._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.fu.NewCSR.{CSRPermitModule, NewCSR}
import xiangshan.backend.fu.util._
import xiangshan.backend.fu.{FuConfig, FuncUnit}

class CSR(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg)
{
  val csrIn = io.csrio.get
  val csrOut = io.csrio.get

  val setFsDirty = csrIn.fpu.dirty_fs
  val setFflags = csrIn.fpu.fflags
  val setVsDirty = csrIn.vpu.dirty_vs
  val setVxsat = csrIn.vpu.vxsat

  val flushPipe = Wire(Bool())

  val (valid, src1, src2, func) = (
    io.in.valid,
    io.in.bits.data.src(0),
    io.in.bits.data.imm,
    io.in.bits.ctrl.fuOpType
  )

  // split imm from IMM_Z
  val addr = src2(11, 0)
  val csri = src2(16, 12)

  import CSRConst._

  private val isEcall  = CSROpType.isSystemOp(func) && addr === privEcall
  private val isEbreak = CSROpType.isSystemOp(func) && addr === privEbreak
  private val isMret   = CSROpType.isSystemOp(func) && addr === privMret
  private val isSret   = CSROpType.isSystemOp(func) && addr === privSret
  private val isDret   = CSROpType.isSystemOp(func) && addr === privDret
  private val isWfi    = CSROpType.isWfi(func)

  val permitMod = Module(new CSRPermitModule)
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

  permitMod.io.in.wen       := csrWen
  permitMod.io.in.addr      := addr
  permitMod.io.in.privState := csrMod.io.out.privState

  csrMod.io.in match {
    case in =>
      in.wen := csrWen && permitMod.io.out.legal
      in.ren := csrAccess
      in.addr := addr
      in.wdata := wdata
  }
  csrMod.io.fromMem.excpVA  := csrIn.memExceptionVAddr
  csrMod.io.fromMem.excpGPA := csrIn.memExceptionGPAddr

  csrMod.io.fromRob.trap.valid := csrIn.exception.valid
  csrMod.io.fromRob.trap.bits.pc := csrIn.exception.bits.pc
  csrMod.io.fromRob.trap.bits.instr := csrIn.exception.bits.instr
  csrMod.io.fromRob.trap.bits.trapVec := csrIn.exception.bits.exceptionVec
  csrMod.io.fromRob.trap.bits.singleStep := csrIn.exception.bits.singleStep
  csrMod.io.fromRob.trap.bits.crossPageIPFFix := csrIn.exception.bits.crossPageIPFFix
  csrMod.io.fromRob.trap.bits.isInterrupt := csrIn.exception.bits.isInterrupt

  csrMod.io.fromRob.commit.fflags := setFflags
  csrMod.io.fromRob.commit.fsDirty := setFsDirty
  csrMod.io.fromRob.commit.vxsat := setVxsat
  csrMod.io.fromRob.commit.vsDirty := setVsDirty

  csrMod.io.mret := isMret
  csrMod.io.sret := isSret
  csrMod.io.dret := isDret
  csrMod.io.wfi  := isWfi

  csrMod.platformIRP.MEIP := csrIn.externalInterrupt.meip
  csrMod.platformIRP.MTIP := csrIn.externalInterrupt.mtip
  csrMod.platformIRP.MSIP := csrIn.externalInterrupt.msip
  csrMod.platformIRP.SEIP := csrIn.externalInterrupt.seip
  csrMod.platformIRP.VSEIP := false.B // Todo
  csrMod.platformIRP.VSTIP := false.B // Todo

  private val exceptionVec = WireInit(VecInit(Seq.fill(XLEN)(false.B)))
  import ExceptionNO._
  exceptionVec(EX_BP    ) := isEbreak
  exceptionVec(EX_MCALL ) := isEcall && privState.isModeM
  exceptionVec(EX_HSCALL) := isEcall && privState.isModeHS
  exceptionVec(EX_VSCALL) := isEcall && privState.isModeVS
  exceptionVec(EX_UCALL ) := isEcall && privState.isModeHUorVU
  exceptionVec(EX_II    ) := csrMod.io.out.EX_II
  exceptionVec(EX_VI    ) := csrMod.io.out.EX_VI // Todo: check other EX_VI

  io.in.ready := true.B // Todo: Async read imsic may block CSR
  io.out.valid := valid
  io.out.bits.ctrl.exceptionVec.get := exceptionVec
  io.out.bits.ctrl.flushPipe.get := csrMod.io.out.flushPipe
  io.out.bits.res.data := csrMod.io.out.rData
  connect0LatencyCtrlSingal

  csrOut.isPerfCnt
  csrOut.fpu.frm := csrMod.io.out.frm
  csrOut.vpu.vstart
  csrOut.vpu.vxsat
  csrOut.vpu.vxrm := csrMod.io.out.vxrm
  csrOut.vpu.vcsr
  csrOut.vpu.vl
  csrOut.vpu.vtype
  csrOut.vpu.vlenb
  csrOut.vpu.vill
  csrOut.vpu.vma
  csrOut.vpu.vta
  csrOut.vpu.vsew
  csrOut.vpu.vlmul

  csrOut.isXRet

  csrOut.trapTarget := csrMod.io.out.targetPc
  csrOut.interrupt
  csrOut.wfi_event

  csrOut.tlb

  csrOut.debugMode

  csrOut.disableSfence

  csrOut.customCtrl match {
    case custom =>
      custom.l1I_pf_enable
      custom.l2_pf_enable
      custom.l1D_pf_enable
      custom.l1D_pf_train_on_hit
      custom.l1D_pf_enable_agt
      custom.l1D_pf_enable_pht
      custom.l1D_pf_active_threshold
      custom.l1D_pf_active_stride
      custom.l1D_pf_enable_stride
      custom.l2_pf_store_only
      // ICache
      custom.icache_parity_enable
      // Labeled XiangShan
      custom.dsid
      // Load violation predictor
      custom.lvpred_disable
      custom.no_spec_load
      custom.storeset_wait_store
      custom.storeset_no_fast_wakeup
      custom.lvpred_timeout
      // Branch predictor
      custom.bp_ctrl
      // Memory Block
      custom.sbuffer_threshold
      custom.ldld_vio_check_enable
      custom.soft_prefetch_enable
      custom.cache_error_enable
      custom.uncache_write_outstanding_enable
      // Rename
      custom.fusion_enable
      custom.wfi_enable
      // Decode
      custom.svinval_enable
      // distribute csr write signal
      // write to frontend and memory
      custom.distribute_csr
      // rename single step
      custom.singlestep
      // trigger
      custom.frontend_trigger
      custom.mem_trigger
  }
}
