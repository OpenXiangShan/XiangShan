package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.CSRs
import org.chipsalliance.cde.config.Parameters
import top.{ArgParser, Generator}
import xiangshan.backend.fu.NewCSR.CSRBundles.{PrivState, RobCommitCSR}
import xiangshan.backend.fu.NewCSR.CSRDefines.{ContextStatus, PrivMode, VirtMode}
import xiangshan.backend.fu.NewCSR.CSREnumTypeImplicitCast._
import xiangshan.backend.fu.NewCSR.CSREvents.{CSREvents, DretEventSinkBundle, EventUpdatePrivStateOutput, MretEventSinkBundle, SretEventSinkBundle, TrapEntryEventInput, TrapEntryHSEventSinkBundle, TrapEntryMEventSinkBundle, TrapEntryVSEventSinkBundle}
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.vector.Bundles.{Vl, Vstart, Vxrm, Vxsat}
import xiangshan.{HasXSParameter, XSCoreParamsKey, XSTileKey}

object CSRConfig {
  final val GEILEN = 63

  final val ASIDLEN = 16 // the length of ASID of XS implementation

  final val ASIDMAX = 16 // the max value of ASIDLEN defined by spec

  final val HIIDWidth = 12 // support Hvictl[27:16](IID)

  final val VMIDLEN = 14 // the length of VMID of XS implementation

  final val VMIDMAX = 14 // the max value of VMIDLEN defined by spec

  // the width of VGEIN
  final val VGEINWidth = 6

  final val VaddrMaxWidth = 41 // only Sv39 and Sv39x4

  final val XLEN = 64 // Todo: use XSParams

  final val VLEN = 128

  // Since we need macro to compute the width of CSR field, the input of macro should be the value that can be computed
  // at compile time. The log2Up function cannot be used as meta-programming function, so we use litral value here
  // log2Up(128 + 1), hold 0~128
  final val VlWidth = 8
}

class NewCSR(implicit val p: Parameters) extends Module
  with HasXSParameter
  with MachineLevel
  with SupervisorLevel
  with HypervisorLevel
  with VirtualSupervisorLevel
  with Unprivileged
  with CSRAIA
  with HasExternalInterruptBundle
  with SupervisorMachineAliasConnect
  with CSREvents
  with DebugLevel
  with CSRCustom
{

  import CSRConfig._

  val io = IO(new Bundle {
    val in = Input(new Bundle {
      val wen = Bool()
      val ren = Bool()
      val addr = UInt(12.W)
      val wdata = UInt(64.W)
    })
    val fromMem = Input(new Bundle {
      val excpVA  = UInt(VaddrMaxWidth.W)
      val excpGPA = UInt(VaddrMaxWidth.W) // Todo: use guest physical address width
    })
    val fromRob = Input(new Bundle {
      val trap = ValidIO(new Bundle {
        val pc = UInt(VaddrMaxWidth.W)
        val instr = UInt(32.W)
        val trapVec = UInt(64.W)
        val singleStep = Bool()
        val crossPageIPFFix = Bool()
        val isInterrupt = Bool()
      })
      val commit = Input(new RobCommitCSR)
    })
    val mret = Input(Bool())
    val sret = Input(Bool())
    val dret = Input(Bool())
    val wfi  = Input(Bool())

    val out = Output(new Bundle {
      val EX_II = Bool()
      val EX_VI = Bool()
      val flushPipe = Bool()
      val rData = UInt(64.W)
      val targetPc = UInt(VaddrMaxWidth.W)
      val regOut = UInt(64.W)
      val privState = new PrivState
      val interrupt = Bool()
      val wfi_event = Bool()
      val disableSfence = Bool()
      // fp
      val fpState = new Bundle {
        val off = Bool()
        val frm = Frm()
      }
      // vec
      val vecState = new Bundle {
        val vstart = Vstart()
        val vxsat = Vxsat()
        val vxrm = Vxrm()
        val vcsr = UInt(XLEN.W)
        val vl = Vl()
        val vtype = UInt(XLEN.W)
        val vlenb = UInt(XLEN.W)
        val off = Bool()
      }
      // perf
      val isPerfCnt = Bool()
      // debug
      val debugMode = Bool()
      val singleStepFlag = Bool()
    })
    // tlb
    val tlb = Output(new Bundle {
      val satp = UInt(XLEN.W)
      val mxr = Bool()
      val sum = Bool()
      val imode = UInt(2.W)
      val dmode = UInt(2.W)
    })
    // customCtrl
    val customCtrl = Output(new Bundle {
      val sbpctl = UInt(XLEN.W)
      val spfctl = UInt(XLEN.W)
      val slvpredctl = UInt(XLEN.W)
      val smblockctl = UInt(XLEN.W)
      val srnctl = UInt(XLEN.W)
      val sdsid = UInt(XLEN.W)
      val sfetchctl  = Bool()
    })
  })

  val toAIA   = IO(Output(new CSRToAIABundle))
  val fromAIA = IO(Flipped(Output(new AIAToCSRBundle)))

  dontTouch(toAIA)
  dontTouch(fromAIA)

  val wen   = io.in.wen
  val addr  = io.in.addr
  val wdata = io.in.wdata

  val ren   = io.in.ren
  val raddr = io.in.addr

  val hasTrap = io.fromRob.trap.valid
  val trapVec = io.fromRob.trap.bits.trapVec
  val trapPC = io.fromRob.trap.bits.pc
  val trapIsInterrupt = io.fromRob.trap.bits.isInterrupt
  val trapIsCrossPageIPF = io.fromRob.trap.bits.crossPageIPFFix

  // CSR Privilege State
  val PRVM = RegInit(PrivMode(0), PrivMode.M)
  val V = RegInit(VirtMode(0), VirtMode.Off)
  val debugMode = RegInit(false.B)

  val permitMod = Module(new CSRPermitModule)

  private val wenLegal = permitMod.io.out.hasLegalWen

  val isSret = permitMod.io.out.hasLegalSret
  val isMret = permitMod.io.out.hasLegalMret
  val isDret = io.dret // Todo: check permission
  val isWfi  = io.wfi  // Todo: check permission

  var csrRwMap =
    machineLevelCSRMap ++
    supervisorLevelCSRMap ++
    hypervisorCSRMap ++
    virtualSupervisorCSRMap ++
    unprivilegedCSRMap ++
    debugCSRMap ++
    aiaCSRMap ++
    customCSRMap

  val csrMods =
    machineLevelCSRMods ++
    supervisorLevelCSRMods ++
    hypervisorCSRMods ++
    virtualSupervisorCSRMods ++
    unprivilegedCSRMods ++
    debugCSRMods ++
    aiaCSRMods ++
    customCSRMods

  var csrOutMap =
    machineLevelCSROutMap ++
    supervisorLevelCSROutMap ++
    hypervisorCSROutMap ++
    virtualSupervisorCSROutMap ++
    unprivilegedCSROutMap ++
    debugCSROutMap ++
    aiaCSROutMap ++
    customCSROutMap

  val trapHandleMod = Module(new TrapHandleModule)

  trapHandleMod.io.in.trapInfo.valid := hasTrap
  trapHandleMod.io.in.trapInfo.bits.trapVec := trapVec.asUInt
  trapHandleMod.io.in.trapInfo.bits.isInterrupt := trapIsInterrupt
  trapHandleMod.io.in.privState.PRVM := PRVM
  trapHandleMod.io.in.privState.V := V
  trapHandleMod.io.in.mideleg := mideleg.regOut
  trapHandleMod.io.in.medeleg := medeleg.regOut
  trapHandleMod.io.in.hideleg := hideleg.regOut
  trapHandleMod.io.in.hedeleg := hedeleg.regOut
  trapHandleMod.io.in.mtvec := mtvec.regOut
  trapHandleMod.io.in.stvec := stvec.regOut
  trapHandleMod.io.in.vstvec := vstvec.regOut

  val entryPrivState = trapHandleMod.io.out.entryPrivState

    // interrupt
  val intrMod = Module(new InterruptFilter)
  intrMod.io.in.mstatusMIE := mstatus.rdata.MIE.asBool
  intrMod.io.in.sstatusSIE := mstatus.rdata.SIE.asBool
  intrMod.io.in.vsstatusSIE := vsstatus.rdata.SIE.asBool
  intrMod.io.in.mip := mip.rdata.asUInt
  intrMod.io.in.mie := mie.rdata.asUInt
  intrMod.io.in.mideleg := mideleg.rdata.asUInt
  intrMod.io.in.privState.PRVM := PRVM
  intrMod.io.in.privState.V := V
  intrMod.io.in.hip := hip.rdata.asUInt
  intrMod.io.in.hie := hie.rdata.asUInt
  intrMod.io.in.hideleg := hideleg.rdata.asUInt
  intrMod.io.in.hvictl := hvictl.rdata.asUInt
  intrMod.io.in.hstatus := hstatus.rdata.asUInt
  intrMod.io.in.mtopei := mtopei.rdata.asUInt
  intrMod.io.in.stopei := stopei.rdata.asUInt
  intrMod.io.in.vstopei := vstopei.rdata.asUInt
  intrMod.io.in.hviprio1 := hviprio1.rdata.asUInt
  intrMod.io.in.hviprio2 := hviprio2.rdata.asUInt
  intrMod.io.in.iprios := Cat(iprios.map(iprio => iprio.rdata.asUInt))
  // val disableInterrupt = debugMode || (dcsr.rdata.STEP.asBool && !dcsr.rdata.STEPIE.asBool)
  // val intrVec = Cat(debugIntr && !debugMode, mie.rdata.asUInt(11, 0) & mip.rdata.asUInt & intrVecEnable.asUInt) // Todo: asUInt(11,0) is ok?

  for ((id, (wBundle, _)) <- csrRwMap) {
    wBundle.wen := wenLegal && addr === id.U
    wBundle.wdata := wdata
  }

  // Todo: support set dirty only when fcsr has changed
  private val writeFpState = wenLegal && Seq(CSRs.fflags, CSRs.frm, CSRs.fcsr).map(_.U === addr).reduce(_ || _)
  private val writeVecState = wenLegal && Seq(CSRs.vstart, CSRs.vxsat, CSRs.vxrm, CSRs.vcsr).map(_.U === addr).reduce(_ || _)

  permitMod.io.in.csrAccess.ren := ren
  permitMod.io.in.csrAccess.wen := wen
  permitMod.io.in.csrAccess.addr := addr

  permitMod.io.in.privState.V := V
  permitMod.io.in.privState.PRVM := PRVM

  permitMod.io.in.mret := isMret
  permitMod.io.in.sret := isSret

  permitMod.io.in.status.tsr := mstatus.rdata.TSR.asBool
  permitMod.io.in.status.vtsr := hstatus.rdata.VTSR.asBool

  csrMods.foreach { mod =>
    mod match {
      case m: HypervisorBundle =>
        m.hstatus := hstatus.regOut
        m.hvip := hvip.regOut
        m.hideleg := hideleg.regOut
        m.hedeleg := hedeleg.regOut
        m.hgeip := hgeip.regOut
        m.hgeie := hgeie.regOut
        m.hip := hip.regOut
        m.hie := hie.regOut
      case _ =>
    }
    mod match {
      case m: HasMachineInterruptBundle =>
        m.mvien := mvien.regOut
        m.mvip := mvip.regOut
        m.mip := mip.regOut
        m.mie := mie.regOut
      case _ =>
    }
    mod match {
      case m: HasMachineDelegBundle =>
        m.mideleg := mideleg.regOut
        m.medeleg := medeleg.regOut
      case _ =>
    }
    mod match {
      case m: HasMachineCounterControlBundle =>
        m.mcountinhibit := mcountinhibit.regOut
      case _ =>
    }
    mod match {
      case m: HasExternalInterruptBundle =>
        m.platformIRP := this.platformIRP
      case _ =>
    }
    mod match {
      case m: HasRobCommitBundle =>
        m.robCommit := io.fromRob.commit
        m.robCommit.fsDirty := io.fromRob.commit.fsDirty || writeFpState
        m.robCommit.vsDirty := io.fromRob.commit.vsDirty || writeVecState
      case _ =>
    }
    mod match {
      case m: TrapEntryMEventSinkBundle =>
        m.trapToM := trapEntryMEvent.out
      case _ =>
    }
    mod match {
      case m: TrapEntryHSEventSinkBundle =>
        m.trapToHS := trapEntryHSEvent.out
      case _ =>
    }
    mod match {
      case m: TrapEntryVSEventSinkBundle =>
        m.trapToVS := trapEntryVSEvent.out
      case _ =>
    }
    mod match {
      case m: MretEventSinkBundle =>
        m.retFromM := mretEvent.out
      case _ =>
    }
    mod match {
      case m: SretEventSinkBundle =>
        m.retFromS := sretEvent.out
      case _ =>
    }
    mod match {
      case m: DretEventSinkBundle =>
        m.retFromD := dretEvent.out
      case _ =>
    }
    mod match {
      case m: HasAIABundle =>
        m.aiaToCSR.rdata.valid := fromAIA.rdata.valid
        m.aiaToCSR.rdata.bits.data := fromAIA.rdata.bits.data
        m.aiaToCSR.rdata.bits.illegal := fromAIA.rdata.bits.illegal
        m.aiaToCSR.mtopei.valid := fromAIA.mtopei.valid
        m.aiaToCSR.stopei.valid := fromAIA.stopei.valid
        m.aiaToCSR.vstopei.valid := fromAIA.vstopei.valid
        m.aiaToCSR.mtopei.bits := fromAIA.mtopei.bits
        m.aiaToCSR.stopei.bits := fromAIA.stopei.bits
        m.aiaToCSR.vstopei.bits := fromAIA.vstopei.bits
      case _ =>
    }
    mod match {
      case m: HasInterruptFilterBundle =>
        m.topIn.mtopi  := intrMod.io.out.mtopi
        m.topIn.stopi  := intrMod.io.out.stopi
        m.topIn.vstopi := intrMod.io.out.vstopi
      case _ =>
    }
    mod match {
      case m: HasISelectBundle =>
        m.privState.PRVM := PRVM
        m.privState.V := V
        m.miselect := miselect.regOut
        m.siselect := siselect.regOut
        m.mireg := mireg.regOut.asUInt
        m.sireg := sireg.regOut.asUInt
      case _ =>
    }
  }

  csrMods.foreach { mod =>
    mod.commonIn.status := mstatus.mstatus
    mod.commonIn.prvm := PRVM
    mod.commonIn.v := V
    mod.commonIn.hstatus := hstatus.rdata
    println(s"${mod.modName}: ")
    println(mod.dumpFields)
  }

  trapEntryMEvent.valid  := entryPrivState.isModeM
  trapEntryHSEvent.valid := entryPrivState.isModeHS
  trapEntryVSEvent.valid := entryPrivState.isModeVS

  Seq(trapEntryMEvent, trapEntryHSEvent, trapEntryVSEvent).foreach { eMod =>
    eMod.in match {
      case in: TrapEntryEventInput =>
        in.causeNO := trapHandleMod.io.out.causeNO
        in.trapPc := trapPC
        in.isCrossPageIPF := trapIsCrossPageIPF

        in.iMode.PRVM := PRVM
        in.iMode.V := V
        in.dMode.PRVM := Mux(mstatus.rdata.MPRV.asBool, mstatus.rdata.MPP, PRVM)
        in.dMode.V := Mux(mstatus.rdata.MPRV.asBool, mstatus.rdata.MPV, V)

        in.privState.PRVM := PRVM
        in.privState.V := V
        in.mstatus := mstatus.regOut
        in.hstatus := hstatus.regOut
        in.sstatus := mstatus.sstatus
        in.vsstatus := vsstatus.regOut
        in.pcFromXtvec := trapHandleMod.io.out.pcFromXtvec

        in.satp := satp.rdata
        in.vsatp := vsatp.rdata

        in.memExceptionVAddr := io.fromMem.excpVA
        in.memExceptionGPAddr := io.fromMem.excpGPA
    }
  }

  mretEvent.valid := isMret
  mretEvent.in match {
    case in =>
      in.mstatus := mstatus.regOut
      in.mepc := mepc.regOut
  }

  sretEvent.valid := isSret
  sretEvent.in match {
    case in =>
      in.privState.PRVM := PRVM
      in.privState.V := V
      in.sstatus := mstatus.sstatus
      in.hstatus := hstatus.regOut
      in.vsstatus := vsstatus.regOut
      in.sepc := sepc.regOut
      in.vsepc := vsepc.regOut
  }

  dretEvent.valid := isDret
  dretEvent.in match {
    case in =>
      in.dcsr := dcsr.regOut
      in.dpc  := dpc.regOut
      in.mstatus := mstatus.regOut
  }

  PRVM := MuxCase(
    PRVM,
    events.filter(_.out.isInstanceOf[EventUpdatePrivStateOutput]).map {
      x => x.out match {
        case xx: EventUpdatePrivStateOutput => (xx.privState.valid -> xx.privState.bits.PRVM)
      }
    }
  )

  V := MuxCase(
    V,
    events.filter(_.out.isInstanceOf[EventUpdatePrivStateOutput]).map {
      x => x.out match {
        case xx: EventUpdatePrivStateOutput => (xx.privState.valid -> xx.privState.bits.V)
      }
    }
  )

  // perf
  val addrInPerfCnt = (addr >= CSRs.mcycle.U) && (addr <= CSRs.mhpmcounter31.U) ||
    (addr >= mcountinhibit.addr.U) && (addr <= mhpmevents.last.addr.U) ||
    (addr >= CSRs.cycle.U) && (addr <= CSRs.hpmcounter31.U) ||
    (addr === CSRs.mip.U) ||
    (addr === CSRs.hip.U)
  // Todo: may be vsip and sip

  // flush
  val resetSatp = addr === satp.addr.U && wenLegal // write to satp will cause the pipeline be flushed

  val wFcsrChangeRM = addr === fcsr.addr.U && wenLegal && wdata(7, 5) =/= fcsr.frm
  val wFrmChangeRM  = addr === CSRs.frm.U  && wenLegal && wdata(2, 0) =/= fcsr.frm
  val frmChange = wFcsrChangeRM || wFrmChangeRM

  val wVcsrChangeRM = addr === CSRs.vcsr.U && wenLegal && wdata(2, 1) =/= vcsr.vxrm
  val wVxrmChangeRM = addr === CSRs.vxrm.U && wenLegal && wdata(1, 0) =/= vcsr.vxrm
  val vxrmChange = wVcsrChangeRM || wVxrmChangeRM

  val flushPipe = resetSatp || frmChange || vxrmChange

  // debug
  val debugIntrEnable = RegInit(true.B) // debug interrupt will be handle only when debugIntrEnable
  debugMode := dretEvent.out.debugMode
  debugIntrEnable := dretEvent.out.debugIntrEnable
  val debugIntr = platformIRP.debugIP && debugIntrEnable

  // fence
  // csr access check, special case
  val tvmNotPermit = PRVM === PrivMode.S && mstatus.rdata.TVM.asBool

  private val rdata = Mux1H(csrRwMap.map { case (id, (_, rBundle)) =>
    (raddr === id.U) -> rBundle.asUInt
  })

  private val regOut = Mux1H(csrOutMap.map { case (id, regOut) =>
    (raddr === id.U) -> regOut
  })

  private val hasEvent = mretEvent.out.targetPc.valid || sretEvent.out.targetPc.valid || dretEvent.out.targetPc.valid ||
    trapEntryMEvent.out.targetPc.valid || trapEntryHSEvent.out.targetPc.valid || trapEntryVSEvent.out.targetPc.valid

  io.out.EX_II     := false.B // Todo
  io.out.EX_VI     := false.B // Todo
  io.out.flushPipe := flushPipe

  io.out.rData := Mux(ren, rdata, 0.U)
  io.out.regOut := regOut
  io.out.targetPc := RegEnable(Mux1H(Seq(
    mretEvent.out.targetPc.valid -> mretEvent.out.targetPc.bits,
    sretEvent.out.targetPc.valid -> sretEvent.out.targetPc.bits,
    dretEvent.out.targetPc.valid -> dretEvent.out.targetPc.bits,
    trapEntryMEvent.out.targetPc.valid -> trapEntryMEvent.out.targetPc.bits,
    trapEntryHSEvent.out.targetPc.valid -> trapEntryHSEvent.out.targetPc.bits,
    trapEntryVSEvent.out.targetPc.valid -> trapEntryVSEvent.out.targetPc.bits,
  )), hasEvent)

  io.out.privState.PRVM := PRVM
  io.out.privState.V    := V

  io.out.fpState.frm := fcsr.frm
  io.out.fpState.off := mstatus.rdata.FS === ContextStatus.Off
  io.out.vecState.vstart := vstart.rdata.asUInt
  io.out.vecState.vxsat := vcsr.vxsat
  io.out.vecState.vxrm := vcsr.vxrm
  io.out.vecState.vcsr := vcsr.rdata.asUInt
  io.out.vecState.vl := vl.rdata.asUInt
  io.out.vecState.vtype := vtype.rdata.asUInt // Todo: check correct
  io.out.vecState.vlenb := vlenb.rdata.asUInt
  io.out.vecState.off := mstatus.rdata.VS === ContextStatus.Off
  io.out.isPerfCnt := addrInPerfCnt
  io.out.interrupt := intrMod.io.out.interruptVec.valid
  io.out.wfi_event := debugIntr || (mie.rdata.asUInt & mip.rdata.asUInt).orR
  io.out.debugMode := debugMode
  io.out.disableSfence := tvmNotPermit || PRVM === PrivMode.U
  io.out.singleStepFlag := !debugMode && dcsr.rdata.STEP

  // Todo: record the last address to avoid xireg is different with xiselect
  toAIA.addr.valid := wenLegal && Seq(miselect, siselect, vsiselect).map(
    _.addr.U === addr
  ).reduce(_ || _)
  toAIA.addr.bits.addr := addr
  toAIA.addr.bits.prvm := PRVM
  toAIA.addr.bits.v := V
  toAIA.vgein := hstatus.rdata.VGEIN.asUInt
  toAIA.wdata.valid := wenLegal && Seq(mireg, sireg, vsireg).map(
    _.addr.U === addr
  ).reduce(_ || _)
  toAIA.wdata.bits.data := wdata
  toAIA.mClaim := wenLegal && mtopei.addr.U === addr
  toAIA.sClaim := wenLegal && stopei.addr.U === addr
  toAIA.vsClaim := wenLegal && vstopei.addr.U === addr

  // tlb
  io.tlb.satp := satp.rdata.asUInt
  io.tlb.mxr := mstatus.rdata.MXR.asBool
  io.tlb.sum := mstatus.rdata.SUM.asBool
  io.tlb.imode := PRVM.asUInt
  io.tlb.dmode := Mux((debugMode && dcsr.rdata.MPRVEN.asBool || !debugMode) && mstatus.rdata.MPRV.asBool, mstatus.rdata.MPP.asUInt, PRVM.asUInt)

  // customCtrl
  io.customCtrl.sbpctl := sbpctl.rdata.asUInt
  io.customCtrl.spfctl := spfctl.rdata.asUInt
  io.customCtrl.slvpredctl := slvpredctl.rdata.asUInt
  io.customCtrl.smblockctl := smblockctl.rdata.asUInt
  io.customCtrl.srnctl := srnctl.rdata.asUInt
  io.customCtrl.sdsid := sdsid.rdata.asUInt
  io.customCtrl.sfetchctl := sfetchctl.rdata.ICACHE_PARITY_ENABLE.asBool
}

trait SupervisorMachineAliasConnect { self: NewCSR with MachineLevel with SupervisorLevel =>
  mip.fromMvip := mvip.toMip
  mip.fromSip := sip.toMip
  mie.fromSie := sie.toMie
}

object NewCSRMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform" :+ "--target" :+ "verilog")

  val defaultConfig = config.alterPartial({
    // Get XSCoreParams and pass it to the "small module"
    case XSCoreParamsKey => config(XSTileKey).head
  })

  Generator.execute(
    firrtlOpts :+ "--full-stacktrace" :+ "--target-dir" :+ "backend",
    new NewCSR()(defaultConfig),
    firtoolOpts
  )

  println("done")
}