package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.CSRs
import difftest._
import org.chipsalliance.cde.config.Parameters
import top.{ArgParser, Generator}
import utility.{DataHoldBypass, SignExt, ZeroExt}
import utils.OptionWrapper
import xiangshan.backend.fu.NewCSR.CSRBundles.{CSRCustomState, PrivState, RobCommitCSR}
import xiangshan.backend.fu.NewCSR.CSRDefines.{ContextStatus, PrivMode, SatpMode, VirtMode}
import xiangshan.backend.fu.NewCSR.CSREnumTypeImplicitCast._
import xiangshan.backend.fu.NewCSR.CSREvents.{CSREvents, DretEventSinkBundle, EventUpdatePrivStateOutput, MretEventSinkBundle, SretEventSinkBundle, TrapEntryEventInput, TrapEntryHSEventSinkBundle, TrapEntryMEventSinkBundle, TrapEntryVSEventSinkBundle}
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.vector.Bundles.{Vl, Vstart, Vxrm, Vxsat}
import xiangshan.{HasXSParameter, XSCoreParamsKey, XSTileKey}

import scala.collection.immutable.SeqMap

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

  final val PMPAddrWidth = 36

  final val PMPOffBits = 2

  final val PMPAddrBits = PMPAddrWidth - PMPOffBits

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
  with CSRPMP
{

  import CSRConfig._

  val io = IO(new Bundle {
    val fromTop = Input(new Bundle {
      val hartId = UInt(hartIdLen.W)
    })
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
      val wfiEvent = Bool()
      val tvm = Bool()
      val vtvm = Bool()
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
      // custom
      val custom = new CSRCustomState
    })
    // tlb
    val tlb = Output(new Bundle {
      val satpASIDChanged = Bool()
      val satp = new SatpBundle
      val mxr = Bool()
      val sum = Bool()
      val imode = UInt(2.W)
      val dmode = UInt(2.W)
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

  private val privState = Wire(new PrivState)
  privState.PRVM := PRVM
  privState.V := V

  private val isModeM              = privState.isModeM
  private val (isModeHS, isModeHU) = (privState.isModeHS, privState.isModeHU)
  private val (isModeVS, isModeVU) = (privState.isModeVS, privState.isModeVU)

  val permitMod = Module(new CSRPermitModule)

  private val wenLegal = permitMod.io.out.hasLegalWen

  val legalSret = permitMod.io.out.hasLegalSret
  val legalMret = permitMod.io.out.hasLegalMret
  val isDret = io.dret // Todo: check permission
  val isWfi  = permitMod.io.out.hasLegalWfi

  var csrRwMap: SeqMap[Int, (CSRAddrWriteBundle[_], Data)] =
    machineLevelCSRMap ++
    supervisorLevelCSRMap ++
    hypervisorCSRMap ++
    virtualSupervisorCSRMap ++
    unprivilegedCSRMap ++
    debugCSRMap ++
    aiaCSRMap ++
    customCSRMap ++
    pmpCSRMap

  val csrMods: Seq[CSRModule[_]] =
    machineLevelCSRMods ++
    supervisorLevelCSRMods ++
    hypervisorCSRMods ++
    virtualSupervisorCSRMods ++
    unprivilegedCSRMods ++
    debugCSRMods ++
    aiaCSRMods ++
    customCSRMods ++
    pmpCSRMods

  var csrOutMap: SeqMap[Int, UInt] =
    machineLevelCSROutMap ++
    supervisorLevelCSROutMap ++
    hypervisorCSROutMap ++
    virtualSupervisorCSROutMap ++
    unprivilegedCSROutMap ++
    debugCSROutMap ++
    aiaCSROutMap ++
    customCSROutMap ++
    pmpCSROutMap

  val trapHandleMod = Module(new TrapHandleModule)

  trapHandleMod.io.in.trapInfo.valid := hasTrap
  trapHandleMod.io.in.trapInfo.bits.trapVec := trapVec.asUInt
  trapHandleMod.io.in.trapInfo.bits.isInterrupt := trapIsInterrupt
  trapHandleMod.io.in.privState := privState
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
  intrMod.io.in.privState := privState
  intrMod.io.in.mstatusMIE := mstatus.rdata.MIE.asBool
  intrMod.io.in.sstatusSIE := mstatus.rdata.SIE.asBool
  intrMod.io.in.vsstatusSIE := vsstatus.rdata.SIE.asBool
  intrMod.io.in.mip := mip.rdata.asUInt
  intrMod.io.in.mie := mie.rdata.asUInt
  intrMod.io.in.mideleg := mideleg.rdata.asUInt
  intrMod.io.in.sip := sip.rdata.asUInt
  intrMod.io.in.sie := sie.rdata.asUInt
  intrMod.io.in.hip := hip.rdata.asUInt
  intrMod.io.in.hie := hie.rdata.asUInt
  intrMod.io.in.hideleg := hideleg.rdata.asUInt
  intrMod.io.in.vsip := vsip.rdata.asUInt
  intrMod.io.in.vsie := vsie.rdata.asUInt
  intrMod.io.in.hvictl := hvictl.rdata.asUInt
  intrMod.io.in.hstatus := hstatus.rdata.asUInt
  intrMod.io.in.mtopei := mtopei.rdata.asUInt
  intrMod.io.in.stopei := stopei.rdata.asUInt
  intrMod.io.in.vstopei := vstopei.rdata.asUInt
  intrMod.io.in.hviprio1 := hviprio1.rdata.asUInt
  intrMod.io.in.hviprio2 := hviprio2.rdata.asUInt
  intrMod.io.in.miprios := Cat(miregiprios.map(_.rdata.asInstanceOf[CSRBundle].asUInt).reverse)
  intrMod.io.in.hsiprios := Cat(siregiprios.map(_.rdata.asInstanceOf[CSRBundle].asUInt).reverse)
  // val disableInterrupt = debugMode || (dcsr.rdata.STEP.asBool && !dcsr.rdata.STEPIE.asBool)
  // val intrVec = Cat(debugIntr && !debugMode, mie.rdata.asUInt(11, 0) & mip.rdata.asUInt & intrVecEnable.asUInt) // Todo: asUInt(11,0) is ok?

  // PMP
  val pmpEntryMod = Module(new PMPEntryHandleModule)
  pmpEntryMod.io.in.pmpCfg  := Cat(cfgs.map(_.regOut.asInstanceOf[CSRBundle].asUInt(7, 0)).reverse)
  pmpEntryMod.io.in.pmpAddr := Cat(pmpaddr.map(_.regOut.asInstanceOf[CSRBundle].asUInt(PMPAddrBits-1, 0)).reverse)
  pmpEntryMod.io.in.ren   := ren
  pmpEntryMod.io.in.wen   := wen
  pmpEntryMod.io.in.addr  := addr
  pmpEntryMod.io.in.wdata := wdata

  for ((id, (wBundle, _)) <- csrRwMap) {
    if (vsMapS.contains(id)) {
      // VS access CSR by S: privState.isModeVS && addrMappedToVS === sMapVS(id).U
      wBundle.wen := wenLegal && (isModeVS && addr === vsMapS(id).U) || (!isModeVS && addr === id.U)
      wBundle.wdata := wdata
    } else if (sMapVS.contains(id)) {
      wBundle.wen := wenLegal && !isModeVS && addr === id.U
      wBundle.wdata := wdata
    } else {
      wBundle.wen := wenLegal && addr === id.U
      wBundle.wdata := wdata
    }
  }

  // Todo: support set dirty only when fcsr has changed
  private val writeFpState = wenLegal && Seq(CSRs.fflags, CSRs.frm, CSRs.fcsr).map(_.U === addr).reduce(_ || _)
  private val writeVecState = wenLegal && Seq(CSRs.vstart, CSRs.vxsat, CSRs.vxrm, CSRs.vcsr).map(_.U === addr).reduce(_ || _)

  permitMod.io.in.csrAccess.ren := ren
  permitMod.io.in.csrAccess.wen := wen
  permitMod.io.in.csrAccess.addr := addr

  permitMod.io.in.privState := privState

  permitMod.io.in.mret := io.mret
  permitMod.io.in.sret := io.sret
  permitMod.io.in.wfi  := io.wfi

  permitMod.io.in.status.tsr := mstatus.rdata.TSR.asBool
  permitMod.io.in.status.vtsr := hstatus.rdata.VTSR.asBool

  permitMod.io.in.status.tw := mstatus.rdata.TW.asBool
  permitMod.io.in.status.vtw := hstatus.rdata.VTW.asBool

  miregiprios.foreach { mod =>
    mod.w.wen := (addr === mireg.addr.U) && (miselect.regOut.ALL.asUInt === mod.addr.U)
    mod.w.wdata := wdata
  }

  siregiprios.foreach { mod =>
    mod.w.wen := (addr === sireg.addr.U) && (siselect.regOut.ALL.asUInt === mod.addr.U)
    mod.w.wdata := wdata
  }

  mhartid.hartid := this.io.fromTop.hartId

  cfgs.zipWithIndex.foreach { case (mod, i) =>
    mod.w.wen := wen && (addr === (0x3A0 + i / 8 * 2).U)
    mod.w.wdata := pmpEntryMod.io.out.pmpCfgWData(8*((i%8)+1)-1,8*(i%8))
  }

  pmpaddr.zipWithIndex.foreach{ case(mod, i) =>
    mod.w.wen := wen && (addr === (0x3B0 + i).U)
    mod.w.wdata := pmpEntryMod.io.out.pmpAddrWData(i)
  }

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
      case m: HasInterruptFilterSink =>
        m.topIR.mtopi  := intrMod.io.out.mtopi
        m.topIR.stopi  := intrMod.io.out.stopi
        m.topIR.vstopi := intrMod.io.out.vstopi
      case _ =>
    }
    mod match {
      case m: HasISelectBundle =>
        m.privState := privState
        m.miselect := miselect.regOut
        m.siselect := siselect.regOut
        m.mireg := mireg.regOut.asUInt
        m.sireg := sireg.regOut.asUInt
      case _ =>
    }
    mod match {
      case m: HasPMPAddrSink =>
        m.addrRData := pmpEntryMod.io.out.pmpAddrRData
      case _ =>
    }
  }

  csrMods.foreach { mod =>
    println(s"${mod.modName}: ")
    println(mod.dumpFields)
  }

  trapEntryMEvent .valid := hasTrap && entryPrivState.isModeM
  trapEntryHSEvent.valid := hasTrap && entryPrivState.isModeHS
  trapEntryVSEvent.valid := hasTrap && entryPrivState.isModeVS

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

        in.privState := privState
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

  mretEvent.valid := legalMret
  mretEvent.in match {
    case in =>
      in.mstatus := mstatus.regOut
      in.mepc := mepc.regOut
  }

  sretEvent.valid := legalSret
  sretEvent.in match {
    case in =>
      in.privState := privState
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

  debugMode := MuxCase(
    debugMode,
    Seq(
      dretEvent.out.debugMode.valid -> dretEvent.out.debugMode.bits
    )
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
  debugIntrEnable := dretEvent.out.debugIntrEnable
  val debugIntr = platformIRP.debugIP && debugIntrEnable

  // fence
  val tvm = mstatus.rdata.TVM.asBool
  val vtvm = hstatus.rdata.VTVM.asBool

  private val rdata = Mux1H(csrRwMap.map { case (id, (_, rBundle)) =>
    (raddr === id.U) -> rBundle.asUInt
  })

  private val regOut = Mux1H(csrOutMap.map { case (id, regOut) =>
    (raddr === id.U) -> regOut
  })

  private val needTargetUpdate = mretEvent.out.targetPc.valid || sretEvent.out.targetPc.valid || dretEvent.out.targetPc.valid ||
    trapEntryMEvent.out.targetPc.valid || trapEntryHSEvent.out.targetPc.valid || trapEntryVSEvent.out.targetPc.valid

  private val noCSRIllegal = (ren || wen) && Cat(csrRwMap.keys.toSeq.sorted.map(csrAddr => !(addr === csrAddr.U))).andR

  io.out.EX_II := permitMod.io.out.EX_II || noCSRIllegal
  io.out.EX_VI := permitMod.io.out.EX_VI
  io.out.flushPipe := flushPipe

  io.out.rData := Mux(ren, rdata, 0.U)
  io.out.regOut := regOut
  io.out.targetPc := DataHoldBypass(Mux1H(Seq(
    mretEvent.out.targetPc.valid -> mretEvent.out.targetPc.bits,
    sretEvent.out.targetPc.valid -> sretEvent.out.targetPc.bits,
    dretEvent.out.targetPc.valid -> dretEvent.out.targetPc.bits,
    trapEntryMEvent.out.targetPc.valid -> trapEntryMEvent.out.targetPc.bits,
    trapEntryHSEvent.out.targetPc.valid -> trapEntryHSEvent.out.targetPc.bits,
    trapEntryVSEvent.out.targetPc.valid -> trapEntryVSEvent.out.targetPc.bits,
  )), needTargetUpdate)

  io.out.privState := privState

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
  io.out.wfiEvent := debugIntr || (mie.rdata.asUInt & mip.rdata.asUInt).orR
  io.out.debugMode := debugMode
  io.out.singleStepFlag := !debugMode && dcsr.rdata.STEP
  io.out.tvm := tvm
  io.out.vtvm := vtvm

  /**
   * [[io.out.custom]] connection
   */
  io.out.custom.l1I_pf_enable           := spfctl.rdata.L1I_PF_ENABLE.asBool
  io.out.custom.l2_pf_enable            := spfctl.rdata.L2_PF_ENABLE.asBool
  io.out.custom.l1D_pf_enable           := spfctl.rdata.L1D_PF_ENABLE.asBool
  io.out.custom.l1D_pf_train_on_hit     := spfctl.rdata.L1D_PF_TRAIN_ON_HIT.asBool
  io.out.custom.l1D_pf_enable_agt       := spfctl.rdata.L1D_PF_ENABLE_AGT.asBool
  io.out.custom.l1D_pf_enable_pht       := spfctl.rdata.L1D_PF_ENABLE_PHT.asBool
  io.out.custom.l1D_pf_active_threshold := spfctl.rdata.L1D_PF_ACTIVE_THRESHOLD.asUInt
  io.out.custom.l1D_pf_active_stride    := spfctl.rdata.L1D_PF_ACTIVE_STRIDE.asUInt
  io.out.custom.l1D_pf_enable_stride    := spfctl.rdata.L1D_PF_ENABLE_STRIDE.asBool
  io.out.custom.l2_pf_store_only        := spfctl.rdata.L2_PF_STORE_ONLY.asBool

  io.out.custom.icache_parity_enable    := sfetchctl.rdata.ICACHE_PARITY_ENABLE.asBool

  io.out.custom.lvpred_disable          := slvpredctl.rdata.LVPRED_DISABLE.asBool
  io.out.custom.no_spec_load            := slvpredctl.rdata.NO_SPEC_LOAD.asBool
  io.out.custom.storeset_wait_store     := slvpredctl.rdata.STORESET_WAIT_STORE.asBool
  io.out.custom.storeset_no_fast_wakeup := slvpredctl.rdata.STORESET_NO_FAST_WAKEUP.asBool
  io.out.custom.lvpred_timeout          := slvpredctl.rdata.LVPRED_TIMEOUT.asUInt

  io.out.custom.bp_ctrl.ubtb_enable     := sbpctl.rdata.UBTB_ENABLE .asBool
  io.out.custom.bp_ctrl.btb_enable      := sbpctl.rdata.BTB_ENABLE  .asBool
  io.out.custom.bp_ctrl.bim_enable      := sbpctl.rdata.BIM_ENABLE  .asBool
  io.out.custom.bp_ctrl.tage_enable     := sbpctl.rdata.TAGE_ENABLE .asBool
  io.out.custom.bp_ctrl.sc_enable       := sbpctl.rdata.SC_ENABLE   .asBool
  io.out.custom.bp_ctrl.ras_enable      := sbpctl.rdata.RAS_ENABLE  .asBool
  io.out.custom.bp_ctrl.loop_enable     := sbpctl.rdata.LOOP_ENABLE .asBool

  io.out.custom.sbuffer_threshold                := smblockctl.rdata.SBUFFER_THRESHOLD.asUInt
  io.out.custom.ldld_vio_check_enable            := smblockctl.rdata.LDLD_VIO_CHECK_ENABLE.asBool
  io.out.custom.soft_prefetch_enable             := smblockctl.rdata.SOFT_PREFETCH_ENABLE.asBool
  io.out.custom.cache_error_enable               := smblockctl.rdata.CACHE_ERROR_ENABLE.asBool
  io.out.custom.uncache_write_outstanding_enable := smblockctl.rdata.UNCACHE_WRITE_OUTSTANDING_ENABLE.asBool

  io.out.custom.fusion_enable           := srnctl.rdata.FUSION_ENABLE.asBool
  io.out.custom.wfi_enable              := srnctl.rdata.WFI_ENABLE.asBool
  io.out.custom.svinval_enable          := srnctl.rdata.SVINVAL_ENABLE.asBool

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
  io.tlb.satpASIDChanged := wenLegal && addr === CSRs.satp.U && satp.rdata.ASID =/= wdata.asTypeOf(new SatpBundle).ASID
  io.tlb.satp := satp.rdata
  io.tlb.mxr := mstatus.rdata.MXR.asBool
  io.tlb.sum := mstatus.rdata.SUM.asBool
  io.tlb.imode := PRVM.asUInt
  io.tlb.dmode := Mux((debugMode && dcsr.rdata.MPRVEN.asBool || !debugMode) && mstatus.rdata.MPRV.asBool, mstatus.rdata.MPP.asUInt, PRVM.asUInt)

  // Always instantiate basic difftest modules.
  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    val hartId = io.fromTop.hartId
    val trapValid = io.fromRob.trap.valid
    val trapNO = trapHandleMod.io.out.causeNO.ExceptionCode.asUInt
    val interrupt = trapHandleMod.io.out.causeNO.Interrupt.asBool
    val interruptNO = Mux(interrupt, trapNO, 0.U)
    val exceptionNO = Mux(!interrupt, trapNO, 0.U)
    val ivmHS = isModeHS && satp.rdata.MODE =/= SatpMode.Bare
    val ivmVS = isModeVS && vsatp.rdata.MODE =/= SatpMode.Bare
    // When enable virtual memory, the higher bit should fill with the msb of address of Sv39/Sv48/Sv57
    val exceptionPC = Mux(ivmHS || ivmVS, SignExt(trapPC, XLEN), ZeroExt(trapPC, XLEN))

    val diffArchEvent = DifftestModule(new DiffArchEvent, delay = 3, dontCare = true)
    diffArchEvent.coreid := hartId
    diffArchEvent.valid := trapValid
    diffArchEvent.interrupt := interruptNO
    diffArchEvent.exception := exceptionNO
    diffArchEvent.exceptionPC := exceptionPC
    if (env.EnableDifftest) {
      diffArchEvent.exceptionInst := io.fromRob.trap.bits.instr
    }

    val diffCSRState = DifftestModule(new DiffCSRState)
    diffCSRState.coreid         := hartId
    diffCSRState.privilegeMode  := privState.PRVM.asUInt
    diffCSRState.mstatus        := mstatus.rdata.asUInt
    diffCSRState.sstatus        := mstatus.sstatus.asUInt
    diffCSRState.mepc           := mepc.rdata.asUInt
    diffCSRState.sepc           := sepc.rdata.asUInt
    diffCSRState.mtval          := mtval.rdata.asUInt
    diffCSRState.stval          := stval.rdata.asUInt
    diffCSRState.mtvec          := mtvec.rdata.asUInt
    diffCSRState.stvec          := stvec.rdata.asUInt
    diffCSRState.mcause         := mcause.rdata.asUInt
    diffCSRState.scause         := scause.rdata.asUInt
    diffCSRState.satp           := satp.rdata.asUInt
    diffCSRState.mip            := mip.regOut.asUInt
    diffCSRState.mie            := mie.rdata.asUInt
    diffCSRState.mscratch       := mscratch.rdata.asUInt
    diffCSRState.sscratch       := sscratch.rdata.asUInt
    diffCSRState.mideleg        := mideleg.rdata.asUInt
    diffCSRState.medeleg        := medeleg.rdata.asUInt

    val diffDebugMode = DifftestModule(new DiffDebugMode)
    diffDebugMode.coreid    := hartId
    diffDebugMode.debugMode := debugMode
    diffDebugMode.dcsr      := dcsr.rdata.asUInt
    diffDebugMode.dpc       := dpc.rdata.asUInt
    diffDebugMode.dscratch0 := dscratch0.rdata.asUInt
    diffDebugMode.dscratch1 := dscratch1.rdata.asUInt

    val diffVecCSRState = DifftestModule(new DiffVecCSRState)
    diffVecCSRState.coreid := hartId
    diffVecCSRState.vstart := vstart.rdata.asUInt
    diffVecCSRState.vxsat := vcsr.vxsat.asUInt
    diffVecCSRState.vxrm := vcsr.vxrm.asUInt
    diffVecCSRState.vcsr := vcsr.rdata.asUInt
    diffVecCSRState.vl := vl.rdata.asUInt
    diffVecCSRState.vtype := vtype.rdata.asUInt
    diffVecCSRState.vlenb := vlenb.rdata.asUInt

    val diffHCSRState = DifftestModule(new DiffHCSRState)
    diffHCSRState.coreid      := hartId
    diffHCSRState.virtMode    := privState.V.asBool
    diffHCSRState.mtval2      := mtval2.rdata.asUInt
    diffHCSRState.mtinst      := mtinst.rdata.asUInt
    diffHCSRState.hstatus     := hstatus.rdata.asUInt
    diffHCSRState.hideleg     := hideleg.rdata.asUInt
    diffHCSRState.hedeleg     := hedeleg.rdata.asUInt
    diffHCSRState.hcounteren  := hcounteren.rdata.asUInt
    diffHCSRState.htval       := htval.rdata.asUInt
    diffHCSRState.htinst      := htinst.rdata.asUInt
    diffHCSRState.hgatp       := hgatp.rdata.asUInt
    diffHCSRState.vsstatus    := vsstatus.rdata.asUInt
    diffHCSRState.vstvec      := vstvec.rdata.asUInt
    diffHCSRState.vsepc       := vsepc.rdata.asUInt
    diffHCSRState.vscause     := vscause.rdata.asUInt
    diffHCSRState.vstval      := vstval.rdata.asUInt
    diffHCSRState.vsatp       := vsatp.rdata.asUInt
    diffHCSRState.vsscratch   := vsscratch.rdata.asUInt

  }
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