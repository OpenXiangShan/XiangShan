package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.CSRs
import difftest._
import org.chipsalliance.cde.config.Parameters
import top.{ArgParser, Generator}
import utility.{DataHoldBypass, GatedValidRegNext, SignExt, ZeroExt}
import utils.OptionWrapper
import xiangshan.backend.fu.NewCSR.CSRBundles.{CSRCustomState, PrivState, RobCommitCSR}
import xiangshan.backend.fu.NewCSR.CSRDefines.{ContextStatus, PrivMode, SatpMode, VirtMode}
import xiangshan.backend.fu.NewCSR.CSREnumTypeImplicitCast._
import xiangshan.backend.fu.NewCSR.CSREvents.{CSREvents, DretEventSinkBundle, EventUpdatePrivStateOutput, MretEventSinkBundle, SretEventSinkBundle, TrapEntryDEventSinkBundle, TrapEntryEventInput, TrapEntryHSEventSinkBundle, TrapEntryMEventSinkBundle, TrapEntryVSEventSinkBundle}
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.vector.Bundles.{Vl, Vstart, Vxrm, Vxsat}
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.{FrontendTdataDistributeIO, HasXSParameter, MemTdataDistributeIO, XSCoreParamsKey, XSTileKey}
import xiangshan._
import xiangshan.backend.fu.util.CSRConst

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

  final val PAddrWidth = 36

  final val AddrWidthInPage = 12

  final val PMPAddrWidth = 36

  final val PMPOffBits = 2

  final val PMPAddrBits = PMPAddrWidth - PMPOffBits

  // trigger
  final val triggerNum    = 4     // Todo: use XSParams
  final val tselectWidth  = 2     // log2Up(triggerNum)

  final val EXT_SSTC = true
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
  with CSREvents
  with DebugLevel
  with CSRCustom
  with CSRPMP
  with IpIeAliasConnect
{

  import CSRConfig._

  val io = IO(new Bundle {
    val fromTop = Input(new Bundle {
      val hartId = UInt(hartIdLen.W)
      val clintTime = Input(ValidIO(UInt(64.W)))
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
        val triggerCf = new TriggerCf
        val crossPageIPFFix = Bool()
        val isInterrupt = Bool()
      })
      val commit = Input(new RobCommitCSR)
    })
    val mret = Input(Bool())
    val sret = Input(Bool())
    val dret = Input(Bool())
    val wfi  = Input(Bool())
    val ebreak = Input(Bool())

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
      // trigger
      val frontendTrigger = new FrontendTdataDistributeIO()
      val memTrigger = new MemTdataDistributeIO()
      // custom
      val custom = new CSRCustomState
    })
    // tlb
    val tlb = Output(new Bundle {
      val satpASIDChanged = Bool()
      val vsatpASIDChanged = Bool()
      val hgatpVMIDChanged = Bool()
      val satp = new SatpBundle
      val vsatp = new SatpBundle
      val hgatp = new HgatpBundle
      val mxr = Bool()
      val sum = Bool()
      val vmxr = Bool()
      val vsum = Bool()
      val spvp = Bool()
      val imode = UInt(2.W)
      val dmode = UInt(2.W)
    })

    val toDecode = new CSRToDecode
  })

  val toAIA   = IO(Output(new CSRToAIABundle))
  val fromAIA = IO(Flipped(Output(new AIAToCSRBundle)))

  dontTouch(toAIA)
  dontTouch(fromAIA)
  dontTouch(io.fromTop.clintTime)

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
  val triggerCf = io.fromRob.trap.bits.triggerCf

  // debug_intrrupt
  val debugIntrEnable = RegInit(true.B) // debug interrupt will be handle only when debugIntrEnable
  val debugIntr = platformIRP.debugIP && debugIntrEnable

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
  val sstcIRGen = Module(new SstcInterruptGen)

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

  // interrupt
  val intrMod = Module(new InterruptFilter)
  intrMod.io.in.privState := privState
  intrMod.io.in.mstatusMIE := mstatus.regOut.MIE.asBool
  intrMod.io.in.sstatusSIE := mstatus.regOut.SIE.asBool
  intrMod.io.in.vsstatusSIE := vsstatus.regOut.SIE.asBool
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
  intrMod.io.in.miprios := Cat(miregiprios.map(_.rdata).reverse)
  intrMod.io.in.hsiprios := Cat(siregiprios.map(_.rdata).reverse)
  // val disableInterrupt = debugMode || (dcsr.rdata.STEP.asBool && !dcsr.rdata.STEPIE.asBool)
  // val intrVec = Cat(debugIntr && !debugMode, mie.rdata.asUInt(11, 0) & mip.rdata.asUInt & intrVecEnable.asUInt) // Todo: asUInt(11,0) is ok?

  val intrVec = RegEnable(intrMod.io.out.interruptVec.bits, 0.U, intrMod.io.out.interruptVec.valid)

  val trapHandleMod = Module(new TrapHandleModule)

  trapHandleMod.io.in.trapInfo.valid := hasTrap
  trapHandleMod.io.in.trapInfo.bits.trapVec := trapVec.asUInt
  trapHandleMod.io.in.trapInfo.bits.intrVec := intrVec
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

  // PMP
  val pmpEntryMod = Module(new PMPEntryHandleModule)
  pmpEntryMod.io.in.pmpCfg  := cfgs.map(_.regOut.asInstanceOf[PMPCfgBundle])
  pmpEntryMod.io.in.pmpAddr := pmpaddr.map(_.regOut.asInstanceOf[PMPAddrBundle])
  pmpEntryMod.io.in.ren   := ren
  pmpEntryMod.io.in.wen   := wen
  pmpEntryMod.io.in.addr  := addr
  pmpEntryMod.io.in.wdata := wdata

  for ((id, (wBundle, _)) <- csrRwMap) {
    if (vsMapS.contains(id)) {
      // VS access CSR by S: privState.isModeVS && addrMappedToVS === sMapVS(id).U
      wBundle.wen := wenLegal && ((isModeVS && addr === vsMapS(id).U) || (!isModeVS && addr === id.U))
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
  permitMod.io.in.debugMode := debugMode

  permitMod.io.in.mret := io.mret
  permitMod.io.in.sret := io.sret
  permitMod.io.in.wfi  := io.wfi
  permitMod.io.in.csrIsCustom := customCSRMods.map(_.addr.U === addr).reduce(_ || _).orR

  permitMod.io.in.status.tsr := mstatus.regOut.TSR.asBool
  permitMod.io.in.status.vtsr := hstatus.regOut.VTSR.asBool

  permitMod.io.in.status.tw := mstatus.regOut.TW.asBool
  permitMod.io.in.status.vtw := hstatus.regOut.VTW.asBool

  permitMod.io.in.status.tvm  := mstatus.regOut.TVM.asBool
  permitMod.io.in.status.vtvm := hstatus.regOut.VTVM.asBool

  permitMod.io.in.status.mcounteren := mcounteren.rdata
  permitMod.io.in.status.hcounteren := mcounteren.rdata
  permitMod.io.in.status.scounteren := mcounteren.rdata

  permitMod.io.in.status.menvcfg := menvcfg.rdata
  permitMod.io.in.status.henvcfg := henvcfg.rdata

  sstcIRGen.i.stime.valid := time.updated
  sstcIRGen.i.stime.bits  := time.stime
  sstcIRGen.i.vstime.valid := time.updated
  sstcIRGen.i.vstime.bits  := time.vstime
  sstcIRGen.i.stimecmp := stimecmp.rdata
  sstcIRGen.i.vstimecmp := vstimecmp.rdata
  sstcIRGen.i.menvcfgSTCE := menvcfg.regOut.STCE.asBool
  sstcIRGen.i.henvcfgSTCE := henvcfg.regOut.STCE.asBool

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
      case _ =>
    }
    mod match {
      case m: VirtualSupervisorBundle =>
        m.v := V.asUInt.asBool
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
        m.platformIRP.STIP  := sstcIRGen.o.STIP
        m.platformIRP.VSTIP := sstcIRGen.o.VSTIP
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
      case m: TrapEntryDEventSinkBundle =>
        m.trapToD := trapEntryDEvent.out
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
    mod match {
      case m: HasMHPMSink =>
        // cycle from mcycle
        m.mHPM.cycle := mcycle.rdata
        // time from clint
        m.mHPM.time  := io.fromTop.clintTime
        // instret from minstret
        m.mHPM.instret := minstret.rdata
        // VS-Mode or VU-Mode
        m.v := privState.isVirtual
        m.htimedelta := htimedelta.rdata
      case _ =>
    }
    mod match {
      case m: HasMachineEnvBundle =>
        m.menvcfg := menvcfg.regOut
      case _ =>
    }
    mod match {
      case m: HasHypervisorEnvBundle =>
        m.menvcfg := menvcfg.regOut
        m.privState := privState
        m.accessStimecmp := (ren || wen) && (addr === CSRs.stimecmp.U || addr === CSRs.vstimecmp.U)
      case _ =>
    }
    mod match {
      case m: HasIpIeBundle =>
        m.mideleg := mideleg.regOut
        m.mip := mip.regOut
        m.mie := mie.regOut
        m.mvip := mvip.regOut
        m.mvien := mvien.regOut
        m.hideleg := hideleg.regOut
        m.hip := hip.regOut
        m.hie := hie.regOut
        m.hvien := hvien.regOut
        m.hvip := hvip.regOut
        m.sip := sip.regOut
        m.sie := sie.regOut
        m.vsip := vsip.regOut
        m.vsie := vsie.regOut
        m.hgeip := hgeip.regOut
        m.hgeie := hgeie.regOut
        m.hstatusVGEIN := hstatus.regOut.VGEIN
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

  Seq(trapEntryMEvent, trapEntryHSEvent, trapEntryVSEvent, trapEntryDEvent).foreach { eMod =>
    eMod.in match {
      case in: TrapEntryEventInput =>
        in.causeNO := trapHandleMod.io.out.causeNO
        in.trapPc := trapPC
        in.isCrossPageIPF := trapIsCrossPageIPF

        in.iMode.PRVM := PRVM
        in.iMode.V := V
        in.dMode.PRVM := Mux(mstatus.regOut.MPRV.asBool, mstatus.regOut.MPP, PRVM)
        in.dMode.V := Mux(mstatus.regOut.MPRV.asBool, mstatus.regOut.MPV, V)

        in.privState := privState
        in.mstatus := mstatus.regOut
        in.hstatus := hstatus.regOut
        in.sstatus := mstatus.sstatus
        in.vsstatus := vsstatus.regOut
        in.pcFromXtvec := trapHandleMod.io.out.pcFromXtvec
        in.tcontrol := tcontrol.regOut

        in.satp  := satp.regOut
        in.vsatp := vsatp.regOut
        in.hgatp := hgatp.regOut

        in.memExceptionVAddr := io.fromMem.excpVA
        in.memExceptionGPAddr := io.fromMem.excpGPA
    }
  }

  mretEvent.valid := legalMret
  mretEvent.in match {
    case in =>
      in.mstatus := mstatus.regOut
      in.mepc := mepc.regOut
      in.tcontrol := tcontrol.regOut
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
      dretEvent.out.debugMode.valid -> dretEvent.out.debugMode.bits,
      trapEntryDEvent.out.debugMode.valid -> trapEntryDEvent.out.debugMode.bits
    )
  )

  debugIntrEnable := MuxCase(
    debugIntrEnable,
    Seq(
      dretEvent.out.debugIntrEnable.valid -> dretEvent.out.debugIntrEnable.bits,
      trapEntryDEvent.out.debugIntrEnable.valid -> trapEntryDEvent.out.debugIntrEnable.bits
    )
  )

  // perf
  val addrInPerfCnt = (addr >= CSRs.mcycle.U) && (addr <= CSRs.mhpmcounter31.U) ||
    (addr >= mcountinhibit.addr.U) && (addr <= mhpmevents.last.addr.U) ||
    (addr >= CSRs.cycle.U) && (addr <= CSRs.hpmcounter31.U) ||
    (addr === CSRs.mip.U) ||
    (addr === CSRs.hip.U) ||
    Cat(aiaCSRMap.keys.toSeq.sorted.map(_.U === addr)).orR ||
    (addr === CSRs.stimecmp.U) ||
    (addr === CSRs.mcounteren.U) ||
    (addr === CSRs.scounteren.U) ||
    (addr === CSRs.menvcfg.U)
  // Todo: may be vsip and sip

  // flush
  val resetSatp = Cat(Seq(satp, vsatp, hgatp).map(_.addr.U === addr)).orR && wenLegal // write to satp will cause the pipeline be flushed

  val wFcsrChangeRM = addr === fcsr.addr.U && wenLegal && wdata(7, 5) =/= fcsr.frm
  val wFrmChangeRM  = addr === CSRs.frm.U  && wenLegal && wdata(2, 0) =/= fcsr.frm
  val frmChange = wFcsrChangeRM || wFrmChangeRM

  val wVcsrChangeRM = addr === CSRs.vcsr.U && wenLegal && wdata(2, 1) =/= vcsr.vxrm
  val wVxrmChangeRM = addr === CSRs.vxrm.U && wenLegal && wdata(1, 0) =/= vcsr.vxrm
  val vxrmChange = wVcsrChangeRM || wVxrmChangeRM

  val wMstatusChangeVS = addr === CSRs.mstatus.U && wenLegal && ((mstatus.regOut.VS === ContextStatus.Off && wdata(10, 9) =/= ContextStatus.Off.asUInt) ||
                                                                 (mstatus.regOut.VS =/= ContextStatus.Off && wdata(10, 9) === ContextStatus.Off.asUInt))
  val wMstatusChangeFS = addr === CSRs.mstatus.U && wenLegal && ((mstatus.regOut.FS === ContextStatus.Off && wdata(14, 13) =/= ContextStatus.Off.asUInt) ||
                                                                 (mstatus.regOut.FS =/= ContextStatus.Off && wdata(14, 13) === ContextStatus.Off.asUInt))
  val mstatusChange = wMstatusChangeVS || wMstatusChangeFS

  val triggerFrontendChange = Wire(Bool())
  val flushPipe = resetSatp || frmChange || vxrmChange || triggerFrontendChange || mstatusChange

  // fence
  val tvm = mstatus.regOut.TVM.asBool
  val vtvm = hstatus.regOut.VTVM.asBool

  private val rdata = Mux1H(csrRwMap.map { case (id, (_, rBundle)) =>
    (raddr === id.U) -> rBundle.asUInt
  })

  private val regOut = Mux1H(csrOutMap.map { case (id, regOut) =>
    (raddr === id.U) -> regOut
  })

  private val needTargetUpdate = mretEvent.out.targetPc.valid || sretEvent.out.targetPc.valid || dretEvent.out.targetPc.valid ||
    trapEntryMEvent.out.targetPc.valid || trapEntryHSEvent.out.targetPc.valid || trapEntryVSEvent.out.targetPc.valid || trapEntryDEvent.out.targetPc.valid

  private val noCSRIllegal = (ren || wen) && Cat(csrRwMap.keys.toSeq.sorted.map(csrAddr => !(addr === csrAddr.U))).andR

  io.out.EX_II := permitMod.io.out.EX_II || noCSRIllegal
  io.out.EX_VI := permitMod.io.out.EX_VI
  io.out.flushPipe := flushPipe

  io.out.rData := Mux(ren, rdata, 0.U)
  io.out.regOut := regOut
  io.out.targetPc := DataHoldBypass(
    Mux(trapEntryDEvent.out.targetPc.valid,
      trapEntryDEvent.out.targetPc.bits,
      Mux1H(Seq(
        mretEvent.out.targetPc.valid -> mretEvent.out.targetPc.bits,
        sretEvent.out.targetPc.valid -> sretEvent.out.targetPc.bits,
        dretEvent.out.targetPc.valid -> dretEvent.out.targetPc.bits,
        trapEntryMEvent.out.targetPc.valid -> trapEntryMEvent.out.targetPc.bits,
        trapEntryHSEvent.out.targetPc.valid -> trapEntryHSEvent.out.targetPc.bits,
        trapEntryVSEvent.out.targetPc.valid -> trapEntryVSEvent.out.targetPc.bits)
      )
    ),
  needTargetUpdate)

  io.out.privState := privState

  io.out.fpState.frm := fcsr.frm
  io.out.fpState.off := mstatus.regOut.FS === ContextStatus.Off
  io.out.vecState.vstart := vstart.rdata.asUInt
  io.out.vecState.vxsat := vcsr.vxsat
  io.out.vecState.vxrm := vcsr.vxrm
  io.out.vecState.vcsr := vcsr.rdata.asUInt
  io.out.vecState.vl := vl.rdata.asUInt
  io.out.vecState.vtype := vtype.rdata.asUInt // Todo: check correct
  io.out.vecState.vlenb := vlenb.rdata.asUInt
  io.out.vecState.off := mstatus.regOut.VS === ContextStatus.Off
  io.out.isPerfCnt := addrInPerfCnt
  io.out.interrupt := intrMod.io.out.interruptVec.valid
  io.out.wfiEvent := debugIntr || (mie.rdata.asUInt & mip.rdata.asUInt).orR
  io.out.debugMode := debugMode
  io.out.singleStepFlag := !debugMode && dcsr.regOut.STEP
  io.out.tvm := tvm
  io.out.vtvm := vtvm

  /**
   * debug_begin
   *
   * ways to entry Dmode：
   *    1. debug intr(from external debug module)
   *    2. ebreak inst in nonDmode
   *    3. trigger fire in nonDmode
   *    4. single step(debug module set dcsr.step before hart resume)
   */
  // debug_intr
  val hasIntr = hasTrap && trapIsInterrupt
  val hasDebugIntr = hasIntr && intrVec(CSRConst.IRQ_DEBUG)

  // debug_exception_ebreak
  val hasExp = hasTrap && !trapIsInterrupt
  val breakPoint = trapVec(ExceptionNO.breakPoint).asBool
  val hasBreakPoint = hasExp && breakPoint
  val ebreakEnterDebugMode =
    (privState.isModeM && dcsr.regOut.EBREAKM.asBool) ||
      (privState.isModeHS && dcsr.regOut.EBREAKS.asBool) ||
      (privState.isModeHU && dcsr.regOut.EBREAKU.asBool)
  val hasDebugEbreakException = hasBreakPoint && ebreakEnterDebugMode

  // debug_exception_trigger
  val triggerFrontendHitVec = triggerCf.frontendHit
  val triggerMemHitVec = triggerCf.backendHit
  val triggerHitVec = triggerFrontendHitVec.asUInt | triggerMemHitVec.asUInt // Todo: update mcontrol.hit
  val triggerFrontendCanFireVec = triggerCf.frontendCanFire.asUInt
  val triggerMemCanFireVec = triggerCf.backendCanFire.asUInt
  val triggerCanFireVec = triggerFrontendCanFireVec | triggerMemCanFireVec
  val tdata1WireVec = tdata1RegVec.map{ mod => {
      val tdata1Wire = Wire(new Tdata1Bundle)
      tdata1Wire := mod.rdata
      tdata1Wire
  }}
  val tdata2WireVec = tdata2RegVec.map{ mod => {
      val tdata2Wire = Wire(new Tdata2Bundle)
      tdata2Wire := mod.rdata
      tdata2Wire
  }}
  val mcontrolWireVec = tdata1WireVec.map{ mod => {
    val mcontrolWire = Wire(new Mcontrol)
    mcontrolWire := mod.DATA.asUInt
    mcontrolWire
  }}

  // More than one triggers can hit at the same time, but only fire one
  // We select the first hit trigger to fire
  val triggerFireOH = PriorityEncoderOH(triggerCanFireVec)
  val triggerFireAction = PriorityMux(triggerFireOH, tdata1WireVec.map(_.getTriggerAction)).asUInt
  val hasTriggerFire = hasExp && triggerCf.canFire
  val hasDebugTriggerException = hasTriggerFire && (triggerFireAction === TrigAction.DebugMode.asUInt)
  val triggerCanFire = hasTriggerFire && (triggerFireAction === TrigAction.BreakpointExp.asUInt) &&
                          Mux(privState.isModeM && !debugMode, tcontrol.regOut.MTE.asBool, true.B) // todo: Should trigger be fire in dmode?

  // debug_exception_single
  val hasSingleStep = hasExp && io.fromRob.trap.bits.singleStep

  val hasDebugException = hasDebugEbreakException || hasDebugTriggerException || hasSingleStep
  val hasDebugTrap = hasDebugException || hasDebugIntr

  trapEntryDEvent.valid                       := hasDebugTrap && !debugMode
  trapEntryDEvent.in.hasDebugIntr             := hasDebugIntr
  trapEntryDEvent.in.debugMode                := debugMode
  trapEntryDEvent.in.hasTrap                  := hasTrap
  trapEntryDEvent.in.hasSingleStep            := hasSingleStep
  trapEntryDEvent.in.hasTriggerFire           := hasTriggerFire
  trapEntryDEvent.in.hasDebugEbreakException  := hasDebugEbreakException
  trapEntryDEvent.in.breakPoint               := breakPoint

  trapHandleMod.io.in.trapInfo.bits.singleStep  := hasSingleStep
  trapHandleMod.io.in.trapInfo.bits.triggerFire := triggerCanFire

  intrMod.io.in.debugMode := debugMode
  intrMod.io.in.debugIntr := debugIntr
  intrMod.io.in.dcsr      := dcsr.rdata.asUInt

  val tselect1H = UIntToOH(tselect.rdata.asUInt, TriggerNum).asBools
  val chainVec = mcontrolWireVec.map(_.CHAIN.asBool)
  val newTriggerChainVec = tselect1H.zip(chainVec).map{case(a, b) => a | b}
  val newTriggerChainIsLegal = TriggerUtil.TriggerCheckChainLegal(newTriggerChainVec, TriggerChainMaxLength)

  val tdata1Update  = addr === tdata1.addr.U && wenLegal
  val tdata2Update  = addr === tdata2.addr.U && wenLegal
  val triggerUpdate = tdata1Update || tdata2Update

  tdata1RegVec.foreach { mod =>
    mod match {
      case m: HasdebugModeBundle =>
        m.debugMode := debugMode
        m.chainable := newTriggerChainIsLegal
      case _ =>
    }
  }
  tdata1RegVec.zip(tdata2RegVec).zipWithIndex.map { case ((mod1, mod2), idx) => {
      mod1.w.wen    := tdata1Update && (tselect.rdata === idx.U)
      mod1.w.wdata  := wdata
      mod2.w.wen    := tdata2Update && (tselect.rdata === idx.U)
      mod2.w.wdata  := wdata
    }
  }

  val tdata1Wdata = Wire(new Tdata1Bundle)
  tdata1Wdata := wdata
  val mcontrolWdata = Wire(new Mcontrol)
  mcontrolWdata := tdata1Wdata.DATA.asUInt
  val tdata1TypeWdata = tdata1Wdata.TYPE

  val tdata1Selected = Wire(new Tdata1Bundle)
  tdata1Selected := tdata1.rdata.asUInt
  val mcontrolSelected = Wire(new Mcontrol)
  mcontrolSelected := tdata1Selected.DATA.asUInt
  val tdata2Selected = Wire(new Tdata2Bundle)
  tdata2Selected := tdata2.rdata.asUInt

  val frontendTriggerUpdate =
    tdata1Update && tdata1TypeWdata.isLegal && mcontrolWdata.isFetchTrigger ||
      mcontrolSelected.isFetchTrigger && triggerUpdate

  val memTriggerUpdate =
    tdata1Update && tdata1TypeWdata.isLegal && mcontrolWdata.isMemAccTrigger ||
      mcontrolSelected.isMemAccTrigger && triggerUpdate

  val triggerEnableVec = tdata1WireVec.zip(mcontrolWireVec).map { case(tdata1, mcontrol) =>
    tdata1.TYPE.isLegal && (
      mcontrol.M && privState.isModeM  ||
        mcontrol.S && privState.isModeHS ||
        mcontrol.U && privState.isModeHU)
  }

  val fetchTriggerEnableVec = triggerEnableVec.zip(mcontrolWireVec).map {
    case (tEnable, mod) => tEnable && mod.isFetchTrigger
  }
  val memAccTriggerEnableVec = triggerEnableVec.zip(mcontrolWireVec).map {
    case (tEnable, mod) => tEnable && mod.isMemAccTrigger
  }

  triggerFrontendChange := frontendTriggerUpdate

  io.out.frontendTrigger.tUpdate.valid       := RegNext(RegNext(frontendTriggerUpdate))
  io.out.frontendTrigger.tUpdate.bits.addr   := tselect.rdata.asUInt
  io.out.frontendTrigger.tUpdate.bits.tdata.GenTdataDistribute(tdata1Selected, tdata2Selected)
  io.out.frontendTrigger.tEnableVec          := fetchTriggerEnableVec
  io.out.memTrigger.tUpdate.valid            := RegNext(RegNext(memTriggerUpdate))
  io.out.memTrigger.tUpdate.bits.addr        := tselect.rdata.asUInt
  io.out.memTrigger.tUpdate.bits.tdata.GenTdataDistribute(tdata1Selected, tdata2Selected)
  io.out.memTrigger.tEnableVec               := memAccTriggerEnableVec
  /**
   * debug_end
   */

  /**
   * [[io.out.custom]] connection
   */
  io.out.custom.l1I_pf_enable           := spfctl.regOut.L1I_PF_ENABLE.asBool
  io.out.custom.l2_pf_enable            := spfctl.regOut.L2_PF_ENABLE.asBool
  io.out.custom.l1D_pf_enable           := spfctl.regOut.L1D_PF_ENABLE.asBool
  io.out.custom.l1D_pf_train_on_hit     := spfctl.regOut.L1D_PF_TRAIN_ON_HIT.asBool
  io.out.custom.l1D_pf_enable_agt       := spfctl.regOut.L1D_PF_ENABLE_AGT.asBool
  io.out.custom.l1D_pf_enable_pht       := spfctl.regOut.L1D_PF_ENABLE_PHT.asBool
  io.out.custom.l1D_pf_active_threshold := spfctl.regOut.L1D_PF_ACTIVE_THRESHOLD.asUInt
  io.out.custom.l1D_pf_active_stride    := spfctl.regOut.L1D_PF_ACTIVE_STRIDE.asUInt
  io.out.custom.l1D_pf_enable_stride    := spfctl.regOut.L1D_PF_ENABLE_STRIDE.asBool
  io.out.custom.l2_pf_store_only        := spfctl.regOut.L2_PF_STORE_ONLY.asBool

  io.out.custom.icache_parity_enable    := sfetchctl.regOut.ICACHE_PARITY_ENABLE.asBool

  io.out.custom.lvpred_disable          := slvpredctl.regOut.LVPRED_DISABLE.asBool
  io.out.custom.no_spec_load            := slvpredctl.regOut.NO_SPEC_LOAD.asBool
  io.out.custom.storeset_wait_store     := slvpredctl.regOut.STORESET_WAIT_STORE.asBool
  io.out.custom.storeset_no_fast_wakeup := slvpredctl.regOut.STORESET_NO_FAST_WAKEUP.asBool
  io.out.custom.lvpred_timeout          := slvpredctl.regOut.LVPRED_TIMEOUT.asUInt

  io.out.custom.bp_ctrl.ubtb_enable     := sbpctl.regOut.UBTB_ENABLE .asBool
  io.out.custom.bp_ctrl.btb_enable      := sbpctl.regOut.BTB_ENABLE  .asBool
  io.out.custom.bp_ctrl.bim_enable      := sbpctl.regOut.BIM_ENABLE  .asBool
  io.out.custom.bp_ctrl.tage_enable     := sbpctl.regOut.TAGE_ENABLE .asBool
  io.out.custom.bp_ctrl.sc_enable       := sbpctl.regOut.SC_ENABLE   .asBool
  io.out.custom.bp_ctrl.ras_enable      := sbpctl.regOut.RAS_ENABLE  .asBool
  io.out.custom.bp_ctrl.loop_enable     := sbpctl.regOut.LOOP_ENABLE .asBool

  io.out.custom.sbuffer_threshold                := smblockctl.regOut.SBUFFER_THRESHOLD.asUInt
  io.out.custom.ldld_vio_check_enable            := smblockctl.regOut.LDLD_VIO_CHECK_ENABLE.asBool
  io.out.custom.soft_prefetch_enable             := smblockctl.regOut.SOFT_PREFETCH_ENABLE.asBool
  io.out.custom.cache_error_enable               := smblockctl.regOut.CACHE_ERROR_ENABLE.asBool
  io.out.custom.uncache_write_outstanding_enable := smblockctl.regOut.UNCACHE_WRITE_OUTSTANDING_ENABLE.asBool

  io.out.custom.fusion_enable           := srnctl.regOut.FUSION_ENABLE.asBool
  io.out.custom.wfi_enable              := srnctl.regOut.WFI_ENABLE.asBool

  // Todo: record the last address to avoid xireg is different with xiselect
  toAIA.addr.valid := wenLegal && Seq(miselect, siselect, vsiselect).map(
    _.addr.U === addr
  ).reduce(_ || _)
  toAIA.addr.bits.addr := addr
  toAIA.addr.bits.prvm := PRVM
  toAIA.addr.bits.v := V
  toAIA.vgein := hstatus.regOut.VGEIN.asUInt
  toAIA.wdata.valid := wenLegal && Seq(mireg, sireg, vsireg).map(
    _.addr.U === addr
  ).reduce(_ || _)
  toAIA.wdata.bits.data := wdata
  toAIA.mClaim := wenLegal && mtopei.addr.U === addr
  toAIA.sClaim := wenLegal && stopei.addr.U === addr
  toAIA.vsClaim := wenLegal && vstopei.addr.U === addr

  // tlb
  io.tlb.satpASIDChanged  := wenLegal && addr === CSRs. satp.U && satp .regOut.ASID =/= wdata.asTypeOf(new SatpBundle).ASID
  io.tlb.vsatpASIDChanged := wenLegal && addr === CSRs.vsatp.U && vsatp.regOut.ASID =/= wdata.asTypeOf(new SatpBundle).ASID
  io.tlb.hgatpVMIDChanged := wenLegal && addr === CSRs.hgatp.U && hgatp.regOut.VMID =/= wdata.asTypeOf(new HgatpBundle).VMID
  io.tlb.satp := satp.rdata
  io.tlb.vsatp := vsatp.rdata
  io.tlb.hgatp := hgatp.rdata
  io.tlb.mxr  :=  mstatus.regOut.MXR.asBool
  io.tlb.sum  :=  mstatus.regOut.SUM.asBool
  io.tlb.vmxr := vsstatus.regOut.MXR.asBool
  io.tlb.vsum := vsstatus.regOut.SUM.asBool
  io.tlb.spvp :=  hstatus.regOut.SPVP.asBool

  io.tlb.imode := PRVM.asUInt
  io.tlb.dmode := Mux((debugMode && dcsr.regOut.MPRVEN.asBool || !debugMode) && mstatus.regOut.MPRV.asBool, mstatus.regOut.MPP.asUInt, PRVM.asUInt)

  io.toDecode.illegalInst.sfenceVMA  := isModeHS && mstatus.regOut.TVM  || isModeHU
  io.toDecode.virtualInst.sfenceVMA  := isModeVS && hstatus.regOut.VTVM || isModeVU
  io.toDecode.illegalInst.sfencePart := isModeHU
  io.toDecode.virtualInst.sfencePart := isModeVU
  io.toDecode.illegalInst.hfenceGVMA := isModeHS && mstatus.regOut.TVM || isModeHU
  io.toDecode.illegalInst.hfenceVVMA := isModeHU
  io.toDecode.virtualInst.hfence     := isModeVS || isModeVU
  io.toDecode.illegalInst.hlsv       := isModeHU && hstatus.regOut.HU
  io.toDecode.virtualInst.hlsv       := isModeVS || isModeVU
  io.toDecode.illegalInst.fsIsOff    := mstatus.regOut.FS === ContextStatus.Off || (isModeVS || isModeVU) && vsstatus.regOut.FS === ContextStatus.Off
  io.toDecode.illegalInst.vsIsOff    := mstatus.regOut.VS === ContextStatus.Off || (isModeVS || isModeVU) && vsstatus.regOut.VS === ContextStatus.Off

  // Always instantiate basic difftest modules.
  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    val hartId = io.fromTop.hartId
    val trapValid = io.fromRob.trap.valid
    val trapNO = trapHandleMod.io.out.causeNO.ExceptionCode.asUInt
    val interrupt = trapHandleMod.io.out.causeNO.Interrupt.asBool
    val interruptNO = Mux(interrupt, trapNO, 0.U)
    val exceptionNO = Mux(!interrupt, trapNO, 0.U)
    val ivmHS = isModeHS &&  satp.regOut.MODE =/= SatpMode.Bare
    val ivmVS = isModeVS && vsatp.regOut.MODE =/= SatpMode.Bare
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

trait IpIeAliasConnect {
  self: NewCSR with MachineLevel with SupervisorLevel with VirtualSupervisorLevel with HypervisorLevel =>

  mip.fromMvip  := mvip.toMip
  mip.fromSip   := sip.toMip
  mip.fromVSip  := vsip.toMip
  mvip.fromMip  := mip.toMvip
  mvip.fromSip  := sip.toMvip
  mvip.fromVSip := vsip.toMvip
  hvip.fromMip  := mip.toHvip
  hvip.fromHip  := hip.toHvip
  hvip.fromVSip := vsip.toHvip

  mie.fromHie  := hie.toMie
  mie.fromSie  := sie.toMie
  mie.fromVSie := vsie.toMie
  sie.fromVSie := vsie.toSie
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