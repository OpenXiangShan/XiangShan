package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.rocket.CSRs
import org.chipsalliance.cde.config.Parameters
import top.{ArgParser, Generator}
import utility.{DataHoldBypass, DelayN, GatedValidRegNext, RegNextWithEnable, SignExt, ZeroExt, HPerfMonitor, PerfEvent}
import utils.OptionWrapper
import xiangshan.backend.fu.NewCSR.CSRBundles.{CSRCustomState, PrivState, RobCommitCSR}
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSREnumTypeImplicitCast._
import xiangshan.backend.fu.NewCSR.CSREvents.{CSREvents, DretEventSinkBundle, EventUpdatePrivStateOutput, MNretEventSinkBundle, MretEventSinkBundle, SretEventSinkBundle, TargetPCBundle, TrapEntryDEventSinkBundle, TrapEntryEventInput, TrapEntryHSEventSinkBundle, TrapEntryMEventSinkBundle, TrapEntryMNEventSinkBundle, TrapEntryVSEventSinkBundle}
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.util.CSRConst
import xiangshan.backend.fu.vector.Bundles.{Vl, Vstart, Vxrm, Vxsat}
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.backend.rob.RobPtr
import xiangshan._
import xiangshan.backend.fu.PerfCounterIO
import xiangshan.ExceptionNO._

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

  final val VaddrMaxWidth = 48 + 2 // support Sv39/Sv48/Sv39x4/Sv48x4

  final val InstWidth = 32

  final val XLEN = 64 // Todo: use XSParams

  final val VLEN = 128

  // Since we need macro to compute the width of CSR field, the input of macro should be the value that can be computed
  // at compile time. The log2Up function cannot be used as meta-programming function, so we use litral value here
  // log2Up(128 + 1), hold 0~128
  final val VlWidth = 8

  final val PAddrWidth = 48

  final val AddrWidthInPage = 12

  final val PMPAddrWidth = 48

  final val PMPOffBits = 2

  final val PMPAddrBits = PMPAddrWidth - PMPOffBits

  // perf
  final val perfCntNum = 29       // in Spec

  final val EXT_SSTC = true

  final val PPNLength = 44
}

class NewCSRInput(implicit p: Parameters) extends Bundle {
  val wen = Bool()
  val ren = Bool()
  val op = UInt(2.W)
  val addr = UInt(12.W)
  val src = UInt(64.W)
  val wdata = UInt(64.W)
  val mnret = Input(Bool())
  val mret = Input(Bool())
  val sret = Input(Bool())
  val dret = Input(Bool())
}

class NewCSROutput(implicit p: Parameters) extends Bundle {
  val EX_II = Bool()
  val EX_VI = Bool()
  val flushPipe = Bool()
  val rData = UInt(64.W)
  val targetPcUpdate = Bool()
  val targetPc = new TargetPCBundle
  val regOut = UInt(64.W)
  // perf
  val isPerfCnt = Bool()
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
  with HasNonMaskableIRPBundle
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
    val in = Flipped(DecoupledIO(new NewCSRInput))
    val trapInst = Input(ValidIO(UInt(InstWidth.W)))
    val fromMem = Input(new Bundle {
      val excpVA  = UInt(XLEN.W)
      val excpGPA = UInt(XLEN.W)
      val excpIsForVSnonLeafPTE = Bool()
    })
    val fromRob = Input(new Bundle {
      val trap = ValidIO(new Bundle {
        val pc = UInt(VaddrMaxWidth.W)
        val pcGPA = UInt(VaddrMaxWidth.W)
        val instr = UInt(InstWidth.W)
        val trapVec = UInt(64.W)
        val singleStep = Bool()
        val trigger = TriggerAction()
        val crossPageIPFFix = Bool()
        val isInterrupt = Bool()
        val isHls = Bool()
        val isFetchMalAddr = Bool()
        val isForVSnonLeafPTE = Bool()
      })
      val commit = Input(new RobCommitCSR)
      val robDeqPtr = Input(new RobPtr)
    })

    val fromVecExcpMod = Input(new Bundle {
      val busy = Bool()
    })

    val perf = Input(new PerfCounterIO)

    /** Output should be a DecoupledIO, since now CSR writing to integer register file might be blocked (by arbiter) */
    val out = DecoupledIO(new NewCSROutput)
    val status = Output(new Bundle {
      val privState = new PrivState
      val interrupt = Bool()
      val wfiEvent = Bool()
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
      // debug
      val debugMode = Bool()
      val singleStepFlag = Bool()
      // trigger
      val frontendTrigger = new FrontendTdataDistributeIO()
      val memTrigger = new MemTdataDistributeIO()
      // Instruction fetch address translation type
      val instrAddrTransType = new AddrTransType
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
      val dvirt = Bool()
      val mPBMTE = Bool()
      val hPBMTE = Bool()
    })

    val toDecode = new CSRToDecode

    val fetchMalTval = Input(UInt(XLEN.W))

    val distributedWenLegal = Output(Bool())
  })

  val toAIA   = IO(Output(new CSRToAIABundle))
  val fromAIA = IO(Flipped(Output(new AIAToCSRBundle)))

  dontTouch(toAIA)
  dontTouch(fromAIA)
  dontTouch(io.fromTop.clintTime)

  /* Alias of input valid/ready */
  val valid = io.in.valid

  /* Alias of input signals */
  val wen   = io.in.bits.wen && valid
  val addr  = io.in.bits.addr
  val wdata = io.in.bits.wdata

  val ren   = io.in.bits.ren && valid
  val raddr = io.in.bits.addr

  val hasTrap = io.fromRob.trap.valid
  val trapVec = io.fromRob.trap.bits.trapVec
  val trapPC = io.fromRob.trap.bits.pc
  val trapPCGPA = io.fromRob.trap.bits.pcGPA
  val trapIsInterrupt = io.fromRob.trap.bits.isInterrupt
  val trapIsCrossPageIPF = io.fromRob.trap.bits.crossPageIPFFix
  val trigger = io.fromRob.trap.bits.trigger
  val singleStep = io.fromRob.trap.bits.singleStep
  val trapIsHls = io.fromRob.trap.bits.isHls
  val trapIsFetchMalAddr = io.fromRob.trap.bits.isFetchMalAddr
  val trapIsForVSnonLeafPTE = io.fromRob.trap.bits.isForVSnonLeafPTE

  // debug_intrrupt
  val debugIntrEnable = RegInit(true.B) // debug interrupt will be handle only when debugIntrEnable
  val debugIntr = platformIRP.debugIP && debugIntrEnable

  // CSR Privilege State
  val PRVM = RegInit(PrivMode(1, 0), PrivMode.M)
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

  val legalSret  = permitMod.io.out.hasLegalSret
  val legalMret  = permitMod.io.out.hasLegalMret
  val legalMNret = permitMod.io.out.hasLegalMNret
  val legalDret  = permitMod.io.out.hasLegalDret

  var csrRwMap: SeqMap[Int, (CSRAddrWriteBundle[_], UInt)] =
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
  val nmip = RegInit(new NonMaskableIRPendingBundle, (new NonMaskableIRPendingBundle).init)
  when(nonMaskableIRP.NMI_43) {
    nmip.NMI_43 := true.B
  }
  when(nonMaskableIRP.NMI_31) {
    nmip.NMI_31 := true.B
  }

  val intrMod = Module(new InterruptFilter)
  intrMod.io.in.privState := privState
  intrMod.io.in.mstatusMIE := mstatus.regOut.MIE.asBool
  intrMod.io.in.sstatusSIE := mstatus.regOut.SIE.asBool
  intrMod.io.in.vsstatusSIE := vsstatus.regOut.SIE.asBool
  intrMod.io.in.mip := mip.rdataFields
  intrMod.io.in.mie := mie.regOut
  intrMod.io.in.mideleg := mideleg.regOut
  intrMod.io.in.sip := sip.regOut
  intrMod.io.in.sie := sie.regOut
  intrMod.io.in.hip := hip.regOut
  intrMod.io.in.hie := hie.regOut
  intrMod.io.in.hideleg := hideleg.regOut
  intrMod.io.in.vsip := vsip.regOut
  intrMod.io.in.vsie := vsie.regOut
  intrMod.io.in.hvictl := hvictl.regOut
  intrMod.io.in.hstatus := hstatus.regOut
  intrMod.io.in.mtopei := mtopei.regOut
  intrMod.io.in.stopei := stopei.regOut
  intrMod.io.in.vstopei := vstopei.regOut
  intrMod.io.in.hviprio1 := hviprio1.regOut
  intrMod.io.in.hviprio2 := hviprio2.regOut
  intrMod.io.in.miprios := Cat(miregiprios.map(_.rdata).reverse)
  intrMod.io.in.hsiprios := Cat(siregiprios.map(_.rdata).reverse)
  intrMod.io.in.mnstatusNMIE := mnstatus.regOut.NMIE.asBool
  intrMod.io.in.nmi := nmip.asUInt.orR
  intrMod.io.in.nmiVec := nmip.asUInt

  when(intrMod.io.out.nmi && intrMod.io.out.interruptVec.valid) {
    nmip.NMI_31 := nmip.NMI_31 & !intrMod.io.out.interruptVec.bits(NonMaskableIRNO.NMI_31).asBool
    nmip.NMI_43 := nmip.NMI_43 & !intrMod.io.out.interruptVec.bits(NonMaskableIRNO.NMI_43).asBool
  }
  val intrVec = RegEnable(intrMod.io.out.interruptVec.bits, 0.U, intrMod.io.out.interruptVec.valid)
  val nmi = RegEnable(intrMod.io.out.nmi, false.B, intrMod.io.out.interruptVec.valid)
  val virtualInterruptIsHvictlInject = RegEnable(intrMod.io.out.virtualInterruptIsHvictlInject, false.B, intrMod.io.out.interruptVec.valid)

  val trapHandleMod = Module(new TrapHandleModule)

  trapHandleMod.io.in.trapInfo.valid := hasTrap
  trapHandleMod.io.in.trapInfo.bits.trapVec := trapVec.asUInt
  trapHandleMod.io.in.trapInfo.bits.nmi := nmi
  trapHandleMod.io.in.trapInfo.bits.intrVec := intrVec
  trapHandleMod.io.in.trapInfo.bits.isInterrupt := trapIsInterrupt
  trapHandleMod.io.in.privState := privState
  trapHandleMod.io.in.mideleg := mideleg.regOut
  trapHandleMod.io.in.medeleg := medeleg.regOut
  trapHandleMod.io.in.hideleg := hideleg.regOut
  trapHandleMod.io.in.hedeleg := hedeleg.regOut
  trapHandleMod.io.in.mvien := mvien.regOut
  trapHandleMod.io.in.hvien := hvien.regOut
  trapHandleMod.io.in.mtvec := mtvec.regOut
  trapHandleMod.io.in.stvec := stvec.regOut
  trapHandleMod.io.in.vstvec := vstvec.regOut
  trapHandleMod.io.in.virtualInterruptIsHvictlInject := virtualInterruptIsHvictlInject

  val entryPrivState = trapHandleMod.io.out.entryPrivState
  val entryDebugMode = WireInit(false.B)

  // PMP
  val pmpEntryMod = Module(new PMPEntryHandleModule)
  pmpEntryMod.io.in.pmpCfg  := cfgs.map(_.regOut.asInstanceOf[PMPCfgBundle])
  pmpEntryMod.io.in.pmpAddr := pmpaddr.map(_.regOut.asInstanceOf[PMPAddrBundle])
  pmpEntryMod.io.in.ren   := ren
  pmpEntryMod.io.in.wen   := wenLegal
  pmpEntryMod.io.in.addr  := addr
  pmpEntryMod.io.in.wdata := wdata

  // Todo: all wen and wdata of CSRModule assigned in this for loop
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

  private val writeFpLegal  = permitMod.io.out.hasLegalWriteFcsr
  private val writeVecLegal = permitMod.io.out.hasLegalWriteVcsr

  permitMod.io.in.csrAccess.ren := ren && valid
  permitMod.io.in.csrAccess.wen := wen
  permitMod.io.in.csrAccess.addr := addr

  permitMod.io.in.privState := privState
  permitMod.io.in.debugMode := debugMode

  permitMod.io.in.mnret := io.in.bits.mnret && valid
  permitMod.io.in.mret  := io.in.bits.mret  && valid
  permitMod.io.in.sret  := io.in.bits.sret  && valid
  permitMod.io.in.dret  := io.in.bits.dret  && valid
  permitMod.io.in.csrIsCustom := customCSRMods.map(_.addr.U === addr).reduce(_ || _).orR

  permitMod.io.in.status.tsr := mstatus.regOut.TSR.asBool
  permitMod.io.in.status.vtsr := hstatus.regOut.VTSR.asBool

  permitMod.io.in.status.tvm  := mstatus.regOut.TVM.asBool
  permitMod.io.in.status.vtvm := hstatus.regOut.VTVM.asBool

  permitMod.io.in.status.mcounteren := mcounteren.rdata
  permitMod.io.in.status.hcounteren := hcounteren.rdata
  permitMod.io.in.status.scounteren := scounteren.rdata

  permitMod.io.in.status.mstateen0 := mstateen0.rdata
  permitMod.io.in.status.hstateen0 := hstateen0.rdata
  permitMod.io.in.status.sstateen0 := sstateen0.rdata

  permitMod.io.in.status.menvcfg := menvcfg.rdata
  permitMod.io.in.status.henvcfg := henvcfg.rdata

  permitMod.io.in.status.mstatusFSOff  :=  mstatus.regOut.FS === ContextStatus.Off
  permitMod.io.in.status.mstatusVSOff  :=  mstatus.regOut.VS === ContextStatus.Off
  permitMod.io.in.status.vsstatusFSOff := vsstatus.regOut.FS === ContextStatus.Off
  permitMod.io.in.status.vsstatusVSOff := vsstatus.regOut.VS === ContextStatus.Off

  permitMod.io.in.aia.miselectIsIllegal  := miselect.isIllegal
  permitMod.io.in.aia.siselectIsIllegal  := siselect.isIllegal
  permitMod.io.in.aia.vsiselectIsIllegal := vsiselect.isIllegal
  permitMod.io.in.aia.siselect := siselect.rdata
  permitMod.io.in.aia.vsiselect := vsiselect.rdata
  permitMod.io.in.aia.mvienSEIE := mvien.regOut.SEIE.asBool
  permitMod.io.in.aia.hvictlVTI := hvictl.regOut.VTI.asBool

  sstcIRGen.i.stime.valid := time.updated
  sstcIRGen.i.stime.bits  := time.stime
  sstcIRGen.i.vstime.valid := time.updated
  sstcIRGen.i.vstime.bits  := time.vstime
  sstcIRGen.i.stimecmp := stimecmp.rdata
  sstcIRGen.i.vstimecmp := vstimecmp.rdata
  sstcIRGen.i.menvcfgSTCE := menvcfg.regOut.STCE.asBool
  sstcIRGen.i.henvcfgSTCE := henvcfg.regOut.STCE.asBool

  miregiprios.foreach { mod =>
    mod.w.wen := mireg.w.wen && (miselect.regOut.ALL.asUInt === mod.addr.U)
    mod.w.wdata := wdata
  }

  siregiprios.foreach { mod =>
    mod.w.wen := sireg.w.wen && (siselect.regOut.ALL.asUInt === mod.addr.U)
    mod.w.wdata := wdata
  }

  mhartid.hartid := this.io.fromTop.hartId

  cfgs.zipWithIndex.foreach { case (mod, i) =>
    mod.w.wen := wenLegal && (addr === (0x3A0 + i / 8 * 2).U)
    mod.w.wdata := pmpEntryMod.io.out.pmpCfgWData(8*((i%8)+1)-1,8*(i%8))
  }

  pmpaddr.zipWithIndex.foreach{ case(mod, i) =>
    mod.w.wen := wenLegal && (addr === (0x3B0 + i).U)
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
        m.hgatp := hgatp.regOut
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
        // Todo: move RegNext from ROB to CSR
        m.robCommit.instNum := io.fromRob.commit.instNum
        m.robCommit.fflags  := RegNextWithEnable(io.fromRob.commit.fflags)
        m.robCommit.fsDirty := GatedValidRegNext(io.fromRob.commit.fsDirty)
        m.robCommit.vsDirty := GatedValidRegNext(io.fromRob.commit.vsDirty)
        m.robCommit.vxsat   := RegNextWithEnable(io.fromRob.commit.vxsat)
        m.robCommit.vtype   := RegNextWithEnable(io.fromRob.commit.vtype)
        m.robCommit.vl      := RegNext          (io.fromRob.commit.vl)
        m.robCommit.vstart  := RegNextWithEnable(io.fromRob.commit.vstart)
        m.writeFCSR         := writeFpLegal
        m.writeVCSR         := writeVecLegal
        m.isVirtMode        := V.asUInt.asBool
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
      case m: TrapEntryMNEventSinkBundle =>
        m.trapToMN := trapEntryMNEvent.out
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
      case m: MNretEventSinkBundle =>
        m.retFromMN := mnretEvent.out
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
        m.aiaToCSR.meip    := fromAIA.meip
        m.aiaToCSR.seip    := fromAIA.seip
        m.aiaToCSR.vseip   := fromAIA.vseip
        m.aiaToCSR.mtopei  := fromAIA.mtopei
        m.aiaToCSR.stopei  := fromAIA.stopei
        m.aiaToCSR.vstopei := fromAIA.vstopei
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
        m.mHPM.hpmcounters.zip(mhpmcounters).map{
          case(counter, mcounter) => counter := mcounter.rdata
        }
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
      case _ =>
    }
    mod match {
      case m: HasIpIeBundle =>
        m.mideleg := mideleg.regOut
        m.mip := mip.rdata
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
    mod match {
      case m: HasMhpmeventOfBundle =>
        m.ofVec := VecInit(mhpmevents.map(event => event.rdata.head(1).asBool)).asUInt //todo：fix
        m.privState := privState
        m.mcounteren := mcounteren.rdata
        m.hcounteren := hcounteren.rdata
      case _ =>
    }
    mod match {
      case m: HasStateen0Bundle =>
        m.fromMstateen0 := mstateen0.regOut
        m.fromHstateen0 := hstateen0.regOut
        m.privState     := privState
      case _ =>
    }
  }

  csrMods.foreach { mod =>
    println(s"${mod.modName}: ")
    println(mod.dumpFields)
  }

  trapEntryMEvent.valid  := hasTrap && entryPrivState.isModeM && !entryDebugMode  && !debugMode && !nmi
  trapEntryMNEvent.valid := hasTrap && nmi && !debugMode
  trapEntryHSEvent.valid := hasTrap && entryPrivState.isModeHS && !entryDebugMode && !debugMode
  trapEntryVSEvent.valid := hasTrap && entryPrivState.isModeVS && !entryDebugMode && !debugMode

  Seq(trapEntryMEvent, trapEntryMNEvent, trapEntryHSEvent, trapEntryVSEvent, trapEntryDEvent).foreach { eMod =>
    eMod.in match {
      case in: TrapEntryEventInput =>
        in.causeNO := trapHandleMod.io.out.causeNO
        in.trapPc := trapPC
        in.trapPcGPA := trapPCGPA // only used by trapEntryMEvent & trapEntryHSEvent
        in.trapInst := io.trapInst
        in.fetchMalTval := io.fetchMalTval
        in.isCrossPageIPF := trapIsCrossPageIPF
        in.isHls := trapIsHls
        in.isFetchMalAddr := trapIsFetchMalAddr
        in.trapIsForVSnonLeafPTE := trapIsForVSnonLeafPTE

        in.iMode.PRVM := PRVM
        in.iMode.V := V
        // when NMIE is zero, force to behave as MPRV is zero
        in.dMode.PRVM := Mux(mstatus.regOut.MPRV.asBool && mnstatus.regOut.NMIE.asBool, mstatus.regOut.MPP, PRVM)
        in.dMode.V := V.asUInt.asBool || mstatus.regOut.MPRV && mnstatus.regOut.NMIE.asBool && (mstatus.regOut.MPP =/= PrivMode.M) && mstatus.regOut.MPV

        in.privState := privState
        in.mstatus := mstatus.regOut
        in.hstatus := hstatus.regOut
        in.sstatus := mstatus.sstatus
        in.vsstatus := vsstatus.regOut
        in.pcFromXtvec := trapHandleMod.io.out.pcFromXtvec

        in.satp  := satp.regOut
        in.vsatp := vsatp.regOut
        in.hgatp := hgatp.regOut

        in.memExceptionVAddr := io.fromMem.excpVA
        in.memExceptionGPAddr := io.fromMem.excpGPA
        in.memExceptionIsForVSnonLeafPTE := io.fromMem.excpIsForVSnonLeafPTE

        in.virtualInterruptIsHvictlInject := virtualInterruptIsHvictlInject
        in.hvictlIID := hvictl.regOut.IID.asUInt
    }
  }

  mnretEvent.valid := legalMNret
  mnretEvent.in match {
    case in =>
      in.mstatus := mstatus.regOut
      in.mnepc   := mnepc.regOut
      in.mnstatus:= mnstatus.regOut
      in.satp := satp.regOut
      in.vsatp := vsatp.regOut
      in.hgatp := hgatp.regOut
  }

  mretEvent.valid := legalMret
  mretEvent.in match {
    case in =>
      in.mstatus := mstatus.regOut
      in.mepc := mepc.regOut
      in.satp := satp.regOut
      in.vsatp := vsatp.regOut
      in.hgatp := hgatp.regOut
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
      in.satp := satp.regOut
      in.vsatp := vsatp.regOut
      in.hgatp := hgatp.regOut
  }

  dretEvent.valid := legalDret
  dretEvent.in match {
    case in =>
      in.dcsr := dcsr.regOut
      in.dpc  := dpc.regOut
      in.mstatus := mstatus.regOut
      in.satp := satp.regOut
      in.vsatp := vsatp.regOut
      in.hgatp := hgatp.regOut
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
  val addrInPerfCnt = (wenLegal || ren) && (
    (addr >= CSRs.mcycle.U) && (addr <= CSRs.mhpmcounter31.U) ||
    (addr === mcountinhibit.addr.U) ||
    (addr >= CSRs.cycle.U) && (addr <= CSRs.hpmcounter31.U) ||
    Cat(aiaSkipCSRs.map(_.addr.U === addr)).orR
  )

  // flush
  val resetSatp = Cat(Seq(satp, vsatp, hgatp).map(_.addr.U === addr)).orR && wenLegal // write to satp will cause the pipeline be flushed

  val floatStatusOnOff = mstatus.w.wen && (
    mstatus.w.wdataFields.FS === ContextStatus.Off && mstatus.regOut.FS =/= ContextStatus.Off ||
    mstatus.w.wdataFields.FS =/= ContextStatus.Off && mstatus.regOut.FS === ContextStatus.Off
  ) || mstatus.wAliasSstatus.wen && (
    mstatus.wAliasSstatus.wdataFields.FS === ContextStatus.Off && mstatus.regOut.FS =/= ContextStatus.Off ||
    mstatus.wAliasSstatus.wdataFields.FS =/= ContextStatus.Off && mstatus.regOut.FS === ContextStatus.Off
  ) || vsstatus.w.wen && (
    vsstatus.w.wdataFields.FS === ContextStatus.Off && vsstatus.regOut.FS =/= ContextStatus.Off ||
    vsstatus.w.wdataFields.FS =/= ContextStatus.Off && vsstatus.regOut.FS === ContextStatus.Off
  )

  val vectorStatusOnOff = mstatus.w.wen && (
    mstatus.w.wdataFields.VS === ContextStatus.Off && mstatus.regOut.VS =/= ContextStatus.Off ||
    mstatus.w.wdataFields.VS =/= ContextStatus.Off && mstatus.regOut.VS === ContextStatus.Off
  ) || mstatus.wAliasSstatus.wen && (
    mstatus.wAliasSstatus.wdataFields.VS === ContextStatus.Off && mstatus.regOut.VS =/= ContextStatus.Off ||
    mstatus.wAliasSstatus.wdataFields.VS =/= ContextStatus.Off && mstatus.regOut.VS === ContextStatus.Off
  ) || vsstatus.w.wen && (
    vsstatus.w.wdataFields.VS === ContextStatus.Off && vsstatus.regOut.VS =/= ContextStatus.Off ||
    vsstatus.w.wdataFields.VS =/= ContextStatus.Off && vsstatus.regOut.VS === ContextStatus.Off
  )

  val triggerFrontendChange = Wire(Bool())

  val vstartChange = vstart.w.wen && (
    vstart.w.wdata === 0.U && vstart.regOut.vstart.asUInt =/= 0.U ||
    vstart.w.wdata =/= 0.U && vstart.regOut.vstart.asUInt === 0.U
  )

  // flush pipe when write frm and data > 4 or write fcsr and data[7:5] > 4 or write frm/fcsr and frm is reserved
  val frmIsReserved = fcsr.frm(2) && fcsr.frm(1, 0).orR
  val frmWdataReserved = fcsr.wAliasFfm.wdata(2) && fcsr.wAliasFfm.wdata(1, 0).orR
  val fcsrWdataReserved = fcsr.w.wdata(7) && fcsr.w.wdata(6, 5).orR
  val frmChange = fcsr.wAliasFfm.wen && (!frmIsReserved && frmWdataReserved || frmIsReserved && !frmWdataReserved) ||
    fcsr.w.wen && (!frmIsReserved && fcsrWdataReserved || frmIsReserved && !fcsrWdataReserved)

  val flushPipe = resetSatp ||
    triggerFrontendChange || floatStatusOnOff || vectorStatusOnOff ||
    vstartChange || frmChange

  private val rdata = Mux1H(csrRwMap.map { case (id, (_, rdata)) =>
    if (vsMapS.contains(id)) {
      ((isModeVS && addr === vsMapS(id).U) || !isModeVS && addr === id.U) -> rdata
    } else if (sMapVS.contains(id)) {
      (!isModeVS && addr === id.U) -> rdata
    } else {
      (raddr === id.U) -> rdata
    }
  })

  private val regOut = Mux1H(csrOutMap.map { case (id, regOut) =>
    if (vsMapS.contains(id)) {
      ((isModeVS && addr === vsMapS(id).U) || !isModeVS && addr === id.U) -> regOut
    } else if (sMapVS.contains(id)) {
      (!isModeVS && addr === id.U) -> regOut
    } else {
      (raddr === id.U) -> regOut
    }
  })

  private val needTargetUpdate = mnretEvent.out.targetPc.valid || mretEvent.out.targetPc.valid || sretEvent.out.targetPc.valid || dretEvent.out.targetPc.valid ||
    trapEntryMEvent.out.targetPc.valid || trapEntryMNEvent.out.targetPc.valid || trapEntryHSEvent.out.targetPc.valid || trapEntryVSEvent.out.targetPc.valid || trapEntryDEvent.out.targetPc.valid

  private val noCSRIllegal = (ren || wen) && Cat(csrRwMap.keys.toSeq.sorted.map(csrAddr => !(addr === csrAddr.U))).andR

  private val s_idle :: s_waitIMSIC :: s_finish :: Nil = Enum(3)

  /** the state machine of newCSR module */
  private val state = RegInit(s_idle)
  /** the next state of newCSR */
  private val stateNext = WireInit(state)
  state := stateNext

  /**
   * Asynchronous read operation of CSR. Check whether a read is asynchronous when read-enable is high.
   * AIA registers are designed to be read asynchronously, so newCSR will wait for response.
   **/
  private val asyncRead = ren && !(permitMod.io.out.EX_II || permitMod.io.out.EX_VI) && (
    mireg.addr.U === addr && miselect.inIMSICRange ||
    sireg.addr.U === addr && ((!V.asUInt.asBool && siselect.inIMSICRange) || (V.asUInt.asBool && vsiselect.inIMSICRange)) ||
    vsireg.addr.U === addr && vsiselect.inIMSICRange
  )

  /** State machine of newCSR */
  switch(state) {
    is(s_idle) {
      when(valid && asyncRead) {
        stateNext := s_waitIMSIC
      }.elsewhen(valid && !io.out.ready) {
        stateNext := s_finish
      }
    }
    is(s_waitIMSIC) {
      when(fromAIA.rdata.valid) {
        when(io.out.ready) {
          stateNext := s_idle
        }.otherwise {
          stateNext := s_finish
        }
      }
    }
    is(s_finish) {
      when(io.out.ready) {
        stateNext := s_idle
      }
    }
  }


  // Todo: check IMSIC EX_II and EX_VI
  private val imsicIllegal = fromAIA.rdata.valid && fromAIA.rdata.bits.illegal
  private val imsic_EX_II = imsicIllegal && !V.asUInt.asBool
  private val imsic_EX_VI = imsicIllegal && V.asUInt.asBool

  /** Set io.in.ready when state machine is ready to receive a new request synchronously */
  io.in.ready := (state === s_idle)

  /**
   * Valid signal of newCSR output.
   * When in IDLE state, when input_valid is high, we set it.
   * When in waitIMSIC state, and the next state is IDLE, we set it.
   **/

  /** Data that have been read before,and should be stored because output not fired */
  io.out.valid := state === s_idle && valid && !asyncRead ||
                  state === s_waitIMSIC && fromAIA.rdata.valid ||
                  state === s_finish
  io.out.bits.EX_II := DataHoldBypass(permitMod.io.out.EX_II || noCSRIllegal, false.B, io.in.fire) ||
                       DataHoldBypass(imsic_EX_II, false.B, fromAIA.rdata.valid)
  io.out.bits.EX_VI := DataHoldBypass(permitMod.io.out.EX_VI, false.B, io.in.fire) ||
                       DataHoldBypass(imsic_EX_VI, false.B, fromAIA.rdata.valid)
  io.out.bits.flushPipe := DataHoldBypass(flushPipe, false.B, io.in.fire)

  /** Prepare read data for output */
  io.out.bits.rData := DataHoldBypass(
    Mux1H(Seq(
      io.in.fire -> rdata,
      fromAIA.rdata.valid -> fromAIA.rdata.bits.data
    )), 0.U(64.W), io.in.fire || fromAIA.rdata.valid)
  io.out.bits.regOut := regOut
  io.out.bits.targetPc := DataHoldBypass(
    Mux(trapEntryDEvent.out.targetPc.valid,
      trapEntryDEvent.out.targetPc.bits,
      Mux1H(Seq(
        mnretEvent.out.targetPc.valid -> mnretEvent.out.targetPc.bits,
        mretEvent.out.targetPc.valid  -> mretEvent.out.targetPc.bits,
        sretEvent.out.targetPc.valid  -> sretEvent.out.targetPc.bits,
        dretEvent.out.targetPc.valid  -> dretEvent.out.targetPc.bits,
        trapEntryMEvent.out.targetPc.valid -> trapEntryMEvent.out.targetPc.bits,
        trapEntryMNEvent.out.targetPc.valid -> trapEntryMNEvent.out.targetPc.bits,
        trapEntryHSEvent.out.targetPc.valid -> trapEntryHSEvent.out.targetPc.bits,
        trapEntryVSEvent.out.targetPc.valid -> trapEntryVSEvent.out.targetPc.bits)
      )
    ),
  needTargetUpdate)
  io.out.bits.targetPcUpdate := needTargetUpdate
  io.out.bits.isPerfCnt := DataHoldBypass(addrInPerfCnt, false.B, io.in.fire)

  io.status.privState := privState
  io.status.fpState.frm := fcsr.frm
  io.status.fpState.off := mstatus.regOut.FS === ContextStatus.Off
  io.status.vecState.vstart := vstart.rdata.asUInt
  io.status.vecState.vxsat := vcsr.vxsat
  io.status.vecState.vxrm := vcsr.vxrm
  io.status.vecState.vcsr := vcsr.rdata.asUInt
  io.status.vecState.vl := vl.rdata.asUInt
  io.status.vecState.vtype := vtype.rdata.asUInt // Todo: check correct
  io.status.vecState.vlenb := vlenb.rdata.asUInt
  io.status.vecState.off := mstatus.regOut.VS === ContextStatus.Off
  io.status.interrupt := intrMod.io.out.interruptVec.valid
  io.status.wfiEvent := debugIntr || (mie.rdata.asUInt & mip.rdata.asUInt).orR
  io.status.debugMode := debugMode
  io.status.singleStepFlag := !debugMode && dcsr.regOut.STEP

  /**
   * debug_begin
   */
  val tdata1Selected = Wire(new Tdata1Bundle)
  tdata1Selected := tdata1.rdata
  val dmodeInSelectedTrigger = tdata1Selected.DMODE.asBool
  val triggerCanWrite = dmodeInSelectedTrigger && debugMode || !dmodeInSelectedTrigger
  val tdata1Update  = tdata1.w.wen && triggerCanWrite
  val tdata2Update  = tdata2.w.wen && triggerCanWrite
  val tdata1Vec = tdata1RegVec.map{ mod => {
    val tdata1Wire = Wire(new Tdata1Bundle)
    tdata1Wire := mod.rdata
    tdata1Wire
  }}

  val triggerCanRaiseBpExp = !(privState.isModeM && !mstatus.regOut.MIE ||
    medeleg.regOut.EX_BP && privState.isModeHS && !mstatus.sstatus.SIE ||
    medeleg.regOut.EX_BP && hedeleg.regOut.EX_BP && privState.isModeVS && !vsstatus.regOut.SIE)

  val debugMod = Module(new Debug)
  debugMod.io.in.trapInfo.valid            := hasTrap
  debugMod.io.in.trapInfo.bits.trapVec     := trapVec.asUInt
  debugMod.io.in.trapInfo.bits.intrVec     := intrVec
  debugMod.io.in.trapInfo.bits.isInterrupt := trapIsInterrupt
  debugMod.io.in.trapInfo.bits.trigger     := trigger
  debugMod.io.in.trapInfo.bits.singleStep  := singleStep
  debugMod.io.in.privState                 := privState
  debugMod.io.in.debugMode                 := debugMode
  debugMod.io.in.dcsr                      := dcsr.regOut
  debugMod.io.in.tselect                   := tselect.regOut
  debugMod.io.in.tdata1Vec                 := tdata1Vec
  debugMod.io.in.tdata1Selected            := tdata1.rdata
  debugMod.io.in.tdata2Selected            := tdata2.rdata
  debugMod.io.in.tdata1Update              := tdata1Update
  debugMod.io.in.tdata2Update              := tdata2Update
  debugMod.io.in.tdata1Wdata               := wdata
  debugMod.io.in.triggerCanRaiseBpExp      := triggerCanRaiseBpExp

  entryDebugMode := debugMod.io.out.hasDebugTrap && !debugMode

  trapEntryDEvent.valid                       := entryDebugMode
  trapEntryDEvent.in.hasDebugIntr             := debugMod.io.out.hasDebugIntr
  trapEntryDEvent.in.debugMode                := debugMode
  trapEntryDEvent.in.hasTrap                  := hasTrap
  trapEntryDEvent.in.hasSingleStep            := debugMod.io.out.hasSingleStep
  trapEntryDEvent.in.triggerEnterDebugMode    := debugMod.io.out.triggerEnterDebugMode
  trapEntryDEvent.in.hasDebugEbreakException  := debugMod.io.out.hasDebugEbreakException
  trapEntryDEvent.in.breakPoint               := debugMod.io.out.breakPoint

  trapHandleMod.io.in.trapInfo.bits.singleStep  := debugMod.io.out.hasSingleStep

  intrMod.io.in.debugMode := debugMode
  intrMod.io.in.debugIntr := debugIntr
  intrMod.io.in.dcsr      := dcsr.regOut

  tdata1RegVec.foreach { mod =>
    mod match {
      case m: HasdebugModeBundle =>
        m.debugMode := debugMode
        m.chainable := debugMod.io.out.newTriggerChainIsLegal
      case _ =>
    }
  }
  tdata1RegVec.zip(tdata2RegVec).zipWithIndex.map { case ((mod1, mod2), idx) => {
    mod1.w.wen    := tdata1Update && (tselect.rdata === idx.U)
    mod1.w.wdata  := wdata
    mod2.w.wen    := tdata2Update && (tselect.rdata === idx.U)
    mod2.w.wdata  := wdata
  }}

  triggerFrontendChange := debugMod.io.out.triggerFrontendChange

  io.status.frontendTrigger := debugMod.io.out.frontendTrigger
  io.status.memTrigger      := debugMod.io.out.memTrigger
  /**
   * debug_end
   */

  /**
   * perf_begin
   * perf number: 29 (frontend 8, ctrlblock 8, memblock 8, huancun 5)
   */
  // tmp: mhpmevents is wrapper of perfEvents, read/write/update mhpmevents -> read/write/update perfEvents
  val csrevents = perfEvents.slice(24, 29)

  val hcEvents = Wire(Vec(numPCntHc * coreParams.L2NBanks, new PerfEvent))
  for (i <- 0 until numPCntHc * coreParams.L2NBanks) {
    hcEvents(i) := io.perf.perfEventsHc(i)
  }

  val hpmHc = HPerfMonitor(csrevents, hcEvents)

  val privState1H = Cat(privState.isModeM, privState.isModeHS, privState.isModeHU, privState.isModeVS, privState.isModeVU)
  val countingEn = RegInit(0.U.asTypeOf(Vec(perfCntNum, Bool())))
  for (i <-0 until perfCntNum) {
    countingEn(i) := ((~mhpmevents(i).rdata(62, 58)).asUInt & privState1H).orR
  }
  val allPerfEvents = io.perf.perfEventsFrontend ++
    io.perf.perfEventsBackend ++
    io.perf.perfEventsLsu ++
    hpmHc.getPerf

  val ofFromPerfCntVec  = Wire(Vec(perfCntNum, Bool()))
  val lcofiReqVec       = Wire(Vec(perfCntNum, Bool()))
  for(i <- 0 until perfCntNum) {
    mhpmcounters(i) match {
      case m: HasPerfCounterBundle =>
        m.countingEn        := countingEn(i)
        m.perf              := allPerfEvents(i)
        ofFromPerfCntVec(i) := m.toMhpmeventOF
      case _ =>
    }
    perfEvents(i)  := Mux(mhpmevents(i).w.wen, wdata, (perfEvents(i).head(1).asBool || ofFromPerfCntVec(i)) ## perfEvents(i).tail(1))
    lcofiReqVec(i) := ofFromPerfCntVec(i) && !mhpmevents(i).rdata.head(1)
  }

  val lcofiReq = lcofiReqVec.asUInt.orR
  mip match {
    case m: HasLocalInterruptReqBundle =>
      m.lcofiReq := lcofiReq
    case _ =>
  }
  /**
   * perf_end
   */

  /**
   * [[io.status.custom]] connection
   */
  io.status.custom.l1I_pf_enable           := spfctl.regOut.L1I_PF_ENABLE.asBool
  io.status.custom.l2_pf_enable            := spfctl.regOut.L2_PF_ENABLE.asBool
  io.status.custom.l1D_pf_enable           := spfctl.regOut.L1D_PF_ENABLE.asBool
  io.status.custom.l1D_pf_train_on_hit     := spfctl.regOut.L1D_PF_TRAIN_ON_HIT.asBool
  io.status.custom.l1D_pf_enable_agt       := spfctl.regOut.L1D_PF_ENABLE_AGT.asBool
  io.status.custom.l1D_pf_enable_pht       := spfctl.regOut.L1D_PF_ENABLE_PHT.asBool
  io.status.custom.l1D_pf_active_threshold := spfctl.regOut.L1D_PF_ACTIVE_THRESHOLD.asUInt
  io.status.custom.l1D_pf_active_stride    := spfctl.regOut.L1D_PF_ACTIVE_STRIDE.asUInt
  io.status.custom.l1D_pf_enable_stride    := spfctl.regOut.L1D_PF_ENABLE_STRIDE.asBool
  io.status.custom.l2_pf_store_only        := spfctl.regOut.L2_PF_STORE_ONLY.asBool

  io.status.custom.icache_parity_enable    := sfetchctl.regOut.ICACHE_PARITY_ENABLE.asBool

  io.status.custom.lvpred_disable          := slvpredctl.regOut.LVPRED_DISABLE.asBool
  io.status.custom.no_spec_load            := slvpredctl.regOut.NO_SPEC_LOAD.asBool
  io.status.custom.storeset_wait_store     := slvpredctl.regOut.STORESET_WAIT_STORE.asBool
  io.status.custom.storeset_no_fast_wakeup := slvpredctl.regOut.STORESET_NO_FAST_WAKEUP.asBool
  io.status.custom.lvpred_timeout          := slvpredctl.regOut.LVPRED_TIMEOUT.asUInt

  io.status.custom.bp_ctrl.ubtb_enable     := sbpctl.regOut.UBTB_ENABLE .asBool
  io.status.custom.bp_ctrl.btb_enable      := sbpctl.regOut.BTB_ENABLE  .asBool
  io.status.custom.bp_ctrl.bim_enable      := sbpctl.regOut.BIM_ENABLE  .asBool
  io.status.custom.bp_ctrl.tage_enable     := sbpctl.regOut.TAGE_ENABLE .asBool
  io.status.custom.bp_ctrl.sc_enable       := sbpctl.regOut.SC_ENABLE   .asBool
  io.status.custom.bp_ctrl.ras_enable      := sbpctl.regOut.RAS_ENABLE  .asBool
  io.status.custom.bp_ctrl.loop_enable     := sbpctl.regOut.LOOP_ENABLE .asBool

  io.status.custom.sbuffer_threshold                := smblockctl.regOut.SBUFFER_THRESHOLD.asUInt
  io.status.custom.ldld_vio_check_enable            := smblockctl.regOut.LDLD_VIO_CHECK_ENABLE.asBool
  io.status.custom.soft_prefetch_enable             := smblockctl.regOut.SOFT_PREFETCH_ENABLE.asBool
  io.status.custom.cache_error_enable               := smblockctl.regOut.CACHE_ERROR_ENABLE.asBool
  io.status.custom.uncache_write_outstanding_enable := smblockctl.regOut.UNCACHE_WRITE_OUTSTANDING_ENABLE.asBool
  io.status.custom.hd_misalign_st_enable            := smblockctl.regOut.HD_MISALIGN_ST_ENABLE.asBool
  io.status.custom.hd_misalign_ld_enable            := smblockctl.regOut.HD_MISALIGN_LD_ENABLE.asBool

  io.status.custom.fusion_enable           := srnctl.regOut.FUSION_ENABLE.asBool
  io.status.custom.wfi_enable              := srnctl.regOut.WFI_ENABLE.asBool

  io.status.instrAddrTransType.bare := privState.isModeM ||
    (!privState.isVirtual && satp.regOut.MODE === SatpMode.Bare) ||
    (privState.isVirtual && vsatp.regOut.MODE === SatpMode.Bare && hgatp.regOut.MODE === HgatpMode.Bare)
  io.status.instrAddrTransType.sv39 := !privState.isModeM && !privState.isVirtual && satp.regOut.MODE === SatpMode.Sv39 ||
    privState.isVirtual && vsatp.regOut.MODE === SatpMode.Sv39
  io.status.instrAddrTransType.sv48 := !privState.isModeM && !privState.isVirtual && satp.regOut.MODE === SatpMode.Sv48 ||
    privState.isVirtual && vsatp.regOut.MODE === SatpMode.Sv48
  io.status.instrAddrTransType.sv39x4 := privState.isVirtual && vsatp.regOut.MODE === SatpMode.Bare && hgatp.regOut.MODE === HgatpMode.Sv39x4
  io.status.instrAddrTransType.sv48x4 := privState.isVirtual && vsatp.regOut.MODE === SatpMode.Bare && hgatp.regOut.MODE === HgatpMode.Sv48x4
  assert(PopCount(io.status.instrAddrTransType.asUInt) === 1.U, "Exactly one inst trans type should be asserted")

  private val csrAccess = wenLegal || ren

  private val imsicAddrValid =
    csrAccess &&  addr === CSRs.mireg.U &&  miselect.inIMSICRange ||
    csrAccess &&  addr === CSRs.sireg.U && !isModeVS && siselect.inIMSICRange ||
    csrAccess && (addr === CSRs.sireg.U &&  isModeVS || addr === CSRs.vsireg.U) && vsiselect.inIMSICRange

  private val imsicAddr = Mux1H(Seq(
    (csrAccess &&  addr === CSRs.mireg.U) -> miselect.rdata,
    (csrAccess &&  addr === CSRs.sireg.U && !isModeVS) -> siselect.rdata,
    (csrAccess && (addr === CSRs.sireg.U &&  isModeVS || addr === CSRs.vsireg.U)) -> vsiselect.rdata,
  ))

  private val imsicAddrPrivState = Mux1H(Seq(
    (csrAccess &&  addr === CSRs.mireg.U) -> PrivState.ModeM,
    (csrAccess &&  addr === CSRs.sireg.U && !isModeVS) -> PrivState.ModeHS,
    (csrAccess && (addr === CSRs.sireg.U &&  isModeVS || addr === CSRs.vsireg.U)) -> PrivState.ModeVS,
  ))

  private val imsicWdataValid =
    mireg.w.wen  && miselect.inIMSICRange ||
    sireg.w.wen  && siselect.inIMSICRange ||
    vsireg.w.wen && vsiselect.inIMSICRange

  toAIA.addr.valid     := imsicAddrValid
  toAIA.addr.bits.addr := imsicAddr
  toAIA.addr.bits.prvm := imsicAddrPrivState.PRVM
  toAIA.addr.bits.v    := imsicAddrPrivState.V

  toAIA.wdata.valid := imsicWdataValid
  toAIA.wdata.bits.op := io.in.bits.op
  toAIA.wdata.bits.data := io.in.bits.src
  toAIA.vgein := hstatus.regOut.VGEIN.asUInt
  toAIA.mClaim  := mtopei.w.wen
  toAIA.sClaim  := stopei.w.wen
  toAIA.vsClaim := vstopei.w.wen

  // tlb
  io.tlb.satpASIDChanged  := GatedValidRegNext(satp.w.wen  && satp .regOut.ASID =/=  satp.w.wdataFields.ASID)
  io.tlb.vsatpASIDChanged := GatedValidRegNext(vsatp.w.wen && vsatp.regOut.ASID =/= vsatp.w.wdataFields.ASID)
  io.tlb.hgatpVMIDChanged := GatedValidRegNext(hgatp.w.wen && hgatp.regOut.VMID =/= hgatp.w.wdataFields.VMID)
  io.tlb.satp := satp.rdata
  io.tlb.vsatp := vsatp.rdata
  io.tlb.hgatp := hgatp.rdata
  io.tlb.mxr  :=  mstatus.regOut.MXR.asBool
  io.tlb.sum  :=  mstatus.regOut.SUM.asBool
  io.tlb.vmxr := vsstatus.regOut.MXR.asBool
  io.tlb.vsum := vsstatus.regOut.SUM.asBool
  io.tlb.spvp :=  hstatus.regOut.SPVP.asBool

  io.tlb.imode := PRVM.asUInt
  // when NMIE is zero, force to behave as MPRV is zero
  io.tlb.dmode := Mux(
    (debugMode && dcsr.regOut.MPRVEN || !debugMode) && mstatus.regOut.MPRV && mnstatus.regOut.NMIE,
    mstatus.regOut.MPP.asUInt,
    PRVM.asUInt
  )
  io.tlb.dvirt := Mux(
    (debugMode && dcsr.regOut.MPRVEN || !debugMode) && mstatus.regOut.MPRV && mnstatus.regOut.NMIE && mstatus.regOut.MPP =/= PrivMode.M,
    mstatus.regOut.MPV.asUInt,
    V.asUInt
  )
  io.tlb.mPBMTE := RegNext(menvcfg.regOut.PBMTE.asBool)
  io.tlb.hPBMTE := RegNext(henvcfg.regOut.PBMTE.asBool)

  io.toDecode.illegalInst.sfenceVMA  := isModeHS && mstatus.regOut.TVM  || isModeHU
  io.toDecode.virtualInst.sfenceVMA  := isModeVS && hstatus.regOut.VTVM || isModeVU
  io.toDecode.illegalInst.sfencePart := isModeHU
  io.toDecode.virtualInst.sfencePart := isModeVU
  io.toDecode.illegalInst.hfenceGVMA := isModeHS && mstatus.regOut.TVM || isModeHU
  io.toDecode.illegalInst.hfenceVVMA := isModeHU
  io.toDecode.virtualInst.hfence     := isModeVS || isModeVU
  io.toDecode.illegalInst.hlsv       := isModeHU && !hstatus.regOut.HU
  io.toDecode.virtualInst.hlsv       := isModeVS || isModeVU
  io.toDecode.illegalInst.fsIsOff    := mstatus.regOut.FS === ContextStatus.Off || (isModeVS || isModeVU) && vsstatus.regOut.FS === ContextStatus.Off
  io.toDecode.illegalInst.vsIsOff    := mstatus.regOut.VS === ContextStatus.Off || (isModeVS || isModeVU) && vsstatus.regOut.VS === ContextStatus.Off
  io.toDecode.illegalInst.wfi        := isModeHU || !isModeM && mstatus.regOut.TW
  io.toDecode.virtualInst.wfi        := isModeVS && !mstatus.regOut.TW && hstatus.regOut.VTW || isModeVU && !mstatus.regOut.TW
  io.toDecode.illegalInst.frm        := frmIsReserved
  // Ref: The RISC-V Instruction Set Manual Volume I - 20.5. Control and Status Register State
  io.toDecode.illegalInst.cboZ       := !isModeM && !menvcfg.regOut.CBZE || isModeHU && !senvcfg.regOut.CBZE
  io.toDecode.virtualInst.cboZ       := menvcfg.regOut.CBZE && (
    isModeVS && !henvcfg.regOut.CBZE ||
    isModeVU && !(henvcfg.regOut.CBZE && senvcfg.regOut.CBZE)
  )
  io.toDecode.illegalInst.cboCF      := !isModeM && !menvcfg.regOut.CBCFE || isModeHU && !senvcfg.regOut.CBCFE
  io.toDecode.virtualInst.cboCF      := menvcfg.regOut.CBCFE && (
    isModeVS && !henvcfg.regOut.CBCFE ||
    isModeVU && !(henvcfg.regOut.CBCFE && senvcfg.regOut.CBCFE)
  )
  io.toDecode.illegalInst.cboI       :=
    !isModeM && menvcfg.regOut.CBIE === EnvCBIE.Off ||
    isModeHU && senvcfg.regOut.CBIE === EnvCBIE.Off
  io.toDecode.virtualInst.cboI       := menvcfg.regOut.CBIE =/= EnvCBIE.Off && (
    isModeVS && henvcfg.regOut.CBIE === EnvCBIE.Off ||
    isModeVU &&(henvcfg.regOut.CBIE === EnvCBIE.Off || senvcfg.regOut.CBIE === EnvCBIE.Off)
  )
  io.toDecode.special.cboI2F := !io.toDecode.illegalInst.cboI && !io.toDecode.virtualInst.cboI && (
    menvcfg.regOut.CBIE === EnvCBIE.Flush && !isModeM ||
    senvcfg.regOut.CBIE === EnvCBIE.Flush && (isModeHU || isModeVU) ||
    henvcfg.regOut.CBIE === EnvCBIE.Flush && (isModeVS || isModeVU)
  )

  io.distributedWenLegal := wenLegal

  // Always instantiate basic difftest modules.
  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    // Delay trap passed to difftest until VecExcpMod is not busy
    val pendingTrap = RegInit(false.B)
    when (hasTrap) {
      pendingTrap := true.B
    }.elsewhen (!io.fromVecExcpMod.busy) {
      pendingTrap := false.B
    }

    val hartId = io.fromTop.hartId
    val trapValid = pendingTrap && !io.fromVecExcpMod.busy
    val trapNO = Mux(virtualInterruptIsHvictlInject && hasTrap, hvictl.regOut.IID.asUInt, trapHandleMod.io.out.causeNO.ExceptionCode.asUInt)
    val interrupt = trapHandleMod.io.out.causeNO.Interrupt.asBool
    val hasNMI = nmi && hasTrap
    val interruptNO = Mux(interrupt, trapNO, 0.U)
    val exceptionNO = Mux(!interrupt, trapNO, 0.U)
    val isSv39: Bool =
      (isModeHS || isModeHU) &&  satp.regOut.MODE === SatpMode.Sv39 ||
      (isModeVS || isModeVU) && vsatp.regOut.MODE === SatpMode.Sv39
    val isSv48: Bool =
      (isModeHS || isModeHU) &&  satp.regOut.MODE === SatpMode.Sv48 ||
      (isModeVS || isModeVU) && vsatp.regOut.MODE === SatpMode.Sv48
    val isBare = !isSv39 && !isSv48
    val sv39PC = SignExt(trapPC.take(39), XLEN)
    val sv48PC = SignExt(trapPC.take(48), XLEN)
    val barePC = ZeroExt(trapPC.take(PAddrBits), XLEN)
    // When enable virtual memory, the higher bit should fill with the msb of address of Sv39/Sv48/Sv57
    val exceptionPC = Mux1H(Seq(
      isSv39 -> sv39PC,
      isSv48 -> sv48PC,
      isBare -> barePC,
    ))

    val diffArchEvent = DifftestModule(new DiffArchEvent, delay = 3, dontCare = true)
    diffArchEvent.coreid := hartId
    diffArchEvent.valid := trapValid
    diffArchEvent.interrupt := RegEnable(interruptNO, hasTrap)
    diffArchEvent.exception := RegEnable(exceptionNO, hasTrap)
    diffArchEvent.exceptionPC := RegEnable(exceptionPC, hasTrap)
    diffArchEvent.hasNMI := RegEnable(hasNMI, hasTrap)
    diffArchEvent.virtualInterruptIsHvictlInject := virtualInterruptIsHvictlInject && hasTrap
    if (env.EnableDifftest) {
      diffArchEvent.exceptionInst := RegEnable(io.fromRob.trap.bits.instr, hasTrap)
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
    diffCSRState.mip            := mip.rdata.asUInt
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

    val diffTriggerCSRState = DifftestModule(new DiffTriggerCSRState)
    diffTriggerCSRState.coreid    := hartId
    diffTriggerCSRState.tselect   := tselect.rdata
    diffTriggerCSRState.tdata1    := tdata1.rdata
    diffTriggerCSRState.tinfo     := tinfo.rdata

    val diffVecCSRState = DifftestModule(new DiffVecCSRState)
    diffVecCSRState.coreid := hartId
    diffVecCSRState.vstart := vstart.rdata.asUInt
    diffVecCSRState.vxsat := vcsr.vxsat.asUInt
    diffVecCSRState.vxrm := vcsr.vxrm.asUInt
    diffVecCSRState.vcsr := vcsr.rdata.asUInt
    diffVecCSRState.vl := RegNext(io.fromRob.commit.vl)
    diffVecCSRState.vtype := vtype.rdata.asUInt
    diffVecCSRState.vlenb := vlenb.rdata.asUInt

    val diffFpCSRState = DifftestModule(new DiffFpCSRState)
    diffFpCSRState.coreid := hartId
    diffFpCSRState.fcsr := fcsr.rdata.asUInt

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

    val platformIRPMeipChange = !platformIRP.MEIP &&  RegNext(platformIRP.MEIP) ||
                                 platformIRP.MEIP && !RegNext(platformIRP.MEIP) ||
                                !fromAIA.meip     &&  RegNext(fromAIA.meip)     ||
                                 fromAIA.meip     && !RegNext(fromAIA.meip)
    val platformIRPMtipChange = !platformIRP.MTIP &&  RegNext(platformIRP.MTIP) || platformIRP.MTIP && !RegNext(platformIRP.MTIP)
    val platformIRPMsipChange = !platformIRP.MSIP &&  RegNext(platformIRP.MSIP) || platformIRP.MSIP && !RegNext(platformIRP.MSIP)
    val platformIRPSeipChange = !platformIRP.SEIP &&  RegNext(platformIRP.SEIP) ||
                                 platformIRP.SEIP && !RegNext(platformIRP.SEIP) ||
                                !fromAIA.seip     &&  RegNext(fromAIA.seip)     ||
                                 fromAIA.seip     && !RegNext(fromAIA.seip)
    val platformIRPStipChange = !sstcIRGen.o.STIP &&  RegNext(sstcIRGen.o.STIP) || sstcIRGen.o.STIP && !RegNext(sstcIRGen.o.STIP)
    val platformIRPVseipChange = !platformIRP.VSEIP &&  RegNext(platformIRP.VSEIP) ||
                                  platformIRP.VSEIP && !RegNext(platformIRP.VSEIP) ||
                                 !hgeip.rdata.asUInt(hstatus.regOut.VGEIN.asUInt) &&  RegNext(hgeip.rdata.asUInt(hstatus.regOut.VGEIN.asUInt)) ||
                                  hgeip.rdata.asUInt(hstatus.regOut.VGEIN.asUInt) && !RegNext(hgeip.rdata.asUInt(hstatus.regOut.VGEIN.asUInt))
    val platformIRPVstipChange = !sstcIRGen.o.VSTIP && RegNext(sstcIRGen.o.VSTIP) || sstcIRGen.o.VSTIP && !RegNext(sstcIRGen.o.VSTIP)
    val lcofiReqChange         = !lcofiReq && RegNext(lcofiReq) || lcofiReq && !RegNext(lcofiReq)

    val diffNonRegInterruptPendingEvent = DifftestModule(new DiffNonRegInterruptPendingEvent)
    diffNonRegInterruptPendingEvent.coreid           := hartId
    diffNonRegInterruptPendingEvent.valid            := platformIRPMeipChange || platformIRPMtipChange || platformIRPMsipChange ||
                                                        platformIRPSeipChange || platformIRPStipChange ||
                                                        platformIRPVseipChange || platformIRPVstipChange ||
                                                        lcofiReqChange
    diffNonRegInterruptPendingEvent.platformIRPMeip  := platformIRP.MEIP || fromAIA.meip
    diffNonRegInterruptPendingEvent.platformIRPMtip  := platformIRP.MTIP
    diffNonRegInterruptPendingEvent.platformIRPMsip  := platformIRP.MSIP
    diffNonRegInterruptPendingEvent.platformIRPSeip  := platformIRP.SEIP || fromAIA.seip
    diffNonRegInterruptPendingEvent.platformIRPStip  := sstcIRGen.o.STIP
    diffNonRegInterruptPendingEvent.platformIRPVseip := platformIRP.VSEIP || hgeip.rdata.asUInt(hstatus.regOut.VGEIN.asUInt)
    diffNonRegInterruptPendingEvent.platformIRPVstip := sstcIRGen.o.VSTIP
    diffNonRegInterruptPendingEvent.localCounterOverflowInterruptReq  := mip.regOut.LCOFIP.asBool

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