package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState
import xiangshan.backend.fu.util.CSRConst
import xiangshan._

class Debug(implicit val p: Parameters) extends Module with HasXSParameter {
  val io = IO(new DebugIO)

  private val trapInfo        = io.in.trapInfo
  private val hasTrap         = trapInfo.valid
  private val trapIsInterrupt = trapInfo.bits.isInterrupt
  private val intrVec         = trapInfo.bits.intrVec
  private val trapVec         = trapInfo.bits.trapVec
  private val singleStep      = trapInfo.bits.singleStep
  private val triggerCf       = io.in.trapInfo.bits.triggerCf

  private val privState = io.in.privState
  private val debugMode = io.in.debugMode

  private val dcsr = io.in.dcsr
  private val tcontrol = io.in.tcontrol
  private val tselect = io.in.tselect
  private val tdata1Selected = io.in.tdata1Selected
  private val tdata2Selected = io.in.tdata2Selected
  private val tdata1Vec = io.in.tdata1Vec

  private val tdata1Update  = io.in.tdata1Update
  private val tdata2Update  = io.in.tdata2Update
  private val tdata1Wdata   = io.in.tdata1Wdata

  /**
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
    (privState.isModeM && dcsr.EBREAKM.asBool) ||
      (privState.isModeHS && dcsr.EBREAKS.asBool) ||
      (privState.isModeHU && dcsr.EBREAKU.asBool) ||
      (privState.isModeVS && dcsr.EBREAKVS.asBool) ||
      (privState.isModeVU && dcsr.EBREAKVU.asBool)
  val hasDebugEbreakException = hasBreakPoint && ebreakEnterDebugMode

  // debug_exception_trigger
  val triggerFrontendHitVec = triggerCf.frontendHit
  val triggerMemHitVec = triggerCf.backendHit
  val triggerHitVec = triggerFrontendHitVec.asUInt | triggerMemHitVec.asUInt // Todo: update mcontrol.hit
  val triggerFrontendCanFireVec = triggerCf.frontendCanFire.asUInt
  val triggerMemCanFireVec = triggerCf.backendCanFire.asUInt
  val triggerCanFireVec = triggerFrontendCanFireVec | triggerMemCanFireVec

  val mcontrolWireVec = tdata1Vec.map{ mod => {
    val mcontrolWire = Wire(new Mcontrol)
    mcontrolWire := mod.DATA.asUInt
    mcontrolWire
  }}

  // More than one triggers can hit at the same time, but only fire one
  // We select the first hit trigger to fire
  val triggerFireOH = PriorityEncoderOH(triggerCanFireVec)
  val triggerFireAction = PriorityMux(triggerFireOH, tdata1Vec.map(_.getTriggerAction)).asUInt
  val hasTriggerFire = hasExp && triggerCf.canFire
  val hasDebugTriggerException = hasTriggerFire && (triggerFireAction === TrigAction.DebugMode.asUInt)
  val triggerCanFire = hasTriggerFire && (triggerFireAction === TrigAction.BreakpointExp.asUInt) &&
    Mux(privState.isModeM && !debugMode, tcontrol.MTE.asBool, true.B) // todo: Should trigger be fire in dmode?

  // debug_exception_single
  val hasSingleStep = hasExp && singleStep

  val hasDebugException = hasDebugEbreakException || hasDebugTriggerException || hasSingleStep
  val hasDebugTrap = hasDebugException || hasDebugIntr

  val tselect1H = UIntToOH(tselect.asUInt, TriggerNum).asBools
  val chainVec = mcontrolWireVec.map(_.CHAIN.asBool)
  val newTriggerChainVec = tselect1H.zip(chainVec).map{case(a, b) => a | b}
  val newTriggerChainIsLegal = TriggerUtil.TriggerCheckChainLegal(newTriggerChainVec, TriggerChainMaxLength)

  val triggerUpdate = tdata1Update || tdata2Update

  val mcontrolWdata = Wire(new Mcontrol)
  mcontrolWdata := tdata1Wdata.DATA.asUInt
  val tdata1TypeWdata = tdata1Wdata.TYPE

  val mcontrolSelected = Wire(new Mcontrol)
  mcontrolSelected := tdata1Selected.DATA.asUInt

  val frontendTriggerUpdate =
    tdata1Update && tdata1TypeWdata.isLegal && mcontrolWdata.isFetchTrigger ||
      mcontrolSelected.isFetchTrigger && triggerUpdate

  val memTriggerUpdate =
    tdata1Update && tdata1TypeWdata.isLegal && mcontrolWdata.isMemAccTrigger ||
      mcontrolSelected.isMemAccTrigger && triggerUpdate

  val triggerEnableVec = tdata1Vec.zip(mcontrolWireVec).map { case(tdata1, mcontrol) =>
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
  
  io.out.frontendTrigger.tUpdate.valid       := RegNext(RegNext(frontendTriggerUpdate))
  io.out.frontendTrigger.tUpdate.bits.addr   := tselect.asUInt
  io.out.frontendTrigger.tUpdate.bits.tdata.GenTdataDistribute(tdata1Selected, tdata2Selected)
  io.out.frontendTrigger.tEnableVec          := fetchTriggerEnableVec
  io.out.memTrigger.tUpdate.valid            := RegNext(RegNext(memTriggerUpdate))
  io.out.memTrigger.tUpdate.bits.addr        := tselect.asUInt
  io.out.memTrigger.tUpdate.bits.tdata.GenTdataDistribute(tdata1Selected, tdata2Selected)
  io.out.memTrigger.tEnableVec               := memAccTriggerEnableVec

  io.out.triggerFrontendChange  := frontendTriggerUpdate
  io.out.newTriggerChainIsLegal := newTriggerChainIsLegal

  io.out.hasDebugTrap            := hasDebugTrap
  io.out.hasDebugIntr            := hasDebugIntr
  io.out.triggerCanFire          := triggerCanFire
  io.out.hasSingleStep           := hasSingleStep
  io.out.hasTriggerFire          := hasTriggerFire
  io.out.hasDebugEbreakException := hasDebugEbreakException
  io.out.breakPoint              := breakPoint
}

class DebugIO(implicit val p: Parameters) extends Bundle with HasXSParameter {
  val in = Input(new Bundle {
    val trapInfo = ValidIO(new Bundle {
      val trapVec = UInt(64.W)
      val intrVec = UInt(64.W)
      val isInterrupt = Bool()
      val singleStep = Bool()
      val triggerCf = new TriggerCf
    })

    val privState = new PrivState
    val debugMode = Bool()

    val dcsr = new DcsrBundle
    val tcontrol = new TcontrolBundle
    val tselect = new TselectBundle(TriggerNum)
    val tdata1Selected = new Tdata1Bundle
    val tdata2Selected = new Tdata2Bundle
    val tdata1Vec = Vec(TriggerNum, new Tdata1Bundle)

    val tdata1Update = Bool()
    val tdata2Update = Bool()
    val tdata1Wdata = new Tdata1Bundle
  })

  val out = Output(new Bundle{
    // trigger
    val triggerFrontendChange = Bool()
    val newTriggerChainIsLegal = Bool()
    val memTrigger = new MemTdataDistributeIO()
    val frontendTrigger = new FrontendTdataDistributeIO()

    val hasDebugTrap = Bool()
    val hasDebugIntr = Bool()
    val hasSingleStep = Bool()
    val hasTriggerFire = Bool()
    val triggerCanFire = Bool()
    val hasDebugEbreakException = Bool()
    val breakPoint = Bool()
  })
}