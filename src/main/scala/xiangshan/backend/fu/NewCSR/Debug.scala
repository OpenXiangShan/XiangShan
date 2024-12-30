package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.cache.HasDCacheParameters
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState
import xiangshan.backend.fu.util.SdtrigExt
import xiangshan._
import utils._

class Debug(implicit val p: Parameters) extends Module with HasXSParameter {
  val io = IO(new DebugIO)

  private val trapInfo        = io.in.trapInfo
  private val hasTrap         = trapInfo.valid
  private val trapIsInterrupt = trapInfo.bits.isInterrupt
  private val isDebugIntr     = trapInfo.bits.isDebugIntr
  private val trapVec         = trapInfo.bits.trapVec
  private val singleStep      = trapInfo.bits.singleStep
  private val trigger         = io.in.trapInfo.bits.trigger

  private val privState = io.in.privState
  private val debugMode = io.in.debugMode

  private val dcsr = io.in.dcsr
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
   *    5. critical error state(when dcsr.cetrig assert)
   */
  // debug_intr
  val hasIntr = hasTrap && trapIsInterrupt
  val hasDebugIntr = hasIntr && isDebugIntr

  // debug_exception_ebreak
  val hasExp = hasTrap && !trapIsInterrupt
  val breakPoint = trapVec(ExceptionNO.breakPoint).asBool
  val isEbreak = hasExp && breakPoint && !TriggerAction.isExp(trigger)
  val ebreakEnterDebugMode =
    (privState.isModeM && dcsr.EBREAKM.asBool) ||
      (privState.isModeHS && dcsr.EBREAKS.asBool) ||
      (privState.isModeHU && dcsr.EBREAKU.asBool) ||
      (privState.isModeVS && dcsr.EBREAKVS.asBool) ||
      (privState.isModeVU && dcsr.EBREAKVU.asBool)
  val hasDebugEbreakException = isEbreak && ebreakEnterDebugMode

  // debug_exception_trigger
  val mcontrol6WireVec = tdata1Vec.map{ mod => {
    val mcontrol6Wire = Wire(new Mcontrol6)
    mcontrol6Wire := mod.DATA.asUInt
    mcontrol6Wire
  }}

  val triggerCanRaiseBpExp = io.in.triggerCanRaiseBpExp
  val triggerEnterDebugMode = hasExp && TriggerAction.isDmode(trigger)

  // debug_exception_single
  val hasSingleStep = hasExp && singleStep


  // critical error state
  val criticalErrorStateEnterDebug = trapInfo.bits.criticalErrorState && dcsr.CETRIG.asBool

  val hasDebugException = hasDebugEbreakException || triggerEnterDebugMode || hasSingleStep || criticalErrorStateEnterDebug
  val hasDebugTrap = hasDebugException || hasDebugIntr

  val tselect1H = UIntToOH(tselect.asUInt, TriggerNum).asBools
  val chainVec = mcontrol6WireVec.map(_.CHAIN.asBool)
  val newTriggerChainVec = tselect1H.zip(chainVec).map{case(a, b) => a | b}
  val newTriggerChainIsLegal = TriggerUtil.TriggerCheckChainLegal(newTriggerChainVec, TriggerChainMaxLength)

  val triggerUpdate = tdata1Update || tdata2Update

  val mcontrol6Wdata = Wire(new Mcontrol6)
  mcontrol6Wdata := tdata1Wdata.DATA.asUInt
  val tdata1TypeWdata = tdata1Wdata.TYPE

  val mcontrol6Selected = Wire(new Mcontrol6)
  mcontrol6Selected := tdata1Selected.DATA.asUInt

  val frontendTriggerUpdate =
    tdata1Update && tdata1TypeWdata.isLegal && mcontrol6Wdata.isFetchTrigger ||
      mcontrol6Selected.isFetchTrigger && triggerUpdate

  val memTriggerUpdate =
    tdata1Update && tdata1TypeWdata.isLegal && mcontrol6Wdata.isMemAccTrigger ||
      mcontrol6Selected.isMemAccTrigger && triggerUpdate

  val triggerEnableVec = tdata1Vec.zip(mcontrol6WireVec).map { case(tdata1, mcontrol6) =>
    tdata1.TYPE.isLegal && (
      mcontrol6.M && privState.isModeM  ||
        mcontrol6.S && privState.isModeHS ||
        mcontrol6.U && privState.isModeHU ||
        mcontrol6.VS && privState.isModeVS ||
        mcontrol6.VU && privState.isModeVU)
  }

  val fetchTriggerEnableVec = triggerEnableVec.zip(mcontrol6WireVec).map {
    case (tEnable, mod) => tEnable && mod.isFetchTrigger
  }
  val memAccTriggerEnableVec = triggerEnableVec.zip(mcontrol6WireVec).map {
    case (tEnable, mod) => tEnable && mod.isMemAccTrigger
  }
  
  io.out.frontendTrigger.tUpdate.valid        := RegNext(RegNext(frontendTriggerUpdate))
  io.out.frontendTrigger.tUpdate.bits.addr    := tselect.asUInt
  io.out.frontendTrigger.tUpdate.bits.tdata.GenTdataDistribute(tdata1Selected, tdata2Selected)
  io.out.frontendTrigger.tEnableVec           := fetchTriggerEnableVec
  io.out.frontendTrigger.triggerCanRaiseBpExp := triggerCanRaiseBpExp
  io.out.frontendTrigger.debugMode            := debugMode

  io.out.memTrigger.tUpdate.valid            := RegNext(RegNext(memTriggerUpdate))
  io.out.memTrigger.tUpdate.bits.addr        := tselect.asUInt
  io.out.memTrigger.tUpdate.bits.tdata.GenTdataDistribute(tdata1Selected, tdata2Selected)
  io.out.memTrigger.tEnableVec               := memAccTriggerEnableVec
  io.out.memTrigger.triggerCanRaiseBpExp     := triggerCanRaiseBpExp
  io.out.memTrigger.debugMode                := debugMode

  io.out.triggerFrontendChange  := frontendTriggerUpdate
  io.out.newTriggerChainIsLegal := newTriggerChainIsLegal

  io.out.hasDebugTrap                 := hasDebugTrap
  io.out.hasDebugIntr                 := hasDebugIntr
  io.out.hasSingleStep                := hasSingleStep
  io.out.triggerEnterDebugMode        := triggerEnterDebugMode
  io.out.hasDebugEbreakException      := hasDebugEbreakException
  io.out.breakPoint                   := breakPoint
  io.out.criticalErrorStateEnterDebug := criticalErrorStateEnterDebug
}

class DebugIO(implicit val p: Parameters) extends Bundle with HasXSParameter {
  val in = Input(new Bundle {
    val trapInfo = ValidIO(new Bundle {
      val trapVec = UInt(64.W)
      val isDebugIntr = Bool()
      val isInterrupt = Bool()
      val singleStep = Bool()
      val trigger = TriggerAction()
      val criticalErrorState = Bool()
    })

    val privState = new PrivState
    val debugMode = Bool()

    val dcsr = new DcsrBundle
    val tselect = new TselectBundle(TriggerNum)
    val tdata1Selected = new Tdata1Bundle
    val tdata2Selected = new Tdata2Bundle
    val tdata1Vec = Vec(TriggerNum, new Tdata1Bundle)
    val triggerCanRaiseBpExp = Bool()

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
    val triggerEnterDebugMode = Bool()
    val hasDebugEbreakException = Bool()
    val breakPoint = Bool()
    val criticalErrorStateEnterDebug = Bool()
  })
}

class CsrTriggerBundle(implicit val p: Parameters) extends Bundle with HasXSParameter {
  val tdataVec = Vec(TriggerNum, new MatchTriggerIO)
  val tEnableVec = Vec(TriggerNum, Bool())
  val debugMode = Bool()
  val triggerCanRaiseBpExp = Bool()
}

object MemType {
  val LOAD  = true
  val STORE = false
}


class BaseTriggerIO(implicit p: Parameters) extends XSBundle{
  val fromCsrTrigger = Input(new CsrTriggerBundle)

  val fromLoadStore = Input(new Bundle {
    val vaddr = UInt(VAddrBits.W)
    val isVectorUnitStride = Bool()
    val mask = UInt((VLEN/8).W)
  })

  val toLoadStore = Output(new Bundle{
    val triggerAction = TriggerAction()
    val triggerVaddr  = UInt(VAddrBits.W)
    val triggerMask  = UInt((VLEN/8).W)
  })
}


abstract class BaseTrigger()(implicit val p: Parameters) extends Module with HasXSParameter with SdtrigExt with HasDCacheParameters {
  lazy val io = IO(new BaseTriggerIO)

  def getTriggerHitVec(): Vec[Bool]
  def highBitsEq(): Vec[Bool]
  def DcacheLineBitsEq(): (Bool, Vec[Bool])

  val tdataVec      = io.fromCsrTrigger.tdataVec
  val tEnableVec    = io.fromCsrTrigger.tEnableVec
  val triggerCanRaiseBpExp = io.fromCsrTrigger.triggerCanRaiseBpExp
  val debugMode = io.fromCsrTrigger.debugMode
  val vaddr = io.fromLoadStore.vaddr

  val triggerTimingVec = VecInit(tdataVec.map(_.timing))
  val triggerChainVec = VecInit(tdataVec.map(_.chain))

  // Trigger can't hit/fire in debug mode.
  val triggerHitVec = getTriggerHitVec()
  val triggerCanFireVec = WireInit(VecInit(Seq.fill(TriggerNum)(false.B)))
  // for vector unit-stride, match Type only support equal
  val lowBitWidth = log2Up(VLEN/8)
  val isVectorStride = io.fromLoadStore.isVectorUnitStride
  val mask = io.fromLoadStore.mask

  val (isCacheLine, cacheLineEq) = DcacheLineBitsEq()

  val highEq = highBitsEq()

  val lowMatch = tdataVec.map(tdata => UIntToOH(tdata.tdata2(lowBitWidth-1, 0)) & mask)
  val lowEq  = VecInit(lowMatch.map(lm => lm.orR))

  val hitVecVectorStride  = VecInit(highEq.zip(lowEq).map{case(hi, lo) => hi && lo})

  val tiggerVaddrHit = Mux(isCacheLine, cacheLineEq, Mux(isVectorStride, hitVecVectorStride, triggerHitVec))
  TriggerCheckCanFire(TriggerNum, triggerCanFireVec, tiggerVaddrHit, triggerTimingVec, triggerChainVec)
  val triggerFireOH = PriorityEncoderOH(triggerCanFireVec)
  val triggerVaddr  = PriorityMux(triggerFireOH, VecInit(tdataVec.map(_.tdata2))).asUInt
  val triggerMask   = PriorityMux(triggerFireOH, VecInit(tdataVec.map(x => UIntToOH(x.tdata2(lowBitWidth-1, 0))))).asUInt

  val actionVec = VecInit(tdataVec.map(_.action))
  val triggerAction = Wire(TriggerAction())
  TriggerUtil.triggerActionGen(triggerAction, triggerCanFireVec, actionVec, triggerCanRaiseBpExp)

  io.toLoadStore.triggerAction := triggerAction
  io.toLoadStore.triggerVaddr  := triggerVaddr
  io.toLoadStore.triggerMask   := triggerMask
}


class MemTrigger(memType: Boolean = MemType.LOAD)(override implicit val p: Parameters) extends BaseTrigger {

  class MemTriggerIO extends BaseTriggerIO{
    val isCbo = OptionWrapper(memType == MemType.STORE, Input(Bool()))
  }

  override lazy val io = IO(new MemTriggerIO)

  override def getTriggerHitVec(): Vec[Bool] = {
    val triggerHitVec = WireInit(VecInit(Seq.fill(TriggerNum)(false.B)))
    for (i <- 0 until TriggerNum) {
      triggerHitVec(i) := !tdataVec(i).select && !debugMode && TriggerCmp(
      vaddr,
      tdataVec(i).tdata2,
      tdataVec(i).matchType,
      tEnableVec(i) && (if(memType == MemType.LOAD) tdataVec(i).load else tdataVec(i).store)
      )
    }
    triggerHitVec
  }

  override def highBitsEq(): Vec[Bool] = {
    VecInit(tdataVec.zip(tEnableVec).map{ case(tdata, en) =>
      !tdata.select && !debugMode && en &&
        (if(memType == MemType.LOAD) tdata.load else tdata.store) &&
        (vaddr >> lowBitWidth) === (tdata.tdata2 >> lowBitWidth)
    })
  }

  def DcacheLineBitsEq(): (Bool, Vec[Bool])= {
    (
    io.isCbo.getOrElse(false.B),
    VecInit(tdataVec.zip(tEnableVec).map{ case(tdata, en) =>
      !tdata.select && !debugMode && en &&
        tdata.store && io.isCbo.getOrElse(false.B) &&
        (vaddr >> DCacheLineOffset) === (tdata.tdata2 >> DCacheLineOffset)
    })
    )
  }

}

class VSegmentTrigger(override implicit val p: Parameters) extends BaseTrigger {

  class VSegmentTriggerIO extends BaseTriggerIO{
    val memType = Input(Bool())
  }

  override lazy val io = IO(new VSegmentTriggerIO)

  override def getTriggerHitVec(): Vec[Bool] = {
    val triggerHitVec = WireInit(VecInit(Seq.fill(TriggerNum)(false.B)))
    for (i <- 0 until TriggerNum) {
      triggerHitVec(i) := !tdataVec(i).select && !debugMode && TriggerCmp(
        vaddr,
        tdataVec(i).tdata2,
        tdataVec(i).matchType,
        tEnableVec(i) && Mux(io.memType === MemType.LOAD.asBool, tdataVec(i).load, tdataVec(i).store)
      )
    }
    triggerHitVec
  }

  override def highBitsEq(): Vec[Bool] = {
    VecInit(tdataVec.zip(tEnableVec).map{ case(tdata, en) =>
      !tdata.select && !debugMode && en &&
        Mux(io.memType === MemType.LOAD.asBool, tdata.load, tdata.store) &&
        (vaddr >> lowBitWidth) === (tdata.tdata2 >> lowBitWidth)
    })
  }

  // vector segment does not have a cbo
  def DcacheLineBitsEq(): (Bool, Vec[Bool]) = {
    (false.B, VecInit(Seq.fill(tdataVec.length)(false.B)))
  }
}