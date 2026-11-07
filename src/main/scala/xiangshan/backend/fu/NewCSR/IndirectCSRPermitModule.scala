package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState

abstract class IndirectCSRWindowPermitModule extends Module {
  val io = IO(new Bundle() {
    val in = Input(new Bundle {
      val csrAccess = new csrAccessIO
      val privState = new PrivState
      val aia = new aiaIO
      val xcounteren = new xcounterenIO
      val xenvcfg = new xenvcfgIO
      val xstateen = new xstateenIO
    })
    val out = Output(new Bundle {
      val indirectCSRExII = Bool()
      val indirectCSRExVI = Bool()
    })
  })

  protected final val (addr, privState) = (
    io.in.csrAccess.addr,
    io.in.privState,
  )

  protected final val (miselect, siselect, vsiselect) = (
    io.in.aia.miselect,
    io.in.aia.siselect,
    io.in.aia.vsiselect,
  )

  protected final val isModeM  = privState.isModeM
  protected final val isModeHS = privState.isModeHS
  protected final val isModeVS = privState.isModeVS
  protected final val isModeVU = privState.isModeVU
  protected final val isVirtual = privState.isVirtual

  protected final val isMireg = Ireg.isInMireg(addr)
  protected final val isMiregX = Ireg.isInMiregX(addr)
  protected final val isMireg2To6 = Ireg.isInMireg2To6(addr)

  protected final val isSireg = Ireg.isInSireg(addr)
  protected final val isSireg2 = Ireg.isInSireg2(addr)
  protected final val isSiregX = Ireg.isInSiregX(addr)
  protected final val isSireg2To6 = Ireg.isInSireg2To6(addr)
  protected final val isSireg3To6 = Ireg.isInSireg3To6(addr)

  protected final val isVSireg = Ireg.isInVSireg(addr)
  protected final val isVSiregX = Ireg.isInVSiregX(addr)
  protected final val isVSireg2To6 = Ireg.isInVSireg2To6(addr)

  // In VS/VU-mode, accesses to sireg* are really accesses to vsireg* selected by vsiselect.
  protected final val effectiveSiregSelect = Mux(isVirtual, vsiselect, siselect)
}

class IndirectSmcdelegPermitModule extends IndirectCSRWindowPermitModule {
  private val menvcfgCDE = io.in.xenvcfg.menvcfg(60)
  // Counter delegation is defined only for the supervisor-level siselect window.
  // CSRIND only control direct access.
  private val siregCounterDelegated = io.in.xcounteren.mcounteren(siselect(4, 0))

  private val miregInSmcdeleg = Iselect.isInSmcdeleg(miselect)
  private val vsiregInSmcdeleg = Iselect.isInSmcdeleg(vsiselect)
  private val siregInSmcdeleg = Iselect.isInSmcdeleg(effectiveSiregSelect)
  private val siregIsSmcdelegTime = Iselect.isSmcdelegTime(siselect)

  private val rwMiregXExII = miregInSmcdeleg && isMiregX

  private val rwVSiregXExII = vsiregInSmcdeleg && isVSiregX

  private val rwSireg1To2ExII = (
    !isVirtual && (!menvcfgCDE || !siregCounterDelegated || siregIsSmcdelegTime) ||
    isModeVS && !menvcfgCDE
  ) && siregInSmcdeleg && (isSireg || isSireg2)

  private val rwSireg3To6ExII = (
    !isVirtual ||
    isModeVS && !menvcfgCDE
  ) && siregInSmcdeleg && isSireg3To6

  private val rwSiregXExVI = isModeVS && menvcfgCDE && siregInSmcdeleg && isSiregX

  io.out.indirectCSRExII :=  rwMiregXExII || rwVSiregXExII || rwSireg1To2ExII || rwSireg3To6ExII
  io.out.indirectCSRExVI := rwSiregXExVI
}

class IndirectAIAPermitModule extends IndirectCSRWindowPermitModule {
  private val mstateen0 = io.in.xstateen.mstateen0

  private val miregInAIA = Iselect.isInAIA(miselect)
  private val siregInAIA = Iselect.isInAIA(effectiveSiregSelect)
  private val vsiregInAIA = Iselect.isInAIA(vsiselect)

  private val rwMiregExII = miregInAIA && Iselect.isOdd(miselect) && isMireg
  private val rwMireg2To6ExII = miregInAIA && isMireg2To6

  private val rwVSiregXExII = vsiregInAIA && isVSiregX

  private val rwSiregExII = (
    !isVirtual && Iselect.isOdd(siselect) ||
    isModeHS && !mstateen0.AIA.asBool
  ) && siregInAIA && isSireg

  private val rwSiregExVI = isModeVS && siregInAIA && isSireg

  private val rwSireg2To6ExII = !isVirtual && siregInAIA && isSireg2To6
  private val rwSireg2To6ExVI = isModeVS && siregInAIA && isSireg2To6


  io.out.indirectCSRExII := (
    rwMiregExII ||
    rwMireg2To6ExII ||
    rwVSiregXExII ||
    rwSiregExII ||
    rwSireg2To6ExII
  )
  io.out.indirectCSRExVI := rwSiregExVI || rwSireg2To6ExVI
}

class IndirectIMSICPermitModule extends IndirectCSRWindowPermitModule {
  private val mstateen0 = io.in.xstateen.mstateen0
  private val hstateen0 = io.in.xstateen.hstateen0
  private val mvienSEIE = io.in.aia.mvienSEIE

  private val miregInIMSIC = Iselect.isInImsic(miselect)
  private val siregInIMSIC = Iselect.isInImsic(effectiveSiregSelect)
  private val vsiregInIMSIC = Iselect.isInImsic(vsiselect)

  private val rwMireg2To6ExII = miregInIMSIC && isMireg2To6

  private val rwVSiregExII = vsiregInIMSIC && isModeHS && !mstateen0.IMSIC.asBool && isVSireg
  private val rwVSireg2To6ExII = vsiregInIMSIC && isVSireg2To6

  private val rwSiregExII = (
    isModeHS && (mvienSEIE || !mstateen0.IMSIC.asBool) ||
    isModeVS && !mstateen0.IMSIC.asBool
  ) && siregInIMSIC && isSireg
  private val rwSiregExVI = isModeVS && !hstateen0.IMSIC.asBool && siregInIMSIC && isSireg

  private val rwSireg2To6ExII = !isVirtual && siregInIMSIC && isSireg2To6
  private val rwSireg2To6ExVI = isModeVS && vsiregInIMSIC && isSireg2To6



  io.out.indirectCSRExII := (
    rwMireg2To6ExII ||
    rwVSiregExII ||
    rwVSireg2To6ExII ||
    rwSiregExII ||
    rwSireg2To6ExII
  )
  io.out.indirectCSRExVI := rwSiregExVI || rwSireg2To6ExVI
}

class IndirectOtherPermitModule extends IndirectCSRWindowPermitModule {
  private val miregInOther = Iselect.isInOthers(miselect)
  private val siregInOther = Iselect.isInOthers(effectiveSiregSelect)
  private val vsiregInOther = Iselect.isInOthers(vsiselect)

  private val rwMiregXExII = miregInOther && isMiregX

  private val rwVSiregXExII = vsiregInOther && isVSiregX

  private val rwSiregXExII = siregInOther && isSiregX


  io.out.indirectCSRExII := rwMiregXExII || rwSiregXExII || rwVSiregXExII
  io.out.indirectCSRExVI := false.B
}

class IndirectCSRPermitModule extends Module {
  val io = IO(new Bundle() {
    val in = Input(new Bundle {
      val csrAccess = new csrAccessIO
      val privState = new PrivState
      val aia = new aiaIO
      val xcounteren = new xcounterenIO
      val xenvcfg = new xenvcfgIO
      val xstateen = new xstateenIO
    })
    val out = Output(new Bundle {
      val indirectCSRExII = Bool()
      val indirectCSRExVI = Bool()
    })
  })

  private val smcdelegPermitMod = Module(new IndirectSmcdelegPermitModule)
  private val aiaPermitMod = Module(new IndirectAIAPermitModule)
  private val imsicPermitMod = Module(new IndirectIMSICPermitModule)
  private val otherPermitMod = Module(new IndirectOtherPermitModule)

  smcdelegPermitMod.io.in := io.in
  aiaPermitMod.io.in := io.in
  imsicPermitMod.io.in := io.in
  otherPermitMod.io.in := io.in

  io.out.indirectCSRExII := (
    smcdelegPermitMod.io.out.indirectCSRExII ||
    aiaPermitMod.io.out.indirectCSRExII ||
    imsicPermitMod.io.out.indirectCSRExII ||
    otherPermitMod.io.out.indirectCSRExII
  )
  io.out.indirectCSRExVI := (
    smcdelegPermitMod.io.out.indirectCSRExVI ||
    aiaPermitMod.io.out.indirectCSRExVI ||
    imsicPermitMod.io.out.indirectCSRExVI ||
    otherPermitMod.io.out.indirectCSRExVI
  )
}
