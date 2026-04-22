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
      val indirectCSR_EX_II = Bool()
      val indirectCSR_EX_VI = Bool()
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
  protected final val isMireg2_6 = Ireg.isInMireg2_6(addr)

  protected final val isSireg = Ireg.isInSireg(addr)
  protected final val isSireg2 = Ireg.isInSireg2(addr)
  protected final val isSiregX = Ireg.isInSiregX(addr)
  protected final val isSireg2_6 = Ireg.isInSireg2_6(addr)
  protected final val isSireg3_6 = Ireg.isInSireg3_6(addr)

  protected final val isVSireg = Ireg.isInVSireg(addr)
  protected final val isVSiregX = Ireg.isInVSiregX(addr)
  protected final val isVSireg2_6 = Ireg.isInVSireg2_6(addr)

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

  private val rwMiregX_EX_II = miregInSmcdeleg && isMiregX

  private val rwVSiregX_EX_II = vsiregInSmcdeleg && isVSiregX

  private val rwSireg1_2_EX_II = (
    !isVirtual && (!menvcfgCDE || !siregCounterDelegated || siregIsSmcdelegTime) ||
    isModeVS && !menvcfgCDE
  ) && siregInSmcdeleg && (isSireg || isSireg2)

  private val rwSireg3_6_EX_II = (
    !isVirtual ||
    isModeVS && !menvcfgCDE
  ) && siregInSmcdeleg && isSireg3_6

  private val rwSiregX_EX_VI = isModeVS && menvcfgCDE && siregInSmcdeleg && isSiregX

  io.out.indirectCSR_EX_II :=  rwMiregX_EX_II || rwVSiregX_EX_II || rwSireg1_2_EX_II || rwSireg3_6_EX_II
  io.out.indirectCSR_EX_VI := rwSiregX_EX_VI
}

class IndirectAIAPermitModule extends IndirectCSRWindowPermitModule {
  private val mstateen0 = io.in.xstateen.mstateen0

  private val miregInAIA = Iselect.isInAIA(miselect)
  private val siregInAIA = Iselect.isInAIA(effectiveSiregSelect)
  private val vsiregInAIA = Iselect.isInAIA(vsiselect)

  private val rwMireg_EX_II = miregInAIA && Iselect.isOdd(miselect) && isMireg
  private val rwMireg2_6_EX_II = miregInAIA && isMireg2_6

  private val rwVSiregX_EX_II = vsiregInAIA && isVSiregX

  private val rwSireg_EX_II = (
    !isVirtual && Iselect.isOdd(siselect) ||
    isModeHS && !mstateen0.AIA.asBool ||
    isModeVS && !mstateen0.AIA.asBool
  ) && siregInAIA && isSireg

  private val rwSireg_EX_VI = isModeVS && siregInAIA && isSireg

  private val rwSireg2_6_EX_II = !isVirtual && siregInAIA && isSireg2_6
  private val rwSireg2_6_EX_VI = isModeVS && siregInAIA && isSireg2_6


  io.out.indirectCSR_EX_II := (
    rwMireg_EX_II ||
    rwMireg2_6_EX_II ||
    rwVSiregX_EX_II ||
    rwSireg_EX_II ||
    rwSireg2_6_EX_II
  )
  io.out.indirectCSR_EX_VI := rwSireg_EX_VI || rwSireg2_6_EX_VI
}

class IndirectIMSICPermitModule extends IndirectCSRWindowPermitModule {
  private val mstateen0 = io.in.xstateen.mstateen0
  private val hstateen0 = io.in.xstateen.hstateen0
  private val mvienSEIE = io.in.aia.mvienSEIE

  private val miregInIMSIC = Iselect.isInImsic(miselect)
  private val siregInIMSIC = Iselect.isInImsic(effectiveSiregSelect)
  private val vsiregInIMSIC = Iselect.isInImsic(vsiselect)

  private val rwMireg2_6_EX_II = miregInIMSIC && isMireg2_6

  private val rwVSireg_EX_II = vsiregInIMSIC && isModeHS && !mstateen0.IMSIC.asBool && isVSireg
  private val rwVSireg2_6_EX_II = vsiregInIMSIC && isVSireg2_6

  private val rwSireg_EX_II = (
    isModeHS && (mvienSEIE || !mstateen0.IMSIC.asBool) ||
    isModeVS && !mstateen0.IMSIC.asBool
  ) && siregInIMSIC && isSireg
  private val rwSireg_EX_VI = isModeVS && !hstateen0.IMSIC.asBool && siregInIMSIC && isSireg

  private val rwSireg2_6_EX_II = !isVirtual && siregInIMSIC && isSireg2_6
  private val rwSireg2_6_EX_VI = isModeVS && vsiregInIMSIC && isSireg2_6



  io.out.indirectCSR_EX_II := (
    rwMireg2_6_EX_II ||
    rwVSireg_EX_II ||
    rwVSireg2_6_EX_II ||
    rwSireg_EX_II ||
    rwSireg2_6_EX_II
  )
  io.out.indirectCSR_EX_VI := rwSireg_EX_VI || rwSireg2_6_EX_VI
}

class IndirectOtherPermitModule extends IndirectCSRWindowPermitModule {
  private val miregInOther = Iselect.isInOthers(miselect)
  private val siregInOther = Iselect.isInOthers(effectiveSiregSelect)
  private val vsiregInOther = Iselect.isInOthers(vsiselect)

  private val rwMiregX_EX_II = miregInOther && isMiregX

  private val rwVSiregX_EX_II = vsiregInOther && isVSiregX

  private val rwSiregX_EX_II = siregInOther && isSiregX


  io.out.indirectCSR_EX_II := rwMiregX_EX_II || rwSiregX_EX_II || rwVSiregX_EX_II
  io.out.indirectCSR_EX_VI := false.B
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
      val indirectCSR_EX_II = Bool()
      val indirectCSR_EX_VI = Bool()
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

  io.out.indirectCSR_EX_II := (
    smcdelegPermitMod.io.out.indirectCSR_EX_II ||
    aiaPermitMod.io.out.indirectCSR_EX_II ||
    imsicPermitMod.io.out.indirectCSR_EX_II ||
    otherPermitMod.io.out.indirectCSR_EX_II
  )
  io.out.indirectCSR_EX_VI := (
    smcdelegPermitMod.io.out.indirectCSR_EX_VI ||
    aiaPermitMod.io.out.indirectCSR_EX_VI ||
    imsicPermitMod.io.out.indirectCSR_EX_VI ||
    otherPermitMod.io.out.indirectCSR_EX_VI
  )
}
