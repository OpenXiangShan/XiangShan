package xiangshan.backend.fu.NewCSR

import chisel3._
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW}
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState


class SstateenBundle0 extends CSRBundle {
  override val len: Int = 32
  val JVT  = RO(2).withReset(0.U) // jvt CSR in Zcmt extension
  val FCSR = RO(1).withReset(0.U) // fp inst op 'x' register not f in Zfinx, Zdinx; misa.F =1 -> RO 0; misa.F=0 & this=0 -> V/EX_II
  val C    = RW(0).withReset(1.U) // custom state enable, [m|h|s]stateen is standard, not custom.
}

class HstateenBundle0 extends SstateenBundle0 {
  override val len: Int = 64
  val SE0     = RW(63).withReset(1.U) // m: [h|s]stateen                h: sstateen
  val ENVCFG  = RW(62).withReset(1.U) // m: [h|s]envcfg                 h: senvcfg
  // Bits in any stateen CSR that are defined to control state that a hart doesn’t implement are read-only
  // zeros for that hart. Smcsrind/Sscsrind is not implemented.
  val CSRIND  = RO(60).withReset(1.U) // m: [vs|s]iselect, [vs|s]ireg*  h: siselect, sireg*
  val AIA     = RW(59).withReset(1.U) // all other state added by the AIA and not controlled by bits 60 and 58
  val IMSIC   = RW(58).withReset(1.U) // m: [vs|s]topei                 h: stopei
  val CONTEXT = RO(57).withReset(0.U) // m: [h|s]context in Sdtrig      h: scontext
}

class MstateenBundle0 extends HstateenBundle0 {
  val P1P13   = RO(56).withReset(0.U) // hedelegh in Priv Spec V1.13
}

trait HasStateen0Bundle { self: CSRModule[_] =>
  val fromMstateen0 = IO(Input(new MstateenBundle0))
  val fromHstateen0 = IO(Input(new HstateenBundle0))
  val privState     = IO(Input(new PrivState))
}
