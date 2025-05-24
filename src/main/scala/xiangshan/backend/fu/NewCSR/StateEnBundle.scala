package xiangshan.backend.fu.NewCSR

import chisel3._
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW}
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState


class Sstateen0Bundle extends CSRBundle {
  override val len: Int = 32
  val JVT  = RO(2).withReset(0.U) // jvt CSR in Zcmt extension
  val FCSR = RO(1).withReset(0.U) // fp inst op 'x' register not f in Zfinx, Zdinx; misa.F =1 -> RO 0; misa.F=0 & this=0 -> V/EX_II
  val C    = RW(0)                // custom state enable, [m|h|s]stateen is standard, not custom.
}

class Hstateen0Bundle extends Sstateen0Bundle {
  override val len: Int = 64
  val SE0     = RW(63)                // m: [h|s]stateen                h: sstateen
  val ENVCFG  = RW(62)                // m: [h|s]envcfg                 h: senvcfg
  // Bits in any stateen CSR that are defined to control state that a hart doesn’t implement are read-only
  // zeros for that hart. Smcsrind/Sscsrind is not implemented.
  val CSRIND  = RW(60)                // m: [vs|s]iselect, [vs|s]ireg*  h: siselect, sireg*
  val AIA     = RW(59)                // all other state added by the AIA and not controlled by bits 60 and 58
  val IMSIC   = RW(58)                // m: [vs|s]topei                 h: stopei
  val CONTEXT = RW(57)                // m: [h|s]context in Sdtrig      h: scontext
}

class Mstateen0Bundle extends Hstateen0Bundle {
  override val SE0     = RW(63).withReset(0.U) // m: [h|s]stateen                h: sstateen
  override val ENVCFG  = RW(62).withReset(0.U) // m: [h|s]envcfg                 h: senvcfg
  // Bits in any stateen CSR that are defined to control state that a hart doesn’t implement are read-only
  // zeros for that hart. Smcsrind/Sscsrind is not implemented.
  override val CSRIND  = RW(60).withReset(0.U) // m: [vs|s]iselect, [vs|s]ireg*  h: siselect, sireg*
  override val AIA     = RW(59).withReset(0.U) // all other state added by the AIA and not controlled by bits 60 and 58
  override val IMSIC   = RW(58).withReset(0.U) // m: [vs|s]topei                 h: stopei
  override val CONTEXT = RW(57).withReset(0.U) // m: [h|s]context in Sdtrig      h: scontext
  val P1P13            = RO(56).withReset(0.U) // hedelegh in Priv Spec V1.13
  override val C       = RW(0).withReset(0.U)  // custom state enable, [m|h|s]stateen is standard, not custom.
}

class SstateenNonZeroBundle extends CSRBundle {  // for sstateen[1|2|3]
  override val len = 32
  val ALL = RO(31, 0)
}

class HstateenNonZeroBundle extends CSRBundle {  // for hstateen[1|2|3]
  val SE = RW(63)
}
class MstateenNonZeroBundle extends HstateenNonZeroBundle {  // for mstateen[1|2|3]
  override val SE = RW(63).withReset(0.U)
}

trait HasStateenBundle { self: CSRModule[_] =>
  val fromMstateen0 = IO(Input(new Mstateen0Bundle))
  val fromMstateen1 = IO(Input(new MstateenNonZeroBundle))
  val fromMstateen2 = IO(Input(new MstateenNonZeroBundle))
  val fromMstateen3 = IO(Input(new MstateenNonZeroBundle))
  val fromHstateen0 = IO(Input(new Hstateen0Bundle))
  val privState     = IO(Input(new PrivState))
}
