package xiangshan.backend.fu.NewCSR

import chisel3._
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW}
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState


class Sstateen0Bundle extends CSRBundle {
  override val len: Int = 32
  val JVT  = RO(2).withReset(0.U).withDescription("Enable jump-vector-table state from the Zcmt extension.")
  val FCSR = RO(1).withReset(0.U).withDescription("Enable floating-point CSR state. When misa.F is clear and this bit is 0, lower-privilege floating-point CSR access is blocked.")
  val C    = RW(0).withDescription("Enable implementation-defined custom state.")
}

class Hstateen0Bundle extends Sstateen0Bundle {
  override val len: Int = 64
  val SE0     = RW(63).withDescription("Enable access to lower-privilege state-enable CSRs, namely hstateen and sstateen from M-mode or sstateen from HS-mode.")
  val ENVCFG  = RW(62).withDescription("Enable access to lower-privilege envcfg CSRs, namely henvcfg and senvcfg from M-mode or senvcfg from HS-mode.")
  // Bits in any stateen CSR that are defined to control state that a hart doesn’t implement are read-only
  // zeros for that hart. Smcsrind/Sscsrind is not implemented.
  val CSRIND  = RW(60).withDescription("Enable indirect CSR access state, including [vs|s]iselect and [vs|s]ireg* from M-mode or siselect and sireg* from HS-mode.")
  val AIA     = RW(59).withDescription("Enable AIA state not covered by CSRIND or IMSIC.")
  val IMSIC   = RW(58).withDescription("Enable IMSIC state and top-of-interrupt CSRs, including [vs|s]topei from M-mode or stopei from HS-mode.")
  val CONTEXT = RW(57).withDescription("Enable scontext and hcontext state, including [h|s]context in Sdtrig from M-mode or scontext from HS-mode.")
}

class Mstateen0Bundle extends Hstateen0Bundle {
  override val SE0     = RW(63).withReset(0.U).withDescription("Enable access to lower-privilege state-enable CSRs, namely hstateen and sstateen from M-mode or sstateen from HS-mode.")
  override val ENVCFG  = RW(62).withReset(0.U).withDescription("Enable access to lower-privilege envcfg CSRs, namely henvcfg and senvcfg from M-mode or senvcfg from HS-mode.")
  // Bits in any stateen CSR that are defined to control state that a hart doesn’t implement are read-only
  // zeros for that hart. Smcsrind/Sscsrind is not implemented.
  override val CSRIND  = RW(60).withReset(0.U).withDescription("Enable indirect CSR access state, including [vs|s]iselect and [vs|s]ireg* from M-mode or siselect and sireg* from HS-mode.")
  override val AIA     = RW(59).withReset(0.U).withDescription("Enable AIA state not covered by CSRIND or IMSIC.")
  override val IMSIC   = RW(58).withReset(0.U).withDescription("Enable IMSIC state and top-of-interrupt CSRs, including [vs|s]topei from M-mode or stopei from HS-mode.")
  override val CONTEXT = RW(57).withReset(0.U).withDescription("Enable scontext and hcontext state, including [h|s]context in Sdtrig from M-mode or scontext from HS-mode.")
  val P1P13            = RO(56).withReset(0.U).withDescription("Enable access to hedelegh as defined by Privileged Spec 1.13.")
  override val C       = RW(0).withReset(0.U).withDescription("Enable implementation-defined custom state.")
}

class SstateenNonZeroBundle extends CSRBundle {  // for sstateen[1|2|3]
  override val len = 32
  val ALL = RO(31, 0).withDescription("Reserved supervisor state-enable bits. XiangShan implements them as zero.")
}

class HstateenNonZeroBundle extends CSRBundle {  // for hstateen[1|2|3]
  val SE = RW(63).withDescription("Enable access to the next lower-privilege state-enable CSR in this slot.")
}
class MstateenNonZeroBundle extends HstateenNonZeroBundle {  // for mstateen[1|2|3]
  override val SE = RW(63).withReset(0.U).withDescription("Enable access to the next lower-privilege state-enable CSR in this slot.")
}

trait HasStateenBundle { self: CSRModule[_] =>
  val fromMstateen0 = IO(Input(new Mstateen0Bundle))
  val fromMstateen1 = IO(Input(new MstateenNonZeroBundle))
  val fromMstateen2 = IO(Input(new MstateenNonZeroBundle))
  val fromMstateen3 = IO(Input(new MstateenNonZeroBundle))
  val fromHstateen0 = IO(Input(new Hstateen0Bundle))
  val privState     = IO(Input(new PrivState))
}
