package xiangshan.backend.fu.NewCSR

import chisel3._
import xiangshan.backend.fu.NewCSR.CSRDefines.{
  XtvecMode,
  CSRROField => RO,
  CSRRWField => RW,
  CSRWARLField => WARL,
}
import xiangshan.backend.fu.NewCSR.CSRFunc._

object CSRBundles {
  class XtvecBundle extends CSRBundle {
    val mode = XtvecMode(1, 0, wNoFilter)
    val addr = WARL(63, 2, wNoFilter)
  }

  class CauseBundle extends CSRBundle {
    val Interrupt = RW(63)
    val ExceptionCode = RW(62, 0)
  }

  class Counteren extends CSRBundle {
    val CY = RW(0)
    val TM = RW(1)
    val IR = RW(2)
    val HPM = RW(31, 3)
  }

  class OneFieldBundle extends CSRBundle {
    val ALL = RW(63, 0)
  }

  class Envcfg extends CSRBundle {
    val STCE  = RO(    63).withReset(0.U)
    val PBMTE = RO(    62).withReset(0.U)
    val ADUE  = RO(    61).withReset(0.U)
    val PMM   = RO(33, 32).withReset(0.U)
    val CBZE  = RO(     7).withReset(0.U)
    val CBCFE = RO(     6).withReset(0.U)
    val CBIE  = RO( 5,  4).withReset(0.U)
    val FIOM  = RO(     0).withReset(0.U)
  }
}
