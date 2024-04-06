package xiangshan.backend.fu.NewCSR

import xiangshan.backend.fu.NewCSR.CSRDefines.{
  XtvecMode,
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
}
