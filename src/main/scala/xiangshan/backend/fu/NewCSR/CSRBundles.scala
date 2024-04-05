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

  class OneFieldBundle extends CSRBundle {
    val ALL = RW(63, 0)
  }
}
