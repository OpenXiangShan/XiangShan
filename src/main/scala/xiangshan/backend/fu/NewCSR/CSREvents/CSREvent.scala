package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import utility.{SignExt, ZeroExt}
import xiangshan.ExceptionNO
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, OneFieldBundle, PrivState}
import xiangshan.backend.fu.NewCSR.CSRConfig._
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.util.CSRConst

trait CSREvents { self: NewCSR with MachineLevel =>
  val trapEntryMEvent = Module(new TrapEntryMEventModule)

  val events = Seq(trapEntryMEvent)

  events.foreach(x => dontTouch(x.out))
}

trait EventUpdatePrivStateOutput {
  val privState = ValidIO(new PrivState)
}
