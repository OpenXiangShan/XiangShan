package xiangshan.backend.fu.NewCSR

import chisel3._

import scala.collection.immutable.SeqMap

trait SupervisorLevel { self: NewCSR with MachineLevel =>
  val supervisorLevelCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], Data)] = SeqMap(
    0x100 -> (mstatus.wAliasSstatus, mstatus.sstatus),
  )

  val supervisorLevelCSRMods: Seq[CSRModule[_]] = Seq(
  )
}
