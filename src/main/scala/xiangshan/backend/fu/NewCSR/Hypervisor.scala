package xiangshan.backend.fu.NewCSR

import chisel3._
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRFunc._

import scala.collection.immutable.SeqMap

trait Hypervisor { self: NewCSR with MachineLevel =>

  val hip = Module(new CSRModule("Hip", new CSRBundle {
    val VSSIP = CSRWARLField( 2, wNoFilter)
    val VSTIP = CSRWARLField( 6, wNoEffect)
    val VSEIP = CSRWARLField(10, wNoEffect)
    val SGEIP = CSRWARLField(12, wNoEffect)
  }) {} )

  val hypervisorCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], Data)] = SeqMap(
    0x644 -> (hip.w -> hip.rdata),
  )

  val hypervisorCSRMods: Seq[CSRModule[_]] = Seq(
    hip,
  )
}
