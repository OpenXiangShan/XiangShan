package xiangshan.backend.fu.NewCSR

import chisel3._
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRFunc._

import scala.collection.immutable.SeqMap

trait Hypervisor { self: NewCSR =>

  val hip = Module(new CSRModule("Hip", new CSRBundle {
    val VSSIP = CSRRWField( 2)
    val VSTIP = CSRROField( 6)
    val VSEIP = CSRROField(10)
    val SGEIP = CSRROField(12)
  }) {} )

  val hypervisorCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], Data)] = SeqMap(
    0x644 -> (hip.w -> hip.rdata),
  )

  val hypervisorCSRMods: Seq[CSRModule[_]] = Seq(
    hip,
  )
}
