package xiangshan.backend.fu.NewCSR

import chisel3._
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRFunc._

import scala.collection.immutable.SeqMap

trait SupervisorLevel { self: NewCSR with MachineLevel =>
  val supervisorLevelCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], Data)] = SeqMap(
    0x100 -> (mstatus.wAliasSstatus, mstatus.sstatus),
  )

  val supervisorLevelCSRMods: Seq[CSRModule[_]] = Seq(
  )
}

class SstatusBundle extends CSRBundle {
  val SIE  = CSRWARLField   (1, wNoFilter)
  val SPIE = CSRWARLField   (5, wNoFilter)
  val UBE  = CSRROField     (6)
  val SPP  = CSRWARLField   (8, wNoFilter)
  val VS   = ContextStatus  (10, 9)
  val FS   = ContextStatus  (14, 13)
  val XS   = ContextStatusRO(16, 15).withReset(0.U)
  val SUM  = CSRWARLField   (18, wNoFilter)
  val MXR  = CSRWARLField   (19, wNoFilter)
  val UXL  = XLENField      (33, 32).withReset(XLENField.XLEN64)
  val SD   = CSRROField     (63, (_, _) => FS === ContextStatus.Dirty || VS === ContextStatus.Dirty)
}