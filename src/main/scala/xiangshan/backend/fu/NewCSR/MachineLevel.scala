package xiangshan.backend.fu.NewCSR

import chisel3._
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRFunc._

import scala.collection.immutable.SeqMap

trait MachineLevel { self: NewCSR =>
  val mstatus = Module(new MstatusModule)

  val mtvec = Module(new CSRModule("Mtvec", new CSRBundle {
      val mode = MtvecMode(1, 0, wNoFilter)
      val addr = CSRWARLField(63, 2, wNoFilter)
    }
  ) {} )

  val machineLevelCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], Data)] = SeqMap(
    0x300 -> (mstatus.w -> mstatus.rdata),
    0x305 -> (mtvec.w -> mtvec.rdata),
  )

  val machineLevelCSRMods: Seq[CSRModule[_]] = Seq(
    mstatus,
    mtvec,
  )
}

class MstatusBundle extends CSRBundle {

  val SIE  = CSRWARLField   (1, wNoFilter)
  val MIE  = CSRWARLField   (3, wNoFilter)
  val SPIE = CSRWARLField   (5, wNoFilter)
  val UBE  = CSRROField     (6, rFixValue(0.U))
  val MPIE = CSRWARLField   (7, wNoFilter)
  val SPP  = CSRWARLField   (8, wNoFilter)
  val VS   = ContextStatus  (10, 9)
  val MPP  = CSRWARLField   (12, 11, wNoFilter)
  val FS   = ContextStatus  (14, 13)
  val XS   = ContextStatusRO(16, 15, rFixValue(0.U))
  val MPRV = CSRWARLField   (17, wNoFilter)
  val SUM  = CSRWARLField   (18, wNoFilter)
  val MXR  = CSRWARLField   (19, wNoFilter)
  val TVM  = CSRWARLField   (20, wNoFilter)
  val TW   = CSRWARLField   (21, wNoFilter)
  val TSR  = CSRWARLField   (22, wNoFilter)
  val UXL  = UXLField       (33, 32, rFixValue("b10".U))
  val SXL  = SXLField       (35, 34, rFixValue("b10".U))
  val SBE  = CSRROField     (36, rFixValue(0.U))
  val MBE  = CSRROField     (37, rFixValue(0.U))
  val GVA  = CSRWARLField   (38, wNoFilter)
  val MPV  = CSRWARLField   (39, wNoFilter)
  val SD   = CSRROField     (63,
    (_, _) =>
      FS === FS.factory.asInstanceOf[ContextStatusDef].Dirty ||
        VS === VS.factory.asInstanceOf[ContextStatusDef].Dirty
  )
}

class MstatusModule extends CSRModule("MStatus", new MstatusBundle) {
  val mstatus = IO(Output(bundle))
  val sstatus = IO(Output(new SstatusBundle))

  val wAliasSstatus = IO(Input(new CSRAddrWriteBundle(new SstatusBundle)))

  // write connection
  this.wfn(reg)(Seq(wAliasSstatus))

  // read connection
  mstatus :|= reg
  sstatus := mstatus
  rdata := mstatus.asUInt

  class SstatusBundle extends CSRBundle {
    val SIE  = CSRWARLField(1, wNoFilter)
    val SPIE = CSRWARLField(5, wNoFilter)
    val SPP  = CSRWARLField(8, wNoFilter)
    val VS   = ContextStatus(10, 9)
    val FS   = ContextStatus(14, 13)
    val SUM  = CSRWARLField(18, wNoFilter)
    val MXR  = CSRWARLField(19, wNoFilter)
    val SD   = CSRROField(63, rNoFilter)
  }
}
