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
  ))

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

  val SIE  = CSRRWField     (1).withReset(0.U)
  val MIE  = CSRRWField     (3).withReset(0.U)
  val SPIE = CSRRWField     (5).withReset(0.U)
  val UBE  = CSRROField     (6).withReset(0.U)
  val MPIE = CSRRWField     (7).withReset(0.U)
  val SPP  = CSRRWField     (8).withReset(0.U)
  val VS   = ContextStatus  (10,  9).withReset(ContextStatus.Initial)
  val MPP  = PrivMode       (12, 11).withReset(PrivMode.U)
  val FS   = ContextStatus  (14, 13).withReset(ContextStatus.Initial)
  val XS   = ContextStatusRO(16, 15).withReset(0.U)
  val MPRV = CSRRWField     (17).withReset(0.U)
  val SUM  = CSRRWField     (18).withReset(0.U)
  val MXR  = CSRRWField     (19).withReset(0.U)
  val TVM  = CSRRWField     (20).withReset(0.U)
  val TW   = CSRRWField     (21).withReset(0.U)
  val TSR  = CSRRWField     (22).withReset(0.U)
  val UXL  = XLENField      (33, 32).withReset(XLENField.XLEN64)
  val SXL  = XLENField      (35, 34).withReset(XLENField.XLEN64)
  val SBE  = CSRROField     (36).withReset(0.U)
  val MBE  = CSRROField     (37).withReset(0.U)
  val GVA  = CSRRWField     (38).withReset(0.U)
  val MPV  = CSRRWField     (39).withReset(0.U)
  val SD   = CSRROField     (63,
    (_, _) => FS === ContextStatus.Dirty || VS === ContextStatus.Dirty
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
