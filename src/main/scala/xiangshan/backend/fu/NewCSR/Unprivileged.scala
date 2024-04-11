package xiangshan.backend.fu.NewCSR

import chisel3._
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRFunc._

import scala.collection.immutable.SeqMap

trait Unprivileged { self: NewCSR with MachineLevel with SupervisorLevel =>

  val fcsr = Module(new CSRModule("Fcsr", new CSRBundle {
    val NX = CSRWARLField(0, wNoFilter)
    val UF = CSRWARLField(1, wNoFilter)
    val OF = CSRWARLField(2, wNoFilter)
    val DZ = CSRWARLField(3, wNoFilter)
    val NV = CSRWARLField(4, wNoFilter)
    val FRM = CSRWARLField(7, 5, wNoFilter)
  }) {
    val wAliasFflags = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val NX = CSRWARLField(0, wNoFilter)
      val UF = CSRWARLField(1, wNoFilter)
      val OF = CSRWARLField(2, wNoFilter)
      val DZ = CSRWARLField(3, wNoFilter)
      val NV = CSRWARLField(4, wNoFilter)
    })))
    val wAliasFfm = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val FRM = CSRWARLField(2, 0, wNoFilter)
    })))
    val fflags = IO(Output(UInt(64.W)))
    val frm = IO(Output(UInt(64.W)))

    // write connection
    this.wfn(reg)(Seq(wAliasFflags, wAliasFfm))

    // read connection
    fflags := reg.asUInt(4, 0)
    frm := reg.FRM.asUInt
  })

  val unprivilegedCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], Data)] = SeqMap(
    0x001 -> (fcsr.wAliasFflags -> fcsr.fflags),
    0x002 -> (fcsr.wAliasFfm -> fcsr.frm),
    0x003 -> (fcsr.w -> fcsr.rdata),
  )

  val unprivilegedCSRMods: Seq[CSRModule[_]] = Seq(
    fcsr,
  )

  val unprivilegedCSROutMap: SeqMap[Int, UInt] = SeqMap(
    0x001 -> fcsr.fflags.asUInt,
    0x002 -> fcsr.frm.asUInt,
    0x003 -> fcsr.rdata.asUInt,
  )
}
