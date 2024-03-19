package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.NewCSR.CSRDefines._

trait Unprivileged { self: NewCSR =>
  val fcsr = Module(new CSRModule("Fcsr", new CSRBundle {
    val NX = CSRFieldWARLBits(0, wNoFilter)
    val UF = CSRFieldWARLBits(1, wNoFilter)
    val OF = CSRFieldWARLBits(2, wNoFilter)
    val DZ = CSRFieldWARLBits(3, wNoFilter)
    val NV = CSRFieldWARLBits(4, wNoFilter)
    val FRM = CSRFieldWARLBits(7, 5, wNoFilter)
  }) {
    val wAliasFflags = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val NX = CSRFieldWARLBits(0, wNoFilter)
      val UF = CSRFieldWARLBits(1, wNoFilter)
      val OF = CSRFieldWARLBits(2, wNoFilter)
      val DZ = CSRFieldWARLBits(3, wNoFilter)
      val NV = CSRFieldWARLBits(4, wNoFilter)
    })))
    val wAliasFfm = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val FRM = CSRFieldWARLBits(2, 0, wNoFilter)
    })))
    val fflags = IO(Output(UInt(64.W)))
    val frm = IO(Output(UInt(64.W)))

    // write connection
    this.wfn(reg)(Seq(wAliasFflags, wAliasFfm))

    // read connection
    fflags := reg.asUInt(4, 0)
    frm := reg.FRM.asUInt
  })
}
