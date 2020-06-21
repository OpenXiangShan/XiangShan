package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.regfile.RfReadPort

class Dispatch2 extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    // from dispatch1
    val in = Flipped(new Dp1ToDp2IO)

    // read regfile
    val readIntRf = Vec(NRReadPorts, Flipped(new RfReadPort))
    val readFpRf = Vec(NRReadPorts, Flipped(new RfReadPort))
    // read reg status (busy/ready)
    val intPregRdy = Vec(NRReadPorts, Input(Bool()))
    val fpPregRdy = Vec(NRReadPorts, Input(Bool()))

    // enq Issue Queue
    val enqIQCtrl = Vec(exuConfig.ExuCnt, DecoupledIO(new MicroOp))
    val enqIQData = Vec(exuConfig.ExuCnt, ValidIO(new ExuInput))
  })
}
