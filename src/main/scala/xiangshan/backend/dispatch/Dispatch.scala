package xiangshan.backend.dispatch

//import org.scalatest._
//import chiseltest._
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._
import xiangshan.backend.regfile.RfReadPort

class Dispatch extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    // enq Roq
    val toRoq =  Vec(RenameWidth, DecoupledIO(new MicroOp))
    // get RoqIdx
    val roqIdxs = Input(Vec(RenameWidth, UInt(RoqIdxWidth.W)))
    // read regfile
    val readIntRf = Vec(NRReadPorts, Flipped(new RfReadPort))
    val readFpRf = Vec(NRReadPorts, Flipped(new RfReadPort))
    // read reg status (busy/ready)
    val intPregRdy = Vec(NRReadPorts, Input(Bool()))
    val fpPregRdy = Vec(NRReadPorts, Input(Bool()))
    // ro reservation stations
    val enqIQCtrl = Vec(exuConfig.ExuCnt, DecoupledIO(new MicroOp))
    val enqIQData = Vec(exuConfig.ExuCnt, ValidIO(new ExuInput))
  })
  // pipeline between rename and dispatch
  val dispatch1 = Module(new Dispatch1())
  for (i <- 0 until RenameWidth) {
    PipelineConnect(io.fromRename(i), dispatch1.io.fromRename(i), dispatch1.io.recv(i), false.B)
  }
  val intDq = Module(new DispatchQueue(new MicroOp, dp1Config.IntDqSize, RenameWidth, IntDqDeqWidth, "IntDpQ"))
  val fpDq = Module(new DispatchQueue(new MicroOp, dp1Config.FpDqSize, RenameWidth, FpDqDeqWidth, "FpDpQ"))
  val lsDq = Module(new DispatchQueue(new MicroOp, dp1Config.LsDqSize, RenameWidth, LsDqDeqWidth, "LsDpQ"))
  val dispatch2 = Module(new Dispatch2())

  dispatch1.io.redirect <> io.redirect
  dispatch1.io.toRoq <> io.toRoq
  dispatch1.io.roqIdxs <> io.roqIdxs
  dispatch1.io.toIntDq <> intDq.io.enq
  dispatch1.io.toFpDq <> fpDq.io.enq
  dispatch1.io.toLsDq <> lsDq.io.enq

  dispatch2.io.fromIntDq <> intDq.io.deq
  dispatch2.io.fromFpDq <> fpDq.io.deq
  dispatch2.io.fromLsDq <> lsDq.io.deq
  dispatch2.io.readIntRf <> io.readIntRf
  dispatch2.io.readFpRf <> io.readFpRf
  dispatch2.io.intPregRdy <> io.intPregRdy
  dispatch2.io.fpPregRdy <> io.fpPregRdy
  dispatch2.io.enqIQCtrl <> io.enqIQCtrl
  dispatch2.io.enqIQData <> io.enqIQData
}

class DispatchWrapper extends XSModule {
  val io = IO(new Bundle() {
//    val redirect = Flipped(ValidIO(new Redirect))
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    // enq Roq
    val toRoq =  Vec(RenameWidth, DecoupledIO(new MicroOp))
    // get RoqIdx
    val roqIdxs = Input(Vec(RenameWidth, UInt(RoqIdxWidth.W)))
    // read regfile
    val readIntRf = Vec(NRReadPorts, Flipped(new RfReadPort))
    val readFpRf = Vec(NRReadPorts, Flipped(new RfReadPort))
    // read reg status (busy/ready)
    val intPregRdy = Vec(NRReadPorts, Input(Bool()))
    val fpPregRdy = Vec(NRReadPorts, Input(Bool()))
    // ro reservation stations
    val enqIQCtrl = Vec(exuConfig.ExuCnt, DecoupledIO(new MicroOp))
    val enqIQData = Vec(exuConfig.ExuCnt, ValidIO(new ExuInput))
  })
  io <> DontCare

  val dispatch = Module(new Dispatch())
  dispatch.io <> DontCare

  for (i <- 0 until RenameWidth) {
    dispatch.io.fromRename(i).valid := io.fromRename(i).valid
    dispatch.io.fromRename(i).bits.cf.pc := io.fromRename(i).bits.cf.pc
    dispatch.io.fromRename(i).bits.psrc1 := io.fromRename(i).bits.psrc1
    dispatch.io.fromRename(i).bits.psrc2 := io.fromRename(i).bits.psrc2
    dispatch.io.fromRename(i).bits.psrc3 := io.fromRename(i).bits.psrc3
    dispatch.io.fromRename(i).bits.ctrl.fuType := io.fromRename(i).bits.ctrl.fuType
    io.fromRename(i).ready := dispatch.io.fromRename(i).ready

    dispatch.io.toRoq(i).ready := io.toRoq(i).ready
    io.toRoq(i).valid := dispatch.io.toRoq(i).valid
    io.toRoq(i).bits.cf.pc := dispatch.io.toRoq(i).bits.cf.pc

    dispatch.io.roqIdxs(i) := io.roqIdxs(i)
  }

  for (i <- 0 until NRReadPorts) {
    dispatch.io.readIntRf(i).data := io.readIntRf(i).data
    io.readIntRf(i).addr := dispatch.io.readIntRf(i).addr
    dispatch.io.readFpRf(i).data := io.readFpRf(i).data
    io.readFpRf(i).addr := dispatch.io.readFpRf(i).addr
    dispatch.io.intPregRdy(i) := io.intPregRdy(i)
    dispatch.io.fpPregRdy(i) := io.fpPregRdy(i)
  }

  for (i <- 0 until exuConfig.ExuCnt) {
    io.enqIQCtrl(i).valid := dispatch.io.enqIQCtrl(i).valid
    dispatch.io.enqIQCtrl(i).ready := io.enqIQCtrl(i).ready
    io.enqIQCtrl(i).bits.cf.pc := dispatch.io.enqIQCtrl(i).bits.cf.pc
    io.enqIQCtrl(i).bits.psrc1 := dispatch.io.enqIQCtrl(i).bits.psrc1
    io.enqIQCtrl(i).bits.psrc2 := dispatch.io.enqIQCtrl(i).bits.psrc2
    io.enqIQCtrl(i).bits.psrc3 := dispatch.io.enqIQCtrl(i).bits.psrc3
    io.enqIQCtrl(i).bits.roqIdx := dispatch.io.enqIQCtrl(i).bits.roqIdx

    io.enqIQData(i).valid := dispatch.io.enqIQData(i).valid
    io.enqIQData(i).bits.uop.cf.pc := dispatch.io.enqIQData(i).bits.uop.cf.pc
    io.enqIQData(i).bits.uop.psrc1 := dispatch.io.enqIQData(i).bits.uop.psrc1
    io.enqIQData(i).bits.uop.psrc2 := dispatch.io.enqIQData(i).bits.uop.psrc2
    io.enqIQData(i).bits.uop.psrc3 := dispatch.io.enqIQData(i).bits.uop.psrc3
    io.enqIQData(i).bits.uop.roqIdx := dispatch.io.enqIQData(i).bits.uop.roqIdx
    io.enqIQData(i).bits.src1 := dispatch.io.enqIQData(i).bits.src1
    io.enqIQData(i).bits.src2 := dispatch.io.enqIQData(i).bits.src2
    io.enqIQData(i).bits.src3 := dispatch.io.enqIQData(i).bits.src3
  }
}

object DispatchTest extends App {
  Driver.execute(args, () => new DispatchWrapper())
}

//class DispatchTest extends FlapSpec with ChiselScalatestTester with Matchers {
//
//}

