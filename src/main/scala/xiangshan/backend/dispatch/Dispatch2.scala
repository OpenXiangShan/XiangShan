package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.exu.ExuConfig
import xiangshan.backend.regfile.RfReadPort
import utils.{XSDebug, XSInfo}

class Dispatch2(exuCfg: Array[ExuConfig]) extends XSModule{
  val io = IO(new Bundle() {
    // from dispatch queues
    val fromIntDq = Flipped(Vec(dpParams.IntDqDeqWidth, DecoupledIO(new MicroOp)))
    val fromFpDq = Flipped(Vec(dpParams.FpDqDeqWidth, DecoupledIO(new MicroOp)))
    val fromLsDq = Flipped(Vec(dpParams.LsDqDeqWidth, DecoupledIO(new MicroOp)))
    // read regfile
    val readIntRf = Vec(NRIntReadPorts, Flipped(new RfReadPort))
    val readFpRf = Vec(NRFpReadPorts - exuParameters.StuCnt, Flipped(new RfReadPort))
    // read reg status (busy/ready)
    val intPregRdy = Vec(NRIntReadPorts, Input(Bool()))
    val fpPregRdy = Vec(NRFpReadPorts - exuParameters.StuCnt, Input(Bool()))
    // load + store reg status (busy/ready)
    val intMemRegAddr = Vec(NRMemReadPorts, Output(UInt(PhyRegIdxWidth.W)))
    val fpMemRegAddr = Vec(exuParameters.StuCnt, Output(UInt(PhyRegIdxWidth.W)))
    val intMemRegRdy = Vec(NRMemReadPorts, Input(Bool()))
    val fpMemRegRdy = Vec(exuParameters.StuCnt, Input(Bool()))
    // to reservation stations
    val numExist = Input(Vec(exuParameters.ExuCnt, UInt(log2Ceil(IssQueSize).W)))
    val enqIQCtrl = Vec(exuParameters.ExuCnt, DecoupledIO(new MicroOp))
    val enqIQData = Vec(exuParameters.ExuCnt - exuParameters.LsExuCnt, ValidIO(new ExuInput))
  })

  for (i <- 0 until dpParams.IntDqDeqWidth) {
    XSDebug(io.fromIntDq(i).valid,
      p"int dp queue $i: ${Hexadecimal(io.fromIntDq(i).bits.cf.pc)} type ${Binary(io.fromIntDq(i).bits.ctrl.fuType)}\n")
  }
  for (i <- 0 until dpParams.FpDqDeqWidth) {
    XSDebug(io.fromFpDq(i).valid,
      p"fp dp queue $i: ${Hexadecimal(io.fromFpDq(i).bits.cf.pc)} type ${Binary(io.fromFpDq(i).bits.ctrl.fuType)}\n")
  }
  for (i <- 0 until dpParams.LsDqDeqWidth) {
    XSDebug(io.fromLsDq(i).valid,
      p"ls dp queue $i: ${Hexadecimal(io.fromLsDq(i).bits.cf.pc)} type ${Binary(io.fromLsDq(i).bits.ctrl.fuType)}\n")
  }

  // inst indexes for reservation stations
  val rsIndexGen = Module(new DispatchGen(exuCfg))
  rsIndexGen.io.fromIntDq := io.fromIntDq
  rsIndexGen.io.fromFpDq := io.fromFpDq
  rsIndexGen.io.fromLsDq := io.fromLsDq
  rsIndexGen.io.numExist := io.numExist

  val instValid = rsIndexGen.io.enqIQIndex.map(_.valid)
  val allIndex = rsIndexGen.io.enqIQIndex.map(_.bits)

  allIndex.zipWithIndex.map({case(index, i) => XSDebug(instValid(i), p"dispatch to iq index $i: $index\n")})

  // regfile read ports
  io.readIntRf <> DontCare
  io.readFpRf <> DontCare
  val regfileRPGen = Module(new RegfileReadPortGen())
  (0 until exuParameters.IntExuCnt).map(i => regfileRPGen.io.intIQEnqIndex(i) := rsIndexGen.io.enqIQIndex(i))
  (0 until exuParameters.FpExuCnt).map(i => regfileRPGen.io.fpIQEnqIndex(i) := rsIndexGen.io.enqIQIndex(exuParameters.IntExuCnt + i))
  (0 until exuParameters.LsExuCnt).map(i => regfileRPGen.io.lsIQEnqIndex(i) := rsIndexGen.io.enqIQIndex(exuParameters.IntExuCnt + exuParameters.FpExuCnt + i))
  for (i <- 0 until 2 * exuParameters.AluCnt) {
    val bits = io.fromIntDq(regfileRPGen.io.readIntRf(i)).bits
    io.readIntRf(i).addr := (if (i % 2 == 0) bits.psrc1 else bits.psrc2)
    XSDebug(p"regfile $i from ${regfileRPGen.io.readIntRf(i)}\n")
  }
  for (i <- 0 until 3*exuParameters.FmacCnt) {
    val bits = io.fromFpDq(regfileRPGen.io.readFpRf(i)).bits
    io.readFpRf(i).addr := (if (i % 3 == 0) bits.psrc1 else if (i % 3 == 1) bits.psrc2 else bits.psrc3)
  }
//  assert(exuParameters.LsExuCnt == 1)
//  io.readIntRf(8).addr := io.fromLsDq(regfileRPGen.io.readIntRf(8)).bits.psrc1
//  io.readIntRf(9).addr := io.fromLsDq(regfileRPGen.io.readIntRf(9)).bits.psrc2
//  io.readFpRf(12).addr := io.fromLsDq(regfileRPGen.io.readFpRf(12)).bits.psrc2
//  for (i <- 0 until exuParameters.LduCnt) {
//    val start = 2 * exuParameters.AluCnt
//    io.readIntRf(start+i).addr := io.fromLsDq(regfileRPGen.io.readIntRf(start+i))
//  }
//  for (i <- 0 until 2*exuParameters.StuCnt) {
//    val start = 2 * exuParameters.AluCnt + exuParameters.LduCnt
//    val bits = io.fromLsDq(regfileRPGen.io.readIntRf(start + i)).bits
////    io.readIntRf(start + i).addr := (if (i % 2 == 0) bits.psrc1 else bits.psrc2)
//    if (i % 2 == 0) {
//      io.readIntRf(start + i).addr := bits.psrc1
//    }
//    else {
//      io.readIntRf(start + i).addr := bits.psrc2
//      io.readFpRf(2*exuParameters.FpExuCnt + (i / 2)) := bits.psrc2
//    }
//  }
  for (i <- 0 until NRIntReadPorts) {
    XSDebug(p"int regfile $i: addr ${io.readIntRf(i).addr}, state ${io.intPregRdy(i)}\n")
    XSDebug(p"fp regfile $i: addr ${io.readFpRf(i).addr}, state ${io.fpPregRdy(i)}\n")
  }

  // insert into reservation station
  val instIdxes = (0 until exuParameters.ExuCnt).map(i => Cat(!instValid(i), allIndex(i)))
  io.enqIQCtrl.zipWithIndex map { case (enq, i) =>
    if (i < exuParameters.IntExuCnt) {
      enq.valid := !instIdxes(i)(2) && io.fromIntDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromIntDq(instIdxes(i)(1, 0)).bits
      val startIndex = regfileRPGen.io.intIQRfSrc(i)
      enq.bits.src1State := io.intPregRdy(startIndex)
      enq.bits.src2State := io.intPregRdy(startIndex + 1.U)
    }
    else if (i < exuParameters.IntExuCnt + exuParameters.FpExuCnt) {
      val startIndex = regfileRPGen.io.fpIQRfSrc(i - exuParameters.IntExuCnt)
      enq.valid := !instIdxes(i)(2) && io.fromFpDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromFpDq(instIdxes(i)(1, 0)).bits
      enq.bits.src1State := io.fpPregRdy(startIndex)
      enq.bits.src2State := io.fpPregRdy(startIndex + 1.U)
      enq.bits.src3State := io.fpPregRdy(startIndex + 2.U)
    }
    else {
      val startIndex = 8.U// regfileRPGen.io.lsIQRfSrc(0)
      enq.valid := !instIdxes(i)(2) && io.fromLsDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromLsDq(instIdxes(i)(1, 0)).bits
//      if (i < exuParameters.IntExuCnt + exuParameters.FpExuCnt + exuParameters.LduCnt) {
//        enq.bits.src1State := io.intPregRdy(startIndex)
//      }
//      else {
      enq.bits.src1State := io.intPregRdy(startIndex)
      enq.bits.src2State := Mux(enq.bits.ctrl.src2Type === SrcType.fp,
        io.fpPregRdy(12),
        io.intPregRdy(startIndex + 1.U))
//      }
    }

    XSInfo(enq.fire(), "pc 0x%x with type %b srcState(%d %d %d) enters reservation station %d from %d\n",
      enq.bits.cf.pc, enq.bits.ctrl.fuType, enq.bits.src1State, enq.bits.src2State, enq.bits.src3State, i.U, instIdxes(i))
  }

  // responds to dispatch queue
  for (i <- 0 until dpParams.IntDqDeqWidth) {
    io.fromIntDq(i).ready := (io.enqIQCtrl.zipWithIndex map {case (rs, j) =>
      (rs.ready && instIdxes(j) === i.U && (j < exuParameters.IntExuCnt).asBool())
    }).reduce((l, r) => l || r)
    XSInfo(io.fromIntDq(i).fire(), "pc 0x%x leaves Int dispatch queue with nroq %d\n",
      io.fromIntDq(i).bits.cf.pc, io.fromIntDq(i).bits.roqIdx)
    XSDebug(io.fromIntDq(i).valid && !io.fromIntDq(i).ready,
      "pc 0x%x waits at Int dispatch queue with index %d\n",
      io.fromIntDq(i).bits.cf.pc, i.U)
  }
  for (i <- 0 until dpParams.FpDqDeqWidth) {
    io.fromFpDq(i).ready := (io.enqIQCtrl.zipWithIndex map {case (rs, j) =>
      (rs.ready && instIdxes(j) === i.U
        && (j >= exuParameters.IntExuCnt && j < exuParameters.IntExuCnt + exuParameters.FpExuCnt).asBool())
    }).reduce((l, r) => l || r)
    XSInfo(io.fromFpDq(i).fire(), "pc 0x%x leaves Fp dispatch queue with nroq %d\n",
      io.fromFpDq(i).bits.cf.pc, io.fromFpDq(i).bits.roqIdx)
    XSDebug(io.fromFpDq(i).valid && !io.fromFpDq(i).ready,
      "pc 0x%x waits at Fp dispatch queue with index %d\n",
      io.fromFpDq(i).bits.cf.pc, i.U)
  }
  for (i <- 0 until dpParams.LsDqDeqWidth) {
    io.fromLsDq(i).ready := (io.enqIQCtrl.zipWithIndex map {case (rs, j) =>
      (rs.ready && instIdxes(j) === i.U
        && (j >= exuParameters.IntExuCnt + exuParameters.FpExuCnt).asBool())
    }).reduce((l, r) => l || r)
    XSInfo(io.fromLsDq(i).fire(), "pc 0x%x leaves Ls dispatch queue with nroq %d\n",
      io.fromLsDq(i).bits.cf.pc, io.fromLsDq(i).bits.roqIdx)
    XSDebug(io.fromLsDq(i).valid && !io.fromLsDq(i).ready,
      "pc 0x%x waits at Ls dispatch queue with index %d\n",
      io.fromLsDq(i).bits.cf.pc, i.U)
  }

  val intExuIndexReg = Reg(Vec(exuParameters.IntExuCnt, UInt(log2Ceil(NRIntReadPorts).W)))
  val fpExuIndexReg = Reg(Vec(exuParameters.FpExuCnt, UInt(log2Ceil(NRFpReadPorts).W)))
  val lsExuIndexReg = Reg(Vec(exuParameters.LduCnt + exuParameters.StuCnt, UInt(log2Ceil(NRIntReadPorts).W)))
  (0 until exuParameters.IntExuCnt).map(i => intExuIndexReg(i) := regfileRPGen.io.intIQRfSrc(i))
  (0 until exuParameters.FpExuCnt).map(i => fpExuIndexReg(i) := regfileRPGen.io.fpIQRfSrc(i))
  (0 until exuParameters.LsExuCnt).map(i => lsExuIndexReg(i) := Mux(io.enqIQCtrl(exuParameters.IntExuCnt + exuParameters.FpExuCnt).bits.ctrl.src2Type === SrcType.fp,
    regfileRPGen.io.lsIQRfSrc(1), regfileRPGen.io.lsIQRfSrc(0)))
  // TODO: remove uop when reservation stations deal with imme
  val uop_reg = Reg(Vec(exuParameters.ExuCnt, new MicroOp))
  val data_valid = Reg(Vec(exuParameters.ExuCnt, Bool()))
  for (i <- 0 until exuParameters.ExuCnt) {
    data_valid(i) := io.enqIQCtrl(i).fire()
    uop_reg(i) := io.enqIQCtrl(i).bits
    io.enqIQData(i).valid := DontCare
    io.enqIQData(i).bits := DontCare

    val srcIndex = Wire(Vec(3, UInt(4.W)))
    if (i < exuParameters.IntExuCnt) {
      val startIndex = intExuIndexReg(i)
      io.enqIQData(i).bits.src1 := Mux(uop_reg(i).ctrl.src1Type === SrcType.pc,
        uop_reg(i).cf.pc, io.readIntRf(startIndex).data)
      io.enqIQData(i).bits.src2 := Mux(uop_reg(i).ctrl.src2Type === SrcType.imm,
        uop_reg(i).ctrl.imm, io.readIntRf(startIndex + 1.U).data)
      srcIndex(0) := startIndex
      srcIndex(1) := startIndex + 1.U
      srcIndex(2) := 0.U
    }
    else if (i < exuParameters.IntExuCnt + exuParameters.FpExuCnt) {
      val startIndex = fpExuIndexReg(i - exuParameters.IntExuCnt)
      io.enqIQData(i).bits.src1 := io.readFpRf(startIndex).data
      io.enqIQData(i).bits.src2 := io.readFpRf(startIndex + 1.U).data
      io.enqIQData(i).bits.src3 := io.readFpRf(startIndex + 2.U).data
      srcIndex(0) := startIndex
      srcIndex(1) := startIndex + 1.U
      srcIndex(2) := startIndex + 2.U
    }
    else {
      val startIndex = lsExuIndexReg(i - exuParameters.IntExuCnt - exuParameters.FpExuCnt)
      io.enqIQData(i).bits.src1 := Mux(uop_reg(i).ctrl.src1Type === SrcType.pc,
        uop_reg(i).cf.pc, io.readIntRf(8).data)
      io.enqIQData(i).bits.src2 := Mux(uop_reg(i).ctrl.src2Type === SrcType.imm,
        uop_reg(i).ctrl.imm,
        Mux(uop_reg(i).ctrl.src2Type === SrcType.fp,
          io.readFpRf(12).data,
          io.readIntRf(9).data))
      srcIndex(0) := startIndex
      srcIndex(1) := startIndex + 1.U
      srcIndex(2) := 0.U
    }

    XSDebug(data_valid(i),
      "pc 0x%x reads operands from (%d, %d, %x), (%d, %d, %x), (%d, %d, %x)\n",
      uop_reg(i).cf.pc,
      srcIndex(0), uop_reg(i).psrc1, io.enqIQData(i).bits.src1,
      srcIndex(1), uop_reg(i).psrc2, io.enqIQData(i).bits.src2,
      srcIndex(2), uop_reg(i).psrc3, io.enqIQData(i).bits.src3)
  }
}
