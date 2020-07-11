package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.regfile.RfReadPort
import xiangshan.utils.{XSDebug, XSInfo}

class Dispatch2 extends XSModule {
  val io = IO(new Bundle() {
    // from dispatch queues
    val fromIntDq = Flipped(Vec(IntDqDeqWidth, DecoupledIO(new MicroOp)))
    val fromFpDq = Flipped(Vec(FpDqDeqWidth, DecoupledIO(new MicroOp)))
    val fromLsDq = Flipped(Vec(LsDqDeqWidth, DecoupledIO(new MicroOp)))

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

  for (i <- 0 until IntDqDeqWidth) {
    XSDebug(io.fromIntDq(i).valid,
      p"int dp queue $i: ${Hexadecimal(io.fromIntDq(i).bits.cf.pc)} type ${Binary(io.fromIntDq(i).bits.ctrl.fuType)}\n")
  }
  for (i <- 0 until FpDqDeqWidth) {
    XSDebug(io.fromFpDq(i).valid,
      p"fp dp queue $i: ${Hexadecimal(io.fromFpDq(i).bits.cf.pc)} type ${Binary(io.fromFpDq(i).bits.ctrl.fuType)}\n")
  }
  for (i <- 0 until LsDqDeqWidth) {
    XSDebug(io.fromLsDq(i).valid,
      p"ls dp queue $i: ${Hexadecimal(io.fromLsDq(i).bits.cf.pc)} type ${Binary(io.fromLsDq(i).bits.ctrl.fuType)}\n")
  }

  // inst indexes for reservation stations
  val rsIndexGen = Module(new DispatchGen())
  rsIndexGen.io.fromIntDq := io.fromIntDq
  rsIndexGen.io.fromFpDq := io.fromFpDq
  rsIndexGen.io.fromLsDq := io.fromLsDq
  rsIndexGen.io.busyIQ := DontCare // TODO: from issue queue

  val instValid = rsIndexGen.io.enqIQIndex.map(_.valid)
  val allIndex = rsIndexGen.io.enqIQIndex.map(_.bits)

  allIndex.zipWithIndex.map({case(index, i) => XSDebug(instValid(i), p"dispatch to iq index $i: $index\n")})

  // regfile read ports
  io.readIntRf <> DontCare
  val regfileRPGen = Module(new RegfileReadPortGen())
  (0 until exuConfig.IntExuCnt).map(i => regfileRPGen.io.intIQEnqIndex(i) := rsIndexGen.io.enqIQIndex(i))
  (0 until exuConfig.FpExuCnt).map(i => regfileRPGen.io.fpIQEnqIndex(i) := rsIndexGen.io.enqIQIndex(exuConfig.IntExuCnt + i))
  (0 until exuConfig.LsExuCnt).map(i => regfileRPGen.io.lsIQEnqIndex(i) := rsIndexGen.io.enqIQIndex(exuConfig.IntExuCnt + exuConfig.FpExuCnt + i))
  for (i <- 0 until 2 * exuConfig.IntExuCnt) {
    val bits = io.fromIntDq(regfileRPGen.io.readIntRf(i)).bits
    io.readIntRf(i).addr := (if (i % 2 == 0) bits.psrc1 else bits.psrc2)
    XSDebug(p"regfile $i from ${regfileRPGen.io.readIntRf(i)}\n")
  }
  for (i <- 0 until 3*exuConfig.FpExuCnt) {
    val bits = io.fromFpDq(regfileRPGen.io.readFpRf(i)).bits
    io.readFpRf(i).addr := (if (i % 3 == 0) bits.psrc1 else if (i % 3 == 1) bits.psrc2 else bits.psrc3)
  }
  for (i <- 0 until exuConfig.LduCnt) {
    val start = 2 * exuConfig.AluCnt
    io.readIntRf(start+i).addr := io.fromLsDq(regfileRPGen.io.readIntRf(start+i))
  }
  for (i <- 0 until 2*exuConfig.StuCnt) {
    val start = 2 * exuConfig.AluCnt + exuConfig.LduCnt
    val bits = io.fromLsDq(regfileRPGen.io.readIntRf(start + i)).bits
    io.readIntRf(start + i).addr := (if (i % 2 == 0) bits.psrc1 else bits.psrc2)
  }
  for (i <- 0 until NRReadPorts) {
    XSDebug(p"regfile $i: addr ${io.readIntRf(i).addr}, state ${io.intPregRdy(i)}\n")
  }

  // TODO uncomment me when fmac > 0
  io.readFpRf <> DontCare

  // insert into reservation station
  val instIdxes = (0 until exuConfig.ExuCnt).map(i => Cat(!instValid(i), allIndex(i)))
  io.enqIQCtrl.zipWithIndex map { case (enq, i) =>
    if (i < exuConfig.IntExuCnt) {
      enq.valid := !instIdxes(i)(2) && io.fromIntDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromIntDq(instIdxes(i)(1, 0)).bits
      val startIndex = regfileRPGen.io.intIQRfSrc(i)
      enq.bits.src1State := io.intPregRdy(startIndex)
      enq.bits.src2State := io.intPregRdy(startIndex + 1.U)
    }
    else if (i < exuConfig.IntExuCnt + exuConfig.FpExuCnt) {
      val startIndex = regfileRPGen.io.fpIQRfSrc(i - exuConfig.IntExuCnt)
      enq.valid := !instIdxes(i)(2) && io.fromFpDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromFpDq(instIdxes(i)(1, 0)).bits
      enq.bits.src1State := io.fpPregRdy(startIndex)
      enq.bits.src2State := io.fpPregRdy(startIndex + 1.U)
      enq.bits.src3State := io.fpPregRdy(startIndex + 2.U)
    }
    else {
      // TODO: load store with fp
      val startIndex = regfileRPGen.io.lsIQRfSrc(i - exuConfig.IntExuCnt - exuConfig.FpExuCnt)
      enq.valid := !instIdxes(i)(2) && io.fromLsDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromLsDq(instIdxes(i)(1, 0)).bits
      if (i < exuConfig.IntExuCnt + exuConfig.FpExuCnt + exuConfig.LduCnt) {
        enq.bits.src1State := io.intPregRdy(startIndex)
      }
      else {
        enq.bits.src1State := io.intPregRdy(startIndex)
        enq.bits.src2State := io.intPregRdy(startIndex + 1.U)
      }
    }

    XSInfo(enq.fire(), "pc 0x%x with type %b srcState(%d %d %d) enters reservation station %d from %d\n",
      enq.bits.cf.pc, enq.bits.ctrl.fuType, enq.bits.src1State, enq.bits.src2State, enq.bits.src3State, i.U, instIdxes(i))
  }

  // responds to dispatch queue
  for (i <- 0 until IntDqDeqWidth) {
    io.fromIntDq(i).ready := (io.enqIQCtrl.zipWithIndex map {case (rs, j) =>
      (rs.ready && instIdxes(j) === i.U && (j < exuConfig.IntExuCnt).asBool())
    }).reduce((l, r) => l || r)
    XSInfo(io.fromIntDq(i).fire(), "pc 0x%x leaves Int dispatch queue with nroq %d\n",
      io.fromIntDq(i).bits.cf.pc, io.fromIntDq(i).bits.roqIdx)
    XSDebug(io.fromIntDq(i).valid && !io.fromIntDq(i).ready,
      "pc 0x%x waits at Int dispatch queue with index %d\n",
      io.fromIntDq(i).bits.cf.pc, i.U)
  }
  for (i <- 0 until FpDqDeqWidth) {
    io.fromFpDq(i).ready := (io.enqIQCtrl.zipWithIndex map {case (rs, j) =>
      (rs.ready && instIdxes(j) === i.U
        && (j >= exuConfig.IntExuCnt && j < exuConfig.IntExuCnt + exuConfig.FpExuCnt).asBool())
    }).reduce((l, r) => l || r)
    XSInfo(io.fromFpDq(i).fire(), "pc 0x%x leaves Fp dispatch queue with nroq %d\n",
      io.fromFpDq(i).bits.cf.pc, io.fromFpDq(i).bits.roqIdx)
    XSDebug(io.fromFpDq(i).valid && !io.fromFpDq(i).ready,
      "pc 0x%x waits at Fp dispatch queue with index %d\n",
      io.fromFpDq(i).bits.cf.pc, i.U)
  }
  for (i <- 0 until LsDqDeqWidth) {
    io.fromLsDq(i).ready := (io.enqIQCtrl.zipWithIndex map {case (rs, j) =>
      (rs.ready && instIdxes(j) === i.U
        && (j >= exuConfig.IntExuCnt + exuConfig.FpExuCnt).asBool())
    }).reduce((l, r) => l || r)
    XSInfo(io.fromLsDq(i).fire(), "pc 0x%x leaves Ls dispatch queue with nroq %d\n",
      io.fromLsDq(i).bits.cf.pc, io.fromLsDq(i).bits.roqIdx)
    XSDebug(io.fromLsDq(i).valid && !io.fromLsDq(i).ready,
      "pc 0x%x waits at Ls dispatch queue with index %d\n",
      io.fromLsDq(i).bits.cf.pc, i.U)
  }

  // TODO: store needs data from FpRegfile
  val intExuIndexReg = Reg(Vec(exuConfig.IntExuCnt, UInt(log2Ceil(NRReadPorts).W)))
  val fpExuIndexReg = Reg(Vec(exuConfig.FpExuCnt, UInt(log2Ceil(NRReadPorts).W)))
  val lsExuIndexReg = Reg(Vec(exuConfig.LduCnt + exuConfig.StuCnt, UInt(log2Ceil(NRReadPorts).W)))
  (0 until exuConfig.IntExuCnt).map(i => intExuIndexReg(i) := regfileRPGen.io.intIQRfSrc(i))
  (0 until exuConfig.FpExuCnt).map(i => fpExuIndexReg(i) := regfileRPGen.io.fpIQRfSrc(i))
  (0 until exuConfig.LsExuCnt).map(i => lsExuIndexReg(i) := regfileRPGen.io.lsIQRfSrc(i))
  // TODO: remove uop when reservation stations deal with imme
  val uop_reg = Reg(Vec(exuConfig.ExuCnt, new MicroOp))
  val data_valid = Reg(Vec(exuConfig.ExuCnt, Bool()))
  for (i <- 0 until exuConfig.ExuCnt) {
    data_valid(i) := io.enqIQCtrl(i).fire()
    uop_reg(i) := io.enqIQCtrl(i).bits
    io.enqIQData(i).valid := DontCare
    io.enqIQData(i).bits := DontCare

    val srcIndex = Wire(Vec(3, UInt(4.W)))
    if (i < exuConfig.IntExuCnt) {
      val startIndex = intExuIndexReg(i)
      io.enqIQData(i).bits.src1 := Mux(uop_reg(i).ctrl.src1Type === SrcType.pc,
        uop_reg(i).cf.pc, io.readIntRf(startIndex).data)
      io.enqIQData(i).bits.src2 := Mux(uop_reg(i).ctrl.src2Type === SrcType.imm,
        uop_reg(i).ctrl.imm, io.readIntRf(startIndex + 1.U).data)
      srcIndex(0) := startIndex
      srcIndex(1) := startIndex + 1.U
      srcIndex(2) := 0.U
    }
    else if (i < exuConfig.IntExuCnt + exuConfig.FpExuCnt) {
      val startIndex = fpExuIndexReg(i - exuConfig.IntExuCnt)
      io.enqIQData(i).bits.src1 := io.readFpRf(startIndex).data
      io.enqIQData(i).bits.src2 := io.readFpRf(startIndex + 1.U).data
      io.enqIQData(i).bits.src3 := io.readFpRf(startIndex + 2.U).data
      srcIndex(0) := startIndex
      srcIndex(1) := startIndex + 1.U
      srcIndex(2) := startIndex + 2.U
    }
    else {
      val startIndex = lsExuIndexReg(i - exuConfig.IntExuCnt - exuConfig.FpExuCnt)
      io.enqIQData(i).bits.src1 := Mux(uop_reg(i).ctrl.src1Type === SrcType.pc,
        uop_reg(i).cf.pc, io.readIntRf(startIndex).data)
      io.enqIQData(i).bits.src2 := Mux(uop_reg(i).ctrl.src2Type === SrcType.imm,
        uop_reg(i).ctrl.imm, io.readIntRf(startIndex + 1.U).data)
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
