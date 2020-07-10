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

  val bruInstIdx = Cat(!instValid(0), allIndex(0))
  val aluInstIdx = (0 until exuConfig.AluCnt).map(i => Cat(!instValid(1+i), allIndex(1+i)))
  val mulInstIdx = Cat(!instValid(5), allIndex(5))
  val muldivInstIdx = Cat(!instValid(6), allIndex(6))
  val fmacInstIdx = (0 until exuConfig.FmacCnt).map(i => Cat(!instValid(7+i), allIndex(7+i)))
  val fmisc0InstIdx = 4.U//TODO: Cat(instValid(11), allIndex(11))
  val fmisc1InstIdx = 4.U// TODO: Cat(instValid(12), allIndex(12))
  val loadInstIdx = Seq(4.U, 0.U)//TODO: (0 until exuConfig.LduCnt).map(i => Cat(instValid(13+i), allIndex(13+i)))
  val storeInstIdx = Seq(Cat(!instValid(7), allIndex(7)), 4.U) //TODO: 15+i

  // regfile read ports
  val regfileRPGen = Module(new RegfileReadPortGen())
  regfileRPGen.io.enqIQIndex := rsIndexGen.io.enqIQIndex
  for (i <- 0 until 4) {
    io.readIntRf(2*i).addr := io.fromIntDq(regfileRPGen.io.readIntRf(i)).bits.psrc1
    io.readIntRf(2*i+1).addr := io.fromIntDq(regfileRPGen.io.readIntRf(i)).bits.psrc2
    XSDebug(p"regfile $i from ${regfileRPGen.io.readIntRf(i)}\n")
  }

  // FMAC, FMISC can use the 12 read ports
  // priority: FMAC > FMISC
  val fpExuIndex = WireInit(VecInit(Seq.fill(2)(0.U(2.W))))
  val fpDeqChoice = Wire(Vec(4, UInt(2.W)))
  fpDeqChoice := DontCare
  for (i <- 0 until exuConfig.FmacCnt) {
    val readPortSrc = Seq(fmacInstIdx(i), fmisc0InstIdx, fmisc1InstIdx)
    val wantReadPort = (0 until 3).map(j => (
      if (i == 0) !readPortSrc(j)(2)
      else {
        val prevMax = (0 until i).map(fpDeqChoice(_)).reduce((a, b) => Mux(a > b, a, b))
        !readPortSrc(j)(2) && (j.U > prevMax || j.U === 0.U)
      }))
    val readIdxVec = Wire(Vec(3, UInt(2.W)))
    for (j <- 0 until 3) {
      readIdxVec(j) := readPortSrc(j)(1, 0)
    }
    fpDeqChoice(i) := PriorityEncoder(wantReadPort)
    XSDebug("fp %d: want %b, deqChoice: %d\n", i.U, Cat(wantReadPort), fpDeqChoice(i))
    val target = readIdxVec(fpDeqChoice(i))
    io.readFpRf(3 * i).addr := io.fromFpDq(target).bits.psrc1
    io.readFpRf(3 * i + 1).addr := io.fromFpDq(target).bits.psrc2
    io.readFpRf(3 * i + 2).addr := io.fromFpDq(target).bits.psrc3
  }
  // fpExuIndex: which regfile read ports are assigned to FMISC0 FMISC1
  for (j <- 0 until (exuConfig.FmiscCnt + exuConfig.FmiscDivSqrtCnt)) {
    fpExuIndex(j) := PriorityEncoder((0 until 4).map(i => fpDeqChoice(i) === (j + 1).U))
  }
  XSDebug("fpExuIndex: %d %d\n", fpExuIndex(0), fpExuIndex(1))

  // TODO uncomment me when fmac > 0
  io.readFpRf <> DontCare
  io.readIntRf(2*IntDqDeqWidth).addr := io.fromLsDq(loadInstIdx(0)).bits.psrc1
  io.readIntRf(2*IntDqDeqWidth + 1).addr := io.fromLsDq(loadInstIdx(1)).bits.psrc1
  io.readIntRf(2*IntDqDeqWidth + 2).addr := io.fromLsDq(storeInstIdx(0)).bits.psrc1
  io.readIntRf(2*IntDqDeqWidth + 3).addr := io.fromLsDq(storeInstIdx(0)).bits.psrc2
  io.readIntRf(2*IntDqDeqWidth + 4).addr := io.fromLsDq(storeInstIdx(1)).bits.psrc1//TODO
  io.readIntRf(2*IntDqDeqWidth + 5).addr := io.fromLsDq(storeInstIdx(1)).bits.psrc2//TODO
  io.readFpRf(3*FpDqDeqWidth).addr := io.fromLsDq(storeInstIdx(0)).bits.psrc1
  io.readFpRf(3*FpDqDeqWidth + 1).addr := io.fromLsDq(storeInstIdx(1)).bits.psrc1//TODO

  for (i <- 0 until NRReadPorts) {
    XSDebug(p"regfile $i: addr ${io.readIntRf(i).addr}, state ${io.intPregRdy(i)}\n")
  }

  // insert into reservation station
  val instIdxes = Seq(bruInstIdx, aluInstIdx(0), aluInstIdx(1), aluInstIdx(2), aluInstIdx(3), mulInstIdx, muldivInstIdx,
    /*load0InstIdx, */storeInstIdx(0))
  io.enqIQCtrl.zipWithIndex map { case (enq, i) =>
    if (i < exuConfig.IntExuCnt) {
      enq.valid := !instIdxes(i)(2) && io.fromIntDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromIntDq(instIdxes(i)(1, 0)).bits
      val startIndex = regfileRPGen.io.intIQRfSrc(i)
      enq.bits.src1State := io.intPregRdy(startIndex)
      enq.bits.src2State := io.intPregRdy(startIndex + 1.U)
    }
    else if (i < exuConfig.IntExuCnt + exuConfig.FpExuCnt) {
      val startIndex = if (i < exuConfig.IntExuCnt + 4) (3 * (i - exuConfig.IntExuCnt)).U
        else 3.U * fpExuIndex(i - exuConfig.IntExuCnt - 4)
      enq.valid := !instIdxes(i)(2) && io.fromFpDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromFpDq(instIdxes(i)(1, 0)).bits
      enq.bits.src1State := io.fpPregRdy(startIndex)
      enq.bits.src2State := io.fpPregRdy(startIndex + 1.U)
      enq.bits.src3State := io.fpPregRdy(startIndex + 2.U)
    }
    else {
      enq.valid := !instIdxes(i)(2) && io.fromLsDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromLsDq(instIdxes(i)(1, 0)).bits
      // TODO load and store
      enq.bits.src1State := io.intPregRdy(10)
      enq.bits.src2State := io.intPregRdy(11)
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
  val fpExuIndexReg = Reg(Vec(2, UInt(2.W)))
  (0 until exuConfig.IntExuCnt).map(i => intExuIndexReg(i) := regfileRPGen.io.intIQRfSrc(i))
  (0 until 2).map(i => fpExuIndexReg(i) := fpExuIndex(i))
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
      val startIndex = if (i < exuConfig.IntExuCnt + 4) (3 * (i - exuConfig.IntExuCnt)).U
        else 3.U * fpExuIndexReg(i - exuConfig.IntExuCnt - 4)
      io.enqIQData(i).bits.src1 := io.readFpRf(startIndex).data
      io.enqIQData(i).bits.src2 := io.readFpRf(startIndex + 1.U).data
      io.enqIQData(i).bits.src3 := io.readFpRf(startIndex + 2.U).data
      srcIndex(0) := startIndex
      srcIndex(1) := startIndex + 1.U
      srcIndex(2) := startIndex + 2.U
    }
    else {
      io.enqIQData(i).bits.src1 := Mux(uop_reg(i).ctrl.src1Type === SrcType.pc,
        uop_reg(i).cf.pc, io.readIntRf(10).data)
      io.enqIQData(i).bits.src2 := Mux(uop_reg(i).ctrl.src2Type === SrcType.imm,
        uop_reg(i).ctrl.imm, io.readIntRf(11).data)
      srcIndex(0) := 10.U
      srcIndex(1) := 11.U
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
