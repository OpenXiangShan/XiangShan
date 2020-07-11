package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.regfile.RfReadPort
import xiangshan.utils.{XSDebug, XSInfo}

class Dispatch2 extends XSModule{
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
    val enqIQCtrl = Vec(exuParameters.ExuCnt, DecoupledIO(new MicroOp))
    val enqIQData = Vec(exuParameters.ExuCnt, ValidIO(new ExuInput))
  })
  // inst indexes for reservation stations
  // append a true.B to avoid PriorityEncode(0000) -> 3
  // if find a target uop, index[2] == 0, else index[2] == 1
  val bruInstIdx = PriorityEncoder(
    io.fromIntDq.map(
      deq => deq.bits.ctrl.fuType === FuType.jmp && deq.valid
    ) :+ true.B
  )
  val aluInstIdxsTry = RegInit(VecInit(Seq.tabulate(exuParameters.AluCnt)(i => i.U(2.W))))
  val aluInstIdxs = Wire(Vec(exuParameters.AluCnt, UInt(3.W)))
  for (i <- 0 until exuParameters.AluCnt) {
    assert(exuParameters.AluCnt == IntDqDeqWidth)
    // 0 -> 1 -> 2 -> 3 -> 0
    aluInstIdxsTry(i) := aluInstIdxsTry(i) + 1.U
    val deqAluInvalid =
      io.fromIntDq(aluInstIdxsTry(i)).bits.ctrl.fuType =/= FuType.alu ||
        !io.fromIntDq(aluInstIdxsTry(i)).valid
    aluInstIdxs(i) := Cat(deqAluInvalid.asUInt(), aluInstIdxsTry(i))
  }
  val mulInstIdx = PriorityEncoder(
    io.fromIntDq.map(
      deq => deq.bits.ctrl.fuType === FuType.mul && deq.valid
    ) :+ true.B
  )
  val muldivInstIdx = PriorityEncoder((io.fromIntDq.zipWithIndex map { case (deq, i) =>
    (
      (
        deq.bits.ctrl.fuType === FuType.mul && i.U > mulInstIdx) ||
        deq.bits.ctrl.fuType === FuType.div) && deq.valid
  }) :+ true.B)

  // TODO uncomment when FmacCnt > 0
  val fmacInstIdxsTry = Reg(Vec(exuParameters.FmacCnt, UInt(2.W)))
//  val fmacInstIdxsTry = RegInit(VecInit(Seq.tabulate(exuConfig.FmacCnt)(i => i.U(2.W))))
  val fmacInstIdxs = Wire(Vec(exuParameters.FmacCnt, UInt(3.W)))
//  val deqFmacInstIdxs = RegInit(VecInit(Seq.tabulate(exuConfig.FmacCnt)(i => i.U(2.W))))
  for (i <- 0 until exuParameters.FmacCnt) {
    assert(exuParameters.FmacCnt == FpDqDeqWidth)
    // 0 -> 1 -> 2 -> 3 -> 0
    fmacInstIdxsTry(i) := fmacInstIdxsTry(i) + 1.U
    val deqFmacInvalid = io.fromFpDq(fmacInstIdxsTry(i)).bits.ctrl.fuType =/= FuType.fmac || !io.fromFpDq(fmacInstIdxsTry(i)).valid
    fmacInstIdxs(i) := Cat(deqFmacInvalid.asUInt(), fmacInstIdxsTry(i))
  }
  val fmisc0InstIdx = PriorityEncoder(
    io.fromFpDq.map(
      deq => deq.bits.ctrl.fuType === FuType.fmisc && deq.valid
    ) :+ true.B
  )
  val fmisc1InstIdx = PriorityEncoder(
    (
      io.fromFpDq.zipWithIndex map { case (deq, i) =>
        ((deq.bits.ctrl.fuType === FuType.fmisc && i.U > fmisc0InstIdx) ||
          deq.bits.ctrl.fuType === FuType.fDivSqrt) && deq.valid
  }) :+ true.B)

  // TODO: currently there's only one load/store reservation station
  val load0InstIdx = PriorityEncoder(io.fromLsDq.map(_.bits.ctrl.fuType === FuType.ldu) :+ true.B)
  val load1InstIdx = PriorityEncoder((io.fromLsDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.ldu && i.U > load0InstIdx
  }) :+ true.B)
//  val store0InstIdx = PriorityEncoder(io.fromLsDq.map(_.bits.ctrl.fuType === FuType.stu) :+ true.B)
  val store0InstIdx = PriorityEncoder(
    io.fromLsDq.map(deq => FuType.isMemExu(deq.bits.ctrl.fuType)) :+ true.B
  )
  val store1InstIdx = PriorityEncoder((io.fromLsDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.stu && i.U > store0InstIdx
  }) :+ true.B)

  XSDebug("%d %d %d %d %d %d %d %d\n", bruInstIdx, aluInstIdxs(0), aluInstIdxs(1), aluInstIdxs(2),
    aluInstIdxs(3), mulInstIdx, muldivInstIdx, store0InstIdx)

  // regfile read ports
  // regfile is sync-read, data can used at the next cycle
  // BRU, MUL0, MUL1 can use the 8 read ports
  // priority: ALU > BRU > MUL
  val intExuIndex = WireInit(VecInit(Seq.fill(3)(0.U(2.W))))
  val intDeqChoice = Wire(Vec(4, UInt(2.W)))
  for (i <- 0 until 4) {
    val readPortSrc = Seq(aluInstIdxs(i), bruInstIdx, mulInstIdx, muldivInstIdx)
    val wantReadPort = (0 until 4).map(j => (
      if (i == 0) !readPortSrc(j)(2)
      else {
        val prevMax = (0 until i).map(intDeqChoice(_)).reduce((a, b) => Mux(a > b, a, b))
        !readPortSrc(j)(2) && (j.U > prevMax || j.U === 0.U)
      }))
    val readIdxVec = Wire(Vec(4, UInt(2.W)))
    for (j <- 0 until 4) {
      readIdxVec(j) := readPortSrc(j)(1, 0)
    }
    intDeqChoice(i) := PriorityEncoder(wantReadPort)
    XSDebug("int %d: want %b, deqChoice: %d\n", i.U, Cat(wantReadPort), intDeqChoice(i))
    val target = readIdxVec(intDeqChoice(i)(1, 0))
    io.readIntRf(2 * i).addr := io.fromIntDq(target).bits.psrc1
    io.readIntRf(2 * i + 1).addr := io.fromIntDq(target).bits.psrc2
  }
  // intExuIndex: which regfile read ports are assigned to BRU, MUL, MULDIV
  for (j <- 0 until 3) {
    intExuIndex(j) := PriorityEncoder((0 until 4).map(i => intDeqChoice(i) === (j + 1).U))
  }
  XSDebug("intExuIndex: %d %d %d\n", intExuIndex(0), intExuIndex(1), intExuIndex(2))

  // FMAC, FMISC can use the 12 read ports
  // priority: FMAC > FMISC
  val fpExuIndex = WireInit(VecInit(Seq.fill(2)(0.U(2.W))))
  val fpDeqChoice = Wire(Vec(4, UInt(2.W)))
  fpDeqChoice := DontCare
  for (i <- 0 until exuParameters.FmacCnt) {
    val readPortSrc = Seq(fmacInstIdxs(i), fmisc0InstIdx, fmisc1InstIdx)
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
  for (j <- 0 until (exuParameters.FmiscCnt + exuParameters.FmiscDivSqrtCnt)) {
    fpExuIndex(j) := PriorityEncoder((0 until 4).map(i => fpDeqChoice(i) === (j + 1).U))
  }
  XSDebug("fpExuIndex: %d %d\n", fpExuIndex(0), fpExuIndex(1))

  // TODO uncomment me when fmac > 0
  io.readFpRf <> DontCare
  io.readIntRf(2*IntDqDeqWidth).addr := io.fromLsDq(load0InstIdx).bits.psrc1
  io.readIntRf(2*IntDqDeqWidth + 1).addr := io.fromLsDq(load1InstIdx).bits.psrc1
  io.readIntRf(2*IntDqDeqWidth + 2).addr := io.fromLsDq(store0InstIdx).bits.psrc1
  io.readIntRf(2*IntDqDeqWidth + 3).addr := io.fromLsDq(store0InstIdx).bits.psrc2
  io.readIntRf(2*IntDqDeqWidth + 4).addr := io.fromLsDq(store1InstIdx).bits.psrc1
  io.readIntRf(2*IntDqDeqWidth + 5).addr := io.fromLsDq(store1InstIdx).bits.psrc2
  io.readFpRf(3*FpDqDeqWidth).addr := io.fromLsDq(store0InstIdx).bits.psrc1
  io.readFpRf(3*FpDqDeqWidth + 1).addr := io.fromLsDq(store1InstIdx).bits.psrc1

  // insert into reservation station
  val instIdxes = Seq(bruInstIdx, aluInstIdxs(0), aluInstIdxs(1), aluInstIdxs(2), aluInstIdxs(3), mulInstIdx, muldivInstIdx,
    /*load0InstIdx, */store0InstIdx)
  io.enqIQCtrl.zipWithIndex map { case (enq, i) =>
    if (i < exuParameters.IntExuCnt) {
      enq.valid := !instIdxes(i)(2) && io.fromIntDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromIntDq(instIdxes(i)(1, 0)).bits
      val startIndex = if (i == 0) 2.U * intExuIndex(0)
        else if (i > 4) 2.U * intExuIndex(i - 4)
        else (2 * (i - 1)).U
      enq.bits.src1State := io.intPregRdy(startIndex)
      enq.bits.src2State := io.intPregRdy(startIndex + 1.U)
    }
    else if (i < exuParameters.IntExuCnt + exuParameters.FpExuCnt) {
      val startIndex = if (i < exuParameters.IntExuCnt + 4) (3 * (i - exuParameters.IntExuCnt)).U
        else 3.U * fpExuIndex(i - exuParameters.IntExuCnt - 4)
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
      (rs.ready && instIdxes(j) === i.U && (j < exuParameters.IntExuCnt).asBool())
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
        && (j >= exuParameters.IntExuCnt && j < exuParameters.IntExuCnt + exuParameters.FpExuCnt).asBool())
    }).reduce((l, r) => l || r)
    XSInfo(io.fromFpDq(i).fire(), "pc 0x%x leaves Fp dispatch queue with nroq %d\n",
      io.fromFpDq(i).bits.cf.pc, io.fromFpDq(i).bits.roqIdx)
    XSDebug(io.fromFpDq(i).valid && !io.fromFpDq(i).ready,
      "pc 0x%x waits at Fp dispatch queue with index %d\n",
      io.fromFpDq(i).bits.cf.pc, i.U)
  }
  for (i <- 0 until LsDqDeqWidth) {
    io.fromLsDq(i).ready := (io.enqIQCtrl.zipWithIndex map {case (rs, j) =>
      (rs.ready && 0.U === i.U
        && (j >= exuParameters.IntExuCnt + exuParameters.FpExuCnt).asBool())
    }).reduce((l, r) => l || r)
    XSInfo(io.fromLsDq(i).fire(), "pc 0x%x leaves Ls dispatch queue with nroq %d\n",
      io.fromLsDq(i).bits.cf.pc, io.fromLsDq(i).bits.roqIdx)
    XSDebug(io.fromLsDq(i).valid && !io.fromLsDq(i).ready,
      "pc 0x%x waits at Ls dispatch queue with index %d\n",
      io.fromLsDq(i).bits.cf.pc, i.U)
  }

  // TODO: store needs data from FpRegfile
  val intExuIndexReg = Reg(Vec(3, UInt(2.W)))
  val fpExuIndexReg = Reg(Vec(2, UInt(2.W)))
  (0 until 3).map(i => intExuIndexReg(i) := intExuIndex(i))
  (0 until 2).map(i => fpExuIndexReg(i) := fpExuIndex(i))
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
      val startIndex = if (i == 0)2.U * intExuIndexReg(0)
        else if (i > 4) 2.U * intExuIndexReg(i - 4)
        else (2 * (i - 1)).U
      io.enqIQData(i).bits.src1 := Mux(uop_reg(i).ctrl.src1Type === SrcType.pc,
        uop_reg(i).cf.pc, io.readIntRf(startIndex).data)
      io.enqIQData(i).bits.src2 := Mux(uop_reg(i).ctrl.src2Type === SrcType.imm,
        uop_reg(i).ctrl.imm, io.readIntRf(startIndex + 1.U).data)
      srcIndex(0) := startIndex
      srcIndex(1) := startIndex + 1.U
      srcIndex(2) := 0.U
    }
    else if (i < exuParameters.IntExuCnt + exuParameters.FpExuCnt) {
      val startIndex = if (i < exuParameters.IntExuCnt + 4) (3 * (i - exuParameters.IntExuCnt)).U
        else 3.U * fpExuIndexReg(i - exuParameters.IntExuCnt - 4)
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
