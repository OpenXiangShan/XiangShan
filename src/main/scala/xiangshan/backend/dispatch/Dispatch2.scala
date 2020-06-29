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

  // inst indexes for reservation stations
  // append a true.B to avoid PriorityEncode(0000) -> 3
  // if find a target uop, index[2] == 0, else index[2] == 1
  val bruInstIdx = PriorityEncoder(io.fromIntDq.map(_.bits.ctrl.fuType === FuType.bru) :+ true.B)
  val alu0InstIdx = PriorityEncoder(io.fromIntDq.map(_.bits.ctrl.fuType === FuType.alu) :+ true.B)
  val alu1InstIdx = PriorityEncoder((io.fromIntDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.alu && i.U > alu0InstIdx
  }) :+ true.B)
  val alu2InstIdx = PriorityEncoder((io.fromIntDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.alu && i.U > alu1InstIdx
  }) :+ true.B)
  val alu3InstIdx = PriorityEncoder((io.fromIntDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.alu && i.U > alu2InstIdx
  }) :+ true.B)
  val mulInstIdx = PriorityEncoder(io.fromIntDq.map(_.bits.ctrl.fuType === FuType.mul) :+ true.B)
  val muldivInstIdx = PriorityEncoder((io.fromIntDq.zipWithIndex map { case (uop, i) =>
    (uop.bits.ctrl.fuType === FuType.mul && i.U > mulInstIdx) || uop.bits.ctrl.fuType === FuType.mdu
  }) :+ true.B)

  val fmac0InstIdx = PriorityEncoder(io.fromFpDq.map(_.bits.ctrl.fuType === FuType.fmac) :+ true.B)
  val fmac1InstIdx = PriorityEncoder((io.fromFpDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.fmac && i.U > fmac0InstIdx
  }) :+ true.B)
  val fmac2InstIdx = PriorityEncoder((io.fromFpDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.fmac && i.U > fmac1InstIdx
  }) :+ true.B)
  val fmac3InstIdx = PriorityEncoder((io.fromFpDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.fmac && i.U > fmac2InstIdx
  }) :+ true.B)
  val fmisc0InstIdx = PriorityEncoder(io.fromFpDq.map(_.bits.ctrl.fuType === FuType.fmisc) :+ true.B)
  val fmisc1InstIdx = PriorityEncoder((io.fromFpDq.zipWithIndex map { case (uop, i) =>
    (uop.bits.ctrl.fuType === FuType.fmisc && i.U > fmisc0InstIdx) || uop.bits.ctrl.fuType === FuType.fmiscDivSqrt
  }) :+ true.B)

  // TODO: currently there's only one load/store reservation station
  // val load0InstIdx = PriorityEncoder(io.fromLsDq.map(deq => (deq.bits.ctrl.fuType === FuType.ldu)) :+ true.B)
  val load0InstIdx = PriorityEncoder(io.fromLsDq.map(deq => FuType.isMemExu(deq.bits.ctrl.fuType)) :+ true.B)
  val load1InstIdx = PriorityEncoder((io.fromLsDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.ldu && i.U > load0InstIdx
  }) :+ true.B)
  val store0InstIdx = PriorityEncoder(io.fromLsDq.map(_.bits.ctrl.fuType === FuType.stu))
  val store1InstIdx = PriorityEncoder((io.fromLsDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.stu && i.U > store0InstIdx
  }) :+ true.B)

  // regfile read ports
  // regfile is sync-read, data can used at the next cycle
  for (i <- 0 until IntDqDeqWidth) {
    io.readIntRf(2 * i).addr := io.fromIntDq(i).bits.psrc1
    io.readIntRf(2 * i + 1).addr := io.fromIntDq(i).bits.psrc2
  }
  for (i <- 0 until FpDqDeqWidth) {
    io.readFpRf(3 * i).addr := io.fromFpDq(i).bits.psrc1
    io.readFpRf(3 * i + 1).addr := io.fromFpDq(i).bits.psrc2
    io.readFpRf(3 * i + 2).addr := io.fromFpDq(i).bits.psrc3
  }
  io.readIntRf(2*IntDqDeqWidth).addr := io.fromLsDq(load0InstIdx).bits.psrc1
  io.readIntRf(2*IntDqDeqWidth + 1).addr := io.fromLsDq(load1InstIdx).bits.psrc1
  io.readIntRf(2*IntDqDeqWidth + 2).addr := io.fromLsDq(store0InstIdx).bits.psrc1
  io.readIntRf(2*IntDqDeqWidth + 3).addr := io.fromLsDq(store0InstIdx).bits.psrc2
  io.readIntRf(2*IntDqDeqWidth + 4).addr := io.fromLsDq(store1InstIdx).bits.psrc1
  io.readIntRf(2*IntDqDeqWidth + 5).addr := io.fromLsDq(store1InstIdx).bits.psrc2
  io.readFpRf(3*FpDqDeqWidth).addr := io.fromLsDq(store0InstIdx).bits.psrc1
  io.readFpRf(3*FpDqDeqWidth + 1).addr := io.fromLsDq(store1InstIdx).bits.psrc1

  // insert into reservation station
  val instIdxes = Seq(bruInstIdx, alu0InstIdx, alu1InstIdx, alu2InstIdx, alu3InstIdx, mulInstIdx, muldivInstIdx,
    fmac0InstIdx, fmac1InstIdx, fmac2InstIdx, fmac3InstIdx, fmisc0InstIdx, fmisc1InstIdx,
    load0InstIdx)//, store0InstIdx)
  io.enqIQCtrl.zipWithIndex map { case (enq, i) =>
    if (i < exuConfig.IntExuCnt) {
      enq.valid := !instIdxes(i)(2) && io.fromIntDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromIntDq(instIdxes(i)(1, 0)).bits
      enq.bits.src1State := io.intPregRdy((instIdxes(i) << 1).asUInt())
      enq.bits.src2State := io.intPregRdy((instIdxes(i) << 1).asUInt() + 1.U)
    }
    else if (i < exuConfig.IntExuCnt + exuConfig.FpExuCnt) {
      enq.valid := !instIdxes(i)(2) && io.fromFpDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromFpDq(instIdxes(i)(1, 0)).bits
      enq.bits.src1State := io.fpPregRdy(instIdxes(i) * 3.U)
      enq.bits.src2State := io.fpPregRdy(instIdxes(i) * 3.U + 1.U)
      enq.bits.src3State := io.fpPregRdy(instIdxes(i) * 3.U + 2.U)
    }
    else {
      enq.valid := !instIdxes(i)(2) && io.fromLsDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromLsDq(instIdxes(i)(1, 0)).bits
      // TODO load and store
      enq.bits.src1State := Mux(enq.bits.ctrl.fuType === FuType.ldu, io.intPregRdy(8), io.intPregRdy(10))
      enq.bits.src2State := io.intPregRdy(11)
    }

    XSInfo(enq.fire(), "instruction 0x%x with type %b srcState(%d %d %d) enters reservation station %d from %d\n",
      enq.bits.cf.pc, enq.bits.ctrl.fuType, enq.bits.src1State, enq.bits.src2State, enq.bits.src3State, i.U, instIdxes(i))
  }

  // responds to dispatch queue
  for (i <- 0 until IntDqDeqWidth) {
    io.fromIntDq(i).ready := (io.enqIQCtrl.zipWithIndex map {case (rs, j) =>
      (rs.ready && instIdxes(j) === i.U && (j < exuConfig.IntExuCnt).asBool())
    }).reduce((l, r) => l || r)
    XSInfo(io.fromIntDq(i).fire(), "instruction 0x%x leaves Int dispatch queue with nroq %d\n",
      io.fromIntDq(i).bits.cf.pc, io.fromIntDq(i).bits.roqIdx)
    XSDebug(io.fromIntDq(i).valid && !io.fromIntDq(i).ready,
      "instruction 0x%x waits at Int dispatch queue with index %d\n",
      io.fromIntDq(i).bits.cf.pc, i.U)
  }
  for (i <- 0 until FpDqDeqWidth) {
    io.fromFpDq(i).ready := (io.enqIQCtrl.zipWithIndex map {case (rs, j) =>
      (rs.ready && instIdxes(j) === i.U
        && (j >= exuConfig.IntExuCnt && j < exuConfig.IntExuCnt + exuConfig.FpExuCnt).asBool())
    }).reduce((l, r) => l || r)
    XSInfo(io.fromFpDq(i).fire(), "instruction 0x%x leaves Fp dispatch queue with nroq %d\n",
      io.fromFpDq(i).bits.cf.pc, io.fromFpDq(i).bits.roqIdx)
    XSDebug(io.fromFpDq(i).valid && !io.fromFpDq(i).ready,
      "instruction 0x%x waits at Fp dispatch queue with index %d\n",
      io.fromFpDq(i).bits.cf.pc, i.U)
  }
  for (i <- 0 until LsDqDeqWidth) {
    io.fromLsDq(i).ready := (io.enqIQCtrl.zipWithIndex map {case (rs, j) =>
      (rs.ready && instIdxes(j) === i.U
        && (j >= exuConfig.IntExuCnt + exuConfig.FpExuCnt).asBool())
    }).reduce((l, r) => l || r)
    XSInfo(io.fromLsDq(i).fire(), "instruction 0x%x leaves Ls dispatch queue with nroq %d\n",
      io.fromLsDq(i).bits.cf.pc, io.fromLsDq(i).bits.roqIdx)
    XSDebug(io.fromLsDq(i).valid && !io.fromLsDq(i).ready,
      "instruction 0x%x waits at Ls dispatch queue with index %d\n",
      io.fromLsDq(i).bits.cf.pc, i.U)
  }

  // next stage: insert data
  val data_valid = Reg(Vec(exuConfig.ExuCnt, Bool()))
  val uop_reg = Reg(Vec(exuConfig.ExuCnt, new MicroOp))
  // indexes can be one-hot to reduce overhead
  val index_reg = Reg(Vec(exuConfig.ExuCnt, UInt(instIdxes(0).getWidth.W)))
  // types: 0 for Int, 1 for Fp, 2 for empty
  // TODO: store needs data from FpRegfile
  val src1Type = (0 until exuConfig.ExuCnt).map(i =>
    if (i < exuConfig.IntExuCnt) 0.U
    else if (i < exuConfig.IntExuCnt + exuConfig.FpExuCnt) 1.U
    else if (i == exuConfig.IntExuCnt + exuConfig.FpExuCnt) 0.U
    else 0.U // TODO: Mux(uop_reg(i).ctrl)
  )
  val src2Type = (0 until exuConfig.ExuCnt).map(i =>
    if (i < exuConfig.IntExuCnt) 0.U
    else if (i < exuConfig.IntExuCnt + exuConfig.FpExuCnt) 1.U
    else if (i == exuConfig.IntExuCnt + exuConfig.FpExuCnt) 2.U
    else 0.U
  )
  val src3Type = (0 until exuConfig.ExuCnt).map(i =>
    if (i < exuConfig.IntExuCnt) 2.U
    else if (i < exuConfig.IntExuCnt + exuConfig.FpExuCnt) 1.U
    else if (i == exuConfig.IntExuCnt + exuConfig.FpExuCnt) 2.U
    else 2.U
  )
  val src1Index = (0 until exuConfig.ExuCnt).map(i =>
    if (i < exuConfig.IntExuCnt) (index_reg(i) << 1).asUInt()
    else if (i < exuConfig.IntExuCnt + exuConfig.FpExuCnt) (index_reg(i) * 3.U).asUInt()
    else if (i == exuConfig.IntExuCnt + exuConfig.FpExuCnt) 8.U
    else 10.U
  )
  val src2Index = (0 until exuConfig.ExuCnt).map(i =>
    if (i < exuConfig.IntExuCnt) (index_reg(i) << 1).asUInt() + 1.U
    else if (i < exuConfig.IntExuCnt + exuConfig.FpExuCnt) index_reg(i) * 3.U + 1.U
    else if (i == exuConfig.IntExuCnt + exuConfig.FpExuCnt) 0.U
    else 11.U
  )
  val src3Index = (0 until exuConfig.ExuCnt).map(i =>
    if (i < exuConfig.IntExuCnt) 0.U
    else if (i < exuConfig.IntExuCnt + exuConfig.FpExuCnt) index_reg(i) * 3.U + 2.U
    else if (i == exuConfig.IntExuCnt + exuConfig.FpExuCnt) 0.U
    else 0.U
  )
  for (i <- 0 until exuConfig.ExuCnt) {
    data_valid(i) := io.enqIQCtrl(i).fire()
    uop_reg(i) := io.enqIQCtrl(i).bits
    index_reg(i) := instIdxes(i)

    io.enqIQData(i).valid := data_valid(i)
    io.enqIQData(i).bits.uop := uop_reg(i)
//    io.enqIQData(i).bits.uop.src1State := Mux(src1Type(i)(1), SrcState.rdy,
//      Mux(src1Type(i)(0), io.intPregRdy(src1Index(i)), io.fpPregRdy(src1Index(i))))
//    io.enqIQData(i).bits.uop.src2State := Mux(src2Type(i)(1), SrcState.rdy,
//      Mux(src2Type(i)(0), io.intPregRdy(src2Index(i)), io.fpPregRdy(src2Index(i))))
//    io.enqIQData(i).bits.uop.src3State := Mux(src3Type(i)(1), SrcState.rdy,
//      Mux(src3Type(i)(0), io.intPregRdy(src3Index(i)), io.fpPregRdy(src3Index(i))))
    val src1 = Mux(src1Type(i)(1), io.readFpRf(src1Index(i)).data, io.readIntRf(src1Index(i)).data)
    io.enqIQData(i).bits.src1 := Mux(io.enqIQData(i).bits.uop.ctrl.src1Type === SrcType.pc,
        io.enqIQData(i).bits.uop.cf.pc, Mux(index_reg(i)(2), 0.U, src1))
    val src2 = Mux(src2Type(i)(1), io.readFpRf(src2Index(i)).data, io.readIntRf(src2Index(i)).data)
    io.enqIQData(i).bits.src2 := Mux(io.enqIQData(i).bits.uop.ctrl.src2Type === SrcType.imm,
      io.enqIQData(i).bits.uop.ctrl.imm, Mux(index_reg(i)(2), 0.U, src2))
    val src3 = Mux(src3Type(i)(1), io.readFpRf(src3Index(i)).data, io.readIntRf(src3Index(i)).data)
    io.enqIQData(i).bits.src3 := Mux(index_reg(i)(2), 0.U, src3)

    XSDebug(io.enqIQData(i).valid,
      "instruction 0x%x reads operands from (%d, %d, %d, %x), (%d, %d, %d, %x), (%d, %d, %d, %x)\n",
      io.enqIQData(i).bits.uop.cf.pc,
      src1Type(i), src1Index(i), io.enqIQData(i).bits.uop.psrc1, io.enqIQData(i).bits.src1,
      src2Type(i), src2Index(i), io.enqIQData(i).bits.uop.psrc2, io.enqIQData(i).bits.src2,
      src3Type(i), src3Index(i), io.enqIQData(i).bits.uop.psrc3, io.enqIQData(i).bits.src3)
  }

}
