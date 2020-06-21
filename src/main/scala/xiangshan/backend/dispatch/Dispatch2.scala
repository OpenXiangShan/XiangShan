package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.regfile.RfReadPort

class Dispatch2 extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    // from dispatch1
//    val in = Flipped(new Dp1ToDp2IO)
    val fromIntDq = Flipped(Vec(IntDqDeqWidth, DecoupledIO(new MicroOp)))
    val fromFpDq = Flipped(Vec(FpDqDeqWidth, DecoupledIO(new MicroOp)))
    val fromLsDq = Flipped(Vec(LsDqDeqWidth, DecoupledIO(new MicroOp)))

    // read regfile
    val readIntRf = Vec(NRReadPorts, Flipped(new RfReadPort))
    val readFpRf = Vec(NRReadPorts, Flipped(new RfReadPort))

    // enq Issue Queue
    val enqIQCtrl = Vec(exuConfig.ExuCnt, DecoupledIO(new MicroOp))
    val enqIQData = Vec(exuConfig.ExuCnt, ValidIO(new ExuInput))
  })
  // disp

  // inst indexes for reservation stations
  // append a true.B to avoid PriorityEncode(0000) -> 3
  // if find a target uop, index[2] == 0, else index[2] == 1
  val bruInstIdx = PriorityEncoder(true.B +: io.fromIntDq.map(_.bits.ctrl.fuType === FuType.bru))
  val alu0InstIdx = PriorityEncoder(true.B +: io.fromIntDq.map(_.bits.ctrl.fuType === FuType.alu))
  val alu1InstIdx = PriorityEncoder(true.B +: (io.fromIntDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.alu && i.U > alu0InstIdx
  }))
  val alu2InstIdx = PriorityEncoder(true.B +: (io.fromIntDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.alu && i.U > alu1InstIdx
  }))
  val alu3InstIdx = PriorityEncoder(true.B +: (io.fromIntDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.alu && i.U > alu2InstIdx
  }))
  val mulInstIdx = PriorityEncoder(true.B +: (io.fromIntDq.map(_.bits.ctrl.fuType === FuType.mul)))
  val muldivInstIdx = PriorityEncoder(true.B +: (io.fromIntDq.zipWithIndex map { case (uop, i) =>
    (uop.bits.ctrl.fuType === FuType.mul && i.U > mulInstIdx) || uop.bits.ctrl.fuType === FuType.mdu
  }))

  val fmac0InstIdx = PriorityEncoder(true.B +: io.fromFpDq.map(_.bits.ctrl.fuType === FuType.fmac))
  val fmac1InstIdx = PriorityEncoder(true.B +: (io.fromFpDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.fmac && i.U > fmac0InstIdx
  }))
  val fmac2InstIdx = PriorityEncoder(true.B +: (io.fromFpDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.fmac && i.U > fmac1InstIdx
  }))
  val fmac3InstIdx = PriorityEncoder(true.B +: (io.fromFpDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.fmac && i.U > fmac2InstIdx
  }))
  val fmisc0InstIdx = PriorityEncoder(true.B +: io.fromFpDq.map(_.bits.ctrl.fuType === FuType.fmisc))
  val fmisc1InstIdx = PriorityEncoder(true.B +: (io.fromFpDq.zipWithIndex map { case (uop, i) =>
    (uop.bits.ctrl.fuType === FuType.fmisc && i.U > fmisc0InstIdx) || uop.bits.ctrl.fuType === FuType.fmiscDivSqrt
  }))

  val load0InstIdx = PriorityEncoder(io.fromLsDq.map(_.bits.ctrl.fuType === FuType.ldu))
  val load1InstIdx = PriorityEncoder(io.fromLsDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.ldu && i.U > load0InstIdx
  })
  val store0InstIdx = PriorityEncoder(io.fromLsDq.map(_.bits.ctrl.fuType === FuType.stu))
  val store1InstIdx = PriorityEncoder(io.fromLsDq.zipWithIndex map { case (uop, i) =>
    uop.bits.ctrl.fuType === FuType.stu && i.U > store0InstIdx
  })

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
    load0InstIdx, store0InstIdx)
  io.enqIQCtrl.zipWithIndex map { case (enq, i) =>
    if (i < exuConfig.IntExuCnt) {
      enq.valid := !instIdxes(i)(2) && io.fromIntDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromIntDq(instIdxes(i)(1, 0)).bits
    }
    else if (i < exuConfig.IntExuCnt + exuConfig.FpExuCnt) {
      enq.valid := !instIdxes(i)(2) && io.fromFpDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromFpDq(instIdxes(i)(1, 0)).bits
    }
    else {
      enq.valid := !instIdxes(i)(2) && io.fromLsDq(instIdxes(i)(1, 0)).valid
      enq.bits := io.fromLsDq(instIdxes(i)(1, 0)).bits
    }
  }

  // responds to dispatch queue
  val portIndexMapping
  for (i <- 0 until IntDqDeqWidth) {
    io.fromIntDq(i).ready := (io.enqIQCtrl.zipWithIndex map {case (rs, j) =>
      (rs.ready && instIdxes(j) === i.U && (i < exuConfig.IntExuCnt).asBool())
    }).reduce((l, r) => l || r)
  }
  for (i <- 0 until FpDqDeqWidth) {
    io.fromFpDq(i).ready := (io.enqIQCtrl.zipWithIndex map {case (rs, j) =>
      (rs.ready && instIdxes(j) === i.U
        && (i > exuConfig.IntExuCnt && i < exuConfig.IntExuCnt + exuConfig.FpExuCnt).asBool())
    }).reduce((l, r) => l || r)
  }
  for (i <- 0 until LsDqDeqWidth) {
    io.fromLsDq(i).ready := (io.enqIQCtrl.zipWithIndex map {case (rs, j) =>
      (rs.ready && instIdxes(j) === i.U
        && (i > exuConfig.IntExuCnt + exuConfig.FpExuCnt).asBool())
    }).reduce((l, r) => l || r)
  }

  // next stage: insert data
  val data_valid = Reg(Vec(exuConfig.ExuCnt, Bool()))
  val uop_reg = Reg(Vec(exuConfig.ExuCnt, new MicroOp))
  // indexes can be one-hot to reduce overhead
  val index_reg = Reg(Vec(exuConfig.ExuCnt, UInt(instIdxes(0).getWidth.W)))
  for (i <- 0 until exuConfig.ExuCnt) {
    data_valid(i) := io.enqIQCtrl(i).fire()
    uop_reg := io.enqIQCtrl(i).bits
    index_reg(i) := instIdxes(i)

    io.enqIQData(i).valid := data_valid(i)
    io.enqIQData(i).bits.uop := uop_reg(i)
    val intSrc1 = io.readIntRf((index_reg(i) << 1).asUInt()).data
    val fpSrc1 = io.readFpRf((index_reg(i) * 3.U).asUInt()).data
    io.enqIQData(i).bits.src1 := Mux(index_reg(i)(2), 0.U, if (i < exuConfig.IntExuCnt) intSrc1
    else if (i < exuConfig.IntExuCnt + exuConfig.FpExuCnt)
    io.enqIQData(i).bits.src2 :=
    io.enqIQData(i).bits.src3 :=
    io.enqIQData(i).bits.isRVF =
  }


}
