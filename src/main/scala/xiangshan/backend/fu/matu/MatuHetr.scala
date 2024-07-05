//package xiangshan.backend.fu.matu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import matu.DataBuffer._
import matu.SystolicArray._
import utils._
import xiangshan._

/*class lsuIO (implicit p: Parameters) extends XSBundle {

  val ldIn = Vec(exuParameters.LduCnt, Flipped(DecoupledIO(new ExuOutput)))

  val data = ValidIO(UInt(XLEN.W))
}

class robIO (implicit p: Parameters) extends XSBundle {
  val deqPtrVec_v = Vec(CommitWidth, Input(UInt(5.W)))
}

class MatuHetr(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val lsuIO = new lsuIO()
    val robIO = new robIO()
  })
  val dataModule = Module(new XS_miniTPU_M)

  dataModule.io.xsIO.in(0).bits.ldIn := io.lsuIO.ldIn(0).bits.data
  dataModule.io.xsIO.in(1).bits.ldIn := io.lsuIO.ldIn(1).bits.data
  dataModule.io.xsIO.in(0).bits.OpType := io.lsuIO.ldIn(0).bits.uop.ctrl.fuOpType
  dataModule.io.xsIO.in(1).bits.OpType := io.lsuIO.ldIn(1).bits.uop.ctrl.fuOpType
  dataModule.io.xsIO.in(0).bits.robIdx := io.lsuIO.ldIn(0).bits.uop.robIdx.value
  dataModule.io.xsIO.in(1).bits.robIdx := io.lsuIO.ldIn(1).bits.uop.robIdx.value
  dataModule.io.xsIO.in(0).valid := io.lsuIO.ldIn(0).valid
  dataModule.io.xsIO.in(1).valid := io.lsuIO.ldIn(1).valid
  dataModule.io.xsIO.deqptr := io.robIO.deqPtrVec_v

  io.lsuIO.ldIn(0).ready := true.B // TODO: Back Pressure
  io.lsuIO.ldIn(1).ready := true.B

  dataModule.io.xsIO.out.ready := false.B
  io.lsuIO.data.valid := dataModule.io.xsIO.out.valid
  io.lsuIO.data.bits := dataModule.io.xsIO.out.bits.data

}

class xsFUInput_M(implicit p: Parameters) extends XSBundle {
  val ldIn = Input(UInt(XLEN.W))
  val OpType = Input(FuOpType())
  val robIdx = Input(UInt(5.W))
}

class xsFUOutput_M(implicit p: Parameters) extends XSBundle {
  val data = Output(UInt(XLEN.W))
}

class xsFUIO_M (implicit p: Parameters) extends XSBundle {

  val in = Vec(exuParameters.LduCnt, Flipped(ValidIO(new xsFUInput_M)))
  val deqptr = Vec(CommitWidth, Input(UInt(5.W)))

  val out = DecoupledIO(new xsFUOutput_M())
}

class XS_miniTPU_M(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val xsIO = new xsFUIO_M()
  })

  val mini_tpu  = Module(new top_M(4,16,2,2))
  val outBridge = Module(new OutputBridge_M())
  val inBridge_m  = Module(new InputBridge_M())
  val rob = Module(new SA_ROB)

  rob.io.ldin.valid(0) := io.xsIO.in(0).valid
  rob.io.ldin.valid(1) := io.xsIO.in(1).valid
  rob.io.ldin.data(0) := io.xsIO.in(0).bits.ldIn
  rob.io.ldin.data(1) := io.xsIO.in(1).bits.ldIn
  rob.io.ldin.robIdx(0) := io.xsIO.in(0).bits.robIdx
  rob.io.ldin.robIdx(1) := io.xsIO.in(1).bits.robIdx
  rob.io.ldin.fuOptype(0) := io.xsIO.in(0).bits.OpType
  rob.io.ldin.fuOptype(1) := io.xsIO.in(1).bits.OpType
  rob.io.deqptr := io.xsIO.deqptr

  inBridge_m.io.in_valid := rob.io.out.valid
  rob.io.out.ready := inBridge_m.io.out_ready
  inBridge_m.io.ldIn := rob.io.out.bits.asSInt

  inBridge_m.io.in_ready := mini_tpu.io.tpuIO.in.in_a.ready | mini_tpu.io.tpuIO.in.in_b.ready
  mini_tpu.io.tpuIO.in.in_a.valid := inBridge_m.io.out_valid_h
  mini_tpu.io.tpuIO.in.in_b.valid := inBridge_m.io.out_valid_v
  mini_tpu.io.tpuIO.in.in_a.bits := inBridge_m.io.in_a
  mini_tpu.io.tpuIO.in.in_b.bits := inBridge_m.io.in_b
  mini_tpu.io.tpuIO.in.in_c := Seq.fill(2)(0.S(16.W))

  outBridge.io.in_valid := mini_tpu.io.tpuIO.out.valid
  mini_tpu.io.tpuIO.out.ready := outBridge.io.out_ready
  outBridge.io.out_c := mini_tpu.io.tpuIO.out.bits.out_c

  outBridge.io.in_ready := io.xsIO.out.ready
  io.xsIO.out.valid := outBridge.io.out_valid
  io.xsIO.out.bits.data := outBridge.io.result.asUInt

}

class OutputBridge_M() extends Module {
  val io = IO(new Bundle {
    val in_valid  = Input(Bool())
    val out_ready = Output(Bool())
    val out_c     = Input(Vec(2, SInt(16.W)))
    val in_ready  = Input(Bool())
    val out_valid = Output(Bool())
    val result    = Output(SInt(64.W))
  })

  val out_ptr      = RegInit(false.B)
  val out_valid_r  = RegInit(false.B)
  val result       = Seq.fill(2, 2)(RegInit(0.S(16.W)))

  val indices = Seq((0, 0), (0, 1), (1, 0), (1, 1))

  when (out_ptr === false.B && io.in_valid) {
    out_ptr         := true.B
    out_valid_r     := false.B
    Seq(indices(2), indices(3)).zip(io.out_c).foreach {case ((i, j), out) => result(i)(j) := out}

  }.elsewhen (out_ptr === true.B && io.in_valid){
    out_ptr         := false.B
    out_valid_r     := io.in_valid
    Seq(indices(0), indices(1)).zip(io.out_c).foreach {case ((i, j), out) => result(i)(j) := out}
  }.elsewhen (io.in_ready) {
    out_ptr         := false.B
    out_valid_r     := io.in_valid
    Seq(indices(0), indices(1), indices(2), indices(3)).foreach {case (i, j) => result(i)(j) := io.out_c(j)}
  }

  io.out_valid := out_valid_r
  io.out_ready := io.in_ready
  io.result    := Cat(indices.map {case (i, j) => result(i)(j)}.reverse).asSInt

}

class ROBIn(implicit p: Parameters) extends XSBundle {
  val data = Vec(exuParameters.LduCnt, Input(UInt(XLEN.W)))
  val robIdx = Vec(exuParameters.LduCnt, Input(UInt(5.W)))
  val fuOptype = Vec(exuParameters.LduCnt, Input(FuOpType()))
  val valid = Vec(exuParameters.LduCnt, Input(Bool()))
}

class SA_ROB(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val deqptr = Vec(CommitWidth, Input(UInt(5.W)))
    val ldin = new ROBIn()
    val out = DecoupledIO(UInt(XLEN.W))
    // val ready = Input(Bool())
  })

  val data_queue = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(4)(0.U(64.W))))))
  val data_writePtr = RegInit(VecInit(Seq.fill(2)(0.U(2.W))))

  val robIdx_queue = Seq.fill(2)(RegInit(VecInit(Seq.fill(4)(0.U(5.W)))))
  val robIdx_writePtr = RegInit(VecInit(Seq.fill(2)(0.U(2.W))))

  val valid_queue = Seq.fill(2)(RegInit(VecInit(Seq.fill(4)(false.B))))
  val valid_writePtr = RegInit(VecInit(Seq.fill(2)(0.U(2.W))))

  val deqptrReg = RegInit(VecInit(Seq.fill(6)(0.U(5.W))))

  val MatchVec = Seq.fill(2)(WireInit(VecInit(Seq.fill(4)(VecInit(Seq.fill(6)(false.B))))))
  val MatchVec_lastCycle = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(4)(0.U(6.W))))))
  val MatchVecUInt = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(4)(0.U(12.W))))))
  val MatchVecMask = WireInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(4)(4095.U(12.W))))))

  val deqData = WireInit(0.U(64.W))

  deqptrReg := io.deqptr
  MatchVec_lastCycle := MatchVecUInt


  for (i <- 0 until 2) {
    when((io.ldin.fuOptype(i) === LSUOpType.mlb) && (io.ldin.valid(i) === true.B)){
      data_writePtr(i) := data_writePtr(i) + 1.U
      data_queue(i)(data_writePtr(i)):= io.ldin.data(i)

      robIdx_writePtr(i) := robIdx_writePtr(i) + 1.U
      robIdx_queue(i)(robIdx_writePtr(i)) := io.ldin.robIdx(i)

      valid_writePtr(i) := valid_writePtr(i) + 1.U
      valid_queue(i)(valid_writePtr(i)) := io.ldin.valid(i)
    }
  }

  for(i <- 0 until 2) {
    for(j <- 0 until 4) {
      for(k <- 0 until 6) {
        when (valid_queue(i)(j) && (robIdx_queue(i)(j) === deqptrReg(k))) {
          MatchVec(i)(j)(k) := true.B
        }
      }
    }
  }

  val minMask = Wire(UInt(12.W))
  val minMask_0 = Wire(UInt(12.W))
  val minMask_1 = Wire(UInt(12.W))
  val minMask_0_0_1 = Wire(UInt(12.W))
  val minMask_0_2_3 = Wire(UInt(12.W))
  val minMask_1_0_1 = Wire(UInt(12.W))
  val minMask_1_2_3 = Wire(UInt(12.W))
  val minIndex_i = WireInit(0.U(1.W))
  val minIndex_j = WireInit(0.U(2.W))

  val maxMatch = RegInit(0.U(12.W))
  val maxMatch_0 = Wire(UInt(12.W))
  val maxMatch_1 = Wire(UInt(12.W))
  val maxMatch_0_0_1 = Wire(UInt(12.W))
  val maxMatch_0_2_3 = Wire(UInt(12.W))
  val maxMatch_1_0_1 = Wire(UInt(12.W))
  val maxMatch_1_2_3 = Wire(UInt(12.W))

  minMask_0_0_1 := Mux(MatchVecMask(0)(0) > MatchVecMask(0)(1), MatchVecMask(0)(1), MatchVecMask(0)(0))
  minMask_0_2_3 := Mux(MatchVecMask(0)(2) > MatchVecMask(0)(3), MatchVecMask(0)(3), MatchVecMask(0)(2))
  minMask_0 := Mux(minMask_0_0_1 > minMask_0_2_3, minMask_0_2_3, minMask_0_0_1)

  minMask_1_0_1 := Mux(MatchVecMask(1)(0) > MatchVecMask(1)(1), MatchVecMask(1)(1), MatchVecMask(1)(0))
  minMask_1_2_3 := Mux(MatchVecMask(1)(2) > MatchVecMask(1)(3), MatchVecMask(1)(3), MatchVecMask(1)(2))
  minMask_1 := Mux(minMask_1_0_1 > minMask_1_2_3, minMask_1_2_3, minMask_1_0_1)

  minMask := Mux(minMask_0 > minMask_1, minMask_1, minMask_0)

  maxMatch_0_0_1 := Mux(MatchVecUInt(0)(0) < MatchVecUInt(0)(1), MatchVecUInt(0)(1), MatchVecUInt(0)(0))
  maxMatch_0_2_3 := Mux(MatchVecUInt(0)(2) < MatchVecUInt(0)(3), MatchVecUInt(0)(3), MatchVecUInt(0)(2))
  maxMatch_0 := Mux(maxMatch_0_0_1 < maxMatch_0_2_3, maxMatch_0_2_3, maxMatch_0_0_1)

  maxMatch_1_0_1 := Mux(MatchVecUInt(1)(0) < MatchVecUInt(1)(1), MatchVecUInt(1)(1), MatchVecUInt(1)(0))
  maxMatch_1_2_3 := Mux(MatchVecUInt(1)(2) < MatchVecUInt(1)(3), MatchVecUInt(1)(3), MatchVecUInt(1)(2))
  maxMatch_1 := Mux(maxMatch_1_0_1 < maxMatch_1_2_3, maxMatch_1_2_3, maxMatch_1_0_1)

  maxMatch := Mux(maxMatch_0 < maxMatch_1, maxMatch_1, maxMatch_0)

  for (i <- 0 until 2) {
    for (j <- 0 until 4) {
      when(MatchVec(i)(j).asUInt > 0.U && MatchVec_lastCycle(i)(j) === 0.U) {
        MatchVecUInt(i)(j) := MatchVec(i)(j).asUInt + maxMatch
      }.elsewhen(MatchVec(i)(j).asUInt > 0.U) {
        MatchVecUInt(i)(j) := MatchVec(i)(j).asUInt
      }
    }
  }

  for (i <- 0 until 2) {
    for (j <- 0 until 4) {
      when(MatchVecUInt(i)(j) === 0.U) {
        MatchVecMask(i)(j) := ~MatchVecUInt(i)(j)
      }.otherwise {
        MatchVecMask(i)(j) := MatchVecUInt(i)(j)
      }
    }
  }

  for(i <- 0 until 2) {
    for(j <- 0 until 4){
      when(minMask === MatchVecMask(i)(j) && (MatchVecMask(i)(j) < 4095.U)) {
        minIndex_i := i.U
        minIndex_j := j.U
        valid_queue(i)(j) := false.B
        MatchVecUInt(i)(j) := 0.U
        deqData := data_queue(minIndex_i)(minIndex_j)
      }
    }
  }
  io.out.bits := deqData
  io.out.valid := minMask < 4095.U

}




class InputBridge_M() extends Module {
  val io = IO(new Bundle() {
    val in_valid  = Input(Bool())
    val out_ready = Output(Bool())
    val ldIn      = Input(SInt(64.W))
    val in_ready  = Input(Bool())
    val out_valid_h = Output(Bool())
    val out_valid_v = Output(Bool())
    val in_a      = Output(Vec(2, SInt(4.W)))
    val in_b      = Output(Vec(2, SInt(4.W)))
  })

  val cnt     = RegInit(0.U(2.W))
  val in_ptr  = RegInit(false.B)
  when (in_ptr === false.B && cnt < 1.U && io.in_ready && io.in_valid ) {
    cnt := cnt + 1.U
    io.in_a(0) := io.ldIn(3, 0).asSInt
    io.in_a(1) := io.ldIn(7, 4).asSInt
    io.out_valid_h := io.in_valid
    io.out_valid_v := false.B
    io.in_b(0) := 0.S
    io.in_b(1) := 0.S
  }.elsewhen ( in_ptr === false.B && cnt === 1.U && io.in_ready && io.in_valid ) {
    in_ptr     := true.B
    cnt        := 0.U
    io.out_valid_h := io.in_valid
    io.out_valid_v := false.B
    io.in_a(0) := io.ldIn(3, 0).asSInt
    io.in_a(1) := io.ldIn(7, 4).asSInt
    io.in_b(0) := 0.S
    io.in_b(1) := 0.S
  }.elsewhen ( in_ptr === true.B && cnt < 1.U && io.in_ready && io.in_valid ) {
    cnt := cnt + 1.U
    io.in_a(0) := 0.S
    io.in_a(1) := 0.S
    io.out_valid_h := false.B
    io.out_valid_v := io.in_valid
    io.in_b(0) := io.ldIn(3, 0).asSInt
    io.in_b(1) := io.ldIn(7, 4).asSInt
  }.elsewhen ( in_ptr === true.B && cnt === 1.U && io.in_ready && io.in_valid ) {
    in_ptr     := false.B
    cnt        := 0.U
    io.in_a(0) := 0.S
    io.in_a(1) := 0.S
    io.out_valid_h := false.B
    io.out_valid_v := io.in_valid
    io.in_b(0) := io.ldIn(3, 0).asSInt
    io.in_b(1) := io.ldIn(7, 4).asSInt
  }.otherwise {
    io.out_valid_h := io.in_valid
    io.out_valid_v := io.in_valid
    io.in_a(0) := 0.S
    io.in_a(1) := 0.S
    io.in_b(0) := 0.S
    io.in_b(1) := 0.S
  }
  io.out_ready := io.in_ready
}

class miniTPUInput_M(val IN_WIDTH: Int, val C_WIDTH: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Bundle {
  val in_a = Flipped(DecoupledIO(Vec(SA_ROWS, SInt(IN_WIDTH.W))))
  val in_b = Flipped(DecoupledIO(Vec(SA_COLS, SInt(IN_WIDTH.W))))
  val in_c = Input(Vec(SA_COLS, SInt(C_WIDTH.W)))
}

class miniTPUOutput_M(val IN_WIDTH: Int, val C_WIDTH: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Bundle {
  val out_c = Output(Vec(SA_COLS, SInt(C_WIDTH.W)))
}

class miniTPUIO_M (val IN_WIDTH: Int, val C_WIDTH: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Bundle {
  val in = new miniTPUInput_M(IN_WIDTH, C_WIDTH, SA_ROWS, SA_COLS)
  val out = DecoupledIO(new miniTPUOutput_M(IN_WIDTH, C_WIDTH, SA_ROWS, SA_COLS))
}

/*
   type-0 : pipeline systolic array
   type-1 : algorithm accelerator
   ...
*/
class top_M (val IN_WIDTH: Int, val C_WIDTH: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Module {
  val io = IO(new Bundle {
    val tpuIO = new miniTPUIO_M(IN_WIDTH, C_WIDTH, SA_ROWS, SA_COLS)
  })

  val sa = Module(new SystolicArray(IN_WIDTH, C_WIDTH, SA_ROWS, SA_COLS))
  val controller = Module(new Controller(SA_ROWS, SA_COLS))
  val inBuffer_h = Module(new SAInputBuffer(IN_WIDTH, SA_ROWS, SA_COLS)) // horizontal and vertical data buffer
  val inBuffer_v = Module(new SAInputBuffer(IN_WIDTH, SA_COLS, SA_ROWS)) // TODO: add control logic to select data( B or D)
  val outBuffer = Module(new SAOutputBuffer(C_WIDTH, SA_COLS, SA_ROWS))

  inBuffer_h.io.data_in.valid := io.tpuIO.in.in_a.valid
  inBuffer_h.io.data_in.bits := io.tpuIO.in.in_a.bits
  inBuffer_h.io.ctrl_ib_data_out := controller.io.ctrl_ib_data_out

  inBuffer_v.io.data_in.valid := io.tpuIO.in.in_b.valid
  inBuffer_v.io.data_in.bits := io.tpuIO.in.in_b.bits
  inBuffer_v.io.ctrl_ib_data_out := controller.io.ctrl_ib_data_out
  io.tpuIO.in.in_a.ready := inBuffer_h.io.data_in.ready
  io.tpuIO.in.in_b.ready := inBuffer_v.io.data_in.ready

  io.tpuIO.out.valid := outBuffer.io.data_out.valid
  io.tpuIO.out.bits.out_c := outBuffer.io.data_out.bits
  outBuffer.io.data_out.ready := io.tpuIO.out.ready
  outBuffer.io.ctrl_ob_data_in := controller.io.ctrl_ob_data_in

  sa.io.in_a := inBuffer_h.io.data_out
  sa.io.in_b := inBuffer_v.io.data_out
  sa.io.in_c := io.tpuIO.in.in_c // TODO: preload in_c as bias
  outBuffer.io.data_in := sa.io.out_c
  sa.io.in_control.foreach(_.ctrl_sa_send_data := controller.io.ctrl_sa_send_data)

  controller.io.ibh_data_in_done := inBuffer_h.io.ib_data_in_done
  controller.io.ibv_data_in_done := inBuffer_v.io.ib_data_in_done
  controller.io.ob_empty := outBuffer.io.ob_empty
}*/