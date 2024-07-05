package matu.DataBuffer

import chisel3._
import chisel3.util._


class SAInputBuffer(val IN_WIDTH: Int, val QUEUE_NUM: Int, val QUEUE_LEN: Int) extends Module {
  val io = IO(new Bundle {
    val ctrl_ib_data_out = Input(Bool())
    val data_in = Flipped(DecoupledIO(Vec(QUEUE_NUM, Vec(QUEUE_LEN, SInt(IN_WIDTH.W)))))

    val data_out = Output(Vec(QUEUE_NUM, SInt(IN_WIDTH.W)))
    val ib_data_in_done = Output(Bool())
  })

  val data_queue = Seq.fill(QUEUE_NUM)(Module(new MultiWritePortFIFO(IN_WIDTH, QUEUE_LEN)))

  // when delay_count count to 0, queue start to output data
  val delay_count = Reg(Vec(QUEUE_NUM, UInt(log2Ceil(QUEUE_NUM).W)))

  val ib_data_in_done = WireInit(false.B)
  io.ib_data_in_done := ib_data_in_done

  val allFull = WireInit(false.B)
  val allEmpty = WireInit(false.B)
  allFull := data_queue.map(_.io.full).reduce(_ & _)
  allEmpty := data_queue.map(_.io.empty).reduce(_ & _)

  val prepare :: data_out :: Nil = Enum(2)
  val state = RegInit(prepare)

  for (i <- 0 until QUEUE_NUM) {
    data_queue(i).io.enq := io.data_in.fire
    data_queue(i).io.deq := state === data_out && delay_count(i) === 0.U && !data_queue(i).io.empty
    data_queue(i).io.enqData <> io.data_in.bits(i)
    io.data_out(i) := data_queue(i).io.deqData
  }
  io.data_in.ready := state === prepare

  when(state === prepare) {
    when(allFull) {
      ib_data_in_done := true.B
      state := Mux(io.ctrl_ib_data_out, data_out, prepare)
    }
    for (i <- 0 until QUEUE_NUM) {
      delay_count(i) := i.U
    }
  }.elsewhen(state === data_out) {
    when(allEmpty) {
      state := prepare
    }
    for (i <- 0 until QUEUE_NUM) {
      delay_count(i) := Mux(delay_count(i) =/= 0.U, delay_count(i) - 1.U, 0.U)
    }
  }

}
