package utils

import chisel3._
import chisel3.util._

class OverrideableQueue[T <: Data](gen: T, n: Int) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(ValidIO(gen))
    val out = Decoupled(gen)
  })
  val entries = Seq.fill(n){ Reg(gen) }
  val valids = Seq.fill(n){ RegInit(false.B) }
  val rd_ptr = RegInit(0.U(log2Up(n).W))
  val wr_ptr = RegInit(0.U(log2Up(n).W))

  when(io.in.valid){
    wr_ptr := wr_ptr + 1.U
  }
  // Overwrites can leave the read head empty while other slots remain valid.
  val advance_empty_head = !io.out.valid && io.out.ready && valids.reduce(_ || _)
  val next_valid_ptr = Mux1H(
    (0 until n).map(i => i.U === rd_ptr),
    (0 until n).map(i => PriorityMux((1 until n).map(j =>
      valids((i + j) % n) -> ((i + j) % n).U(log2Up(n).W)
    )))
  )
  when(io.out.fire){
    rd_ptr := rd_ptr + 1.U
  }.elsewhen(advance_empty_head){
    rd_ptr := next_valid_ptr
  }

  val w_mask = (0 until n).map(i => i.U === wr_ptr)
  val r_mask = (0 until n).map(i => i.U === rd_ptr)

  for((v, r) <- valids.zip(r_mask)){
    when(r && io.out.fire){
      v := false.B
    }
  }

  for(((v, e), w) <- valids.zip(entries).zip(w_mask)){
    when(io.in.valid && w){
      v := true.B
      e := io.in.bits
    }
  }

  io.out.valid := Mux1H(r_mask, valids)
  io.out.bits := Mux1H(r_mask, entries)
}
