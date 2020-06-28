package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan.utils.{XSDebug, XSInfo}
import xiangshan.{MicroOp, Redirect, XSBundle, XSModule}


class DispatchQueueIO(enqnum: Int, deqnum: Int) extends XSBundle {
  val enq = Vec(enqnum, Flipped(DecoupledIO(new MicroOp)))
  val deq = Vec(deqnum, DecoupledIO(new MicroOp))
  val redirect = Flipped(ValidIO(new Redirect))

  override def cloneType: DispatchQueueIO.this.type =
    new DispatchQueueIO(enqnum, deqnum).asInstanceOf[this.type]
}

class DispatchQueue(size: Int, enqnum: Int, deqnum: Int, name: String) extends XSModule {
  val io = IO(new DispatchQueueIO(enqnum, deqnum))
  val index_width = log2Ceil(size)

  // queue data array
  val entries = Reg(Vec(size, new MicroOp))
  val entriesValid = Reg(Vec(size, Bool()))
  val head = RegInit(0.U(index_width.W))
  val tail = RegInit(0.U(index_width.W))
  val enq_index = Wire(Vec(enqnum, UInt(index_width.W)))
  val enq_count = Wire(Vec(enqnum, UInt((index_width + 1).W)))
  val deq_index = Wire(Vec(deqnum, UInt(index_width.W)))
  val head_direction = RegInit(0.U(1.W))
  val tail_direction = RegInit(0.U(1.W))

  val valid_entries = Mux(head_direction === tail_direction, tail - head, size.U + tail - head)
  val empty_entries = size.U - valid_entries

  // check whether valid uops are canceled
  val cancelled = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    cancelled(i) := ((entries(i).brMask & UIntToOH(io.redirect.bits.brTag)) =/= 0.U) && io.redirect.valid
  }

  // calcelled uops should be set to invalid from enqueue input
  // we don't need to compare their brTags here
  for (i <- 0 until enqnum) {
    enq_count(i) := PopCount(io.enq.slice(0, i + 1).map(_.valid))
    enq_index(i) := (tail + enq_count(i) - 1.U) % size.U
    when (io.enq(i).fire()) {
      entries(enq_index(i)) := io.enq(i).bits
      entriesValid(enq_index(i)) := true.B
    }
  }

  for (i <- 0 until deqnum) {
    deq_index(i) := ((head + i.U) % size.U).asUInt()
    when (io.deq(i).fire()) {
      entriesValid(deq_index(i)) := false.B
    }
  }

  // cancel uops currently in the queue
  for (i <- 0 until size) {
    when (cancelled(i) && entriesValid(i)) {
      entriesValid(i) := false.B
    }
    XSInfo(cancelled(i) && entriesValid(i),
      name + ": valid entry(%d)(pc = %x) cancelled with brMask %x brTag %x\n",
      i.U, entries(i).cf.pc, entries(i).brMask, io.redirect.bits.brTag)
  }

  // enqueue
  val num_enq_try = enq_count(enqnum - 1)
  val num_enq = Mux(empty_entries > num_enq_try, num_enq_try, empty_entries)
  (0 until enqnum).map(i => io.enq(i).ready := enq_count(i) <= num_enq)
  tail := (tail + num_enq) % size.U
  tail_direction := ((Cat(0.U(1.W), tail) + num_enq) >= size.U).asUInt() ^ tail_direction

  // dequeue
  val num_deq_try = Mux(valid_entries > deqnum.U, deqnum.U, valid_entries)
  val num_deq_fire = PriorityEncoder((io.deq.zipWithIndex map { case (deq, i) =>
    !deq.fire() && entriesValid(deq_index(i))
  }) :+ true.B)
  val num_deq = Mux(num_deq_try > num_deq_fire, num_deq_fire, num_deq_try)
  for (i <- 0 until deqnum) {
    io.deq(i).bits := entries(deq_index(i))
    // needs to cancel uops trying to dequeue
    io.deq(i).valid := (i.U < num_deq_try) && entriesValid(deq_index(i)) && !cancelled(deq_index(i))
  }
  head := (head + num_deq) % size.U
  head_direction := ((Cat(0.U(1.W), head) + num_deq) >= size.U).asUInt() ^ head_direction

  XSDebug(num_deq > 0.U, name + ": num_deq = %d, head = (%d -> %d)\n",
      num_deq, head, (head + num_deq) % size.U)
  XSDebug(num_enq > 0.U, name + ": num_enq = %d, tail = (%d -> %d)\n",
      num_enq, tail, (tail + num_enq) % size.U)
  XSDebug(valid_entries > 0.U, name + ": valid_entries = %d, head = (%d, %d), tail = (%d, %d), \n",
      valid_entries, head_direction, head, tail_direction, tail)
}
