package utils

import chisel3._
import chisel3.util._

class LockBundle extends Bundle {
  val lock = Input(Bool())
  val unlock = Input(Bool())
  val holding = Output(Bool())
}

class Lock(n: Int) extends Module {
  val io = IO(new Bundle {
    val bundle = Vec(n, new LockBundle)
  })

  val lock = RegInit(0.U(n.W))
  val lockReq = VecInit(io.bundle.map(_.lock)).asUInt
  val unlockReq = VecInit(io.bundle.map(_.unlock)).asUInt

  val lockEmpty = lock === 0.U
  val hasLockReq = lockReq =/= 0.U
  val lockNext = 1.U << PriorityEncoder(lockReq)
  when (lockEmpty && hasLockReq) { lock := lockNext }

  val hasUnlockReq = unlockReq =/= 0.U
  assert(PopCount(unlockReq) <= 1.U, "only the lock holder can issue unlock request")
  assert(!(lockEmpty && hasUnlockReq), "only the lock holder can issue unlock request")
  assert((lock & lockReq) === 0.U, "can not issue lock request when holding the lock")
  when (!lockEmpty && hasUnlockReq) {
    assert(unlockReq === lock, "only the lock holder can issue unlock request")
    lock := 0.U
  }

  val holding = Mux(lockEmpty && hasLockReq, lockNext, lock)
  io.bundle.map(_.holding).zip(holding.asBools).map{ case (l, r) => l := r }
  assert(PopCount(io.bundle.map(_.holding)) <= 1.U, "there should be only one lock holder")
}
