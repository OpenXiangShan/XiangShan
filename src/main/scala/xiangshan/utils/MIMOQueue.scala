package xiangshan.utils

import chisel3._
import chisel3.experimental.{DataMirror, requireIsChiselType}
import chisel3.util._

class IndexableMem[T <: Data]
(
  entries: Int,
  gen: T,
  mem: Boolean,
  init: Option[Seq[T]]
){
  require(!(init.nonEmpty && init.get.size!=entries))
  val ram = Mem(entries, gen)
  val vec = Reg(Vec(entries, gen))
  val initializedVec = if(init.nonEmpty) RegInit(VecInit(init.get)) else null
  def apply(idx: UInt): T = {
    if(mem) ram(idx)
    else if(init.nonEmpty) initializedVec(idx)
    else vec(idx)
  }
}

object IndexableMem {
  def apply[T <: Data]
  (
    entries: Int,
    gen: T,
    mem: Boolean = false,
    init: Option[Seq[T]] = None
  ): IndexableMem[T] = {
    new IndexableMem[T](entries, gen, mem, init)
  }
}

class MIMOQueueIO[T <: Data](gen: T, entries: Int, inCnt: Int, outCnt: Int) extends Bundle
{
  val flush = Input(Bool())
  val enq = Vec(inCnt, Flipped(DecoupledIO(gen)))
  val deq = Vec(outCnt, DecoupledIO(gen))

  override def cloneType: MIMOQueueIO.this.type =
    new MIMOQueueIO[T](gen, entries, inCnt, outCnt).asInstanceOf[this.type]
}

class MIMOQueue[T <: Data]
(
  gen: T,
  val entries: Int,
  val inCnt: Int,
  val outCnt: Int,
  mem: Boolean = false,
  perf: Boolean = false,
  init: Option[Seq[T]] = None
) extends Module {

  require(isPow2(entries), "MIMOQueue: entries must be a power of 2!")
  require(!(init.nonEmpty && mem), "MIMOQueue: Mem can't be init!")
  require(!(init.nonEmpty && init.get.size!=entries))

  def ptr_width = log2Up(entries)

  val io = IO(new MIMOQueueIO[T](gen, entries, inCnt, outCnt))

  val genType = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }

  val ram = IndexableMem(entries, genType, mem, init)

  val valids = if(perf){
    RegInit(VecInit((0 until entries).map(_ => if(init.nonEmpty) true.B else false.B)))
  } else null

  val enqPtrInitVal = 0.U((ptr_width+1).W)
  val deqPtrInitVal = (if(init.nonEmpty) 1<<ptr_width else 0).U((ptr_width+1).W)

  val enq_ptr = RegInit(enqPtrInitVal)
  val deq_ptr = RegInit(deqPtrInitVal)

  def ptrToIdx(ptr: UInt): UInt = ptr.tail(1)
  def isFull(ptr1: UInt, ptr2: UInt): Bool = (ptr1.head(1)=/=ptr2.head(1)) && (ptr1.tail(1) === ptr2.tail(1))
  def isEmpty(ptr1: UInt, ptr2: UInt): Bool = ptr1 === ptr2


  def genPtrs(init: UInt, vec: Vec[DecoupledIO[T]]) = {
    if(perf){
      vec.indices.map(i => {
        init + PopCount(vec.take(i).map(_.fire()))
      })
    } else {
      val ptrs = vec.map(_ => Wire(UInt((ptr_width+1).W)))
      for(i <- vec.indices){
        ptrs(i) := {if(i==0) init else ptrs(i-1) + vec(i-1).fire()}
      }
      ptrs
    }
  }

  // dequeue
  val deq_ptrs = genPtrs(deq_ptr, io.deq)

  for((deq, deq_ptr_wire) <- io.deq.zip(deq_ptrs)){
    val deq_idx = ptrToIdx(deq_ptr_wire)
    deq.valid := {
      if(perf) valids(deq_idx)
      else !isEmpty(deq_ptr_wire, enq_ptr)
    }
    deq.bits := ram(deq_idx)
    if(perf) when(deq.fire()){ valids(deq_idx) := false.B }
  }

  deq_ptr := Mux(io.flush, deqPtrInitVal, deq_ptrs.last + io.deq(outCnt-1).fire())

  // enqueue
  val enq_ptrs = genPtrs(enq_ptr, io.enq)

  for((enq, enq_ptr_wire) <- io.enq.zip(enq_ptrs)){
    val enq_idx = ptrToIdx(enq_ptr_wire)
    enq.ready := {
      if(perf) !valids(enq_idx)
      else !isFull(enq_ptr_wire, deq_ptr)
    }
    when(enq.fire()){
      ram(enq_idx) := enq.bits
      if(perf){
        valids(enq_idx) := true.B
      }
    }
  }

  enq_ptr := Mux(io.flush, enqPtrInitVal, enq_ptrs.last + io.enq(inCnt-1).fire())

  Debug(){
    val cnt = RegInit((if(init.nonEmpty) entries else 0).U(32.W))
    val enqCnt = PopCount(io.enq.map(_.fire()))
    val deqCnt = PopCount(io.deq.map(_.fire()))
    cnt := cnt + enqCnt - deqCnt
    assert(cnt > deqCnt, "MIMOQueue underflow!")
    assert(cnt + enqCnt < entries.U(32.W), "MIMOQueue overflow!")
    printf(p"cnt: $cnt enqCnt:$enqCnt deqCnt:$deqCnt\n")
  }

}
