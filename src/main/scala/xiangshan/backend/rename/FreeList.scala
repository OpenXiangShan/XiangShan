package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._

class FreeList extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))

    // alloc new phy regs
    val allocReqs = Input(Vec(RenameWidth, Bool()))
    val pdests = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    val allocPtrs = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    val canAlloc = Output(Vec(RenameWidth, Bool()))

    // dealloc phy regs
    val deallocReqs = Input(Vec(CommitWidth, Bool()))
    val deallocPregs = Input(Vec(CommitWidth, UInt(PhyRegIdxWidth.W)))
  })

  val freeList = RegInit(VecInit(Seq.tabulate(NRPhyRegs)(i => i.U(PhyRegIdxWidth.W))))
  val headPtr, tailPtr = RegInit(0.U((PhyRegIdxWidth+1).W))

  def ptrToIndex(ptr: UInt): UInt = ptr.tail(1)
  def isEmpty(ptr1: UInt, ptr2: UInt): Bool = ptr1 === ptr2

  // dealloc: commited instructions's 'old_pdest' enqueue
  var tailPtrNext = WireInit(tailPtr)
  for((deallocValid, deallocReg) <- io.deallocReqs.zip(io.deallocPregs)){
    when(deallocValid){
      freeList(ptrToIndex(tailPtrNext)) := deallocReg
    }
    tailPtrNext = tailPtrNext + deallocValid
  }
  tailPtr := tailPtrNext

  // allocate new pregs to rename instructions
  var empty = WireInit(isEmpty(headPtr, tailPtr))
  var headPtrNext = WireInit(headPtr)
  for(
    (((allocReq, canAlloc),pdest),allocPtr) <- io.allocReqs.zip(io.canAlloc).zip(io.pdests).zip(io.allocPtrs)
  ){
    canAlloc := !empty
    pdest := freeList(ptrToIndex(headPtrNext))
    allocPtr := headPtrNext
    headPtrNext = headPtrNext + (allocReq && canAlloc)
    empty = isEmpty(headPtrNext, tailPtr)
  }

  headPtr := Mux(io.redirect.valid,
    io.redirect.bits.freelistAllocPtr, // mispredict or exception happen
    headPtrNext
  )

}
