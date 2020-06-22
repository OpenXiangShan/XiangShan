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
  val headPtr = RegInit(0.U(PhyRegIdxWidth.W))
  val tailPtr = RegInit(0.U(PhyRegIdxWidth.W))
  val full = RegInit(true.B)

  // dealloc: commited instructions's 'old_pdest' enqueue
  var tailPtrNext = WireInit(tailPtr)
  for((deallocValid, deallocReg) <- io.deallocReqs.zip(io.deallocPregs)){
    when(deallocValid){
      freeList(tailPtrNext) := deallocReg
    }
    tailPtrNext = tailPtrNext + deallocValid
  }
  tailPtr := tailPtrNext

  // allocate new pregs to rename instructions
  var empty = WireInit(!full && (headPtr === tailPtr))
  var headPtrNext = WireInit(headPtr)
  for(
    (((allocReq, canAlloc),pdest),allocPtr) <- io.allocReqs.zip(io.canAlloc).zip(io.pdests).zip(io.allocPtrs)
  ){
    canAlloc := !empty
    pdest := freeList(headPtrNext)
    allocPtr := headPtrNext
    headPtrNext = headPtrNext + (allocReq && canAlloc)
    empty = empty && (headPtrNext === tailPtr)
  }

  full := !empty && (headPtrNext === tailPtrNext)

  headPtr := Mux(io.redirect.valid,
    io.redirect.bits.freelistAllocPtr, // mispredict or exception happen
    headPtrNext
  )

}
