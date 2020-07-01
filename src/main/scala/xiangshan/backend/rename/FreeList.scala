package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._

trait HasFreeListConsts extends HasXSParameter {
  def FL_SIZE: Int = NRPhyRegs-32
  def PTR_WIDTH = log2Up(FL_SIZE)
}

class FreeListPtr extends Bundle with HasFreeListConsts {

  val flag = Bool()
  val value = UInt(PTR_WIDTH.W)

  final def +(inc: Bool): FreeListPtr = {
    Mux(inc && (value ===  (FL_SIZE-1).U),
      FreeListPtr(!flag, 0.U),
      FreeListPtr(flag, value + inc)
    )
  }

  final def ===(that: FreeListPtr): Bool = {
    (this.value===that.value) && (this.flag===that.flag)
  }
}

object FreeListPtr {
  def apply(f: Bool, v:UInt): FreeListPtr = {
    val ptr = Wire(new FreeListPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class FreeList extends XSModule with HasFreeListConsts {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))

    // alloc new phy regs
    val allocReqs = Input(Vec(RenameWidth, Bool()))
    val pdests = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    val allocPtrs = Output(Vec(RenameWidth, new FreeListPtr))
    val canAlloc = Output(Vec(RenameWidth, Bool()))

    // dealloc phy regs
    val deallocReqs = Input(Vec(CommitWidth, Bool()))
    val deallocPregs = Input(Vec(CommitWidth, UInt(PhyRegIdxWidth.W)))
  })

  // init: [32, 127]
  val freeList = RegInit(VecInit(Seq.tabulate(FL_SIZE)(i => (i+32).U(PhyRegIdxWidth.W))))
  val headPtr = RegInit(FreeListPtr(false.B, 0.U))
  val tailPtr = RegInit(FreeListPtr(true.B, 0.U))

  def isEmpty(ptr1: FreeListPtr, ptr2: FreeListPtr): Bool = ptr1===ptr2

  // dealloc: commited instructions's 'old_pdest' enqueue
  var tailPtrNext = WireInit(tailPtr)
  for((deallocValid, deallocReg) <- io.deallocReqs.zip(io.deallocPregs)){
    when(deallocValid){
      freeList(tailPtrNext.value) := deallocReg
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
    pdest := freeList(headPtrNext.value)
    headPtrNext = headPtrNext + (allocReq && canAlloc)
    allocPtr := headPtrNext
    empty = isEmpty(headPtrNext, tailPtr)
  }

  headPtr := Mux(io.redirect.valid,
    io.redirect.bits.freelistAllocPtr, // mispredict or exception happen
    headPtrNext
  )

}
