package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._
import utils.XSDebug
import xiangshan.backend.brq.BrqPtr

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

  override def toPrintable: Printable = {
    p"$flag:$value"
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
    val canAlloc = Output(Vec(RenameWidth, Bool()))

    // do checkpoints
    val cpReqs = Vec(RenameWidth, Flipped(ValidIO(new BrqPtr)))

    // dealloc phy regs
    val deallocReqs = Input(Vec(CommitWidth, Bool()))
    val deallocPregs = Input(Vec(CommitWidth, UInt(PhyRegIdxWidth.W)))
  })

  // init: [32, 127]
  val freeList = RegInit(VecInit(Seq.tabulate(FL_SIZE)(i => (i+32).U(PhyRegIdxWidth.W))))
  val headPtr = RegInit(FreeListPtr(false.B, 0.U))
  val tailPtr = RegInit(FreeListPtr(true.B, 0.U))

  val checkPoints = Reg(Vec(BrqSize, new FreeListPtr()))

  def isEmpty(ptr1: FreeListPtr, ptr2: FreeListPtr): Bool = ptr1===ptr2

  // dealloc: commited instructions's 'old_pdest' enqueue
  var tailPtrNext = WireInit(tailPtr)
  for((deallocValid, deallocReg) <- io.deallocReqs.zip(io.deallocPregs)){
    when(deallocValid){
      freeList(tailPtrNext.value) := deallocReg
      XSDebug(p"dealloc preg: $deallocReg\n")
    }
    tailPtrNext = tailPtrNext + deallocValid
  }
  tailPtr := tailPtrNext

  // allocate new pregs to rename instructions
  var empty = WireInit(isEmpty(headPtr, tailPtr))
  var headPtrNext = WireInit(headPtr)
  for(
    (((allocReq, canAlloc),pdest),cpReq) <-
    io.allocReqs.zip(io.canAlloc).zip(io.pdests).zip(io.cpReqs)
  ){
    canAlloc := !empty
    pdest := freeList(headPtrNext.value)
    headPtrNext = headPtrNext + (allocReq && canAlloc)
    when(cpReq.valid){
      checkPoints(cpReq.bits.value) := headPtrNext
      XSDebug(p"do checkPt at BrqIdx=${cpReq.bits.value} headPtr:$headPtrNext\n")
    }
    empty = isEmpty(headPtrNext, tailPtr)
    XSDebug(p"req:$allocReq canAlloc:$canAlloc pdest:$pdest headNext:$headPtrNext\n")
  }

  headPtr := Mux(io.redirect.valid, // mispredict or exception happen
    Mux(io.redirect.bits.isException,
      FreeListPtr(!tailPtr.flag, tailPtr.value),
      Mux(io.redirect.bits.isMisPred,
        checkPoints(io.redirect.bits.brTag.value),
        headPtrNext // replay
      )
    ),
    headPtrNext
  )

  XSDebug(p"head:$headPtr tail:$tailPtr\n")

  XSDebug(io.redirect.valid, p"redirect: brqIdx=${io.redirect.bits.brTag.value}\n")



}
