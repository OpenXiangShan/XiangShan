package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._
import utils.{CircularQueuePtr, HasCircularQueuePtrHelper, XSDebug}
import xiangshan.backend.brq.BrqPtr

trait HasFreeListConsts extends HasXSParameter {
  def FL_SIZE: Int = NRPhyRegs-32
  def PTR_WIDTH = log2Up(FL_SIZE)
}

class FreeListPtr extends CircularQueuePtr(FreeListPtr.FL_SIZE) {

//  final def ===(that: FreeListPtr): Bool = {
//    (this.value===that.value) && (this.flag===that.flag)
//  }

//  override def toPrintable: Printable = {
//    p"$flag:$value"
//  }

}

object FreeListPtr extends HasFreeListConsts {
  def apply(f: Bool, v:UInt): FreeListPtr = {
    val ptr = Wire(new FreeListPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class FreeList extends XSModule with HasFreeListConsts with HasCircularQueuePtrHelper{
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

  // dealloc: commited instructions's 'old_pdest' enqueue
  for(i <- 0 until CommitWidth){
    val offset = if(i == 0) 0.U else PopCount(io.deallocReqs.take(i))
    val ptr = tailPtr + offset
    val idx = ptr.value
    when(io.deallocReqs(i)){
      freeList(idx) := io.deallocPregs(i)
      XSDebug(p"dealloc preg: ${io.deallocPregs(i)}\n")
    }
  }
  val tailPtrNext = tailPtr + PopCount(io.deallocReqs)
  tailPtr := tailPtrNext

  // allocate new pregs to rename instructions

  // number of free regs in freelist
  val freeRegs = Wire(UInt())
  // use RegNext for better timing
  val hasEnoughRegs = RegNext(freeRegs >= RenameWidth.U, true.B)
  XSDebug(p"free regs: $freeRegs\n")


  val newHeadPtrs = ((0 until RenameWidth) map {i =>
    if(i == 0) headPtr else headPtr + PopCount(io.allocReqs.take(i))
  }) :+ (headPtr + PopCount(io.allocReqs))

  for(i <- 0 until RenameWidth){
    val ptr = newHeadPtrs(i)
    val idx = ptr.value
    io.canAlloc(i) := hasEnoughRegs
    io.pdests(i) := freeList(idx)
    when(io.cpReqs(i).valid){
      checkPoints(io.cpReqs(i).bits.value) := newHeadPtrs(i+1)
      XSDebug(p"do checkPt at BrqIdx=${io.cpReqs(i).bits.value} ${newHeadPtrs(i+1)}\n")
    }
    XSDebug(p"req:${io.allocReqs(i)} canAlloc:$hasEnoughRegs pdest:${io.pdests(i)}\n")
  }
  val headPtrNext = Mux(hasEnoughRegs, newHeadPtrs.last, headPtr)
  freeRegs := distanceBetween(tailPtr, headPtrNext)

  headPtr := Mux(io.redirect.valid, // mispredict or exception happen
    Mux(io.redirect.bits.isException || io.redirect.bits.isFlushPipe, // TODO: need check by JiaWei
      FreeListPtr(!tailPtrNext.flag, tailPtrNext.value),
      Mux(io.redirect.bits.isMisPred,
        checkPoints(io.redirect.bits.brTag.value),
        headPtrNext // replay
      )
    ),
    headPtrNext
  )

  XSDebug(p"head:$headPtr tail:$tailPtr\n")

  XSDebug(io.redirect.valid, p"redirect: brqIdx=${io.redirect.bits.brTag.value}\n")

  val enableFreelistCheck = false
  if(env.EnableDebug && enableFreelistCheck){
    for( i <- 0 until FL_SIZE){
      for(j <- i+1 until FL_SIZE){
      assert(freeList(i) != freeList(j), s"Found same entry in freelist! (i=$i j=$j)")
      }
    }
  }

}
