package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils.{XSDebug}

class DispatchGen extends XSModule {
  val io = IO(new Bundle() {
    // from dispatch queues
    val fromIntDq = Flipped(Vec(IntDqDeqWidth, ValidIO(new MicroOp)))
    val fromFpDq = Flipped(Vec(FpDqDeqWidth, ValidIO(new MicroOp)))
    val fromLsDq = Flipped(Vec(LsDqDeqWidth, ValidIO(new MicroOp)))

    // enq Issue Queue
    val numExist = Input(Vec(exuParameters.ExuCnt, UInt(log2Ceil(IssQueSize).W)))
    val enqIQIndex = Vec(exuParameters.ExuCnt, ValidIO(UInt(log2Ceil(IntDqDeqWidth).W)))
  })

  assert(IntDqDeqWidth >= FpDqDeqWidth)
  assert(IntDqDeqWidth >= LsDqDeqWidth)

  def PriorityGen(numExist: Seq[UInt]) = {
    assert(numExist.length > 1)
    val sortedIndex = Wire(Vec(numExist.length, UInt(log2Ceil(numExist.length).W)))
    val priority = WireInit(VecInit(Seq.tabulate(numExist.length)(i => 0.U(log2Ceil(numExist.length).W))))
    for (i <- 0 until numExist.length) {
      sortedIndex(i) := PriorityEncoder((0 until numExist.length).map(each => {
        // itself should not be found yet
        val equalPrevious = (if (i == 0) false.B else Cat((0 until i).map(l => each.U === sortedIndex(l))).orR())
        val largerThanAnyOther = Cat((0 until numExist.length).map(another => {
          // no need to be compared with the larger ones
          val anotherEqualPrevious = (if (i == 0) false.B else Cat((0 until i).map(l => another.U === sortedIndex(l))).orR())
          // need to be no smaller than any other numbers except the previoud found larger ones
          (numExist(each) <= numExist(another)) || anotherEqualPrevious
        })).andR()
        largerThanAnyOther && !equalPrevious
      }))
      priority(sortedIndex(i)) := i.U
    }
    for (i <- 0 until numExist.length) {
      XSDebug(p"priority: data($i) = ${numExist(i)}, priority = ${priority(i)}\n")
    }
    priority
  }

  def genIQIndex(exunum: Int, deqnum: Int, deq: Seq[Bool], numExist: Seq[UInt]) = {
    assert(isPow2(deqnum))
    assert(exunum == numExist.length)
    // index without priority
    val IQIndex = Wire(Vec(exunum, UInt((log2Ceil(deqnum) + 1).W)))
    var last_deq = deq
    for (i <- 0 until exunum) {
      IQIndex(i) := PriorityEncoder(last_deq :+ true.B)
      val onehot = UIntToOH(IQIndex(i))
      last_deq = (0 until deqnum).map(i => !onehot(i) && last_deq(i))
    }
    // now consider the IQ priority with numExist
    val priority = (if (numExist.length > 1) PriorityGen(numExist) else Seq(0.U))
    (0 until exunum).map(i => IQIndex(priority(i)))
  }

  val bruIQIndex = genIQIndex(exuParameters.JmpCnt, IntDqDeqWidth, io.fromIntDq.map(_.bits.ctrl.fuType === FuType.jmp),
    (0 until exuParameters.JmpCnt).map(i => io.numExist(i)))
  val aluIQIndex = genIQIndex(exuParameters.AluCnt, IntDqDeqWidth, io.fromIntDq.map(_.bits.ctrl.fuType === FuType.alu),
    (0 until exuParameters.AluCnt).map(i => io.numExist(exuParameters.JmpCnt+i)))
  val mulIQIndex = genIQIndex(exuParameters.MulCnt, IntDqDeqWidth, io.fromIntDq.map(_.bits.ctrl.fuType === FuType.mul),
    (0 until exuParameters.MulCnt).map(i => io.numExist(exuParameters.JmpCnt+exuParameters.AluCnt+i)))
  val muldivIQIndex = genIQIndex(exuParameters.MduCnt, IntDqDeqWidth, io.fromIntDq.map(_.bits.ctrl.fuType === FuType.div),
    (0 until exuParameters.MduCnt).map(i => io.numExist(exuParameters.JmpCnt+exuParameters.AluCnt+exuParameters.MulCnt+i)))
  val fmacIQIndex = genIQIndex(exuParameters.FmacCnt, FpDqDeqWidth, io.fromFpDq.map(_.bits.ctrl.fuType === FuType.fmac),
    (0 until exuParameters.FmacCnt).map(i => io.numExist(exuParameters.IntExuCnt+i)))
  val fmiscIQIndex = genIQIndex(exuParameters.FmiscCnt, FpDqDeqWidth, io.fromFpDq.map(_.bits.ctrl.fuType === FuType.fmisc),
    (0 until exuParameters.FmiscCnt).map(i => io.numExist(exuParameters.IntExuCnt+exuParameters.FmacCnt+i)))
  val lduIQIndex = genIQIndex(exuParameters.LduCnt, LsDqDeqWidth, io.fromLsDq.map(_.bits.ctrl.fuType === FuType.ldu),
    (0 until exuParameters.LduCnt).map(i => io.numExist(exuParameters.IntExuCnt+exuParameters.FpExuCnt+i)))
//  val stuIQIndex = genIQIndex(exuParameters.StuCnt, LsDqDeqWidth, io.fromLsDq.map(_.bits.ctrl.fuType === FuType.stu))
  val stuIQIndex = genIQIndex(exuParameters.StuCnt, LsDqDeqWidth, io.fromLsDq.map(deq => FuType.isMemExu(deq.bits.ctrl.fuType)),
    (0 until exuParameters.StuCnt).map(i => io.numExist(exuParameters.IntExuCnt+exuParameters.FpExuCnt+exuParameters.LduCnt+i)))

  val allIndex = Seq(bruIQIndex, aluIQIndex, mulIQIndex, muldivIQIndex,
    fmacIQIndex, fmiscIQIndex,
    lduIQIndex, stuIQIndex
  )
  val allCnt = Seq(exuParameters.JmpCnt, exuParameters.AluCnt, exuParameters.MulCnt, exuParameters.MduCnt,
    exuParameters.FmacCnt, exuParameters.FmiscCnt,
    exuParameters.LduCnt, exuParameters.StuCnt
  )
  assert(allIndex.length == allCnt.length)
  var startIndex = 0
  for (i <- 0 until allIndex.length) {
    for (j <- 0 until allCnt(i)) {
      io.enqIQIndex(startIndex + j).valid := !allIndex(i)(j)(2)
      io.enqIQIndex(startIndex + j).bits := allIndex(i)(j)(1, 0)
    }
    startIndex += allCnt(i)
  }
}
