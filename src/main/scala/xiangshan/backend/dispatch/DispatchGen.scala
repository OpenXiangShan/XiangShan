package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.regfile.RfReadPort
import xiangshan.utils.{XSDebug, XSInfo}

class DispatchGen extends XSModule {
  val io = IO(new Bundle() {
    // from dispatch queues
    val fromIntDq = Flipped(Vec(IntDqDeqWidth, ValidIO(new MicroOp)))
    val fromFpDq = Flipped(Vec(FpDqDeqWidth, ValidIO(new MicroOp)))
    val fromLsDq = Flipped(Vec(LsDqDeqWidth, ValidIO(new MicroOp)))

    // enq Issue Queue
    val emptySlots = Input(Vec(exuConfig.ExuCnt, UInt(3.W)))//TODO
    val enqIQIndex = Vec(exuConfig.ExuCnt, ValidIO(UInt(2.W)))//TODO
  })

  def genIQIndex(exunum: Int, deqnum: Int, deq: Seq[Bool]) = {
    assert(isPow2(deqnum))
    val IQIndex = Wire(Vec(exunum, UInt((log2Ceil(deqnum) + 1).W)))
    var last_deq = deq
    for (i <- 0 until exunum) {
      IQIndex(i) := PriorityEncoder(last_deq :+ true.B)
      val onehot = UIntToOH(IQIndex(i))
      last_deq = (0 until deqnum).map(i => !onehot(i) && last_deq(i))
    }
    IQIndex
  }

  val bruIQIndex = genIQIndex(exuConfig.BruCnt, IntDqDeqWidth, io.fromIntDq.map(_.bits.ctrl.fuType === FuType.bru))
  val aluIQIndex = genIQIndex(exuConfig.AluCnt, IntDqDeqWidth, io.fromIntDq.map(_.bits.ctrl.fuType === FuType.alu))
  val mulIQIndex = genIQIndex(exuConfig.MulCnt, IntDqDeqWidth, io.fromIntDq.map(_.bits.ctrl.fuType === FuType.mul))
  val muldivIQIndex = genIQIndex(exuConfig.MduCnt, IntDqDeqWidth, io.fromIntDq.map(_.bits.ctrl.fuType === FuType.mdu))
  val fmacIQIndex = genIQIndex(exuConfig.FmacCnt, FpDqDeqWidth, io.fromFpDq.map(_.bits.ctrl.fuType === FuType.fmac))
  val fmiscIQIndex = genIQIndex(exuConfig.FmiscCnt, FpDqDeqWidth, io.fromFpDq.map(_.bits.ctrl.fuType === FuType.fmisc))
  val lduIQIndex = genIQIndex(exuConfig.LduCnt, LsDqDeqWidth, io.fromLsDq.map(_.bits.ctrl.fuType === FuType.ldu))
//  val stuIQIndex = genIQIndex(exuConfig.StuCnt, LsDqDeqWidth, io.fromLsDq.map(_.bits.ctrl.fuType === FuType.stu))
  val stuIQIndex = genIQIndex(exuConfig.StuCnt, LsDqDeqWidth, io.fromLsDq.map(deq => FuType.isMemExu(deq.bits.ctrl.fuType)))

  val allIndex = Seq(bruIQIndex, aluIQIndex, mulIQIndex, muldivIQIndex,
    fmacIQIndex, fmiscIQIndex,
    lduIQIndex, stuIQIndex
  )
  val allCnt = Seq(exuConfig.BruCnt, exuConfig.AluCnt, exuConfig.MulCnt, exuConfig.MduCnt,
    exuConfig.FmacCnt, exuConfig.FmiscCnt,
    exuConfig.LduCnt, exuConfig.StuCnt
  )
  assert(allIndex.length == allCnt.length)
  var startIndex = 0
  for (i <- 0 until allIndex.length) {
    for (j <- 0 until allCnt(i)) {
      println(i, j, allIndex.length)
      io.enqIQIndex(startIndex + j).valid := !allIndex(i)(j)(2)
      io.enqIQIndex(startIndex + j).bits := allIndex(i)(j)(1, 0)
      XSDebug(p"$i $j ${startIndex + j}: ${allIndex(i)(j)}\n")
    }
    startIndex += allCnt(i)
  }
}
