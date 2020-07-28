package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class IndexMapping(inWidth: Int, outWidth: Int) extends XSModule {
  val io = IO(new Bundle() {
    val validBits = Input(Vec(inWidth, Bool()))
    val mapping = Output(Vec(outWidth, ValidIO(UInt(log2Ceil(inWidth).W))))
    val reverseMapping = Output(Vec(inWidth, ValidIO(UInt(log2Ceil(outWidth).W))))
  })

  for (j <- 0 until inWidth) {
    io.reverseMapping(j).valid := false.B
    io.reverseMapping(j).bits := DontCare
  }

  var maskedValidBits = (0 until inWidth).map(i => io.validBits(i))
  for (i <- 0 until outWidth) {
    val onehot = PriorityEncoderOH(maskedValidBits)
    io.mapping(i).valid := Cat(onehot).orR()
    io.mapping(i).bits := OHToUInt(onehot)
    maskedValidBits = (0 until inWidth).map(i => maskedValidBits(i) && !onehot(i))
    for (j <- 0 until inWidth) {
      when (io.mapping(i).valid && io.mapping(i).bits === j.U) {
        io.reverseMapping(i).valid := true.B
        io.reverseMapping(i).bits := i.U
      }
    }
  }

}

object PriorityGen {
  def apply(numExist: Seq[UInt]) = {
    assert(numExist.length > 1)
    val sortedIndex = Wire(Vec(numExist.length, UInt(log2Ceil(numExist.length).W)))
    val priority = WireInit(VecInit(Seq.tabulate(numExist.length)(_ => 0.U(log2Ceil(numExist.length).W))))
    for (i <- numExist.indices) {
      sortedIndex(i) := PriorityEncoder(numExist.indices.map(each => {
        // itself should not be found yet
        val equalPrevious = (if (i == 0) false.B else Cat((0 until i).map(l => each.U === sortedIndex(l))).orR())
        val largerThanAnyOther = Cat(numExist.indices.map(another => {
          // no need to be compared with the larger ones
          val anotherEqualPrevious = (if (i == 0) false.B else Cat((0 until i).map(l => another.U === sortedIndex(l))).orR())
          // need to be no smaller than any other numbers except the previoud found larger ones
          (numExist(each) <= numExist(another)) || anotherEqualPrevious
        })).andR()
        largerThanAnyOther && !equalPrevious
      }))
      priority(sortedIndex(i)) := i.U
    }
    priority
  }
}