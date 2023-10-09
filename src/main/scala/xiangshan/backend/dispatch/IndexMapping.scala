/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.dispatch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._

class IndexMapping(inWidth: Int, outWidth: Int, withPriority: Boolean)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val validBits = Vec(inWidth, Input(Bool()))
    val priority = Vec(outWidth, Input(UInt(log2Ceil(outWidth).W)))
    val mapping = Vec(outWidth, ValidIO(UInt(log2Ceil(inWidth).W)))
    val reverseMapping = Vec(inWidth, ValidIO(UInt(log2Ceil(outWidth).W)))
  })

  // find the ones in vector (assumed the vector is not one-hot)
  def get_ones(vec: Vec[Bool], num: Int, zeros: Int = 0) : (Bool, UInt) = {
    val vecLeft = vec.drop(zeros)
    val maskedVec = VecInit(Seq.fill(zeros)(false.B) ++ vecLeft)
    if (num == 1) {
      (Cat(vecLeft).orR, PriorityEncoder(maskedVec))
    }
    else if (num + zeros == vec.size) {
      (Cat(vecLeft).andR, (vec.size - 1).U)
    }
    else {
      val tail_minus_1 = get_ones(vec, num - 1, zeros + 1)
      val tail_orig = get_ones(vec, num, zeros + 1)
      val valid = (tail_minus_1._1 && vec(zeros)) || tail_orig._1
      val index = Mux(vec(zeros), tail_minus_1._2, tail_orig._2)
      (valid, index)
    }
  }

  for (j <- 0 until inWidth) {
    io.reverseMapping(j).valid := false.B
    io.reverseMapping(j).bits := DontCare
  }

  val unsortedMapping = Wire(Vec(outWidth, UInt(log2Ceil(inWidth).W)))
  val unsortedValid = Wire(Vec(outWidth, Bool()))
  for (i <- 0 until outWidth) {
    val (valid, map) = get_ones(io.validBits, i + 1)
    unsortedValid(i) := valid
    unsortedMapping(i) := map

    val index = if (withPriority) io.priority(i) else i.U
    io.mapping(i).valid := unsortedValid(index)
    io.mapping(i).bits := unsortedMapping(index)

    for (j <- 0 until inWidth) {
      when (io.mapping(i).valid && io.mapping(i).bits === j.U) {
        io.reverseMapping(j).valid := true.B
        io.reverseMapping(j).bits := i.U
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
        val equalPrevious = (if (i == 0) false.B else Cat((0 until i).map(l => each.U === sortedIndex(l))).orR)
        val largerThanAnyOther = Cat(numExist.indices.map(another => {
          // no need to be compared with the larger ones
          val anotherEqualPrevious = (if (i == 0) false.B else Cat((0 until i).map(l => another.U === sortedIndex(l))).orR)
          // need to be no smaller than any other numbers except the previoud found larger ones
          (numExist(each) <= numExist(another)) || anotherEqualPrevious
        })).andR
        largerThanAnyOther && !equalPrevious
      }))
      priority(sortedIndex(i)) := i.U
    }
    (priority, sortedIndex)
  }
}

object RegfileReadPortGen {
  def apply(staticMappedValid: Seq[Bool], dynamicMappedValid: Seq[Bool]) = {
    val choiceCount = dynamicMappedValid.length + 1
    val readPortSrc = Wire(Vec(staticMappedValid.length, UInt(log2Ceil(choiceCount).W)))
    var hasAssigned = (0 until choiceCount).map(_ => false.B)
    for (i <- staticMappedValid.indices) {
      val valid = staticMappedValid(i) +: dynamicMappedValid
      val wantReadPort = (0 until choiceCount).map(j => valid(j) && ((j == 0).asBool || !hasAssigned(j)))
      readPortSrc(i) := PriorityEncoder(wantReadPort)
      val onehot = UIntToOH(readPortSrc(i))
      hasAssigned = (0 until choiceCount).map(i => hasAssigned(i) || onehot(i))
    }
    val dynamicExuSrc = Wire(Vec(dynamicMappedValid.length, UInt(log2Ceil(staticMappedValid.length).W)))
    for (i <- dynamicMappedValid.indices) {
      val targetMatch = staticMappedValid.indices.map(j => readPortSrc(j) === (i + 1).U)
      dynamicExuSrc(i) := PriorityEncoder(targetMatch)
    }
    (readPortSrc, dynamicExuSrc)
  }
}
