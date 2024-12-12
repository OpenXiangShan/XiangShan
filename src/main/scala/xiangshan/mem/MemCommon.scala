/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.mem


import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import utils._
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, MemExuInput}
import xiangshan.backend.rob.RobPtr
import xiangshan.cache._
import xiangshan.backend.fu.FenceToSbuffer
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.mem.prefetch.PrefetchReqBundle
import math._

object genWmask {
  def apply(addr: UInt, sizeEncode: UInt): UInt = {
    (LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(2:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U, //1111
      "b11".U -> 0xff.U //11111111
    )) << addr(2, 0)).asUInt
  }
}

object genVWmask {
  def apply(addr: UInt, sizeEncode: UInt): UInt = {
    (LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(2:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U, //1111
      "b11".U -> 0xff.U //11111111
    )) << addr(3, 0)).asUInt
  }
}

object genWdata {
  def apply(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(16, data(7, 0)),
      "b01".U -> Fill(8, data(15, 0)),
      "b10".U -> Fill(4, data(31, 0)),
      "b11".U -> Fill(2, data(63,0))
    ))
  }
}

object shiftDataToLow {
  def apply(addr: UInt,data : UInt): UInt = {
    Mux(addr(3), (data >> 64).asUInt,data)
  }
}
object shiftMaskToLow {
  def apply(addr: UInt,mask: UInt): UInt = {
    Mux(addr(3),(mask >> 8).asUInt,mask)
  }
}

object ReplayCauseNO {
  def memoryAmbiguous = 0
  def tlbMiss         = 1
  def forwardFail     = 2
  def dcacheReplay    = 3
  def dcacheMiss      = 4
  def wpuPredictFail  = 5
  def bankConflict    = 6
  def rarNack         = 7
  def rawNack         = 8
  def nuke            = 9

  def priorities = Seq(
    memoryAmbiguous,
    tlbMiss,
    forwardFail,
    dcacheReplay,
    dcacheMiss,
    wpuPredictFail,
    bankConflict,
    rarNack,
    rawNack,
    nuke
  )
  def allCauses = priorities.length
}

object ReplayCauseVec {
  def apply() = Vec(ReplayCauseNO.allCauses, Bool())
  def apply(init: Bool) = VecInit(Seq.fill(ReplayCauseNO.allCauses)(init))
}

object AddPipelineReg {
  class PipelineRegModule[T <: Data](gen: T) extends Module {
    val io = IO(new Bundle() {
      val in = Flipped(DecoupledIO(gen.cloneType))
      val out = DecoupledIO(gen.cloneType)
      val isFlush = Input(Bool())
    })

    val valid = RegInit(false.B)
    valid.suggestName("pipeline_reg_valid")
    when (io.out.fire) { valid := false.B }
    when (io.in.fire) { valid := true.B }
    when (io.isFlush) { valid := false.B }

    io.in.ready := !valid || io.out.ready
    io.out.bits := RegEnable(io.in.bits, io.in.fire)
    io.out.valid := valid //&& !isFlush
  }

  def apply[T <: Data]
  (left: DecoupledIO[T], right: DecoupledIO[T], isFlush: Bool,
   moduleName: Option[String] = None
  ): Unit = {
    val pipelineReg = Module(new PipelineRegModule[T](left.bits.cloneType))
    if(moduleName.nonEmpty) pipelineReg.suggestName(moduleName.get)
    pipelineReg.io.in <> left
    right <> pipelineReg.io.out
    pipelineReg.io.isFlush := isFlush
  }
}

object SelectOldest {
  class SelectOldest[T <: Data, R <: Data](inType: T, selType: R, numIn: Int, fn: (R, R) => Bool) extends Module {
    val io = IO(new Bundle() {
      val in = Flipped(Vec(numIn, ValidIO(chiselTypeOf(inType))))
      val sel = Input(Vec(numIn, chiselTypeOf(selType)))
      val oldest = ValidIO(chiselTypeOf(inType))
    })

    private def findOldest(ins: Seq[(Bool, (T, R))]): Seq[(Bool, (T, R))] = {
      ins.length match {
        case 0 | 1  => ins
        case 2      =>
          val (left, right) = (ins.head, ins.last)
          val oldest = MuxT(left._1 && right._1,
                        MuxT(fn(left._2._2, right._2._2), left._2, right._2),
                          MuxT(left._1 && !right._1, left._2, right._2))
          Seq((left._1 || right._1, oldest))
        case _      =>
          val left = findOldest(ins.take(ins.length/2))
          val right = findOldest(ins.drop(ins.length/2))
          findOldest(left ++ right)
      }
    }

    val oldest = findOldest(io.in.zip(io.sel).map(x => (x._1.valid, (x._1.bits, x._2))))
    io.oldest.valid := oldest.head._1
    io.oldest.bits  := oldest.head._2._1
  }

  /**
   * `SelectOldest` is a module designed to select the "oldest" element from an
   * input sequence.
   *
   * @param inType The data type of the input elements.
   * @param selType The data type of the selection criterion.
   * @param numIn The number of input elements.
   * @param fn A comparison function to determine the "oldest" element based on the selection criterion.
   */

  def apply[T <: Data, R <: Data](
    ins:  Vec[ValidIO[T]],
    sels: Vec[R],
    fn:   (R, R) => Bool,
    moduleName: Option[String] = None
  ): ValidIO[T] = {
    def inType = ins.head.bits.cloneType
    def selType = sels.head.cloneType

    val selector = Module(new SelectOldest(inType, selType, ins.length, fn)).suggestName(moduleName.getOrElse("SelectOldest"))
    selector.io.in  <> ins
    selector.io.sel <> sels
    selector.io.oldest
  }
}
