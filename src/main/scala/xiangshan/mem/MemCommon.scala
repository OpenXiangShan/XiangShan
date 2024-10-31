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
  class SelectOldest[T <: Data](gen: T, numIn: Int, fn: (T, T) => Bool) extends Module {
    val io = IO(new Bundle() {
      val in = Flipped(Vec(numIn, ValidIO(chiselTypeOf(gen))))
      val oldest = ValidIO(chiselTypeOf(gen))
    })

    def findOldest: (T, T) => Bool = fn

    def selectOldest(valid: Seq[Bool], bits: Seq[T]): (Seq[Bool], Seq[T]) = {
      assert(valid.length == bits.length)
      if (valid.length == 0 || valid.length == 1) {
        (valid, bits)
      } else if (valid.length == 2) {
        val res = Seq.fill(2)(Wire(ValidIO(chiselTypeOf(bits(0)))))
        for (i <- res.indices) {
          res(i).valid := valid(i)
          res(i).bits := bits(i)
        }
        val oldest = Mux(valid(0) && valid(1),
          Mux(findOldest(bits(0), bits(1)), res(1), res(0)),
          Mux(valid(0) && !valid(1), res(0), res(1)))
        (Seq(oldest.valid), Seq(oldest.bits))
      } else {
        val left = selectOldest(valid.take(valid.length / 2), bits.take(bits.length / 2))
        val right = selectOldest(valid.takeRight(valid.length - (valid.length / 2)), bits.takeRight(bits.length - (bits.length / 2)))
        selectOldest(left._1 ++ right._1, left._2 ++ right._2)
      }
    }

    val oldest = selectOldest(io.in.map(_.valid), io.in.map(_.bits))
    io.oldest.valid := oldest._1.head
    io.oldest.bits  := oldest._2.head
  }

  def apply[T <: Data]
  (gen: T, ins: Vec[ValidIO[T]], fn: (T, T) => Bool,
   moduleName: Option[String] = None
  ) = {
    val selectOldest = Module(new SelectOldest[T](gen = ins.head.bits.cloneType, numIn = ins.length, fn = fn))
    selectOldest.io.in <> ins
    selectOldest.io.oldest
  }
}