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
import xiangshan.backend.fu.FenceToSbuffer
import xiangshan.mem.prefetch.PrefetchReqBundle
import xiangshan.cache._
import xiangshan.cache.wpu.ReplayCarry
import scala.collection.mutable
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

object genBasemask {
  /**
   *
   * @param addr
   * @param sizeEncode
   * @return Return 16-byte aligned mask.
   *
   *         Example:
   *         Address: 0x80000003 Encoding size: ‘b11
   *         Return: 0xff
   */
  def apply(addr: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U,
      "b01".U -> 0x3.U,
      "b10".U -> 0xf.U,
      "b11".U -> 0xff.U
    ))
  }
}

object shiftDataToLow {
  def apply(addr: UInt, data : UInt): UInt = {
    Mux(addr(3), (data >> 64).asUInt, data)
  }
}
object shiftMaskToLow {
  def apply(addr: UInt, mask: UInt): UInt = {
    Mux(addr(3), (mask >> 8).asUInt, mask)
  }
}
object shiftDataToHigh {
  def apply(addr: UInt, data : UInt): UInt = {
    Mux(addr(3), (data << 64).asUInt, data)
  }
}
object shiftMaskToHigh {
  def apply(addr: UInt, mask: UInt): UInt = {
    Mux(addr(3), (mask << 8).asUInt, mask)
  }
}

object ReplayCauseNO {
  private val causes: mutable.Set[(Int, String)] = mutable.Set()

  private var initVal = 0

  private def addCause(name: String): Int = {
    val causeNo = initVal
    causes += (initVal -> name)
    initVal += 1
    causeNo
  }

  // priority: highest to lowest
  val C_MA   = addCause(name = "memory ambiguous")
  val C_TM   = addCause(name = "tlb miss")
  val C_FF   = addCause(name = "forward fail")
  val C_DR   = addCause(name = "dcache replay")
  val C_DM   = addCause(name = "dcache miss")
  val C_WPF  = addCause(name = "way predict fail")
  val C_BC   = addCause(name = "bank conflict")
  val C_RARF = addCause(name = "loadQueueRAR full")
  val C_RAWF = addCause(name = "loadQueueRAW full")
  val C_NK   = addCause(name = "nuke")
  val C_MF   = addCause(name = "misalign buffer full")

  val numCauses: Int = causes.size

  def apply() = Vec(numCauses, Bool())

  def apply(init: Bool) = VecInit(Seq.fill(numCauses)(init))

  def hasMA(cause: Vec[Bool]): Bool = cause(C_MA)

  def hasTM(cause: Vec[Bool]): Bool = cause(C_TM)

  def hasFF(cause: Vec[Bool]): Bool = cause(C_FF)

  def hasDR(cause: Vec[Bool]): Bool = cause(C_DR)

  def hasDM(cause: Vec[Bool]): Bool = cause(C_DM)

  def hasWPF(cause: Vec[Bool]): Bool = cause(C_WPF)

  def hasBC(cause: Vec[Bool]): Bool = cause(C_BC)

  def hasRARF(cause: Vec[Bool]): Bool = cause(C_RARF)

  def hasRAWF(cause: Vec[Bool]): Bool = cause(C_RAWF)

  def hasNK(cause: Vec[Bool]): Bool = cause(C_NK)

  def hasMF(cause: Vec[Bool]): Bool = cause(C_MF)

  def getHigherCauseThan(cause: Int): Seq[Int] = {
    val priorities = causes.map(_._1).toSeq
    val idx = priorities.indexOf(cause, 0)
    require(idx != -1, s"The $cause does not exists in IntPriority Seq")
    priorities.slice(0, idx)
  }

  def slice(seq: Seq[Bool], lower: Int, upper: Int): Seq[Bool] = {
    seq.zipWithIndex.filter(x => x._2 >= lower && x._2 < upper).map(_._1)
  }

  def staCauses(): Seq[Int] = Seq(C_TM)

  def lduCauses(): Seq[Int] = causes.map(_._1).toSeq

  def hyuCauses(): Seq[Int] = (staCauses() ++ lduCauses()).distinct

  def highestSelect(cause: UInt): Vec[Bool] = {
    VecInit(PriorityEncoderOH(cause.asUInt).asBools)
  }

  def highestSelect(cause: Vec[Bool]): Vec[Bool] = highestSelect(cause.asUInt)

  def needReplay(cause: Vec[Bool]): Bool = cause.asUInt.orR
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
