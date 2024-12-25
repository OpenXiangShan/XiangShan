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
  private val causes: Set[(Int, String)] = Set()

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
  val C_RARN = addCause(name = "loadQueueRAR nack")
  val C_RAWN = addCause(name = "loadQueueRAW nack")
  val C_SN   = addCause(name = "slight nuke")
  val C_MBN  = addCause(name = "misalign buffer nack")

  val numCauses: Int = causes.size

  def apply() = Vec(numCauses, Bool())

  def apply(init: Bool) = VecInit(Seq.fill(numCauses)(init))

  def isMA(cause: Vec[Bool]): Bool = cause(C_MA)

  def isTM(cause: Vec[Bool]): Bool = cause(C_TM)

  def isFF(cause: Vec[Bool]): Bool = cause(C_FF)

  def isDR(cause: Vec[Bool]): Bool = cause(C_DR)

  def isDM(cause: Vec[Bool]): Bool = cause(C_DM)

  def isWPF(cause: Vec[Bool]): Bool = cause(C_WPF)

  def isBC(cause: Vec[Bool]): Bool = cause(C_BC)

  def isRARN(cause: Vec[Bool]): Bool = cause(C_RARN)

  def isRAWN(cause: Vec[Bool]): Bool = cause(C_RAWN)

  def isSN(cause: Vec[Bool]): Bool = cause(C_SN)

  def isMBN(cause: Vec[Bool]): Bool = cause(C_MBN)

  def getHigherCauseThan(cause: Int): Seq[Int] = {
    val priorities = causes.map(_._1).toSeq
    val idx = priorities.indexOf(cause, 0)
    require(idx != -1, s"The $cause does not exists in IntPriority Seq")
    priorities.slice(0, idx)
  }

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