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
import xiangshan.backend.Bundles.DynInst
import xiangshan.backend.rob.RobPtr
import xiangshan.cache._
import xiangshan.backend.fu.FenceToSbuffer
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.mem.prefetch.PrefetchReqBundle
import math._

object genWmask {
  def apply(addr: UInt, sizeEncode: UInt): UInt = {
    require(sizeEncode.getWidth == LSUOpType.Size.width)
    (LookupTree(sizeEncode, List(
      LSUOpType.B.U -> 0x1.U, //0001 << addr(2:0)
      LSUOpType.H.U -> 0x3.U, //0011
      LSUOpType.W.U -> 0xf.U, //1111
      LSUOpType.D.U -> 0xff.U //11111111
    )) << addr(2, 0)).asUInt
  }
}

object genVWmask {
  def apply(addr: UInt, sizeEncode: UInt): UInt = {
    require(sizeEncode.getWidth == LSUOpType.Size.width)
    (LookupTree(sizeEncode, List(
      LSUOpType.B.U -> 0x1.U, //0001 << addr(2:0)
      LSUOpType.H.U -> 0x3.U, //0011
      LSUOpType.W.U -> 0xf.U, //1111
      LSUOpType.D.U -> 0xff.U //11111111
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
    require(sizeEncode.getWidth == LSUOpType.Size.width)
    LookupTree(sizeEncode, List(
      LSUOpType.B.U -> 0x1.U,
      LSUOpType.H.U -> 0x3.U,
      LSUOpType.W.U -> 0xf.U,
      LSUOpType.D.U -> 0xff.U
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

object MemorySize {

  sealed abstract class Size (uint: UInt) {
    def U: UInt = this.uint
    def ByteOffset: UInt
  }

  object Size {
    def width:           Int = 3
    def ByteOffsetWidth: Int = 5
    val all:      List[Size] = List(B, H, W, D, Q)
  }

  /*
  * ByteOffset is for generate ByteEnd, the range of request is [BytesStart, ByteEnd]
  */
  case object B extends Size("b000".U(Size.width.W)){
    def ByteOffset = 0.U(Size.ByteOffsetWidth.W)
  }
  case object H extends Size("b001".U(Size.width.W)){
    def ByteOffset = 1.U(Size.ByteOffsetWidth.W)
  }
  case object W extends Size("b010".U(Size.width.W)){
    def ByteOffset = 3.U(Size.ByteOffsetWidth.W)
  }
  case object D extends Size("b011".U(Size.width.W)){
    def ByteOffset = 7.U(Size.ByteOffsetWidth.W)
  }
  case object Q extends Size("b100".U(Size.width.W)){
    def ByteOffset = 15.U(Size.ByteOffsetWidth.W)
  }

  /*
  * According to memorySize to select byteOffset
  */
  def ByteOffset (size: UInt): UInt = {
    require(size.getWidth == Size.width)
    LookupTree(size, Size.all.map(s => s.U -> s.ByteOffset))
  }

  // The range of request is [BytesStart, ByteEnd]
  def CaculateSelectMask(start: UInt, end: UInt): UInt = {
    end - start + 1.U
  }

  def sizeIs(op: UInt, sz: Size): Bool = {
    op === sz.U
  }
}
