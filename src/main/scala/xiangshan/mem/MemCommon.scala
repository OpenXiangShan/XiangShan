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
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util._
import utility._
import utils._
import xiangshan._
import xiangshan.backend.Bundles.DynInst
import xiangshan.backend.rob.RobPtr
import xiangshan.cache._
import xiangshan.backend.fu.FenceToSbuffer
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.frontend.icache.PrefetchReqBundle

import math._

object genWmask {
  def apply(addr: UInt, sizeEncode: UInt): UInt = {
    (LookupTree(sizeEncode.take(2), List(
      LSUOpType.B.U -> 0x1.U, //0001 << addr(2:0)
      LSUOpType.H.U -> 0x3.U, //0011
      LSUOpType.W.U -> 0xf.U, //1111
      LSUOpType.D.U -> 0xff.U //11111111
    )) << addr(2, 0)).asUInt
  }
}

object genVWmask {
  def apply(addr: UInt, sizeEncode: UInt): UInt = {
    (LookupTree(sizeEncode.take(2), List(
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
    LookupTree(sizeEncode.take(2), List(
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

  sealed abstract class Size (val bitpat: BitPat) {
    def U: UInt = bitPatToUInt(bitpat)
    def ByteOffset: UInt
  }

  object Size {
    def width:           Int = 3
    def ByteOffsetWidth: Int = 4
    val all:      List[Size] = List(B, H, W, D, Q)
  }

  /*
  * ByteOffset is for generate ByteEnd, the range of request is [BytesStart, ByteEnd]
  */
  case object B extends Size(BitPat("b000")){
    def ByteOffset = 0.U(Size.ByteOffsetWidth.W)
  }
  case object H extends Size(BitPat("b001")){
    def ByteOffset = 1.U(Size.ByteOffsetWidth.W)
  }
  case object W extends Size(BitPat("b010")){
    def ByteOffset = 3.U(Size.ByteOffsetWidth.W)
  }
  case object D extends Size(BitPat("b011")){
    def ByteOffset = 7.U(Size.ByteOffsetWidth.W)
  }
  case object Q extends Size(BitPat("b1??")){
    def ByteOffset = 15.U(Size.ByteOffsetWidth.W)

    override def U: UInt = {
      throw new NoSuchMethodException(
        "To avoid wrong comparison **Q.U === size**, calling this method will raise exception.\n" +
          "If you want to check whether it is 128b load/store, please use sizeIs(_.Q)(size) instead.\n" +
          "If you want to generate a UInt signal, please use QB.U\n"
      )
    }
  }
  case object QB extends Size(BitPat("b100")) {
    def ByteOffset = Q.ByteOffset
  }

  /*
  * According to memorySize to select byteOffset
  */
  def ByteOffset (size: UInt): UInt = {
    require(size.getWidth == Size.width)
    Mux1H(Size.all.map(x => (x.bitpat === size) -> x.ByteOffset))
  }

  // The range of request is [BytesStart, ByteEnd]
  def CalculateSelectMask(start: UInt, end: UInt): UInt = {
    end - start +& 1.U
  }

  def sizeIs(op: UInt, sz: Size): Bool = {
    op === sz.bitpat
  }

  def sizeIs(sz: this.type => Size)(size: UInt): Bool = {
    sz(this).bitpat === size
  }
}

class SelectOldest[T <: Data](gen: T, numIn: Int, f: (T, T) => Bool) extends Module {
  val io = IO(new Bundle{
    val in = Vec(numIn, Flipped(ValidIO(gen.cloneType)))
    val out = ValidIO(gen.cloneType)
  })
  def findOlder: (T, T) => Bool = f

  val validSeq = io.in.map(_.valid)
  val bitSeq = io.in.map(_.bits)
  def selectPartialOldest[T <: Data](
    valid: Seq[Bool], bits: Seq[T], isOlderFu: (T, T) => Bool
  ): (Seq[Bool], Seq[T]) = {
    assert(valid.length == bits.length)
    if (valid.length == 0 || valid.length == 1) {
      (valid, bits)
    } else if (valid.length == 2) {
      val res = Seq.fill(2)(Wire(ValidIO(chiselTypeOf(bits(0)))))
      for (i <- res.indices) {
        res(i).valid := valid(i)
        res(i).bits := bits(i)
      }
      val oldest = Mux(
        valid(0) && valid(1),
        Mux(isOlderFu(bits(0), bits(1)), res(0), res(1)),
        Mux(valid(0) && !valid(1), res(0), res(1))
      )
      (Seq(oldest.valid), Seq(oldest.bits))
    } else {
      val left = selectPartialOldest(valid.take(valid.length / 2), bits.take(bits.length / 2), isOlderFu)
      val right = selectPartialOldest(valid.takeRight(valid.length - (valid.length / 2)), bits.takeRight(bits.length - (bits.length / 2)), isOlderFu)
      selectPartialOldest(left._1 ++ right._1, left._2 ++ right._2, isOlderFu)
    }
  }

  val oldest = selectPartialOldest(validSeq, bitSeq, findOlder)

  io.out.valid := oldest._1.head
  io.out.bits := oldest._2.head

}
