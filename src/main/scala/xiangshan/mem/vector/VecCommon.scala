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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Bundles.VEew
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.datapath.DataConfig.{VecData, V0Data, VlData}

/**
  * Common used parameters or functions in vlsu
  */
trait VLSUConstants {
  // 1/2/4/8
  lazy val vlmBindexBits = 8 //will be overrided later
  lazy val vsmBindexBits = 8 // will be overrided later

  def alignTypes = 5 // eew/sew = 1/2/4/8, last indicate 128 bit element
  def alignTypeBits = log2Up(alignTypes)
  def maxMUL = 8
  // 64
  def maxFlowNum = 16
  def maxElemNum = maxMUL * maxFlowNum // 128
  // def uopIdxBits = log2Up(maxUopNum) // to index uop inside an robIdx
  def elemIdxBits = log2Up(maxElemNum) + 1 // to index which element in an instruction

  def ewBits = 3 // bits-width of EEW/SEW
  def mulBits = 3 // bits-width of emul/lmul
}

trait HasVLSUParameters extends HasMemBlockParameters with VLSUConstants {
  override lazy val vlmBindexBits = log2Up(coreParams.VlMergeBufferSize)
  override lazy val vsmBindexBits = log2Up(coreParams.VsMergeBufferSize)
}

/**
  * when emul is greater than or equal to 1, this means the entire register needs to be written;
  * otherwise, only write the specified number of bytes */
object MulDataSize {
  def apply (mul: UInt): UInt = { //mul means emul or lmul
    (LookupTree(mul,List(
      "b101".U -> 2.U  , // 1/8
      "b110".U -> 4.U  , // 1/4
      "b111".U -> 8.U  , // 1/2
      "b000".U -> 16.U , // 1
      "b001".U -> 16.U , // 2
      "b010".U -> 16.U , // 4
      "b011".U -> 16.U   // 8
    )))}
}

//eew decode
object EewLog2 extends VLSUConstants {
  // def apply (eew: UInt): UInt = {
  //   (LookupTree(eew,List(
  //     "b000".U -> "b000".U , // 1
  //     "b101".U -> "b001".U , // 2
  //     "b110".U -> "b010".U , // 4
  //     "b111".U -> "b011".U   // 8
  //   )))}
  def apply(eew: UInt): UInt = {
    require(eew.getWidth == 2, "The eew width must be 2.")
    ZeroExt(eew, ewBits)
  }
}

object GenRealFlowNum {
  /**
   * unit-stride instructions don't use this method;
   * other instructions generate realFlowNum by EmulDataSize >> eew,
   * EmulDataSize means the number of bytes that need to be written to the register,
   * eew means the number of bytes written at once.
   *
   * @param instType As the name implies.
   * @param emul As the name implies.
   * @param lmul As the name implies.
   * @param eew As the name implies.
   * @param sew As the name implies.
   * @param isSegment Only modules related to segment need to be set to true.
   * @return FlowNum of instruction.
   *
   */
  def apply (instType: UInt, emul: UInt, lmul: UInt, eew: UInt, sew: UInt, isSegment: Boolean = false): UInt = {
    require(instType.getWidth == 3, "The instType width must be 3, (isSegment, mop)")
    require(eew.getWidth == 2, "The eew width must be 2.")
    // Because the new segmentunit is needed. But the previous implementation is retained for the time being in case of emergency.
    val segmentIndexFlowNum =  if (isSegment) (MulDataSize(lmul) >> sew(1,0)).asUInt
    else Mux(emul.asSInt > lmul.asSInt, (MulDataSize(emul) >> eew).asUInt, (MulDataSize(lmul) >> sew(1,0)).asUInt)
    (LookupTree(instType,List(
      "b000".U ->  (MulDataSize(emul) >> eew).asUInt, // store use, load do not use
      "b010".U ->  (MulDataSize(emul) >> eew).asUInt, // strided
      "b001".U ->  Mux(emul.asSInt > lmul.asSInt, (MulDataSize(emul) >> eew).asUInt, (MulDataSize(lmul) >> sew(1,0)).asUInt), // indexed-unordered
      "b011".U ->  Mux(emul.asSInt > lmul.asSInt, (MulDataSize(emul) >> eew).asUInt, (MulDataSize(lmul) >> sew(1,0)).asUInt), // indexed-ordered
      "b100".U ->  (MulDataSize(emul) >> eew).asUInt, // segment unit-stride
      "b110".U ->  (MulDataSize(emul) >> eew).asUInt, // segment strided
      "b101".U ->  segmentIndexFlowNum, // segment indexed-unordered
      "b111".U ->  segmentIndexFlowNum  // segment indexed-ordered
    )))}
}

object GenUSWholeEmul extends VLSUConstants{
  def apply(nf: UInt): UInt={
    LookupTree(nf,List(
      "b000".U -> "b000".U(mulBits.W),
      "b001".U -> "b001".U(mulBits.W),
      "b011".U -> "b010".U(mulBits.W),
      "b111".U -> "b011".U(mulBits.W)
    ))
  }
}

object genVWmask128 {
  def apply(addr: UInt, sizeEncode: UInt): UInt = (Mux1H(Seq(
    (BitPat("b000") === sizeEncode) -> 0x1.U,  //0001 << addr(2:0)
    (BitPat("b001") === sizeEncode) -> 0x3.U,  //0011
    (BitPat("b010") === sizeEncode) -> 0xf.U,  //1111
    (BitPat("b011") === sizeEncode) -> 0xff.U, //11111111
    (BitPat("b1??") === sizeEncode) -> 0xffff.U,
  )) << addr(3, 0)).asUInt
}

object genVFirstUnmask extends VLSUConstants {
  /**
   * Find the lowest unmasked number of bits.
   * example:
   *   mask = 16'b1111_1111_1110_0000
   *   return 5
   * @param mask 16bits of mask.
   * @return lowest unmasked number of bits.
   */
  def apply(mask: UInt): UInt = {
    require(mask.getWidth == 16, "The mask width must be 16")
    val select = (0 until 16).zip(mask.asBools).map{case (i, v) =>
      (v, i.U)
    }
    PriorityMuxDefault(select, 0.U)
  }

  def apply(mask: UInt, regOffset: UInt): UInt = {
    require(mask.getWidth == 16, "The mask width must be 16")
    val realMask = (mask >> regOffset).asUInt
    val select = (0 until 16).zip(realMask.asBools).map{case (i, v) =>
      (v, i.U)
    }
    PriorityMuxDefault(select, 0.U)
  }
}
