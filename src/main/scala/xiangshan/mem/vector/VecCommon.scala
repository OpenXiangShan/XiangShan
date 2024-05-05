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

/**
  * Common used parameters or functions in vlsu
  */
trait VLSUConstants {
  val VLEN = 128
  //for pack unit-stride flow
  val AlignedNum = 4 // 1/2/4/8
  def VLENB = VLEN/8
  def vOffsetBits = log2Up(VLENB) // bits-width to index offset inside a vector reg
  lazy val vlmBindexBits = 8 //will be overrided later
  lazy val vsmBindexBits = 8 // will be overrided later

  def alignTypes = 5 // eew/sew = 1/2/4/8, last indicate 128 bit element
  def alignTypeBits = log2Up(alignTypes)
  def maxMUL = 8
  def maxFields = 8
  /**
    * In the most extreme cases like a segment indexed instruction, eew=64, emul=8, sew=8, lmul=1,
    * and nf=8, each data reg is mapped with 8 index regs and there are 8 data regs in total,
    * each for a field. Therefore an instruction can be divided into 64 uops at most.
    */
  def maxUopNum = maxMUL * maxFields // 64
  def maxFlowNum = 16
  def maxElemNum = maxMUL * maxFlowNum // 128
  // def uopIdxBits = log2Up(maxUopNum) // to index uop inside an robIdx
  def elemIdxBits = log2Up(maxElemNum) + 1 // to index which element in an instruction
  def flowIdxBits = log2Up(maxFlowNum) + 1 // to index which flow in a uop
  def fieldBits = log2Up(maxFields) + 1 // 4-bits to indicate 1~8

  def ewBits = 3 // bits-width of EEW/SEW
  def mulBits = 3 // bits-width of emul/lmul

  def getSlice(data: UInt, i: Int, alignBits: Int): UInt = {
    require(data.getWidth >= (i+1) * alignBits)
    data((i+1) * alignBits - 1, i * alignBits)
  }
  def getNoAlignedSlice(data: UInt, i: Int, alignBits: Int): UInt = {
    data(i * 8 + alignBits - 1, i * 8)
  }

  def getByte(data: UInt, i: Int = 0) = getSlice(data, i, 8)
  def getHalfWord(data: UInt, i: Int = 0) = getSlice(data, i, 16)
  def getWord(data: UInt, i: Int = 0) = getSlice(data, i, 32)
  def getDoubleWord(data: UInt, i: Int = 0) = getSlice(data, i, 64)
  def getDoubleDoubleWord(data: UInt, i: Int = 0) = getSlice(data, i, 128)
}

trait HasVLSUParameters extends HasXSParameter with VLSUConstants {
  override val VLEN = coreParams.VLEN
  override lazy val vlmBindexBits = log2Up(coreParams.VlMergeBufferSize)
  override lazy val vsmBindexBits = log2Up(coreParams.VsMergeBufferSize)
  def isUnitStride(instType: UInt) = instType(1, 0) === "b00".U
  def isStrided(instType: UInt) = instType(1, 0) === "b10".U
  def isIndexed(instType: UInt) = instType(0) === "b1".U
  def isNotIndexed(instType: UInt) = instType(0) === "b0".U
  def isSegment(instType: UInt) = instType(2) === "b1".U
  def is128Bit(alignedType: UInt) = alignedType(2) === "b1".U

  def mergeDataWithMask(oldData: UInt, newData: UInt, mask: UInt): Vec[UInt] = {
    require(oldData.getWidth == newData.getWidth)
    require(oldData.getWidth == mask.getWidth * 8)
    VecInit(mask.asBools.zipWithIndex.map { case (en, i) =>
      Mux(en, getByte(newData, i), getByte(oldData, i))
    })
  }

  // def asBytes(data: UInt) = {
  //   require(data.getWidth % 8 == 0)
  //   (0 until data.getWidth/8).map(i => getByte(data, i))
  // }

  def mergeDataWithElemIdx(
    oldData: UInt,
    newData: Seq[UInt],
    alignedType: UInt,
    elemIdx: Seq[UInt],
    valids: Seq[Bool]
  ): UInt = {
    require(newData.length == elemIdx.length)
    require(newData.length == valids.length)
    LookupTree(alignedType, List(
      "b00".U -> VecInit(elemIdx.map(e => UIntToOH(e(3, 0)).asBools).transpose.zipWithIndex.map { case (selVec, i) =>
        ParallelPosteriorityMux(
          true.B +: selVec.zip(valids).map(x => x._1 && x._2),
          getByte(oldData, i) +: newData.map(getByte(_))
        )}).asUInt,
      "b01".U -> VecInit(elemIdx.map(e => UIntToOH(e(2, 0)).asBools).transpose.zipWithIndex.map { case (selVec, i) =>
        ParallelPosteriorityMux(
          true.B +: selVec.zip(valids).map(x => x._1 && x._2),
          getHalfWord(oldData, i) +: newData.map(getHalfWord(_))
        )}).asUInt,
      "b10".U -> VecInit(elemIdx.map(e => UIntToOH(e(1, 0)).asBools).transpose.zipWithIndex.map { case (selVec, i) =>
        ParallelPosteriorityMux(
          true.B +: selVec.zip(valids).map(x => x._1 && x._2),
          getWord(oldData, i) +: newData.map(getWord(_))
        )}).asUInt,
      "b11".U -> VecInit(elemIdx.map(e => UIntToOH(e(0)).asBools).transpose.zipWithIndex.map { case (selVec, i) =>
        ParallelPosteriorityMux(
          true.B +: selVec.zip(valids).map(x => x._1 && x._2),
          getDoubleWord(oldData, i) +: newData.map(getDoubleWord(_))
        )}).asUInt
    ))
  }

  def mergeDataWithElemIdx(oldData: UInt, newData: UInt, alignedType: UInt, elemIdx: UInt): UInt = {
    mergeDataWithElemIdx(oldData, Seq(newData), alignedType, Seq(elemIdx), Seq(true.B))
  }
  /**
    * for merge 128-bits data of unit-stride
    */
  object mergeDataByByte{
    def apply(oldData: UInt, newData: UInt, mask: UInt): UInt = {
      val selVec = Seq(mask).map(_.asBools).transpose
      VecInit(selVec.zipWithIndex.map{ case (selV, i) =>
        ParallelPosteriorityMux(
          true.B +: selV.map(x => x),
          getByte(oldData, i) +: Seq(getByte(newData, i))
        )}).asUInt
    }
  }

  /**
    * for merge Unit-Stride data to 256-bits
    * merge 128-bits data to 256-bits
    * if have 3 port,
    *   if is port0, it is 6 to 1 Multiplexer -> (128'b0, data) or (data, 128'b0) or (data, port2data) or (port2data, data) or (data, port3data) or (port3data, data)
    *   if is port1, it is 4 to 1 Multiplexer -> (128'b0, data) or (data, 128'b0) or (data, port3data) or (port3data, data)
    *   if is port3, it is 2 to 1 Multiplexer -> (128'b0, data) or (data, 128'b0)
    *
    */
  object mergeDataByIndex{
    def apply(data:  Seq[UInt], mask: Seq[UInt], index: UInt, valids: Seq[Bool]): (UInt, UInt) = {
      require(data.length == valids.length)
      require(data.length == mask.length)
      val muxLength = data.length
      val selDataMatrix = Wire(Vec(muxLength, Vec(2, UInt((VLEN * 2).W)))) // 3 * 2 * 256
      val selMaskMatrix = Wire(Vec(muxLength, Vec(2, UInt((VLENB * 2).W)))) // 3 * 2 * 16
      dontTouch(selDataMatrix)
      dontTouch(selMaskMatrix)
      for(i <- 0 until muxLength){
        if(i == 0){
          selDataMatrix(i)(0) := Cat(0.U(VLEN.W), data(i))
          selDataMatrix(i)(1) := Cat(data(i), 0.U(VLEN.W))
          selMaskMatrix(i)(0) := Cat(0.U(VLENB.W), mask(i))
          selMaskMatrix(i)(1) := Cat(mask(i), 0.U(VLENB.W))
        }
        else{
          selDataMatrix(i)(0) := Cat(data(i), data(0))
          selDataMatrix(i)(1) := Cat(data(0), data(i))
          selMaskMatrix(i)(0) := Cat(mask(i), mask(0))
          selMaskMatrix(i)(1) := Cat(mask(0), mask(i))
        }
      }
      val selIdxVec = (0 until muxLength).map(_.U)
      val selIdx    = PriorityMux(valids.reverse, selIdxVec.reverse)

      val selData = Mux(index === 0.U,
                        selDataMatrix(selIdx)(0),
                        selDataMatrix(selIdx)(1))
      val selMask = Mux(index === 0.U,
                        selMaskMatrix(selIdx)(0),
                        selMaskMatrix(selIdx)(1))
      (selData, selMask)
    }
  }
  def mergeDataByIndex(data:  UInt, mask: UInt, index: UInt): (UInt, UInt) = {
    mergeDataByIndex(Seq(data), Seq(mask), index, Seq(true.B))
  }
}
abstract class VLSUModule(implicit p: Parameters) extends XSModule
  with HasVLSUParameters
  with HasCircularQueuePtrHelper
abstract class VLSUBundle(implicit p: Parameters) extends XSBundle
  with HasVLSUParameters

class VLSUBundleWithMicroOp(implicit p: Parameters) extends VLSUBundle {
  val uop = new DynInst
}

class OnlyVecExuOutput(implicit p: Parameters) extends VLSUBundle {
  val isvec = Bool()
  val vecdata = UInt(VLEN.W)
  val mask = UInt(VLENB.W)
  // val rob_idx_valid = Vec(2, Bool())
  // val inner_idx = Vec(2, UInt(3.W))
  // val rob_idx = Vec(2, new RobPtr)
  // val offset = Vec(2, UInt(4.W))
  val reg_offset = UInt(vOffsetBits.W)
  val vecActive = Bool() // 1: vector active element, 0: vector not active element
  val is_first_ele = Bool()
  val elemIdx = UInt(elemIdxBits.W) // element index
  val elemIdxInsideVd = UInt(elemIdxBits.W) // element index in scope of vd
  // val uopQueuePtr = new VluopPtr
  // val flowPtr = new VlflowPtr
}

class VecExuOutput(implicit p: Parameters) extends MemExuOutput with HasVLSUParameters {
  val vec = new OnlyVecExuOutput
  val alignedType       = UInt(alignTypeBits.W)
   // feedback
  val vecFeedback       = Bool()
}

// class VecStoreExuOutput(implicit p: Parameters) extends MemExuOutput with HasVLSUParameters {
//   val elemIdx = UInt(elemIdxBits.W)
//   val uopQueuePtr = new VsUopPtr
//   val fieldIdx = UInt(fieldBits.W)
//   val segmentIdx = UInt(elemIdxBits.W)
//   val vaddr = UInt(VAddrBits.W)
//   // pack
//   val isPackage         = Bool()
//   val packageNum        = UInt((log2Up(VLENB) + 1).W)
//   val originAlignedType = UInt(alignTypeBits.W)
//   val alignedType       = UInt(alignTypeBits.W)
// }

class VecUopBundle(implicit p: Parameters) extends VLSUBundleWithMicroOp {
  val flowMask       = UInt(VLENB.W) // each bit for a flow
  val byteMask       = UInt(VLENB.W) // each bit for a byte
  val data           = UInt(VLEN.W)
  // val fof            = Bool() // fof is only used for vector loads
  val excp_eew_index = UInt(elemIdxBits.W)
  // val exceptionVec   = ExceptionVec() // uop has exceptionVec
  val baseAddr = UInt(VAddrBits.W)
  val stride = UInt(VLEN.W)
  val flow_counter = UInt(flowIdxBits.W)

  // instruction decode result
  val flowNum = UInt(flowIdxBits.W) // # of flows in a uop
  // val flowNumLog2 = UInt(log2Up(flowIdxBits).W) // log2(flowNum), for better timing of multiplication
  val nfields = UInt(fieldBits.W) // NFIELDS
  val vm = Bool() // whether vector masking is enabled
  val usWholeReg = Bool() // unit-stride, whole register load
  val usMaskReg = Bool() // unit-stride, masked store/load
  val eew = UInt(ewBits.W) // size of memory elements
  val sew = UInt(ewBits.W)
  val emul = UInt(mulBits.W)
  val lmul = UInt(mulBits.W)
  val vlmax = UInt(elemIdxBits.W)
  val instType = UInt(3.W)
  val vd_last_uop = Bool()
  val vd_first_uop = Bool()
}

class VecFlowBundle(implicit p: Parameters) extends VLSUBundleWithMicroOp {
  val vaddr             = UInt(VAddrBits.W)
  val mask              = UInt(VLENB.W)
  val alignedType       = UInt(alignTypeBits.W)
  val vecActive         = Bool()
  val elemIdx           = UInt(elemIdxBits.W)
  val is_first_ele      = Bool()

  // pack
  val isPackage         = Bool()
  val packageNum        = UInt((log2Up(VLENB) + 1).W)
  val originAlignedType = UInt(alignTypeBits.W)
}

class VecMemExuOutput(isVector: Boolean = false)(implicit p: Parameters) extends VLSUBundle{
  val output = new MemExuOutput(isVector)
  val vecFeedback = Bool()
  val mmio = Bool()
  val usSecondInv = Bool()
  val elemIdx = UInt(elemIdxBits.W)
  val alignedType = UInt(alignTypeBits.W)
  val mbIndex     = UInt(vsmBindexBits.W)
  val mask        = UInt(VLENB.W)
  val vaddr       = UInt(VAddrBits.W)
}

object MulNum {
  def apply (mul: UInt): UInt = { //mul means emul or lmul
    (LookupTree(mul,List(
      "b101".U -> 1.U , // 1/8
      "b110".U -> 1.U , // 1/4
      "b111".U -> 1.U , // 1/2
      "b000".U -> 1.U , // 1
      "b001".U -> 2.U , // 2
      "b010".U -> 4.U , // 4
      "b011".U -> 8.U   // 8
    )))}
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

object OneRegNum {
  def apply (eew: UInt): UInt = { //mul means emul or lmul
    (LookupTree(eew,List(
      "b000".U -> 16.U , // 1
      "b101".U -> 8.U , // 2
      "b110".U -> 4.U , // 4
      "b111".U -> 2.U   // 8
    )))}
}

//index inst read data byte
object SewDataSize {
  def apply (sew: UInt): UInt = {
    (LookupTree(sew,List(
      "b000".U -> 1.U , // 1
      "b001".U -> 2.U , // 2
      "b010".U -> 4.U , // 4
      "b011".U -> 8.U   // 8
    )))}
}

// strided inst read data byte
object EewDataSize {
  def apply (eew: UInt): UInt = {
    (LookupTree(eew,List(
      "b000".U -> 1.U , // 1
      "b101".U -> 2.U , // 2
      "b110".U -> 4.U , // 4
      "b111".U -> 8.U   // 8
    )))}
}

object loadDataSize {
  def apply (instType: UInt, emul: UInt, eew: UInt, sew: UInt): UInt = {
    (LookupTree(instType,List(
      "b000".U ->  MulDataSize(emul), // unit-stride
      "b010".U ->  EewDataSize(eew)  , // strided
      "b001".U ->  SewDataSize(sew)  , // indexed-unordered
      "b011".U ->  SewDataSize(sew)  , // indexed-ordered
      "b100".U ->  EewDataSize(eew)  , // segment unit-stride
      "b110".U ->  EewDataSize(eew)  , // segment strided
      "b101".U ->  SewDataSize(sew)  , // segment indexed-unordered
      "b111".U ->  SewDataSize(sew)    // segment indexed-ordered
    )))}
}

object storeDataSize {
  def apply (instType: UInt, eew: UInt, sew: UInt): UInt = {
    (LookupTree(instType,List(
      "b000".U ->  EewDataSize(eew)  , // unit-stride, do not use
      "b010".U ->  EewDataSize(eew)  , // strided
      "b001".U ->  SewDataSize(sew)  , // indexed-unordered
      "b011".U ->  SewDataSize(sew)  , // indexed-ordered
      "b100".U ->  EewDataSize(eew)  , // segment unit-stride
      "b110".U ->  EewDataSize(eew)  , // segment strided
      "b101".U ->  SewDataSize(sew)  , // segment indexed-unordered
      "b111".U ->  SewDataSize(sew)    // segment indexed-ordered
    )))}
}

object GenVecStoreMask {
  def apply (instType: UInt, eew: UInt, sew: UInt): UInt = {
    val mask = Wire(UInt(16.W))
    mask := UIntToOH(storeDataSize(instType = instType, eew = eew, sew = sew)) - 1.U
    mask
  }
}

/**
  * these are used to obtain immediate addresses for  index instruction */
object EewEq8 {
  def apply(index:UInt, flow_inner_idx: UInt): UInt = {
    (LookupTree(flow_inner_idx,List(
      0.U  -> index(7 ,0   ),
      1.U  -> index(15,8   ),
      2.U  -> index(23,16  ),
      3.U  -> index(31,24  ),
      4.U  -> index(39,32  ),
      5.U  -> index(47,40  ),
      6.U  -> index(55,48  ),
      7.U  -> index(63,56  ),
      8.U  -> index(71,64  ),
      9.U  -> index(79,72  ),
      10.U -> index(87,80  ),
      11.U -> index(95,88  ),
      12.U -> index(103,96 ),
      13.U -> index(111,104),
      14.U -> index(119,112),
      15.U -> index(127,120)
    )))}
}

object EewEq16 {
  def apply(index: UInt, flow_inner_idx: UInt): UInt = {
    (LookupTree(flow_inner_idx, List(
      0.U -> index(15, 0),
      1.U -> index(31, 16),
      2.U -> index(47, 32),
      3.U -> index(63, 48),
      4.U -> index(79, 64),
      5.U -> index(95, 80),
      6.U -> index(111, 96),
      7.U -> index(127, 112)
    )))}
}

object EewEq32 {
  def apply(index: UInt, flow_inner_idx: UInt): UInt = {
    (LookupTree(flow_inner_idx, List(
      0.U -> index(31, 0),
      1.U -> index(63, 32),
      2.U -> index(95, 64),
      3.U -> index(127, 96)
    )))}
}

object EewEq64 {
  def apply (index: UInt, flow_inner_idx: UInt): UInt = {
    (LookupTree(flow_inner_idx, List(
      0.U -> index(63, 0),
      1.U -> index(127, 64)
    )))}
}

object IndexAddr {
  def apply (index: UInt, flow_inner_idx: UInt, eew: UInt): UInt = {
    (LookupTree(eew,List(
      "b000".U -> EewEq8 (index = index, flow_inner_idx = flow_inner_idx ), // Imm is 1 Byte // TODO: index maybe cross register
      "b101".U -> EewEq16(index = index, flow_inner_idx = flow_inner_idx ), // Imm is 2 Byte
      "b110".U -> EewEq32(index = index, flow_inner_idx = flow_inner_idx ), // Imm is 4 Byte
      "b111".U -> EewEq64(index = index, flow_inner_idx = flow_inner_idx )  // Imm is 8 Byte
    )))}
}

object Log2Num {
  def apply (num: UInt): UInt = {
    (LookupTree(num,List(
      16.U -> 4.U,
      8.U  -> 3.U,
      4.U  -> 2.U,
      2.U  -> 1.U,
      1.U  -> 0.U
    )))}
}

object GenUopIdxInField {
  def apply (instType: UInt, emul: UInt, lmul: UInt, uopIdx: UInt): UInt = {
    val isIndexed = instType(0)
    val mulInField = Mux(
      isIndexed,
      Mux(lmul.asSInt > emul.asSInt, lmul, emul),
      emul
    )
    LookupTree(mulInField, List(
      "b101".U -> 0.U,
      "b110".U -> 0.U,
      "b111".U -> 0.U,
      "b000".U -> 0.U,
      "b001".U -> uopIdx(0),
      "b010".U -> uopIdx(1, 0),
      "b011".U -> uopIdx(2, 0)
    ))
  }
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
  def apply(eew: UInt): UInt = ZeroExt(eew(1, 0), ewBits)
}

/**
  * unit-stride instructions don't use this method;
  * other instructions generate realFlowNum by EmulDataSize >> eew(1,0),
  * EmulDataSize means the number of bytes that need to be written to the register,
  * eew(1,0) means the number of bytes written at once*/
object GenRealFlowNum {
  def apply (instType: UInt, emul: UInt, lmul: UInt, eew: UInt, sew: UInt): UInt = {
    require(instType.getWidth == 3, "The instType width must be 3, (isSegment, mop)")
    (LookupTree(instType,List(
      "b000".U ->  (MulDataSize(emul) >> eew(1,0)).asUInt, // store use, load do not use
      "b010".U ->  (MulDataSize(emul) >> eew(1,0)).asUInt, // strided
      "b001".U ->  Mux(emul.asSInt > lmul.asSInt, (MulDataSize(emul) >> eew(1,0)).asUInt, (MulDataSize(lmul) >> sew(1,0)).asUInt), // indexed-unordered
      "b011".U ->  Mux(emul.asSInt > lmul.asSInt, (MulDataSize(emul) >> eew(1,0)).asUInt, (MulDataSize(lmul) >> sew(1,0)).asUInt), // indexed-ordered
      "b100".U ->  (MulDataSize(emul) >> eew(1,0)).asUInt, // segment unit-stride
      "b110".U ->  (MulDataSize(emul) >> eew(1,0)).asUInt, // segment strided
      "b101".U ->  Mux(emul.asSInt > lmul.asSInt, (MulDataSize(emul) >> eew(1,0)).asUInt, (MulDataSize(lmul) >> sew(1,0)).asUInt), // segment indexed-unordered
      "b111".U ->  Mux(emul.asSInt > lmul.asSInt, (MulDataSize(emul) >> eew(1,0)).asUInt, (MulDataSize(lmul) >> sew(1,0)).asUInt)  // segment indexed-ordered
    )))}
}

/**
  * GenRealFlowLog2 = Log2(GenRealFlowNum)
  */
object GenRealFlowLog2 extends VLSUConstants {
  def apply(instType: UInt, emul: UInt, lmul: UInt, eew: UInt, sew: UInt): UInt = {
    require(instType.getWidth == 3, "The instType width must be 3, (isSegment, mop)")
    val emulLog2 = Mux(emul.asSInt >= 0.S, 0.U, emul)
    val lmulLog2 = Mux(lmul.asSInt >= 0.S, 0.U, lmul)
    val eewRealFlowLog2 = emulLog2 + log2Up(VLENB).U - eew(1, 0)
    val sewRealFlowLog2 = lmulLog2 + log2Up(VLENB).U - sew(1, 0)
    (LookupTree(instType, List(
      "b000".U -> eewRealFlowLog2, // unit-stride
      "b010".U -> eewRealFlowLog2, // strided
      "b001".U -> Mux(emul.asSInt > lmul.asSInt, eewRealFlowLog2, sewRealFlowLog2), // indexed-unordered
      "b011".U -> Mux(emul.asSInt > lmul.asSInt, eewRealFlowLog2, sewRealFlowLog2), // indexed-ordered
      "b100".U -> eewRealFlowLog2, // segment unit-stride
      "b110".U -> eewRealFlowLog2, // segment strided
      "b101".U -> Mux(emul.asSInt > lmul.asSInt, eewRealFlowLog2, sewRealFlowLog2), // segment indexed-unordered
      "b111".U -> Mux(emul.asSInt > lmul.asSInt, eewRealFlowLog2, sewRealFlowLog2), // segment indexed-ordered
    )))
  }
}

/**
  * GenElemIdx generals an element index within an instruction, given a certain uopIdx and a known flowIdx
  * inside the uop.
  */
object GenElemIdx extends VLSUConstants {
  def apply(instType: UInt, emul: UInt, lmul: UInt, eew: UInt, sew: UInt,
    uopIdx: UInt, flowIdx: UInt): UInt = {
    val isIndexed = instType(0).asBool
    val eewUopFlowsLog2 = Mux(emul.asSInt > 0.S, 0.U, emul) + log2Up(VLENB).U - eew(1, 0)
    val sewUopFlowsLog2 = Mux(lmul.asSInt > 0.S, 0.U, lmul) + log2Up(VLENB).U - sew(1, 0)
    val uopFlowsLog2 = Mux(
      isIndexed,
      Mux(emul.asSInt > lmul.asSInt, eewUopFlowsLog2, sewUopFlowsLog2),
      eewUopFlowsLog2
    )
    LookupTree(uopFlowsLog2, List(
      0.U -> uopIdx,
      1.U -> uopIdx ## flowIdx(0),
      2.U -> uopIdx ## flowIdx(1, 0),
      3.U -> uopIdx ## flowIdx(2, 0),
      4.U -> uopIdx ## flowIdx(3, 0)
    ))
  }
}

/**
  * GenVLMAX calculates VLMAX, which equals MUL * ew
  */
object GenVLMAXLog2 extends VLSUConstants {
  def apply(lmul: UInt, sew: UInt): UInt = lmul + log2Up(VLENB).U - sew
}
object GenVLMAX {
  def apply(lmul: UInt, sew: UInt): UInt = 1.U << GenVLMAXLog2(lmul, sew)
}

object GenUSWholeRegVL extends VLSUConstants {
  def apply(nfields: UInt, eew: UInt): UInt = {
    LookupTree(eew(1, 0), List(
      "b00".U -> (nfields << (log2Up(VLENB) - 0)),
      "b01".U -> (nfields << (log2Up(VLENB) - 1)),
      "b10".U -> (nfields << (log2Up(VLENB) - 2)),
      "b11".U -> (nfields << (log2Up(VLENB) - 3))
    ))
  }
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


object GenUSMaskRegVL extends VLSUConstants {
  def apply(vl: UInt): UInt = {
    Mux(vl(2,0) === 0.U , (vl >> 3.U), ((vl >> 3.U) + 1.U))
  }
}

object GenUopByteMask {
  def apply(flowMask: UInt, alignedType: UInt): UInt = {
    LookupTree(alignedType, List(
      "b000".U -> flowMask,
      "b001".U -> FillInterleaved(2, flowMask),
      "b010".U -> FillInterleaved(4, flowMask),
      "b011".U -> FillInterleaved(8, flowMask),
      "b100".U -> FillInterleaved(16, flowMask)
    ))
  }
}

object GenVdIdxInField extends VLSUConstants {
  def apply(instType: UInt, emul: UInt, lmul: UInt, uopIdx: UInt): UInt = {
    val vdIdx = Wire(UInt(log2Up(maxMUL).W))
    when (instType(1,0) === "b00".U || instType(1,0) === "b10".U || lmul.asSInt > emul.asSInt) {
      // Unit-stride or Strided, or indexed with lmul >= emul
      vdIdx := uopIdx
    }.otherwise {
      // Indexed with lmul <= emul
      val multiple = emul - lmul
      val uopIdxWidth = uopIdx.getWidth
      vdIdx := LookupTree(multiple, List(
        0.U -> uopIdx,
        1.U -> (uopIdx >> 1),
        2.U -> (uopIdx >> 2),
        3.U -> (uopIdx >> 3)
      ))
    }
    vdIdx
  }
}
/**
* Use start and vl to generate flow activative mask
* mod = true fill 0
* mod = false fill 1
*/
object GenFlowMask extends VLSUConstants {
  def apply(elementMask: UInt, start: UInt, vl: UInt , mod: Boolean): UInt = {
    val startMask = ~UIntToMask(start, VLEN)
    val vlMask = UIntToMask(vl, VLEN)
    val maskVlStart = vlMask & startMask
    if(mod){
      elementMask & maskVlStart
    }
    else{
      (~elementMask).asUInt & maskVlStart
    }
  }
}

object CheckAligned extends VLSUConstants {
  def apply(addr: UInt): UInt = {
    val aligned_16 = (addr(0) === 0.U) // 16-bit
    val aligned_32 = (addr(1,0) === 0.U) // 32-bit
    val aligned_64 = (addr(2,0) === 0.U) // 64-bit
    val aligned_128 = (addr(3,0) === 0.U) // 128-bit
    Cat(true.B, aligned_16, aligned_32, aligned_64, aligned_128)
  }
}

/**
  search if mask have continue 'len' bit '1'
  mask: source mask
  len: search length
*/
object GenPackMask{
  def leadX(mask: Seq[Bool], len: Int): Bool = {
    if(len == 1){
      mask.head
    }
    else{
      leadX(mask.drop(1),len-1) & mask.head
    }
  }
  def leadOneVec(shiftMask: Seq[Bool]): UInt = {
    // max is 64-bit, so the max num of flow to pack is 8

    val lead1 = leadX(shiftMask, 1) // continue 1 bit
    val lead2 = leadX(shiftMask, 2) // continue 2 bit
    val lead4 = leadX(shiftMask, 4) // continue 4 bit
    val lead8 = leadX(shiftMask, 8) // continue 8 bit
    val lead16 = leadX(shiftMask, 16) // continue 16 bit
    Cat(lead1, lead2, lead4, lead8, lead16)
  }

  def apply(shiftMask: UInt) = {
    // pack mask
    val packMask = leadOneVec(shiftMask.asBools)
    packMask
  }
}
/**
PackEnable = (LeadXVec >> eew) & alignedVec, where the 0th bit represents the ability to merge into a 64 bit flow, the second bit represents the ability to merge into a 32 bit flow, and so on.

example:
  addr = 0x0, activeMask = b00011100101111, flowIdx = 0, eew = 0(8-bit)

  step 0 : addrAlignedVec = (1, 1, 1, 1) elemIdxAligned = (1, 1, 1, 1)
  step 1 : activePackVec = (1, 1, 1, 0), inactivePackVec = (0, 0, 0, 0)
  step 2 : activePackEnable = (1, 1, 1, 0), inactivePackVec = (0, 0, 0, 0)

  we can package 4 8-bit activative flows into a 32-bit flow.
*/
object GenPackVec extends VLSUConstants{
  def apply(addr: UInt, shiftMask: UInt, eew: UInt, elemIdx: UInt): UInt = {
    val addrAlignedVec = CheckAligned(addr)
    val elemIdxAligned = CheckAligned(elemIdx)
    val packMask = GenPackMask(shiftMask)
    // generate packVec
    val packVec = addrAlignedVec & elemIdxAligned & (packMask.asUInt >> eew)

    packVec
  }
}

object GenPackAlignedType extends VLSUConstants{
  def apply(packVec: UInt): UInt = {
    val packAlignedType = PriorityMux(Seq(
      packVec(0) -> "b100".U,
      packVec(1) -> "b011".U,
      packVec(2) -> "b010".U,
      packVec(3) -> "b001".U,
      packVec(4) -> "b000".U
    ))
    packAlignedType
  }
}

object GenPackNum extends VLSUConstants{
  def apply(alignedType: UInt, packAlignedType: UInt): UInt = {
    (1.U << (packAlignedType - alignedType)).asUInt
  }
}

object genVWmask128 {
  def apply(addr: UInt, sizeEncode: UInt): UInt = {
    (LookupTree(sizeEncode, List(
      "b000".U -> 0x1.U, //0001 << addr(2:0)
      "b001".U -> 0x3.U, //0011
      "b010".U -> 0xf.U, //1111
      "b011".U -> 0xff.U, //11111111
      "b100".U -> 0xffff.U //1111111111111111
    )) << addr(3, 0)).asUInt
  }
}
/*
* only use in max length is 128
*/
object genVWdata {
  def apply(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b000".U -> Fill(16, data(7, 0)),
      "b001".U -> Fill(8, data(15, 0)),
      "b010".U -> Fill(4, data(31, 0)),
      "b011".U -> Fill(2, data(63,0)),
      "b100".U -> data(127,0)
    ))
  }
}

object genUSSplitAddr{
  def apply(addr: UInt, index: UInt): UInt = {
    val tmpAddr = Cat(addr(38, 4), 0.U(4.W))
    val nextCacheline = tmpAddr + 16.U
    LookupTree(index, List(
      0.U -> tmpAddr,
      1.U -> nextCacheline
    ))
  }
}

object genUSSplitMask{
  def apply(mask: UInt, index: UInt, addrOffset: UInt): UInt = {
    val tmpMask = Cat(0.U(16.W),mask) << addrOffset // 32-bits
    LookupTree(index, List(
      0.U -> tmpMask(15, 0),
      1.U -> tmpMask(31, 16),
    ))
  }
}

object genUSSplitData{
  def apply(data: UInt, index: UInt, addrOffset: UInt): UInt = {
    val tmpData = WireInit(0.U(256.W))
    val lookupTable = (0 until 16).map{case i =>
      if(i == 0){
        i.U -> Cat(0.U(128.W), data)
      }else{
        i.U -> Cat(0.U(((16-i)*8).W), data, 0.U((i*8).W))
      }
    }
    tmpData := LookupTree(addrOffset, lookupTable).asUInt

    LookupTree(index, List(
      0.U -> tmpData(127, 0),
      1.U -> tmpData(255, 128)
    ))
  }
}

object genVSData extends VLSUConstants {
  def apply(data: UInt, elemIdx: UInt, alignedType: UInt): UInt = {
    LookupTree(alignedType, List(
      "b000".U -> ZeroExt(LookupTree(elemIdx(3, 0), List.tabulate(VLEN/8)(i => i.U -> getByte(data, i))), VLEN),
      "b001".U -> ZeroExt(LookupTree(elemIdx(2, 0), List.tabulate(VLEN/16)(i => i.U -> getHalfWord(data, i))), VLEN),
      "b010".U -> ZeroExt(LookupTree(elemIdx(1, 0), List.tabulate(VLEN/32)(i => i.U -> getWord(data, i))), VLEN),
      "b011".U -> ZeroExt(LookupTree(elemIdx(0), List.tabulate(VLEN/64)(i => i.U -> getDoubleWord(data, i))), VLEN),
      "b100".U -> data // if have wider element, it will broken
    ))
  }
}

// TODO: more elegant
object genVStride extends VLSUConstants {
  def apply(uopIdx: UInt, stride: UInt): UInt = {
    LookupTree(uopIdx, List(
      0.U -> 0.U,
      1.U -> stride,
      2.U -> (stride << 1),
      3.U -> ((stride << 1).asUInt + stride),
      4.U -> (stride << 2),
      5.U -> ((stride << 2).asUInt + stride),
      6.U -> ((stride << 2).asUInt + (stride << 1)),
      7.U -> ((stride << 2).asUInt + (stride << 1) + stride)
    ))
  }
}
/**
 * generate uopOffset, not used in segment instruction
 * */
object genVUopOffset extends VLSUConstants {
  def apply(instType: UInt, isfof: Bool, uopidx: UInt, nf: UInt, eew: UInt, stride: UInt, alignedType: UInt): UInt = {
    val uopInsidefield = (uopidx >> nf).asUInt // when nf == 0, is uopidx

    val fofVUopOffset = (LookupTree(instType,List(
      "b000".U -> ( genVStride(uopInsidefield, stride) << (log2Up(VLENB).U - eew)   ) , // unit-stride fof
      "b100".U -> ( genVStride(uopInsidefield, stride) << (log2Up(VLENB).U - eew)   ) , // segment unit-stride fof
    ))).asUInt

    val otherVUopOffset = (LookupTree(instType,List(
      "b000".U -> ( uopInsidefield << alignedType                                   ) , // unit-stride
      "b010".U -> ( genVStride(uopInsidefield, stride) << (log2Up(VLENB).U - eew)   ) , // strided
      "b001".U -> ( 0.U                                                             ) , // indexed-unordered
      "b011".U -> ( 0.U                                                             ) , // indexed-ordered
      "b100".U -> ( uopInsidefield << alignedType                                   ) , // segment unit-stride
      "b110".U -> ( genVStride(uopInsidefield, stride) << (log2Up(VLENB).U - eew)   ) , // segment strided
      "b101".U -> ( 0.U                                                             ) , // segment indexed-unordered
      "b111".U -> ( 0.U                                                             )   // segment indexed-ordered
    ))).asUInt

    Mux(isfof, fofVUopOffset, otherVUopOffset)
  }
}



object searchVFirstUnMask extends VLSUConstants {
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

