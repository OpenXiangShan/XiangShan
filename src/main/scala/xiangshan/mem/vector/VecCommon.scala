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

/**
  * Common used parameters or functions in vlsu
  */
trait VLSUConstants {
  val VLEN = 128
  def VLENB = VLEN/8
  def vOffsetBits = log2Up(VLENB) // bits-width to index offset inside a vector reg

  def alignTypes = 4 // eew/sew = 1/2/4/8
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

  def getByte(data: UInt, i: Int = 0) = getSlice(data, i, 8)
  def getHalfWord(data: UInt, i: Int = 0) = getSlice(data, i, 16)
  def getWord(data: UInt, i: Int = 0) = getSlice(data, i, 32)
  def getDoubleWord(data: UInt, i: Int = 0) = getSlice(data, i, 64)
}

trait HasVLSUParameters extends HasXSParameter with VLSUConstants {
  override val VLEN = coreParams.VLEN
  def isUnitStride(instType: UInt) = instType(1, 0) === "b00".U
  def isStrided(instType: UInt) = instType(1, 0) === "b10".U
  def isIndexed(instType: UInt) = instType(0) === "b1".U
  def isNotIndexed(instType: UInt) = instType(0) === "b0".U
  def isSegment(instType: UInt) = instType(2) === "b1".U

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
}
abstract class VLSUModule(implicit p: Parameters) extends XSModule
  with HasVLSUParameters
  with HasCircularQueuePtrHelper
abstract class VLSUBundle(implicit p: Parameters) extends XSBundle
  with HasVLSUParameters

class VLSUBundleWithMicroOp(implicit p: Parameters) extends VLSUBundle {
  val uop = new DynInst
}

// Where is VecOperand used?
class VecOperand(implicit p: Parameters) extends VLSUBundleWithMicroOp {
  val vmask = UInt(VLEN.W) // the mask of inst which is readed from reg
  val vecData = UInt(VLEN.W)
  val baseAddr = UInt(VAddrBits.W) // base address from rs1
  val stride = UInt(XLEN.W) // stride from rs2
  val index = UInt(VLEN.W) // index from vs2
  val pvd = UInt(5.W) // physical vector register destination
  val lmul = UInt(3.W)
  val sew = UInt(2.W)
  val vma = Bool()
  val vta = Bool()
  val inner_idx = UInt(3.W) // the number index among 8 uop
  val vl = UInt(8.W)
  // TODO: How will OOO calculatr vector register numbers?
  //  (EEW / SEW) * LMUL or (vl * EEW) / VLEN ?
  //  So OOO will always use eew ?
  // val eew = UInt(3.W)
  val total_num = UInt(3.W) // An inst to how many uops
}

class VecDecode(implicit p: Parameters) extends VLSUBundle {
  val uop_segment_num = UInt(3.W)
  val uop_type = UInt(2.W)
  val mask_en = Bool()
  val uop_unit_stride_whole_reg = Bool()
  val uop_unit_stride_mask = Bool()
  val uop_unit_stride_fof = Bool()
  val uop_eew = UInt(ewBits.W) // this is also the index width when the inst is a index load

  def apply(inst: UInt) = {
    this.uop_segment_num := inst(31, 29)
    this.uop_type := inst(27, 26)
    this.mask_en := inst(25)
    this.uop_unit_stride_whole_reg := (inst(24,20) === "b01000".U)
    this.uop_unit_stride_mask := (inst(24,20) === "b01011".U)
    this.uop_unit_stride_fof := (inst(24,20) === "b10000".U)
    this.uop_eew := inst(12 + ewBits - 1, 12)
    this
  }

  def isUnitStride = uop_type === "b00".U
  def isStrided = uop_type === "b10".U
  def isIndexed = uop_type(0) === "b1".U
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
  val exp = Bool()
  val is_first_ele = Bool()
  val elemIdx = UInt(elemIdxBits.W) // element index
  val elemIdxInsideVd = UInt(elemIdxBits.W) // element index in scope of vd
  val uopQueuePtr = new VluopPtr
  val flowPtr = new VlflowPtr
}

class VecExuOutput(implicit p: Parameters) extends MemExuOutput with HasVLSUParameters {
  val vec = new OnlyVecExuOutput
}

class VecStoreExuOutput(implicit p: Parameters) extends MemExuOutput with HasVLSUParameters {
  val elemIdx = UInt(elemIdxBits.W)
  val uopQueuePtr = new VsUopPtr
}

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
  val exp               = Bool()
  val elemIdx           = UInt(elemIdxBits.W)
  val is_first_ele      = Bool()
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

// object GenVecLoadMask extends VLSUConstants {
//   def apply(alignedType: UInt, vaddr: UInt): UInt = {
//     LookupTree(alignedType, List(
//       "b00".U -> 0x1.U, // b1
//       "b01".U -> 0x3.U, // b11
//       "b10".U -> 0xf.U, // b1111
//       "b11".U -> 0xff.U // b11111111
//     )) << vaddr(vOffsetBits - 1, 0)
//   }
// }

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
/*
object RegFLowCnt {
  def apply (emul: UInt, lmul:UInt, eew: UInt, uopIdx: UInt, flowIdx: UInt): UInt = {

    (LookupTree(Cat(emul,lmul),List(
      "b001000".U -> ((uopIdx(0  ) << Log2Num((16.U >> eew(1,0)).asUInt)).asUInt + flowIdx),//emul = 2,lmul = 1
      "b010000".U -> ((uopIdx(1,0) << Log2Num((16.U >> eew(1,0)).asUInt)).asUInt + flowIdx),//emul = 4,lmul = 1
      "b011000".U -> ((uopIdx(2,0) << Log2Num((16.U >> eew(1,0)).asUInt)).asUInt + flowIdx),//emul = 8,lmul = 1
      "b010001".U -> ((uopIdx(0  ) << Log2Num((16.U >> eew(1,0)).asUInt)).asUInt + flowIdx),//emul = 4,lmul = 2
      "b011001".U -> ((uopIdx(1,0) << Log2Num((16.U >> eew(1,0)).asUInt)).asUInt + flowIdx),//emul = 8,lmul = 2
      "b011010".U -> ((uopIdx(0  ) << Log2Num((16.U >> eew(1,0)).asUInt)).asUInt + flowIdx) //emul = 8,lmul = 4
    )))}
}

object AddrFLowCnt {
  def apply (emul: UInt, lmul:UInt, sew:UInt, uopIdx: UInt, flowIdx: UInt):UInt = {
    (LookupTree(Cat(lmul,emul),List(
      "b001000".U -> ((uopIdx(0  ) << Log2Num((MulDataSize(lmul) >> sew(1,0)).asUInt)).asUInt + flowIdx),//lmul = 2, emul = 1
      "b010000".U -> ((uopIdx(1,0) << Log2Num((MulDataSize(lmul) >> sew(1,0)).asUInt)).asUInt + flowIdx),//lmul = 4, emul = 1
      "b011000".U -> ((uopIdx(2,0) << Log2Num((MulDataSize(lmul) >> sew(1,0)).asUInt)).asUInt + flowIdx),//lmul = 8, emul = 1
      "b010001".U -> ((uopIdx(0  ) << Log2Num((MulDataSize(lmul) >> sew(1,0)).asUInt)).asUInt + flowIdx),//lmul = 4, emul = 2
      "b011001".U -> ((uopIdx(1,0) << Log2Num((MulDataSize(lmul) >> sew(1,0)).asUInt)).asUInt + flowIdx),//lmul = 8, emul = 2
      "b011011".U -> ((uopIdx(0  ) << Log2Num((MulDataSize(lmul) >> sew(1,0)).asUInt)).asUInt + flowIdx) //lmul = 8, emul = 4
    )))}
}
*/

object RegFLowCnt {
  def apply (emulNum: UInt, lmulNum:UInt, eew: UInt, uopIdx: UInt, flowIdx: UInt):UInt = {
    (LookupTree(emulNum/lmulNum,List(
      //"d1".U -> flowIdx,
      "d2".U -> ((uopIdx(0  ) << Log2Num((16.U >> eew(1,0)).asUInt)).asUInt + flowIdx),
      "d4".U -> ((uopIdx(1,0) << Log2Num((16.U >> eew(1,0)).asUInt)).asUInt + flowIdx),
      "d8".U -> ((uopIdx(2,0) << Log2Num((16.U >> eew(1,0)).asUInt)).asUInt + flowIdx)
    )))}
}

object AddrFLowCnt {
  def apply (emulNum: UInt, lmulNum:UInt, sew:UInt, uopIdx: UInt, flowIdx: UInt):UInt = {
    (LookupTree(lmulNum/emulNum,List(
      "d1".U -> flowIdx,
      "d2".U -> ((uopIdx(0  ) << Log2Num((16.U >> sew(1,0)).asUInt)).asUInt + flowIdx),
      "d4".U -> ((uopIdx(1,0) << Log2Num((16.U >> sew(1,0)).asUInt)).asUInt + flowIdx),
      "d8".U -> ((uopIdx(2,0) << Log2Num((16.U >> sew(1,0)).asUInt)).asUInt + flowIdx)
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

/**
  * when emul is less than or equal to 1, the nf is equal to uopIdx;
  * when emul is equal to 2, the nf is equal to uopIdx >> 1, and so on*/
object GenSegNfIdx {
  def apply (mul: UInt, uopIdx: UInt):UInt = { // mul means lmul or emul
    (LookupTree(mul,List(
      "b101".U -> uopIdx,           // 1/8
      "b110".U -> uopIdx,           // 1/4
      "b111".U -> uopIdx,           // 1/2
      "b000".U -> uopIdx,           // 1
      "b001".U -> (uopIdx >> 1.U),  // 2
      "b010".U -> (uopIdx >> 2.U),  // 4
      "b011".U -> (uopIdx >> 3.U),  // 8
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

object GenSegNfIdxMul {
  def apply (emul: UInt, lmul: UInt, uopIdx: UInt):UInt = {
    (LookupTree(Cat(emul,lmul),List(
      "b001000".U -> uopIdx(5,1), //emul = 2,lmul = 1
      "b010000".U -> uopIdx(5,2), //emul = 4,lmul = 1
      "b011000".U -> uopIdx(5,3), //emul = 8,lmul = 1
      "b010001".U -> uopIdx(5,3), //emul = 4,lmul = 2
      "b011001".U -> uopIdx(5,4), //emul = 8,lmul = 2
      "b011010".U -> uopIdx(5,5)  //emul = 8,lmul = 4
    )))}
}

/**
  * when emul is less than or equal to 1, only one segEmulIdx, so the segEmulIdx is 0.U;
  * when emul is equal to 2, the segEmulIdx is equal to uopIdx(0), and so on*/
object GenSegMulIdx {
  def apply (mul: UInt, uopIdx: UInt): UInt = { //mul means emul or lmul
    (LookupTree(mul,List(
      "b101".U -> 0.U        , // 1/8
      "b110".U -> 0.U        , // 1/4
      "b111".U -> 0.U        , // 1/2
      "b000".U -> 0.U        , // 1
      "b001".U -> uopIdx(0)  , // 2
      "b010".U -> uopIdx(1,0), // 4
      "b011".U -> uopIdx(2,0)  //8
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
  def apply(eew: UInt): UInt = ZeroExt(eew(1, 0), ewBits)
}

/**
  * unit-stride instructions don't use this method;
  * other instructions generate realFlowNum by EmulDataSize >> eew(1,0),
  * EmulDataSize means the number of bytes that need to be written to the register,
  * eew(1,0) means the number of bytes written at once*/
object GenRealFlowNum {
  def apply (instType: UInt, emul: UInt, lmul: UInt, eew: UInt, sew: UInt): UInt = {
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
  // def apply(alignedType: UInt, uopIdx: UInt, flowIdx: UInt): UInt = {
  //   LookupTree(
  //     alignedType,
  //     (0 until alignTypes).map(i =>
  //       i.U -> ((uopIdx ## flowIdx(log2Up(VLENB) - i - 1, 0))(log2Up(maxElemNum) - 1, 0))
  //     )
  //   )
  // }
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
    (vl >> 3.U)
  }
}

object GenUopByteMask {
  def apply(flowMask: UInt, alignedType: UInt): UInt = {
    LookupTree(alignedType, List(
      "b00".U -> flowMask,
      "b01".U -> FillInterleaved(2, flowMask),
      "b10".U -> FillInterleaved(4, flowMask),
      "b11".U -> FillInterleaved(8, flowMask)
    ))
  }
}

object GenFlowMaskInsideReg extends VLSUConstants {
  def apply(alignedType: UInt, elemIdx: UInt): UInt = {
    LookupTree(alignedType, List(
      "b00".U -> UIntToOH(elemIdx(3, 0)),
      "b01".U -> FillInterleaved(2, UIntToOH(elemIdx(2, 0))),
      "b10".U -> FillInterleaved(4, UIntToOH(elemIdx(1, 0))),
      "b11".U -> FillInterleaved(8, UIntToOH(elemIdx(0)))
    ))
  }
}

// TODO: delete this in vs flow queue
object GenEleIdx {
  def apply(instType: UInt, emul: UInt, lmul: UInt, eew: UInt, sew: UInt, uopIdx: UInt, flowIdx: UInt): UInt = {
    val eleIdx = Wire(UInt(7.W))
    when (instType(1,0) === "b00".U || instType(1,0) === "b10".U || emul.asSInt > lmul.asSInt) {
      eleIdx := (uopIdx << Log2Num((MulDataSize(emul) >> eew(1,0)).asUInt)).asUInt + flowIdx
    }.otherwise {
      eleIdx := (uopIdx << Log2Num((MulDataSize(lmul) >> sew(1,0)).asUInt)).asUInt + flowIdx
    }
    eleIdx
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

// object GenFieldMask {
//   def apply(instType: UInt, emul: UInt, lmul: UInt, eew: UInt, sew: UInt): UInt = {
//     val isSegment = instType(2)
//     val isIndexed = instType(0)
//     val alignedType = Mux(isIndexed, sew(1, 0), eew(1, 0))
//     val mul = Mux(isIndexed, lmul, emul)
//     val vlmaxMask = GenVLMAX(lmul, sew) - 1.U
//     val mulMask = LookupTree(alignedType, List(
//       "b00".U -> "b01111".U,
//       "b01".U -> "b00111".U,
//       "b10".U -> "b00011".U,
//       "b11".U -> "b00001".U
//     ))
//     Mux(
//       !isSegment || mul.asSInt >= 0.S,
//       vlmaxMask,
//       mulMask
//     )
//   }
// }