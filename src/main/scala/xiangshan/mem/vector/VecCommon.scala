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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.rob.RobPtr

class VecOperand(implicit p: Parameters) extends XSBundleWithMicroOp {
  val vmask = UInt(VLEN.W) // the mask of inst which is readed from reg
  val vecData = UInt(VLEN.W)
  val baseaddr = UInt(VAddrBits.W) // base address from rs1
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

class VecDecode(implicit p: Parameters) extends XSBundle {
  val uop_segment_num = UInt(3.W)
  val uop_type = UInt(2.W)
  val mask_en = Bool()
  val uop_unit_stride_whole_reg = Bool()
  val uop_unit_stride_mask = Bool()
  val uop_unit_stride_fof = Bool()
  val uop_eew = UInt(3.W) // this is also the index width when the inst is a index load

  def apply(inst: UInt) = {
    this.uop_segment_num := inst(31, 29)
    this.uop_type := inst(27, 26)
    this.mask_en := inst(25)
    this.uop_unit_stride_whole_reg := (inst(24,20) === "b01000".U)
    this.uop_unit_stride_mask := (inst(24,20) === "b01011".U)
    this.uop_unit_stride_fof := (inst(24,20) === "b10000".U)
    this.uop_eew := inst(14, 12)
    this
  }
}

class OnlyVecExuOutput(implicit p: Parameters) extends XSBundle {
  val isvec = Bool()
  val vecdata = UInt(VLEN.W)
  val mask = UInt((VLEN/8).W)
  val rob_idx_valid = Vec(2, Bool())
  val inner_idx = Vec(2, UInt(3.W))
  val rob_idx = Vec(2, new RobPtr)
  val offset = Vec(2, UInt(4.W))
  val reg_offset = Vec(2, UInt(4.W))
  val exp = Bool()
  val is_first_ele = Bool()
  val exp_ele_index = UInt(8.W)
}

class VecExuOutput(implicit p: Parameters) extends ExuOutput {
  val vec = new OnlyVecExuOutput
}

class Uop2Flow(implicit p: Parameters) extends ExuInput(isVpu = true){
  val vstart   = UInt(8.W)
  val mask     = UInt(16.W)
  val eew      = UInt(3.W)
  val emul     = UInt(3.W)
  val instType = UInt(3.W)
  val uop_unit_stride_fof = Bool()
  val uop_unit_whole_reg = Bool()
  val agnedType = UInt(2.W)
  val uop_segment_num = UInt(3.W)
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

object GenVecLoadMask {
  def apply (instType: UInt, emul: UInt, eew: UInt, sew: UInt): UInt = {
    val mask = Wire(UInt(16.W))
    mask := UIntToOH(loadDataSize(instType = instType, emul = emul, eew = eew, sew = sew)) - 1.U
    mask
  }
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
  * when emul is less than or equal to 1, the nf is equal to uop_inner_idx;
  * when emul is equal to 2, the nf is equal to uop_inner_idx(2,1), and so on*/
object GenSegNfIdx {
  def apply (mul: UInt, uopIdx: UInt):UInt = { // mul means lmul or emul
    (LookupTree(mul,List(
      "b101".U -> uopIdx     , // 1/8
      "b110".U -> uopIdx     , // 1/4
      "b111".U -> uopIdx     , // 1/2
      "b000".U -> uopIdx     , // 1
      "b001".U -> uopIdx(2,1), // 2
      "b010".U -> uopIdx(2)  , // 4
      "b011".U -> 0.U          //8
    )))}
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
object EewLog2 {
  def apply (eew: UInt): UInt = {
    (LookupTree(eew,List(
      "b000".U -> "b000".U , // 1
      "b101".U -> "b001".U , // 2
      "b110".U -> "b010".U , // 4
      "b111".U -> "b011".U   // 8
    )))}
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

object GenEleIdx {
  def apply (instType: UInt, emul: UInt, lmul: UInt, eew: UInt, sew: UInt, uopIdx:UInt, flowIdx: UInt):UInt = {
    val eleIdx = Wire(UInt(7.W))
    when (instType(1,0) === "b00".U || instType(1,0) === "b10".U || emul.asSInt > lmul.asSInt) {
      eleIdx := (uopIdx << Log2Num((MulDataSize(emul) >> eew(1,0)).asUInt)).asUInt + flowIdx
    }.otherwise {
      eleIdx := (uopIdx << Log2Num((MulDataSize(lmul) >> sew(1,0)).asUInt)).asUInt + flowIdx
    }
    eleIdx
  }
}
