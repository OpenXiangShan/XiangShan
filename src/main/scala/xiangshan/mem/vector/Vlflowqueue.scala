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

class VlflowPtr(implicit p: Parameters) extends CircularQueuePtr[VlflowPtr](
  p => p(XSCoreParamsKey).VlFlowSize
){
}

object VlflowPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): VlflowPtr = {
    val ptr = Wire(new VlflowPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

object lmulDataSize {
  def apply (lmul: UInt): UInt = {
    (LookupTree(lmul,List(
      "b101".U -> 2.U , // 1/8
      "b110".U -> 4.U , // 1/4
      "b111".U -> 8.U , // 1/2
      "b000".U -> 16.U, // 1
      "b001".U -> 16.U, // 2
      "b010".U -> 16.U, // 4
      "b011".U -> 16.U  // 8
    )))}
}

object genDataSizeMask {
  def apply (dataSize: UInt, offset: UInt): UInt = {
    (LookupTree(dataSize,List(
      "b0000".U -> 0x1.U,
      "b0001".U -> 0x3.U,
      "b0010".U -> 0x7.U,
      "b0011".U -> 0xf.U,
      "b0100".U -> 0x1f.U,
      "b0101".U -> 0x3f.U,
      "b0110".U -> 0x7f.U,
      "b0111".U -> 0xff.U,
      "b1000".U -> 0x1ff.U,
      "b1001".U -> 0x3ff.U,
      "b1010".U -> 0x7ff.U,
      "b1011".U -> 0xfff.U,
      "b1100".U -> 0x1fff.U,
      "b1101".U -> 0x3fff.U,
      "b1110".U -> 0x7fff.U,
      "b1111".U -> 0xffff.U
    )) << offset(3,0)).asUInt
  }
}

object genVecMask {
  def apply (instType: UInt, dataSize: UInt, offset: UInt, lmul: UInt): UInt = {
    (LookupTree(instType,List(
      "b000001".U ->lmulDataSize(lmul), //unit-stride
      "b000010".U -> 16.U,//strided
      "b000100".U -> 16.U,//index
      "b001000".U -> 16.U,//segment unit-stride
      "b010000".U -> 16.U,//segment strided
      "b100000".U -> 16.U //segment index
    )))}
}

object genVecAddr {
  def apply(baseaddr: UInt, instType: UInt, flow_inner_idx: UInt): UInt = {
    (LookupTree(instType,List(
      "b000001".U -> (baseaddr + (flow_inner_idx << 4.U).asUInt), //unit-stride
      "b000010".U ->  baseaddr,//strided
      "b000100".U ->  baseaddr,//index
      "b001000".U ->  baseaddr,//segment unit-stride
      "b010000".U ->  baseaddr,//segment strided
      "b100000".U ->  baseaddr //segment index
    )))}
}

class VecLoadPipelineBundle(implicit p: Parameters) extends XSBundleWithMicroOp{
  val vaddr               = UInt(VAddrBits.W)
  val mask                = UInt((VLEN/8).W)
  //val dataSize            = UInt(4.W)   //the memory access width of flow entry
  val uop_unit_stride_fof = Bool()
  val rob_idx_valid       = Vec(2,Bool())
  val rob_idx             = Vec(2,UInt(log2Up(RobSize).W))
  val reg_offset          = Vec(2,UInt(4.W))
  val offset              = Vec(2,UInt(4.W))
}

class VlflowQueueIOBundle(implicit p: Parameters) extends XSBundle {
  val loadRegIn   = Vec(2, Flipped(Decoupled(new VecOperand())))
  val loadPipeOut = Vec(LoadPipelineWidth, Decoupled(new VecLoadPipelineBundle))
}

class VlflowBundle(implicit p: Parameters) extends XSBundle {
  val uop               = new MicroOp
  val vaddr             = UInt(VAddrBits.W)
  val mask              = UInt((VLEN/8).W)
  //val dataSize          = UInt(4.W)
  val unit_stride_fof   = Bool()
  val rob_idx_valid     = Vec(2,Bool())
  val rob_idx           = Vec(2,UInt(log2Up(RobSize).W))
  val reg_offset        = Vec(2,UInt(4.W)) //Which element to write
  val offset            = Vec(2,UInt(4.W))
}

class VlflowQueue(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper
{
  val io = IO(new VlflowQueueIOBundle())
  println("LoadFlowQueue: size:" + VlFlowSize)

  // TODO: merge these to an FlowQueue Entry?
  val flow_entry       = Reg(Vec(2, Vec(VlFlowSize, new VlflowBundle)))
  val flow_entry_valid = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(VlFlowSize)(false.B)))))
  val LoadInstDec      = Wire(Vec(2, new VecDecode()))

  val needAlloc       = Wire(Vec(2, Bool()))
  val baseAddr        = Wire(Vec(2, UInt(VAddrBits.W)))
  val dataWidth       = Wire(Vec(2, UInt(10.W)))
  val vend_0          = Wire(Vec(2,UInt(5.W)))
  val vend_1          = Wire(Vec(2,UInt(6.W)))
  val vend_2          = Wire(Vec(2,UInt(7.W)))
  val vend_3          = Wire(Vec(2,UInt(8.W)))
  val flowWriteNumber = Wire(Vec(2, UInt(3.W)))
  val realFlowNum     = Wire(Vec(2, UInt(4.W)))

  val loadInstDec = Wire(Vec(2,new VecDecode()))
  val index       = Wire(Vec(2, UInt(5.W)))
  val cross128    = Wire(Vec(2, Bool()))
  val startRobIdx = Wire(Vec(2,UInt(log2Ceil(RobSize).W)))
  val vaddr       = Wire(Vec(2, UInt(VAddrBits.W)))

  val validCount = Wire(Vec(2, UInt(4.W)))
  val allowEnqueue = Wire(Vec(2, Bool()))


    for(i <- 0 until exuParameters.LduCnt){
    // TODO: Why ===, Should be =/= ?
    //  Should be confirmed
    needAlloc(i)    := !flow_entry.map(_.map(_.uop.robIdx.value === io.loadRegIn(i).bits.uop.robIdx.value).reduce(_ || _)).reduce(_ || _) && io.loadRegIn(i).valid
    loadInstDec(i)  := LoadInstDec(i).apply(io.loadRegIn(i).bits.uop.cf.instr)
    dataWidth(i)    := io.loadRegIn(i).bits.vl << loadInstDec(i).uop_eew(1,0)
    vend_0(i)       := baseAddr(i)(3,0) + dataWidth(i)(3,0)
    vend_1(i)       := baseAddr(i)(4,0) + dataWidth(i)(4,0)
    vend_2(i)       := baseAddr(i)(5,0) + dataWidth(i)(5,0)
    vend_3(i)       := baseAddr(i)(6,0) + dataWidth(i)(6,0)
    flowWriteNumber(i) := Mux(vend_3(i)(7) === 1.U, vend_3(i)(7,4), Mux(vend_2(i)(6) === 1.U, vend_2(i)(6,4), Mux(vend_1(i)(5) === 1.U, vend_1(i)(5,4), vend_0(i)(4))))
  }

  val enqPtr = RegInit(VecInit(Seq.fill(2)(0.U.asTypeOf(new VlflowPtr))))
  val deqPtr = RegInit(VecInit(Seq.fill(2)(0.U.asTypeOf(new VlflowPtr))))

  for(i <- 0 until LoadPipelineWidth) {
    baseAddr(i)     := io.loadRegIn(i).bits.baseaddr
    cross128(i)     := baseAddr(i)(3, 0) =/= 0.U(4.W)
    realFlowNum(i)  := flowWriteNumber(i).asUInt + cross128(i)
    enqPtr(i).value := enqPtr(i).value + realFlowNum(i)
  }

  for(i <- 0 until exuParameters.LduCnt) {
    validCount(i) := distanceBetween(enqPtr(i), deqPtr(i))
    allowEnqueue(i) := validCount(i) >= 16.U
    io.loadRegIn(i).ready := allowEnqueue(i)
  }

  for (i <- 0 until LoadPipelineWidth) {
    startRobIdx(i) := DontCare
    vaddr(i) := DontCare
    index(i) := DontCare
    when (needAlloc(i)) {
      startRobIdx(i) := io.loadRegIn(i).bits.uop.robIdx.value - io.loadRegIn(i).bits.inner_idx
      for (j <- 0 until 9) {
        when (j.U < realFlowNum(i)) {
          index(i) := enqPtr(i).value + j.U
          flow_entry(i)(index(i)) := DontCare
          flow_entry_valid(i)(index(i))           := true.B
          flow_entry(i)(index(i)).uop             := io.loadRegIn(i).bits.uop
          flow_entry(i)(index(i)).unit_stride_fof := LoadInstDec(i).uop_unit_stride_fof
          //flow_entry(i)(index(i)).dataSize        := genDataSize("b000001".U,io.loadRegIn(i).bits.lmul)
          vaddr(i)                                := genVecAddr(baseAddr(i),"b000001".U,j.U)
          flow_entry(i)(index(i)).mask            := genVecMask(instType = "b000001".U, dataSize = 0.U, offset = 0.U, lmul = io.loadRegIn(i).bits.lmul)
          flow_entry(i)(index(i)).vaddr           := vaddr(i)

          when (j.U =/= realFlowNum(i) - 1.U) {
            flow_entry(i)(index(i)).rob_idx_valid(0)     := true.B
            flow_entry(i)(index(i)).rob_idx(0)           := startRobIdx(i) + j.U
            flow_entry(i)(index(i)).offset(0)            := vaddr(i)(3,0)
            flow_entry(i)(index(i)).reg_offset(0)        := 0.U
          }.otherwise {
            flow_entry(i)(index(i)).rob_idx_valid(0)     := cross128(i)
            flow_entry(i)(index(i)).rob_idx(0)           := startRobIdx(i) + j.U - cross128(i).asUInt
            flow_entry(i)(index(i)).offset(0)            := 0.U
            flow_entry(i)(index(i)).reg_offset(0)        := 16.U - genVecAddr(baseAddr(i),"b000001".U,(j.U-1.U))(3,0)
          }

          when (j.U =/= 0.U) {
            flow_entry(i)(index(i)).rob_idx_valid(1)     := cross128(i)
            flow_entry(i)(index(i)).rob_idx(1)           := startRobIdx(i) + j.U - cross128(i).asUInt
            flow_entry(i)(index(i)).offset(1)            := 0.U
            flow_entry(i)(index(i)).reg_offset(1)        := 16.U - genVecAddr(baseAddr(i),"b000001".U,(j.U-1.U))(3,0)
          }
        }
      }
    }
  }


  // flow deqPtr
  for (i <- 0 until LoadPipelineWidth) {
    // TODO: Need to do some changes
    //  1. DontCare?
    //  2. Other information?
    io.loadPipeOut(i).bits := DontCare
    io.loadPipeOut(i).valid := false.B
    when (flow_entry_valid(i)(deqPtr(i).value)){
      io.loadPipeOut(i).valid                    := true.B
      io.loadPipeOut(i).bits.uop_unit_stride_fof := flow_entry(i)(deqPtr(i).value).unit_stride_fof
      io.loadPipeOut(i).bits.vaddr               := flow_entry(i)(deqPtr(i).value).vaddr
      //io.loadPipeOut(i).bits.dataSize            := flow_entry(i)(deqPtr(i).value).dataSize
      io.loadPipeOut(i).bits.mask                := flow_entry(i)(deqPtr(i).value).mask
      io.loadPipeOut(i).bits.rob_idx_valid       := flow_entry(i)(deqPtr(i).value).rob_idx_valid
      io.loadPipeOut(i).bits.rob_idx             := flow_entry(i)(deqPtr(i).value).rob_idx
      io.loadPipeOut(i).bits.offset              := flow_entry(i)(deqPtr(i).value).offset
      io.loadPipeOut(i).bits.reg_offset          := flow_entry(i)(deqPtr(i).value).reg_offset
    }
  }

  for (i <- 0 until LoadPipelineWidth) {
    when (io.loadPipeOut(i).fire) {
      flow_entry_valid(i)(deqPtr(i).value) := false.B
      deqPtr(i) := deqPtr(i) + 1.U
    }
  }

}