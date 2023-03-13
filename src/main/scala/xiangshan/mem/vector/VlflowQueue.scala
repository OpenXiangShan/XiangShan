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

class VecLoadPipelineBundle(implicit p: Parameters) extends XSBundleWithMicroOp{
  // val pipe_accept = Input(Bool()) need pipe ready to send req to pipe
  val vaddr = Output(UInt(VAddrBits.W))
  val sram_mask = Output(UInt(8.W))
  val stq_forward_mask = Output(UInt((CacheLineSize / 8).W))//TODO:need add support logic in LoadUnit
  val flow_entry_index = Output(UInt(5.W))
  val uop_unit_stride_fof = Output(Bool())
}

// For example: 0 is 8, 1 is 16, 2 is 32, 3 is 64
// So we only use two bits
class Flow2UopBuddle(implicit p: Parameters) extends XSBundle{
  val flow_index = Output(UInt(5.W)) // index of flow entry
  val flow_inner_index = Output(UInt(4.W)) // An uop can be split to 0 to 15//TODO: need ???
  val flow_offset = Output(UInt(6.W)) // Offset of vaddr // TODO：Maybe every flow_robIdx need a flow_offset
  val flow_robIdx = Output(UInt(log2Ceil(RobSize).W))  //change width TODO：Maybe need more flow_robIdx,
}

class VlflowQueueIOBundle(implicit p: Parameters) extends XSBundle {
  val loadRegIn = Vec(2, Flipped(Decoupled(new VecOperand())))
  val loadPipeOut = Vec(LoadPipelineWidth, Decoupled(new VecLoadPipelineBundle))
  //val loadFlow2UopOut = Vec(LoadPipelineWidth, Decoupled(new Flow2UopBuddle()))
}

class VlflowBundle(implicit p: Parameters) extends XSBundle {
  val uop = new MicroOp
  val inner_index = UInt(5.W)//TODO: need???
  val vaddr = UInt(VAddrBits.W)
  val sram_mask = UInt(8.W)
  val stqfwd_mask = UInt((CacheLineSize / 8).W)
  val unit_stride_fof = Bool()
}

class VlflowQueue(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper
{
  val io = IO(new VlflowQueueIOBundle())

  println("LoadFlowQueue: size:" + VlFlowSize)

  // TODO: merge these to an FlowQueue Entry?
  val flow_entry = Reg(Vec(2, Vec(VlFlowSize, new VlflowBundle))) //TODO: VlFlowSize need to be smaller
  val flow_entry_valid = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(VlFlowSize)(false.B)))))
  val LoadInstDec = Wire(Vec(2, new VecDecode()))

  val needAlloc = Wire(Vec(2, Bool()))

  for(i <- 0 until 2){
    // TODO: Why ===, Should be =/= ?
    //  Should be confirmed
    needAlloc(i) := !flow_entry.map(_.map(_.uop.robIdx.value === io.loadRegIn(i).bits.uop.robIdx.value).reduce(_ || _)).reduce(_ || _) && io.loadRegIn(i).valid
    LoadInstDec(i).apply(io.loadRegIn(i).bits.uop.cf.instr)
  }

  //val flowWriteNumber = Wire(Vec(2, UInt(5.W)))
  //val startAddr = Wire(Vec(UInt(VAddrBits.W)))
  //val flowWriteNumber = WireInit(VecInit(2, 1.U(4.W)))
  val flowWriteNumber = Seq(1,1)

  //val enqPtrExt = RegInit(VecInit((0 until flowWriteNumber).map(_.U.asTypeOf(new VlflowPtr))))
  val enqPtr = RegInit(VecInit(Seq.fill(2)(0.U.asTypeOf(new VlflowPtr))))
  val deqPtr = RegInit(VecInit(Seq.fill(2)(0.U.asTypeOf(new VlflowPtr))))

  for(i <- 0 until flowWriteNumber(0)) {
    val index = (enqPtr(0) + i.U).value
    when(needAlloc(0)) {
      flow_entry(0)(index).uop := io.loadRegIn(0).bits.uop
      // TODO: implement inner_index?
      flow_entry(0)(index).inner_index := 0.U
      flow_entry(0)(index).sram_mask := Fill(8, true.B)
      flow_entry(0)(index).stqfwd_mask := Fill((CacheLineSize / 8), true.B)
      flow_entry(0)(index).vaddr := io.loadRegIn(0).bits.baseaddr
      flow_entry(0)(index).unit_stride_fof := LoadInstDec(0).uop_unit_stride_fof
      flow_entry_valid(0)(index) := true.B
    }
  }

  for (i <- 0 until flowWriteNumber(1)) {
    val index = (enqPtr(1) + i.U).value
    when(needAlloc(1) && (!needAlloc(0) || io.loadRegIn(0).bits.uop.robIdx.value =/= io.loadRegIn(1).bits.uop.robIdx.value)) {
      flow_entry(1)(index).uop := io.loadRegIn(1).bits.uop
      // TODO: implement inner_index?
      flow_entry(1)(index).inner_index := 0.U
      flow_entry(1)(index).sram_mask := Fill(8, true.B)
      flow_entry(1)(index).stqfwd_mask := Fill(CacheLineSize / 8, true.B)
      flow_entry(1)(index).vaddr := io.loadRegIn(1).bits.baseaddr
      flow_entry(1)(index).unit_stride_fof := LoadInstDec(1).uop_unit_stride_fof
      flow_entry_valid(1)(index) := true.B
    }
  }

  enqPtr(0) := enqPtr(0) + flowWriteNumber(0).asUInt
  enqPtr(1) := enqPtr(1) + flowWriteNumber(1).asUInt

  // output from the index of deqPtr
  for (i <- 0 until LoadPipelineWidth) {
    val index = deqPtr(i).value
    // TODO: Need to do some changes
    //  1. DontCare?
    //  2. Other information?
    io.loadPipeOut(i).bits := DontCare
    //io.loadFlow2UopOut(i).bits := DontCare
    io.loadPipeOut(i).valid := false.B
    //io.loadFlow2UopOut(i).valid := false.B
    when (flow_entry_valid(i)(index)){
      io.loadPipeOut(i).valid := true.B
      io.loadPipeOut(i).bits.flow_entry_index := index
      io.loadPipeOut(i).bits.vaddr := flow_entry(i)(index).vaddr
      io.loadPipeOut(i).bits.uop_unit_stride_fof := flow_entry(i)(index).unit_stride_fof
      //io.loadFlow2UopOut(i).valid := true.B
      //io.loadFlow2UopOut(i).bits.flow_robIdx := flow_entry(i)(index).uop.robIdx.value
      //io.loadFlow2UopOut(i).bits.flow_index := index
      //io.loadFlow2UopOut(i).bits.flow_inner_index := flow_entry(i)(index).inner_index
      //io.loadFlow2UopOut(i).bits.flow_offset := flow_entry(i)(index).vaddr(5, 0)
      when (io.loadPipeOut(i).fire) { //io.loadFlow2UopOut(i).fire
        flow_entry_valid(i)(index) := false.B
        deqPtr(i) := deqPtr(i) + 1.U
      }
    }

    val validCount = Wire(Vec(2, UInt(4.W)))
    val allowEnqueue = Wire(Vec(2, Bool()))
    for(i <- 0 until 2) {
      validCount(i) := distanceBetween(enqPtr(i), deqPtr(i))
      allowEnqueue(i) := validCount(i) >= 16.U
      io.loadRegIn(i).ready := allowEnqueue(i)
    }
  }
}

