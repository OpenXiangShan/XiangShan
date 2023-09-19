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
import chisel3.{util, _}
import chisel3.util._
import utils._
import utility._
import xiangshan._

class VsUopPtr(implicit p: Parameters) extends CircularQueuePtr[VsUopPtr](
  p => p(XSCoreParamsKey).VsUopSize
){
}

object VsUopPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): VsUopPtr = {
    val ptr = Wire(new VsUopPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class VsUopQueueIOBundle (implicit p: Parameters) extends XSBundle {
  val redirect = Flipped(ValidIO(new Redirect))
  val storeIn  = Flipped(Decoupled(new ExuInput(isVpu = true)))
  val flowIssue = Vec(VecStorePipelineWidth, Decoupled(new VsFlowBundle()))
  val flowWriteback = Vec(VecStorePipelineWidth, Flipped(DecoupledIO(new VecStoreExuOutput())))
  val uopWriteback = DecoupledIO(new ExuOutput(isVpu = true))
}
class VsUopQueue(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new VsUopQueueIOBundle())

  println("StoreUopQueue: size:" + VsUopSize)

  /**
    * TODO @zlj
    */
  io <> DontCare

  // val valid = RegInit(VecInit(Seq.fill(VsUopSize)(false.B)))
  // val vsUopEntry = Reg(Vec(VsUopSize,new Uop2Flow()))

  // val loadInstDec = Wire(Vec(VecStorePipelineWidth,new VecDecode()))
  // val eew = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  // val sew = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  // val lmul = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  // val emul = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  // val isSegment = Wire(Vec(VecStorePipelineWidth, Bool()))
  // val instType = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  // val storeInValid = WireInit(VecInit(Seq.fill(VecStorePipelineWidth)(false.B)))
  // val needFlush = WireInit(VecInit(Seq.fill(VsUopSize)(false.B)))
  // val free = WireInit(VecInit(Seq.fill(VecStorePipelineWidth)(0.U(VsUopSize.W))))

  // def getRemBits(input: UInt)(rem: Int): UInt = {
  //   VecInit((0 until VsUopSize / VecStorePipelineWidth).map(i => {input(VecStorePipelineWidth * i + rem)})).asUInt
  // }

  // val uopFreeList = Module(new VsUopFreeList(
  //                           size = VsUopSize,
  //                           allocWidth = VecStorePipelineWidth,
  //                           freeWidth = 4,
  //                           moduleName = "vsUopFreeList"))

  // for (i <- 0 until VecStorePipelineWidth) {
  //   io.storeIn(i).ready :=  uopFreeList.io.accllReq(i).ready
  // }

  // for (i <- 0 until VecStorePipelineWidth) {
  //   storeInValid(i) := !io.storeIn(i).bits.uop.robIdx.needFlush(io.redirect) && io.storeIn(i).fire
  // }

  // /**
  //   * Redirection occurred, flush VsUopQueue */
  // for (entry <- 0 until VsUopSize) {
  //   needFlush(entry) := vsUopEntry(entry).uop.robIdx.needFlush(io.redirect) && valid(entry)
  //   when(needFlush(entry)) {
  //     valid(entry) := false.B
  //   }
  // }

  // val lastRedriect = RegNext(io.redirect)
  // when (lastRedriect.valid) {
  //   uopFreeList.io.free := RegNext(needFlush.asUInt)
  // }.otherwise {
  //   uopFreeList.io.free := free.reduce(_|_)
  // }

  // for (i <- 0 until VecStorePipelineWidth) {
  //   loadInstDec(i).apply(io.storeIn(i).bits.uop.cf.instr)
  //   eew(i)       := loadInstDec(i).uop_eew
  //   sew(i)       := io.storeIn(i).bits.uop.ctrl.vconfig.vtype.vsew
  //   lmul(i)      := io.storeIn(i).bits.uop.ctrl.vconfig.vtype.vlmul
  //   emul(i)      := EewLog2(eew(i)) - sew(i) + lmul(i)
  //   isSegment(i) := loadInstDec(i).uop_segment_num =/= "b000".U && !loadInstDec(i).uop_unit_stride_whole_reg
  //   instType(i)  := Cat(isSegment(i),loadInstDec(i).uop_type)
  // }

  // //enqueue
  // for (i <- 0 until VecStorePipelineWidth) {
  //   uopFreeList.io.accllReq(i) := DontCare
  //   when (storeInValid(i)) {
  //     uopFreeList.io.accllReq(i).valid := true.B
  //     val enqPtr = uopFreeList.io.idxValue(i)
  //     vsUopEntry(enqPtr)          := DontCare
  //     valid     (enqPtr)          := true.B
  //     vsUopEntry(enqPtr).src      := io.storeIn(i).bits.src
  //     vsUopEntry(enqPtr).uop      := io.storeIn(i).bits.uop
  //     vsUopEntry(enqPtr).vstart   := io.vstart(i) //FIXME
  //     vsUopEntry(enqPtr).mask     := GenVecStoreMask(instType = instType(i), eew = eew(i), sew = sew(i))
  //     vsUopEntry(enqPtr).eew      := eew(i)
  //     vsUopEntry(enqPtr).emul     := emul(i)
  //     vsUopEntry(enqPtr).instType := instType(i)
  //     vsUopEntry(enqPtr).uop_unit_stride_fof := loadInstDec(i).uop_unit_stride_fof
  //     vsUopEntry(enqPtr).uop_unit_whole_reg := loadInstDec(i).uop_unit_stride_whole_reg
  //     vsUopEntry(enqPtr).uop_segment_num := loadInstDec(i).uop_segment_num
  //   }
  // }

  // //dequeue
  // val UopQueueBank = VecInit(Seq.tabulate(VecStorePipelineWidth)(i => getRemBits(valid.asUInt)(i)))
  // val deqPtr = VecInit(Seq.tabulate(VecStorePipelineWidth)(i => {
  //   val value = PriorityEncoder(UopQueueBank(i))
  //   Cat(value,i.U(log2Up(VecStorePipelineWidth).W))
  // }))

  // for (i <- 0 until VecStorePipelineWidth) {
  //   io.uop2Flow(i).bits  := DontCare
  //   io.uop2Flow(i).valid := valid(deqPtr(i))//FIXME: performace, 1 interface may use incorrect valid
  //   io.uop2Flow(i).bits  := vsUopEntry(deqPtr(i))
  //   when (io.uop2Flow(i).fire) {
  //     valid(deqPtr(i)) := false.B
  //     free(i) := UIntToOH(deqPtr(i))
  //   }
  // }

}
