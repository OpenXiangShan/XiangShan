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
import xiangshan.mem._
import xiangshan.backend.fu.vector.Bundles._
import xiangshan.backend.exu.ExeUnitParams


class VfofDataBundle(implicit p: Parameters) extends VLSUBundle{
  val uop              = new DynInst
  val vl               = UInt(elemIdxBits.W)
  val hasException     = Bool()
}


class VfofBuffer(val param: ExeUnitParams)(implicit p: Parameters) extends VLSUModule{
  val io = IO(new VfofDataBuffIO(param))

  private def isOlder(left: DynInst, right: DynInst): Bool = {
    (right.vpu.vl > left.vpu.vl || left.exceptionVec.asUInt.orR) && !right.exceptionVec.asUInt.orR
  }
  implicit val vfofParam: ExeUnitParams = param

  val entries = RegInit(0.U.asTypeOf(new VfofDataBundle()))
  val valid   = RegInit(false.B)
  val selectOldestModule = Module(new SelectOldest(new DynInst, VLUopWritebackWidth, isOlder))

  val entriesIsFixVl = entries.uop.vpu.lastUop && entries.uop.vpu.isVleff

  //Enq
  io.in.map(_.ready := true.B)
  val enqIsfof = io.in.map { x =>
    x.valid && x.bits.vpu.get.isVleff
  }

  val enqValid = enqIsfof.reduce(_ || _)
  val enqBits  = ParallelPriorityMux(enqIsfof, io.in.map(_.bits))
  val enqNeedCancel = enqBits.robIdx.needFlush(io.redirect)
  val enqIsFixVl = enqBits.vpu.get.isVleff && enqBits.vpu.get.lastUop

  XSError(entries.uop.robIdx.value =/= enqBits.robIdx.value && valid && enqValid, "There should be no new fof instrction coming in!\n")
  XSError(entriesIsFixVl && valid && enqValid, "There should not new uop enqueue!\n")

  when(enqValid && !enqNeedCancel) {
    when(!valid){
      entries.uop           := enqBits.toDynInst()
      entries.vl            := enqBits.vl.get
      entries.hasException  := false.B
    }.elsewhen(valid && enqIsFixVl){
      entries.uop     := enqBits.toDynInst()
    }
  }

  //Control Signal
  val needRedirect = entries.uop.robIdx.needFlush(io.redirect)


  when(io.uopWriteback.fire) {
    valid := false.B  //Deq
  }.elsewhen(needRedirect) {
    valid := false.B //Redirect
  }.elsewhen(enqValid && !enqNeedCancel) {
    valid := true.B //Enq
  }


  //Gather writeback information
  val wbIsfof = io.mergeUopWriteback.map{ x => x.valid && x.bits.robIdx === entries.uop.robIdx }

  //Update uop vl
  io.mergeUopWriteback.map{_.ready := true.B}
  val portUop         = Wire(Vec(VLUopWritebackWidth, new DynInst))
  portUop.zip(io.mergeUopWriteback.map(_.bits)).map{ case(sink, source) =>
    sink              := WireInit(0.U.asTypeOf(new DynInst))
    sink.robIdx       := source.robIdx
    sink.vpu.vl       := source.vl
    sink.exceptionVec := source.exceptionVec
  }
  selectOldestModule.io.in.zipWithIndex.map{case (sink, i) =>
    sink.valid := wbIsfof(i)
    sink.bits := portUop(i)
  }
  val wbBits          = selectOldestModule.io.out.bits
  val wbValid         = selectOldestModule.io.out.valid
  val wbHasException  = wbBits.exceptionVec.asUInt.orR
  val wbUpdateValid = wbValid && (wbBits.vpu.vl < entries.vl || wbHasException) && valid && !needRedirect && !entries.hasException

  XSError(wbValid && wbHasException && valid && entries.hasException, "The same instruction triggers an exception multiple times!\n")

  when(wbUpdateValid) {
    entries.vl                    := wbBits.vpu.vl
    entries.hasException          := wbHasException
  }

  // Deq
  io.uopWriteback.valid := valid && entries.uop.vpu.lastUop && entries.uop.vpu.isVleff && !needRedirect
  io.uopWriteback.bits := 0.U.asTypeOf(new ExuOutput(param))
  io.uopWriteback.bits.data := VecInit(Seq.fill(param.wbPathNum)(entries.vl))
  io.uopWriteback.bits.pdestVl.get := entries.uop.pdestVl
  io.uopWriteback.bits.robIdx := entries.uop.robIdx
  io.uopWriteback.bits.intWen.foreach(_ := entries.uop.rfWen)
  io.uopWriteback.bits.fpWen.foreach(_ := entries.uop.fpWen)
  io.uopWriteback.bits.vecWen.foreach(_ := entries.uop.vecWen)
  io.uopWriteback.bits.v0Wen.foreach(_ := entries.uop.v0Wen)
  io.uopWriteback.bits.vlWen.foreach(_ := entries.uop.vlWen)
  io.uopWriteback.bits.exceptionVec.foreach(_ := 0.U.asTypeOf(ExceptionVec()))
  io.uopWriteback.bits.lqIdx.foreach(_ := entries.uop.lqIdx)
  io.uopWriteback.bits.sqIdx.foreach(_ := entries.uop.sqIdx)
  io.uopWriteback.bits.vls.foreach(vls => {
    vls.vpu := entries.uop.vpu
    vls.vpu.vl := entries.vl
    vls.vpu.vmask := Fill(VLEN, 1.U)
  })
  io.uopWriteback.bits.perfDebugInfo.foreach(_ := entries.uop.perfDebugInfo)
  io.uopWriteback.bits.debug_seqNum.foreach(_ := entries.uop.debug_seqNum)

}
