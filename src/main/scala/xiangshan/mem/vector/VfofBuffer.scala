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


class VfofDataBundle(implicit p: Parameters) extends VLSUBundle{
  val uop              = new DynInst
  val vl               = UInt(elemIdxBits.W)
  val hasException     = Bool()
}


class VfofBuffer(implicit p: Parameters) extends VLSUModule{
  val io = IO(new VfofDataBuffIO())
  
  val entries = RegInit(0.U.asTypeOf(new VfofDataBundle()))
  val valid   = RegInit(false.B)

  val entriesIsFixVl = entries.uop.vpu.lastUop && entries.uop.vpu.isVleff

  //Enq
  io.in.map(_.ready := true.B)
  val enqIsfof = io.in.map { x =>
    x.valid && x.bits.uop.vpu.isVleff
  }

  val enqValid = enqIsfof.reduce(_ || _)
  val enqBits  = ParallelPriorityMux(enqIsfof, io.in.map(_.bits))
  val enqNeedCancel = enqBits.uop.robIdx.needFlush(io.redirect)
  val enqIsFixVl = enqBits.uop.vpu.isVleff && enqBits.uop.vpu.lastUop

  XSError(entries.uop.robIdx.value =/= enqBits.uop.robIdx.value && valid && enqValid, "There should be no new fof instrction coming in!\n")
  XSError(entriesIsFixVl && valid && enqValid, "There should not new uop enqueue!\n")

  when(enqValid && !enqNeedCancel) {
    when(!valid){
      entries.uop           := enqBits.uop
      entries.vl            := enqBits.src_vl.asTypeOf(VConfig()).vl
      entries.hasException  := false.B
    }.elsewhen(valid && enqIsFixVl){
      entries.uop     := enqBits.uop
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
  val wbIsfof = io.mergeUopWriteback.map{ x => x.valid && x.bits.uop.robIdx === entries.uop.robIdx }

  def getOldest(valid: Seq[Bool], bits: Seq[DynInst]): DynInst = {
    def getOldest_recursion[T <: Data](valid: Seq[Bool], bits: Seq[DynInst]): (Seq[Bool], Seq[DynInst]) = {
      assert(valid.length == bits.length)
      if (valid.length == 1) {
        (valid, bits)
      } else if (valid.length == 2) {
        val res = Seq.fill(2)(Wire(ValidIO(chiselTypeOf(bits(0)))))
        for (i <- res.indices) {
          res(i).valid := valid(i)
          res(i).bits := bits(i)
        }
        val withExcep0 = bits(0).exceptionVec.asUInt.orR
        val withExcep1 = bits(1).exceptionVec.asUInt.orR
        XSError(this.valid && withExcep0 && withExcep1 && valid(0) && valid(1), "Writeback to multiple Uop with exceptions at the same time!\n")
        val oldest = Mux(
          valid(0) && valid(1),
          Mux((bits(1).vpu.vl > bits(0).vpu.vl || withExcep0) && !withExcep1, res(0), res(1)),
          Mux(valid(0) && !valid(1), res(0), res(1))
        )
        (Seq(oldest.valid), Seq(oldest.bits))
      } else {
        val left = getOldest_recursion(valid.take(valid.length / 2), bits.take(valid.length / 2))
        val right = getOldest_recursion(valid.drop(valid.length / 2), bits.drop(valid.length / 2))
        getOldest_recursion(left._1 ++ right._1, left._2 ++ right._2)
      }
    }
    getOldest_recursion(valid, bits)._2.head
  }

  //Update uop vl
  io.mergeUopWriteback.map{_.ready := true.B}
  val wbBits          = getOldest(wbIsfof, io.mergeUopWriteback.map(_.bits.uop))
  val wbValid         = wbIsfof.reduce(_ || _)
  val wbHasException  = wbBits.exceptionVec.asUInt.orR
  val wbUpdateValid = wbValid && (wbBits.vpu.vl < entries.vl || wbHasException) && valid && !needRedirect && !entries.hasException

  XSError(wbValid && wbHasException && valid && entries.hasException, "The same instruction triggers an exception multiple times!\n")

  when(wbUpdateValid) {
    entries.vl                    := wbBits.vpu.vl
    entries.hasException          := wbHasException
  }

  //Deq
  io.uopWriteback.bits                  := 0.U.asTypeOf(new MemExuOutput(isVector = true))
  io.uopWriteback.bits.uop              := entries.uop
  io.uopWriteback.bits.uop.exceptionVec := 0.U.asTypeOf(ExceptionVec())
  io.uopWriteback.bits.data             := entries.vl
  io.uopWriteback.bits.uop.vpu.vl       := entries.vl
  io.uopWriteback.bits.mask.get         := Fill(VLEN, 1.U)
  io.uopWriteback.bits.uop.vpu.vmask    := Fill(VLEN, 1.U)
  io.uopWriteback.valid                 := valid && entries.uop.vpu.lastUop && entries.uop.vpu.isVleff && !needRedirect


}
