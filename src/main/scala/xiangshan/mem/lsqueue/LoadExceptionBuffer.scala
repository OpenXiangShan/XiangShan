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
import xiangshan.backend.fu.fpu.FPU
import xiangshan.backend.rob.RobLsqIO
import xiangshan.cache._
import xiangshan.frontend.FtqPtr
import xiangshan.ExceptionNO._
import xiangshan.cache.dcache.ReplayCarry
import xiangshan.backend.rob.RobPtr

class LqExceptionBuffer(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val redirect      = Flipped(Valid(new Redirect))
    val req           = Vec(LoadPipelineWidth, Flipped(Valid(new LqWriteBundle)))
    val exceptionAddr = new ExceptionAddrIO
  })

  val req_valid = RegInit(false.B)
  val req = Reg(new LqWriteBundle)

  // enqueue
  // s1: 
  val s1_req = VecInit(io.req.map(_.bits)) 
  val s1_valid = VecInit(io.req.map(x => x.valid))

  // s2: delay 1 cycle
  val s2_req = RegNext(s1_req)
  val s2_valid = (0 until LoadPipelineWidth).map(i => 
    RegNext(s1_valid(i)) && 
    !s2_req(i).uop.robIdx.needFlush(RegNext(io.redirect)) &&
    !s2_req(i).uop.robIdx.needFlush(io.redirect)
  )
  val s2_has_exception = s2_req.map(x => ExceptionNO.selectByFu(x.uop.cf.exceptionVec, lduCfg).asUInt.orR)

  val s2_enqueue = Wire(Vec(LoadPipelineWidth, Bool())) 
  for (w <- 0 until LoadPipelineWidth) {
    s2_enqueue(w) := s2_valid(w) && s2_has_exception(w) 
  }

  when (req.uop.robIdx.needFlush(io.redirect)) {
    req_valid := false.B
  } .elsewhen (s2_enqueue.asUInt.orR) {
    req_valid := req_valid || true.B
  }

  def selectOldest[T <: LqWriteBundle](valid: Seq[Bool], bits: Seq[T]): (Seq[Bool], Seq[T]) = {
    assert(valid.length == bits.length)
    if (valid.length == 0 || valid.length == 1) {
      (valid, bits)
    } else if (valid.length == 2) {
      val res = Seq.fill(2)(Wire(ValidIO(chiselTypeOf(bits(0)))))
      for (i <- res.indices) {
        res(i).valid := valid(i)
        res(i).bits := bits(i)
      }
      val oldest = Mux(valid(0) && valid(1), Mux(isAfter(bits(0).uop.robIdx, bits(1).uop.robIdx), res(1), res(0)), Mux(valid(0) && !valid(1), res(0), res(1)))
      (Seq(oldest.valid), Seq(oldest.bits))
    } else {
      val left = selectOldest(valid.take(valid.length / 2), bits.take(bits.length / 2))
      val right = selectOldest(valid.takeRight(valid.length - (valid.length / 2)), bits.takeRight(bits.length - (bits.length / 2)))
      selectOldest(left._1 ++ right._1, left._2 ++ right._2)
    }
  }

  val reqSel = selectOldest(s2_enqueue, s2_req)

  when (req_valid) {
    req := Mux(reqSel._1(0) && isAfter(req.uop.robIdx, reqSel._2(0).uop.robIdx), reqSel._2(0), req)
  } .elsewhen (s2_enqueue.asUInt.orR) {
    req := reqSel._2(0)
  }

  io.exceptionAddr.vaddr := req.vaddr
  XSPerfAccumulate("exception", !RegNext(req_valid) && req_valid)
  
  // end
}