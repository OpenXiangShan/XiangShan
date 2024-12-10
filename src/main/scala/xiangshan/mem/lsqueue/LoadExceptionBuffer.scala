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
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.fu.fpu.FPU
import xiangshan.backend.rob.RobLsqIO
import xiangshan.cache._
import xiangshan.frontend.FtqPtr
import xiangshan.ExceptionNO._
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.backend.rob.RobPtr

class LqExceptionBuffer(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val enqPortNum = LoadPipelineWidth + VecLoadPipelineWidth + 1 // 1 for mmio bus non-data error

  val io = IO(new Bundle() {
    val redirect      = Flipped(Valid(new Redirect))
    val req           = Vec(enqPortNum, Flipped(Valid(new LqWriteBundle)))
    val exceptionAddr = new ExceptionAddrIO
  })

  val req_valid = RegInit(false.B)
  val req = Reg(new LqWriteBundle)

  // enqueue
  // s1:
  val s1_req = VecInit(io.req.map(_.bits))
  val s1_valid = VecInit(io.req.map(x => x.valid))

  // s2: delay 1 cycle
  val s2_req = (0 until enqPortNum).map(i => {
    RegEnable(s1_req(i), s1_valid(i))})
  val s2_valid = (0 until enqPortNum).map(i =>
    RegNext(s1_valid(i)) &&
    !s2_req(i).uop.robIdx.needFlush(RegNext(io.redirect)) &&
    !s2_req(i).uop.robIdx.needFlush(io.redirect)
  )
  val s2_has_exception = s2_req.map(x => ExceptionNO.selectByFu(x.uop.exceptionVec, LduCfg).asUInt.orR)

  val s2_enqueue = Wire(Vec(enqPortNum, Bool()))
  for (w <- 0 until enqPortNum) {
    s2_enqueue(w) := s2_valid(w) && s2_has_exception(w)
  }

  when (req_valid && req.uop.robIdx.needFlush(io.redirect)) {
    req_valid := s2_enqueue.asUInt.orR
  } .elsewhen (s2_enqueue.asUInt.orR) {
    req_valid := true.B
  }

  val reqSel = SelectOldestUopIdx(s2_enqueue, s2_req)

  when (req_valid) {
    req := Mux(
      reqSel._1(0) && (isAfter(req.uop.robIdx, reqSel._2(0).uop.robIdx) || (req.uop.robIdx === reqSel._2(0).uop.robIdx && req.uop.uopIdx > reqSel._2(0).uop.uopIdx)),
      reqSel._2(0),
      req)
  } .elsewhen (s2_enqueue.asUInt.orR) {
    req := reqSel._2(0)
  }

  io.exceptionAddr.vaddr  := req.fullva
  io.exceptionAddr.vaNeedExt := req.vaNeedExt
  io.exceptionAddr.isHyper := req.isHyper
  io.exceptionAddr.vstart := req.uop.vpu.vstart
  io.exceptionAddr.vl     := req.uop.vpu.vl
  io.exceptionAddr.gpaddr := req.gpaddr
  io.exceptionAddr.isForVSnonLeafPTE := req.isForVSnonLeafPTE

  XSPerfAccumulate("exception", !RegNext(req_valid) && req_valid)

  // end
}