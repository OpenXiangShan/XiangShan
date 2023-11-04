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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xiangshan._
import xiangshan.backend.rob.{RobPtr, RobLsqIO}
import xiangshan.ExceptionNO._
import xiangshan.cache._
import utils._
import utility._

class UncacheBufferEntry(entryIndex: Int)(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper
  with HasLoadHelper
{
  val io = IO(new Bundle() {
    val id = Input(UInt())

    val redirect = Flipped(Valid(new Redirect))

    // client requests
    val req = Flipped(Valid(new LqWriteBundle))

    // writeback mmio data
    val ldout = DecoupledIO(new ExuOutput)
    val ld_raw_data = Output(new LoadDataFromLQBundle)

    // rob: uncache commit
    val rob = Flipped(new RobLsqIO)

    // uncache io
    val uncache = new UncacheWordIO

    // trigger
    val trigger = new LqTriggerIO

    // select this entry
    val select = Output(Bool())

    // flush this entry
    val flush = Output(Bool())

  })

  val req_valid = RegInit(false.B)
  val req = Reg(new LqWriteBundle)
  val triggerResult = RegInit(VecInit(Seq.fill(3)(false.B)))

  //
  val s_idle :: s_req :: s_resp :: s_wait :: Nil = Enum(4)
  val uncacheState = RegInit(s_idle)
  val uncacheData = Reg(io.uncache.resp.bits.data.cloneType)

  // enqueue
  when (req_valid && req.uop.robIdx.needFlush(io.redirect)) {
    req_valid := false.B
  } .elsewhen (io.req.valid) {
    XSError(req_valid, p"UncacheBuffer: You can not write an valid entry: $entryIndex")
    req_valid := true.B
    req := io.req.bits
  } .elsewhen (io.ldout.fire) {
    req_valid := false.B
  }

  when (io.req.valid) {
    when (io.req.bits.data_wen_dup(5)) {
      triggerResult := io.trigger.hitLoadAddrTriggerHitVec
    }
  }

  io.trigger.lqLoadAddrTriggerHitVec := Mux(
    io.ldout.valid,
    RegNext(triggerResult),
    VecInit(Seq.fill(3)(false.B))
  )

  io.flush := req_valid && req.uop.robIdx.needFlush(io.redirect)
  /**
    * Memory mapped IO / other uncached operations
    *
    * States:
    * (1) writeback from store units: mark as pending
    * (2) when they reach ROB's head, they can be sent to uncache channel
    * (3) response from uncache channel: mark as datavalid
    * (4) writeback to ROB (and other units): mark as writebacked
    * (5) ROB commits the instruction: same as normal instructions
    */

  io.rob.mmio := DontCare
  io.rob.uop := DontCare
  val pendingld = RegNext(io.rob.pendingld)
  val pendingPtr = RegNext(io.rob.pendingPtr)

  switch (uncacheState) {
    is (s_idle) {
      when (req_valid && pendingld && req.uop.robIdx === pendingPtr) {
        uncacheState := s_req
      }
    }
    is (s_req) {
      when (io.uncache.req.fire) {
        uncacheState := s_resp
      }
    }
    is (s_resp) {
      when (io.uncache.resp.fire) {
        uncacheState := s_wait
      }
    }
    is (s_wait) {
      when (io.ldout.fire) {
        uncacheState := s_idle // ready for next mmio
      }
    }
  }

  io.select := uncacheState =/= s_idle

  io.uncache.req.valid     := uncacheState === s_req
  io.uncache.req.bits      := DontCare
  io.uncache.req.bits.cmd  := MemoryOpConstants.M_XRD
  io.uncache.req.bits.data := DontCare
  io.uncache.req.bits.addr := req.paddr
  io.uncache.req.bits.mask := Mux(req.paddr(3), req.mask(15, 8), req.mask(7, 0))
  io.uncache.req.bits.id   := io.id
  io.uncache.req.bits.instrtype := DontCare
  io.uncache.req.bits.replayCarry := DontCare
  io.uncache.req.bits.atomic := true.B

  io.uncache.resp.ready := true.B

  when (io.uncache.req.fire) {
    XSDebug("uncache req: pc %x addr %x data %x op %x mask %x\n",
      req.uop.cf.pc,
      io.uncache.req.bits.addr,
      io.uncache.req.bits.data,
      io.uncache.req.bits.cmd,
      io.uncache.req.bits.mask
    )
  }

  // (3) response from uncache channel
  when (io.uncache.resp.fire) {
    uncacheData := io.uncache.resp.bits.data
  }

  // uncache writeback
  val selUop = req.uop
  val func = selUop.ctrl.fuOpType
  val raddr = req.paddr
  val rdataSel = LookupTree(raddr(2, 0), List(
      "b000".U -> uncacheData(63,  0),
      "b001".U -> uncacheData(63,  8),
      "b010".U -> uncacheData(63, 16),
      "b011".U -> uncacheData(63, 24),
      "b100".U -> uncacheData(63, 32),
      "b101".U -> uncacheData(63, 40),
      "b110".U -> uncacheData(63, 48),
      "b111".U -> uncacheData(63, 56)
    ))
  val rdataPartialLoad = rdataHelper(selUop, rdataSel)

  io.ldout.valid              := (uncacheState === s_wait)
  io.ldout.bits               := DontCare
  io.ldout.bits.uop           := selUop
  io.ldout.bits.uop.lqIdx     := req.uop.lqIdx
  io.ldout.bits.data          := rdataPartialLoad
  io.ldout.bits.redirectValid := false.B
  io.ldout.bits.redirect      := DontCare
  io.ldout.bits.debug.isMMIO  := true.B
  io.ldout.bits.debug.paddr   := req.paddr
  io.ldout.bits.debug.vaddr   := req.vaddr
  io.ldout.bits.fflags        := DontCare

  io.ld_raw_data.lqData     := uncacheData
  io.ld_raw_data.uop        := req.uop
  io.ld_raw_data.addrOffset := req.paddr


  when (io.ldout.fire) {
    req_valid := false.B

    XSInfo("int load miss write to cbd robidx %d lqidx %d pc 0x%x mmio %x\n",
      io.ldout.bits.uop.robIdx.asUInt,
      io.ldout.bits.uop.lqIdx.asUInt,
      io.ldout.bits.uop.cf.pc,
      true.B
    )
  }

  // end
}

class UncacheBuffer(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    // control
    val redirect = Flipped(Valid(new Redirect))

    //
    val req = Vec(LoadPipelineWidth, Flipped(Valid(new LqWriteBundle)))

    // writeback mmio data
    val ldout = Vec(LoadPipelineWidth, DecoupledIO(new ExuOutput))
    val ld_raw_data = Vec(LoadPipelineWidth, Output(new LoadDataFromLQBundle))

    // rob: uncache commit
    val rob = Flipped(new RobLsqIO)

    // uncache io
    val uncache = new UncacheWordIO

    // trigger io
    val trigger = Vec(LoadPipelineWidth, new LqTriggerIO)

    // rollback
    val rollback = Output(Valid(new Redirect))
  })

  val entries = Seq.tabulate(LoadUncacheBufferSize)(i => Module(new UncacheBufferEntry(i)))

  // freeliset: store valid entries index.
  // +---+---+--------------+-----+-----+
  // | 0 | 1 |      ......  | n-2 | n-1 |
  // +---+---+--------------+-----+-----+
  val freeList = Module(new FreeList(
    size = LoadUncacheBufferSize,
    allocWidth = LoadPipelineWidth,
    freeWidth = 4,
    enablePreAlloc = true,
    moduleName = "UncacheBuffer freelist"
  ))
  freeList.io := DontCare

  // set enqueue default
  entries.foreach {
    case (e) =>
      e.io.req.valid := false.B
      e.io.req.bits := DontCare
  }

  // set uncache default
  io.uncache.req.valid := false.B
  io.uncache.req.bits := DontCare
  io.uncache.resp.ready := false.B

  entries.foreach {
    case (e) =>
      e.io.uncache.req.ready := false.B
      e.io.uncache.resp.valid := false.B
      e.io.uncache.resp.bits := DontCare
  }

  // set writeback default
  for (w <- 0 until LoadPipelineWidth) {
    io.ldout(w).valid := false.B
    io.ldout(w).bits := DontCare
    io.ld_raw_data(w) := DontCare
  }

  // set trigger default
  entries.foreach {
    case (e) =>
      e.io.trigger.hitLoadAddrTriggerHitVec := VecInit(Seq.fill(3)(false.B))
  }

  io.trigger.foreach {
    case (t) =>
      t.lqLoadAddrTriggerHitVec := VecInit(Seq.fill(3)(false.B))
  }

  // enqueue
  // s1:
  val s1_req = VecInit(io.req.map(_.bits))
  val s1_valid = VecInit(io.req.map(_.valid))

  // s2: enqueue
  val s2_req = RegNext(s1_req)
  val s2_valid = (0 until LoadPipelineWidth).map(i => {
    RegNext(s1_valid(i)) &&
    !s2_req(i).uop.robIdx.needFlush(RegNext(io.redirect)) &&
    !s2_req(i).uop.robIdx.needFlush(io.redirect)
  })
  val s2_has_exception = s2_req.map(x => ExceptionNO.selectByFu(x.uop.cf.exceptionVec, lduCfg).asUInt.orR)
  val s2_need_replay = s2_req.map(_.rep_info.need_rep)

  val s2_enqueue = Wire(Vec(LoadPipelineWidth, Bool()))
  for (w <- 0 until LoadPipelineWidth) {
    s2_enqueue(w) := s2_valid(w) && !s2_has_exception(w) && !s2_need_replay(w) && s2_req(w).mmio
  }

  //
  val enqValidVec = Wire(Vec(LoadPipelineWidth, Bool()))
  val enqIndexVec = Wire(Vec(LoadPipelineWidth, UInt()))

  for (w <- 0 until LoadPipelineWidth) {
    freeList.io.allocateReq(w) := true.B
  }

  // freeList real-allocate
  for (w <- 0 until LoadPipelineWidth) {
    freeList.io.doAllocate(w) := enqValidVec(w)
  }

  for (w <- 0 until LoadPipelineWidth) {
    enqValidVec(w) := s2_enqueue(w) && freeList.io.canAllocate(w)

    val offset = PopCount(s2_enqueue.take(w))
    enqIndexVec(w) := freeList.io.allocateSlot(offset)
  }

  //
  val uncacheReq = Wire(DecoupledIO(io.uncache.req.bits.cloneType))
  val ldout = Wire(DecoupledIO(io.ldout(0).bits.cloneType))
  val ld_raw_data = Wire(io.ld_raw_data(0).cloneType)
  val lqLoadAddrTriggerHitVec = Wire(io.trigger(0).lqLoadAddrTriggerHitVec.cloneType)

  // init
  uncacheReq.valid := false.B
  uncacheReq.bits  := DontCare
  ldout.valid      := false.B
  ldout.bits       := DontCare
  ld_raw_data        := DontCare
  lqLoadAddrTriggerHitVec := DontCare

  entries.zipWithIndex.foreach {
    case (e, i) =>
      e.io.redirect <> io.redirect
      e.io.id := i.U

      // enqueue
      for (w <- 0 until LoadPipelineWidth) {
        when (enqValidVec(w) && (i.U === enqIndexVec(w))) {
          e.io.req.valid := true.B
          e.io.req.bits := s2_req(w)
          e.io.trigger.hitLoadAddrTriggerHitVec := io.trigger(w).hitLoadAddrTriggerHitVec
        }
      }

      // uncache logic
      e.io.rob <> io.rob
      e.io.uncache.req.ready := uncacheReq.ready
      e.io.ldout.ready := ldout.ready

      when (e.io.select) {
        uncacheReq.valid := e.io.uncache.req.valid
        uncacheReq.bits := e.io.uncache.req.bits

        ldout.valid   := e.io.ldout.valid
        ldout.bits    := e.io.ldout.bits
        ld_raw_data   := e.io.ld_raw_data
        // Read vaddr for mem exception
        // no inst will be commited 1 cycle before tval update
        // read vaddr for mmio, and only port 0 is used
        lqLoadAddrTriggerHitVec := e.io.trigger.lqLoadAddrTriggerHitVec
      }

      when (i.U === io.uncache.resp.bits.id) {
        e.io.uncache.resp <> io.uncache.resp
      }
  }

  // uncache Request
  AddPipelineReg(uncacheReq, io.uncache.req, false.B)

  // uncache Writeback
  AddPipelineReg(ldout, io.ldout(0), false.B)

  // uncache RAW data
  // FIXME: remove it?
  io.ld_raw_data(0) := RegEnable(ld_raw_data, ldout.fire)
  io.trigger(0).lqLoadAddrTriggerHitVec := RegEnable(lqLoadAddrTriggerHitVec, ldout.fire)

  for (i <- 0 until LoadPipelineWidth) {
    io.rob.mmio(i) := RegNext(s1_valid(i) && s1_req(i).mmio)
    io.rob.uop(i) := RegNext(s1_req(i).uop)
  }

  // UncacheBuffer deallocate
  val freeMaskVec = Wire(Vec(LoadUncacheBufferSize, Bool()))

  // init
  freeMaskVec.map(e => e := false.B)

  // dealloc logic
  entries.zipWithIndex.foreach {
    case (e, i) =>
      when ((e.io.select && e.io.ldout.fire) || e.io.flush) {
        freeMaskVec(i) := true.B
      }
  }

  freeList.io.free := freeMaskVec.asUInt

  /**
    * Uncache rollback detection
    *
    * When uncache loads enqueue, it searches uncache loads, They can not enqueue and need re-execution.
    *
    * Cycle 0: uncache enqueue.
    * Cycle 1: Select oldest uncache loads.
    * Cycle 2: Redirect Fire.
    *   Choose the oldest load from LoadPipelineWidth oldest loads.
    *   Prepare redirect request according to the detected rejection.
    *   Fire redirect request (if valid)
    */
  //               Load_S3  .... Load_S3
  // stage 0:        lq            lq
  //                 |             | (can not enqueue)
  // stage 1:        lq            lq
  //                 |             |
  //                 ---------------
  //                        |
  // stage 2:               lq
  //                        |
  //                     rollback req
  def selectOldest[T <: MicroOp](valid: Seq[Bool], bits: Seq[T]): (Seq[Bool], Seq[T]) = {
    assert(valid.length == bits.length)
    if (valid.length == 0 || valid.length == 1) {
      (valid, bits)
    } else if (valid.length == 2) {
      val res = Seq.fill(2)(Wire(ValidIO(chiselTypeOf(bits(0)))))
      for (i <- res.indices) {
        res(i).valid := valid(i)
        res(i).bits := bits(i)
      }
      val oldest = Mux(valid(0) && valid(1), Mux(isAfter(bits(0).robIdx, bits(1).robIdx), res(1), res(0)), Mux(valid(0) && !valid(1), res(0), res(1)))
      (Seq(oldest.valid), Seq(oldest.bits))
    } else {
      val left = selectOldest(valid.take(valid.length / 2), bits.take(bits.length / 2))
      val right = selectOldest(valid.takeRight(valid.length - (valid.length / 2)), bits.takeRight(bits.length - (bits.length / 2)))
      selectOldest(left._1 ++ right._1, left._2 ++ right._2)
    }
  }
  def detectRollback() = {
    val reqNeedCheck = VecInit((0 until LoadPipelineWidth).map(w =>
      s2_enqueue(w) && !enqValidVec(w)
    ))
    val reqSelUops = VecInit(s2_req.map(_.uop))
    val reqSelect = selectOldest(reqNeedCheck, reqSelUops)
    (RegNext(reqSelect._1(0)), RegNext(reqSelect._2(0)))
  }

  val (rollbackValid, rollbackUop) = detectRollback()
  io.rollback.bits           := DontCare
  io.rollback.bits.rawNuke   := false.B
  io.rollback.bits.isRVC     := rollbackUop.cf.pd.isRVC
  io.rollback.bits.robIdx    := rollbackUop.robIdx
  io.rollback.bits.ftqIdx    := rollbackUop.cf.ftqPtr
  io.rollback.bits.ftqOffset := rollbackUop.cf.ftqOffset
  io.rollback.bits.level     := RedirectLevel.flush
  io.rollback.bits.cfiUpdate.target := rollbackUop.cf.pc
  io.rollback.bits.debug_runahead_checkpoint_id := rollbackUop.debugInfo.runahead_checkpoint_id

  val lastCycleRedirect = RegNext(io.redirect)
  val lastLastCycleRedirect = RegNext(lastCycleRedirect)
  io.rollback.valid := rollbackValid &&
                      !rollbackUop.robIdx.needFlush(io.redirect) &&
                      !rollbackUop.robIdx.needFlush(lastCycleRedirect) &&
                      !rollbackUop.robIdx.needFlush(lastLastCycleRedirect)

  //  perf counter
  val validCount = freeList.io.validCount
  val allowEnqueue = !freeList.io.empty
  QueuePerf(LoadUncacheBufferSize, validCount, !allowEnqueue)

  XSPerfAccumulate("mmioCycle", VecInit(entries.map(_.io.select)).asUInt.orR)
  XSPerfAccumulate("mmioCnt", io.uncache.req.fire)
  XSPerfAccumulate("mmio_writeback_success", io.ldout(0).fire)
  XSPerfAccumulate("mmio_writeback_blocked", io.ldout(0).valid && !io.ldout(0).ready)
  XSPerfAccumulate("uncache_full_rollback", io.rollback.valid)

  val perfEvents: Seq[(String, UInt)] = Seq(
    ("mmioCycle", VecInit(entries.map(_.io.select)).asUInt.orR),
    ("mmioCnt", io.uncache.req.fire),
    ("mmio_writeback_success", io.ldout(0).fire),
    ("mmio_writeback_blocked", io.ldout(0).valid && !io.ldout(0).ready),
    ("uncache_full_rollback",  io.rollback.valid)
  )
  // end
}