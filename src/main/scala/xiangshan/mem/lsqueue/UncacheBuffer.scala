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
import chipsalliance.rocketchip.config._
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
    val loadOut = DecoupledIO(new ExuOutput) 
    val loadRawDataOut = Output(new LoadDataFromLQBundle) 

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
  val uncacheCommitFired = RegInit(false.B)
  val uncacheData = Reg(io.uncache.resp.bits.data.cloneType)

  // enqueue
  when (req_valid && req.uop.robIdx.needFlush(io.redirect)) {
    req_valid := false.B
  } .elsewhen (io.req.valid) {
    XSError(req_valid, p"UncacheBuffer: You can not write an valid entry: $entryIndex")
    req_valid := true.B
    req := io.req.bits
  } .elsewhen (io.loadOut.fire) {
    req_valid := false.B
  }

  when (io.req.valid) {
    when (io.req.bits.lqDataWenDup(5)) {
      triggerResult := io.trigger.hitLoadAddrTriggerHitVec 
    }
  }

  io.trigger.lqLoadAddrTriggerHitVec := Mux(
    io.loadOut.valid,
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
  when (uncacheState === s_req) {
    uncacheCommitFired := false.B
  }

  io.rob.mmio := DontCare
  io.rob.uop := DontCare

  switch (uncacheState) {
    is (s_idle) {
      when (RegNext(io.rob.pendingld && req_valid && req.uop.robIdx === io.rob.pendingPtr)) {
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
      when (RegNext(io.rob.commit)) {
        uncacheState := s_idle // ready for next mmio
      }
    }
  }  

  io.select := uncacheState =/= s_idle

  io.uncache.req.valid := uncacheState === s_req
  io.uncache.req.bits := DontCare
  io.uncache.req.bits.cmd := MemoryOpConstants.M_XRD
  io.uncache.req.bits.data := DontCare
  io.uncache.req.bits.addr := req.paddr
  io.uncache.req.bits.mask := req.mask
  io.uncache.req.bits.id := io.id
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

  io.loadOut.valid := (uncacheState === s_wait) && !uncacheCommitFired
  io.loadOut.bits := DontCare
  io.loadOut.bits.uop := selUop
  io.loadOut.bits.uop.lqIdx := req.uop.lqIdx
  io.loadOut.bits.data := rdataPartialLoad 
  io.loadOut.bits.redirectValid := false.B
  io.loadOut.bits.redirect := DontCare
  io.loadOut.bits.debug.isMMIO := true.B
  io.loadOut.bits.debug.paddr := req.paddr
  io.loadOut.bits.debug.vaddr := req.vaddr
  io.loadOut.bits.fflags := DontCare

  io.loadRawDataOut.lqData := uncacheData
  io.loadRawDataOut.uop := req.uop
  io.loadRawDataOut.addrOffset := req.paddr

  
  val dummyCtrl = RegNext(io.loadOut.valid)
  when (io.loadOut.fire && dummyCtrl) {
    req_valid := false.B
    uncacheCommitFired := true.B

    XSInfo("int load miss write to cbd robidx %d lqidx %d pc 0x%x mmio %x\n",
      io.loadOut.bits.uop.robIdx.asUInt,
      io.loadOut.bits.uop.lqIdx.asUInt,
      io.loadOut.bits.uop.cf.pc,
      true.B
    )    
  }
  // end
}

class UncacheBuffer(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val redirect = Flipped(Valid(new Redirect))

    // 
    val req = Vec(LoadPipelineWidth, Flipped(Valid(new LqWriteBundle)))

    // writeback mmio data
    val loadOut = Vec(LoadPipelineWidth, DecoupledIO(new ExuOutput))
    val loadRawDataOut = Vec(LoadPipelineWidth, Output(new LoadDataFromLQBundle))

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
    io.loadOut(w).valid := false.B
    io.loadOut(w).bits := DontCare
    io.loadRawDataOut(w) := DontCare
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
  val s2_need_replay = s2_req.map(_.replayInfo.needReplay())

  val s2_enqueue = Wire(Vec(LoadPipelineWidth, Bool()))
  for (w <- 0 until LoadPipelineWidth) {
    s2_enqueue(w) := s2_valid(w) && !s2_has_exception(w) && !s2_need_replay(w) && s2_req(w).mmio
  }

  //
  val enqValidVec = Wire(Vec(LoadPipelineWidth, Bool()))
  val enqIndexVec = Wire(Vec(LoadPipelineWidth, UInt()))
  val enqOffset = Wire(Vec(LoadPipelineWidth, UInt(log2Up(LoadPipelineWidth + 1).W)))

  for (w <- 0 until LoadPipelineWidth) {
    freeList.io.allocateReq(w) := s2_enqueue(w)
  }

  // freeList real-allocate 
  for (w <- 0 until LoadPipelineWidth) {
    freeList.io.doAllocate(w) := enqValidVec(w)
  }

  for (w <- 0 until LoadPipelineWidth) {
    enqOffset(w) := PopCount(s2_enqueue.take(w))
    enqValidVec(w) := s2_enqueue(w) && freeList.io.canAllocate(enqOffset(w))
    enqIndexVec(w) := freeList.io.allocateSlot(enqOffset(w))
  }

  // 
  val uncacheReq = Wire(Valid(io.uncache.req.bits.cloneType))
  val loadOut = Wire(Valid(io.loadOut(0).bits.cloneType))
  val loadRawDataOut = Wire(io.loadRawDataOut(0).cloneType)
  val lqLoadAddrTriggerHitVec = Wire(io.trigger(0).lqLoadAddrTriggerHitVec.cloneType)

  // init
  uncacheReq.valid := false.B 
  uncacheReq.bits := DontCare
  loadOut.valid := false.B 
  loadOut.bits := DontCare 
  loadRawDataOut := DontCare
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
      e.io.uncache.req.ready <> io.uncache.req.ready
      e.io.loadOut.ready <> io.loadOut(0).ready

      when (e.io.select) {
        uncacheReq.valid := e.io.uncache.req.valid
        uncacheReq.bits := e.io.uncache.req.bits

        loadOut.valid := e.io.loadOut.valid 
        loadOut.bits := e.io.loadOut.bits 
        loadRawDataOut := e.io.loadRawDataOut
        // Read vaddr for mem exception
        // no inst will be commited 1 cycle before tval update
        // read vaddr for mmio, and only port 0 is used 
        lqLoadAddrTriggerHitVec := e.io.trigger.lqLoadAddrTriggerHitVec
      }

      when (i.U === io.uncache.resp.bits.id) {
        e.io.uncache.resp <> io.uncache.resp
      }
  }

  io.uncache.req.valid := RegNext(uncacheReq.valid)
  io.uncache.req.bits := RegNext(uncacheReq.bits)
  io.loadOut(0).valid := RegNext(loadOut.valid)
  io.loadOut(0).bits := RegNext(loadOut.bits)
  io.loadRawDataOut(0) := RegNext(loadRawDataOut)
  io.trigger(0).lqLoadAddrTriggerHitVec := RegNext(lqLoadAddrTriggerHitVec)

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
      when ((e.io.select && io.loadOut(0).fire) || e.io.flush) {
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
  io.rollback.bits := DontCare
  io.rollback.bits.robIdx := rollbackUop.robIdx
  io.rollback.bits.ftqIdx := rollbackUop.cf.ftqPtr
  io.rollback.bits.ftqOffset := rollbackUop.cf.ftqOffset
  io.rollback.bits.level := RedirectLevel.flush
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
  XSPerfAccumulate("mmio_writeback_success", io.loadOut(0).fire)
  XSPerfAccumulate("mmio_writeback_blocked", io.loadOut(0).valid && !io.loadOut(0).ready)
  XSPerfAccumulate("uncache_full_rollback", io.rollback.valid)

  val perfEvents: Seq[(String, UInt)] = Seq(
    ("mmioCycle", VecInit(entries.map(_.io.select)).asUInt.orR),
    ("mmioCnt", io.uncache.req.fire),
    ("mmio_writeback_success", io.loadOut(0).fire),
    ("mmio_writeback_blocked", io.loadOut(0).valid && !io.loadOut(0).ready),
    ("uncache_full_rollback",  io.rollback.valid)
  )
  // end
}