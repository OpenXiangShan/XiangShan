package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils._

// for load/sta/std, not drop on redirect
trait RSDropNotOnRedirect {this: BaseReservationStation =>
  for ((enqFlush, robIdx) <- s0_enqFlushed.zip(io.fromDispatch.map(_.bits.robIdx))) {
    enqFlush := robIdx.needFlush(io.redirect)
  }
}

// for load/sta
trait RSImpMemAddrIOConnect { this: BaseReservationStationImp =>
  extra.feedback <> rs.flatMap(_.extra.feedback)
  rs.foreach(_.extra.checkwait <> extra.checkwait)
}

// for load/sta
// 1) feedback 2) checkWaitBit
class RSWithMemAddr(params: RSParams)(implicit p: Parameters) extends BaseReservationStation(params) with RSDropNotOnRedirect {

  // -------------------- 更新 s1阶段uop.bits.cf.loadWaitBit 该指令是否需要在保留站中等待依赖的load指令
  // s1_dispatchUops_dup是一个寄存器
  // Allocation: store dispatch uops into payload and data array
  s1_dispatchUops_dup.foreach(_.zip(enqReverse(io.fromDispatch)).zipWithIndex.foreach{ case ((uop, in), i) =>
    val s0_valid = in.fire && !enqReverse(s0_enqFlushed)(i)
    when (s0_valid) {
      // a temp fix for blocked. This will release the load wait for some instructions earlier.
      // copied from status array
      val blockNotReleased = isAfter(in.bits.sqIdx, extra.checkwait.stIssuePtr)                                   // 什么情况下需要阻塞
      val storeAddrWaitforIsIssuing = VecInit((0 until StorePipelineWidth).map(i => {                             // 
        extra.checkwait.memWaitUpdateReq.staIssue(i).valid &&
          extra.checkwait.memWaitUpdateReq.staIssue(i).bits.uop.robIdx.value === in.bits.cf.waitForRobIdx.value   // 新入指令等待的指令是正在发射的指令
      })).asUInt.orR && !in.bits.cf.loadWaitStrict // is waiting for store addr ready
      uop.bits.cf.loadWaitBit := in.bits.cf.loadWaitBit &&                                                      // 新入指令存在load依赖
        !storeAddrWaitforIsIssuing &&                                                                           // 等待的load指令非正在发射
        blockNotReleased                                                                                        // 当前指令在 等待的load指令之后
    }
  })

  // -------------------- 将 loadWaitBit 信息更新到 statusArray 中
  for (((statusUpdate, uop), i) <- statusArray.io.update.zip(s1_dispatchUops_dup.head).zipWithIndex) {
    statusUpdate.data.blocked := params.checkWaitBit.B && uop.bits.cf.loadWaitBit
  }
  // -------------------- 类似唤醒吧，将store发射到FU的信息转发到保留站，进行 chechwait
  // We need to block issue until the corresponding store issues.
  statusArray.io.stIssuePtr := extra.checkwait.stIssuePtr                    // StoreQueue中发射的uop对应的指针
  statusArray.io.memWaitUpdateReq := extra.checkwait.memWaitUpdateReq        // sta功能单元输入 std功能单元输入

  // -------------------- 用于性能统计，无所谓
  for (i <- 0 until params.numDeq) {
    if (params.hasFeedback) {
      extra.feedback(i).rsIdx := s3_issuePtr(i)             // 要发射的指令 在 RS 中的 索引 ---- 给性能计数器使用，不影响逻辑正确性
      extra.feedback(i).isFirstIssue := s3_first_issue(i)   // 给性能计算器使用的，不影响逻辑正确性？？？
    }
  }

  // -------------------- 将load/sta需要重发的指令信息发给RS
  for (i <- 0 until params.numDeq) {
    // feedbackSlow --------------------------------------loaod+sta-----------------  在 load stage 3 阶段检测出 load 指令需要重发   load stage 1 阶段检测出 sta 指令需要重发
    statusArray.io.deqResp(2*i).valid := extra.feedback(i).feedbackSlow.valid
    statusArray.io.deqResp(2*i).bits.rsMask := UIntToOH(extra.feedback(i).feedbackSlow.bits.rsIdx) // 需要重发的load指令在RS中的索引
    statusArray.io.deqResp(2*i).bits.success := extra.feedback(i).feedbackSlow.bits.hit
    statusArray.io.deqResp(2*i).bits.resptype := extra.feedback(i).feedbackSlow.bits.sourceType    // 重发原因
    statusArray.io.deqResp(2*i).bits.dataInvalidSqIdx := extra.feedback(i).feedbackSlow.bits.dataInvalidSqIdx // store - load 前递 -- load 所依赖的 store 的 sqIdx
    // feedbackFast, for load pipeline only --------------load   ------------------- 在 load stage 1 阶段检测出 load 指令需要重发
    statusArray.io.deqResp(2*i+1).valid := extra.feedback(i).feedbackFast.valid
    statusArray.io.deqResp(2*i+1).bits.rsMask := UIntToOH(extra.feedback(i).feedbackFast.bits.rsIdx)
    statusArray.io.deqResp(2*i+1).bits.success := extra.feedback(i).feedbackFast.bits.hit
    statusArray.io.deqResp(2*i+1).bits.resptype := extra.feedback(i).feedbackFast.bits.sourceType
    statusArray.io.deqResp(2*i+1).bits.dataInvalidSqIdx := DontCare
  }
  for ((deq, i) <- io.deq.zipWithIndex) {
    XSPerfAccumulate(s"deq_not_first_issue_$i", deq.fire && !extra.feedback(i).isFirstIssue)
  }
  // logs
  for ((dispatch, i) <- io.fromDispatch.zipWithIndex) {
    XSPerfAccumulate(s"load_wait_$i", dispatch.fire && dispatch.bits.cf.loadWaitBit)
  }
}