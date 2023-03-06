//package xiangshan.backend.issue
//
//import chipsalliance.rocketchip.config.Parameters
//import chisel3._
//import chisel3.util._
//import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
//import utils._
//
//// for load/sta/std, not drop on redirect
//trait RSDropNotOnRedirect {this: BaseReservationStation =>
//  for ((enqFlush, robIdx) <- s0_enqFlushed.zip(io.fromDispatch.map(_.bits.robIdx))) {
//    enqFlush := robIdx.needFlush(io.redirect)
//  }
//}
//
//// for load/sta
//trait RSImpMemAddrIOConnect { this: BaseReservationStationImp =>
//  extra.feedback <> rs.flatMap(_.extra.feedback)
//  rs.foreach(_.extra.checkwait <> extra.checkwait)
//}
//
//// for load/sta
//// 1) feedback 2) checkWaitBit
//class RSWithMemAddr(params: RSParams)(implicit p: Parameters) extends BaseReservationStation(params) with RSDropNotOnRedirect {
//
//  if (params.checkWaitBit) {
//    // Allocation: store dispatch uops into payload and data array
//    s1_dispatchUops_dup.foreach(_.zip(enqReverse(io.fromDispatch)).zipWithIndex.foreach{ case ((uop, in), i) =>
//      val s0_valid = in.fire && !enqReverse(s0_enqFlushed)(i)
//      when (s0_valid) {
//        // a temp fix for blocked. This will release the load wait for some instructions earlier.
//        // copied from status array
//        val blockNotReleased = isAfter(in.bits.sqIdx, extra.checkwait.stIssuePtr)
//        val storeAddrWaitforIsIssuing = VecInit((0 until StorePipelineWidth).map(i => {
//          extra.checkwait.memWaitUpdateReq.staIssue(i).valid &&
//            extra.checkwait.memWaitUpdateReq.staIssue(i).bits.uop.robIdx.value === in.bits.cf.waitForRobIdx.value
//        })).asUInt.orR && !in.bits.cf.loadWaitStrict // is waiting for store addr ready
//        uop.bits.cf.loadWaitBit := in.bits.cf.loadWaitBit &&
//          !storeAddrWaitforIsIssuing &&
//          blockNotReleased
//      }
//    })
//
//    for (((statusUpdate, uop), i) <- statusArray.io.update.zip(s1_dispatchUops_dup.head).zipWithIndex) {
//      statusUpdate.data.blocked := params.checkWaitBit.B && uop.bits.cf.loadWaitBit
//    }
//    // We need to block issue until the corresponding store issues.
//    statusArray.io.stIssuePtr := extra.checkwait.stIssuePtr
//    statusArray.io.memWaitUpdateReq := extra.checkwait.memWaitUpdateReq
//
//    // logs
//    for ((dispatch, i) <- io.fromDispatch.zipWithIndex) {
//      XSPerfAccumulate(s"load_wait_$i", dispatch.fire && dispatch.bits.cf.loadWaitBit)
//    }
//  }
//
//  if (params.hasFeedback) {
//    for (i <- 0 until params.numDeq) {
//      if (params.hasFeedback) {
//        extra.feedback(i).rsIdx := s2_issuePtr(i)
//        extra.feedback(i).isFirstIssue := s2_first_issue(i)
//      }
//    }
//
//    for (i <- 0 until params.numDeq) {
//      // feedbackSlow
//      statusArray.io.deqResp(2*i).valid := extra.feedback(i).feedbackSlow.valid
//      statusArray.io.deqResp(2*i).bits.rsMask := UIntToOH(extra.feedback(i).feedbackSlow.bits.rsIdx)
//      statusArray.io.deqResp(2*i).bits.success := extra.feedback(i).feedbackSlow.bits.hit
//      statusArray.io.deqResp(2*i).bits.resptype := extra.feedback(i).feedbackSlow.bits.sourceType
//      statusArray.io.deqResp(2*i).bits.dataInvalidSqIdx := extra.feedback(i).feedbackSlow.bits.dataInvalidSqIdx
//      // feedbackFast, for load pipeline only
//      statusArray.io.deqResp(2*i+1).valid := extra.feedback(i).feedbackFast.valid
//      statusArray.io.deqResp(2*i+1).bits.rsMask := UIntToOH(extra.feedback(i).feedbackFast.bits.rsIdx)
//      statusArray.io.deqResp(2*i+1).bits.success := extra.feedback(i).feedbackFast.bits.hit
//      statusArray.io.deqResp(2*i+1).bits.resptype := extra.feedback(i).feedbackFast.bits.sourceType
//      statusArray.io.deqResp(2*i+1).bits.dataInvalidSqIdx := DontCare
//    }
//    for ((deq, i) <- io.deq.zipWithIndex) {
//      XSPerfAccumulate(s"deq_not_first_issue_$i", deq.fire && !extra.feedback(i).isFirstIssue)
//    }
//  }
//
//}
