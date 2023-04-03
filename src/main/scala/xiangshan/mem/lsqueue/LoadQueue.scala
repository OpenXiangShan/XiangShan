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
import xiangshan.mem.mdp._
import xiangshan.backend.rob.RobPtr

class LqPtr(implicit p: Parameters) extends CircularQueuePtr[LqPtr](
  p => p(XSCoreParamsKey).LoadQueueFlagSize
){
}

object LqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): LqPtr = {
    val ptr = Wire(new LqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

trait HasLoadHelper { this: XSModule =>
  def rdataHelper(uop: MicroOp, rdata: UInt): UInt = {
    val fpWen = uop.ctrl.fpWen
    LookupTree(uop.ctrl.fuOpType, List(
      LSUOpType.lb   -> SignExt(rdata(7, 0) , XLEN),
      LSUOpType.lh   -> SignExt(rdata(15, 0), XLEN),
      /*
          riscv-spec-20191213: 12.2 NaN Boxing of Narrower Values
          Any operation that writes a narrower result to an f register must write
          all 1s to the uppermost FLEN−n bits to yield a legal NaN-boxed value.
      */
      LSUOpType.lw   -> Mux(fpWen, FPU.box(rdata, FPU.S), SignExt(rdata(31, 0), XLEN)),
      LSUOpType.ld   -> Mux(fpWen, FPU.box(rdata, FPU.D), SignExt(rdata(63, 0), XLEN)),
      LSUOpType.lbu  -> ZeroExt(rdata(7, 0) , XLEN),
      LSUOpType.lhu  -> ZeroExt(rdata(15, 0), XLEN),
      LSUOpType.lwu  -> ZeroExt(rdata(31, 0), XLEN),
    ))
  }
}

class LqEnqIO(implicit p: Parameters) extends XSBundle {
  val canAccept = Output(Bool())
  val sqCanAccept = Input(Bool())
  val needAlloc = Vec(exuParameters.LsExuCnt, Input(Bool()))
  val req = Vec(exuParameters.LsExuCnt, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(exuParameters.LsExuCnt, Output(new LqPtr))
}

class LqTriggerIO(implicit p: Parameters) extends XSBundle {
  val hitLoadAddrTriggerHitVec = Input(Vec(3, Bool()))
  val lqLoadAddrTriggerHitVec = Output(Vec(3, Bool()))
}


class LoadQueue(implicit p: Parameters) extends XSModule 
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    val redirect = Flipped(Valid(new Redirect)) 
    val enq = new LqEnqIO
    val ldu = new Bundle() {
      val s2 = new Bundle() {
        val storeLoadViolationQuery = Vec(LoadPipelineWidth, Flipped(new LoadViolationQueryIO))
        val loadLoadViolationQuery = Vec(LoadPipelineWidth, Flipped(new LoadViolationQueryIO))
      }
      val s3 = new Bundle() {
        val loadIn = Vec(StorePipelineWidth, Flipped(Valid(new LqWriteBundle)))
      }
    }
    val sta = new Bundle() {
      val s1 = new Bundle() {
        val storeAddrIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
      }
    }
    val std = new Bundle() {
      val s0 = new Bundle() {
        val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new ExuOutput))) // store data, send to sq from rs
      }
    }
    val sq = new Bundle() {
      val stAddrReadySqPtr = Input(new SqPtr)      
      val stAddrReadyVec = Input(Vec(StoreQueueSize, Bool()))
      val stDataReadySqPtr = Input(new SqPtr)
      val stDataReadyVec = Input(Vec(StoreQueueSize, Bool()))
      val stIssuePtr = Input(new SqPtr)
      val sqEmpty = Input(Bool())
    }
    val loadOut = Vec(LoadPipelineWidth, DecoupledIO(new ExuOutput))
    val ldRawDataOut = Vec(LoadPipelineWidth, Output(new LoadDataFromLQBundle))
    val replay = Vec(LoadPipelineWidth, Decoupled(new LsPipelineBundle))
    val refill = Flipped(ValidIO(new Refill)) 
    val release = Flipped(Valid(new Release))
    val rollback = Output(Valid(new Redirect)) 
    val correctTableUpdate = Valid(new CorrectTableUpdate) 
    val rob = Flipped(new RobLsqIO)
    val uncache = new UncacheWordIO
    val trigger = Vec(LoadPipelineWidth, new LqTriggerIO)
    val exceptionAddr = new ExceptionAddrIO
    val lqFlagFull = Output(Bool())
    val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
    val lqCancelCnt = Output(UInt(log2Up(LoadQueueFlagSize+1).W))
    val lqReplayFull = Output(Bool())
    val lqReplayCanAccept = Output(Vec(LoadPipelineWidth, Bool()))
    val tlbReplayDelayCycleCtrl = Vec(4, Input(UInt(ReSelectLen.W))) 
  })

  val loadQueueRAR = Module(new LoadQueueRAR)  //  ld-ld violation
  val loadQueueRAW = Module(new LoadQueueRAW)  //  st-ld violation
  val loadQueueReplay = Module(new LoadQueueReplay)  //  enqueue if need replay
  val loadQueueFlag = Module(new LoadQueueFlag)  //  control state 

  /**
   * LoadQueueRAR
   */  
  loadQueueRAR.io.redirect <> io.redirect
  loadQueueRAR.io.release <> io.release
  loadQueueRAR.io.ldIssuePtr <> loadQueueFlag.io.ldIssuePtr
  loadQueueRAR.io.query <> io.ldu.s2.loadLoadViolationQuery //  enqueue & query
  loadQueueRAR.io.deallocate <> io.ldu.s3.loadIn

  /**
   * LoadQueueRAW
   */  
  loadQueueRAW.io.redirect <> io.redirect 
  loadQueueRAW.io.rollback <> io.rollback
  loadQueueRAW.io.storeIn <> io.sta.s1.storeAddrIn
  loadQueueRAW.io.correctTableUpdate <> io.correctTableUpdate
  loadQueueRAW.io.stAddrReadySqPtr <> io.sq.stAddrReadySqPtr
  loadQueueRAW.io.stIssuePtr <> io.sq.stIssuePtr
  loadQueueRAW.io.ldIssuePtr := loadQueueFlag.io.ldIssuePtr
  loadQueueRAW.io.lqEmpty := loadQueueFlag.io.lqEmpty
  loadQueueRAW.io.sqEmpty <> io.sq.sqEmpty
  loadQueueRAW.io.query <> io.ldu.s2.storeLoadViolationQuery  // enqueue
  loadQueueRAW.io.deallocate <> io.ldu.s3.loadIn
  
  /**
   * LoadQueueFlag
   */  
  loadQueueFlag.io.redirect <> io.redirect
  loadQueueFlag.io.enq <> io.enq 
  loadQueueFlag.io.loadIn <> io.ldu.s3.loadIn
  loadQueueFlag.io.loadOut <> io.loadOut
  loadQueueFlag.io.ldRawDataOut <> io.ldRawDataOut
  loadQueueFlag.io.rob <> io.rob 
  loadQueueFlag.io.uncache <> io.uncache
  loadQueueFlag.io.trigger <> io.trigger
  loadQueueFlag.io.exceptionAddr <> io.exceptionAddr
  loadQueueFlag.io.lqFull <> io.lqFlagFull
  loadQueueFlag.io.lqDeq <> io.lqDeq
  loadQueueFlag.io.lqCancelCnt <> io.lqCancelCnt

  /**
   * LoadQueueReplay
   */  
  loadQueueReplay.io.redirect <> io.redirect
  loadQueueReplay.io.enq <> io.ldu.s3.loadIn
  loadQueueReplay.io.storeAddrIn <> io.sta.s1.storeAddrIn
  loadQueueReplay.io.storeDataIn <> io.std.s0.storeDataIn
  loadQueueReplay.io.replay <> io.replay
  loadQueueReplay.io.refill <> io.refill 
  loadQueueReplay.io.stAddrReadySqPtr <> io.sq.stAddrReadySqPtr
  loadQueueReplay.io.stAddrReadyVec <> io.sq.stAddrReadyVec
  loadQueueReplay.io.stDataReadySqPtr <> io.sq.stDataReadySqPtr
  loadQueueReplay.io.stDataReadyVec <> io.sq.stDataReadyVec
  loadQueueReplay.io.sqEmpty <> io.sq.sqEmpty
  loadQueueReplay.io.lqFull <> io.lqReplayFull
  loadQueueReplay.io.lqCanAccept <> io.lqReplayCanAccept
  loadQueueReplay.io.tlbReplayDelayCycleCtrl <> io.tlbReplayDelayCycleCtrl
  loadQueueReplay.io.ldIssuePtr := loadQueueFlag.io.ldIssuePtr

  // perf cnt
  val perfEvents = Seq(loadQueueFlag, loadQueueRAR, loadQueueRAW, loadQueueReplay).flatMap(_.getPerfEvents)
  generatePerfEvent()
  // end
}