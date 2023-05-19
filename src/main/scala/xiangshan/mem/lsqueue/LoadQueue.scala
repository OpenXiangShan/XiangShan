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
import chisel3.ExcitingUtils
import xiangshan.cache.dcache.ReplayCarry
import xiangshan.backend.Bundles.{DynInst, MemExuOutput, MemMicroOpRbExt}
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
  def rdataHelper(uop: DynInst, rdata: UInt): UInt = {
    val fpWen = uop.fpWen
    LookupTree(uop.fuOpType, List(
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
  private val LsExuCnt = backendParams.StaCnt + backendParams.LduCnt
  val canAccept = Output(Bool())
  val sqCanAccept = Input(Bool())
  val needAlloc = Vec(LsExuCnt, Input(Bool()))
  val req = Vec(LsExuCnt, Flipped(ValidIO(new DynInst)))
  val resp = Vec(LsExuCnt, Output(new LqPtr))
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
      val stDataReadySqPtr = Input(new SqPtr)
      val sqEmpty = Input(Bool())
    }
    val loadOut = Vec(LoadPipelineWidth, DecoupledIO(new ExuOutput))
    val ldRawDataOut = Vec(LoadPipelineWidth, Output(new LoadDataFromLQBundle))
    val replay = Vec(LoadPipelineWidth, Decoupled(new LsPipelineBundle))
    val refill = Flipped(ValidIO(new Refill)) 
    val release = Flipped(Valid(new Release))
                              Mux(!io.replaySlow(i).cache_hited, block_cycles_cache(block_ptr_cache(idx)) + data_in_last_beat,
                               Mux(!io.replaySlow(i).cache_no_replay || !io.replaySlow(i).st_ld_check_ok, block_cycles_others(block_ptr_others(idx)), 0.U)))
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

  /**
   * LoadQueueRAW
   */  
  loadQueueRAW.io.redirect <> io.redirect 
  loadQueueRAW.io.rollback <> io.rollback
  loadQueueRAW.io.storeIn <> io.sta.s1.storeAddrIn
  loadQueueRAW.io.correctTableUpdate <> io.correctTableUpdate
  loadQueueRAW.io.stAddrReadySqPtr <> io.sq.stAddrReadySqPtr
  loadQueueRAW.io.ldIssuePtr := loadQueueFlag.io.ldIssuePtr
  loadQueueRAW.io.lqEmpty := loadQueueFlag.io.lqEmpty
  loadQueueRAW.io.sqEmpty <> io.sq.sqEmpty
  loadQueueRAW.io.query <> io.ldu.s2.storeLoadViolationQuery  // enqueue

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
  loadQueueReplay.io.stDataReadySqPtr <> io.sq.stDataReadySqPtr
  loadQueueReplay.io.sqEmpty <> io.sq.sqEmpty
  loadQueueReplay.io.lqFull <> io.lqReplayFull
  loadQueueReplay.io.tlbReplayDelayCycleCtrl <> io.tlbReplayDelayCycleCtrl

  val perfEvents = Seq(loadQueueFlag, loadQueueRAR, loadQueueRAW, loadQueueReplay).flatMap(_.getPerfEvents)
  generatePerfEvent()
  // end
}
