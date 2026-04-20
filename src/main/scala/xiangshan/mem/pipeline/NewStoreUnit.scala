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
import utility._
import xiangshan._
import xiangshan.backend.Bundles.{ExuInput, NewExuOutput, StoreUnitToLFST}
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.mem.Bundles._
import xiangshan.mem.StoreStage._

class StoreUnitS0(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS0()
) extends StoreUnitStage(param) {

}

class StoreUnitS1(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS1()
) extends StoreUnitStage(param) {

}

class StoreUnitS2(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS2()
) extends StoreUnitStage(param) {

}

class StoreUnitS3(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS3()
) extends StoreUnitStage(param) {

}

class StoreUnitS4(param: ExeUnitParams)(
  implicit p: Parameters,
  override implicit val s: StoreStage.StoreStage = StoreS4()
) extends StoreUnitStage(param) {
  
}

class StoreUnitIO(val param: ExeUnitParams)(implicit p: Parameters) extends XSBundle {
  val redirect = Flipped(ValidIO(new Redirect))
  val csrCtrl = Flipped(new CustomCSRCtrlIO)
  val csrTrigger = Input(new CsrTriggerBundle)
  // Request sources
  val stin = Flipped(Decoupled(new ExuInput(param, hasCopySrc = true)))
  val vecstin = Flipped(Decoupled(new VecPipeBundle(isVStore = true)))
  val prefetchReq = Flipped(DecoupledIO(new StorePrefetchReq))
  // TLB / PMA / PMP
  val tlb = new TlbRequestIO
  val pmp = Flipped(new PMPRespBundle)
  // DCache
  val dcache = new DCacheStoreIO
  // MDP
  val updateLFST = Valid(new StoreUnitToLFST)
  // Store mask, send to sq in s0
  val toSqMask = Valid(new StoreMaskBundle)
  // Store addr, send to sq in s1
  val toSqAddr = ValidIO(new StoreAddrIO)
  // Exception info and memory type, send to sq in s2
  val toSqAddrRe = Output(new StoreAddrIO)
  // UnalignTail req addr, send to sq in s2
  val toUnalignQueue = DecoupledIO(new UnalignQueueIO)
  // Nuke check req to LoadUnit
  val staNukeQueryReq = ValidIO(new StoreNukeQueryReq)
  // Prefetch Train
  val prefetchTrain = ValidIO(new LsPrefetchTrainBundle())
  // Feedback to RS in s2, for store issue control
  val feedbackSlow = ValidIO(new RSFeedback)
  // Writeback
  val stout = new NewExuOutput(param)
  val vecstout = DecoupledIO(new VecPipelineFeedbackIO(isVStore = true))
  val exceptionInfo = ValidIO(new MemExceptionInfo)
  // TODO: Consider these wire
  // s1_prefetch_spec? s2_prefetch_spec?
  // debug_ls ? s0_s1_s2_valid
}

class NewStoreUnit(val param: ExeUnitParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new StoreUnitIO(param))
  
  val s0 = Module(new StoreUnitS0(param))
  val s1 = Module(new StoreUnitS1(param))
  val s2 = Module(new StoreUnitS2(param))
  val s3 = Module(new StoreUnitS3(param))
  val s4 = Module(new StoreUnitS4(param))
}

abstract class StoreUnitStage(val param: ExeUnitParams)(
  implicit p: Parameters,
  implicit val s: StoreStage
) extends XSModule with OnStoreStage {
  val io_pipeIn = if (afterS1) {
    Some(IO(Flipped(DecoupledIO(new StoreStageIO()(p, prevStage(s))))))
  } else None
  val io_pipeOut = if (!lastStage) {
    Some(IO(DecoupledIO(new StoreStageIO)))
  } else None

  def <>(that: StoreUnitStage): Unit = {
    this.io_pipeIn.foreach(_ <> that.io_pipeOut.get)
  }
}