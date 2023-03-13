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

class LqPtr(implicit p: Parameters) extends CircularQueuePtr[LqPtr](
  p => p(XSCoreParamsKey).LoadQueueSize
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

class LqPaddrWriteBundle(implicit p: Parameters) extends XSBundle {
  val paddr = Output(UInt(PAddrBits.W))
  val lqIdx = Output(new LqPtr)
}

class LqVaddrWriteBundle(implicit p: Parameters) extends XSBundle {
  val vaddr = Output(UInt(VAddrBits.W))
  val lqIdx = Output(new LqPtr)
}

class LqTriggerIO(implicit p: Parameters) extends XSBundle {
  val hitLoadAddrTriggerHitVec = Input(Vec(3, Bool()))
  val lqLoadAddrTriggerHitVec = Output(Vec(3, Bool()))
}

class LoadQueueIOBundle(implicit p: Parameters) extends XSBundle {
  val enq = new LqEnqIO
  val brqRedirect = Flipped(ValidIO(new Redirect))
  val loadOut = Vec(LoadPipelineWidth, Decoupled(new LsPipelineBundle)) // select load from lq to load pipeline 
  val loadPaddrIn = Vec(LoadPipelineWidth, Flipped(Valid(new LqPaddrWriteBundle)))
  val loadVaddrIn = Vec(LoadPipelineWidth, Flipped(Valid(new LqVaddrWriteBundle)))
  val loadIn = Vec(LoadPipelineWidth, Flipped(Valid(new LqWriteBundle)))
  val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
  val s2_load_data_forwarded = Vec(LoadPipelineWidth, Input(Bool()))
  val s3_delayed_load_error = Vec(LoadPipelineWidth, Input(Bool()))
  val s2_dcache_require_replay = Vec(LoadPipelineWidth, Input(Bool()))
  val s3_replay_from_fetch = Vec(LoadPipelineWidth, Input(Bool()))
  val ldout = Vec(2, DecoupledIO(new ExuOutput)) // writeback int load
  val ldRawDataOut = Vec(2, Output(new LoadDataFromLQBundle))
  val load_s1 = Vec(LoadPipelineWidth, Flipped(new PipeLoadForwardQueryIO)) // TODO: to be renamed
  val loadViolationQuery = Vec(LoadPipelineWidth, Flipped(new LoadViolationQueryIO))
  val rob = Flipped(new RobLsqIO)
  val rollback = Output(Valid(new Redirect)) // replay now starts from load instead of store
  val refill = Flipped(ValidIO(new Refill)) // TODO: to be renamed
  val release = Flipped(ValidIO(new Release))
  val uncache = new UncacheWordIO
  val exceptionAddr = new ExceptionAddrIO
  val lqFull = Output(Bool())
  val lqCancelCnt = Output(UInt(log2Up(LoadQueueSize + 1).W))
  val trigger = Vec(LoadPipelineWidth, new LqTriggerIO)

  // for load replay (recieve feedback from load pipe line)
  val replayFast = Vec(LoadPipelineWidth, Flipped(new LoadToLsqFastIO))
  val replaySlow = Vec(LoadPipelineWidth, Flipped(new LoadToLsqSlowIO))

  val storeDataValidVec = Vec(StoreQueueSize, Input(Bool()))

  val tlbReplayDelayCycleCtrl = Vec(4, Input(UInt(ReSelectLen.W)))
}

// Load Queue
class LoadQueue(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasPerfEvents
{
  val io = IO(new LoadQueueIOBundle())

  // dontTouch(io)

  println("LoadQueue: size:" + LoadQueueSize)

  val uop = Reg(Vec(LoadQueueSize, new MicroOp))
  val replayCarryReg = RegInit(VecInit(List.fill(LoadQueueSize)(ReplayCarry.init)))
  // val data = Reg(Vec(LoadQueueSize, new LsRobEntry))
  val dataModule = Module(new LoadQueueDataWrapper(LoadQueueSize, wbNumWrite = LoadPipelineWidth))
  dataModule.io := DontCare
  // vaddrModule's read port 0 for exception addr, port 1 for uncache vaddr read, port {2, 3} for load replay
  val vaddrModule = Module(new SyncDataModuleTemplate(UInt(VAddrBits.W), LoadQueueSize, numRead = 1 + 1 + LoadPipelineWidth, numWrite = LoadPipelineWidth))
  vaddrModule.io := DontCare
  val vaddrTriggerResultModule = Module(new SyncDataModuleTemplate(Vec(3, Bool()), LoadQueueSize, numRead = LoadPipelineWidth, numWrite = LoadPipelineWidth))
  vaddrTriggerResultModule.io := DontCare
  val allocated = RegInit(VecInit(List.fill(LoadQueueSize)(false.B))) // lq entry has been allocated
  val datavalid = RegInit(VecInit(List.fill(LoadQueueSize)(false.B))) // data is valid
  val writebacked = RegInit(VecInit(List.fill(LoadQueueSize)(false.B))) // inst has been writebacked to CDB
  val released = RegInit(VecInit(List.fill(LoadQueueSize)(false.B))) // load data has been released by dcache
  val error = RegInit(VecInit(List.fill(LoadQueueSize)(false.B))) // load data has been corrupted
  val miss = Reg(Vec(LoadQueueSize, Bool())) // load inst missed, waiting for miss queue to accept miss request
  // val listening = Reg(Vec(LoadQueueSize, Bool())) // waiting for refill result
  val pending = Reg(Vec(LoadQueueSize, Bool())) // mmio pending: inst is an mmio inst, it will not be executed until it reachs the end of rob
  val refilling = WireInit(VecInit(List.fill(LoadQueueSize)(false.B))) // inst has been writebacked to CDB

  /**
    * used for load replay control
    */

  val tlb_hited = RegInit(VecInit(List.fill(LoadQueueSize)(true.B)))
  val ld_ld_check_ok = RegInit(VecInit(List.fill(LoadQueueSize)(true.B)))
  val st_ld_check_ok = RegInit(VecInit(List.fill(LoadQueueSize)(true.B)))
  val cache_bank_no_conflict = RegInit(VecInit(List.fill(LoadQueueSize)(true.B)))
  val cache_no_replay = RegInit(VecInit(List.fill(LoadQueueSize)(true.B)))
  val forward_data_valid = RegInit(VecInit(List.fill(LoadQueueSize)(true.B)))
  val cache_hited = RegInit(VecInit(List.fill(LoadQueueSize)(true.B)))


  /**
    * used for re-select control
    */

  val credit = RegInit(VecInit(List.fill(LoadQueueSize)(0.U(ReSelectLen.W))))
  
  // ptrs to control which cycle to choose
  val block_ptr_tlb = RegInit(VecInit(List.fill(LoadQueueSize)(0.U(2.W))))
  val block_ptr_cache = RegInit(VecInit(List.fill(LoadQueueSize)(0.U(2.W))))
  val block_ptr_others = RegInit(VecInit(List.fill(LoadQueueSize)(0.U(2.W))))

  // specific cycles to block
  val block_cycles_tlb = Reg(Vec(4, UInt(ReSelectLen.W)))
  block_cycles_tlb := io.tlbReplayDelayCycleCtrl
  val block_cycles_cache = RegInit(VecInit(Seq(11.U(ReSelectLen.W), 0.U(ReSelectLen.W), 31.U(ReSelectLen.W), 0.U(ReSelectLen.W))))
  val block_cycles_others = RegInit(VecInit(Seq(0.U(ReSelectLen.W), 0.U(ReSelectLen.W), 0.U(ReSelectLen.W), 0.U(ReSelectLen.W))))

  val sel_blocked = RegInit(VecInit(List.fill(LoadQueueSize)(false.B)))

  // data forward block
  val block_sq_idx = RegInit(VecInit(List.fill(LoadQueueSize)(0.U((log2Ceil(StoreQueueSize).W)))))
  val block_by_data_forward_fail = RegInit(VecInit(List.fill(LoadQueueSize)(false.B)))

  // dcache miss block
  val miss_mshr_id = RegInit(VecInit(List.fill(LoadQueueSize)(0.U((log2Up(cfg.nMissEntries).W)))))
  val block_by_cache_miss = RegInit(VecInit(List.fill(LoadQueueSize)(false.B)))

  val true_cache_miss_replay = WireInit(VecInit(List.fill(LoadQueueSize)(false.B)))
  (0 until LoadQueueSize).map{i => {
    true_cache_miss_replay(i) := tlb_hited(i) && ld_ld_check_ok(i) && st_ld_check_ok(i) && cache_bank_no_conflict(i) && 
                                 cache_no_replay(i) && forward_data_valid(i) && !cache_hited(i)
  }}
  
  val creditUpdate = WireInit(VecInit(List.fill(LoadQueueSize)(0.U(ReSelectLen.W))))

  credit := creditUpdate

  (0 until LoadQueueSize).map(i => {
    creditUpdate(i) := Mux(credit(i) > 0.U(ReSelectLen.W), credit(i) - 1.U(ReSelectLen.W), credit(i))
    sel_blocked(i) := creditUpdate(i) =/= 0.U(ReSelectLen.W)
  })

  (0 until LoadQueueSize).map(i => {
    block_by_data_forward_fail(i) := Mux(block_by_data_forward_fail(i) === true.B && io.storeDataValidVec(block_sq_idx(i)) === true.B , false.B, block_by_data_forward_fail(i))
  })

  (0 until LoadQueueSize).map(i => {
    block_by_cache_miss(i) := Mux(block_by_cache_miss(i) === true.B && io.refill.valid && io.refill.bits.id === miss_mshr_id(i), false.B, block_by_cache_miss(i))
    when(creditUpdate(i) === 0.U && block_by_cache_miss(i) === true.B) {
      block_by_cache_miss(i) := false.B
    }
    when(block_by_cache_miss(i) === true.B && io.refill.valid && io.refill.bits.id === miss_mshr_id(i)) {
      creditUpdate(i) := 0.U
    }
  })

  val debug_mmio = Reg(Vec(LoadQueueSize, Bool())) // mmio: inst is an mmio inst
  val debug_paddr = Reg(Vec(LoadQueueSize, UInt(PAddrBits.W))) // mmio: inst is an mmio inst

  val enqPtrExt = RegInit(VecInit((0 until io.enq.req.length).map(_.U.asTypeOf(new LqPtr))))
  val deqPtrExt = RegInit(0.U.asTypeOf(new LqPtr))
  val deqPtrExtNext = Wire(new LqPtr)

  val enqPtr = enqPtrExt(0).value
  val deqPtr = deqPtrExt.value

  val validCount = distanceBetween(enqPtrExt(0), deqPtrExt)
  val allowEnqueue = validCount <= (LoadQueueSize - LoadPipelineWidth).U

  val deqMask = UIntToMask(deqPtr, LoadQueueSize)
  val enqMask = UIntToMask(enqPtr, LoadQueueSize)

  val commitCount = RegNext(io.rob.lcommit)

  val release1cycle = io.release
  val release2cycle = RegNext(io.release)
  val release2cycle_dup_lsu = RegNext(io.release)

  /**
    * Enqueue at dispatch
    *
    * Currently, LoadQueue only allows enqueue when #emptyEntries > EnqWidth
    */
  io.enq.canAccept := allowEnqueue

  val canEnqueue = io.enq.req.map(_.valid)
  val enqCancel = io.enq.req.map(_.bits.robIdx.needFlush(io.brqRedirect))
  for (i <- 0 until io.enq.req.length) {
    val offset = if (i == 0) 0.U else PopCount(io.enq.needAlloc.take(i))
    val lqIdx = enqPtrExt(offset)
    val index = io.enq.req(i).bits.lqIdx.value
    when (canEnqueue(i) && !enqCancel(i)) {
      uop(index) := io.enq.req(i).bits
      // NOTE: the index will be used when replay
      uop(index).lqIdx := lqIdx
      allocated(index) := true.B
      datavalid(index) := false.B
      writebacked(index) := false.B
      released(index) := false.B
      miss(index) := false.B
      pending(index) := false.B
      error(index) := false.B

      /**
        * used for load replay control
        */
      tlb_hited(index) := true.B
      ld_ld_check_ok(index) := true.B
      st_ld_check_ok(index) := true.B
      cache_bank_no_conflict(index) := true.B
      cache_no_replay(index) := true.B
      forward_data_valid(index) := true.B
      cache_hited(index) := true.B

      /**
        * used for delaying load(block-ptr to control how many cycles to block)
        */
      credit(index) := 0.U(ReSelectLen.W)
      block_ptr_tlb(index) := 0.U(2.W)
      block_ptr_cache(index) := 0.U(2.W)
      block_ptr_others(index) := 0.U(2.W)

      block_by_data_forward_fail(index) := false.B
      block_by_cache_miss(index) := false.B

      XSError(!io.enq.canAccept || !io.enq.sqCanAccept, s"must accept $i\n")
      XSError(index =/= lqIdx.value, s"must be the same entry $i\n")
    }
    io.enq.resp(i) := lqIdx
  }
  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(io.enq.req.map(_.valid)))}\n")

  val lastCycleRedirect = RegNext(io.brqRedirect)
  val lastlastCycleRedirect = RegNext(lastCycleRedirect)

  // replay logic
  // replay is splited into 2 stages

  // stage1: select 2 entries and read their vaddr
  val s0_block_load_mask = WireInit(VecInit((0 until LoadQueueSize).map(x=>false.B)))
  val s1_block_load_mask = RegNext(s0_block_load_mask)
  val s2_block_load_mask = RegNext(s1_block_load_mask)

  val loadReplaySel = Wire(Vec(LoadPipelineWidth, UInt(log2Up(LoadQueueSize).W))) // index selected last cycle
  val loadReplaySelV = Wire(Vec(LoadPipelineWidth, Bool())) // index selected in last cycle is valid

  val loadReplaySelVec = VecInit((0 until LoadQueueSize).map(i => {
    val blocked = s1_block_load_mask(i) || s2_block_load_mask(i) || sel_blocked(i) || block_by_data_forward_fail(i) || block_by_cache_miss(i)
    allocated(i) && (!tlb_hited(i) || !ld_ld_check_ok(i) || !st_ld_check_ok(i) || !cache_bank_no_conflict(i) || !cache_no_replay(i) || !forward_data_valid(i) || !cache_hited(i)) && !blocked
  })).asUInt() // use uint instead vec to reduce verilog lines

  val remReplayDeqMask = Seq.tabulate(LoadPipelineWidth)(getRemBits(deqMask)(_))

  // generate lastCycleSelect mask
  val remReplayFireMask = Seq.tabulate(LoadPipelineWidth)(rem => getRemBits(UIntToOH(loadReplaySel(rem)))(rem))

  val loadReplayRemSelVecFire = Seq.tabulate(LoadPipelineWidth)(rem => getRemBits(loadReplaySelVec)(rem) & ~remReplayFireMask(rem))
  val loadReplayRemSelVecNotFire = Seq.tabulate(LoadPipelineWidth)(getRemBits(loadReplaySelVec)(_))

  val replayRemFire = Seq.tabulate(LoadPipelineWidth)(rem => WireInit(false.B))

  val loadReplayRemSel = Seq.tabulate(LoadPipelineWidth)(rem => Mux(
    replayRemFire(rem),
    getFirstOne(toVec(loadReplayRemSelVecFire(rem)), remReplayDeqMask(rem)),
    getFirstOne(toVec(loadReplayRemSelVecNotFire(rem)), remReplayDeqMask(rem))
  ))

  val loadReplaySelGen = Wire(Vec(LoadPipelineWidth, UInt(log2Up(LoadQueueSize).W)))
  val loadReplaySelVGen = Wire(Vec(LoadPipelineWidth, Bool()))

  (0 until LoadPipelineWidth).foreach(index => {
    loadReplaySelGen(index) := (
      if (LoadPipelineWidth > 1) Cat(loadReplayRemSel(index), index.U(log2Ceil(LoadPipelineWidth).W))
      else loadReplayRemSel(index)
    )
    loadReplaySelVGen(index) := Mux(replayRemFire(index), loadReplayRemSelVecFire(index).asUInt.orR, loadReplayRemSelVecNotFire(index).asUInt.orR)
  })

  (0 until LoadPipelineWidth).map(i => {
    // vaddrModule rport 0 and 1 is used by exception and mmio 
    vaddrModule.io.raddr(2 + i) := loadReplaySelGen(i)
  })

  (0 until LoadPipelineWidth).map(i => {
    loadReplaySel(i) := RegNext(loadReplaySelGen(i))
    loadReplaySelV(i) := RegNext(loadReplaySelVGen(i), init = false.B)
  })
  
  // stage2: replay to load pipeline (if no load in S0)
  (0 until LoadPipelineWidth).map(i => {
    when(replayRemFire(i)) {
      s0_block_load_mask(loadReplaySel(i)) := true.B
    }
  })
  
  // init
  (0 until LoadPipelineWidth).map(i => {
    replayRemFire(i) := false.B
  })

  for(i <- 0 until LoadPipelineWidth) {
    val replayIdx = loadReplaySel(i)
    val notRedirectLastCycle = !uop(replayIdx).robIdx.needFlush(RegNext(io.brqRedirect))

    io.loadOut(i).valid := loadReplaySelV(i) && notRedirectLastCycle

    io.loadOut(i).bits := DontCare
    io.loadOut(i).bits.uop := uop(replayIdx)
    io.loadOut(i).bits.vaddr := vaddrModule.io.rdata(LoadPipelineWidth + i)
    io.loadOut(i).bits.mask := genWmask(vaddrModule.io.rdata(LoadPipelineWidth + i), uop(replayIdx).ctrl.fuOpType(1,0))
    io.loadOut(i).bits.isFirstIssue := false.B
    io.loadOut(i).bits.isLoadReplay := true.B
    io.loadOut(i).bits.replayCarry := replayCarryReg(replayIdx)
    io.loadOut(i).bits.mshrid := miss_mshr_id(replayIdx)
    io.loadOut(i).bits.forward_tlDchannel := true_cache_miss_replay(replayIdx)

    when(io.loadOut(i).fire) {
      replayRemFire(i) := true.B
    }

  }
  /**
    * Writeback load from load units
    *
    * Most load instructions writeback to regfile at the same time.
    * However,
    *   (1) For an mmio instruction with exceptions, it writes back to ROB immediately.
    *   (2) For an mmio instruction without exceptions, it does not write back.
    * The mmio instruction will be sent to lower level when it reaches ROB's head.
    * After uncache response, it will write back through arbiter with loadUnit.
    *   (3) For cache misses, it is marked miss and sent to dcache later.
    * After cache refills, it will write back through arbiter with loadUnit.
    */
  for (i <- 0 until LoadPipelineWidth) {
    dataModule.io.wb.wen(i) := false.B
    dataModule.io.paddr.wen(i) := false.B
    vaddrModule.io.wen(i) := false.B
    vaddrTriggerResultModule.io.wen(i) := false.B
    val loadWbIndex = io.loadIn(i).bits.uop.lqIdx.value

    // most lq status need to be updated immediately after load writeback to lq
    // flag bits in lq needs to be updated accurately
    when(io.loadIn(i).fire()) {
      when(io.loadIn(i).bits.miss) {
        XSInfo(io.loadIn(i).valid, "load miss write to lq idx %d pc 0x%x vaddr %x paddr %x mask %x forwardData %x forwardMask: %x mmio %x\n",
          io.loadIn(i).bits.uop.lqIdx.asUInt,
          io.loadIn(i).bits.uop.cf.pc,
          io.loadIn(i).bits.vaddr,
          io.loadIn(i).bits.paddr,
          io.loadIn(i).bits.mask,
          io.loadIn(i).bits.forwardData.asUInt,
          io.loadIn(i).bits.forwardMask.asUInt,
          io.loadIn(i).bits.mmio
        )
      }.otherwise {
        XSInfo(io.loadIn(i).valid, "load hit write to cbd lqidx %d pc 0x%x vaddr %x paddr %x mask %x forwardData %x forwardMask: %x mmio %x\n",
        io.loadIn(i).bits.uop.lqIdx.asUInt,
        io.loadIn(i).bits.uop.cf.pc,
        io.loadIn(i).bits.vaddr,
        io.loadIn(i).bits.paddr,
        io.loadIn(i).bits.mask,
        io.loadIn(i).bits.forwardData.asUInt,
        io.loadIn(i).bits.forwardMask.asUInt,
        io.loadIn(i).bits.mmio
      )}
      if(EnableFastForward){
        datavalid(loadWbIndex) := !io.loadIn(i).bits.miss &&
          !io.loadIn(i).bits.mmio && // mmio data is not valid until we finished uncache access
          !io.s2_dcache_require_replay(i) // do not writeback if that inst will be resend from rs
      } else {
        datavalid(loadWbIndex) := !io.loadIn(i).bits.miss &&
          !io.loadIn(i).bits.mmio // mmio data is not valid until we finished uncache access
      }
      writebacked(loadWbIndex) := !io.loadIn(i).bits.miss && !io.loadIn(i).bits.mmio

      debug_mmio(loadWbIndex) := io.loadIn(i).bits.mmio
      debug_paddr(loadWbIndex) := io.loadIn(i).bits.paddr

      val dcacheMissed = io.loadIn(i).bits.miss && !io.loadIn(i).bits.mmio
      if(EnableFastForward){
        miss(loadWbIndex) := dcacheMissed && !io.s2_load_data_forwarded(i) && !io.s2_dcache_require_replay(i)
      } else {
        miss(loadWbIndex) := dcacheMissed && !io.s2_load_data_forwarded(i)
      }
      pending(loadWbIndex) := io.loadIn(i).bits.mmio
      released(loadWbIndex) := release2cycle.valid &&
        io.loadIn(i).bits.paddr(PAddrBits-1, DCacheLineOffset) === release2cycle.bits.paddr(PAddrBits-1, DCacheLineOffset) ||
        release1cycle.valid &&
        io.loadIn(i).bits.paddr(PAddrBits-1, DCacheLineOffset) === release1cycle.bits.paddr(PAddrBits-1, DCacheLineOffset)
    }

    // data bit in lq can be updated when load_s2 valid
    // when(io.loadIn(i).bits.lq_data_wen){
    //   val loadWbData = Wire(new LQDataEntry)
    //   loadWbData.paddr := io.loadIn(i).bits.paddr
    //   loadWbData.mask := io.loadIn(i).bits.mask
    //   loadWbData.data := io.loadIn(i).bits.forwardData.asUInt // fwd data
    //   loadWbData.fwdMask := io.loadIn(i).bits.forwardMask
    //   dataModule.io.wbWrite(i, loadWbIndex, loadWbData)
    //   dataModule.io.wb.wen(i) := true.B

    //   // dirty code for load instr
    //   uop(loadWbIndex).pdest := io.loadIn(i).bits.uop.pdest
    //   uop(loadWbIndex).cf := io.loadIn(i).bits.uop.cf
    //   uop(loadWbIndex).ctrl := io.loadIn(i).bits.uop.ctrl
    //   uop(loadWbIndex).debugInfo := io.loadIn(i).bits.uop.debugInfo

    //   vaddrTriggerResultModule.io.waddr(i) := loadWbIndex
    //   vaddrTriggerResultModule.io.wdata(i) := io.trigger(i).hitLoadAddrTriggerHitVec

    //   vaddrTriggerResultModule.io.wen(i) := true.B
    // }

    // dirty code to reduce load_s2.valid fanout
    when(io.loadIn(i).bits.lq_data_wen_dup(0)){
      dataModule.io.wbWrite(i, loadWbIndex, io.loadIn(i).bits.mask)
      dataModule.io.wb.wen(i) := true.B
    }
    // dirty code for load instr
    when(io.loadIn(i).bits.lq_data_wen_dup(1)){
      uop(loadWbIndex).pdest := io.loadIn(i).bits.uop.pdest
    }
    when(io.loadIn(i).bits.lq_data_wen_dup(2)){
      uop(loadWbIndex).cf := io.loadIn(i).bits.uop.cf
    }
    when(io.loadIn(i).bits.lq_data_wen_dup(3)){
      uop(loadWbIndex).ctrl := io.loadIn(i).bits.uop.ctrl
    }
    when(io.loadIn(i).bits.lq_data_wen_dup(4)){
      uop(loadWbIndex).debugInfo := io.loadIn(i).bits.uop.debugInfo
    }
    when(io.loadIn(i).bits.lq_data_wen_dup(5)){
      vaddrTriggerResultModule.io.waddr(i) := loadWbIndex
      vaddrTriggerResultModule.io.wdata(i) := io.trigger(i).hitLoadAddrTriggerHitVec
      vaddrTriggerResultModule.io.wen(i) := true.B
    }

    when(io.loadPaddrIn(i).valid) {
      dataModule.io.paddr.wen(i) := true.B
      dataModule.io.paddr.waddr(i) := io.loadPaddrIn(i).bits.lqIdx.value
      dataModule.io.paddr.wdata(i) := io.loadPaddrIn(i).bits.paddr
    }
    
    // update vaddr in load S1
    when(io.loadVaddrIn(i).valid) {
      vaddrModule.io.wen(i) := true.B
      vaddrModule.io.waddr(i) := io.loadVaddrIn(i).bits.lqIdx.value
      vaddrModule.io.wdata(i) := io.loadVaddrIn(i).bits.vaddr
    }

    /**
      * used for feedback and replay
      */
    when(io.replayFast(i).valid){
      val idx = io.replayFast(i).ld_idx

      ld_ld_check_ok(idx) := io.replayFast(i).ld_ld_check_ok
      st_ld_check_ok(idx) := io.replayFast(i).st_ld_check_ok
      cache_bank_no_conflict(idx) := io.replayFast(i).cache_bank_no_conflict

      // update tlbReqFirstTime
      uop(idx).debugInfo := io.replayFast(i).debug

      when(io.replayFast(i).needreplay) {
        creditUpdate(idx) := block_cycles_others(block_ptr_others(idx))
        block_ptr_others(idx) := Mux(block_ptr_others(idx) === 3.U(2.W), block_ptr_others(idx), block_ptr_others(idx) + 1.U(2.W))
        // try to replay this load in next cycle
        s1_block_load_mask(idx) := false.B
        s2_block_load_mask(idx) := false.B

        // replay this load in next cycle
        loadReplaySelGen(idx(log2Ceil(LoadPipelineWidth) - 1, 0)) := idx
        loadReplaySelVGen(idx(log2Ceil(LoadPipelineWidth) - 1, 0)) := true.B
      }
    }

    when(io.replaySlow(i).valid){
      val idx = io.replaySlow(i).ld_idx

      tlb_hited(idx) := io.replaySlow(i).tlb_hited
      st_ld_check_ok(idx) := io.replaySlow(i).st_ld_check_ok
      cache_no_replay(idx) := io.replaySlow(i).cache_no_replay
      forward_data_valid(idx) := io.replaySlow(i).forward_data_valid
      replayCarryReg(idx) := io.replaySlow(i).replayCarry
      cache_hited(idx) := io.replaySlow(i).cache_hited

      // update tlbReqFirstTime
      uop(idx).debugInfo := io.replaySlow(i).debug

      val invalid_sq_idx = io.replaySlow(i).data_invalid_sq_idx

      when(io.replaySlow(i).needreplay) {
        // update credit and ptr
        val data_in_last_beat = io.replaySlow(i).data_in_last_beat
        creditUpdate(idx) := Mux( !io.replaySlow(i).tlb_hited, block_cycles_tlb(block_ptr_tlb(idx)), 
                              Mux(!io.replaySlow(i).cache_hited, block_cycles_cache(block_ptr_cache(idx)) + data_in_last_beat,
                               Mux(!io.replaySlow(i).cache_no_replay || !io.replaySlow(i).st_ld_check_ok, block_cycles_others(block_ptr_others(idx)), 0.U)))
        when(!io.replaySlow(i).tlb_hited) {
          block_ptr_tlb(idx) := Mux(block_ptr_tlb(idx) === 3.U(2.W), block_ptr_tlb(idx), block_ptr_tlb(idx) + 1.U(2.W))
        }.elsewhen(!io.replaySlow(i).cache_hited) {
          block_ptr_cache(idx) := Mux(block_ptr_cache(idx) === 3.U(2.W), block_ptr_cache(idx), block_ptr_cache(idx) + 1.U(2.W))
        }.elsewhen(!io.replaySlow(i).cache_no_replay || !io.replaySlow(i).st_ld_check_ok) {
          block_ptr_others(idx) := Mux(block_ptr_others(idx) === 3.U(2.W), block_ptr_others(idx), block_ptr_others(idx) + 1.U(2.W))
        }
      }

      // special case: data forward fail
      block_by_data_forward_fail(idx) := false.B

      when(!io.replaySlow(i).forward_data_valid && io.replaySlow(i).tlb_hited) {
        when(!io.storeDataValidVec(invalid_sq_idx)) {
          block_by_data_forward_fail(idx) := true.B
          block_sq_idx(idx) := invalid_sq_idx
        }
      }

      // special case: cache miss
      miss_mshr_id(idx) := io.replaySlow(i).miss_mshr_id
      block_by_cache_miss(idx) := io.replaySlow(i).tlb_hited && io.replaySlow(i).cache_no_replay && io.replaySlow(i).st_ld_check_ok && // this load tlb hit and no cache replay
                                  !io.replaySlow(i).cache_hited && !io.replaySlow(i).can_forward_full_data && // cache miss
                                  !(io.refill.valid && io.refill.bits.id === io.replaySlow(i).miss_mshr_id) && // no refill in this cycle
                                  creditUpdate(idx) =/= 0.U // credit is not zero
    }

  }

  when(io.refill.valid) {
    XSDebug("miss resp: paddr:0x%x data %x\n", io.refill.bits.addr, io.refill.bits.data)
  }

  // NOTE: we don't refill data from dcache now! 

  val s2_dcache_require_replay = WireInit(VecInit((0 until LoadPipelineWidth).map(i =>{
    RegNext(io.loadIn(i).fire()) && RegNext(io.s2_dcache_require_replay(i))
  })))
  dontTouch(s2_dcache_require_replay)

  for (i <- 0 until LoadPipelineWidth) {
    val loadWbIndex = io.loadIn(i).bits.uop.lqIdx.value
    val lastCycleLoadWbIndex = RegNext(loadWbIndex)
    // update miss state in load s3
    if(!EnableFastForward){
      // s2_dcache_require_replay will be used to update lq flag 1 cycle after for better timing
      //
      // io.s2_dcache_require_replay comes from dcache miss req reject, which is quite slow to generate
      when(s2_dcache_require_replay(i)) {
        // do not writeback if that inst will be resend from rs
        // rob writeback will not be triggered by a refill before inst replay
        miss(lastCycleLoadWbIndex) := false.B // disable refill listening
        datavalid(lastCycleLoadWbIndex) := false.B // disable refill listening
        assert(!datavalid(lastCycleLoadWbIndex))
      }
    }
    // update load error state in load s3
    when(RegNext(io.loadIn(i).fire()) && io.s3_delayed_load_error(i)){
      uop(lastCycleLoadWbIndex).cf.exceptionVec(loadAccessFault) := true.B
    }
    // update inst replay from fetch flag in s3
    when(RegNext(io.loadIn(i).fire()) && io.s3_replay_from_fetch(i)){
      uop(lastCycleLoadWbIndex).ctrl.replayInst := true.B
    }
  }

  /**
    * Load commits
    *
    * When load commited, mark it as !allocated and move deqPtrExt forward.
    */
  (0 until CommitWidth).map(i => {
    when(commitCount > i.U){
      allocated((deqPtrExt+i.U).value) := false.B
      XSError(!allocated((deqPtrExt+i.U).value), s"why commit invalid entry $i?\n")
    }
  })

  def toVec(a: UInt): Vec[Bool] = {
    VecInit(a.asBools)
  }

  def getRemBits(input: UInt)(rem: Int): UInt = {
    VecInit((0 until LoadQueueSize / LoadPipelineWidth).map(i => { input(LoadPipelineWidth * i + rem) })).asUInt
  }

  def getFirstOne(mask: Vec[Bool], startMask: UInt) = {
    val length = mask.length
    val highBits = (0 until length).map(i => mask(i) & ~startMask(i))
    val highBitsUint = Cat(highBits.reverse)
    PriorityEncoder(Mux(highBitsUint.orR(), highBitsUint, mask.asUInt))
  }

  def getOldest[T <: XSBundleWithMicroOp](valid: Seq[Bool], bits: Seq[T]): (Seq[Bool], Seq[T]) = {
    assert(valid.length == bits.length)
    assert(isPow2(valid.length))
    if (valid.length == 1) {
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
      val left = getOldest(valid.take(valid.length / 2), bits.take(valid.length / 2))
      val right = getOldest(valid.takeRight(valid.length / 2), bits.takeRight(valid.length / 2))
      getOldest(left._1 ++ right._1, left._2 ++ right._2)
    }
  }

  def getAfterMask(valid: Seq[Bool], uop: Seq[MicroOp]) = {
    assert(valid.length == uop.length)
    val length = valid.length
    (0 until length).map(i => {
      (0 until length).map(j => {
        Mux(valid(i) && valid(j),
          isAfter(uop(i).robIdx, uop(j).robIdx),
          Mux(!valid(i), true.B, false.B))
      })
    })
  }


  /**
    * Store-Load Memory violation detection
    *
    * When store writes back, it searches LoadQueue for younger load instructions
    * with the same load physical address. They loaded wrong data and need re-execution.
    *
    * Cycle 0: Store Writeback
    *   Generate match vector for store address with rangeMask(stPtr, enqPtr).
    * Cycle 1: Redirect Generation
    *   There're up to 2 possible redirect requests.
    *   Choose the oldest load (part 1). 
    * Cycle 2: Redirect Fire
    *   Choose the oldest load (part 2).
    *   Prepare redirect request according to the detected violation.
    *   Fire redirect request (if valid)
    */

  // stage 0:        lq                 lq
  //                 |                  |  (paddr match)
  // stage 1:        lq                 lq
  //                 |                  |
  //                 |                  |
  //                 |                  |
  // stage 2:        lq                 lq
  //                 |                  |
  //                 --------------------
  //                          |
  //                      rollback req
  io.load_s1 := DontCare
def detectRollback(i: Int) = {
    val startIndex = io.storeIn(i).bits.uop.lqIdx.value
    val lqIdxMask = UIntToMask(startIndex, LoadQueueSize)
    val xorMask = lqIdxMask ^ enqMask
    val sameFlag = io.storeIn(i).bits.uop.lqIdx.flag === enqPtrExt(0).flag
    val stToEnqPtrMask = Mux(sameFlag, xorMask, ~xorMask)

    // check if load already in lq needs to be rolledback
    dataModule.io.violation(i).paddr := io.storeIn(i).bits.paddr
    dataModule.io.violation(i).mask := io.storeIn(i).bits.mask
    val addrMaskMatch = RegNext(dataModule.io.violation(i).violationMask)
    val entryNeedCheck = RegNext(VecInit((0 until LoadQueueSize).map(j => {
      allocated(j) && stToEnqPtrMask(j) && datavalid(j)
    })))
    val lqViolationVec = VecInit((0 until LoadQueueSize).map(j => {
      addrMaskMatch(j) && entryNeedCheck(j)
    }))
    val lqViolation = lqViolationVec.asUInt().orR() && RegNext(!io.storeIn(i).bits.miss)
    val lqViolationIndex = getFirstOne(lqViolationVec, RegNext(lqIdxMask))
    val lqViolationUop = uop(lqViolationIndex)
    // lqViolationUop.lqIdx.flag := deqMask(lqViolationIndex) ^ deqPtrExt.flag
    // lqViolationUop.lqIdx.value := lqViolationIndex
    XSDebug(lqViolation, p"${Binary(Cat(lqViolationVec))}, $startIndex, $lqViolationIndex\n")

    XSDebug(
      lqViolation,
      "need rollback (ld wb before store) pc %x robidx %d target %x\n",
      io.storeIn(i).bits.uop.cf.pc, io.storeIn(i).bits.uop.robIdx.asUInt, lqViolationUop.robIdx.asUInt
    )

    (lqViolation, lqViolationUop)
  }

  def rollbackSel(a: Valid[MicroOpRbExt], b: Valid[MicroOpRbExt]): ValidIO[MicroOpRbExt] = {
    Mux(
      a.valid,
      Mux(
        b.valid,
        Mux(isAfter(a.bits.uop.robIdx, b.bits.uop.robIdx), b, a), // a,b both valid, sel oldest
        a // sel a
      ),
      b // sel b
    )
  }

  // S2: select rollback (part1) and generate rollback request
  // rollback check
  // Lq rollback seq check is done in s3 (next stage), as getting rollbackLq MicroOp is slow
  val rollbackLq = Wire(Vec(StorePipelineWidth, Valid(new MicroOpRbExt)))
  // store ftq index for store set update
  val stFtqIdxS2 = Wire(Vec(StorePipelineWidth, new FtqPtr))
  val stFtqOffsetS2 = Wire(Vec(StorePipelineWidth, UInt(log2Up(PredictWidth).W)))
  for (i <- 0 until StorePipelineWidth) {
    val detectedRollback = detectRollback(i)
    rollbackLq(i).valid := detectedRollback._1 && RegNext(io.storeIn(i).valid)
    rollbackLq(i).bits.uop := detectedRollback._2
    rollbackLq(i).bits.flag := i.U
    stFtqIdxS2(i) := RegNext(io.storeIn(i).bits.uop.cf.ftqPtr)
    stFtqOffsetS2(i) := RegNext(io.storeIn(i).bits.uop.cf.ftqOffset)
  }

  val rollbackLqVReg = rollbackLq.map(x => RegNext(x.valid))
  val rollbackLqReg = rollbackLq.map(x => RegEnable(x.bits, x.valid))

  // S3: select rollback (part2), generate rollback request, then fire rollback request
  // Note that we use robIdx - 1.U to flush the load instruction itself.
  // Thus, here if last cycle's robIdx equals to this cycle's robIdx, it still triggers the redirect.

  // select uop in parallel
  val lqs = getOldest(rollbackLqVReg, rollbackLqReg)
  val rollbackUopExt = lqs._2(0) 
  val stFtqIdxS3 = RegNext(stFtqIdxS2)
  val stFtqOffsetS3 = RegNext(stFtqOffsetS2)
  val rollbackUop = rollbackUopExt.uop
  val rollbackStFtqIdx = stFtqIdxS3(rollbackUopExt.flag)
  val rollbackStFtqOffset = stFtqOffsetS3(rollbackUopExt.flag)

  // check if rollback request is still valid in parallel
  io.rollback.bits.robIdx := rollbackUop.robIdx
  io.rollback.bits.ftqIdx := rollbackUop.cf.ftqPtr
  io.rollback.bits.stFtqIdx := rollbackStFtqIdx
  io.rollback.bits.ftqOffset := rollbackUop.cf.ftqOffset
  io.rollback.bits.stFtqOffset := rollbackStFtqOffset
  io.rollback.bits.level := RedirectLevel.flush
  io.rollback.bits.interrupt := DontCare
  io.rollback.bits.cfiUpdate := DontCare
  io.rollback.bits.cfiUpdate.target := rollbackUop.cf.pc
  io.rollback.bits.debug_runahead_checkpoint_id := rollbackUop.debugInfo.runahead_checkpoint_id
  // io.rollback.bits.pc := DontCare

  io.rollback.valid := rollbackLqVReg.reduce(_|_) &&
                        (!lastCycleRedirect.valid || isBefore(rollbackUop.robIdx, lastCycleRedirect.bits.robIdx)) && 
                        (!lastlastCycleRedirect.valid || isBefore(rollbackUop.robIdx, lastlastCycleRedirect.bits.robIdx))

  when(io.rollback.valid) {
    // XSDebug("Mem rollback: pc %x robidx %d\n", io.rollback.bits.cfi, io.rollback.bits.robIdx.asUInt)
  }

  /**
  * Load-Load Memory violation detection
  *
  * When load arrives load_s1, it searches LoadQueue for younger load instructions
  * with the same load physical address. If younger load has been released (or observed),
  * the younger load needs to be re-execed.
  * 
  * For now, if re-exec it found to be needed in load_s1, we mark the older load as replayInst,
  * the two loads will be replayed if the older load becomes the head of rob.
  *
  * When dcache releases a line, mark all writebacked entrys in load queue with
  * the same line paddr as released.
  */

  // Load-Load Memory violation query
  val deqRightMask = UIntToMask.rightmask(deqPtr, LoadQueueSize)
  (0 until LoadPipelineWidth).map(i => {
    dataModule.io.release_violation(i).paddr := io.loadViolationQuery(i).req.bits.paddr
    io.loadViolationQuery(i).req.ready := true.B
    io.loadViolationQuery(i).resp.valid := RegNext(io.loadViolationQuery(i).req.fire())
    // Generate real violation mask
    // Note that we use UIntToMask.rightmask here
    val startIndex = io.loadViolationQuery(i).req.bits.uop.lqIdx.value
    val lqIdxMask = UIntToMask(startIndex, LoadQueueSize)
    val xorMask = lqIdxMask ^ enqMask
    val sameFlag = io.loadViolationQuery(i).req.bits.uop.lqIdx.flag === enqPtrExt(0).flag
    val ldToEnqPtrMask = Mux(sameFlag, xorMask, ~xorMask)
    val ldld_violation_mask_gen_1 = WireInit(VecInit((0 until LoadQueueSize).map(j => {
      ldToEnqPtrMask(j) && // the load is younger than current load
      allocated(j) && // entry is valid
      released(j) && // cacheline is released
      (datavalid(j) || miss(j)) // paddr is valid
    })))
    val ldld_violation_mask_gen_2 = WireInit(VecInit((0 until LoadQueueSize).map(j => {
      dataModule.io.release_violation(i).match_mask(j)// addr match
      // addr match result is slow to generate, we RegNext() it
    })))
    val ldld_violation_mask = RegNext(ldld_violation_mask_gen_1).asUInt & RegNext(ldld_violation_mask_gen_2).asUInt
    dontTouch(ldld_violation_mask)
    ldld_violation_mask.suggestName("ldldViolationMask_" + i)
    io.loadViolationQuery(i).resp.bits.have_violation := ldld_violation_mask.orR
  })

  // "released" flag update
  // 
  // When io.release.valid (release1cycle.valid), it uses the last ld-ld paddr cam port to
  // update release flag in 1 cycle

  when(release1cycle.valid){
    // Take over ld-ld paddr cam port
    dataModule.io.release_violation.takeRight(1)(0).paddr := release1cycle.bits.paddr
    io.loadViolationQuery.takeRight(1)(0).req.ready := false.B
  }

  when(release2cycle.valid){
    // If a load comes in that cycle, we can not judge if it has ld-ld violation
    // We replay that load inst from RS
    io.loadViolationQuery.map(i => i.req.ready :=
      // use lsu side release2cycle_dup_lsu paddr for better timing
      !i.req.bits.paddr(PAddrBits-1, DCacheLineOffset) === release2cycle_dup_lsu.bits.paddr(PAddrBits-1, DCacheLineOffset)
    )
    // io.loadViolationQuery.map(i => i.req.ready := false.B) // For better timing
  }

  (0 until LoadQueueSize).map(i => {
    when(RegNext(dataModule.io.release_violation.takeRight(1)(0).match_mask(i) && 
      allocated(i) &&
      datavalid(i) &&
      release1cycle.valid
    )){
      // Note: if a load has missed in dcache and is waiting for refill in load queue,
      // its released flag still needs to be set as true if addr matches. 
      released(i) := true.B
    }
  })

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
  //(2) when they reach ROB's head, they can be sent to uncache channel
  val lqTailMmioPending = WireInit(pending(deqPtr))
  val lqTailAllocated = WireInit(allocated(deqPtr))
  val s_idle :: s_req :: s_resp :: s_wait :: Nil = Enum(4)
  val uncacheState = RegInit(s_idle)
  switch(uncacheState) {
    is(s_idle) {
      when(RegNext(io.rob.pendingld && lqTailMmioPending && lqTailAllocated)) {
        uncacheState := s_req
      }
    }
    is(s_req) {
      when(io.uncache.req.fire()) {
        uncacheState := s_resp
      }
    }
    is(s_resp) {
      when(io.uncache.resp.fire()) {
        uncacheState := s_wait
      }
    }
    is(s_wait) {
      when(RegNext(io.rob.commit)) {
        uncacheState := s_idle // ready for next mmio
      }
    }
  }

  // used for uncache commit
  val uncacheData = RegInit(0.U(XLEN.W))
  val uncacheCommitFired = RegInit(false.B)

  when(uncacheState === s_req) {
    uncacheCommitFired := false.B
  }

  io.uncache.req.valid := uncacheState === s_req

  dataModule.io.uncache.raddr := deqPtrExtNext.value

  io.uncache.req.bits.cmd  := MemoryOpConstants.M_XRD
  io.uncache.req.bits.addr := dataModule.io.uncache.rdata.paddr
  io.uncache.req.bits.data := DontCare
  io.uncache.req.bits.mask := dataModule.io.uncache.rdata.mask
  io.uncache.req.bits.id   := RegNext(deqPtrExtNext.value)
  io.uncache.req.bits.instrtype := DontCare
  io.uncache.req.bits.replayCarry := DontCare
  io.uncache.req.bits.atomic := true.B

  io.uncache.resp.ready := true.B

  when (io.uncache.req.fire()) {
    pending(deqPtr) := false.B

    XSDebug("uncache req: pc %x addr %x data %x op %x mask %x\n",
      uop(deqPtr).cf.pc,
      io.uncache.req.bits.addr,
      io.uncache.req.bits.data,
      io.uncache.req.bits.cmd,
      io.uncache.req.bits.mask
    )
  }

  // (3) response from uncache channel: mark as datavalid
  when(io.uncache.resp.fire()){
    datavalid(deqPtr) := true.B
    uncacheData := io.uncache.resp.bits.data(XLEN-1, 0)

    XSDebug("uncache resp: data %x\n", io.refill.bits.data)
  }

  // writeback mmio load, Note: only use ldout(0) to write back
  //
  // Int load writeback will finish (if not blocked) in one cycle
  io.ldout(0).bits.uop := uop(deqPtr)
  io.ldout(0).bits.uop.lqIdx := deqPtr.asTypeOf(new LqPtr)
  io.ldout(0).bits.data := DontCare // not used
  io.ldout(0).bits.redirectValid := false.B
  io.ldout(0).bits.redirect := DontCare
  io.ldout(0).bits.debug.isMMIO := true.B
  io.ldout(0).bits.debug.isPerfCnt := false.B
  io.ldout(0).bits.debug.paddr := debug_paddr(deqPtr)
  io.ldout(0).bits.debug.vaddr := vaddrModule.io.rdata(1)
  io.ldout(0).bits.fflags := DontCare

  io.ldout(0).valid := (uncacheState === s_wait) && !uncacheCommitFired

  io.ldout(1).bits := DontCare
  io.ldout(1).valid := false.B

  // merged data, uop and offset for data sel in load_s3
  io.ldRawDataOut(0).lqData := uncacheData
  io.ldRawDataOut(0).uop := io.ldout(0).bits.uop
  io.ldRawDataOut(0).addrOffset := dataModule.io.uncache.rdata.paddr

  io.ldRawDataOut(1) := DontCare

  when(io.ldout(0).fire()){
    uncacheCommitFired := true.B
  }

  XSPerfAccumulate("uncache_load_write_back", io.ldout(0).fire())

  // Read vaddr for mem exception
  // no inst will be commited 1 cycle before tval update
  vaddrModule.io.raddr(0) := (deqPtrExt + commitCount).value
  io.exceptionAddr.vaddr := vaddrModule.io.rdata(0)

  // read vaddr for mmio, and only port {1} is used
  vaddrModule.io.raddr(1) := deqPtr

  (0 until LoadPipelineWidth).map(i => {
    if(i == 0) {
      vaddrTriggerResultModule.io.raddr(i) := deqPtr
      io.trigger(i).lqLoadAddrTriggerHitVec := Mux(
        io.ldout(i).valid,
        vaddrTriggerResultModule.io.rdata(i),
        VecInit(Seq.fill(3)(false.B))
      )
    }else {
      vaddrTriggerResultModule.io.raddr(i) := DontCare
      io.trigger(i).lqLoadAddrTriggerHitVec := VecInit(Seq.fill(3)(false.B))
    }
    // vaddrTriggerResultModule.io.raddr(i) := loadWbSelGen(i)
    // io.trigger(i).lqLoadAddrTriggerHitVec := Mux(
    //   loadWbSelV(i),
    //   vaddrTriggerResultModule.io.rdata(i),
    //   VecInit(Seq.fill(3)(false.B))
    // )
  })

  // misprediction recovery / exception redirect
  // invalidate lq term using robIdx
  val needCancel = Wire(Vec(LoadQueueSize, Bool()))
  for (i <- 0 until LoadQueueSize) {
    needCancel(i) := uop(i).robIdx.needFlush(io.brqRedirect) && allocated(i)
    when (needCancel(i)) {
      allocated(i) := false.B
    }
  }

  /**
    * update pointers
    */
  val lastEnqCancel = PopCount(RegNext(VecInit(canEnqueue.zip(enqCancel).map(x => x._1 && x._2))))
  val lastCycleCancelCount = PopCount(RegNext(needCancel))
  val enqNumber = Mux(io.enq.canAccept && io.enq.sqCanAccept, PopCount(io.enq.req.map(_.valid)), 0.U)
  when (lastCycleRedirect.valid) {
    // we recover the pointers in the next cycle after redirect
    enqPtrExt := VecInit(enqPtrExt.map(_ - (lastCycleCancelCount + lastEnqCancel)))
  }.otherwise {
    enqPtrExt := VecInit(enqPtrExt.map(_ + enqNumber))
  }

  deqPtrExtNext := deqPtrExt + commitCount
  deqPtrExt := deqPtrExtNext

  io.lqCancelCnt := RegNext(lastCycleCancelCount + lastEnqCancel)

  /**
    * misc
    */
  // perf counter
  QueuePerf(LoadQueueSize, validCount, !allowEnqueue)
  io.lqFull := !allowEnqueue
  XSPerfAccumulate("rollback", io.rollback.valid) // rollback redirect generated
  XSPerfAccumulate("mmioCycle", uncacheState =/= s_idle) // lq is busy dealing with uncache req
  XSPerfAccumulate("mmioCnt", io.uncache.req.fire())
  XSPerfAccumulate("refill", io.refill.valid)
  XSPerfAccumulate("writeback_success", PopCount(VecInit(io.ldout.map(i => i.fire()))))
  XSPerfAccumulate("writeback_blocked", PopCount(VecInit(io.ldout.map(i => i.valid && !i.ready))))
  XSPerfAccumulate("utilization_miss", PopCount((0 until LoadQueueSize).map(i => allocated(i) && miss(i))))

  if (env.EnableTopDown) {
    val stall_loads_bound = WireDefault(0.B)
    ExcitingUtils.addSink(stall_loads_bound, "stall_loads_bound", ExcitingUtils.Perf)
    val have_miss_entry = (allocated zip miss).map(x => x._1 && x._2).reduce(_ || _)
    val l1d_loads_bound = stall_loads_bound && !have_miss_entry
    ExcitingUtils.addSource(l1d_loads_bound, "l1d_loads_bound", ExcitingUtils.Perf)
    XSPerfAccumulate("l1d_loads_bound", l1d_loads_bound)
    val stall_l1d_load_miss = stall_loads_bound && have_miss_entry
    ExcitingUtils.addSource(stall_l1d_load_miss, "stall_l1d_load_miss", ExcitingUtils.Perf)
    ExcitingUtils.addSink(WireInit(0.U), "stall_l1d_load_miss", ExcitingUtils.Perf)
  }

  val perfValidCount = RegNext(validCount)

  val perfEvents = Seq(
    ("rollback         ", io.rollback.valid),
    ("mmioCycle        ", uncacheState =/= s_idle),
    ("mmio_Cnt         ", io.uncache.req.fire()),
    ("refill           ", io.refill.valid),
    ("writeback_success", PopCount(VecInit(io.ldout.map(i => i.fire())))),
    ("writeback_blocked", PopCount(VecInit(io.ldout.map(i => i.valid && !i.ready)))),
    ("ltq_1_4_valid    ", (perfValidCount < (LoadQueueSize.U/4.U))),
    ("ltq_2_4_valid    ", (perfValidCount > (LoadQueueSize.U/4.U)) & (perfValidCount <= (LoadQueueSize.U/2.U))),
    ("ltq_3_4_valid    ", (perfValidCount > (LoadQueueSize.U/2.U)) & (perfValidCount <= (LoadQueueSize.U*3.U/4.U))),
    ("ltq_4_4_valid    ", (perfValidCount > (LoadQueueSize.U*3.U/4.U)))
  )
  generatePerfEvent()

  // debug info
  XSDebug("enqPtrExt %d:%d deqPtrExt %d:%d\n", enqPtrExt(0).flag, enqPtr, deqPtrExt.flag, deqPtr)

  def PrintFlag(flag: Bool, name: String): Unit = {
    when(flag) {
      XSDebug(false, true.B, name)
    }.otherwise {
      XSDebug(false, true.B, " ")
    }
  }

  for (i <- 0 until LoadQueueSize) {
    XSDebug(i + " pc %x pa %x ", uop(i).cf.pc, debug_paddr(i))
    PrintFlag(allocated(i), "a")
    PrintFlag(allocated(i) && datavalid(i), "v")
    PrintFlag(allocated(i) && writebacked(i), "w")
    PrintFlag(allocated(i) && miss(i), "m")
    PrintFlag(allocated(i) && pending(i), "p")
    XSDebug(false, true.B, "\n")
  }

}
