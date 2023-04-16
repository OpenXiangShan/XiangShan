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
import utility._
import utils._
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.cache._
import xiangshan.cache.dcache.ReplayCarry
import xiangshan.backend.Bundles.{DynInst, MemExuInput}

object genWmask {
  def apply(addr: UInt, sizeEncode: UInt): UInt = {
    (LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(2:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U, //1111
      "b11".U -> 0xff.U //11111111
    )) << addr(2, 0)).asUInt()
  }
}

object genWdata {
  def apply(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(8, data(7, 0)),
      "b01".U -> Fill(4, data(15, 0)),
      "b10".U -> Fill(2, data(31, 0)),
      "b11".U -> data
    ))
  }
}

class LsPipelineBundle(implicit val p: Parameters) extends Bundle with HasXSParameter with HasDCacheParameters{
  val uop = new DynInst
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
  // val func = UInt(6.W)
  val mask = UInt(8.W)
  val data = UInt((XLEN+1).W)
  val wlineflag = Bool() // store write the whole cache line

  val miss = Bool()
  val tlbMiss = Bool()
  val ptwBack = Bool()
  val mmio = Bool()
  val atomic = Bool()
  val rsIdx = UInt(log2Up(MemIQSizeMax).W)

  val forwardMask = Vec(8, Bool())
  val forwardData = Vec(8, UInt(8.W))

  //softprefetch
  val isSoftPrefetch = Bool() 

  // For debug usage
  val isFirstIssue = Bool()

  // For load replay
  val isLoadReplay = Bool()
  val replayCarry = new ReplayCarry

  // For dcache miss load
  val mshrid = UInt(log2Up(cfg.nMissEntries).W)

  val forward_tlDchannel = Bool()
}

class LqWriteBundle(implicit p: Parameters) extends LsPipelineBundle {
  // queue entry data, except flag bits, will be updated if writeQueue is true,
  // valid bit in LqWriteBundle will be ignored
  val lq_data_wen_dup = Vec(6, Bool()) // dirty reg dup

  def fromLsPipelineBundle(input: LsPipelineBundle) = {
    vaddr := input.vaddr
    paddr := input.paddr
    mask := input.mask
    data := input.data
    uop := input.uop
    wlineflag := input.wlineflag
    miss := input.miss
    tlbMiss := input.tlbMiss
    ptwBack := input.ptwBack
    mmio := input.mmio
    atomic := input.atomic
    rsIdx := input.rsIdx
    forwardMask := input.forwardMask
    forwardData := input.forwardData
    isSoftPrefetch := input.isSoftPrefetch
    isFirstIssue := input.isFirstIssue
    isLoadReplay := input.isLoadReplay
    mshrid := input.mshrid
    forward_tlDchannel := input.forward_tlDchannel
    replayCarry := input.replayCarry

    lq_data_wen_dup := DontCare
  }
}

class LoadForwardQueryIO(implicit p: Parameters) extends XSBundle {
  val uop = Output(new DynInst)
  val vaddr = Output(UInt(VAddrBits.W))
  val paddr = Output(UInt(PAddrBits.W))
  val mask = Output(UInt(8.W))
  val pc = Output(UInt(VAddrBits.W)) //for debug
  val valid = Output(Bool())

  val forwardMaskFast = Input(Vec(8, Bool())) // resp to load_s1
  val forwardMask = Input(Vec(8, Bool())) // resp to load_s2
  val forwardData = Input(Vec(8, UInt(8.W))) // resp to load_s2

  // val lqIdx = Output(UInt(LoadQueueIdxWidth.W))
  val sqIdx = Output(new SqPtr)

  // dataInvalid suggests store to load forward found forward should happen,
  // but data is not available for now. If dataInvalid, load inst should
  // be replayed from RS. Feedback type should be RSFeedbackType.dataInvalid
  val dataInvalid = Input(Bool()) // Addr match, but data is not valid for now

  // matchInvalid suggests in store to load forward logic, paddr cam result does
  // to equal to vaddr cam result. If matchInvalid, a microarchitectural exception
  // should be raised to flush SQ and committed sbuffer.
  val matchInvalid = Input(Bool()) // resp to load_s2
}

// LoadForwardQueryIO used in load pipeline
//
// Difference between PipeLoadForwardQueryIO and LoadForwardQueryIO:
// PipeIO use predecoded sqIdxMask for better forward timing
class PipeLoadForwardQueryIO(implicit p: Parameters) extends LoadForwardQueryIO {
  // val sqIdx = Output(new SqPtr) // for debug, should not be used in pipeline for timing reasons
  // sqIdxMask is calcuated in earlier stage for better timing
  val sqIdxMask = Output(UInt(StoreQueueSize.W))

  // dataInvalid: addr match, but data is not valid for now
  val dataInvalidFast = Input(Bool()) // resp to load_s1
  // val dataInvalid = Input(Bool()) // resp to load_s2
  val dataInvalidSqIdx = Input(UInt(log2Up(StoreQueueSize).W)) // resp to load_s2, sqIdx value
}

// Query load queue for ld-ld violation
// 
// Req should be send in load_s1
// Resp will be generated 1 cycle later
//
// Note that query req may be !ready, as dcache is releasing a block
// If it happens, a replay from rs is needed.

class LoadViolationQueryReq(implicit p: Parameters) extends XSBundle { // provide lqIdx
  val uop = new DynInst
  val paddr = UInt(PAddrBits.W)
}

class LoadViolationQueryResp(implicit p: Parameters) extends XSBundle {
  val have_violation = Bool()
}

class LoadViolationQueryIO(implicit p: Parameters) extends XSBundle {
  val req = Decoupled(new LoadViolationQueryReq)
  val resp = Flipped(Valid(new LoadViolationQueryResp))
}

class LoadReExecuteQueryIO(implicit p: Parameters) extends XSBundle {
  //  robIdx: Requestor's (a store instruction) rob index for match logic. 
  val robIdx = new RobPtr

  //  paddr: requestor's (a store instruction) physical address for match logic. 
  val paddr = UInt(PAddrBits.W)

  //  mask: requestor's (a store instruction) data width mask for match logic.
  val mask = UInt(8.W)  
}

// Store byte valid mask write bundle
//
// Store byte valid mask write to SQ takes 2 cycles
class StoreMaskBundle(implicit p: Parameters) extends XSBundle {
  val sqIdx = new SqPtr
  val mask = UInt(8.W)
}

class LoadDataFromDcacheBundle(implicit p: Parameters) extends DCacheBundle {
  // old dcache: optimize data sram read fanout
  // val bankedDcacheData = Vec(DCacheBanks, UInt(64.W))
  // val bank_oh = UInt(DCacheBanks.W)  
  
  // new dcache
  val respDcacheData = UInt(XLEN.W)
  val forwardMask = Vec(8, Bool())
  val forwardData = Vec(8, UInt(8.W))
  val uop = new DynInst // for data selection, only fwen and fuOpType are used
  val addrOffset = UInt(3.W) // for data selection
  
  // forward tilelink D channel
  val forward_D = Input(Bool())
  val forwardData_D = Input(Vec(8, UInt(8.W)))

  // forward mshr data
  val forward_mshr = Input(Bool())
  val forwardData_mshr = Input(Vec(8, UInt(8.W)))

  val forward_result_valid = Input(Bool())
  
  def dcacheData(): UInt = {
    // old dcache
    // val dcache_data = Mux1H(bank_oh, bankedDcacheData)
    // new dcache
    val dcache_data = respDcacheData
    val use_D = forward_D && forward_result_valid
    val use_mshr = forward_mshr && forward_result_valid
    Mux(use_D, forwardData_D.asUInt, Mux(use_mshr, forwardData_mshr.asUInt, dcache_data))
  }

  def mergedData(): UInt = {
    val rdataVec = VecInit((0 until XLEN / 8).map(j =>
      Mux(forwardMask(j), forwardData(j), dcacheData()(8*(j+1)-1, 8*j))
    ))
    rdataVec.asUInt
  }
}

// Load writeback data from load queue (refill)
class LoadDataFromLQBundle(implicit p: Parameters) extends XSBundle {
  val lqData = UInt(64.W) // load queue has merged data
  val uop = new DynInst // for data selection, only fwen and fuOpType are used
  val addrOffset = UInt(3.W) // for data selection

  def mergedData(): UInt = {
    lqData
  }
}

// Bundle for load / store wait waking up
class MemWaitUpdateReq(implicit p: Parameters) extends XSBundle {
  val staIssue = Vec(backendParams.StaCnt, ValidIO(new MemExuInput))
  val stdIssue = Vec(backendParams.StdCnt, ValidIO(new MemExuInput))
}

object AddPipelineReg {
  class PipelineRegModule[T <: Data](gen: T) extends Module {
    val io = IO(new Bundle() {
      val in = Flipped(DecoupledIO(gen.cloneType))
      val out = DecoupledIO(gen.cloneType)
      val isFlush = Input(Bool())
    })

    val valid = RegInit(false.B)
    valid.suggestName("pipeline_reg_valid")
    when (io.out.fire()) { valid := false.B }
    when (io.in.fire()) { valid := true.B }
    when (io.isFlush) { valid := false.B }

    io.in.ready := !valid || io.out.ready
    io.out.bits := RegEnable(io.in.bits, io.in.fire())
    io.out.valid := valid //&& !isFlush
  }

  def apply[T <: Data]
  (left: DecoupledIO[T], right: DecoupledIO[T], isFlush: Bool,
   moduleName: Option[String] = None
  ){
    val pipelineReg = Module(new PipelineRegModule[T](left.bits.cloneType))
    if(moduleName.nonEmpty) pipelineReg.suggestName(moduleName.get)
    pipelineReg.io.in <> left
    right <> pipelineReg.io.out
    pipelineReg.io.isFlush := isFlush
  }
}
