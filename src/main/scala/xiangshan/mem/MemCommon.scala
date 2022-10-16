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
import xiangshan._
import utils._
import xiangshan.backend.rob.RobPtr
import xiangshan.cache._
import xiangshan.backend.fu.FenceToSbuffer

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

class LsPipelineBundle(implicit p: Parameters) extends XSBundleWithMicroOp {
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
  val rsIdx = UInt(log2Up(IssQueSize).W)

  val forwardMask = Vec(8, Bool())
  val forwardData = Vec(8, UInt(8.W))

  //softprefetch
  val isSoftPrefetch = Bool() 

  // For debug usage
  val isFirstIssue = Bool()
}

class LqWriteBundle(implicit p: Parameters) extends LsPipelineBundle {
  // queue entry data, except flag bits, will be updated if writeQueue is true,
  // valid bit in LqWriteBundle will be ignored
  val writeQueueData = Bool()

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
    rsIdx := input.rsIdx
    forwardMask := input.forwardMask
    forwardData := input.forwardData
    isSoftPrefetch := input.isSoftPrefetch
    isFirstIssue := input.isFirstIssue

    writeQueueData := false.B
  }
}

class LoadForwardQueryIO(implicit p: Parameters) extends XSBundleWithMicroOp {
  val vaddr = Output(UInt(VAddrBits.W))
  val paddr = Output(UInt(PAddrBits.W))
  val mask = Output(UInt(8.W))
  override val uop = Output(new MicroOp) // for replay
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

class LoadViolationQueryReq(implicit p: Parameters) extends XSBundleWithMicroOp { // provide lqIdx
  val paddr = UInt(PAddrBits.W)
}

class LoadViolationQueryResp(implicit p: Parameters) extends XSBundle {
  val have_violation = Bool()
}

class LoadViolationQueryIO(implicit p: Parameters) extends XSBundle {
  val req = Decoupled(new LoadViolationQueryReq)
  val resp = Flipped(Valid(new LoadViolationQueryResp))
}

// LoadFastRecoveryQueryReq used to detect store-load violation. 
// When a store instruction is executed to Store Pipe stage 1, the pipeine will 
// send a request to Load Pipe to detect store-load violation. If an violation is detected,
// load instruction need to be re-executed (flush is required in the old design, which will bring
// a lot of expense).
class LoadFastRecoveryQueryReq(implicit p: Parameters) extends XSBundle {
  //  Requestor's (a store instruction) rob index for match logic.
  val robIdx = new RobPtr

  //  Requestor's (a store instruction) sq index for match logic.
  val sqIdx = new SqPtr 

  //  Requestor's (a store instruction) physical address for match logic.
  val paddr = UInt(PAddrBits.W)

  //  Requestor's (a store instruction) data width mask for match logic.
  val mask = UInt(8.W)
}

// Bundle for load / store wait waking up
class MemWaitUpdateReq(implicit p: Parameters) extends XSBundle {
  val staIssue = Vec(exuParameters.StuCnt, ValidIO(new ExuInput))
  val stdIssue = Vec(exuParameters.StuCnt, ValidIO(new ExuInput))
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
