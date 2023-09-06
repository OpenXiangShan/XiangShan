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
import chisel3.{util, _}
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.rob.RobPtr

class VluopPtr(implicit p: Parameters) extends CircularQueuePtr[VluopPtr](
  p => p(XSCoreParamsKey).VlUopSize
){
}

object VluopPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): VluopPtr = {
    val ptr = Wire(new VluopPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

object VecGenMask {
  def apply(rob_idx_valid: Vec[Bool], reg_offset: Vec[UInt], offset: Vec[UInt], mask: Vec[UInt]):Vec[UInt] = {
    val vMask = VecInit(Seq.fill(2)(0.U(16.W)))
    for (i <- 0 until 2){
      when (rob_idx_valid(i)) {
        when (offset(i) <= reg_offset(i)) {
          vMask(i) := mask(i) << (reg_offset(i) - offset(i))
        }.otherwise {
          vMask(i) := mask(i) >> (offset(i) - reg_offset(i))
        }
      }
    }
    vMask
  }
}

object VecGenData {
  def apply (rob_idx_valid: Vec[Bool], reg_offset: Vec[UInt], offset: Vec[UInt], data:UInt):Vec[UInt] = {
    val vData = VecInit(Seq.fill(2)(0.U(128.W)))
    for (i <- 0 until 2){
      when (rob_idx_valid(i)) {
        when (offset(i) <= reg_offset(i)) {
          vData(i) := data << ((reg_offset(i) - offset(i)) << 3.U)
        }.otherwise {
          vData(i) := data >> ((offset(i) - reg_offset(i)) << 3.U)
        }
      }
    }
    vData
  }
}

class VluopBundle(implicit p: Parameters) extends VLSUBundle {
  val uop            = new MicroOp
  val dataVMask      = Vec(VLENB, Bool())
  val data           = Vec(VLENB, UInt(8.W))
  val fof            = Bool()
  val excp_eew_index = UInt(elemIdxBits.W)
  // val exceptionVec   = ExceptionVec() // uop has exceptionVec
  val baseAddr = UInt(VAddrBits.W)
  val stride = UInt(VLEN.W)
  val flow_counter = UInt(flowIdxBits.W)
  val vd_last_uop = Bool()

  // def apply (uop: MicroOp, fof: Bool) = {
  //   this.uop  := uop
  //   this.fof  := fof
  //   this
  // }

  // instruction decode result
  val flowNum = UInt(flowIdxBits.W) // # of flows in a uop
  // val flowNumLog2 = UInt(log2Up(flowIdxBits).W) // log2(flowNum), for better timing of multiplication
  val nfields = UInt(fieldBits.W) // NFIELDS
  val vm = Bool() // whether vector masking is enabled
  val usWholeReg = Bool() // unit-stride, whole register load
  val eew = UInt(ewBits.W) // size of memory elements
  val sew = UInt(ewBits.W)
  val emul = UInt(mulBits.W)
  val lmul = UInt(mulBits.W)
  val vlmax = UInt(elemIdxBits.W)
  val instType = UInt(3.W)
}

// class VlUopQueueIOBundle(implicit p: Parameters) extends XSBundle {
//   val loadRegIn   = Vec(VecLoadPipelineWidth, Flipped(DecoupledIO(new ExuInput(isVpu = true))))
//   val redirect    = Flipped(ValidIO(new Redirect))
//   val instType    = Vec(VecLoadPipelineWidth, Input(UInt(3.W)))
//   val fof         = Vec(VecLoadPipelineWidth, Input(Bool()))
//   val whole_reg   = Vec(VecLoadPipelineWidth, Input(Bool()))
//   val emul        = Vec(VecLoadPipelineWidth, Input(UInt(3.W)))
//   val realFlowNum = Vec(VecLoadPipelineWidth, Input(UInt(5.W)))
//   val loadPipeIn  = Vec(VecLoadPipelineWidth, Flipped(DecoupledIO(new VecExuOutput)))
//   val uopVecFeedback = Vec(VecLoadPipelineWidth,ValidIO(Bool()))
//   val vecLoadWriteback = Vec(VecLoadPipelineWidth,DecoupledIO(new ExuOutput(isVpu = true)))
// }

class VlUopQueueIOBundle(implicit p: Parameters) extends VLSUBundle {
  // redirect
  val redirect = Flipped(ValidIO(new Redirect))
  // input from load rs along with regfile src data
  val loadRegIn = Flipped(DecoupledIO(new ExuInput(isVpu = true)))
  // issue 2 flows from uop queue each cycle
  val flowIssue = Vec(VecLoadPipelineWidth, DecoupledIO(new VlflowBundle()))
  // writeback 2 flow results orderly from flow queue each cycle
  val flowWriteback = Vec(VecLoadPipelineWidth, Flipped(DecoupledIO(new VecExuOutput())))
  // feedbacks to signal that uop queue is full
  // val uopFeedback = Output(Vec(VecLoadPipelineWidth, Bool()))
  // writeback uop results
  val uopWriteback = DecoupledIO(new ExuOutput(isVpu = true))
}

/**
  * VlUopQueue(vector load uop queue) receives uops from vector load rs, split uops into flows
  * and send them to load flow queue.
  * 
  * When the head uop of uop queue receives all the component flows, uop queue dequeues the head.
  * Also, uop queue is responsible of jointing the results of all the uops with the same robIdx
  * that are about to write the same vd.
  *
  * @param p
  */
class VlUopQueue(implicit p: Parameters) extends VLSUModule
{
  val io = IO(new VlUopQueueIOBundle())

  println("LoadUopQueue: size:" + VlUopSize)
  val flowIssueWidth = io.flowIssue.length

  /**
    * TODO @zlj
    */
  io.flowWriteback <> DontCare
  io.uopWriteback <> DontCare

  val uopq = Reg(Vec(VlUopSize, new VluopBundle))
  val valid = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  val preAlloc = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))

  val enqPtrExt = RegInit(VecInit((0 until maxMUL).map(_.U.asTypeOf(new VluopPtr))))
  val enqPtr = enqPtrExt(0)
  val deqPtr = RegInit(0.U.asTypeOf(new VluopPtr))
  val flowSplitPtr = RegInit(0.U.asTypeOf(new VluopPtr))
  val flowSplitIdx = RegInit(VecInit((0 until flowIssueWidth).map(_.U(flowIdxBits.W))))
  val vdResult = Reg(UInt(VLEN.W)) // joint all the uops' result that is written into the same vd
  val vdResultValid = RegInit(false.B)

  val full = isFull(enqPtr, deqPtr)

  /**
    * Enqueue from issue queue:
    * 
    * (1) Decode the instruction and obtain nf, eew, etc.
    * (2) Pre-allocate all the incoming uops with the same robIdx that will write into the same vd.
    * 
    * TODO: decode logic is too long for timing.
    */
  val decode = Wire(new VecDecode())
  decode.apply(io.loadRegIn.bits.uop.cf.instr)
  val sew = io.loadRegIn.bits.uop.ctrl.vconfig.vtype.vsew
  val eew = decode.uop_eew
  val lmul = io.loadRegIn.bits.uop.ctrl.vconfig.vtype.vlmul
  val emul = EewLog2(eew) - sew + lmul
  val lmulLog2 = Mux(lmul.asSInt >= 0.S, 0.U, lmul)
  val emulLog2 = Mux(emul.asSInt >= 0.S, 0.U, emul)
  val numEewLog2 = emulLog2 - EewLog2(eew)
  val numSewLog2 = lmulLog2 - sew
  val numUopsSameVd = Mux(
    decode.isIndexed && numSewLog2 > numEewLog2,
    // If this is an index load, and multiple index regs are mapped into a data reg:
    1.U << (numSewLog2 - numEewLog2),
    // otherwise:
    1.U
  )

  when (io.loadRegIn.fire()) {
    val id = enqPtr.value
    val preAllocated = preAlloc(id)
    val isSegment = decode.uop_segment_num =/= 0.U && !decode.uop_unit_stride_whole_reg
    val instType = Cat(isSegment, decode.uop_type)
    val flows = GenRealFlowNum(instType, emul, lmul, eew, sew)
    valid(id) := true.B
    uopq(id).uop := io.loadRegIn.bits.uop
    uopq(id).fof := decode.isUnitStride && decode.uop_unit_stride_fof
    uopq(id).baseAddr := io.loadRegIn.bits.src_rs1
    uopq(id).stride := io.loadRegIn.bits.src_stride
    uopq(id).flow_counter := flows
    uopq(id).flowNum := flows
    // uopq(id).flowNumLog2 := GenRealFlowLog2(instType, emul, lmul, eew, sew)
    uopq(id).nfields := decode.uop_segment_num + 1.U
    uopq(id).vm := decode.mask_en
    uopq(id).usWholeReg := decode.isUnitStride && decode.uop_unit_stride_whole_reg
    uopq(id).eew := eew
    uopq(id).sew := sew
    uopq(id).emul := emul
    uopq(id).lmul := lmul
    uopq(id).vlmax := GenVLMAX(lmul, sew)
    uopq(id).instType := instType

    // Assertion
    assert(!uopq(id).dataVMask.asUInt.orR, "mask should be cleared when a uop entry deallocated")
    assert(PopCount(flows) <= 1.U, "flowNum should be the power of 2")

    // Pre-allocate
    when (!preAllocated) {
      // This is the first uop mapped into the same vd
      enqPtrExt.zipWithIndex.foreach { case (ptr, i) =>
        when (i.U < numUopsSameVd) {
          assert(!preAlloc(ptr.value))
          assert(!uopq(ptr.value).vd_last_uop)

          preAlloc(ptr.value) := true.B
          uopq(ptr.value).vd_last_uop := (i + 1).U === numUopsSameVd
        }
      }
    }.otherwise {
      // Otherwise, this uop has already been pre-allocated
      assert(io.loadRegIn.bits.uop.robIdx === uopq(id).uop.robIdx)
      assert(io.loadRegIn.bits.src_rs1(VAddrBits - 1, 0) === uopq(id).baseAddr)
    }

    // update enqPtrExt
    enqPtrExt.foreach(ptr => ptr := ptr + 1.U)
  }

  /**
    * Split uop into flows
    */
  // Common info of all the flows included in a uop
  val issueValid = valid(flowSplitPtr.value)
  val issueEntry = uopq(flowSplitPtr.value)
  val issueFlowNum = issueEntry.flowNum
  // val issueFlowNumLog2 = issueEntry.flowNumLog2
  val issueBaseAddr = issueEntry.baseAddr
  val issueUop = issueEntry.uop
  val issueUopIdx = issueUop.ctrl.uopIdx(uopIdxBits - 1, 0)
  val issueInstType = issueEntry.instType
  val issueEew = issueEntry.eew
  val issueSew = issueEntry.sew
  val issueAlignedType = Mux(isIndexed(issueInstType), issueSew(1, 0), issueEew(1, 0))
  val issueVLMAXMask = issueEntry.vlmax - 1.U
  val issueVLMAXLog2 = GenVLMAXLog2(issueEntry.lmul, issueEntry.sew)
  val issueNFIELDS = issueEntry.nfields
  val issueVstart = issueUop.ctrl.vconfig.vstart
  val issueVl = issueUop.ctrl.vconfig.vl
  assert(!issueValid || PopCount(issueEntry.vlmax) === 1.U, "VLMAX should be power of 2 and non-zero")

  flowSplitIdx.zip(io.flowIssue).foreach { case (flowIdx, issuePort) =>
    // AGU
    // TODO: DONT use * to implement multiplication!!!
    val elemIdx = GenElemIdx(issueAlignedType, issueUopIdx, flowIdx) // elemIdx inside an inst
    val elemIdxInsideField = elemIdx & issueVLMAXMask // elemIdx inside a field, equals elemIdx when nf = 1
    val nfIdx = elemIdx >> issueVLMAXLog2
    val notIndexedStride = Mux( // stride for strided/unit-stride instruction
      isStrided(issueInstType),
      issueEntry.stride(XLEN - 1, 0), // for strided load, stride = x[rs2]
      issueNFIELDS << issueEew(1, 0) // for unit-stride load, stride = eew * NFIELDS
    ) * elemIdxInsideField
    val indexedStride = IndexAddr( // index for indexed instruction
      index = issueEntry.stride,
      flow_inner_idx = (elemIdxInsideField << issueEew(1, 0))(vOffsetBits - 1, 0) >> issueEew(1, 0),
      eew = issueEew
    )
    val stride = Mux(isIndexed(issueInstType), indexedStride, notIndexedStride)
    val fieldOffset = nfIdx << issueAlignedType // field offset inside a segment
    val vaddr = issueBaseAddr + stride + fieldOffset
    val mask = GenVecLoadMask(issueAlignedType, vaddr)
    val regOffset = (elemIdxInsideField << issueAlignedType)(vOffsetBits - 1, 0)
    val exp = VLExpCtrl(
      vstart = issueVstart,
      vl = Mux(issueEntry.usWholeReg, GenUSWholeRegVL(issueNFIELDS, issueEew), issueVl),
      eleIdx = elemIdxInsideField
    )

    when (issueValid && flowIdx < issueFlowNum) {
      issuePort.valid := true.B
    }
    
    val port = issuePort.bits
    port.uop := issueUop
    port.vaddr := vaddr
    port.mask := mask
    port.unit_stride_fof := issueEntry.fof
    port.reg_offset := regOffset
    port.alignedType := issueAlignedType
    port.exp := exp
    port.flow_idx := elemIdx
    port.is_first_ele := elemIdx === 0.U
  }

  val numFlowIssue = PopCount(io.flowIssue.map(_.fire()))
  val flowIssueFire = Cat(io.flowIssue.map(_.fire())).orR

  when (flowSplitIdx.last < (issueFlowNum - 1.U)) {
    // The uop has not been entirly splited yet
    flowSplitIdx.foreach(p => p := p + numFlowIssue)
    assert(!flowIssueFire || numFlowIssue === flowIssueWidth.U, "both issue port should fire together")
  }.otherwise {
    when (flowIssueFire) {
      // The uop is done spliting
      flowSplitIdx := VecInit((0 until flowIssueWidth).map(_.U)) // initialize flowIdx
      flowSplitPtr := flowSplitPtr + 1.U
    }
  }

  
  /**
    * IO connections
    */
  io.loadRegIn.ready := !full && preAlloc(enqPtr.value) || distanceBetween(enqPtr, deqPtr) >= numUopsSameVd

  assert(!(issueValid && !io.flowIssue(0).valid && io.flowIssue(1).valid), "flow issue port 0 should have higher priority")

  /**
    * Miscs
    */
  def isUnitStride(instType: UInt) = instType(1, 0) === "b00".U
  def isStrided(instType: UInt) = instType(1, 0) === "b10".U
  def isIndexed(instType: UInt) = instType(0) === "b1".U
  def isNotIndexed(instType: UInt) = instType(0) === "b0".U


//   val VluopEntry = Reg(Vec(VlUopSize, new VluopBundle))
//   // For example, an inst -> 4 uops,
//   // When first uop comes, 4 entries are all valid and pre_allocated
//   val valid = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
//   val pre_allocated = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
//   // When an uop really comes, an entry will be allocated
//   val allocated = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
//   // When both data and pdest are readym, this entry is finished
//   val finished = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
//   val counter = RegInit(VecInit(Seq.fill(VlUopSize)(0.U(4.W))))

//   val realFlowNum    = Wire(Vec(VecLoadPipelineWidth, UInt(5.W)))
//   val vend           = Wire(Vec(VecLoadPipelineWidth, UInt(5.W)))
//   val already_in     = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(false.B)))
//   val already_in_vec = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(VlUopSize)(false.B)))))
//   val enq_valid      = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(false.B)))
//   val instType       = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
//   val mul            = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
//   val loadRegInValid = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(false.B)))
//   val needFlush      = WireInit(VecInit(Seq.fill(VlUopSize)(false.B)))
//   val uopNum         = Wire(Vec(VecLoadPipelineWidth, UInt(4.W)))
//   val free           = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U(VlUopSize.W))))

//   //First-level buffer
//   val buffer_valid_s0    = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(false.B)))
//   val data_buffer_s0     = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U(VLEN.W))))
//   val mask_buffer_s0     = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U((VLEN/8).W))))))
//   val rob_idx_valid_s0   = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(false.B)))))
//   val inner_idx_s0       = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U(3.W))))))
//   val rob_idx_s0         = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U.asTypeOf(new RobPtr))))))
//   val reg_offset_s0      = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U(4.W))))))
//   val offset_s0          = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U(4.W))))))
//   val uop_s0             = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U.asTypeOf(new MicroOp))))
//   val excp_s0            = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(false.B)))
//   val is_first_ele_s0    = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(false.B)))
//   val excep_ele_index_s0 = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U(8.W))))
//   val exceptionVec_s0    = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U.asTypeOf(ExceptionVec()))))
//   //Second-level buffer
//   //val buffer_valid_s1  = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(false.B)))))
//   //val data_buffer_s1   = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U(VLEN.W))))))
//   //val mask_buffer_s1   = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U((VLEN/8).W))))))
//   //val rob_idx_valid_s1 = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(false.B)))))
//   //val inner_idx_s1     = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U(3.W))))))
//   //val rob_idx_s1       = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(2)(0.U.asTypeOf(new RobPtr))))))
//   //val uop_s1           = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U.asTypeOf(new MicroOp))))

//   val vlUopFreeList = Module(new VlUopFreeList(size=VlUopSize,
//                                  allocWidth = VecLoadPipelineWidth,
//                                  maxIdxNum = 8,
//                                  freeWidth = 4,
//                                  moduleName = "vlUopFreeList"))

//   def getRemBits(input: UInt)(rem: Int): UInt = {
//     VecInit((0 until VlUopSize / VecLoadPipelineWidth).map(i => {
//       input(VecLoadPipelineWidth * i + rem)
//     })).asUInt
//   }

//   for (i <- 0 until VecLoadPipelineWidth) {
//     io.loadRegIn(i).ready  := vlUopFreeList.io.accllReq(i).ready
//     io.loadPipeIn(i).ready := true.B
//   }
// /**
//   *Check whether the unit-stride uop has enqueued*/
//   for (i <- 0 until VecLoadPipelineWidth) {
//     for (entry <- 0 until VlUopSize) {
//       already_in_vec(i)(entry) := VluopEntry(entry).uop.robIdx.value === io.loadRegIn(i).bits.uop.robIdx.value &&
//                                   VluopEntry(entry).uop.ctrl.uopIdx === io.loadRegIn(i).bits.uop.ctrl.uopIdx &&
//                                   pre_allocated(entry)
//       val debug_hit = WireInit(VecInit(Seq.fill(VlUopSize)(false.B))) // for debug
//       when (already_in_vec(i)(entry) && io.loadRegIn(i).valid) {
//         VluopEntry(entry).apply(uop = io.loadRegIn(i).bits.uop, fof = io.fof(i))
//         allocated(entry) := true.B
//         debug_hit(entry) := true.B
//       }
//       assert(PopCount(debug_hit) <= 1.U, "VlUopQueue Multi-Hit!")
//     }
//   }

//   for (i <- 0 until VecLoadPipelineWidth) {
//     vend(i) := DontCare
//     already_in(i) := already_in_vec(i).asUInt.orR
//     instType(i) := io.instType(i)
//     loadRegInValid(i) := !io.loadRegIn(i).bits.uop.robIdx.needFlush(io.redirect) && io.loadRegIn(i).fire
//     enq_valid(i) := !already_in(i) && loadRegInValid(i)
//     mul(i) := Mux(instType(i)(1,0) === "b00".U || instType(i)(1,0) === "b10".U,io.emul(i),io.loadRegIn(i).bits.uop.ctrl.vconfig.vtype.vlmul(i))
//     when (instType(0) === "b000".U) {
//       vend(i) := io.loadRegIn(i).bits.src(0)(3,0) + MulDataSize(mul=io.emul(i))
//       realFlowNum(i) := vend(i)(4) +& (vend(i)(3,0) =/= 0.U).asUInt
//       uopNum(i) := io.loadRegIn(i).bits.uop.ctrl.total_num //TODO: if the inst has 4 uop, total_num = 4
//     }.otherwise {
//       realFlowNum(i) := io.realFlowNum(i)
//       uopNum(i) := 1.U
//     }
//   }

//   /**
//     * Only unit-stride instructions use vecFeedback
//     */
//   for (i <- 0 until VecLoadPipelineWidth) {
//     io.uopVecFeedback(i).valid := io.loadRegIn(i).valid
//     io.uopVecFeedback(i).bits := already_in(i)
//   }

//   //uop enqueue
//   dontTouch(io.loadRegIn)
//   for (i <- 0 until VecLoadPipelineWidth) {
//     vlUopFreeList.io.accllReq(i) := DontCare
//     when (enq_valid(i)) {
//       vlUopFreeList.io.accllReq(i).valid := true.B
//       vlUopFreeList.io.accllReq(i).bits := uopNum(i)
//       for (j <- 0 until 8) {
//         when (j.U < uopNum(i)) {
//           val enqPtr = vlUopFreeList.io.idxValue(i)(j)
//           val inUop = WireInit(io.loadRegIn(i).bits.uop)
//           //val isPer = !(i.U === io.loadRegIn(0).bits.uop.ctrl.uopIdx || i.U === io.loadRegIn(1).bits.uop.ctrl.uopIdx)
//           inUop.ctrl.uopIdx := Mux(instType(i) === "b000".U, j.U, io.loadRegIn(i).bits.uop.ctrl.uopIdx) //TODO: If flow don't write loadQueue, ldIdx needn't calculate
//           VluopEntry(enqPtr).apply(uop = inUop, fof = io.fof(i))
//           valid(enqPtr) := true.B
//           pre_allocated(enqPtr) := true.B
//           counter(enqPtr) := realFlowNum(i)
//           when ( instType(i) === "b000".U && j.U === io.loadRegIn(i).bits.uop.ctrl.uopIdx || instType(i) =/= "b000".U) {
//             allocated(enqPtr) := true.B
//           }
//         }
//       }
//     }
//   }

//   // write data from loadpipe to first_level buffer
//   for (i <- 0 until VecLoadPipelineWidth) {
//     when (io.loadPipeIn(i).fire) {
//       buffer_valid_s0(i)    := true.B
//       data_buffer_s0(i)     := io.loadPipeIn(i).bits.vec.vecdata
//       rob_idx_valid_s0(i)   := io.loadPipeIn(i).bits.vec.rob_idx_valid
//       rob_idx_s0(i)         := io.loadPipeIn(i).bits.vec.rob_idx
//       inner_idx_s0(i)       := io.loadPipeIn(i).bits.vec.inner_idx
//       reg_offset_s0(i)      := io.loadPipeIn(i).bits.vec.reg_offset
//       offset_s0(i)          := io.loadPipeIn(i).bits.vec.offset
//       uop_s0(i)             := io.loadPipeIn(i).bits.uop
//       excp_s0(i)            := io.loadPipeIn(i).bits.vec.exp
//       is_first_ele_s0(i)    := io.loadPipeIn(i).bits.vec.is_first_ele
//       excep_ele_index_s0(i) := io.loadPipeIn(i).bits.vec.exp_ele_index
//       exceptionVec_s0(i)     := io.loadPipeIn(i).bits.uop.cf.exceptionVec
//       for (j <- 0 until 2) {
//         mask_buffer_s0(i)(j) := io.loadPipeIn(i).bits.vec.mask << io.loadPipeIn(i).bits.vec.offset(j)
//       }
//     }.otherwise {
//       buffer_valid_s0(i)  := false.B
//       rob_idx_valid_s0(i) := VecInit(Seq.fill(2)(false.B))
//     }
//   }
//   // write data from first_level buffer to second_level buffer
//   ///for (i <- 0 until VecLoadPipelineWidth) {
//   ///  when (buffer_valid_s0(i) === true.B) {
//   ///    buffer_valid_s1(i)  := VecInit(Seq.fill(2)(true.B))
//   ///    mask_buffer_s1(i)   := VecGenMask(rob_idx_valid = rob_idx_valid_s0(i), reg_offset = reg_offset_s0(i), offset = offset_s0(i), mask = mask_buffer_s0(i))
//   ///    data_buffer_s1(i)   := VecGenData(rob_idx_valid = rob_idx_valid_s0(i), reg_offset = reg_offset_s0(i), offset = offset_s0(i), data = data_buffer_s0(i))
//   ///    rob_idx_valid_s1(i) := rob_idx_valid_s0(i)
//   ///    inner_idx_s1(i)     := inner_idx_s0(i)
//   ///    rob_idx_s1(i)       := rob_idx_s0(i)
//   ///    uop_s1(i)           := uop_s0(i)
//   ///  }.otherwise {
//   ///    buffer_valid_s1(i)  := VecInit(Seq.fill(2)(false.B))
//   ///    rob_idx_valid_s1(i) := VecInit(Seq.fill(2)(false.B))
//   ///  }
//   ///}
// 0.U.asTypeOf(ExceptionVec())
//   //write data from first_level buffer to VluopEntry
//   for (i <- 0 until VecLoadPipelineWidth) {
//     val mask_buffer = VecGenMask(rob_idx_valid = rob_idx_valid_s0(i),
//                                  reg_offset = reg_offset_s0(i),
//                                  offset = offset_s0(i),
//                                  mask = mask_buffer_s0(i))
//     val data_buffer = VecGenData(rob_idx_valid = rob_idx_valid_s0(i),
//                                  reg_offset = reg_offset_s0(i),
//                                  offset = offset_s0(i),
//                                  data = data_buffer_s0(i))
//     for (j <- 0 until 2) {
//       when(buffer_valid_s0(i) && rob_idx_valid_s0(i)(j)) {
//         for (entry <- 0 until VlUopSize) {
//           when(rob_idx_s0(i)(j).value === VluopEntry(entry).uop.robIdx.value &&
//             inner_idx_s0(i)(j) === VluopEntry(entry).uop.ctrl.uopIdx) {
//             counter(entry) := counter(entry) - 1.U
//             for (k <- 0 until VLEN / 8) {
//               when(mask_buffer(j)(k)) {
//                 VluopEntry(entry).data(k) := data_buffer(j)(k * 8 + 7, k * 8)
//                 VluopEntry(entry).dataVMask(k) := mask_buffer(j)(k)
//               }
//             }
//             when(excp_s0(i)) {
//               when(VluopEntry(entry).fof) {
//                 when(VluopEntry(entry).uop.robIdx.value === 0.U & is_first_ele_s0(i)) {
//                   VluopEntry(entry).excp_eew_index := excep_ele_index_s0(i)
//                   VluopEntry(entry).exceptionVec := exceptionVec_s0(i)
//                 }
//               }.otherwise {
//                 when(VluopEntry(entry).excp_eew_index < excep_ele_index_s0(i)) {
//                   VluopEntry(entry).excp_eew_index := excep_ele_index_s0(i)
//                   VluopEntry(entry).exceptionVec := exceptionVec_s0(i)
//                 }
//               }
//             }
//           }
//         }
//       }
//     }
//   }
//   //write data from second_level buffer to VluopEntry
//   //for (i <- 0 until VecLoadPipelineWidth) {
//   //  for (j <- 0 until 2) {
//   //    when (buffer_valid_s1(i)(j) && rob_idx_valid_s1(i)(j)) {
//   //      for (entry <- 0 until VlUopSize) {
//   //        when (rob_idx_s1(i)(j).value === VluopEntry(entry).uop.robIdx.value && inner_idx_s1(i)(j) === VluopEntry(entry).uop.ctrl.uopIdx) {
//   //          counter(entry) := counter(entry) - 1.U
//   //          for (k <- 0 until VLEN/8) {
//   //            when (mask_buffer_s1(i)(j)(k)) {
//   //              VluopEntry(entry).data(k)      := data_buffer_s1(i)(j)(k*8 + 7,k*8)
//   //              VluopEntry(entry).dataVMask(k) := mask_buffer_s1(i)(j)(k)
//   //            }
//   //          }
//   //        }
//   //      }
//   //    }
//   //  }
//   //}

//   //finished = 1 means completion
//   for (entry <- 0 until VlUopSize) {
//     finished(entry) := valid(entry) && allocated(entry) && counter(entry) === 0.U
//   }

// /**
//   *dequeue logic*/
//   val vlUopQueueBank = VecInit(Seq.tabulate(VecLoadPipelineWidth)(i => getRemBits(finished.asUInt)(i)))
//   val deqPtr = VecInit(Seq.tabulate(VecLoadPipelineWidth)(i => {
//     val value = PriorityEncoder(vlUopQueueBank(i))
//     Cat(value,i.U(log2Up(VecLoadPipelineWidth).W))
//   }))

//   for (i <- 0 until VecLoadPipelineWidth) {
//     io.vecLoadWriteback(i).bits := DontCare
//     io.vecLoadWriteback(i).valid := valid(deqPtr(i)) && finished(deqPtr(i))
//     io.vecLoadWriteback(i).bits.uop := VluopEntry(deqPtr(i)).uop
//     io.vecLoadWriteback(i).bits.data := VluopEntry(deqPtr(i)).data.asUInt
//     when (io.vecLoadWriteback(i).fire) { //TODO:need optimization?
//       valid(deqPtr(i))                := false.B
//       allocated(deqPtr(i))            := false.B
//       pre_allocated(deqPtr(i))        := false.B
//       finished(deqPtr(i))             := false.B
//       VluopEntry(deqPtr(i)).dataVMask := VecInit(Seq.fill(VLEN / 8)(false.B))
//       free(i)                         := UIntToOH(deqPtr(i))
//     }
//   }

//   /**
//     * Redirection occurred, refreshing queue */
//   for (entry <- 0 until VlUopSize) {
//     needFlush(entry) := VluopEntry(entry).uop.robIdx.needFlush(io.redirect) && valid(entry)
//     when (needFlush(entry)) {
//       valid(entry)         := false.B
//       allocated(entry)     := false.B
//       pre_allocated(entry) := false.B
//       finished(entry)      := false.B
//       VluopEntry(entry).dataVMask := VecInit(Seq.fill(VLEN / 8)(false.B))
//     }
//   }

//   val lastRedirect = RegNext(io.redirect)
//   when (lastRedirect.valid) {
//     vlUopFreeList.io.free := RegNext(needFlush.asUInt)
//   }.otherwise {
//     vlUopFreeList.io.free := free.reduce(_|_)
//   }

}