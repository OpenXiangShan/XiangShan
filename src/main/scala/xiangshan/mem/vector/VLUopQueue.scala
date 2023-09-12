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
  val flowMask       = UInt(VLENB.W) // each bit for a flow
  val byteMask       = UInt(VLENB.W) // each bit for a byte
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
  val flowWritebackWidth = io.flowWriteback.length

  /**
    * TODO @zlj
    */

  val uopq = Reg(Vec(VlUopSize, new VluopBundle))
  val valid = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  val finish = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  val preAlloc = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  val exception = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  val vstart = RegInit(VecInit(Seq.fill(VlUopSize)(0.U(elemIdxBits.W)))) // index of the exception element
  val vl = RegInit(VecInit(Seq.fill(VlUopSize)(0.U.asTypeOf(Valid(UInt(elemIdxBits.W)))))) // only for fof instructions that modify vl

  val enqPtrExt = RegInit(VecInit((0 until maxMUL).map(_.U.asTypeOf(new VluopPtr))))
  val enqPtr = enqPtrExt(0)
  val deqPtr = RegInit(0.U.asTypeOf(new VluopPtr))
  val flowSplitPtr = RegInit(0.U.asTypeOf(new VluopPtr))
  val flowSplitIdx = RegInit(VecInit((0 until flowIssueWidth).map(_.U(flowIdxBits.W))))

  /**
    * s_merge -> Writeback of uop is put on hold. vdResult is waiting for posterior uops.
    * s_wb -> vsResult has receives the uop with `vd_last_uop` bit on and is ready for writeback.
    */
  val vdResult = Reg(UInt(VLEN.W)) // joint all the uops' result that is written into the same vd
  val s_merge :: s_wb :: Nil = Enum(2)
  val vdState = RegInit(s_merge)
  val vdException = RegInit(0.U.asTypeOf(Valid(ExceptionVec())))
  val vdUop = Reg(new MicroOp)
  val vdMask = RegInit(0.U(VLENB.W))
  val vdVl = RegInit(0.U.asTypeOf(Valid(UInt(elemIdxBits.W))))

  val full = isFull(enqPtr, deqPtr)

  /**
    * Redirect
    */
  val flushVec = valid.zip(uopq).map { case (v, entry) => v && entry.uop.robIdx.needFlush(io.redirect) }
  val flushEnq = io.loadRegIn.fire() && io.loadRegIn.bits.uop.robIdx.needFlush(io.redirect)
  val flushNumReg = RegNext(PopCount(flushEnq +: flushVec))
  val redirectReg = RegNext(io.redirect)

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
    decode.isIndexed && numSewLog2.asSInt > numEewLog2.asSInt,
    // If this is an index load, and multiple index regs are mapped into a data reg:
    // (*.asUInt - *.asUInt) should be equal to (*.asSInt - *.asSInt) 
    1.U << (numSewLog2 - numEewLog2),
    // otherwise:
    1.U
  )

  when (io.loadRegIn.fire()) {
    val id = enqPtr.value
    val preAllocated = preAlloc(id)
    val isSegment = decode.uop_segment_num =/= 0.U && !decode.uop_unit_stride_whole_reg
    val instType = Cat(isSegment, decode.uop_type)
    val uopIdx = io.loadRegIn.bits.uop.ctrl.uopIdx(uopIdxBits - 1, 0)
    val flows = GenRealFlowNum(instType, emul, lmul, eew, sew)
    val flowsLog2 = GenRealFlowLog2(instType, emul, lmul, eew, sew)
    val flowsPrev = uopIdx << flowsLog2 // # of flow before this uop
    val alignedType = Mux(isIndexed(instType), sew(1, 0), eew(1, 0))
    val srcMask = Mux(decode.mask_en, -1.asSInt.asUInt, io.loadRegIn.bits.src_mask)
    val flowMask = ((srcMask >> flowsPrev) &
      ZeroExt(UIntToMask(flows, maxFlowNum), VLEN))(VLENB - 1, 0)
    valid(id) := true.B
    finish(id) := false.B
    exception(id) := false.B
    vstart(id) := 0.U
    vl(id).valid := false.B
    // vl(id).bits := io.loadRegIn.bits.uop.ctrl.vconfig.vtype.vl
    uopq(id).uop := io.loadRegIn.bits.uop
    uopq(id).flowMask := flowMask
    uopq(id).byteMask := GenUopByteMask(flowMask, alignedType)(VLENB - 1, 0)
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
    // assert(!uopq(id).flowMask.asUInt.orR, "mask should be cleared when a uop entry deallocated")
    assert(PopCount(flows) <= 1.U, "flowNum should be the power of 2")

    // Pre-allocate
    when (!preAllocated) {
      // This is the first uop mapped into the same vd
      enqPtrExt.zipWithIndex.foreach { case (ptr, i) =>
        when (i.U < numUopsSameVd) {
          // assert(!preAlloc(ptr.value))
          // assert(!uopq(ptr.value).vd_last_uop)

          preAlloc(ptr.value) := true.B
          uopq(ptr.value).vd_last_uop := (i + 1).U === numUopsSameVd
        }
      }
    }.otherwise {
      // Otherwise, this uop has already been pre-allocated
      assert(io.loadRegIn.bits.uop.robIdx === uopq(id).uop.robIdx)
      assert(io.loadRegIn.bits.src_rs1(VAddrBits - 1, 0) === uopq(id).baseAddr)
    }
  }

  // update enqPtrExt
  when (RegNext(io.redirect.valid)) {
    enqPtrExt.foreach(ptr => ptr := ptr - flushNumReg)
  }.otherwise {
    when (io.loadRegIn.fire()) {
      enqPtrExt.foreach(ptr => ptr := ptr + 1.U)
    }
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
    val mask = genVWmask(vaddr, issueAlignedType)
    val regOffset = (elemIdxInsideField << issueAlignedType)(vOffsetBits - 1, 0)
    val exp = VLExpCtrl(
      vstart = issueVstart,
      vl = Mux(issueEntry.usWholeReg, GenUSWholeRegVL(issueNFIELDS, issueEew), issueVl),
      eleIdx = elemIdxInsideField
    )

    issuePort.valid := issueValid && flowIdx < issueFlowNum &&
      !issueUop.robIdx.needFlush(io.redirect) &&
      !issueUop.robIdx.needFlush(redirectReg)
    
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

  when (!RegNext(io.redirect.valid) || distanceBetween(enqPtr, flowSplitPtr) > flushNumReg) {
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
  }.otherwise {
    // flowSplitPtr needs to be redirected
    flowSplitPtr := enqPtr - flushNumReg
    flowSplitIdx := VecInit((0 until flowIssueWidth).map(_.U)) // initialize flowIdx
  }

  /**
    * Write back flows from flow queue
    */
  val flowWbMask = Wire(Vec(flowWritebackWidth, UInt(VLENB.W)))
  val flowWbExcp = Wire(Vec(flowWritebackWidth, ExceptionVec()))
  io.flowWriteback.zipWithIndex.foreach{ case (wb, i) =>
    val ptr = wb.bits.vec.uopQueuePtr
    val entry = uopq(ptr.value)
    val alignedType = Mux(isIndexed(entry.instType), entry.sew(1, 0), entry.eew(1, 0))
    flowWbMask(i) := GenFlowMaskInsideReg(alignedType, wb.bits.vec.exp_ele_index)
    flowWbExcp(i) := wb.bits.uop.cf.exceptionVec

    // handle the situation where multiple ports are going to write the same uop queue entry
    val mergedByPrevPort = (i != 0).B && Cat((0 until i).map(j =>
      io.flowWriteback(j).bits.vec.uopQueuePtr === wb.bits.vec.uopQueuePtr)).orR
    val mergePortVec = (0 until flowWritebackWidth).map(j => (j == i).B ||
      (j > i).B &&
      io.flowWriteback(j).bits.vec.uopQueuePtr === wb.bits.vec.uopQueuePtr &&
      io.flowWriteback(j).valid)
    val mergedData = mergeData(
      oldData = entry.data.asUInt,
      newData = VecInit(io.flowWriteback.map(_.bits.vec.vecdata)),
      mask = VecInit(mergePortVec.zip(flowWbMask).map { case (merge, mask) => Mux(merge, mask, 0.U) })
    )
    val nextFlowCnt = entry.flow_counter - PopCount(mergePortVec)

    // update data and decrease flow_counter when the writeback port is not merged
    when (wb.valid && !mergedByPrevPort) {
      entry.data := mergedData
      entry.flow_counter := nextFlowCnt
      finish(ptr.value) := nextFlowCnt === 0.U
      when (!exception(ptr.value) && flowWbExcp(i).asUInt.orR) {
        when (!entry.fof || wb.bits.vec.exp_ele_index === 0.U) {
          // For fof loads, if element 0 raises an exception, vl is not modified, and the trap is taken.
          exception(ptr.value) := true.B
          vstart(ptr.value) := wb.bits.vec.exp_ele_index
          entry.uop.cf.exceptionVec := flowWbExcp(i)
        }.otherwise {
          // If an element > 0 raises an exception, the corresponding trap is not taken, and the vector longth vl is
          // reduced to the index of the element that would have raised an exception.
          when (!vl(ptr.value).valid) {
            vl(ptr.value).valid := true.B
            vl(ptr.value).bits := wb.bits.vec.exp_ele_index
          }
        }
      }
    }

    assert(!(wb.valid && !valid(ptr.value)), "flow queue is trying to write back an empty entry")
  }

  /**
    * Dequeue according to finish bit pointed by deqPtr
    * 
    * However, dequeued entry is not writebacked to issue queue immediately but awaited the last uop that is
    * going to write the same register dequeued.
    */
  val deqValid = finish(deqPtr.value) &&
    !uopq(deqPtr.value).uop.robIdx.needFlush(io.redirect) &&
    !uopq(deqPtr.value).uop.robIdx.needFlush(redirectReg)
  val deqReady = vdState === s_merge

  when (deqValid && deqReady) {
    val id = deqPtr.value
    valid(id) := false.B
    finish(id) := false.B
    preAlloc(id) := false.B
    exception(id) := false.B
    vstart (id) := 0.U
    vl(id).valid := false.B

    uopq(id).flowMask := 0.U
    uopq(id).byteMask := 0.U

    deqPtr := deqPtr + 1.U
  }
  assert(!(finish(deqPtr.value) && !valid(deqPtr.value)))

  when (vdState === s_merge) {
    when (deqValid) {
      val id = deqPtr.value
      val byteMask = uopq(id).byteMask
      val data = uopq(id).data
      vdResult := mergeData(vdResult, data.asUInt, byteMask).asUInt
      vdMask := vdMask | byteMask
      vdUop := uopq(id).uop

      when (!vdException.valid && exception(id)) {
        vdException.valid := true.B
        vdException.bits := uopq(id).uop.cf.exceptionVec
        vdUop.ctrl.vconfig.vstart := vstart(id)
      }

      when (!vdVl.valid && vl(id).valid) {
        vdVl.valid := true.B
        vdVl.bits := vl(id).bits
      }

      when (uopq(id).vd_last_uop) {
        vdState := s_wb
      }
    }

    when (vdUop.robIdx.needFlush(io.redirect)) {
      vdException := 0.U.asTypeOf(vdException)
      vdMask := 0.U
    }
  }

  when (vdState === s_wb) {
    when (io.uopWriteback.ready || vdUop.robIdx.needFlush(io.redirect)) {
      vdException := 0.U.asTypeOf(vdException)
      vdMask := 0.U
      vdVl.valid := false.B

      vdState := s_merge
    }
  }

  /**
    * IO assignments
    */
  io.loadRegIn.ready := !full && preAlloc(enqPtr.value) || distanceBetween(enqPtr, deqPtr) >= numUopsSameVd

  io.flowWriteback.foreach(_.ready := true.B)

  io.uopWriteback.valid := vdState === s_wb && !vdUop.robIdx.needFlush(io.redirect)
  io.uopWriteback.bits.uop := vdUop
  io.uopWriteback.bits.uop.cf.exceptionVec := vdException.bits
  when (vdVl.valid) { io.uopWriteback.bits.uop.ctrl.vconfig.vl := vdVl.bits }
  io.uopWriteback.bits.data := vdResult
  io.uopWriteback.bits.mask := vdMask
  io.uopWriteback.bits.fflags := DontCare
  io.uopWriteback.bits.redirectValid := false.B
  io.uopWriteback.bits.redirect := DontCare
  io.uopWriteback.bits.debug := DontCare

  assert(!(issueValid && !io.flowIssue(0).valid && io.flowIssue(1).valid), "flow issue port 0 should have higher priority")

  /**
    * Miscs
    */
  def isUnitStride(instType: UInt) = instType(1, 0) === "b00".U
  def isStrided(instType: UInt) = instType(1, 0) === "b10".U
  def isIndexed(instType: UInt) = instType(0) === "b1".U
  def isNotIndexed(instType: UInt) = instType(0) === "b0".U

  def getByte(data: UInt, i: Int): UInt = {
    require(data.getWidth >= (i+1)*8)
    data((i+1)*8 - 1, i*8)
  }

  def mergeData(oldData: UInt, newData: UInt, mask: UInt): Vec[UInt] = {
    require(oldData.getWidth == newData.getWidth)
    require(oldData.getWidth == mask.getWidth * 8)
    VecInit(mask.asBools.zipWithIndex.map { case (en, i) =>
      Mux(en, getByte(newData, i), getByte(oldData, i))
    })
  }

  def mergeData(oldData: UInt, newData: Vec[UInt], mask: Vec[UInt]): Vec[UInt] = {
    require(oldData.getWidth == newData.head.getWidth)
    require(oldData.getWidth == mask.head.getWidth * 8)
    require(newData.length == mask.length)
    // When there are multiple flows want to write the same byte, choose the youngest flow to write
    VecInit((0 until mask.head.getWidth).map { case i =>
      val newBytes = newData.map(data => getByte(data, i))
      val oldByte = getByte(oldData, i)
      val wens = mask.map(_(i).asBool)
      ParallelPosteriorityMux(false.B +: wens, oldByte +: newBytes)
    })
  }

}