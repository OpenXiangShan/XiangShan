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
import chisel3.{util, _}
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.vector.Bundles._

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

object VLExpCtrl {
  def apply (vstart: UInt, vl: UInt, eleIdx: UInt):Bool = {
    val exp = Wire(Bool())
    when (vstart >= vl || vl === 0.U) {
      exp := false.B
    }.otherwise {
      when (eleIdx >= vstart && eleIdx < vl) {
        exp := true.B
      }.otherwise {
        exp := false.B
      }
    }
    exp
  }
}

class VluopBundle(implicit p: Parameters) extends VecUopBundle {
  val fof            = Bool()
  val vdIdxInField = UInt(log2Up(maxMUL).W)
}

class VlUopQueueIOBundle(implicit p: Parameters) extends VLSUBundle {
  // redirect
  val redirect = Flipped(ValidIO(new Redirect))
  // input from load rs along with regfile src data
  val loadRegIn = Flipped(DecoupledIO(new MemExuInput(isVector = true)))
  // issue 2 flows from uop queue each cycle
  val flowIssue = Vec(VecLoadPipelineWidth, DecoupledIO(new VlflowBundle()))
  // writeback 2 flow results orderly from flow queue each cycle
  val flowWriteback = Vec(VecLoadPipelineWidth, Flipped(DecoupledIO(new VecExuOutput())))
  // feedbacks to signal that uop queue is full
  // val uopFeedback = Output(Vec(VecLoadPipelineWidth, Bool()))
  // writeback uop results
  val uopWriteback = DecoupledIO(new MemExuOutput(isVector = true))
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

  val uopq = Reg(Vec(VlUopSize, new VluopBundle))
  val valid = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  val finish = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  val preAlloc = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  val exception = RegInit(VecInit(Seq.fill(VlUopSize)(false.B)))
  val vstart = RegInit(VecInit(Seq.fill(VlUopSize)(0.U(elemIdxBits.W)))) // index of the exception element
  val vl = RegInit(VecInit(Seq.fill(VlUopSize)(0.U.asTypeOf(Valid(UInt(elemIdxBits.W)))))) // only for fof instructions that modify vl
  val srcMaskVec = Reg(Vec(VlUopSize, UInt(VLEN.W)))

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
  val vdUop = RegInit(0.U.asTypeOf(new DynInst))
  val vdMask = RegInit(0.U(VLENB.W))
  val vdSrcMask = RegInit(0.U(VLEN.W))
  val vdVl = RegInit(0.U.asTypeOf(Valid(UInt(elemIdxBits.W))))
  val vdIdx = RegInit(0.U(3.W)) // TODO: parameterize width
  val vdIdxInField = RegInit(0.U(log2Up(maxMUL).W))

  val full = isFull(enqPtr, deqPtr)

  /**
    * Redirect
    */
  val flushVec = valid.zip(uopq).map { case (v, entry) => v && entry.uop.robIdx.needFlush(io.redirect) }
  val flushEnq = io.loadRegIn.fire && io.loadRegIn.bits.uop.robIdx.needFlush(io.redirect)
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
  // val decode = Wire(new VecDecode())
  // decode.apply(io.loadRegIn.bits.uop.instr)
  def us_whole_reg(fuOpType: UInt) = fuOpType === VlduType.vlr
  def us_mask(fuOpType: UInt) = fuOpType === VlduType.vlm
  def us_fof(fuOpType: UInt) = fuOpType === VlduType.vleff
  val vtype = io.loadRegIn.bits.uop.vpu.vtype
  val sew = vtype.vsew
  val eew = io.loadRegIn.bits.uop.vpu.veew
  val lmul = vtype.vlmul
  // when load whole register or unit-stride masked , emul should be 1
  val fuOpType = io.loadRegIn.bits.uop.fuOpType
  val mop = fuOpType(6, 5)
  val nf = io.loadRegIn.bits.uop.vpu.nf
  val vm = io.loadRegIn.bits.uop.vpu.vm
  val emul = Mux(us_whole_reg(fuOpType) || us_mask(fuOpType), 0.U(mulBits.W), EewLog2(eew) - sew + lmul)
  val lmulLog2 = Mux(lmul.asSInt >= 0.S, 0.U, lmul)
  val emulLog2 = Mux(emul.asSInt >= 0.S, 0.U, emul)
  val numEewLog2 = emulLog2 - EewLog2(eew)
  val numSewLog2 = lmulLog2 - sew
  val numUopsSameVd = Mux(
    isIndexed(mop) && numSewLog2.asSInt > numEewLog2.asSInt,
    // If this is an index load, and multiple index regs are mapped into a data reg:
    // (*.asUInt - *.asUInt) should be equal to (*.asSInt - *.asSInt) 
    1.U << (numSewLog2 - numEewLog2),
    // otherwise:
    1.U
  )
  val lmulLog2Pos = Mux(lmul.asSInt < 0.S, 0.U, lmul)
  val emulLog2Pos = Mux(emul.asSInt < 0.S, 0.U, emul)
  // numUops = nf * max(lmul, emul)
  val numUops = Mux(
    isIndexed(mop) && lmul.asSInt > emul.asSInt,
    (nf +& 1.U) << lmulLog2Pos,
    (nf +& 1.U) << emulLog2Pos
  )
    

  when (io.loadRegIn.fire) {
    val id = enqPtr.value
    val preAllocated = preAlloc(id)
    val isSegment = nf =/= 0.U && !us_whole_reg(fuOpType)
    val instType = Cat(isSegment, mop)
    val uopIdx = io.loadRegIn.bits.uop.vpu.vuopIdx
    val uopIdxInField = GenUopIdxInField(instType, emul, lmul, uopIdx)
    val vdIdxInField = GenVdIdxInField(instType, emul, lmul, uopIdxInField)
    val numFlowsSameVdLog2 = Mux(
      isIndexed(instType),
      log2Up(VLENB).U - sew(1,0),
      log2Up(VLENB).U - eew(1,0)
    )
    val flows = GenRealFlowNum(instType, emul, lmul, eew, sew)
    val flowsLog2 = GenRealFlowLog2(instType, emul, lmul, eew, sew)
    val flowsPrevThisUop = uopIdxInField << flowsLog2 // # of flows before this uop in a field
    val flowsPrevThisVd = vdIdxInField << numFlowsSameVdLog2 // # of flows before this vd in a field
    val flowsIncludeThisUop = (uopIdxInField +& 1.U) << flowsLog2 // # of flows before this uop besides this uop
    val alignedType = Mux(isIndexed(instType), sew(1, 0), eew(1, 0))
    val srcMask = Mux(vm, Fill(VLEN, 1.U(1.W)), io.loadRegIn.bits.src_mask)
    val flowMask = ((srcMask &
      UIntToMask(flowsIncludeThisUop, VLEN + 1) &
      ~UIntToMask(flowsPrevThisUop, VLEN)
    ) >> flowsPrevThisVd)(VLENB - 1, 0)
    dontTouch(flowsPrevThisUop)
    dontTouch(flowsPrevThisVd)
    dontTouch(flowsIncludeThisUop)
    valid(id) := true.B
    finish(id) := false.B
    exception(id) := false.B
    vstart(id) := 0.U
    vl(id).valid := false.B
    srcMaskVec(id) := srcMask
    uopq(id) match { case x =>
      x.uop := io.loadRegIn.bits.uop
      x.uop.vpu.vl := io.loadRegIn.bits.src_vl.asTypeOf(VConfig()).vl
      x.uop.numUops := numUops
      x.uop.lastUop := (uopIdx +& 1.U) === numUops
      x.flowMask := flowMask
      x.byteMask := GenUopByteMask(flowMask, alignedType)(VLENB - 1, 0)
      x.fof := isUnitStride(mop) && us_fof(fuOpType)
      x.baseAddr := io.loadRegIn.bits.src_rs1
      x.stride := io.loadRegIn.bits.src_stride
      x.flow_counter := flows
      x.flowNum := flows
      x.nfields := nf +& 1.U
      x.vm := vm
      x.usWholeReg := isUnitStride(mop) && us_whole_reg(fuOpType)
      x.usMaskReg := isUnitStride(mop) && us_mask(fuOpType)
      x.eew := eew
      x.sew := sew
      x.emul := emul
      x.lmul := lmul
      x.vlmax := GenVLMAX(lmul, sew)
      x.instType := instType
      x.data := io.loadRegIn.bits.src_vs3
      x.vdIdxInField := vdIdxInField
    }

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
          uopq(ptr.value).vd_first_uop := (i == 0).B
        }
      }
    }
  }

  // update enqPtrExt
  when (redirectReg.valid && flushNumReg =/= 0.U) {
    enqPtrExt.foreach(ptr => ptr := ptr - flushNumReg)
  }.otherwise {
    when (io.loadRegIn.fire) {
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
  val issueBaseAddr = issueEntry.baseAddr
  val issueUop = issueEntry.uop
  val issueUopIdx = issueUop.vpu.vuopIdx
  val issueInstType = issueEntry.instType
  val issueEew = issueEntry.eew
  val issueSew = issueEntry.sew
  val issueAlignedType = Mux(isIndexed(issueInstType), issueSew(1, 0), issueEew(1, 0))
  val issueMUL = Mux(isIndexed(issueInstType), issueEntry.lmul, issueEntry.emul)
  val issueVLMAXMask = issueEntry.vlmax - 1.U
  val issueMULMask = LookupTree(issueAlignedType, List(
    "b00".U -> "b01111".U,
    "b01".U -> "b00111".U,
    "b10".U -> "b00011".U,
    "b11".U -> "b00001".U
  ))
  val issueFieldMask = Mux(
    !isSegment(issueInstType) || issueMUL.asSInt >= 0.S,
    issueVLMAXMask,
    issueMULMask
  )
  val issueNFIELDS = issueEntry.nfields
  val issueVstart = issueUop.vpu.vstart
  val issueVl = issueUop.vpu.vl
  val issueFlowMask = issueEntry.flowMask
  val issueLmulGreaterThanEmul = issueEntry.lmul.asSInt > issueEntry.emul.asSInt
  assert(!issueValid || PopCount(issueEntry.vlmax) === 1.U, "VLMAX should be power of 2 and non-zero")

  val elemIdxInsideVd = Wire(Vec(flowIssueWidth, UInt(flowIdxBits.W)))
  dontTouch(elemIdxInsideVd)
  flowSplitIdx.zip(io.flowIssue).zipWithIndex.foreach { case ((flowIdx, issuePort), portIdx) =>
    // AGU
    // TODO: DONT use * to implement multiplication!!!
    val elemIdx = GenElemIdx(
      alignedType = Mux(
        isIndexed(issueInstType) && issueLmulGreaterThanEmul,
        issueSew(1, 0),
        issueEew(1, 0)
      ),
      uopIdx = issueUopIdx,
      flowIdx = flowIdx
    ) // elemIdx inside an inst
    val elemIdxInsideField = elemIdx & issueFieldMask // elemIdx inside a field, equals elemIdx when nf = 1
    elemIdxInsideVd(portIdx) := elemIdx & issueMULMask // elemIdx inside a vd
    val nfIdx = Mux(
      isIndexed(issueInstType),
      GenSegNfIdx(Mux(issueLmulGreaterThanEmul, issueEntry.lmul, issueEntry.emul), issueUopIdx),
      GenSegNfIdx(issueEntry.emul, issueUopIdx)
    )
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
    val mask = issueEntry.byteMask
    val regOffset = (elemIdxInsideField << issueAlignedType)(vOffsetBits - 1, 0)
    val enable = (issueFlowMask & UIntToOH(elemIdxInsideVd(portIdx))).orR
    val ttttvl = Mux(issueEntry.usWholeReg, GenUSWholeRegVL(issueNFIELDS, issueEew), Mux(issueEntry.usMaskReg, GenUSMaskRegVL(issueVl), issueVl))
    val exp = VLExpCtrl(
      vstart = issueVstart,
      vl = ttttvl,
      eleIdx = elemIdxInsideField
    ) && enable
    dontTouch(ttttvl)
    dontTouch(vstart)
    dontTouch(elemIdxInsideField)
    dontTouch(enable)

    issuePort.valid := issueValid && flowIdx < issueFlowNum &&
      !issueUop.robIdx.needFlush(io.redirect) &&
      !issueUop.robIdx.needFlush(redirectReg)
    
    issuePort.bits match { case x =>
      x.uop := issueUop
      x.vaddr := vaddr
      x.mask := mask
      x.unit_stride_fof := issueEntry.fof
      x.reg_offset := regOffset
      x.alignedType := issueAlignedType
      x.exp := exp
      x.elemIdx := elemIdx
      x.is_first_ele := elemIdx === 0.U
      x.uopQueuePtr := flowSplitPtr
    }
  }
  // unset the byteMask if `exp` of the element is false
  when (issueValid) {
    issueEntry.byteMask := issueEntry.byteMask & ~(
      io.flowIssue.zipWithIndex.map { case (issuePort, i) =>
        val unsetFlowMask = VecInit(Seq.tabulate(VLENB){ j =>
          elemIdxInsideVd(i) === j.U && issuePort.fire && !issuePort.bits.exp
        }).asUInt
        val unsetByteMask = GenUopByteMask(unsetFlowMask, issueAlignedType)(VLENB - 1, 0)
        unsetByteMask
      }.fold(0.U(VLENB.W))(_ | _)
    )
  }

  val numFlowIssue = PopCount(io.flowIssue.map(_.fire))
  val flowIssueFire = Cat(io.flowIssue.map(_.fire)).orR

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
  val flowWbElemIdx = Wire(Vec(flowWritebackWidth, UInt(elemIdxBits.W)))
  val flowWbExcp = Wire(Vec(flowWritebackWidth, ExceptionVec()))
  val flowWbExp = Wire(Vec(flowWritebackWidth, Bool()))
  io.flowWriteback.zipWithIndex.foreach { case (wb, i) =>
    val ptr = wb.bits.vec.uopQueuePtr
    val entry = uopq(ptr.value)
    val alignedType = Mux(isIndexed(entry.instType), entry.sew(1, 0), entry.eew(1, 0))
    flowWbElemIdx(i) := wb.bits.vec.elemIdx
    flowWbExcp(i) := wb.bits.uop.exceptionVec
    flowWbExp(i) := wb.bits.vec.exp
    val flowWbElemIdxInField = flowWbElemIdx(i) & GenFieldMask(
      instType = entry.instType,
      emul = entry.emul,
      lmul = entry.lmul,
      eew = entry.eew,
      sew = entry.sew
    )

    // handle the situation where multiple ports are going to write the same uop queue entry
    val mergedByPrevPort = (i != 0).B && Cat((0 until i).map(j =>
      io.flowWriteback(j).bits.vec.uopQueuePtr === wb.bits.vec.uopQueuePtr)).orR
    val mergePortVec = (0 until flowWritebackWidth).map(j => (j == i).B ||
      (j > i).B &&
      io.flowWriteback(j).bits.vec.uopQueuePtr === wb.bits.vec.uopQueuePtr &&
      io.flowWriteback(j).valid)
    val mergeExpPortVec = (0 until flowWritebackWidth).map(j => flowWbExp(j) && mergePortVec(j))
    val mergedData = mergeDataWithElemIdx(
      oldData = entry.data.asUInt,
      newData = io.flowWriteback.map(_.bits.vec.vecdata),
      alignedType = alignedType,
      elemIdx = flowWbElemIdx,
      valids = mergeExpPortVec
    )
    val nextFlowCnt = entry.flow_counter - PopCount(mergePortVec)

    // handle the situation when the writebacked flows nuke and the vector ld needs to replay from fronend
    val replayInst = Cat((0 until flowWritebackWidth).map(j =>
      mergeExpPortVec(j) && io.flowWriteback(j).bits.uop.replayInst)).orR

    // update data and decrease flow_counter when the writeback port is not merged
    when (wb.valid && !mergedByPrevPort) {
      entry.data := mergedData
      entry.flow_counter := nextFlowCnt
      finish(ptr.value) := nextFlowCnt === 0.U
      when (!exception(ptr.value) && flowWbExcp(i).asUInt.orR) {
        when (!entry.fof || flowWbElemIdxInField === 0.U) {
          // For fof loads, if element 0 raises an exception, vl is not modified, and the trap is taken.
          exception(ptr.value) := true.B
          vstart(ptr.value) := flowWbElemIdxInField
          entry.uop.exceptionVec := flowWbExcp(i)
        }.otherwise {
          // If an element > 0 raises an exception, the corresponding trap is not taken, and the vector longth vl is
          // reduced to the index of the element that would have raised an exception.
          when (!vl(ptr.value).valid) {
            vl(ptr.value).valid := true.B
            vl(ptr.value).bits := flowWbElemIdxInField
          }
        }
      }
      // mark the uops that need to be replayed from frontend
      entry.uop.replayInst := entry.uop.replayInst || replayInst
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
      vdResult := mergeDataWithMask(
        oldData = vdResult,
        newData = data.asUInt,
        /**
          * 1. If this is the first uop of a vd, all the bytes should be written into vdResult,
          *    because the old vd needs to be transfered to backend.
          * 2. Otherwise, only the masked bytes are needed.
          */
        mask = Mux(uopq(id).vd_first_uop, Fill(VLENB, 1.U(1.W)), byteMask)
      ).asUInt
      vdMask := vdMask | byteMask
      vdSrcMask := srcMaskVec(id)
      vdUop := uopq(id).uop
      vdUop.replayInst := vdUop.replayInst || uopq(id).uop.replayInst
      vdIdxInField := uopq(id).vdIdxInField

      when (!vdException.valid && exception(id)) {
        vdException.valid := true.B
        vdException.bits := uopq(id).uop.exceptionVec
        vdUop.vpu.vstart := vstart(id)
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
      vdUop.replayInst := false.B

      when (vdUop.lastUop) {
        vdIdx := 0.U
      }.otherwise {
        vdIdx := vdIdx + 1.U
        assert(vdIdx + 1.U =/= 0.U, "Overflow! The number of vd should be less than 8")
      }

      vdState := s_merge
    }
  }

  /**
    * IO assignments
    */
  io.loadRegIn.ready := !full && preAlloc(enqPtr.value) || hasFreeEntries(enqPtr, deqPtr) >= numUopsSameVd

  io.flowWriteback.foreach(_.ready := true.B)

  io.uopWriteback.valid := vdState === s_wb && !vdUop.robIdx.needFlush(io.redirect)
  io.uopWriteback.bits.uop := vdUop
  io.uopWriteback.bits.uop.exceptionVec := vdException.bits
  when (vdVl.valid) { io.uopWriteback.bits.uop.vpu.vl := vdVl.bits }
  io.uopWriteback.bits.uop.vpu.vmask := vdSrcMask
  io.uopWriteback.bits.data := vdResult
  io.uopWriteback.bits.mask.foreach(_ := vdSrcMask) // TODO: delete vdMask
  io.uopWriteback.bits.vdIdx.foreach(_ := vdIdx)
  io.uopWriteback.bits.vdIdxInField.foreach(_ := vdIdxInField)
  io.uopWriteback.bits.debug := DontCare

  assert(!(issueValid && !io.flowIssue(0).valid && io.flowIssue(1).valid), "flow issue port 0 should have higher priority")

  for (i <- 1 until flowIssueWidth) {
    assert(!(issueValid && !io.flowIssue(i-1).valid && io.flowIssue(i).valid),
      "flow issue port i-1 should have higher priority than port_i")
  }
}