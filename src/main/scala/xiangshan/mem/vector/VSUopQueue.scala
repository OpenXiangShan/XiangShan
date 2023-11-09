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
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.vector.Bundles._

class VsUopPtr(implicit p: Parameters) extends CircularQueuePtr[VsUopPtr](
  p => p(XSCoreParamsKey).VsUopSize
){
}

object VsUopPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): VsUopPtr = {
    val ptr = Wire(new VsUopPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

object GenVSData extends VLSUConstants {
  def apply(data: UInt, elemIdx: UInt, alignedType: UInt): UInt = {
    LookupTree(alignedType, List(
      "b00".U -> ZeroExt(LookupTree(elemIdx(3, 0), List.tabulate(VLEN/8)(i => i.U -> getByte(data, i))), VLEN),
      "b01".U -> ZeroExt(LookupTree(elemIdx(2, 0), List.tabulate(VLEN/16)(i => i.U -> getHalfWord(data, i))), VLEN),
      "b10".U -> ZeroExt(LookupTree(elemIdx(1, 0), List.tabulate(VLEN/32)(i => i.U -> getWord(data, i))), VLEN),
      "b11".U -> ZeroExt(LookupTree(elemIdx(0), List.tabulate(VLEN/64)(i => i.U -> getDoubleWord(data, i))), VLEN)
    ))
  }
}

class VsuopBundle(implicit p: Parameters) extends VecUopBundle

class VsUopQueueIOBundle (implicit p: Parameters) extends XSBundle {
  val redirect = Flipped(ValidIO(new Redirect))
  val storeIn  = Flipped(Decoupled(new MemExuInput(isVector = true)))
  val flowIssue = Vec(VecStorePipelineWidth, Decoupled(new VsFlowBundle()))
  val flowWriteback = Vec(VecStorePipelineWidth, Flipped(DecoupledIO(new VecStoreExuOutput())))
  val uopWriteback = DecoupledIO(new MemExuOutput(isVector = true))
}
class VsUopQueue(implicit p: Parameters) extends VLSUModule {
  val io = IO(new VsUopQueueIOBundle())

  println("StoreUopQueue: size:" + VsUopSize)
  val flowIssueWidth = io.flowIssue.length
  val flowWritebackWidth = io.flowWriteback.length

  val uopq = Reg(Vec(VsUopSize, new VsuopBundle))
  val valid = RegInit(VecInit(Seq.fill(VsUopSize)(false.B)))
  val finish = RegInit(VecInit(Seq.fill(VsUopSize)(false.B)))
  val exception = RegInit(VecInit(Seq.fill(VsUopSize)(false.B)))
  val vstart = RegInit(VecInit(Seq.fill(VsUopSize)(0.U(elemIdxBits.W))))

  val enqPtr = RegInit(0.U.asTypeOf(new VsUopPtr))
  val deqPtr = RegInit(0.U.asTypeOf(new VsUopPtr))
  val flowSplitPtr = RegInit(0.U.asTypeOf(new VsUopPtr))
  val flowSplitIdx = RegInit(VecInit((0 until flowIssueWidth).map(_.U(flowIdxBits.W))))

  val full = isFull(enqPtr, deqPtr)

  /**
    * Redirect
    */
  val flushVec = valid.zip(uopq).map { case (v, entry) => v && entry.uop.robIdx.needFlush(io.redirect) }
  val flushEnq = io.storeIn.fire && io.storeIn.bits.uop.robIdx.needFlush(io.redirect)
  val flushNumReg = RegNext(PopCount(flushEnq +: flushVec))
  val redirectReg = RegNext(io.redirect)

  /**
    * Enqueue and decode logic
    */
  // val decode = Wire(new VecDecode())
  // decode.apply(io.storeIn.bits.uop.instr)
  def us_whole_reg(fuOpType: UInt) = fuOpType === VstuType.vsr
  def us_mask(fuOpType: UInt) = fuOpType === VstuType.vsm
  val vtype = io.storeIn.bits.uop.vpu.vtype
  val sew = vtype.vsew
  val eew = io.storeIn.bits.uop.vpu.veew
  val lmul = vtype.vlmul
  // when store whole register or unit-stride masked , emul should be 1
  val fuOpType = io.storeIn.bits.uop.fuOpType
  val mop = fuOpType(6, 5)
  val nf = io.storeIn.bits.uop.vpu.nf
  val vm = io.storeIn.bits.uop.vpu.vm
  val emul = Mux(us_whole_reg(fuOpType) || us_mask(fuOpType), 0.U(mulBits.W), EewLog2(eew) - sew + lmul)
  val numUops = Mux(lmul.asSInt > emul.asSInt, MulNum(lmul), MulNum(emul))
  
  when (io.storeIn.fire) {
    val id = enqPtr.value
    val isSegment = nf =/= 0.U && !us_whole_reg(fuOpType)
    val instType = Cat(isSegment, mop)
    val uopIdx = io.storeIn.bits.uop.vpu.vuopIdx
    val flows = GenRealFlowNum(instType, emul, lmul, eew, sew)
    val flowsLog2 = GenRealFlowLog2(instType, emul, lmul, eew, sew)
    val flowsPrev = uopIdx << flowsLog2 // # of flow before this uop
    val alignedType = Mux(isIndexed(instType), sew(1, 0), eew(1, 0))
    val srcMask = Mux(vm, Fill(VLEN, 1.U(1.W)), io.storeIn.bits.src_mask)
    val flowMask = ((srcMask >> flowsPrev) &
      ZeroExt(UIntToMask(flows, maxFlowNum), VLEN))(VLENB - 1, 0)
    val vlmax = GenVLMAX(lmul, sew)
    valid(id) := true.B
    finish(id) := false.B
    exception(id) := false.B
    vstart(id) := 0.U
    uopq(id) match { case x =>
      x.uop := io.storeIn.bits.uop
      x.uop.vpu.vl := io.storeIn.bits.src_vl.asTypeOf(VConfig()).vl
      x.uop.numUops := numUops
      x.uop.lastUop := (io.storeIn.bits.uop.uopIdx + 1.U) === numUops
      x.flowMask := flowMask
      x.byteMask := GenUopByteMask(flowMask, alignedType)(VLENB - 1, 0)
      x.data := io.storeIn.bits.src_vs3
      x.baseAddr := io.storeIn.bits.src_rs1
      x.stride := io.storeIn.bits.src_stride
      x.flow_counter := flows
      x.flowNum := flows
      x.nfields := nf + 1.U
      x.vm := vm
      x.usWholeReg := isUnitStride(mop) && us_whole_reg(fuOpType)
      x.usMaskReg := isUnitStride(mop) && us_mask(fuOpType)
      x.eew := eew
      x.sew := sew
      x.emul := emul
      x.lmul := lmul
      x.vlmax := vlmax
      x.instType := instType
    }

    // Assertion
    assert(PopCount(flows) <= 1.U, "flowNum should be the power of 2")
  }

  // update enqPtr
  when (redirectReg.valid && flushNumReg =/= 0.U) {
    enqPtr := enqPtr - flushNumReg
  }.otherwise {
    when (io.storeIn.fire) {
      enqPtr := enqPtr + 1.U
    }
  }

  /**
    * Split uop into flows
    */
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
  val issueVLMAXMask = issueEntry.vlmax - 1.U
  val issueVLMAXLog2 = GenVLMAXLog2(issueEntry.lmul, issueEntry.sew)
  val issueNFIELDS = issueEntry.nfields
  val issueVstart = issueUop.vpu.vstart
  val issueVl = issueUop.vpu.vl
  val issueFlowMask = issueEntry.flowMask
  assert(!issueValid || PopCount(issueEntry.vlmax) === 1.U, "VLMAX should be power of 2 and non-zero")

  flowSplitIdx.zip(io.flowIssue).foreach { case (flowIdx, issuePort) =>
    // AGU
    // TODO: DONT use * to implement multiplication!!!
    val elemIdx = GenElemIdx(issueAlignedType, issueUopIdx, flowIdx) // elemIdx inside an inst
    val elemIdxInsideField = elemIdx & issueVLMAXMask
    val nfIdx = elemIdx >> issueVLMAXLog2
    val notIndexedStride = Mux(
      isStrided(issueInstType),
      issueEntry.stride(XLEN - 1, 0), // for strided store, stride = x[rs2]
      issueNFIELDS << issueEew(1, 0) // for unit-stride store, stride = eew * NFIELDS
    ) * elemIdxInsideField
    val indexedStride = IndexAddr(
      index = issueEntry.stride,
      flow_inner_idx = (elemIdxInsideField << issueEew(1, 0))(vOffsetBits - 1, 0) >> issueEew(1, 0),
      eew = issueEew
    )
    val stride = Mux(isIndexed(issueInstType), indexedStride, notIndexedStride)
    val fieldOffset = nfIdx << issueAlignedType // field offset inside a segment
    val vaddr = issueBaseAddr + stride + fieldOffset
    val mask = issueEntry.byteMask
    val regOffset = (elemIdxInsideField << issueAlignedType)(vOffsetBits - 1, 0)
    val enable = (issueFlowMask & VecInit(Seq.tabulate(VLENB){ i => flowIdx === i.U }).asUInt).orR
    val exp = VLExpCtrl(
      vstart = issueVstart,
      vl = Mux(issueEntry.usWholeReg, GenUSWholeRegVL(issueNFIELDS, issueEew), Mux(issueEntry.usMaskReg, GenUSMaskRegVL(issueVl), issueVl)),
      eleIdx = elemIdxInsideField
    ) && enable

    issuePort.valid := issueValid && flowIdx < issueFlowNum &&
      !issueUop.robIdx.needFlush(io.redirect) &&
      !issueUop.robIdx.needFlush(redirectReg)
  
    issuePort.bits match { case x =>
      x.uop := issueUop
      x.vaddr := vaddr
      x.mask := mask
      x.alignedType := issueAlignedType
      x.exp := exp
      x.flow_idx := elemIdx
      x.is_first_ele := elemIdx === 0.U
      x.data := GenVSData(
        data = issueEntry.data.asUInt,
        elemIdx = elemIdxInsideField,
        alignedType = issueAlignedType
      )
      x.uopQueuePtr := flowSplitPtr
      x.isLastElem := (elemIdx +& 1.U) === (issueNFIELDS << issueVLMAXLog2)
    }
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
  val flowWbExcp = Wire(Vec(flowWritebackWidth, ExceptionVec()))
  io.flowWriteback.zipWithIndex.foreach { case (wb, i) =>
    val ptr = wb.bits.uopQueuePtr
    val entry = uopq(ptr.value)
    flowWbExcp(i) := wb.bits.uop.exceptionVec

    // handle the situation where multiple ports are going to write the same uop queue entry
    val mergedByPrevPort = (i != 0).B && Cat((0 until i).map(j =>
      io.flowWriteback(j).bits.uopQueuePtr === wb.bits.uopQueuePtr)).orR
    val mergePortVec = (0 until flowWritebackWidth).map(j => (j == i).B ||
      (j > i).B &&
      io.flowWriteback(j).bits.uopQueuePtr === wb.bits.uopQueuePtr &&
      io.flowWriteback(j).valid)
    val nextFlowCnt = entry.flow_counter - PopCount(mergePortVec)

    // update data and decrease flow_counter when the writeback port is not merged
    when (wb.valid && !mergedByPrevPort) {
      entry.flow_counter := nextFlowCnt
      finish(ptr.value) := nextFlowCnt === 0.U
      when (!exception(ptr.value) && flowWbExcp(i).asUInt.orR) {
        exception(ptr.value) := true.B
        vstart(ptr.value) := wb.bits.exp_ele_index
        entry.uop.exceptionVec := flowWbExcp(i)
      }
    }

    assert(!(wb.valid && !valid(ptr.value)))
  }

  /**
    * Dequeue according to finish bit pointer by deqPtr
    */
  when (io.uopWriteback.fire) {
    val id = deqPtr.value
    valid(id) := false.B
    finish(id) := false.B
    exception(id) := false.B
    vstart(id) := 0.U

    uopq(id).flowMask := 0.U
    uopq(id).byteMask := 0.U

    deqPtr := deqPtr + 1.U
  }
  assert(!(finish(deqPtr.value) && !valid(deqPtr.value)))

  /**
    * IO assignments
    */
  io.storeIn.ready := !full

  io.flowWriteback.foreach(_.ready := true.B)

  io.uopWriteback.valid := finish(deqPtr.value) &&
    !uopq(deqPtr.value).uop.robIdx.needFlush(io.redirect) &&
    !uopq(deqPtr.value).uop.robIdx.needFlush(redirectReg)
  io.uopWriteback.bits match { case x =>
    val id = deqPtr.value
    x.uop := uopq(id).uop
    x.uop.exceptionVec := uopq(id).uop.exceptionVec
    x.data := DontCare
    x.mask.foreach(_ := DontCare)
    x.vdIdx.foreach(_ := DontCare)
    x.debug := DontCare
  }

  for (i <- 1 until flowIssueWidth) {
    assert(!(issueValid && !io.flowIssue(i-1).valid && io.flowIssue(i).valid),
      "flow issue port_(i-1) should have higher priority than port_i")
  }
}