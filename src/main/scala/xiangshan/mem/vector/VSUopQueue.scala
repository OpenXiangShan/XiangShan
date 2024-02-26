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
  val preAlloc = RegInit(VecInit(Seq.fill(VsUopSize)(false.B)))
  val exception = RegInit(VecInit(Seq.fill(VsUopSize)(false.B)))
  val vstart = RegInit(VecInit(Seq.fill(VsUopSize)(0.U(elemIdxBits.W))))

  val enqPtrExt = RegInit(VecInit((0 until maxMUL).map(_.U.asTypeOf(new VsUopPtr))))
  val enqPtr = enqPtrExt(0)
  val deqPtr = RegInit(0.U.asTypeOf(new VsUopPtr))
  val flowSplitPtr = RegInit(0.U.asTypeOf(new VsUopPtr))
  val flowSplitIdx = RegInit(VecInit((0 until flowIssueWidth).map(_.U(flowIdxBits.W))))

  val s_merge :: s_wb :: Nil = Enum(2)
  val vdState = RegInit(s_merge)
  val vdException = RegInit(0.U.asTypeOf(Valid(ExceptionVec())))
  val vdUop = RegInit(0.U.asTypeOf(new DynInst))

  val full = isFull(enqPtr, deqPtr)

  /**
    * Redirect
    */
  val flushVec = valid.zip(uopq).map { case (v, entry) => v && entry.uop.robIdx.needFlush(io.redirect) }
  val flushEnq = io.storeIn.fire && io.storeIn.bits.uop.robIdx.needFlush(io.redirect)
  val flushNumReg = RegNext(PopCount(flushEnq +: flushVec))
  val redirectReg = RegNext(io.redirect)
  val flushVecReg = RegNext(WireInit(VecInit(flushVec)))

  /**
    * Enqueue and decode logic
    */
  def us_whole_reg(fuOpType: UInt) = fuOpType === VstuType.vsr
  def us_mask(fuOpType: UInt) = fuOpType === VstuType.vsm
  val vtype = io.storeIn.bits.uop.vpu.vtype
  val sew = vtype.vsew
  val eew = io.storeIn.bits.uop.vpu.veew
  val lmul = vtype.vlmul
  // when store whole register or unit-stride masked , emul should be 1
  val fuOpType = io.storeIn.bits.uop.fuOpType
  val mop = fuOpType(6, 5)
  val nf =  Mux(us_whole_reg(fuOpType), 0.U, io.storeIn.bits.uop.vpu.nf)
  val vm = io.storeIn.bits.uop.vpu.vm
  val emul = Mux(us_whole_reg(fuOpType), GenUSWholeEmul(io.storeIn.bits.uop.vpu.nf), Mux(us_mask(fuOpType), 0.U(mulBits.W), EewLog2(eew) - sew + lmul))
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

  /**
    * For uops that store the same vd data, only the first one among these uops contain effective data/src_vs3.
    * Therefore the first uop in a vd should reserve src_vs3 for the incoming uops.
    */
  val vs3Reg = Reg(UInt(VLEN.W))
  val numUopsSameVdLeft = RegInit(0.U(log2Up(maxMUL + 1).W))
  val isNewVd = numUopsSameVdLeft === 0.U
  when (io.storeIn.fire) {
    when (isNewVd) {
      numUopsSameVdLeft := numUopsSameVd - 1.U
      vs3Reg := io.storeIn.bits.src_vs3
    }.otherwise {
      numUopsSameVdLeft := numUopsSameVdLeft - 1.U
      assert(!(numUopsSameVdLeft - 1.U > numUopsSameVdLeft), "Overflow!")
    }
  }
  
  when (io.storeIn.fire && !flushEnq) {
    val id = enqPtr.value
    val preAllocated = preAlloc(id)
    val isSegment = nf =/= 0.U && !us_whole_reg(fuOpType)
    val instType = Cat(isSegment, mop)
    val uopIdx = io.storeIn.bits.uop.vpu.vuopIdx
    val uopIdxInField = GenUopIdxInField(instType, emul, lmul, uopIdx)
    val vdIdxInField = GenVdIdxInField(instType, emul, lmul, uopIdxInField)
    val numFlowsSameVdLog2 = Mux(
      isIndexed(instType),
      log2Up(VLENB).U - sew(1,0),
      log2Up(VLENB).U - eew(1,0)
    )
    val isUsWholeReg = isUnitStride(mop) && us_whole_reg(fuOpType)
    val isMaskReg = isUnitStride(mop) && us_mask(fuOpType)
    val vvl = io.storeIn.bits.src_vl.asTypeOf(VConfig()).vl
    val evl = Mux(isUsWholeReg, GenUSWholeRegVL(io.storeIn.bits.uop.vpu.nf +& 1.U, eew), Mux(isMaskReg, GenUSMaskRegVL(vvl), vvl))
    val vvstart = io.storeIn.bits.uop.vpu.vstart
    val flows = GenRealFlowNum(instType, emul, lmul, eew, sew)
    val flowsLog2 = GenRealFlowLog2(instType, emul, lmul, eew, sew)
    val flowsPrevThisUop = uopIdxInField << flowsLog2 // # of flows before this uop in a field
    val flowsPrevThisVd = vdIdxInField << numFlowsSameVdLog2 // # of flows before this vd in a field
    val flowsIncludeThisUop = (uopIdxInField +& 1.U) << flowsLog2 // # of flows before this uop besides this uop
    val alignedType = Mux(isIndexed(instType), sew(1, 0), eew(1, 0))
    val srcMask = GenFlowMask(Mux(vm, Fill(VLEN, 1.U(1.W)), io.storeIn.bits.src_mask), vvstart, evl, true)
    val flowMask = ((srcMask &
      UIntToMask(flowsIncludeThisUop, VLEN + 1) &
      ~UIntToMask(flowsPrevThisUop, VLEN)
    ) >> flowsPrevThisVd)(VLENB - 1, 0)
    val vlmax = GenVLMAX(lmul, sew)
    valid(id) := true.B
    finish(id) := false.B
    exception(id) := false.B
    vstart(id) := 0.U
    uopq(id) match { case x =>
      x.uop := io.storeIn.bits.uop
      x.uop.vpu.vl := evl
      x.uop.numUops := numUops
      x.uop.lastUop := (uopIdx +& 1.U) === numUops
      x.flowMask := flowMask
      x.byteMask := GenUopByteMask(flowMask, alignedType)(VLENB - 1, 0)
      x.data := Mux(isNewVd, io.storeIn.bits.src_vs3, vs3Reg)
      x.baseAddr := io.storeIn.bits.src_rs1
      x.stride := io.storeIn.bits.src_stride
      x.flow_counter := flows
      x.flowNum := flows
      x.nfields := nf +& 1.U
      x.vm := vm
      x.usWholeReg := isUsWholeReg
      x.usMaskReg := isMaskReg
      x.eew := eew
      x.sew := sew
      x.emul := emul
      x.lmul := lmul
      x.vlmax := Mux(isUsWholeReg, evl, vlmax)
      x.instType := instType
    }

    // Assertion
    assert(PopCount(flows) <= 1.U, "flowNum should be the power of 2")

    // Pre-allocate
    when (!preAllocated) {
      // This is the first uop mapped into the same vd
      enqPtrExt.zipWithIndex.foreach { case (ptr, i) =>
        when (i.U < numUopsSameVd) {
          preAlloc(ptr.value) := true.B
          uopq(ptr.value).vd_last_uop := (i + 1).U === numUopsSameVd
          uopq(ptr.value).vd_first_uop := (i == 0).B
        }
      }
    }
  }

  // update enqPtr
  when (redirectReg.valid && flushNumReg =/= 0.U) {
    enqPtrExt.foreach(ptr => ptr := ptr - flushNumReg)
  }.otherwise {
    when (io.storeIn.fire) {
      enqPtrExt.foreach(ptr => ptr := ptr + 1.U)
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
  val issueMUL = Mux(isIndexed(issueInstType), issueEntry.lmul, issueEntry.emul)
  val issueVLMAXMask = issueEntry.vlmax - 1.U
  val issueIsWholeReg = issueEntry.usWholeReg
  val issueVLMAXLog2 = GenVLMAXLog2(issueEntry.lmul, issueEntry.sew)
  val issueMULMask = LookupTree(issueAlignedType, List(
    "b00".U -> "b01111".U,
    "b01".U -> "b00111".U,
    "b10".U -> "b00011".U,
    "b11".U -> "b00001".U
  ))
  // val issueFieldMask = Mux(
  //   !isSegment(issueInstType) || issueMUL.asSInt >= 0.S,
  //   issueVLMAXMask,
  //   issueMULMask
  // )
  val issueNFIELDS = issueEntry.nfields
  val issueVstart = issueUop.vpu.vstart
  val issueVl = issueUop.vpu.vl
  val issueFlowMask = issueEntry.flowMask
  val issueLmulGreaterThanEmul = issueEntry.lmul.asSInt > issueEntry.emul.asSInt
  assert(!issueValid || PopCount(issueEntry.vlmax) === 1.U, "VLMAX should be power of 2 and non-zero")

  val elemIdxInsideVd = Wire(Vec(flowIssueWidth, UInt(flowIdxBits.W)))
  val packagePortVec = Seq.fill(flowIssueWidth)(Wire(Vec(flowIssueWidth, Bool())))
  val packageByPrePort = Wire(Vec(flowIssueWidth, Bool()))
  (packageByPrePort.zipWithIndex).map{
    case (s, i) => {
      s := packagePortVec(i).reduce(_ | _)
    }
  }

  flowSplitIdx.zip(io.flowIssue).zipWithIndex.foreach { case ((flowIdx, issuePort), portIdx) =>
    // AGU
    // TODO: DONT use * to implement multiplication!!!
    val elemIdx = GenElemIdx(
      instType = issueInstType,
      emul = issueEntry.emul,
      lmul = issueEntry.lmul,
      eew = issueEew,
      sew = issueSew,
      uopIdx = issueUopIdx,
      flowIdx = flowIdx
    ) // elemIdx inside an inst
    val elemIdxInsideField = elemIdx & issueVLMAXMask
    elemIdxInsideVd(portIdx) := elemIdx & Mux(
      issueMUL.asSInt < 0.S,
      issueVLMAXMask,
      issueMULMask
    )// elemIdx inside a vd
    val nfIdx = Mux(issueIsWholeReg, 0.U, elemIdx >> issueVLMAXLog2)
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
    val regOffset = (elemIdxInsideField << issueAlignedType)(vOffsetBits - 1, 0)
    val enable = (issueFlowMask & UIntToOH(elemIdxInsideVd(portIdx))).orR
    val vecActive = enable
     // seek unit-stride package
    val inactiveMask = GenFlowMask(issueFlowMask, issueVstart, issueVl, false) // if elemidx > vl || elemidx < vstart, set 1, and inactive element set 1, active element set 0. 
    val shiftMask = (Mux(enable, issueFlowMask, inactiveMask) >> elemIdxInsideVd(portIdx)).asUInt // if active ,select active mask, else inactive mask
    val packVec = GenPackVec(vaddr, shiftMask, issueEew(1, 0), elemIdxInsideVd(portIdx)) // FIXME
    val packAlignedType = GenPackAlignedType(packVec.asUInt)
    val isPackage = isUnitStride(issueInstType) && !isSegment(issueInstType) && (packAlignedType > issueAlignedType) // don't pack segment unit-stride
    val packageNum = Mux(isPackage, GenPackNum(issueAlignedType, packAlignedType), 1.U)
    val realAlignedType = Mux(isPackage, packAlignedType, issueAlignedType)
    val mask = genVWmask(vaddr ,Mux(isPackage, packAlignedType, issueAlignedType))

    (0 until flowIssueWidth).map{
      case i => {
        if(i > portIdx){
          packagePortVec(i)(portIdx) := (isPackage & !(shiftMask(i) ^ shiftMask(portIdx)))
        }
        else {
          packagePortVec(i)(portIdx) := false.B
        }
      }
    }
    // TODO: delete me later
    dontTouch(elemIdxInsideField)
    dontTouch(enable)
    dontTouch(nfIdx)
    dontTouch(notIndexedStride)
    dontTouch(indexedStride)
    dontTouch(stride)
    dontTouch(fieldOffset)

    issuePort.valid := issueValid && flowIdx < issueFlowNum &&
      !issueUop.robIdx.needFlush(io.redirect) &&
      !issueUop.robIdx.needFlush(redirectReg) && !packageByPrePort(portIdx)
  
    issuePort.bits match { case x =>
      x.uop := issueUop
      x.vaddr := vaddr
      x.mask := mask
      x.alignedType := realAlignedType
      x.vecActive := vecActive
      x.elemIdx := elemIdx
      x.is_first_ele := elemIdx === 0.U
      x.data := GenVSData(
        data = issueEntry.data.asUInt,
        elemIdx = Mux(isPackage, (elemIdxInsideField >> (realAlignedType - issueAlignedType)).asUInt, elemIdxInsideField),
        alignedType = realAlignedType
      )
      x.uopQueuePtr := flowSplitPtr
      x.isLastElem := issueUop.lastUop && (flowIdx +& packageNum) === issueFlowNum
      x.nfields := issueNFIELDS
      x.nSegments := issueEntry.vlmax
      x.fieldIdx := nfIdx
      x.segmentIdx := elemIdxInsideField
      //unit-stride package
      x.isPackage := isPackage
      x.packageNum := packageNum
      x.originAlignedType := issueAlignedType
    }
  }

  // unit-stride package handle
  val numFlowIssue = io.flowIssue.map{
    case (issuePort) => {
      Mux(issuePort.fire, issuePort.bits.packageNum, 0.U(log2Up(VLENB).W))
    }
  }.fold(0.U(log2Up(VLENB).W))(_ + _)
  val flowIssueFire = Cat(io.flowIssue.map(_.fire)).orR

  when (!RegNext(io.redirect.valid) || distanceBetween(enqPtr, flowSplitPtr) > flushNumReg) {
    when (flowSplitIdx.last < (issueFlowNum - numFlowIssue)) {
      // The uop has not been entirly splited yet
      flowSplitIdx.foreach(p => p := p + numFlowIssue)
      // assert(!flowIssueFire || numFlowIssue === flowIssueWidth.U, "both issue port should fire together")
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
    val flowWbElemIdxInField = wb.bits.elemIdx & (entry.vlmax - 1.U)
    val isPackage = wb.bits.isPackage.asBool
    val alignedType = Mux(isIndexed(entry.instType), entry.sew(1, 0), Mux(isPackage, wb.bits.alignedType,entry.eew(1, 0)))

    // handle the situation where multiple ports are going to write the same uop queue entry
    val mergedByPrevPort = (i != 0).B && Cat((0 until i).map(j =>
      io.flowWriteback(j).bits.uopQueuePtr === wb.bits.uopQueuePtr)).orR
    val mergePortVec = (0 until flowWritebackWidth).map(j => (j == i).B ||
      (j > i).B &&
      io.flowWriteback(j).bits.uopQueuePtr === wb.bits.uopQueuePtr &&
      io.flowWriteback(j).valid)
    // writeback packNum, if no package, packageNum=1
    val writebackFlowNum = (io.flowWriteback zip mergePortVec).map{
      case (writebackPort, valid) => {
        Mux(valid, writebackPort.bits.packageNum, 0.U(log2Up(VLENB).W))
      }
    }.fold(0.U(log2Up(VLENB).W))(_ +& _)
    val nextFlowCnt = entry.flow_counter - writebackFlowNum

    // update data and decrease flow_counter when the writeback port is not merged
    when (wb.valid && !mergedByPrevPort) {
      entry.flow_counter := nextFlowCnt
      finish(ptr.value) := nextFlowCnt === 0.U
      when (!exception(ptr.value) && flowWbExcp(i).asUInt.orR) {
        exception(ptr.value) := true.B
        vstart(ptr.value) := flowWbElemIdxInField
        entry.uop.exceptionVec := flowWbExcp(i)
      }
    }

    assert(!(wb.valid && !valid(ptr.value)))
  }

  /**
    * Dequeue according to finish bit pointer by deqPtr
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
    vstart(id) := 0.U

    uopq(id).flowMask := 0.U
    uopq(id).byteMask := 0.U

    deqPtr := deqPtr + 1.U
  }
  assert(!(finish(deqPtr.value) && !valid(deqPtr.value)))

  when (vdState === s_merge) {
    when (deqValid) {
      val id = deqPtr.value
      vdUop := uopq(id).uop
      vdUop.replayInst := vdUop.replayInst || uopq(id).uop.replayInst

      when (!vdException.valid && exception(id)) {
        vdException.valid := true.B
        vdException.bits := uopq(id).uop.exceptionVec
        vdUop.vpu.vstart := vstart(id)
      }

      when (uopq(id).vd_last_uop) {
        vdState := s_wb
      }
    }

    when (vdUop.robIdx.needFlush(io.redirect)) {
      vdException := 0.U.asTypeOf(vdException)
    }
  }

  when (vdState === s_wb) {
    when (io.uopWriteback.ready || vdUop.robIdx.needFlush(io.redirect)) {
      vdException := 0.U.asTypeOf(vdException)
      vdUop.replayInst := false.B

      vdState := s_merge
    }
  }

  // recover entry when redirct
  for (i <- 0 until VsUopSize) {
    when(flushVecReg(i) && redirectReg.valid && flushNumReg =/= 0.U) {
      valid(i) := false.B
      finish(i) := false.B
      preAlloc(i) := false.B
      exception(i) := false.B
    }
  }

  /**
    * IO assignments
    */
  io.storeIn.ready := !full && preAlloc(enqPtr.value) || hasFreeEntries(enqPtr, deqPtr) >= numUopsSameVd

  io.flowWriteback.foreach(_.ready := true.B)

  io.uopWriteback.valid := vdState === s_wb && !vdUop.robIdx.needFlush(io.redirect)
  io.uopWriteback.bits match { case x =>
    x.uop := vdUop
    x.uop.exceptionVec := vdException.bits
    x.data := DontCare
    x.mask.foreach(_ := DontCare)
    x.vdIdx.foreach(_ := DontCare)
    x.vdIdxInField.foreach(_ := DontCare)
    x.debug := DontCare
  }

  for (i <- 1 until flowIssueWidth) {
    assert(!(issueValid && !io.flowIssue(i-1).valid && io.flowIssue(i).valid),
      "flow issue port_(i-1) should have higher priority than port_i")
  }
}