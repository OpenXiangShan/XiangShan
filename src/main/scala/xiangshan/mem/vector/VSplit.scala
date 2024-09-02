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
import utils._
import utility._
import xiangshan.ExceptionNO._
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.Bundles._
import xiangshan.mem._
import xiangshan.backend.fu.vector.Bundles._
import xiangshan.backend.fu.FuConfig._


class VSplitPipeline(isVStore: Boolean = false)(implicit p: Parameters) extends VLSUModule{
  val io = IO(new VSplitPipelineIO(isVStore))
  // will be override later
  def us_whole_reg(fuOpType: UInt): Bool = false.B
  def us_mask(fuOpType: UInt): Bool = false.B
  def us_fof(fuOpType: UInt): Bool = false.B
  //TODO vdIdxReg should no longer be useful, don't delete it for now
  val vdIdxReg = RegInit(0.U(3.W))

  val s1_ready = WireInit(false.B)
  io.in.ready := s1_ready

  /**-----------------------------------------------------------
    * s0 stage
    * decode and generate AlignedType, uop mask, preIsSplit
    * ----------------------------------------------------------
    */
  val s0_uop = io.in.bits.uop
  val s0_vtype = s0_uop.vpu.vtype
  val s0_sew = s0_vtype.vsew
  val s0_eew = s0_uop.vpu.veew
  val s0_lmul = s0_vtype.vlmul
  // when load whole register or unit-stride masked , emul should be 1
  val s0_fuOpType = s0_uop.fuOpType
  val s0_mop = s0_fuOpType(6, 5)
  val s0_nf = Mux(us_whole_reg(s0_fuOpType), 0.U, s0_uop.vpu.nf)
  val s0_vm = s0_uop.vpu.vm
  val s0_emul = Mux(us_whole_reg(s0_fuOpType) ,GenUSWholeEmul(s0_uop.vpu.nf), Mux(us_mask(s0_fuOpType), 0.U(mulBits.W), EewLog2(s0_eew) - s0_sew + s0_lmul))
  val s0_preIsSplit = !(isUnitStride(s0_mop) && !us_fof(s0_fuOpType))
  val s0_nfield        = s0_nf +& 1.U

  val s0_valid         = Wire(Bool())
  val s0_kill          = io.in.bits.uop.robIdx.needFlush(io.redirect)
  val s0_can_go        = s1_ready
  val s0_fire          = s0_valid && s0_can_go
  val s0_out           = Wire(new VLSBundle(isVStore))

  val isUsWholeReg = isUnitStride(s0_mop) && us_whole_reg(s0_fuOpType)
  val isMaskReg = isUnitStride(s0_mop) && us_mask(s0_fuOpType)
  val isSegment = s0_nf =/= 0.U && !us_whole_reg(s0_fuOpType)
  val instType = Cat(isSegment, s0_mop)
  val uopIdx = io.in.bits.uop.vpu.vuopIdx
  val uopIdxInField = GenUopIdxInField(instType, s0_emul, s0_lmul, uopIdx)
  val vdIdxInField = GenVdIdxInField(instType, s0_emul, s0_lmul, uopIdxInField)
  val lmulLog2 = Mux(s0_lmul.asSInt >= 0.S, 0.U, s0_lmul)
  val emulLog2 = Mux(s0_emul.asSInt >= 0.S, 0.U, s0_emul)
  val numEewLog2 = emulLog2 - EewLog2(s0_eew)
  val numSewLog2 = lmulLog2 - s0_sew
  val numFlowsSameVdLog2 = Mux(
    isIndexed(instType),
    log2Up(VLENB).U - s0_sew(1,0),
    log2Up(VLENB).U - s0_eew
  )
  // numUops = nf * max(lmul, emul)
  val lmulLog2Pos = Mux(s0_lmul.asSInt < 0.S, 0.U, s0_lmul)
  val emulLog2Pos = Mux(s0_emul.asSInt < 0.S, 0.U, s0_emul)
  val numUops = Mux(
    isIndexed(s0_mop) && s0_lmul.asSInt > s0_emul.asSInt,
    (s0_nf +& 1.U) << lmulLog2Pos,
    (s0_nf +& 1.U) << emulLog2Pos
  )

  val vvl = io.in.bits.src_vl.asTypeOf(VConfig()).vl
  val evl = Mux(isUsWholeReg,
                GenUSWholeRegVL(io.in.bits.uop.vpu.nf +& 1.U, s0_eew),
                Mux(isMaskReg,
                    GenUSMaskRegVL(vvl),
                    vvl))
  val vvstart = io.in.bits.uop.vpu.vstart
  val alignedType = Mux(isIndexed(instType), s0_sew(1, 0), s0_eew)
  val broadenAligendType = Mux(s0_preIsSplit, Cat("b0".U, alignedType), "b100".U) // if is unit-stride, use 128-bits memory access
  val flowsLog2 = GenRealFlowLog2(instType, s0_emul, s0_lmul, s0_eew, s0_sew)
  val flowsPrevThisUop = (uopIdxInField << flowsLog2).asUInt // # of flows before this uop in a field
  val flowsPrevThisVd = (vdIdxInField << numFlowsSameVdLog2).asUInt // # of flows before this vd in a field
  val flowsIncludeThisUop = ((uopIdxInField +& 1.U) << flowsLog2).asUInt // # of flows before this uop besides this uop
  val flowNum = io.in.bits.flowNum.get
  // max index in vd, only use in index instructions for calculate index
  val maxIdxInVdIndex = GenVLMAX(Mux(s0_emul.asSInt > 0.S, 0.U, s0_emul), s0_eew)
  val indexVlMaxInVd = GenVlMaxMask(maxIdxInVdIndex, elemIdxBits)

  // For vectore indexed  instructions:
  //  When emul is greater than lmul, multiple uop correspond to a Vd, e.g:
  //    vsetvli	t1,t0,e8,m1,ta,ma    lmul = 1
  //    vluxei16.v	v2,(a0),v8       emul = 2
  //    In this case, we need to ensure the flownumis right shift by flowsPrevThisUop, However, the mask passed to mergebuff is right shift by flowsPrevThisVd e.g:
  //      vl = 9
  //      srcMask = 0x1FF
  //      uopIdxInField = 0 and vdIdxInField = 0, flowMask = 0x00FF, toMergeBuffMask = 0x01FF
  //      uopIdxInField = 1 and vdIdxInField = 0, flowMask = 0x0001, toMergeBuffMask = 0x01FF
  //      uopIdxInField = 0 and vdIdxInField = 0, flowMask = 0x0000, toMergeBuffMask = 0x0000
  //      uopIdxInField = 0 and vdIdxInField = 0, flowMask = 0x0000, toMergeBuffMask = 0x0000
  val isSpecialIndexed = isIndexed(instType) && s0_emul.asSInt > s0_lmul.asSInt

  val srcMask = GenFlowMask(Mux(s0_vm, Fill(VLEN, 1.U(1.W)), io.in.bits.src_mask), vvstart, evl, true)
  val srcMaskShiftBits = Mux(isSpecialIndexed, flowsPrevThisUop, flowsPrevThisVd)

  val flowMask = ((srcMask &
    UIntToMask(flowsIncludeThisUop.asUInt, VLEN + 1) &
    (~UIntToMask(flowsPrevThisUop.asUInt, VLEN)).asUInt
  ) >> srcMaskShiftBits)(VLENB - 1, 0)
  val indexedSrcMask = (srcMask >> flowsPrevThisVd).asUInt //only for index instructions

  // Used to calculate the element index.
  // See 'splitbuffer' for 'io.out.splitIdxOffset' and 'mergebuffer' for 'merge data'
  val indexedSplitOffset = Mux(isSpecialIndexed, flowsPrevThisUop - flowsPrevThisVd, 0.U) // only for index instructions of emul > lmul
  val vlmax = GenVLMAX(s0_lmul, s0_sew)

  // connect
  s0_out := DontCare
  s0_out match {case x =>
    x.uop := io.in.bits.uop
    x.uop.vpu.vl := evl
    x.uop.uopIdx := uopIdx
    x.uop.numUops := numUops
    x.uop.lastUop := (uopIdx +& 1.U) === numUops
    x.uop.vpu.nf  := s0_nf
    x.flowMask := flowMask
    x.indexedSrcMask := indexedSrcMask // Only vector indexed instructions uses it
    x.indexedSplitOffset := indexedSplitOffset
    x.byteMask := GenUopByteMask(flowMask, Cat("b0".U, alignedType))(VLENB - 1, 0)
    x.fof := isUnitStride(s0_mop) && us_fof(s0_fuOpType)
    x.baseAddr := io.in.bits.src_rs1
    x.stride := io.in.bits.src_stride
    x.flowNum := flowNum
    x.nfields := s0_nfield
    x.vm := s0_vm
    x.usWholeReg := isUsWholeReg
    x.usMaskReg := isMaskReg
    x.eew := s0_eew
    x.sew := s0_sew
    x.emul := s0_emul
    x.lmul := s0_lmul
    x.vlmax := Mux(isUsWholeReg, evl, vlmax)
    x.instType := instType
    x.data := io.in.bits.src_vs3
    x.vdIdxInField := vdIdxInField
    x.preIsSplit  := s0_preIsSplit
    x.alignedType := broadenAligendType
    x.indexVlMaxInVd := indexVlMaxInVd
  }
  s0_valid := io.in.valid && !s0_kill
  /**-------------------------------------
    * s1 stage
    * ------------------------------------
    * generate UopOffset
    */
  val s1_valid         = RegInit(false.B)
  val s1_kill          = Wire(Bool())
  val s1_in            = Wire(new VLSBundle(isVStore))
  val s1_can_go        = io.out.ready && io.toMergeBuffer.req.ready
  val s1_fire          = s1_valid && !s1_kill && s1_can_go

  s1_ready         := s1_kill || !s1_valid || s1_can_go

  when(s0_fire){
    s1_valid := true.B
  }.elsewhen(s1_fire){
    s1_valid := false.B
  }.elsewhen(s1_kill){
    s1_valid := false.B
  }
  s1_in := RegEnable(s0_out, s0_fire)

  val s1_flowNum          = s1_in.flowNum
  val s1_uop              = s1_in.uop
  val s1_uopidx           = s1_uop.vpu.vuopIdx
  val s1_nf               = s1_uop.vpu.nf
  val s1_nfields          = s1_in.nfields
  val s1_eew              = s1_in.eew
  val s1_emul             = s1_in.emul
  val s1_lmul             = s1_in.lmul
  val s1_instType         = s1_in.instType
  val s1_stride           = s1_in.stride
  val s1_vmask            = FillInterleaved(8, s1_in.byteMask)(VLEN-1, 0)
  val s1_alignedType      = s1_in.alignedType
  val s1_isSpecialIndexed = isIndexed(s1_instType) && s1_emul.asSInt > s1_lmul.asSInt
  val s1_mask             = Mux(s1_isSpecialIndexed, s1_in.indexedSrcMask, s1_in.flowMask)
  val s1_vdIdx            = s1_in.vdIdxInField
  val s1_fof              = s1_in.fof
  val s1_notIndexedStride = Mux( // stride for strided/unit-stride instruction
    isStrided(s1_instType),
    s1_stride(XLEN - 1, 0), // for strided load, stride = x[rs2]
    s1_nfields << s1_eew // for unit-stride load, stride = eew * NFIELDS
  )

  val stride     = Mux(isIndexed(s1_instType), s1_stride, s1_notIndexedStride).asUInt // if is index instructions, get index when split
  val uopOffset  = genVUopOffset(s1_instType, s1_fof, s1_uopidx, s1_nf, s1_eew, stride, s1_alignedType)
  val activeNum  = Mux(s1_in.preIsSplit, PopCount(s1_in.flowMask), s1_flowNum)
  // for Unit-Stride, if uop's addr is aligned with 128-bits, split it to one flow, otherwise split two
  val usLowBitsAddr    = getCheckAddrLowBits(s1_in.baseAddr, maxMemByteNum) + getCheckAddrLowBits(uopOffset, maxMemByteNum)
  val usAligned128     = (getCheckAddrLowBits(usLowBitsAddr, maxMemByteNum) === 0.U)// addr 128-bit aligned
  val usMask           = Cat(0.U(VLENB.W), s1_in.byteMask) << getCheckAddrLowBits(usLowBitsAddr, maxMemByteNum)

  s1_kill               := s1_in.uop.robIdx.needFlush(io.redirect)

  // query mergeBuffer
  io.toMergeBuffer.req.valid             := io.out.ready && s1_valid// only can_go will get MergeBuffer entry
  io.toMergeBuffer.req.bits.flowNum      := activeNum
  io.toMergeBuffer.req.bits.data         := s1_in.data
  io.toMergeBuffer.req.bits.uop          := s1_in.uop
  io.toMergeBuffer.req.bits.mask         := s1_mask
  io.toMergeBuffer.req.bits.vaddr        := DontCare
  io.toMergeBuffer.req.bits.vdIdx        := s1_vdIdx  //TODO vdIdxReg should no longer be useful, don't delete it for now
  io.toMergeBuffer.req.bits.fof          := s1_in.fof
  io.toMergeBuffer.req.bits.vlmax        := s1_in.vlmax
//   io.toMergeBuffer.req.bits.vdOffset :=

  //TODO vdIdxReg should no longer be useful, don't delete it for now
//  when (s1_in.uop.lastUop && s1_fire || s1_kill) {
//    vdIdxReg := 0.U
//  }.elsewhen(s1_fire) {
//    vdIdxReg := vdIdxReg + 1.U
//    XSError(vdIdxReg + 1.U === 0.U, s"Overflow! The number of vd should be less than 8\n")
//  }
  // out connect
  io.out.valid          := s1_valid && io.toMergeBuffer.resp.valid && (activeNum =/= 0.U) // if activeNum == 0, this uop do nothing, can be killed.
  io.out.bits           := s1_in
  io.out.bits.uopOffset := uopOffset
  io.out.bits.stride    := stride
  io.out.bits.mBIndex   := io.toMergeBuffer.resp.bits.mBIndex
  io.out.bits.usLowBitsAddr := usLowBitsAddr
  io.out.bits.usAligned128  := usAligned128
  io.out.bits.usMask        := usMask

  XSPerfAccumulate("split_out",     io.out.fire)
  XSPerfAccumulate("pipe_block",    io.out.valid && !io.out.ready)
  XSPerfAccumulate("mbuffer_block", s1_valid && io.out.ready && !io.toMergeBuffer.resp.valid)
}

abstract class VSplitBuffer(isVStore: Boolean = false)(implicit p: Parameters) extends VLSUModule{
  val io = IO(new VSplitBufferIO(isVStore))
  lazy val fuCfg    = if(isVStore) VstuCfg else VlduCfg

  val uopq          = Reg(new VLSBundle(isVStore))
  val allocated     = RegInit(false.B)
  val needCancel    = WireInit(false.B)
  val activeIssue   = Wire(Bool())
  val inActiveIssue = Wire(Bool())
  val splitFinish   = WireInit(false.B)

  // for split
  val splitIdx = RegInit(0.U(flowIdxBits.W))
  val strideOffsetReg = RegInit(0.U(VLEN.W))

  /**
    * Redirect
    */
  val cancelEnq    = io.in.bits.uop.robIdx.needFlush(io.redirect)
  val canEnqueue   = io.in.valid
  val needEnqueue  = canEnqueue && !cancelEnq

  // enqueue
  val offset    = PopCount(needEnqueue)
  val canAccept = !allocated || allocated && splitFinish && (activeIssue || inActiveIssue) // if is valid entry, need split finish and send last uop
  io.in.ready  := canAccept
  val doEnqueue = canAccept && needEnqueue

  when(doEnqueue){
    uopq := io.in.bits
  }

  //split uops
  val issueValid       = allocated && !needCancel
  val issueEntry       = uopq
  val issueMbIndex     = issueEntry.mBIndex
  val issueFlowNum     = issueEntry.flowNum
  val issueBaseAddr    = issueEntry.baseAddr
  val issueUop         = issueEntry.uop
  val issueUopIdx      = issueUop.vpu.vuopIdx
  val issueInstType    = issueEntry.instType
  val issueUopOffset   = issueEntry.uopOffset
  val issueEew         = issueEntry.eew
  val issueSew         = issueEntry.sew
  val issueLmul        = issueEntry.lmul
  val issueEmul        = issueEntry.emul
  val issueAlignedType = issueEntry.alignedType
  val issuePreIsSplit  = issueEntry.preIsSplit
  val issueByteMask    = issueEntry.byteMask
  val issueUsMask      = issueEntry.usMask
  val issueVLMAXMask   = issueEntry.vlmax - 1.U
  val issueIsWholeReg  = issueEntry.usWholeReg
  val issueVLMAXLog2   = GenVLMAXLog2(issueEntry.lmul, issueSew)
  val issueVlMaxInVd   = issueEntry.indexVlMaxInVd
  val issueUsLowBitsAddr = issueEntry.usLowBitsAddr
  val issueUsAligned128  = issueEntry.usAligned128
  val elemIdx = GenElemIdx(
    instType = issueInstType,
    emul = issueEmul,
    lmul = issueLmul,
    eew = issueEew,
    sew = issueSew,
    uopIdx = issueUopIdx,
    flowIdx = splitIdx
  ) // elemIdx inside an inst, for exception

  val splitIdxOffset = issueEntry.indexedSplitOffset + splitIdx

  val indexFlowInnerIdx = elemIdx & issueVlMaxInVd
  val nfIdx = Mux(issueIsWholeReg, 0.U, elemIdx >> issueVLMAXLog2)
  val fieldOffset = nfIdx << issueAlignedType // field offset inside a segment

  val indexedStride    = IndexAddr( // index for indexed instruction
    index = issueEntry.stride,
    flow_inner_idx = indexFlowInnerIdx,
    eew = issueEew
  )
  val issueStride = Mux(isIndexed(issueInstType), indexedStride, strideOffsetReg)
  val vaddr = issueBaseAddr + issueUopOffset + issueStride
  val mask = genVWmask128(vaddr ,issueAlignedType) // scala maske for flow
  val flowMask = issueEntry.flowMask
  val vecActive = (flowMask & UIntToOH(splitIdx)).orR
  /*
   * Unit-Stride split to one flow or two flow.
   * for Unit-Stride, if uop's addr is aligned with 128-bits, split it to one flow, otherwise split two
   */
  val usSplitMask      = genUSSplitMask(issueUsMask, splitIdx)
  val usMaskInSingleUop = (genUSSplitMask(issueUsMask, 1.U) === 0.U) // if second splited Mask is zero, means this uop is unnecessary to split
  val usNoSplit        = (issueUsAligned128 || usMaskInSingleUop) &&
                          !issuePreIsSplit &&
                          (splitIdx === 0.U)// unit-stride uop don't need to split into two flow
  val usSplitVaddr     = genUSSplitAddr(vaddr, splitIdx)
  val regOffset        = getCheckAddrLowBits(issueUsLowBitsAddr, maxMemByteNum) // offset in 256-bits vd
  XSError((splitIdx > 1.U && usNoSplit) || (splitIdx > 1.U && !issuePreIsSplit) , "Unit-Stride addr split error!\n")

  val addrAligned = LookupTree(issueEew, List(
    "b00".U   -> true.B,                   //b
    "b01".U   -> (issueBaseAddr(0)    === 0.U), //h
    "b10".U   -> (issueBaseAddr(1, 0) === 0.U), //w
    "b11".U   -> (issueBaseAddr(2, 0) === 0.U)  //d
  ))

  // data
  io.out.bits match { case x =>
    x.uop                   := issueUop
    x.uop.exceptionVec      := ExceptionNO.selectByFu(issueUop.exceptionVec, fuCfg)
    x.vaddr                 := Mux(!issuePreIsSplit, usSplitVaddr, vaddr)
    x.alignedType           := issueAlignedType
    x.isvec                 := true.B
    x.mask                  := Mux(!issuePreIsSplit, usSplitMask, mask)
    x.reg_offset            := regOffset //for merge unit-stride data
    x.vecActive             := Mux(!issuePreIsSplit, true.B, vecActive) // currently, unit-stride's flow always send to pipeline
    x.is_first_ele          := DontCare
    x.usSecondInv           := usNoSplit
    x.elemIdx               := elemIdx
    x.elemIdxInsideVd       := splitIdxOffset // if is Unit-Stride, elemIdx is the index of 2 splited mem request (for merge data)
    x.uop_unit_stride_fof   := DontCare
    x.isFirstIssue          := DontCare
    x.mBIndex               := issueMbIndex
  }

  // redirect
  needCancel := uopq.uop.robIdx.needFlush(io.redirect) && allocated

 /* Execute logic */
  /** Issue to scala pipeline**/
  val allowIssue = io.out.ready
  val issueCount = Mux(usNoSplit, 2.U, (PopCount(inActiveIssue) + PopCount(activeIssue))) // for dont need split unit-stride, issue two flow
  splitFinish := splitIdx >= (issueFlowNum - issueCount)

  // handshake
  activeIssue := issueValid && allowIssue && (vecActive || !issuePreIsSplit) // active issue, current use in no unit-stride
  inActiveIssue := issueValid && !vecActive && issuePreIsSplit
  when (!issueEntry.uop.robIdx.needFlush(io.redirect)) {
    when (!splitFinish) {
      when (activeIssue || inActiveIssue) {
        // The uop has not been entirly splited yet
        splitIdx := splitIdx + issueCount
        strideOffsetReg := Mux(!issuePreIsSplit, strideOffsetReg, strideOffsetReg + issueEntry.stride) // when normal unit-stride, don't use strideOffsetReg
      }
    }.otherwise {
      when (activeIssue || inActiveIssue) {
        // The uop is done spliting
        splitIdx := 0.U(flowIdxBits.W) // initialize flowIdx
        strideOffsetReg := 0.U
      }
    }
  }.otherwise {
    splitIdx := 0.U(flowIdxBits.W) // initialize flowIdx
    strideOffsetReg := 0.U
  }
  // allocated
  when(doEnqueue){ // if enqueue need to been cancelled, it will be false, so this have high priority
    allocated := true.B
  }.elsewhen(needCancel) { // redirect
    allocated := false.B
  }.elsewhen(splitFinish && (activeIssue || inActiveIssue)){ //dequeue
    allocated := false.B
  }

  // out connect
  io.out.valid := issueValid && (vecActive || !issuePreIsSplit) // TODO: inactive unit-stride uop do not send to pipeline

  XSPerfAccumulate("out_valid",             io.out.valid)
  XSPerfAccumulate("out_fire",              io.out.fire)
  XSPerfAccumulate("out_fire_unitstride",   io.out.fire && !issuePreIsSplit)
  XSPerfAccumulate("unitstride_vlenAlign",  io.out.fire && !issuePreIsSplit && getCheckAddrLowBits(io.out.bits.vaddr, maxMemByteNum) === 0.U)
  XSPerfAccumulate("unitstride_invalid",    io.out.ready && issueValid && !issuePreIsSplit && PopCount(io.out.bits.mask).orR)
}

class VSSplitBufferImp(implicit p: Parameters) extends VSplitBuffer(isVStore = true){
  // split data
  val splitData = genVSData(
        data = issueEntry.data.asUInt,
        elemIdx = splitIdxOffset,
        alignedType = issueAlignedType
      )
  val flowData = genVWdata(splitData, issueAlignedType)
  val usSplitData      = genUSSplitData(issueEntry.data.asUInt, splitIdx, vaddr(3,0))

  val sqIdx = issueUop.sqIdx + splitIdx
  io.out.bits.uop.sqIdx := sqIdx
  io.out.bits.uop.exceptionVec(storeAddrMisaligned) := !addrAligned && !issuePreIsSplit && io.out.bits.mask.orR

  // send data to sq
  val vstd = io.vstd.get
  vstd.valid := issueValid && (vecActive || !issuePreIsSplit)
  vstd.bits.uop := issueUop
  vstd.bits.uop.sqIdx := sqIdx
  vstd.bits.data := Mux(!issuePreIsSplit, usSplitData, flowData)
  vstd.bits.debug := DontCare
  vstd.bits.vdIdx.get := DontCare
  vstd.bits.vdIdxInField.get := DontCare
  vstd.bits.mask.get := Mux(!issuePreIsSplit, usSplitMask, mask)

}

class VLSplitBufferImp(implicit p: Parameters) extends VSplitBuffer(isVStore = false){
  io.out.bits.uop.lqIdx := issueUop.lqIdx + splitIdx
  io.out.bits.uop.exceptionVec(loadAddrMisaligned) := !addrAligned && !issuePreIsSplit && io.out.bits.mask.orR
}

class VSSplitPipelineImp(implicit p: Parameters) extends VSplitPipeline(isVStore = true){
  override def us_whole_reg(fuOpType: UInt): Bool = fuOpType === VstuType.vsr
  override def us_mask(fuOpType: UInt): Bool      = fuOpType === VstuType.vsm
  override def us_fof(fuOpType: UInt): Bool       = false.B // dont have vector fof store
}

class VLSplitPipelineImp(implicit p: Parameters) extends VSplitPipeline(isVStore = false){

  override def us_whole_reg(fuOpType: UInt): Bool = fuOpType === VlduType.vlr
  override def us_mask(fuOpType: UInt): Bool      = fuOpType === VlduType.vlm
  override def us_fof(fuOpType: UInt): Bool       = fuOpType === VlduType.vleff
}

class VLSplitImp(implicit p: Parameters) extends VLSUModule{
  val io = IO(new VSplitIO(isVStore=false))
  val splitPipeline = Module(new VLSplitPipelineImp())
  val splitBuffer = Module(new VLSplitBufferImp())
  // Split Pipeline
  splitPipeline.io.in <> io.in
  splitPipeline.io.redirect <> io.redirect
  io.toMergeBuffer <> splitPipeline.io.toMergeBuffer

  // skid buffer
  skidBuffer(splitPipeline.io.out, splitBuffer.io.in, splitBuffer.io.in.bits.uop.robIdx.needFlush(io.redirect), "VLSplitSkidBuffer")

  // Split Buffer
  splitBuffer.io.redirect <> io.redirect
  io.out <> splitBuffer.io.out
}

class VSSplitImp(implicit p: Parameters) extends VLSUModule{
  val io = IO(new VSplitIO(isVStore=true))
  val splitPipeline = Module(new VSSplitPipelineImp())
  val splitBuffer = Module(new VSSplitBufferImp())
  // Split Pipeline
  splitPipeline.io.in <> io.in
  splitPipeline.io.redirect <> io.redirect
  io.toMergeBuffer <> splitPipeline.io.toMergeBuffer

  // skid buffer
  skidBuffer(splitPipeline.io.out, splitBuffer.io.in, splitBuffer.io.in.bits.uop.robIdx.needFlush(io.redirect),"VSSplitSkidBuffer")

  // Split Buffer
  splitBuffer.io.redirect <> io.redirect
  io.out <> splitBuffer.io.out
  io.vstd.get <> splitBuffer.io.vstd.get
}

