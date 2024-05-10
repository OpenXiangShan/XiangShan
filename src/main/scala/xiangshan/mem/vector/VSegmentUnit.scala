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
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.Bundles._
import xiangshan.mem._
import xiangshan.backend.fu.FuType
import freechips.rocketchip.diplomacy.BufferParams
import xiangshan.cache.mmu._
import xiangshan.cache._
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.backend.fu.util.SdtrigExt
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.vector.Bundles.VConfig
import xiangshan.backend.fu.vector.Utils.VecDataToMaskDataVec

class VSegmentBundle(implicit p: Parameters) extends VLSUBundle
{
  val vaddr            = UInt(VAddrBits.W)
  val uop              = new DynInst
  val paddr            = UInt(PAddrBits.W)
  val mask             = UInt(VLEN.W)
  val valid            = Bool()
  val alignedType      = UInt(alignTypeBits.W)
  val vl               = UInt(elemIdxBits.W)
  val vlmaxInVd        = UInt(elemIdxBits.W)
  val vlmaxMaskInVd    = UInt(elemIdxBits.W)
  // for exception
  val vstart           = UInt(elemIdxBits.W)
  val exceptionvaddr   = UInt(VAddrBits.W)
  val exception_va     = Bool()
  val exception_pa     = Bool()
}

class VSegmentUnit (implicit p: Parameters) extends VLSUModule
  with HasDCacheParameters
  with MemoryOpConstants
  with SdtrigExt
  with HasLoadHelper
{
  val io               = IO(new VSegmentUnitIO)

  val maxSize          = VSegmentBufferSize

  class VSegUPtr(implicit p: Parameters) extends CircularQueuePtr[VSegUPtr](maxSize){
  }

  object VSegUPtr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): VSegUPtr = {
      val ptr           = Wire(new VSegUPtr)
      ptr.flag         := f
      ptr.value        := v
      ptr
    }
  }

  // buffer uop
  val instMicroOp       = Reg(new VSegmentBundle)
  val data              = Reg(Vec(maxSize, UInt(VLEN.W)))
  val pdest             = Reg(Vec(maxSize, UInt(PhyRegIdxWidth.W)))
  val uopIdx            = Reg(Vec(maxSize, UopIdx()))
  val stride            = Reg(Vec(maxSize, UInt(VLEN.W)))
  val allocated         = RegInit(VecInit(Seq.fill(maxSize)(false.B)))
  val enqPtr            = RegInit(0.U.asTypeOf(new VSegUPtr))
  val deqPtr            = RegInit(0.U.asTypeOf(new VSegUPtr))
  val stridePtr         = WireInit(0.U.asTypeOf(new VSegUPtr)) // for select stride/index

  val segmentIdx        = RegInit(0.U(elemIdxBits.W))
  val fieldIdx          = RegInit(0.U(fieldBits.W))
  val segmentOffset     = RegInit(0.U(VAddrBits.W))
  val splitPtr          = RegInit(0.U.asTypeOf(new VSegUPtr)) // for select load/store data
  val splitPtrNext      = WireInit(0.U.asTypeOf(new VSegUPtr))

  val exception_va      = WireInit(false.B)
  val exception_pa      = WireInit(false.B)

  val maxSegIdx         = instMicroOp.vl - 1.U
  val maxNfields        = instMicroOp.uop.vpu.nf

  XSError(segmentIdx > maxSegIdx, s"segmentIdx > vl, something error!\n")
  XSError(fieldIdx > maxNfields, s"fieldIdx > nfields, something error!\n")

  // MicroOp
  val baseVaddr                       = instMicroOp.vaddr
  val alignedType                     = instMicroOp.alignedType
  val fuType                          = instMicroOp.uop.fuType
  val mask                            = instMicroOp.mask
  val exceptionVec                    = instMicroOp.uop.exceptionVec
  val issueEew                        = instMicroOp.uop.vpu.veew
  val issueLmul                       = instMicroOp.uop.vpu.vtype.vlmul
  val issueSew                        = instMicroOp.uop.vpu.vtype.vsew
  val issueEmul                       = EewLog2(issueEew) - issueSew + issueLmul
  val elemIdxInVd                     = segmentIdx & instMicroOp.vlmaxMaskInVd
  val issueInstType                   = Cat(true.B, instMicroOp.uop.fuOpType(6, 5)) // always segment instruction
  val issueVLMAXLog2                  = GenVLMAXLog2(
    Mux(issueLmul.asSInt > 0.S, 0.U, issueLmul),
    Mux(isIndexed(issueInstType), issueSew(1, 0), issueEew(1, 0))
  ) // max element number log2 in vd
  val issueVlMax                      = instMicroOp.vlmaxInVd // max elementIdx in vd
  val issueMaxIdxInIndex              = GenVLMAX(Mux(issueEmul.asSInt > 0.S, 0.U, issueEmul), issueEew) // index element index in index register
  val issueMaxIdxInIndexMask          = UIntToMask(issueMaxIdxInIndex, elemIdxBits)
  val issueMaxIdxInIndexLog2          = GenVLMAXLog2(Mux(issueEmul.asSInt > 0.S, 0.U, issueEmul), issueEew)
  val issueIndexIdx                   = segmentIdx & issueMaxIdxInIndexMask
  val segmentActive                   = (mask & UIntToOH(elemIdxInVd)).orR

  // Segment instruction's FSM
  /*
  * s_idle: wait request
  * s_flush_sbuffer_req: flush sbuffer
  * s_wait_flush_sbuffer_resp: wait sbuffer empty
  * s_tlb_req: request tlb
  * s_wait_tlb_resp: wait tlb resp
  * s_pm: check pmp
  * s_cache_req: request cache
  * s_cache_resp: wait cache resp
  * s_latch_and_merge_data: for read data
  * s_send_data: for send write data
  * s_finish:
  * */
  val s_idle :: s_flush_sbuffer_req :: s_wait_flush_sbuffer_resp :: s_tlb_req :: s_wait_tlb_resp :: s_pm ::s_cache_req :: s_cache_resp :: s_latch_and_merge_data :: s_send_data :: s_finish :: Nil = Enum(11)
  val state             = RegInit(s_idle)
  val stateNext         = WireInit(s_idle)
  val sbufferEmpty      = io.flush_sbuffer.empty

  /**
   * state update
   */
  state  := stateNext

  /**
   * state transfer
   */
  when(state === s_idle){
    stateNext := Mux(isAfter(enqPtr, deqPtr), s_flush_sbuffer_req, s_idle)
  }.elsewhen(state === s_flush_sbuffer_req){
    stateNext := Mux(sbufferEmpty, s_tlb_req, s_wait_flush_sbuffer_resp) // if sbuffer is empty, go to query tlb

  }.elsewhen(state === s_wait_flush_sbuffer_resp){
    stateNext := Mux(sbufferEmpty, s_tlb_req, s_wait_flush_sbuffer_resp)

  }.elsewhen(state === s_tlb_req){
    stateNext := Mux(segmentActive, s_wait_tlb_resp, Mux(FuType.isVLoad(instMicroOp.uop.fuType), s_latch_and_merge_data, s_send_data))

  }.elsewhen(state === s_wait_tlb_resp){
    stateNext := Mux(!io.dtlb.resp.bits.miss && io.dtlb.resp.fire, s_pm, s_tlb_req)

  }.elsewhen(state === s_pm){
    /* if is vStore, send data to sbuffer, so don't need query dcache */
    stateNext := Mux(exception_pa || exception_va,
                     s_finish,
                     Mux(FuType.isVLoad(instMicroOp.uop.fuType), s_cache_req, s_send_data))

  }.elsewhen(state === s_cache_req){
    stateNext := Mux(io.rdcache.req.fire, s_cache_resp, s_cache_req)

  }.elsewhen(state === s_cache_resp){
    when(io.rdcache.resp.fire) {
      when(io.rdcache.resp.bits.miss) {
        stateNext := s_cache_req
      }.otherwise {
        stateNext := Mux(FuType.isVLoad(instMicroOp.uop.fuType), s_latch_and_merge_data, s_send_data)
      }
    }.otherwise{
      stateNext := s_cache_resp
    }

  }.elsewhen(state === s_latch_and_merge_data) {
    when((segmentIdx === maxSegIdx) && (fieldIdx === maxNfields)) {
      stateNext := s_finish // segment instruction finish
    }.otherwise {
      stateNext := s_tlb_req // need continue
    }

  }.elsewhen(state === s_send_data) { // when sbuffer accept data
    when(!io.sbuffer.fire && segmentActive) {
      stateNext := s_send_data
    }.elsewhen((segmentIdx === maxSegIdx) && (fieldIdx === maxNfields)) {
      stateNext := s_finish // segment instruction finish
    }.otherwise {
      stateNext := s_tlb_req // need continue
    }
  }.elsewhen(state === s_finish){ // writeback uop
    stateNext := Mux(distanceBetween(enqPtr, deqPtr) === 0.U, s_idle, s_finish)

  }.otherwise{
    stateNext := s_idle
    XSError(true.B, s"Unknown state!\n")
  }

  /*************************************************************************
   *                            enqueue logic
   *************************************************************************/
  io.in.ready                         := true.B
  val fuOpType                         = io.in.bits.uop.fuOpType
  val vtype                            = io.in.bits.uop.vpu.vtype
  val mop                              = fuOpType(6, 5)
  val instType                         = Cat(true.B, mop)
  val eew                              = io.in.bits.uop.vpu.veew
  val sew                              = vtype.vsew
  val lmul                             = vtype.vlmul
  val vl                               = instMicroOp.vl
  val vm                               = instMicroOp.uop.vpu.vm
  val vstart                           = instMicroOp.uop.vpu.vstart
  val srcMask                          = GenFlowMask(Mux(vm, Fill(VLEN, 1.U(1.W)), io.in.bits.src_mask), vstart, vl, true)
  // first uop enqueue, we need to latch microOp of segment instruction
  when(io.in.fire && !instMicroOp.valid){
    val vlmaxInVd                      = GenVLMAX(Mux(lmul.asSInt > 0.S, 0.U, lmul), Mux(isIndexed(instType), sew(1, 0), eew(1, 0))) // element number in a vd
    instMicroOp.vaddr                 := io.in.bits.src_rs1(VAddrBits - 1, 0)
    instMicroOp.valid                 := true.B // if is first uop
    instMicroOp.alignedType           := Mux(isIndexed(instType), sew(1, 0), eew(1, 0))
    instMicroOp.uop                   := io.in.bits.uop
    instMicroOp.mask                  := srcMask
    instMicroOp.vstart                := 0.U
    instMicroOp.vlmaxInVd             := vlmaxInVd
    instMicroOp.vlmaxMaskInVd         := UIntToMask(vlmaxInVd, elemIdxBits) // for merge data
    instMicroOp.vl                    := io.in.bits.src_vl.asTypeOf(VConfig()).vl
    segmentOffset                     := 0.U
  }
  // latch data
  when(io.in.fire){
    data(enqPtr.value)                := io.in.bits.src_vs3
    stride(enqPtr.value)              := io.in.bits.src_stride
    uopIdx(enqPtr.value)              := io.in.bits.uop.vpu.vuopIdx
    pdest(enqPtr.value)               := io.in.bits.uop.pdest
  }

  // update enqptr, only 1 port
  when(io.in.fire){
    enqPtr                            := enqPtr + 1.U
  }

  /*************************************************************************
   *                            output logic
   *************************************************************************/

  val indexStride                     = IndexAddr( // index for indexed instruction
                                                    index = stride(stridePtr.value),
                                                    flow_inner_idx = issueIndexIdx,
                                                    eew = issueEew
                                                  )
  val realSegmentOffset               = Mux(isIndexed(issueInstType),
                                            indexStride,
                                            segmentOffset)
  val vaddr                           = baseVaddr + (fieldIdx << alignedType).asUInt + realSegmentOffset
  /**
   * tlb req and tlb resq
   */

  // query DTLB IO Assign
  io.dtlb.req                         := DontCare
  io.dtlb.resp.ready                  := true.B
  io.dtlb.req.valid                   := state === s_tlb_req && segmentActive
  io.dtlb.req.bits.cmd                := Mux(FuType.isVLoad(fuType), TlbCmd.read, TlbCmd.write)
  io.dtlb.req.bits.vaddr              := vaddr
  io.dtlb.req.bits.size               := instMicroOp.alignedType(2,0)
  io.dtlb.req.bits.memidx.is_ld       := FuType.isVLoad(fuType)
  io.dtlb.req.bits.memidx.is_st       := FuType.isVStore(fuType)
  io.dtlb.req.bits.debug.robIdx       := instMicroOp.uop.robIdx
  io.dtlb.req.bits.no_translate       := false.B
  io.dtlb.req.bits.debug.pc           := instMicroOp.uop.pc
  io.dtlb.req.bits.debug.isFirstIssue := DontCare
  io.dtlb.req_kill                    := false.B

  // tlb resp
  when(io.dtlb.resp.fire && state === s_wait_tlb_resp){
      exceptionVec(storePageFault)    := io.dtlb.resp.bits.excp(0).pf.st
      exceptionVec(loadPageFault)     := io.dtlb.resp.bits.excp(0).pf.ld
      exceptionVec(storeAccessFault)  := io.dtlb.resp.bits.excp(0).af.st
      exceptionVec(loadAccessFault)   := io.dtlb.resp.bits.excp(0).af.ld
      when(!io.dtlb.resp.bits.miss){
        instMicroOp.paddr             := io.dtlb.resp.bits.paddr(0)
      }
  }
  // pmp
  // NOTE: only handle load/store exception here, if other exception happens, don't send here
  val pmp = WireInit(io.pmpResp)
  when(state === s_pm){
    exception_va := exceptionVec(storePageFault) || exceptionVec(loadPageFault) ||
    exceptionVec(storeAccessFault) || exceptionVec(loadAccessFault)
    exception_pa := pmp.st || pmp.ld

    instMicroOp.exception_pa       := exception_pa
    instMicroOp.exception_va       := exception_va
    // update storeAccessFault bit
    exceptionVec(loadAccessFault)  := exceptionVec(loadAccessFault) || pmp.ld
    exceptionVec(storeAccessFault) := exceptionVec(storeAccessFault) || pmp.st

    when(exception_va || exception_pa){
      instMicroOp.exceptionvaddr     := vaddr
      instMicroOp.vl                 := segmentIdx // for exception
      instMicroOp.vstart             := segmentIdx // for exception
    }
  }

  /**
   * flush sbuffer IO Assign
   */
  io.flush_sbuffer.valid           := !sbufferEmpty && (state === s_flush_sbuffer_req)


  /**
   * merge data for load
   */
  val cacheData = LookupTree(vaddr(3,0), List(
    "b0000".U -> io.rdcache.resp.bits.data_delayed(63,    0),
    "b0001".U -> io.rdcache.resp.bits.data_delayed(63,    8),
    "b0010".U -> io.rdcache.resp.bits.data_delayed(63,   16),
    "b0011".U -> io.rdcache.resp.bits.data_delayed(63,   24),
    "b0100".U -> io.rdcache.resp.bits.data_delayed(63,   32),
    "b0101".U -> io.rdcache.resp.bits.data_delayed(63,   40),
    "b0110".U -> io.rdcache.resp.bits.data_delayed(63,   48),
    "b0111".U -> io.rdcache.resp.bits.data_delayed(63,   56),
    "b1000".U -> io.rdcache.resp.bits.data_delayed(127,  64),
    "b1001".U -> io.rdcache.resp.bits.data_delayed(127,  72),
    "b1010".U -> io.rdcache.resp.bits.data_delayed(127,  80),
    "b1011".U -> io.rdcache.resp.bits.data_delayed(127,  88),
    "b1100".U -> io.rdcache.resp.bits.data_delayed(127,  96),
    "b1101".U -> io.rdcache.resp.bits.data_delayed(127, 104),
    "b1110".U -> io.rdcache.resp.bits.data_delayed(127, 112),
    "b1111".U -> io.rdcache.resp.bits.data_delayed(127, 120)
  ))
  val pickData  = rdataVecHelper(alignedType(1,0), cacheData)
  val mergedData = mergeDataWithElemIdx(
    oldData = data(splitPtr.value),
    newData = Seq(pickData),
    alignedType = alignedType(1,0),
    elemIdx = Seq(elemIdxInVd),
    valids = Seq(true.B)
  )
  when(state === s_latch_and_merge_data && segmentActive){
    data(splitPtr.value) := mergedData
  }
  /**
   * split data for store
   * */
  val splitData = genVSData(
    data = data(splitPtr.value),
    elemIdx = elemIdxInVd,
    alignedType = alignedType
  )
  val flowData  = genVWdata(splitData, alignedType) // TODO: connect vstd, pass vector data
  val wmask     = genVWmask(vaddr, alignedType(1, 0)) & Fill(VLENB, segmentActive)

  /**
   * rdcache req, write request don't need to query dcache, because we write element to sbuffer
   */
  io.rdcache.req                    := DontCare
  io.rdcache.req.valid              := state === s_cache_req && FuType.isVLoad(fuType)
  io.rdcache.req.bits.cmd           := MemoryOpConstants.M_XRD
  io.rdcache.req.bits.vaddr         := vaddr
  io.rdcache.req.bits.mask          := mask
  io.rdcache.req.bits.data          := flowData
  io.rdcache.pf_source              := LOAD_SOURCE.U
  io.rdcache.req.bits.id            := DontCare
  io.rdcache.resp.ready             := true.B
  io.rdcache.s1_paddr_dup_lsu       := instMicroOp.paddr
  io.rdcache.s1_paddr_dup_dcache    := instMicroOp.paddr
  io.rdcache.s1_kill                := false.B
  io.rdcache.s2_kill                := false.B
  if (env.FPGAPlatform){
    io.rdcache.s0_pc                := DontCare
    io.rdcache.s1_pc                := DontCare
    io.rdcache.s2_pc                := DontCare
  }else{
    io.rdcache.s0_pc                := instMicroOp.uop.pc
    io.rdcache.s1_pc                := instMicroOp.uop.pc
    io.rdcache.s2_pc                := instMicroOp.uop.pc
  }
  io.rdcache.replacementUpdated     := false.B
  io.rdcache.is128Req               := false.B


  /**
   * write data to sbuffer
   * */

  io.sbuffer.bits                  := DontCare
  io.sbuffer.valid                 := state === s_send_data && segmentActive
  io.sbuffer.bits.vecValid         := state === s_send_data && segmentActive
  io.sbuffer.bits.mask             := wmask
  io.sbuffer.bits.data             := flowData
  io.sbuffer.bits.vaddr            := vaddr
  io.sbuffer.bits.cmd              := MemoryOpConstants.M_XWR
  io.sbuffer.bits.id               := DontCare
  io.sbuffer.bits.addr             := instMicroOp.paddr

  /**
   * update ptr
   * */

  val splitPtrOffset = Mux(lmul.asSInt < 0.S, 1.U, (1.U << lmul).asUInt)
  splitPtrNext :=
    Mux(fieldIdx === maxNfields,
     (deqPtr + ((segmentIdx +& 1.U) >> issueVLMAXLog2).asUInt), // segment finish
     (splitPtr + splitPtrOffset)) // next field
  dontTouch(issueVLMAXLog2)
  dontTouch(splitPtrNext)
  dontTouch(stridePtr)

  // update splitPtr
  when(state === s_latch_and_merge_data || state === s_send_data){
    splitPtr := splitPtrNext
  }.elsewhen(io.in.fire && !instMicroOp.valid){
    splitPtr := deqPtr // initial splitPtr
  }

  // update stridePtr, only use in index
  val strideOffset = Mux(isIndexed(issueInstType), (segmentIdx +& 1.U) >> issueMaxIdxInIndexLog2, 0.U)
  stridePtr       := deqPtr + strideOffset

  // update fieldIdx
  when(io.in.fire && !instMicroOp.valid){
    fieldIdx := 0.U
  }.elsewhen(fieldIdx === maxNfields && (state === s_latch_and_merge_data || state === s_send_data)){
    fieldIdx := 0.U
  }.elsewhen((state === s_latch_and_merge_data || state === s_send_data)){
    fieldIdx := fieldIdx + 1.U
  }.elsewhen(!segmentActive){ // if segment is inactive, pass segment
    fieldIdx := maxNfields
  }
  //update segmentIdx
  when(io.in.fire && !instMicroOp.valid){
    segmentIdx := 0.U
  }.elsewhen(fieldIdx === maxNfields && (state === s_latch_and_merge_data || state === s_send_data) && segmentIdx =/= maxSegIdx){
    segmentIdx := segmentIdx + 1.U
  }

  //update segmentOffset
  when(fieldIdx === maxNfields && (state === s_latch_and_merge_data || state === s_send_data)){
    segmentOffset := segmentOffset + Mux(isUnitStride(issueInstType), (maxNfields +& 1.U) << issueEew, stride(stridePtr.value))
  }

  //update deqPtr
  when(io.uopwriteback.fire){
    deqPtr := deqPtr + 1.U
  }

  /*************************************************************************
   *                            dequeue logic
   *************************************************************************/
  val uopIdxInField = GenUopIdxInField(instType, issueEmul, issueLmul, uopIdx(deqPtr.value))
  val vdIdxInField  = GenVdIdxInField(instType, issueEmul, issueLmul, uopIdxInField) // for merge oldvd
  /*select mask of vd, maybe remove in feature*/
  val realEw        = Mux(isIndexed(issueInstType), issueSew(1, 0), issueEew(1, 0))
  val maskDataVec: Vec[UInt] = VecDataToMaskDataVec(instMicroOp.mask, realEw)
  val maskUsed      = maskDataVec(vdIdxInField)

  when(stateNext === s_idle){
    instMicroOp.valid := false.B
  }
  io.uopwriteback.valid               := (state === s_finish) && distanceBetween(enqPtr, deqPtr) =/= 0.U
  io.uopwriteback.bits.uop            := instMicroOp.uop
  io.uopwriteback.bits.mask.get       := instMicroOp.mask
  io.uopwriteback.bits.data           := data(deqPtr.value)
  io.uopwriteback.bits.vdIdx.get      := vdIdxInField
  io.uopwriteback.bits.uop.vpu.vl     := instMicroOp.vl
  io.uopwriteback.bits.uop.vpu.vstart := instMicroOp.vstart
  io.uopwriteback.bits.uop.vpu.vmask  := maskUsed
  io.uopwriteback.bits.uop.pdest      := pdest(deqPtr.value)
  io.uopwriteback.bits.debug          := DontCare
  io.uopwriteback.bits.vdIdxInField.get := DontCare

  //to RS
  io.feedback.valid                   := state === s_finish
  io.feedback.bits.hit                := true.B
  io.feedback.bits.robIdx             := instMicroOp.uop.robIdx
  io.feedback.bits.sourceType         := DontCare
  io.feedback.bits.flushState         := DontCare
  io.feedback.bits.dataInvalidSqIdx   := DontCare
  io.feedback.bits.uopIdx.get         := uopIdx(deqPtr.value)

  // exception
  io.exceptionAddr                    := DontCare // TODO: fix it when handle exception
}

