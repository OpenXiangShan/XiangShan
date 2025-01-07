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
import xiangshan.ExceptionNO._
import xiangshan.backend.fu._
import xiangshan.mem.ReplayCauseNO._
import xiangshan.mem.Bundles._
import xiangshan.cache._
import xiangshan.cache.mmu._

class ExceptionGenIO(implicit p: Parameters) extends XSBundle {
  val fromCtrl = new Bundle() {
    val csr = Flipped(new CustomCSRCtrlIO)
  }

  val s0_in = Flipped(ValidIO(new LsPipelineBundle))
  val s1_in = Flipped(ValidIO(new LsPipelineBundle))
  val s2_in = Flipped(ValidIO(new LsPipelineBundle))
  val s3_in = Flipped(ValidIO(new LsPipelineBundle))

  // from
  val fromTlb = Flipped(ValidIO(new TlbResp(2)))
  val fromPmp = Flipped(new PMPRespBundle())
  val fromDCache = Input(new Bundle() {
    val error_delayed = Bool()
  })
  val fromTrigger = Input(new Bundle() {
    val breakPoint = Bool()
  })

  // common out
  val commonOut = Output(new Bundle() {
    val s0_misalign = Bool()
    val s0_misalignWith16Bytes = Bool()
    val s0_isFinalSplit = Bool()
    val s0_exceptionVecOut = ExceptionVec()

    val s1_misalign = Bool()
    val s1_exceptionVecOut = ExceptionVec()

    val s2_exceptionVecOut = ExceptionVec()
    val s3_exceptionVecOut = ExceptionVec()
  })
}

class ExceptionGen(implicit p: Parameters, params: MemUnitParams) extends XSModule
  with HasCircularQueuePtrHelper {

  private def exceptionCatched(exceptionVec: Vec[Bool], exceptionNO: Int) = {
    if (params.exceptionOut.contains(exceptionNO)) {
      exceptionVec(exceptionNO)
    } else {
      false.B
    }
  }

  private def catchException(exceptionVec: Vec[Bool], exceptionNO: Int, set: Bool) = {
    if (params.exceptionOut.contains(exceptionNO)) {
      exceptionVec(exceptionNO) := set
    }
  }

  private def clearException(exceptionVec: Vec[Bool], exceptionNo: Int) = {
    catchException(exceptionVec, exceptionNo, false.B)
  }

  private def clearAllExceptions(exceptionVec: Vec[Bool]) = {
    exceptionVec.zipWithIndex.filter(no => params.exceptionOut.contains(no._2)).foreach {
      case exception =>
        exception._1 := false.B
    }
  }

  val io = IO(new ExceptionGenIO).suggestName("io")

  // Pipeline
  // -------------------------------------------------------------------
  // stage 0
  // -------------------------------------------------------------------
  val s0_exceptionVecOut = WireInit(io.s0_in.bits.uop.exceptionVec)

  /**
    * Determine the alignment type based on whether the uop is
    * vector or scalar. If vector, use the `alignType` field; otherwise,
    * use the `fuOpType` field.
    */
  val s0_alignType = Mux(io.s0_in.bits.isVector, io.s0_in.bits.alignType(1, 0), io.s0_in.bits.uop.fuOpType(1, 0))

  /**
    * Determine whether the address is aligned based on the determined alignment type.
    */
  val s0_addrAligned = LookupTree(s0_alignType, List(
    "b00".U   -> true.B,                        // b
    "b01".U   -> (io.s0_in.bits.vaddr(0)    === 0.U), // h
    "b10".U   -> (io.s0_in.bits.vaddr(1, 0) === 0.U), // w
    "b11".U   -> (io.s0_in.bits.vaddr(2, 0) === 0.U)  // d
  ))
  XSError(io.s0_in.bits.isVector && io.s0_in.bits.vaddr(3, 0) =/= 0.U && io.s0_in.bits.alignType(2),
    "unit-stride 128 bit element is not aligned!")

  /**
    * Extract the lower 5 bits of the virtual address for alignment
    * checks.
    */
  val s0_checkVAddrLow = io.s0_in.bits.vaddr(4, 0)

  /**
    * Determine the upper alignment offset based on the alignment type.
    * LookupTree maps alignment types to specific offsets. Add the base
    * lower address bits to calculate the effective upper address bits
    */
  val s0_checkVAddrUpLow = LookupTree(s0_alignType, List(
    "b00".U -> 0.U,
    "b01".U -> 1.U,
    "b10".U -> 3.U,
    "b11".U -> 7.U
  )) + s0_checkVAddrLow

  /**
    * Determine whether the calculated upper address crosses a 16-byte boundary,
    * which determined by comparing the 4th bit of the upper and lower addresses
    */
  val s0_iqCross16Bytes = s0_checkVAddrUpLow(4) =/= s0_checkVAddrLow(4)
  val s0_misalignWith16Bytes = !s0_iqCross16Bytes && !s0_addrAligned && !io.s0_in.bits.isHWPrefetch
  val s0_isFinalSplit = io.s0_in.bits.isMisalignBuf && io.s0_in.bits.isFinalSplit

  /**
    * General check for misalignment, which is true if the address is not aligned
    * or there is a load/store misalignment exception, and vector operations are active
    */
  val s0_misalign = (!s0_addrAligned ||
    (io.s0_in.bits.uop.exceptionVec(loadAddrMisaligned) ||
    io.s0_in.bits.uop.exceptionVec(storeAddrMisaligned)) && !io.s0_in.bits.misalignWith16Bytes) && io.s0_in.bits.vecActive

  /**
    * Specific check for load address misalignment, which is a load, the
    * address is not aligned, or there is a load misalignment exception,
    * requires vector to be active and not to involve a
    * misalignment within 16 bytes
    */
  val s0_loadAddrMisaligned = (!s0_addrAligned || io.s0_in.bits.uop.exceptionVec(loadAddrMisaligned)) &&
    io.s0_in.bits.vecActive && !s0_misalignWith16Bytes && io.s0_in.bits.isLoad

  catchException(s0_exceptionVecOut, loadAddrMisaligned, s0_loadAddrMisaligned)

  //
  io.commonOut.s0_misalign := s0_misalign
  io.commonOut.s0_misalignWith16Bytes := s0_misalignWith16Bytes
  io.commonOut.s0_isFinalSplit := s0_isFinalSplit
  io.commonOut.s0_exceptionVecOut := s0_exceptionVecOut

  // Pipeline
  // -------------------------------------------------------------------
  // stage 1
  // -------------------------------------------------------------------
  val s1_exceptionVecIn = RegEnable(s0_exceptionVecOut, 0.U.asTypeOf(s0_exceptionVecOut), io.s0_in.valid)
  val s1_exceptionVecOut = WireInit(s1_exceptionVecIn)
  val s1_tlbRealHit = !io.s1_in.bits.tlbNoQuery && io.fromTlb.valid && !io.fromTlb.bits.miss
  val s1_misalign = WireInit(io.s1_in.bits.misalign)

  // set load exception if need.
  when (io.s1_in.bits.isLoad) {
    when (!io.s1_in.bits.delayedError) {
      // set load page fault
      val s1_loadPageFault = io.fromTlb.bits.excp(0).pf.ld && s1_tlbRealHit && io.s1_in.bits.vecActive
      catchException(s1_exceptionVecOut, loadPageFault, s1_loadPageFault)

      // set load guest page fault
      val s1_loadGuestPageFault = io.fromTlb.bits.excp(0).gpf.ld && s1_tlbRealHit && io.s1_in.bits.vecActive
      catchException(s1_exceptionVecOut, loadGuestPageFault, s1_loadGuestPageFault)

      // set load access fault
      val s1_loadAccessFault = io.fromTlb.bits.excp(0).af.ld && s1_tlbRealHit && io.s1_in.bits.vecActive
      catchException(s1_exceptionVecOut, loadAccessFault, s1_loadAccessFault)

      val s1_hasLoadFaultException = exceptionCatched(s1_exceptionVecOut, loadPageFault) ||
        exceptionCatched(s1_exceptionVecOut, loadGuestPageFault) ||
        exceptionCatched(s1_exceptionVecOut, loadAccessFault)

      when (io.s1_in.bits.checkFullVA && s1_hasLoadFaultException) {
        clearException(s1_exceptionVecOut, loadAddrMisaligned)
        s1_misalign := false.B
      }
    }  .otherwise {
      clearException(s1_exceptionVecOut, loadAddrMisaligned)
      clearException(s1_exceptionVecOut, loadPageFault)
      clearException(s1_exceptionVecOut, loadGuestPageFault)
      clearException(s1_exceptionVecOut, hardwareError)
      catchException(s1_exceptionVecOut, loadAccessFault, io.s1_in.bits.vecActive)
      s1_misalign := false.B
    }
  }

  // set store exception if need.
  when (io.s1_in.bits.isStore) {
    // set store addr misalign
    val s1_storeAddrMisaligned = io.s1_in.bits.mmio && io.s1_in.bits.misalign
    catchException(s1_exceptionVecOut, storeAddrMisaligned, s1_storeAddrMisaligned)

    // set store page fault
    val s1_storePageFault = io.fromTlb.bits.excp(0).pf.st && io.s1_in.bits.vecActive
    catchException(s1_exceptionVecOut, storeAddrMisaligned, s1_storeAddrMisaligned)

    // set store guest page fault
    val s1_storeGuestPageFault = io.fromTlb.bits.excp(0).gpf.st && io.s1_in.bits.vecActive
    catchException(s1_exceptionVecOut, storeGuestPageFault, s1_storeGuestPageFault)

    // set store access fault
    val s1_storeAccessFault = io.fromTlb.bits.excp(0).af.st && io.s1_in.bits.vecActive
    catchException(s1_exceptionVecOut, storeAccessFault, s1_storeAccessFault)
  } .otherwise {
    clearException(s1_exceptionVecOut, storeAddrMisaligned)
    clearException(s1_exceptionVecOut, storeGuestPageFault)
    clearException(s1_exceptionVecOut, storeAccessFault)
  }

  // set breakpoint exception
  catchException(s1_exceptionVecOut, breakPoint, io.fromTrigger.breakPoint)

  io.commonOut.s1_exceptionVecOut := s1_exceptionVecOut
  io.commonOut.s1_misalign := s1_misalign

  // Pipeline
  // -------------------------------------------------------------------
  // stage 2
  // -------------------------------------------------------------------
  val s2_exceptionVecIn = RegEnable(s1_exceptionVecOut, 0.U.asTypeOf(s1_exceptionVecOut), io.s1_in.valid)
  val s2_exceptionVecOut = WireInit(s2_exceptionVecIn)

  /**
    * Handle load-related exceptions in s2.
    * This block executes only if the current uop is a load and there is no delayed error.
    */
  when (io.s2_in.bits.isLoad && !io.s2_in.bits.delayedError) {
    /**
      * Determine if a load access fault occurred
      * Conditions for load access fault:
      * - The vector uop is active (io.s2_in.bits.vecActive), and one of the following is true:
      *   1. PMP (Physical Memory Protection) load violation occurs (io.fromPmp.ld).
      *   2. The exception vector indicates a load access fault (exceptionCatched).
      *   3. The uop is a vector load or uses the misalignment buffer (isVector || isMisalignBuf),
      *      and it is non-cacheable (isNoncacheable) but not a prefetch (isPrefetch) and without a TLB miss.
      */
    val s2_loadAccessFault = (io.fromPmp.ld || exceptionCatched(s2_exceptionVecIn, loadAccessFault) ||
      (io.s2_in.bits.isVector || io.s2_in.bits.isMisalignBuf) && io.s2_in.bits.isNoncacheable && !io.s2_in.bits.isPrefetch &&
      !io.s2_in.bits.tlbMiss && io.s2_in.bits.vecActive
    )
    catchException(s2_exceptionVecOut, loadAccessFault, s2_loadAccessFault)

    // soft prefetch will not trigger any exception (but ecc error interrupt may
    // be triggered)
    val s2_tlbUnrelatedExceptions = s2_loadAccessFault || exceptionCatched(s2_exceptionVecIn, breakPoint)

    // clean all exception if need
    val s2_clearAllException = io.s2_in.bits.isLoad && !io.s2_in.bits.delayedError &&
      (io.s2_in.bits.isPrefetch || io.s2_in.bits.tlbMiss && !s2_tlbUnrelatedExceptions)

    when (s2_clearAllException) {
      clearAllExceptions(s2_exceptionVecOut)
    }
  }

  /**
    * Handle store-related exceptions in s2.
    * This block executes only if the current uop is a load.
    */
  when (io.s2_in.bits.isStore) {
    /**
      * Determine if a store access fault occurred in s2.
      * A store access fault can occur under specific conditions when vector operations or misaligned buffer operations
      * are active.
      * Conditions for store access fault:
      * - The vector uop is active (io.s2_in.bits.vecActive), and one of the following is true:
      *   1. PMP (Physical Memory Protection) store violation occurs (io.fromPmp.st).
      *   2. The exception vector indicates a store access fault.
      *   3. The uop is a vector load or uses the misalignment buffer (isVector || isMisalignBuf),
      *      and it is non-cacheable (isNoncacheable) but not a prefetch (isPrefetch) and without a TLB miss.
      */
    val s2_storeAccessFault = io.s2_in.bits.vecActive && (io.fromPmp.st ||
        exceptionCatched(s2_exceptionVecIn, storeAccessFault) ||
        ((io.s2_in.bits.isVector || io.s2_in.bits.isMisalignBuf) &&
        io.s2_in.bits.isNoncacheable && !io.s2_in.bits.isPrefetch && !io.s2_in.bits.tlbMiss)
      )
    catchException(s2_exceptionVecOut, storeAccessFault, s2_storeAccessFault)

    // set store address misalign fault
    val s2_storeAddrMisaligned = io.s2_in.bits.mmio && io.s2_in.bits.misalign
    catchException(s2_exceptionVecOut, storeAddrMisaligned, s2_storeAddrMisaligned)
  }

  io.commonOut.s2_exceptionVecOut := s2_exceptionVecOut

  // Pipeline
  // -------------------------------------------------------------------
  // stage 3
  // -------------------------------------------------------------------
  val s3_exceptionVecIn = RegEnable(s2_exceptionVecOut, 0.U.asTypeOf(s2_exceptionVecOut), io.s2_in.valid)
  val s3_exceptionVecOut = WireInit(s3_exceptionVecIn)

  // handle loadAccessFault
  val s3_loadAccessFault = (io.s3_in.bits.delayedError || exceptionCatched(s3_exceptionVecIn, loadAccessFault)) &&
    io.s3_in.bits.vecActive
  catchException(s3_exceptionVecOut, loadAccessFault, s3_loadAccessFault)

  // handle hardwareError
  val s3_hardwareError =
    if (EnableAccurateLoadError) {
      io.fromDCache.error_delayed && GatedValidRegNext(io.fromCtrl.csr.cache_error_enable)
    } else {
      WireInit(false.B)
    }
  catchException(s3_exceptionVecOut, hardwareError, s3_hardwareError && io.s3_in.bits.vecActive)

  io.commonOut.s3_exceptionVecOut := s3_exceptionVecOut
}
