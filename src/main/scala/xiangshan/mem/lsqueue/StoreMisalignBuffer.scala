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
import xiangshan.frontend.FtqPtr
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.fu.fpu.FPU
import xiangshan.backend.rob.RobLsqIO
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.FuConfig.StaCfg
import xiangshan.backend.fu.FuType.isVStore
import xiangshan.mem.Bundles._
import xiangshan.cache._
import xiangshan.cache.wpu.ReplayCarry

class StoreMisalignBuffer(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper
{
  private val enqPortNum = StorePipelineWidth
  private val maxSplitNum = 2

  require(maxSplitNum == 2)

  private val SB = "b00".U(2.W)
  private val SH = "b01".U(2.W)
  private val SW = "b10".U(2.W)
  private val SD = "b11".U(2.W)

  // encode of how many bytes to shift or truncate
  private val BYTE0 = "b000".U(3.W)
  private val BYTE1 = "b001".U(3.W)
  private val BYTE2 = "b010".U(3.W)
  private val BYTE3 = "b011".U(3.W)
  private val BYTE4 = "b100".U(3.W)
  private val BYTE5 = "b101".U(3.W)
  private val BYTE6 = "b110".U(3.W)
  private val BYTE7 = "b111".U(3.W)

  def getMask(sizeEncode: UInt) = LookupTree(sizeEncode, List(
    SB -> 0x1.U,
    SH -> 0x3.U,
    SW -> 0xf.U,
    SD -> 0xff.U
  ))

  def selectOldest[T <: LsPipelineBundle](valid: Seq[Bool], bits: Seq[T], index: Seq[UInt]): (Seq[Bool], Seq[T], Seq[UInt]) = {
    assert(valid.length == bits.length)
    if (valid.length == 0 || valid.length == 1) {
      (valid, bits, index)
    } else if (valid.length == 2) {
      val res = Seq.fill(2)(Wire(ValidIO(chiselTypeOf(bits(0)))))
      val resIndex = Seq.fill(2)(Wire(chiselTypeOf(index(0))))
      for (i <- res.indices) {
        res(i).valid := valid(i)
        res(i).bits := bits(i)
        resIndex(i) := index(i)
      }
      val oldest = Mux(valid(0) && valid(1),
        Mux(isAfter(bits(0).uop.robIdx, bits(1).uop.robIdx) ||
          (isNotBefore(bits(0).uop.robIdx, bits(1).uop.robIdx) && bits(0).uop.uopIdx > bits(1).uop.uopIdx), res(1), res(0)),
        Mux(valid(0) && !valid(1), res(0), res(1)))

      val oldestIndex = Mux(valid(0) && valid(1),
        Mux(isAfter(bits(0).uop.robIdx, bits(1).uop.robIdx) ||
          (bits(0).uop.robIdx === bits(1).uop.robIdx && bits(0).uop.uopIdx > bits(1).uop.uopIdx), resIndex(1), resIndex(0)),
        Mux(valid(0) && !valid(1), resIndex(0), resIndex(1)))
      (Seq(oldest.valid), Seq(oldest.bits), Seq(oldestIndex))
    } else {
      val left = selectOldest(valid.take(valid.length / 2), bits.take(bits.length / 2), index.take(index.length / 2))
      val right = selectOldest(valid.takeRight(valid.length - (valid.length / 2)), bits.takeRight(bits.length - (bits.length / 2)), index.takeRight(index.length - (index.length / 2)))
      selectOldest(left._1 ++ right._1, left._2 ++ right._2, left._3 ++ right._3)
    }
  }

  val io = IO(new Bundle() {
    val redirect        = Flipped(Valid(new Redirect))
    val req             = Vec(enqPortNum, Flipped(Decoupled(new LsPipelineBundle)))
    val rob             = Flipped(new RobLsqIO)
    val splitStoreReq   = Decoupled(new LsPipelineBundle)
    val splitStoreResp  = Flipped(Valid(new SqWriteBundle))
    val writeBack       = Decoupled(new MemExuOutput)
    val vecWriteBack    = Vec(VecStorePipelineWidth, Decoupled(new VecPipelineFeedbackIO(isVStore = true)))
    val storeOutValid    = Input(Bool())
    val storeVecOutValid = Input(Bool())
    val overwriteExpBuf = Output(new XSBundle {
      val valid = Bool()
      val vaddr = UInt(XLEN.W)
      val isHyper = Bool()
      val gpaddr = UInt(XLEN.W)
      val isForVSnonLeafPTE = Bool()
    })
    val sqControl       = new StoreMaBufToSqControlIO

    val toVecStoreMergeBuffer = Vec(VecStorePipelineWidth, new StoreMaBufToVecStoreMergeBufferIO)
    val full = Bool()
  })

  io.rob.mmio := 0.U.asTypeOf(Vec(LoadPipelineWidth, Bool()))
  io.rob.uop  := 0.U.asTypeOf(Vec(LoadPipelineWidth, new DynInst))

  class StoreMisalignBufferEntry(implicit p: Parameters) extends LsPipelineBundle {
    val portIndex = UInt(log2Up(enqPortNum).W)
  }
  val req_valid = RegInit(false.B)
  val req = Reg(new StoreMisalignBufferEntry)

  val cross4KBPageBoundary = Wire(Bool())
  val needFlushPipe = RegInit(false.B)

  // buffer control:
  //  - s_idle:  Idle
  //  - s_split: Split miss-aligned store into aligned stores
  //  - s_req:   Send split store to sta and get result from sta
  //  - s_resp:  Responds to a split store access request
  //  - s_wb:    writeback yo rob/vecMergeBuffer
  //  - s_block: Wait for this instr to reach the head of Rob.
  val s_idle :: s_split :: s_req :: s_resp :: s_wb :: s_block :: Nil = Enum(6)
  val bufferState    = RegInit(s_idle)

  // enqueue
  // s1:
  val s1_req = VecInit(io.req.map(_.bits))
  val s1_valid = VecInit(io.req.map(x => x.valid))

  val s1_index = (0 until io.req.length).map(_.asUInt)
  val reqSel = selectOldest(s1_valid, s1_req, s1_index)

  val reqSelValid = reqSel._1(0)
  val reqSelBits  = reqSel._2(0)
  val reqSelPort  = reqSel._3(0)

  val reqRedirect = reqSelBits.uop.robIdx.needFlush(io.redirect)

  val canEnq = !req_valid && !reqRedirect && reqSelValid
  val robMatch = req_valid && io.rob.pendingst && (io.rob.pendingPtr === req.uop.robIdx)

  when(canEnq) {
    connectSamePort(req, reqSelBits)
    req.portIndex := reqSelPort
    req_valid := !reqSelBits.hasException
  }
  val cross4KBPageEnq = WireInit(false.B)
  when (cross4KBPageBoundary && !reqRedirect) {
    when(
      reqSelValid && !reqSelBits.hasException &&
      (isAfter(req.uop.robIdx, reqSelBits.uop.robIdx) || (isNotBefore(req.uop.robIdx, reqSelBits.uop.robIdx) && req.uop.uopIdx > reqSelBits.uop.uopIdx)) &&
      bufferState === s_idle
    ) {
      connectSamePort(req, reqSelBits)
      req.portIndex := reqSelPort
      cross4KBPageEnq := true.B
      needFlushPipe   := true.B
    } .otherwise {
      req := req
      cross4KBPageEnq := false.B
    }
  }

  val reqSelCanEnq = UIntToOH(reqSelPort)

  io.req.zipWithIndex.map{
    case (reqPort, index) => reqPort.ready := reqSelCanEnq(index) && (!req_valid || cross4KBPageBoundary && cross4KBPageEnq)
  }

  io.toVecStoreMergeBuffer.zipWithIndex.map{
    case (toStMB, index) => {
      toStMB.flush   := req_valid && cross4KBPageBoundary && cross4KBPageEnq && UIntToOH(req.portIndex)(index)
      toStMB.mbIndex := req.mbIndex
    }
  }
  io.full := req_valid

  //logic
  val splitStoreReqs = RegInit(VecInit(List.fill(maxSplitNum)(0.U.asTypeOf(new LsPipelineBundle))))
  val splitStoreResp = RegInit(VecInit(List.fill(maxSplitNum)(0.U.asTypeOf(new SqWriteBundle))))
  val isCrossPage    = RegInit(false.B)
  val exceptionVec   = RegInit(0.U.asTypeOf(ExceptionVec()))
  val unSentStores   = RegInit(0.U(maxSplitNum.W))
  val unWriteStores  = RegInit(0.U(maxSplitNum.W))
  val curPtr = RegInit(0.U(log2Ceil(maxSplitNum).W))

  // if there is exception or mmio in split store
  val globalException = RegInit(false.B)
  val globalMMIO = RegInit(false.B)

  val hasException = io.splitStoreResp.bits.vecActive && !io.splitStoreResp.bits.need_rep &&
    ExceptionNO.selectByFu(io.splitStoreResp.bits.uop.exceptionVec, StaCfg).asUInt.orR || TriggerAction.isDmode(io.splitStoreResp.bits.uop.trigger)
  val isMMIO = io.splitStoreResp.bits.mmio && !io.splitStoreResp.bits.need_rep

  io.sqControl.toStoreQueue.crossPageWithHit := io.sqControl.toStoreMisalignBuffer.sqPtr === req.uop.sqIdx && isCrossPage
  io.sqControl.toStoreQueue.crossPageCanDeq := !isCrossPage || bufferState === s_block
  io.sqControl.toStoreQueue.paddr := Cat(splitStoreResp(1).paddr(splitStoreResp(1).paddr.getWidth - 1, 3), 0.U(3.W))

  io.sqControl.toStoreQueue.withSameUop := io.sqControl.toStoreMisalignBuffer.uop.robIdx === req.uop.robIdx && io.sqControl.toStoreMisalignBuffer.uop.uopIdx === req.uop.uopIdx && req.isvec && robMatch && isCrossPage

  //state transition
  switch(bufferState) {
    is (s_idle) {
      when(cross4KBPageBoundary) {
        when(robMatch) {
          bufferState := s_split
          isCrossPage := true.B
        }
      } .otherwise {
        when (req_valid) {
          bufferState := s_split
          isCrossPage := false.B
        }
      }

    }

    is (s_split) {
      bufferState := s_req
    }

    is (s_req) {
      when (io.splitStoreReq.fire) {
        bufferState := s_resp
      }
    }

    is (s_resp) {
      when (io.splitStoreResp.valid) {
        val clearOh = UIntToOH(curPtr)
        when (hasException || isMMIO) {
          // commit directly when exception ocurs
          // if any split store reaches mmio space, delegate to software storeAddrMisaligned exception
          bufferState := s_wb
          globalException := hasException
          globalMMIO := isMMIO
        } .elsewhen(io.splitStoreResp.bits.need_rep || (unSentStores & (~clearOh).asUInt).orR) {
          // need replay or still has unsent requests
          bufferState := s_req
        } .otherwise {
          // got result, goto calculate data and control sq
          bufferState := s_wb
        }
      }
    }

    is (s_wb) {
      when (req.isvec) {
        when (io.vecWriteBack.map(x => x.fire).reduce( _ || _)) {
          bufferState := s_idle
          req_valid := false.B
          curPtr := 0.U
          unSentStores := 0.U
          unWriteStores := 0.U
          globalException := false.B
          globalMMIO := false.B
          isCrossPage := false.B
          needFlushPipe := false.B
        }

      }.otherwise {
        when (io.writeBack.fire && (!isCrossPage || globalMMIO || globalException)) {
          bufferState := s_idle
          req_valid := false.B
          curPtr := 0.U
          unSentStores := 0.U
          unWriteStores := 0.U
          globalException := false.B
          globalMMIO := false.B
          isCrossPage := false.B
          needFlushPipe := false.B
        } .elsewhen(io.writeBack.fire && isCrossPage) {
          bufferState := s_block
        } .otherwise {
          bufferState := s_wb
        }

      }
    }

    is (s_block) {
      when (io.sqControl.toStoreMisalignBuffer.doDeq) {
        bufferState := s_idle
        req_valid := false.B
        curPtr := 0.U
        unSentStores := 0.U
        unWriteStores := 0.U
        globalException := false.B
        globalMMIO := false.B
        isCrossPage := false.B
      }
    }
  }

  val alignedType = Mux(req.isvec, req.alignedType(1,0), req.uop.fuOpType(1, 0))

  val highAddress = LookupTree(alignedType, List(
    SB -> 0.U,
    SH -> 1.U,
    SW -> 3.U,
    SD -> 7.U
  )) + req.vaddr(4, 0)

  val highPageAddress = LookupTree(alignedType, List(
    SB -> 0.U,
    SH -> 1.U,
    SW -> 3.U,
    SD -> 7.U
  )) + req.vaddr(12, 0)
  // to see if (vaddr + opSize - 1) and vaddr are in the same 16 bytes region
  val cross16BytesBoundary = req_valid && (highAddress(4) =/= req.vaddr(4))
  cross4KBPageBoundary := req_valid && (highPageAddress(12) =/= req.vaddr(12))
  val aligned16BytesAddr   = (req.vaddr >> 4) << 4// req.vaddr & ~("b1111".U)
  val aligned16BytesSel    = req.vaddr(3, 0)

  // meta of 128 bit store
  val new128Store = WireInit(0.U.asTypeOf(new LsPipelineBundle))
  // meta of split loads
  val lowAddrStore  = WireInit(0.U.asTypeOf(new LsPipelineBundle))
  val highAddrStore = WireInit(0.U.asTypeOf(new LsPipelineBundle))
  // final lowResult = Cat(`lowResultWidth` of store data, 0.U(make it to fill total length of Vlen))
  val lowResultWidth = RegInit(0.U(3.W)) // how many bytes should we take from the store data
  // final highResult = Zero extend to Vlen(`highResultWidth` of (store data >> lowResultWidth))
  val highResultWidth = RegInit(0.U(3.W)) // how many bytes should we take from the store data

  when (bufferState === s_split) {
    when (!cross16BytesBoundary) {
      assert(false.B, s"There should be no non-aligned access that does not cross 16Byte boundaries.")
    } .otherwise {
      // split this unaligned store into `maxSplitNum` aligned stores
      unWriteStores := Fill(maxSplitNum, 1.U(1.W))
      unSentStores := Fill(maxSplitNum, 1.U(1.W))
      curPtr := 0.U
      lowAddrStore.uop := req.uop
      lowAddrStore.uop.exceptionVec(storeAddrMisaligned) := false.B
      highAddrStore.uop := req.uop
      highAddrStore.uop.exceptionVec(storeAddrMisaligned) := false.B

      switch (alignedType(1, 0)) {
        is (SB) {
          assert(false.B, "lb should not trigger miss align")
        }

        is (SH) {
          lowAddrStore.uop.fuOpType := SB
          lowAddrStore.vaddr := req.vaddr
          lowAddrStore.mask  := 0x1.U << lowAddrStore.vaddr(3, 0)
          lowResultWidth    := BYTE1

          highAddrStore.uop.fuOpType := SB
          highAddrStore.vaddr := req.vaddr + 1.U
          highAddrStore.mask  := 0x1.U << highAddrStore.vaddr(3, 0)
          highResultWidth    := BYTE1
        }

        is (SW) {
          switch (req.vaddr(1, 0)) {
            is ("b00".U) {
              assert(false.B, "should not trigger miss align")
            }

            is ("b01".U) {
              lowAddrStore.uop.fuOpType := SW
              lowAddrStore.vaddr := req.vaddr - 1.U
              lowAddrStore.mask  := 0xf.U << lowAddrStore.vaddr(3, 0)
              lowResultWidth    := BYTE3

              highAddrStore.uop.fuOpType := SB
              highAddrStore.vaddr := req.vaddr + 3.U
              highAddrStore.mask  := 0x1.U << highAddrStore.vaddr(3, 0)
              highResultWidth    := BYTE1
            }

            is ("b10".U) {
              lowAddrStore.uop.fuOpType := SH
              lowAddrStore.vaddr := req.vaddr
              lowAddrStore.mask  := 0x3.U << lowAddrStore.vaddr(3, 0)
              lowResultWidth    := BYTE2

              highAddrStore.uop.fuOpType := SH
              highAddrStore.vaddr := req.vaddr + 2.U
              highAddrStore.mask  := 0x3.U << highAddrStore.vaddr(3, 0)
              highResultWidth    := BYTE2
            }

            is ("b11".U) {
              lowAddrStore.uop.fuOpType := SB
              lowAddrStore.vaddr := req.vaddr
              lowAddrStore.mask  := 0x1.U << lowAddrStore.vaddr(3, 0)
              lowResultWidth    := BYTE1

              highAddrStore.uop.fuOpType := SW
              highAddrStore.vaddr := req.vaddr + 1.U
              highAddrStore.mask  := 0xf.U << highAddrStore.vaddr(3, 0)
              highResultWidth    := BYTE3
            }
          }
        }

        is (SD) {
          switch (req.vaddr(2, 0)) {
            is ("b000".U) {
              assert(false.B, "should not trigger miss align")
            }

            is ("b001".U) {
              lowAddrStore.uop.fuOpType := SD
              lowAddrStore.vaddr := req.vaddr - 1.U
              lowAddrStore.mask  := 0xff.U << lowAddrStore.vaddr(3, 0)
              lowResultWidth    := BYTE7

              highAddrStore.uop.fuOpType := SB
              highAddrStore.vaddr := req.vaddr + 7.U
              highAddrStore.mask  := 0x1.U << highAddrStore.vaddr(3, 0)
              highResultWidth    := BYTE1
            }

            is ("b010".U) {
              lowAddrStore.uop.fuOpType := SD
              lowAddrStore.vaddr := req.vaddr - 2.U
              lowAddrStore.mask  := 0xff.U << lowAddrStore.vaddr(3, 0)
              lowResultWidth    := BYTE6

              highAddrStore.uop.fuOpType := SH
              highAddrStore.vaddr := req.vaddr + 6.U
              highAddrStore.mask  := 0x3.U << highAddrStore.vaddr(3, 0)
              highResultWidth    := BYTE2
            }

            is ("b011".U) {
              lowAddrStore.uop.fuOpType := SD
              lowAddrStore.vaddr := req.vaddr - 3.U
              lowAddrStore.mask  := 0xff.U << lowAddrStore.vaddr(3, 0)
              lowResultWidth    := BYTE5

              highAddrStore.uop.fuOpType := SW
              highAddrStore.vaddr := req.vaddr + 5.U
              highAddrStore.mask  := 0xf.U << highAddrStore.vaddr(3, 0)
              highResultWidth    := BYTE3
            }

            is ("b100".U) {
              lowAddrStore.uop.fuOpType := SW
              lowAddrStore.vaddr := req.vaddr
              lowAddrStore.mask  := 0xf.U << lowAddrStore.vaddr(3, 0)
              lowResultWidth    := BYTE4

              highAddrStore.uop.fuOpType := SW
              highAddrStore.vaddr := req.vaddr + 4.U
              highAddrStore.mask  := 0xf.U << highAddrStore.vaddr(3, 0)
              highResultWidth    := BYTE4
            }

            is ("b101".U) {
              lowAddrStore.uop.fuOpType := SD
              lowAddrStore.vaddr := req.vaddr - 5.U
              lowAddrStore.mask  := 0xff.U << lowAddrStore.vaddr(3, 0)
              lowResultWidth    := BYTE3

              highAddrStore.uop.fuOpType := SD
              highAddrStore.vaddr := req.vaddr + 3.U
              highAddrStore.mask  := 0xff.U << highAddrStore.vaddr(3, 0)
              highResultWidth    := BYTE5
            }

            is ("b110".U) {
              lowAddrStore.uop.fuOpType := SD
              lowAddrStore.vaddr := req.vaddr - 6.U
              lowAddrStore.mask  := 0xff.U << lowAddrStore.vaddr(3, 0)
              lowResultWidth    := BYTE2

              highAddrStore.uop.fuOpType := SD
              highAddrStore.vaddr := req.vaddr + 2.U
              highAddrStore.mask  := 0xff.U << highAddrStore.vaddr(3, 0)
              highResultWidth    := BYTE6
            }

            is ("b111".U) {
              lowAddrStore.uop.fuOpType := SD
              lowAddrStore.vaddr := req.vaddr - 7.U
              lowAddrStore.mask  := 0xff.U << lowAddrStore.vaddr(3, 0)
              lowResultWidth    := BYTE1

              highAddrStore.uop.fuOpType := SD
              highAddrStore.vaddr := req.vaddr + 1.U
              highAddrStore.mask  := 0xff.U << highAddrStore.vaddr(3, 0)
              highResultWidth    := BYTE7
            }
          }
        }
      }

      splitStoreReqs(0) := lowAddrStore
      splitStoreReqs(1) := highAddrStore
    }
  }

  io.splitStoreReq.valid := req_valid && (bufferState === s_req)
  io.splitStoreReq.bits  := splitStoreReqs(curPtr)
  io.splitStoreReq.bits.isvec  := req.isvec
  // Restore the information of H extension store
  // bit encoding: | hsv 1 | store 00 | size(2bit) |
  val reqIsHsv  = LSUOpType.isHsv(req.uop.fuOpType)
  io.splitStoreReq.bits.uop.fuOpType := Mux(req.isvec, req.uop.fuOpType, Cat(reqIsHsv, 0.U(2.W), splitStoreReqs(curPtr).uop.fuOpType(1, 0)))
  io.splitStoreReq.bits.alignedType  := Mux(req.isvec, splitStoreReqs(curPtr).uop.fuOpType(1, 0), req.alignedType)
  io.splitStoreReq.bits.isFinalSplit := curPtr(0)

  when (io.splitStoreResp.valid) {
    val resp = io.splitStoreResp.bits
    splitStoreResp(curPtr) := io.splitStoreResp.bits
    when (isMMIO) {
      unWriteStores := 0.U
      unSentStores := 0.U
      exceptionVec := ExceptionNO.selectByFu(0.U.asTypeOf(exceptionVec.cloneType), StaCfg)
      // delegate to software
      exceptionVec(storeAddrMisaligned) := true.B
    } .elsewhen (hasException) {
      unWriteStores := 0.U
      unSentStores := 0.U
      StaCfg.exceptionOut.map(no => exceptionVec(no) := exceptionVec(no) || resp.uop.exceptionVec(no))
    } .elsewhen (!io.splitStoreResp.bits.need_rep) {
      unSentStores := unSentStores & (~UIntToOH(curPtr)).asUInt
      curPtr := curPtr + 1.U
      exceptionVec := 0.U.asTypeOf(ExceptionVec())
    }
  }

  val splitStoreData = RegInit(VecInit(List.fill(maxSplitNum)(0.U.asTypeOf(new XSBundle {
    val wdata = UInt(VLEN.W)
    val wmask = UInt((VLEN / 8).W)
  }))))

  val wmaskLow  = Wire(Vec(VLEN / 8, Bool()))
  val wmaskHigh = Wire(Vec(VLEN / 8, Bool()))
  (0 until (VLEN / 8)).map {
    case i  => {
      when (i.U < highResultWidth) {
        wmaskHigh(i) := true.B
      } .otherwise {
        wmaskHigh(i) := false.B
      }
      when (i.U < lowResultWidth) {
        wmaskLow(i) := true.B
      } .otherwise {
        wmaskLow(i) := false.B
      }
    }
  }

  io.writeBack.valid := req_valid && (bufferState === s_wb) && !io.storeOutValid && !req.isvec
  io.writeBack.bits.uop := req.uop
  io.writeBack.bits.uop.exceptionVec := DontCare
  StaCfg.exceptionOut.map(no => io.writeBack.bits.uop.exceptionVec(no) := (globalMMIO || globalException) && exceptionVec(no))
  io.writeBack.bits.uop.flushPipe := needFlushPipe
  io.writeBack.bits.uop.replayInst := false.B
  io.writeBack.bits.data := DontCare
  io.writeBack.bits.isFromLoadUnit := DontCare
  io.writeBack.bits.debug.isMMIO := globalMMIO
  // FIXME lyq: temporarily set to false
  io.writeBack.bits.debug.isNC := false.B
  io.writeBack.bits.debug.isPerfCnt := false.B
  io.writeBack.bits.debug.paddr := req.paddr
  io.writeBack.bits.debug.vaddr := req.vaddr

  io.vecWriteBack.zipWithIndex.map{
    case (wb, index) => {
      wb.valid := req_valid && (bufferState === s_wb) && req.isvec && !io.storeVecOutValid && UIntToOH(req.portIndex)(index)

      wb.bits.mBIndex           := req.mbIndex
      wb.bits.hit               := true.B
      wb.bits.isvec             := true.B
      wb.bits.sourceType        := RSFeedbackType.tlbMiss
      wb.bits.flushState        := DontCare
      wb.bits.trigger           := TriggerAction.None
      wb.bits.mmio              := globalMMIO
      wb.bits.exceptionVec      := ExceptionNO.selectByFu(exceptionVec, VstuCfg)
      wb.bits.hasException      := globalException
      wb.bits.usSecondInv       := req.usSecondInv
      wb.bits.vecFeedback       := true.B
      wb.bits.elemIdx           := req.elemIdx
      wb.bits.alignedType       := req.alignedType
      wb.bits.mask              := req.mask
      wb.bits.vaddr             := req.vaddr
      wb.bits.vaNeedExt         := req.vaNeedExt
      wb.bits.gpaddr            := req.gpaddr
      wb.bits.isForVSnonLeafPTE := req.isForVSnonLeafPTE
      wb.bits.vstart            := req.uop.vpu.vstart
      wb.bits.vecTriggerMask    := 0.U
      wb.bits.nc                := false.B
    }
  }

  val flush = req_valid && req.uop.robIdx.needFlush(io.redirect)

  when (flush) {
    bufferState := s_idle
    req_valid := Mux(cross4KBPageEnq && cross4KBPageBoundary && !reqRedirect, req_valid, false.B)
    curPtr := 0.U
    unSentStores := 0.U
    unWriteStores := 0.U
    globalException := false.B
    globalMMIO := false.B
    isCrossPage := false.B
    needFlushPipe := false.B
  }

  // NOTE: spectial case (unaligned store cross page, page fault happens in next page)
  // if exception happens in the higher page address part, overwrite the storeExceptionBuffer vaddr
  val shouldOverwrite = req_valid && cross16BytesBoundary && globalException && (curPtr === 1.U)
  val overwriteExpBuf = GatedValidRegNext(shouldOverwrite)
  val overwriteVaddr = RegEnable(splitStoreResp(curPtr).vaddr, shouldOverwrite)
  val overwriteIsHyper = RegEnable(splitStoreResp(curPtr).isHyper, shouldOverwrite)
  val overwriteGpaddr = RegEnable(splitStoreResp(curPtr).gpaddr, shouldOverwrite)
  val overwriteIsForVSnonLeafPTE = RegEnable(splitStoreResp(curPtr).isForVSnonLeafPTE, shouldOverwrite)

  //TODO In theory, there is no need to overwrite, but for now, the signal is retained in the code in this way.
  // and the signal will be removed after sufficient verification.
  io.overwriteExpBuf.valid := false.B
  io.overwriteExpBuf.vaddr := overwriteVaddr
  io.overwriteExpBuf.isHyper := overwriteIsHyper
  io.overwriteExpBuf.gpaddr := overwriteGpaddr
  io.overwriteExpBuf.isForVSnonLeafPTE := overwriteIsForVSnonLeafPTE

  XSPerfAccumulate("alloc",                  RegNext(!req_valid) && req_valid)
  XSPerfAccumulate("flush",                  flush)
  XSPerfAccumulate("flush_idle",             flush && (bufferState === s_idle))
  XSPerfAccumulate("flush_non_idle",         flush && (bufferState =/= s_idle))
}
