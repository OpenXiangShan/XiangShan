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
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.fpu.FPU
import xiangshan.backend.rob.RobLsqIO
import xiangshan.mem.Bundles._
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.Bundles.{ExuOutput, DynInst}
import xiangshan.backend.fu.FuConfig.LduCfg
import xiangshan.cache.mmu.HasTlbConst
import xiangshan.cache._
import xiangshan.cache.wpu.ReplayCarry

class LoadMisalignBuffer(val param: ExeUnitParams)(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasTlbConst
{
  private val enqPortNum = LoadPipelineWidth
  private val maxSplitNum = 2

  require(maxSplitNum == 2)

  private val LB = "b00".U(2.W)
  private val LH = "b01".U(2.W)
  private val LW = "b10".U(2.W)
  private val LD = "b11".U(2.W)

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
    LB -> 0x1.U, // lb
    LH -> 0x3.U, // lh
    LW -> 0xf.U, // lw
    LD -> 0xff.U  // ld
  ))

  def getShiftAndTruncateData(shiftEncode: UInt, truncateEncode: UInt, data: UInt) = {
    val shiftData = LookupTree(shiftEncode, List(
      BYTE0 -> data(63,    0),
      BYTE1 -> data(63,    8),
      BYTE2 -> data(63,   16),
      BYTE3 -> data(63,   24),
      BYTE4 -> data(63,   32),
      BYTE5 -> data(63,   40),
      BYTE6 -> data(63,   48),
      BYTE7 -> data(63,   56)
    ))
    val truncateData = LookupTree(truncateEncode, List(
      BYTE0 -> 0.U(XLEN.W), // can not truncate with 0 byte width
      BYTE1 -> shiftData(7,    0),
      BYTE2 -> shiftData(15,   0),
      BYTE3 -> shiftData(23,   0),
      BYTE4 -> shiftData(31,   0),
      BYTE5 -> shiftData(39,   0),
      BYTE6 -> shiftData(47,   0),
      BYTE7 -> shiftData(55,   0)
    ))
    truncateData(XLEN - 1, 0)
  }

  def selectOldest[T <: LqWriteBundle](valid: Seq[Bool], bits: Seq[T]): (Seq[Bool], Seq[T]) = {
    assert(valid.length == bits.length)
    if (valid.length == 0 || valid.length == 1) {
      (valid, bits)
    } else if (valid.length == 2) {
      val res = Seq.fill(2)(Wire(ValidIO(chiselTypeOf(bits(0)))))
      for (i <- res.indices) {
        res(i).valid := valid(i)
        res(i).bits := bits(i)
      }
      val oldest = Mux(valid(0) && valid(1),
        Mux(isAfter(bits(0).uop.robIdx, bits(1).uop.robIdx) ||
          (bits(0).uop.robIdx === bits(1).uop.robIdx && bits(0).uop.uopIdx > bits(1).uop.uopIdx), res(1), res(0)),
        Mux(valid(0) && !valid(1), res(0), res(1)))
      (Seq(oldest.valid), Seq(oldest.bits))
    } else {
      val left = selectOldest(valid.take(valid.length / 2), bits.take(bits.length / 2))
      val right = selectOldest(valid.takeRight(valid.length - (valid.length / 2)), bits.takeRight(bits.length - (bits.length / 2)))
      selectOldest(left._1 ++ right._1, left._2 ++ right._2)
    }
  }

  val io = IO(new Bundle() {
    val redirect        = Flipped(Valid(new Redirect))
    val enq             = Vec(enqPortNum, Flipped(new MisalignBufferEnqIO))
    val rob             = Flipped(new RobLsqIO)
    val splitLoadReq    = Decoupled(new LsPipelineBundle)
    val splitLoadResp   = Flipped(Valid(new LqWriteBundle))
    val writeBack       = Decoupled(new ExuOutput(param))
    val vecWriteBack    = Decoupled(new VecPipelineFeedbackIO(isVStore = false))
    val overwriteExpBuf = Output(new XSBundle {
      val valid  = Bool()
      val vaddr  = UInt(XLEN.W)
      val isHyper = Bool()
      val gpaddr = UInt(XLEN.W)
      val isForVSnonLeafPTE = Bool()
    })
    val flushLdExpBuff  = Output(Bool())
    val loadMisalignFull = Output(Bool())
  })

  io.rob.mmio := 0.U.asTypeOf(Vec(LoadPipelineWidth, Bool()))
  io.rob.uop  := 0.U.asTypeOf(Vec(LoadPipelineWidth, new DynInst))

  val req_valid = RegInit(false.B)
  val req = Reg(new LqWriteBundle)

  io.loadMisalignFull := req_valid

  (0 until io.enq.length).map{i =>
    if (i == 0) {
      io.enq(0).req.ready := !req_valid && io.enq(0).req.valid
    }
    else {
      io.enq(i).req.ready := !io.enq.take(i).map(_.req.ready).reduce(_ || _) && !req_valid && io.enq(i).req.valid
    }
  }

  val select_req_bit   = ParallelPriorityMux(io.enq.map(_.req.valid), io.enq.map(_.req.bits))
  val select_req_valid = io.enq.map(_.req.valid).reduce(_ || _)
  val canEnqValid = !req_valid && !select_req_bit.uop.robIdx.needFlush(io.redirect) && select_req_valid
  when(canEnqValid) {
    req := select_req_bit
    req_valid := true.B
  }

  // buffer control:
  //  - s_idle:   idle
  //  - s_split:  split misalign laod
  //  - s_req:    issue a split memory access request
  //  - s_resp:   Responds to a split load access request
  //  - s_comb_wakeup_rep: Merge the data and issue a wakeup load
  //  - s_wb: writeback yo rob/vecMergeBuffer
  val s_idle :: s_split :: s_req :: s_resp :: s_comb_wakeup_rep :: s_wb :: Nil = Enum(6)
  val bufferState = RegInit(s_idle)
  val splitLoadReqs = RegInit(VecInit(List.fill(maxSplitNum)(0.U.asTypeOf(new LsPipelineBundle))))
  val splitLoadResp = RegInit(VecInit(List.fill(maxSplitNum)(0.U.asTypeOf(new LqWriteBundle))))
  val exceptionVec = RegInit(0.U.asTypeOf(ExceptionVec()))
  val unSentLoads = RegInit(0.U(maxSplitNum.W))
  val curPtr = RegInit(0.U(log2Ceil(maxSplitNum).W))
  val needWakeUpReqsWire = Wire(Bool())
  val needWakeUpWB       = RegInit(false.B)
  val data_select        = RegEnable(genRdataOH(select_req_bit.uop), 0.U(genRdataOH(select_req_bit.uop).getWidth.W), canEnqValid)

  // if there is exception or uncache in split load
  val globalException = RegInit(false.B)
  val globalUncache = RegInit(false.B)

  // debug info
  val globalMMIO = RegInit(false.B)
  val globalNC   = RegInit(false.B)
  val globalMemBackTypeMM = RegInit(false.B)

  val hasException = io.splitLoadResp.bits.vecActive &&
    ExceptionNO.selectByFu(io.splitLoadResp.bits.uop.exceptionVec, LduCfg).asUInt.orR || TriggerAction.isDmode(io.splitLoadResp.bits.uop.trigger)
  val isUncache = io.splitLoadResp.bits.mmio || io.splitLoadResp.bits.nc
  needWakeUpReqsWire := false.B
  switch(bufferState) {
    is (s_idle) {
      when (req_valid) {
        bufferState := s_split
      }
    }

    is (s_split) {
      bufferState := s_req
    }

    is (s_req) {
      when (io.splitLoadReq.fire) {
        bufferState := s_resp
      }
    }

    is (s_resp) {
      when (io.splitLoadResp.valid) {
        val clearOh = UIntToOH(curPtr)
        when (hasException || isUncache) {
          // commit directly when exception ocurs
          // if any split load reaches uncache space, delegate to software loadAddrMisaligned exception
          bufferState := s_wb
          globalException := hasException
          globalUncache := isUncache
          globalMMIO := io.splitLoadResp.bits.mmio
          globalNC   := io.splitLoadResp.bits.nc
          globalMemBackTypeMM := io.splitLoadResp.bits.memBackTypeMM
        } .elsewhen(io.splitLoadResp.bits.rep_info.need_rep || (unSentLoads & ~clearOh).orR) {
          // need replay or still has unsent requests
          bufferState := s_req
        } .otherwise {
          // merge the split load results
          bufferState := s_comb_wakeup_rep
          needWakeUpWB := !req.isvec
        }
      }
    }

    is (s_comb_wakeup_rep) {
      when(!req.isvec) {
        when(io.splitLoadReq.fire) {
          bufferState := s_wb
        }.otherwise {
          bufferState := s_comb_wakeup_rep
        }
        needWakeUpReqsWire := true.B
      } .otherwise {
        bufferState := s_wb
      }

    }

    is (s_wb) {
      when(req.isvec) {
        when(io.vecWriteBack.fire) {
          bufferState := s_idle
          req_valid := false.B
          curPtr := 0.U
          unSentLoads := 0.U
          globalException := false.B
          globalUncache := false.B
          needWakeUpWB := false.B

          globalMMIO := false.B
          globalNC   := false.B
          globalMemBackTypeMM := false.B
        }

      } .otherwise {
        when(io.writeBack.fire) {
          bufferState := s_idle
          req_valid := false.B
          curPtr := 0.U
          unSentLoads := 0.U
          globalException := false.B
          globalUncache := false.B
          needWakeUpWB := false.B

          globalMMIO := false.B
          globalNC   := false.B
          globalMemBackTypeMM := false.B
        }
      }

    }
  }

  val alignedType = Mux(req.isvec, req.alignedType(1,0), req.uop.fuOpType(1, 0))
  val highAddress = LookupTree(alignedType, List(
    LB -> 0.U,
    LH -> 1.U,
    LW -> 3.U,
    LD -> 7.U
  )) + req.vaddr(4, 0)
  // to see if (vaddr + opSize - 1) and vaddr are in the same 16 bytes region
  val cross16BytesBoundary = req_valid && (highAddress(4) =/= req.vaddr(4))
  val aligned16BytesAddr   = (req.vaddr >> 4) << 4// req.vaddr & ~("b1111".U)
  val aligned16BytesSel    = req.vaddr(3, 0)

  // meta of 128 bit load
  val new128Load = WireInit(0.U.asTypeOf(new LsPipelineBundle))
  // meta of split loads
  val lowAddrLoad  = WireInit(0.U.asTypeOf(new LsPipelineBundle))
  val highAddrLoad = WireInit(0.U.asTypeOf(new LsPipelineBundle))
  val lowResultShift = RegInit(0.U(3.W)) // how many bytes should we shift right when got result
  val lowResultWidth = RegInit(0.U(3.W)) // how many bytes should we take from result
  val highResultShift = RegInit(0.U(3.W))
  val highResultWidth = RegInit(0.U(3.W))

  when (bufferState === s_split) {
    when (!cross16BytesBoundary) {
      assert(false.B, s"There should be no non-aligned access that does not cross 16Byte boundaries.")
    } .otherwise {
      // split this unaligned load into `maxSplitNum` aligned loads
      unSentLoads := Fill(maxSplitNum, 1.U(1.W))
      curPtr := 0.U
      lowAddrLoad.uop := req.uop
      lowAddrLoad.uop.exceptionVec(loadAddrMisaligned) := false.B
      lowAddrLoad.fullva := req.fullva
      highAddrLoad.uop := req.uop
      highAddrLoad.uop.exceptionVec(loadAddrMisaligned) := false.B
      highAddrLoad.fullva := req.fullva

      switch (alignedType(1, 0)) {
        is (LB) {
          assert(false.B, "lb should not trigger miss align")
        }

        is (LH) {
          lowAddrLoad.uop.fuOpType := LB
          lowAddrLoad.vaddr := req.vaddr
          lowAddrLoad.mask  := 0x1.U << lowAddrLoad.vaddr(3, 0)
          lowResultShift    := BYTE0
          lowResultWidth    := BYTE1

          highAddrLoad.uop.fuOpType := LB
          highAddrLoad.vaddr := req.vaddr + 1.U
          highAddrLoad.mask  := 0x1.U << highAddrLoad.vaddr(3, 0)
          highResultShift    := BYTE0
          highResultWidth    := BYTE1
        }

        is (LW) {
          switch (req.vaddr(1, 0)) {
            is ("b00".U) {
              assert(false.B, "should not trigger miss align")
            }

            is ("b01".U) {
              lowAddrLoad.uop.fuOpType := LW
              lowAddrLoad.vaddr := req.vaddr - 1.U
              lowAddrLoad.mask  := 0xf.U << lowAddrLoad.vaddr(3, 0)
              lowResultShift    := BYTE1
              lowResultWidth    := BYTE3

              highAddrLoad.uop.fuOpType := LB
              highAddrLoad.vaddr := req.vaddr + 3.U
              highAddrLoad.mask  := 0x1.U << highAddrLoad.vaddr(3, 0)
              highResultShift    := BYTE0
              highResultWidth    := BYTE1
            }

            is ("b10".U) {
              lowAddrLoad.uop.fuOpType := LH
              lowAddrLoad.vaddr := req.vaddr
              lowAddrLoad.mask  := 0x3.U << lowAddrLoad.vaddr(3, 0)
              lowResultShift    := BYTE0
              lowResultWidth    := BYTE2

              highAddrLoad.uop.fuOpType := LH
              highAddrLoad.vaddr := req.vaddr + 2.U
              highAddrLoad.mask  := 0x3.U << highAddrLoad.vaddr(3, 0)
              highResultShift    := BYTE0
              highResultWidth    := BYTE2
            }

            is ("b11".U) {
              lowAddrLoad.uop.fuOpType := LB
              lowAddrLoad.vaddr := req.vaddr
              lowAddrLoad.mask  := 0x1.U << lowAddrLoad.vaddr(3, 0)
              lowResultShift    := BYTE0
              lowResultWidth    := BYTE1

              highAddrLoad.uop.fuOpType := LW
              highAddrLoad.vaddr := req.vaddr + 1.U
              highAddrLoad.mask  := 0xf.U << highAddrLoad.vaddr(3, 0)
              highResultShift    := BYTE0
              highResultWidth    := BYTE3
            }
          }
        }

        is (LD) {
          switch (req.vaddr(2, 0)) {
            is ("b000".U) {
              assert(false.B, "should not trigger miss align")
            }

            is ("b001".U) {
              lowAddrLoad.uop.fuOpType := LD
              lowAddrLoad.vaddr := req.vaddr - 1.U
              lowAddrLoad.mask  := 0xff.U << lowAddrLoad.vaddr(3, 0)
              lowResultShift    := BYTE1
              lowResultWidth    := BYTE7

              highAddrLoad.uop.fuOpType := LB
              highAddrLoad.vaddr := req.vaddr + 7.U
              highAddrLoad.mask  := 0x1.U << highAddrLoad.vaddr(3, 0)
              highResultShift    := BYTE0
              highResultWidth    := BYTE1
            }

            is ("b010".U) {
              lowAddrLoad.uop.fuOpType := LD
              lowAddrLoad.vaddr := req.vaddr - 2.U
              lowAddrLoad.mask  := 0xff.U << lowAddrLoad.vaddr(3, 0)
              lowResultShift    := BYTE2
              lowResultWidth    := BYTE6

              highAddrLoad.uop.fuOpType := LH
              highAddrLoad.vaddr := req.vaddr + 6.U
              highAddrLoad.mask  := 0x3.U << highAddrLoad.vaddr(3, 0)
              highResultShift    := BYTE0
              highResultWidth    := BYTE2
            }

            is ("b011".U) {
              lowAddrLoad.uop.fuOpType := LD
              lowAddrLoad.vaddr := req.vaddr - 3.U
              lowAddrLoad.mask  := 0xff.U << lowAddrLoad.vaddr(3, 0)
              lowResultShift    := BYTE3
              lowResultWidth    := BYTE5

              highAddrLoad.uop.fuOpType := LW
              highAddrLoad.vaddr := req.vaddr + 5.U
              highAddrLoad.mask  := 0xf.U << highAddrLoad.vaddr(3, 0)
              highResultShift    := BYTE0
              highResultWidth    := BYTE3
            }

            is ("b100".U) {
              lowAddrLoad.uop.fuOpType := LW
              lowAddrLoad.vaddr := req.vaddr
              lowAddrLoad.mask  := 0xf.U << lowAddrLoad.vaddr(3, 0)
              lowResultShift    := BYTE0
              lowResultWidth    := BYTE4

              highAddrLoad.uop.fuOpType := LW
              highAddrLoad.vaddr := req.vaddr + 4.U
              highAddrLoad.mask  := 0xf.U << highAddrLoad.vaddr(3, 0)
              highResultShift    := BYTE0
              highResultWidth    := BYTE4
            }

            is ("b101".U) {
              lowAddrLoad.uop.fuOpType := LW
              lowAddrLoad.vaddr := req.vaddr - 1.U
              lowAddrLoad.mask  := 0xf.U << lowAddrLoad.vaddr(3, 0)
              lowResultShift    := BYTE1
              lowResultWidth    := BYTE3

              highAddrLoad.uop.fuOpType := LD
              highAddrLoad.vaddr := req.vaddr + 3.U
              highAddrLoad.mask  := 0xff.U << highAddrLoad.vaddr(3, 0)
              highResultShift    := BYTE0
              highResultWidth    := BYTE5
            }

            is ("b110".U) {
              lowAddrLoad.uop.fuOpType := LH
              lowAddrLoad.vaddr := req.vaddr
              lowAddrLoad.mask  := 0x3.U << lowAddrLoad.vaddr(3, 0)
              lowResultShift    := BYTE0
              lowResultWidth    := BYTE2

              highAddrLoad.uop.fuOpType := LD
              highAddrLoad.vaddr := req.vaddr + 2.U
              highAddrLoad.mask  := 0xff.U << highAddrLoad.vaddr(3, 0)
              highResultShift    := BYTE0
              highResultWidth    := BYTE6
            }

            is ("b111".U) {
              lowAddrLoad.uop.fuOpType := LB
              lowAddrLoad.vaddr := req.vaddr
              lowAddrLoad.mask  := 0x1.U << lowAddrLoad.vaddr(3, 0)
              lowResultShift    := BYTE0
              lowResultWidth    := BYTE1

              highAddrLoad.uop.fuOpType := LD
              highAddrLoad.vaddr := req.vaddr + 1.U
              highAddrLoad.mask  := 0xff.U << highAddrLoad.vaddr(3, 0)
              highResultShift    := BYTE0
              highResultWidth    := BYTE7
            }
          }
        }
      }

      splitLoadReqs(0) := lowAddrLoad
      splitLoadReqs(1) := highAddrLoad
    }
    exceptionVec := 0.U.asTypeOf(exceptionVec.cloneType)
  }

  io.splitLoadReq.valid := req_valid && (bufferState === s_req || bufferState === s_comb_wakeup_rep && needWakeUpReqsWire && !req.isvec)
  io.splitLoadReq.bits  := splitLoadReqs(curPtr)
  io.splitLoadReq.bits.isvec  := req.isvec
  io.splitLoadReq.bits.misalignNeedWakeUp  := needWakeUpReqsWire
  io.splitLoadReq.bits.isFinalSplit        := curPtr(0) && !needWakeUpReqsWire
  // Restore the information of H extension load
  // bit encoding: | hlv 1 | hlvx 1 | is unsigned(1bit) | size(2bit) |
  val reqIsHlv  = LSUOpType.isHlv(req.uop.fuOpType)
  val reqIsHlvx = LSUOpType.isHlvx(req.uop.fuOpType)
  io.splitLoadReq.bits.uop.fuOpType := Mux(req.isvec, req.uop.fuOpType, Cat(reqIsHlv, reqIsHlvx, 0.U(1.W), splitLoadReqs(curPtr).uop.fuOpType(1, 0)))
  io.splitLoadReq.bits.alignedType  := Mux(req.isvec, splitLoadReqs(curPtr).uop.fuOpType(1, 0), req.alignedType)

  when (io.splitLoadResp.valid && bufferState === s_resp && req.uop.robIdx === io.splitLoadResp.bits.uop.robIdx) {
    val resp = io.splitLoadResp.bits
    splitLoadResp(curPtr) := io.splitLoadResp.bits
    when (isUncache) {
      unSentLoads := 0.U
      exceptionVec := ExceptionNO.selectByFu(0.U.asTypeOf(exceptionVec.cloneType), LduCfg)
      // delegate to software
      exceptionVec(loadAddrMisaligned) := true.B
    } .elsewhen (hasException) {
      unSentLoads := 0.U
      LduCfg.exceptionOut.map(no => exceptionVec(no) := exceptionVec(no) || resp.uop.exceptionVec(no))
    } .elsewhen (!io.splitLoadResp.bits.rep_info.need_rep) {
      unSentLoads := unSentLoads & ~UIntToOH(curPtr)
      curPtr := curPtr + 1.U
      exceptionVec := 0.U.asTypeOf(ExceptionVec())
    }
  }

  val combinedData = RegInit(0.U(XLEN.W))

  when (bufferState === s_comb_wakeup_rep) {
    val lowAddrResult = getShiftAndTruncateData(lowResultShift, lowResultWidth, splitLoadResp(0).data)
                          .asTypeOf(Vec(XLEN / 8, UInt(8.W)))
    val highAddrResult = getShiftAndTruncateData(highResultShift, highResultWidth, splitLoadResp(1).data)
                          .asTypeOf(Vec(XLEN / 8, UInt(8.W)))
    val catResult = Wire(Vec(XLEN / 8, UInt(8.W)))
    (0 until XLEN / 8) .map {
      case i => {
        when (i.U < lowResultWidth) {
          catResult(i) := lowAddrResult(i)
        } .otherwise {
          catResult(i) := highAddrResult(i.U - lowResultWidth)
        }
      }
    }
    combinedData := Mux(req.isvec, rdataVecHelper(req.alignedType, (catResult.asUInt)(XLEN - 1, 0)), rdataHelper(req.uop, (catResult.asUInt)(XLEN - 1, 0)))

  }

  io.writeBack.valid := req_valid && bufferState === s_wb &&
    (io.splitLoadResp.valid && io.splitLoadResp.bits.misalignNeedWakeUp || globalUncache || globalException) &&
    !req.isvec
  io.writeBack.bits := 0.U.asTypeOf(io.writeBack.bits)
  io.writeBack.bits.data := VecInit(Seq.fill(param.wbPathNum)(newRdataHelper(data_select, combinedData)))
  io.writeBack.bits.pdest := req.uop.pdest
  io.writeBack.bits.robIdx := req.uop.robIdx
  io.writeBack.bits.intWen.foreach(_ := !globalException && !globalUncache && req.uop.rfWen)
  io.writeBack.bits.fpWen.foreach(_ := req.uop.fpWen)
  io.writeBack.bits.vecWen.foreach(_ := req.uop.vecWen)
  io.writeBack.bits.v0Wen.foreach(_ := req.uop.v0Wen)
  io.writeBack.bits.vlWen.foreach(_ := req.uop.vlWen)
  io.writeBack.bits.exceptionVec.foreach(excp => {
    LduCfg.exceptionOut.foreach(no => excp(no) := (globalUncache || globalException) && exceptionVec(no))
  })
  io.writeBack.bits.flushPipe.foreach(_ := false.B)
  io.writeBack.bits.replay.foreach(_ := false.B)
  io.writeBack.bits.lqIdx.foreach(_ := req.uop.lqIdx)
  io.writeBack.bits.sqIdx.foreach(_ := req.uop.sqIdx)
  io.writeBack.bits.trigger.foreach(_ := req.uop.trigger)
  io.writeBack.bits.predecodeInfo.foreach(_ := req.uop.preDecodeInfo)
  io.writeBack.bits.vls.foreach(x => {
    x.vpu := req.uop.vpu
    x.oldVdPsrc := req.uop.psrc(2)
    x.vdIdx := DontCare
    x.vdIdxInField := DontCare
    x.isIndexed := VlduType.isIndexed(req.uop.fuOpType)
    x.isMasked := VlduType.isMasked(req.uop.fuOpType)
    x.isStrided := VlduType.isStrided(req.uop.fuOpType)
    x.isWhole := VlduType.isWhole(req.uop.fuOpType)
    x.isVecLoad := VlduType.isVecLd(req.uop.fuOpType)
    x.isVlm := VlduType.isMasked(req.uop.fuOpType) && VlduType.isVecLd(req.uop.fuOpType)
  })
  io.writeBack.bits.debug.isMMIO := globalMMIO
  io.writeBack.bits.debug.isNCIO := globalNC && !globalMemBackTypeMM
  io.writeBack.bits.debug.isPerfCnt := false.B
  io.writeBack.bits.debug.paddr := req.paddr
  io.writeBack.bits.debug.vaddr := req.vaddr

  // vector output
  io.vecWriteBack.valid := req_valid && (bufferState === s_wb) && req.isvec

  io.vecWriteBack.bits.alignedType          := req.alignedType
  io.vecWriteBack.bits.vecFeedback          := true.B
  io.vecWriteBack.bits.vecdata.get          := combinedData
  io.vecWriteBack.bits.isvec                := req.isvec
  io.vecWriteBack.bits.elemIdx              := req.elemIdx
  io.vecWriteBack.bits.elemIdxInsideVd.get  := req.elemIdxInsideVd
  io.vecWriteBack.bits.mask                 := req.mask
  io.vecWriteBack.bits.reg_offset.get       := 0.U
  io.vecWriteBack.bits.usSecondInv          := req.usSecondInv
  io.vecWriteBack.bits.mBIndex              := req.mbIndex
  io.vecWriteBack.bits.hit                  := true.B
  io.vecWriteBack.bits.sourceType           := RSFeedbackType.lrqFull
  io.vecWriteBack.bits.trigger              := TriggerAction.None
  io.vecWriteBack.bits.flushState           := DontCare
  io.vecWriteBack.bits.exceptionVec         := ExceptionNO.selectByFu(exceptionVec, VlduCfg)
  io.vecWriteBack.bits.hasException         := globalException
  io.vecWriteBack.bits.vaddr                := req.fullva
  io.vecWriteBack.bits.vaNeedExt            := req.vaNeedExt
  io.vecWriteBack.bits.gpaddr               := req.gpaddr
  io.vecWriteBack.bits.isForVSnonLeafPTE    := req.isForVSnonLeafPTE
  io.vecWriteBack.bits.mmio                 := globalMMIO
  io.vecWriteBack.bits.vstart               := req.uop.vpu.vstart
  io.vecWriteBack.bits.vecTriggerMask       := req.vecTriggerMask
  io.vecWriteBack.bits.nc                   := globalNC


  val flush = req_valid && req.uop.robIdx.needFlush(io.redirect)

  when (flush) {
    bufferState := s_idle
    req_valid := false.B
    curPtr := 0.U
    unSentLoads := 0.U
    globalException := false.B
    globalUncache := false.B

    globalMMIO := false.B
    globalNC   := false.B
    globalMemBackTypeMM := false.B
  }

  // NOTE: spectial case (unaligned load cross page, page fault happens in next page)
  // if exception happens in the higher page address part, overwrite the loadExceptionBuffer vaddr
  val shouldOverwrite = req_valid && globalException
  val overwriteExpBuf = GatedValidRegNext(shouldOverwrite)
  val overwriteVaddr = RegEnable(
    Mux(
      cross16BytesBoundary && (curPtr === 1.U),
      splitLoadResp(curPtr).vaddr,
      splitLoadResp(curPtr).fullva),
    shouldOverwrite)
  val overwriteGpaddr = RegEnable(splitLoadResp(curPtr).gpaddr, shouldOverwrite)
  val overwriteIsHyper = RegEnable(splitLoadResp(curPtr).isHyper, shouldOverwrite)
  val overwriteIsForVSnonLeafPTE = RegEnable(splitLoadResp(curPtr).isForVSnonLeafPTE, shouldOverwrite)

  //TODO In theory, there is no need to overwrite, but for now, the signal is retained in the code in this way.
  // and the signal will be removed after sufficient verification.
  io.overwriteExpBuf.valid := false.B
  io.overwriteExpBuf.vaddr := overwriteVaddr
  io.overwriteExpBuf.isHyper := overwriteIsHyper
  io.overwriteExpBuf.gpaddr := overwriteGpaddr
  io.overwriteExpBuf.isForVSnonLeafPTE := overwriteIsForVSnonLeafPTE

  // when no exception or uncache, flush loadExceptionBuffer at s_wb
  val flushLdExpBuff = GatedValidRegNext(req_valid && (bufferState === s_wb) && !(globalUncache || globalException))
  io.flushLdExpBuff := flushLdExpBuff

  XSPerfAccumulate("alloc",                  RegNext(!req_valid) && req_valid)
  XSPerfAccumulate("flush",                  flush)
  XSPerfAccumulate("flush_idle",             flush && (bufferState === s_idle))
  XSPerfAccumulate("flush_non_idle",         flush && (bufferState =/= s_idle))
}
