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
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.fu.fpu.FPU
import xiangshan.backend.rob.RobLsqIO
import xiangshan.cache._
import xiangshan.frontend.FtqPtr
import xiangshan.ExceptionNO._
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.Bundles.{MemExuOutput, DynInst}

class LoadMisalignBuffer(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper
  with HasLoadHelper 
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
          (isNotBefore(bits(0).uop.robIdx, bits(1).uop.robIdx) && bits(0).uop.uopIdx > bits(1).uop.uopIdx), res(1), res(0)),
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
    val req             = Vec(enqPortNum, Flipped(Valid(new LqWriteBundle)))
    val rob             = Flipped(new RobLsqIO)
    val splitLoadReq    = Decoupled(new LsPipelineBundle)
    val splitLoadResp   = Flipped(Valid(new LqWriteBundle))
    val writeBack       = Decoupled(new MemExuOutput)
    val overwriteExpBuf = Output(new XSBundle {
      val valid = Bool()
      val vaddr = UInt(VAddrBits.W)
    })
    val flushLdExpBuff  = Output(Bool())
  })

  io.rob.mmio := 0.U.asTypeOf(Vec(LoadPipelineWidth, Bool()))
  io.rob.uop  := 0.U.asTypeOf(Vec(LoadPipelineWidth, new DynInst))

  val req_valid = RegInit(false.B)
  val req = Reg(new LqWriteBundle)

  // enqueue
  // s1:
  val s1_req = VecInit(io.req.map(_.bits))
  val s1_valid = VecInit(io.req.map(x => x.valid))

  // s2: delay 1 cycle
  val s2_req = RegNext(s1_req)
  val s2_valid = (0 until enqPortNum).map(i =>
    RegNext(s1_valid(i)) &&
    !s2_req(i).uop.robIdx.needFlush(RegNext(io.redirect)) &&
    !s2_req(i).uop.robIdx.needFlush(io.redirect)
  )
  val s2_miss_aligned = s2_req.map(x =>
    x.uop.exceptionVec(loadAddrMisaligned) && !x.uop.exceptionVec(breakPoint) && !TriggerAction.isDmode(x.uop.trigger)
  )

  val s2_enqueue = Wire(Vec(enqPortNum, Bool()))
  for (w <- 0 until enqPortNum) {
    s2_enqueue(w) := s2_valid(w) && s2_miss_aligned(w)
  }

  when (req_valid && req.uop.robIdx.needFlush(io.redirect)) {
    req_valid := s2_enqueue.asUInt.orR
  } .elsewhen (s2_enqueue.asUInt.orR) {
    req_valid := req_valid || true.B
  }

  val reqSel = selectOldest(s2_enqueue, s2_req)

  when (req_valid) {
    req := Mux(
      reqSel._1(0) && (isAfter(req.uop.robIdx, reqSel._2(0).uop.robIdx) || (isNotBefore(req.uop.robIdx, reqSel._2(0).uop.robIdx) && req.uop.uopIdx > reqSel._2(0).uop.uopIdx)),
      reqSel._2(0),
      req)
  } .elsewhen (s2_enqueue.asUInt.orR) {
    req := reqSel._2(0)
  }

  val robMatch = req_valid && io.rob.pendingld && (io.rob.pendingPtr === req.uop.robIdx)

  // buffer control:
  //  - split miss-aligned load into aligned loads
  //  - send split load to ldu and get result from ldu
  //  - merge them and write back to rob
  val s_idle :: s_split :: s_req :: s_resp :: s_comb :: s_wb :: s_wait :: Nil = Enum(7)
  val bufferState = RegInit(s_idle)
  val splitLoadReqs = RegInit(VecInit(List.fill(maxSplitNum)(0.U.asTypeOf(new LsPipelineBundle))))
  val splitLoadResp = RegInit(VecInit(List.fill(maxSplitNum)(0.U.asTypeOf(new LqWriteBundle))))
  val unSentLoads = RegInit(0.U(maxSplitNum.W))
  val curPtr = RegInit(0.U(log2Ceil(maxSplitNum).W))

  // if there is exception or mmio in split load
  val globalException = RegInit(false.B)
  val globalMMIO = RegInit(false.B)

  val hasException = ExceptionNO.selectByFu(io.splitLoadResp.bits.uop.exceptionVec, LduCfg).asUInt.orR
  val isMMIO = io.splitLoadResp.bits.mmio

  switch(bufferState) {
    is (s_idle) {
      when (robMatch) {
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
        when (hasException || isMMIO) {
          // commit directly when exception ocurs
          // if any split load reaches mmio space, delegate to software loadAddrMisaligned exception
          bufferState := s_wb
          globalException := hasException
          globalMMIO := isMMIO
        } .elsewhen(io.splitLoadResp.bits.rep_info.need_rep || (unSentLoads & ~clearOh).orR) {
          // need replay or still has unsent requests
          bufferState := s_req
        } .otherwise {
          // merge the split load results
          bufferState := s_comb
        }
      }
    }

    is (s_comb) {
      bufferState := s_wb
    }

    is (s_wb) {
      when(io.writeBack.fire) {
        bufferState := s_wait
      }
    }

    is (s_wait) {
      when(io.rob.lcommit =/= 0.U || req.uop.robIdx.needFlush(io.redirect)) {
        // rob commits the unaligned load or handled the exception, reset all state
        bufferState := s_idle
        req_valid := false.B
        curPtr := 0.U
        unSentLoads := 0.U
        globalException := false.B
        globalMMIO := false.B
      }
    }
  }

  val highAddress = LookupTree(req.uop.fuOpType(1, 0), List(
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
      // change this unaligned load into a 128 bits load
      unSentLoads := 1.U
      curPtr := 0.U
      new128Load.vaddr := aligned16BytesAddr
      // new128Load.mask  := (getMask(req.uop.fuOpType(1, 0)) << aligned16BytesSel).asUInt
      new128Load.mask  := 0xffff.U
      new128Load.fuTypeInMem := req.fuTypeInMem
      new128Load.uop   := req.uop
      new128Load.uop.exceptionVec(loadAddrMisaligned) := false.B
      new128Load.is128bit := true.B
      splitLoadReqs(0) := new128Load
    } .otherwise {
      // split this unaligned load into `maxSplitNum` aligned loads
      unSentLoads := Fill(maxSplitNum, 1.U(1.W))
      curPtr := 0.U
      lowAddrLoad.fuTypeInMem := req.fuTypeInMem
      lowAddrLoad.uop := req.uop
      lowAddrLoad.uop.exceptionVec(loadAddrMisaligned) := false.B
      highAddrLoad.uop := req.uop
      highAddrLoad.fuTypeInMem := req.fuTypeInMem
      highAddrLoad.uop.exceptionVec(loadAddrMisaligned) := false.B

      switch (req.uop.fuOpType(1, 0)) {
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
  }

  io.splitLoadReq.valid := req_valid && (bufferState === s_req)
  io.splitLoadReq.bits  := splitLoadReqs(curPtr)

  when (io.splitLoadResp.valid) {
    splitLoadResp(curPtr) := io.splitLoadResp.bits
    when (isMMIO) {
      unSentLoads := 0.U
      splitLoadResp(curPtr).uop.exceptionVec := ExceptionNO.selectByFu(0.U.asTypeOf(ExceptionVec()), LduCfg)
      // delegate to software
      splitLoadResp(curPtr).uop.exceptionVec(loadAddrMisaligned) := true.B
    } .elsewhen (hasException) {
      unSentLoads := 0.U
    } .elsewhen (!io.splitLoadResp.bits.rep_info.need_rep) {
      unSentLoads := unSentLoads & ~UIntToOH(curPtr)
      curPtr := curPtr + 1.U
    }
  }

  val combinedData = RegInit(0.U(XLEN.W))

  when (bufferState === s_comb) {
    when (!cross16BytesBoundary) {
      val shiftData = LookupTree(aligned16BytesSel, List(
        "b0000".U -> splitLoadResp(0).data(63,     0),
        "b0001".U -> splitLoadResp(0).data(71,     8),
        "b0010".U -> splitLoadResp(0).data(79,    16),
        "b0011".U -> splitLoadResp(0).data(87,    24),
        "b0100".U -> splitLoadResp(0).data(95,    32),
        "b0101".U -> splitLoadResp(0).data(103,   40),
        "b0110".U -> splitLoadResp(0).data(111,   48),
        "b0111".U -> splitLoadResp(0).data(119,   56),
        "b1000".U -> splitLoadResp(0).data(127,   64),
        "b1001".U -> splitLoadResp(0).data(127,   72),
        "b1010".U -> splitLoadResp(0).data(127,   80),
        "b1011".U -> splitLoadResp(0).data(127,   88),
        "b1100".U -> splitLoadResp(0).data(127,   96),
        "b1101".U -> splitLoadResp(0).data(127,  104),
        "b1110".U -> splitLoadResp(0).data(127,  112),
        "b1111".U -> splitLoadResp(0).data(127,  120)
      ))
      val truncateData = LookupTree(req.uop.fuOpType(1, 0), List(
        LB -> shiftData(7,  0), // lb
        LH -> shiftData(15, 0), // lh
        LW -> shiftData(31, 0), // lw
        LD -> shiftData(63, 0)  // ld
      ))
      combinedData := rdataHelper(req.uop, truncateData(XLEN - 1, 0))
    } .otherwise {
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
      combinedData := rdataHelper(req.uop, (catResult.asUInt)(XLEN - 1, 0))
    }
  }
  val exceptionVecSelect = Mux(
    globalMMIO || globalException,
    splitLoadResp(curPtr).uop.exceptionVec,
    0.U.asTypeOf(ExceptionVec()))

  io.writeBack.valid := req_valid && (bufferState === s_wb)
  io.writeBack.bits.uop := req.uop
  io.writeBack.bits.uop.fuType := convertTofuType(req.fuTypeInMem)
  io.writeBack.bits.uop.exceptionVec := ExceptionNO.selectByFu(exceptionVecSelect, LduCfg) // TODO: is this ok?
  io.writeBack.bits.uop.flushPipe := Mux(globalMMIO || globalException, false.B, true.B)
  io.writeBack.bits.uop.replayInst := false.B
  io.writeBack.bits.data := combinedData
  io.writeBack.bits.debug.isMMIO := globalMMIO
  io.writeBack.bits.debug.isPerfCnt := false.B
  io.writeBack.bits.debug.paddr := req.paddr
  io.writeBack.bits.debug.vaddr := req.vaddr
  
  val flush = req_valid && req.uop.robIdx.needFlush(io.redirect)

  when (flush && (bufferState =/= s_idle)) {
    bufferState := s_idle
    req_valid := false.B
    curPtr := 0.U
    unSentLoads := 0.U
    globalException := false.B
    globalMMIO := false.B
  }

  // NOTE: spectial case (unaligned load cross page, page fault happens in next page)
  // if exception happens in the higher page address part, overwrite the loadExceptionBuffer vaddr
  val overwriteExpBuf = GatedValidRegNext(req_valid && cross16BytesBoundary && globalException && (curPtr === 1.U))
  val overwriteAddr = GatedRegNext(splitLoadResp(curPtr).vaddr)

  io.overwriteExpBuf.valid := overwriteExpBuf
  io.overwriteExpBuf.vaddr := overwriteAddr

  // when no exception or mmio, flush loadExceptionBuffer at s_wb
  val flushLdExpBuff = GatedValidRegNext(req_valid && (bufferState === s_wb) && !(globalMMIO || globalException))
  io.flushLdExpBuff := flushLdExpBuff

  XSPerfAccumulate("alloc",                  RegNext(!req_valid) && req_valid)
  XSPerfAccumulate("flush",                  flush)
  XSPerfAccumulate("flush_idle",             flush && (bufferState === s_idle))
  XSPerfAccumulate("flush_non_idle",         flush && (bufferState =/= s_idle))
}