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

  def selectOldest[T <: LsPipelineBundle](valid: Seq[Bool], bits: Seq[T]): (Seq[Bool], Seq[T]) = {
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
    val req             = Vec(enqPortNum, Flipped(Valid(new LsPipelineBundle)))
    val rob             = Flipped(new RobLsqIO)
    val splitStoreReq   = Decoupled(new LsPipelineBundle)
    val splitStoreResp  = Flipped(Valid(new SqWriteBundle))
    val writeBack       = Decoupled(new MemExuOutput)
    val overwriteExpBuf = Output(new XSBundle {
      val valid = Bool()
      val vaddr = UInt(VAddrBits.W)
    })
    val sqControl       = new StoreMaBufToSqControlIO
  })

  io.rob.mmio := 0.U.asTypeOf(Vec(LoadPipelineWidth, Bool()))
  io.rob.uop  := 0.U.asTypeOf(Vec(LoadPipelineWidth, new DynInst))

  val req_valid = RegInit(false.B)
  val req = Reg(new LsPipelineBundle)

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
  val s2_miss_aligned = s2_req.map(x => x.uop.exceptionVec(storeAddrMisaligned))

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

  val robMatch = req_valid && io.rob.pendingst && (io.rob.pendingPtr === req.uop.robIdx)

  // buffer control:
  //  - split miss-aligned store into aligned stores
  //  - send split store to sta and get result from sta
  //  - control sq write to sb
  //  - control sq write this store back
  val s_idle :: s_split :: s_req :: s_resp :: s_cal :: s_sq_req :: s_wb :: s_wait :: Nil = Enum(8)
  val bufferState = RegInit(s_idle)
  val splitStoreReqs = RegInit(VecInit(List.fill(maxSplitNum)(0.U.asTypeOf(new LsPipelineBundle))))
  val splitStoreResp = RegInit(VecInit(List.fill(maxSplitNum)(0.U.asTypeOf(new SqWriteBundle))))
  val unSentStores  = RegInit(0.U(maxSplitNum.W))
  val unWriteStores = RegInit(0.U(maxSplitNum.W))
  val curPtr = RegInit(0.U(log2Ceil(maxSplitNum).W))

  // if there is exception or mmio in split store
  val globalException = RegInit(false.B)
  val globalMMIO = RegInit(false.B)

  val hasException = ExceptionNO.selectByFu(io.splitStoreResp.bits.uop.exceptionVec, StaCfg).asUInt.orR && !io.splitStoreResp.bits.need_rep
  val isMMIO = io.splitStoreResp.bits.mmio && !io.splitStoreResp.bits.need_rep

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
        } .elsewhen(io.splitStoreResp.bits.need_rep || (unSentStores & ~clearOh).orR) {
          // need replay or still has unsent requests
          bufferState := s_req
        } .otherwise {
          // got result, goto calculate data and control sq
          bufferState := s_cal
        }
      }
    }

    is (s_cal) {
      when (io.sqControl.storeInfo.dataReady) {
        bufferState := s_sq_req
        curPtr := 0.U
      }
    }

    is (s_sq_req) {
      when (io.sqControl.storeInfo.completeSbTrans) {
        when (!((unWriteStores & ~UIntToOH(curPtr)).orR)) {
          bufferState := s_wb
        }
      }
    }

    is (s_wb) {
      when (io.writeBack.fire) {
        bufferState := s_wait
      }
    }

    is (s_wait) {
      when (io.rob.scommit =/= 0.U || req.uop.robIdx.needFlush(io.redirect)) {
        // rob commits the unaligned store or handled the exception, reset all state
        bufferState := s_idle
        req_valid := false.B
        curPtr := 0.U
        unSentStores := 0.U
        unWriteStores := 0.U
        globalException := false.B
        globalMMIO := false.B
      }
    }
  }

  val highAddress = LookupTree(req.uop.fuOpType(1, 0), List(
    SB -> 0.U,
    SH -> 1.U,
    SW -> 3.U,
    SD -> 7.U 
  )) + req.vaddr(4, 0)
  // to see if (vaddr + opSize - 1) and vaddr are in the same 16 bytes region
  val cross16BytesBoundary = req_valid && (highAddress(4) =/= req.vaddr(4))
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
      // change this unaligned store into a 128 bits store
      unWriteStores := 1.U
      unSentStores := 1.U
      curPtr := 0.U
      new128Store.vaddr := aligned16BytesAddr
      // new128Store.mask  := (getMask(req.uop.fuOpType(1, 0)) << aligned16BytesSel).asUInt
      new128Store.mask  := 0xffff.U
      new128Store.uop   := req.uop
      new128Store.uop.exceptionVec(storeAddrMisaligned) := false.B
      new128Store.is128bit := true.B
      splitStoreReqs(0) := new128Store
    } .otherwise {
      // split this unaligned store into `maxSplitNum` aligned stores
      unWriteStores := Fill(maxSplitNum, 1.U(1.W))
      unSentStores := Fill(maxSplitNum, 1.U(1.W))
      curPtr := 0.U
      lowAddrStore.uop := req.uop
      lowAddrStore.uop.exceptionVec(storeAddrMisaligned) := false.B
      highAddrStore.uop := req.uop
      highAddrStore.uop.exceptionVec(storeAddrMisaligned) := false.B

      switch (req.uop.fuOpType(1, 0)) {
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

  when (io.splitStoreResp.valid) {
    splitStoreResp(curPtr) := io.splitStoreResp.bits
    when (isMMIO) {
      unWriteStores := 0.U
      unSentStores := 0.U
      splitStoreResp(curPtr).uop.exceptionVec := 0.U.asTypeOf(ExceptionVec())
      // delegate to software
      splitStoreResp(curPtr).uop.exceptionVec(storeAddrMisaligned) := true.B
    } .elsewhen (hasException) {
      unWriteStores := 0.U
      unSentStores := 0.U
    } .elsewhen (!io.splitStoreResp.bits.need_rep) {
      unSentStores := unSentStores & ~UIntToOH(curPtr)
      curPtr := curPtr + 1.U
    }
  }

  val splitStoreData = RegInit(VecInit(List.fill(maxSplitNum)(0.U.asTypeOf(new XSBundle {
    val wdata = UInt(VLEN.W)
    val wmask = UInt((VLEN / 8).W)
  }))))

  val unalignedStoreData = io.sqControl.storeInfo.data
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

  when (bufferState === s_cal) {
    when (!cross16BytesBoundary) {
      splitStoreData(0).wdata := LookupTree(aligned16BytesSel, List(
        "b0000".U ->     unalignedStoreData,
        "b0001".U -> Cat(unalignedStoreData, 0.U(( 1 * 8).W)),
        "b0010".U -> Cat(unalignedStoreData, 0.U(( 2 * 8).W)),
        "b0011".U -> Cat(unalignedStoreData, 0.U(( 3 * 8).W)),
        "b0100".U -> Cat(unalignedStoreData, 0.U(( 4 * 8).W)),
        "b0101".U -> Cat(unalignedStoreData, 0.U(( 5 * 8).W)),
        "b0110".U -> Cat(unalignedStoreData, 0.U(( 6 * 8).W)),
        "b0111".U -> Cat(unalignedStoreData, 0.U(( 7 * 8).W)),
        "b1000".U -> Cat(unalignedStoreData, 0.U(( 8 * 8).W)),
        "b1001".U -> Cat(unalignedStoreData, 0.U(( 9 * 8).W)),
        "b1010".U -> Cat(unalignedStoreData, 0.U((10 * 8).W)),
        "b1011".U -> Cat(unalignedStoreData, 0.U((11 * 8).W)),
        "b1100".U -> Cat(unalignedStoreData, 0.U((12 * 8).W)),
        "b1101".U -> Cat(unalignedStoreData, 0.U((13 * 8).W)),
        "b1110".U -> Cat(unalignedStoreData, 0.U((14 * 8).W)),
        "b1111".U -> Cat(unalignedStoreData, 0.U((15 * 8).W))
      ))(VLEN - 1, 0)
      splitStoreData(0).wmask := getMask(req.uop.fuOpType(1, 0)) << aligned16BytesSel
    } .otherwise {
      // low 16bytes part
      val catData = LookupTree(lowResultWidth, List(
        BYTE0 -> unalignedStoreData,
        BYTE1 -> Cat(unalignedStoreData, 0.U((8 * 15).W)),
        BYTE2 -> Cat(unalignedStoreData, 0.U((8 * 14).W)),
        BYTE3 -> Cat(unalignedStoreData, 0.U((8 * 13).W)),
        BYTE4 -> Cat(unalignedStoreData, 0.U((8 * 12).W)),
        BYTE5 -> Cat(unalignedStoreData, 0.U((8 * 11).W)),
        BYTE6 -> Cat(unalignedStoreData, 0.U((8 * 10).W)),
        BYTE7 -> Cat(unalignedStoreData, 0.U((8 *  9).W))
      ))
      splitStoreData(0).wdata := catData(VLEN - 1, 0)
      splitStoreData(0).wmask := VecInit(wmaskLow.reverse).asUInt
      // high 16bytes part
      val shiftData = LookupTree(lowResultWidth, List(
        BYTE0 -> unalignedStoreData(VLEN - 1,    0),
        BYTE1 -> unalignedStoreData(VLEN - 1,    8),
        BYTE2 -> unalignedStoreData(VLEN - 1,   16),
        BYTE3 -> unalignedStoreData(VLEN - 1,   24),
        BYTE4 -> unalignedStoreData(VLEN - 1,   32),
        BYTE5 -> unalignedStoreData(VLEN - 1,   40),
        BYTE6 -> unalignedStoreData(VLEN - 1,   48),
        BYTE7 -> unalignedStoreData(VLEN - 1,   56)
      ))
      splitStoreData(1).wdata := LookupTree(highResultWidth, List(
        BYTE0 -> ZeroExt(shiftData, VLEN),
        BYTE1 -> ZeroExt(shiftData(7,    0), VLEN),
        BYTE2 -> ZeroExt(shiftData(15,   0), VLEN),
        BYTE3 -> ZeroExt(shiftData(23,   0), VLEN),
        BYTE4 -> ZeroExt(shiftData(31,   0), VLEN),
        BYTE5 -> ZeroExt(shiftData(39,   0), VLEN),
        BYTE6 -> ZeroExt(shiftData(47,   0), VLEN),
        BYTE7 -> ZeroExt(shiftData(55,   0), VLEN)
      ))
      splitStoreData(1).wmask := wmaskHigh.asUInt
    }
  }

  io.sqControl.control.hasException := req_valid && globalException

  io.sqControl.control.writeSb := bufferState === s_sq_req
  io.sqControl.control.wdata   := splitStoreData(curPtr).wdata
  io.sqControl.control.wmask   := splitStoreData(curPtr).wmask
  // the paddr and vaddr is not corresponding to the exact addr of 
  io.sqControl.control.paddr   := splitStoreResp(curPtr).paddr
  io.sqControl.control.vaddr   := splitStoreResp(curPtr).vaddr
  io.sqControl.control.last    := !((unWriteStores & ~UIntToOH(curPtr)).orR)

  when (bufferState === s_sq_req) {
    when (io.sqControl.storeInfo.completeSbTrans) {
      unWriteStores := unWriteStores & ~UIntToOH(curPtr)
      curPtr := curPtr + 1.U
    }
  }

  io.writeBack.valid := req_valid && (bufferState === s_wb) && io.sqControl.storeInfo.dataReady
  io.writeBack.bits.uop := req.uop
  io.writeBack.bits.uop.exceptionVec := Mux(
    globalMMIO || globalException,
    splitStoreResp(curPtr).uop.exceptionVec,
    0.U.asTypeOf(ExceptionVec()) // TODO: is this ok?
  )
  io.writeBack.bits.uop.flushPipe := Mux(globalMMIO || globalException, false.B, true.B)
  io.writeBack.bits.uop.replayInst := false.B
  io.writeBack.bits.data := unalignedStoreData
  io.writeBack.bits.debug.isMMIO := globalMMIO
  io.writeBack.bits.debug.isPerfCnt := false.B
  io.writeBack.bits.debug.paddr := req.paddr
  io.writeBack.bits.debug.vaddr := req.vaddr

  io.sqControl.control.removeSq := req_valid && (bufferState === s_wait) && !(globalMMIO || globalException) && (io.rob.scommit =/= 0.U)
  
  val flush = req_valid && req.uop.robIdx.needFlush(io.redirect)

  when (flush && (bufferState =/= s_idle)) {
    bufferState := s_idle
    req_valid := false.B
    curPtr := 0.U
    unSentStores := 0.U
    unWriteStores := 0.U
    globalException := false.B
    globalMMIO := false.B
  }

  // NOTE: spectial case (unaligned store cross page, page fault happens in next page)
  // if exception happens in the higher page address part, overwrite the storeExceptionBuffer vaddr
  val overwriteExpBuf = GatedValidRegNext(req_valid && cross16BytesBoundary && globalException && (curPtr === 1.U))
  val overwriteAddr = GatedRegNext(splitStoreResp(curPtr).vaddr)

  io.overwriteExpBuf.valid := overwriteExpBuf
  io.overwriteExpBuf.vaddr := overwriteAddr

  XSPerfAccumulate("alloc",                  RegNext(!req_valid) && req_valid)
  XSPerfAccumulate("flush",                  flush)
  XSPerfAccumulate("flush_idle",             flush && (bufferState === s_idle))
  XSPerfAccumulate("flush_non_idle",         flush && (bufferState =/= s_idle))
}