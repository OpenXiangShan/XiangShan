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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.rob.RobPtr

class VlflowPtr(implicit p: Parameters) extends CircularQueuePtr[VlflowPtr](
  p => p(XSCoreParamsKey).VlFlowSize
){
}

object VlflowPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): VlflowPtr = {
    val ptr = Wire(new VlflowPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class UsQueuePtr(implicit p: Parameters) extends CircularQueuePtr[UsQueuePtr](
  p => p(XSCoreParamsKey).UsQueueSize
){
}

object UsQueuePtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): UsQueuePtr = {
    val ptr = Wire(new UsQueuePtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

/**
 * (1) unit-stride instructions access to memory continously, so calculate the address by adding 16 directly (flow_inner_idx << 4.U)
 * (2) stride instructions: flow_inner_idx means the current number of UOP memory accesses,
 *     inner_Idx << Log2Num(GenRealFlowNum(instType,emul,eew,sew)) means the number of all previous UOP memory accesses
 * (3) index instructions: According to flow_ inner_idx obtains immediate value from index, than Calculate address
 * (4) segment instructions: To calculate the address, segment instructions need calculate segEmulIdx and segNfIdx;
 * */
object GenVLAddr {
  def apply (instType: UInt, baseaddr: UInt, emul:UInt, lmul:UInt, inner_Idx:UInt, flow_inner_idx: UInt, stride: UInt,
             index: UInt, eew: UInt, sew: UInt, nf:UInt, segNfIdx: UInt, segEmulIdx: UInt): UInt = {
    (LookupTree(instType,List(
      "b000".U -> (baseaddr + (flow_inner_idx << 4.U).asUInt).asUInt,// unit-stride
      "b010".U -> (baseaddr + stride * (flow_inner_idx + (inner_Idx << Log2Num(GenRealFlowNum(instType,emul,lmul,eew,sew))).asUInt)),// strided
      "b001".U -> (baseaddr +
                    IndexAddr(index= index, flow_inner_idx = (flow_inner_idx + (inner_Idx << Log2Num(GenRealFlowNum(instType,emul,lmul,eew,sew))).asUInt).asUInt, eew = eew)), // indexed-unordered
      "b011".U -> (baseaddr +
                    IndexAddr(index= index, flow_inner_idx = (flow_inner_idx + (inner_Idx << Log2Num(GenRealFlowNum(instType,emul,lmul,eew,sew))).asUInt).asUInt, eew = eew)), // indexed-ordered
      "b100".U -> (baseaddr +
                    (((flow_inner_idx + (segEmulIdx << Log2Num(GenRealFlowNum(instType,emul,lmul,eew,sew))).asUInt).asUInt * nf) << eew(1,0)).asUInt +
                    (segNfIdx << eew(1,0)).asUInt),// segment unit-stride
      "b110".U -> (baseaddr +
                    (flow_inner_idx + (segEmulIdx << Log2Num(GenRealFlowNum(instType,emul,lmul,eew,sew))).asUInt).asUInt * stride +
                    (segNfIdx << eew(1,0)).asUInt), // segment strided
      "b101".U -> (baseaddr +
                    IndexAddr(index= index, flow_inner_idx = (flow_inner_idx + (segEmulIdx << Log2Num(GenRealFlowNum(instType,emul,lmul,eew,sew))).asUInt).asUInt, eew = eew) +
                    (segNfIdx << sew(1,0)).asUInt), // segment indexed-unordered
      "b111".U -> (baseaddr +
                    IndexAddr(index= index, flow_inner_idx = (flow_inner_idx + (segEmulIdx << Log2Num(GenRealFlowNum(instType,emul,lmul,eew,sew))).asUInt).asUInt, eew = eew) +
                    (segNfIdx << sew(1,0)).asUInt)  // segment indexed-ordered
    )))}
}
/*
object GenRobIdx {
  def apply (instType: UInt, robIdx:UInt, startRobIdx: UInt, flow_inner_idx: UInt): UInt = {
    (LookupTree(instType,List(
      "b000".U ->  (startRobIdx + flow_inner_idx),// unit-stride, do not use
      "b010".U ->  (robIdx), // strided
      "b001".U ->  (robIdx), // indexed-unordered
      "b011".U ->  (robIdx), // indexed-ordered
      "b100".U ->  (robIdx), // segment unit-stride
      "b110".U ->  (robIdx), // segment strided
      "b101".U ->  (robIdx), // segment indexed-unordered
      "b111".U ->  (robIdx)  // segment indexed-ordered
    )))}
}*/

object VLRegOffset {
  def apply (instType: UInt, flow_inner_idx: UInt, eew: UInt, sew: UInt): UInt = {
    (LookupTree(instType,List(
      "b000".U ->  0.U                           , // unit-stride
      "b010".U ->  (flow_inner_idx << eew(1,0)).asUInt, // strided
      "b001".U ->  (flow_inner_idx << sew(1,0)).asUInt, // indexed-unordered
      "b011".U ->  (flow_inner_idx << sew(1,0)).asUInt, // indexed-ordered
      "b100".U ->  (flow_inner_idx << eew(1,0)).asUInt, // segment unit-stride
      "b110".U ->  (flow_inner_idx << eew(1,0)).asUInt, // segment strided
      "b101".U ->  (flow_inner_idx << sew(1,0)).asUInt, // segment indexed-unordered
      "b111".U ->  (flow_inner_idx << sew(1,0)).asUInt  // segment indexed-ordered
    )))}
}

class VecLoadPipelineBundle(implicit p: Parameters) extends XSBundleWithMicroOp{
  val vaddr               = UInt(VAddrBits.W)
  val mask                = UInt((VLEN/8).W)
  val uop_unit_stride_fof = Bool()
  val rob_idx_valid       = Vec(2,Bool())
  val rob_idx             = Vec(2,new RobPtr)
  val inner_idx           = Vec(2,UInt(3.W))
  val reg_offset          = Vec(2,UInt(4.W))
  val offset              = Vec(2,UInt(4.W))
  val alignedType         = UInt(2.W)
}

class VlflowQueueIOBundle(implicit p: Parameters) extends XSBundle {
  val loadRegIn    = Vec(VecLoadPipelineWidth, Flipped(Decoupled(new ExuInput(isVpu = true))))
  val Redirect     = Flipped(ValidIO(new Redirect))
  val flowFeedback = Vec(VecLoadPipelineWidth, ValidIO(Bool()))
  val eew          = Vec(VecLoadPipelineWidth, Input(UInt(3.W)))
  val sew          = Vec(VecLoadPipelineWidth, Input(UInt(3.W)))
  val emul         = Vec(VecLoadPipelineWidth, Input(UInt(3.W)))
  val instType     = Vec(VecLoadPipelineWidth, Input(UInt(3.W)))
  val uop_unit_stride_fof = Vec(VecLoadPipelineWidth, Input(Bool()))
  val uop_segment_num = Vec(VecLoadPipelineWidth, Input(UInt(3.W)))
  val loadPipeOut = Vec(VecLoadPipelineWidth, Decoupled(new VecLoadPipelineBundle))
}

class VlflowBundle(implicit p: Parameters) extends XSBundle {
  val uop               = new MicroOp
  val vaddr             = UInt(VAddrBits.W)
  val mask              = UInt((VLEN/8).W)
  val unit_stride_fof   = Bool()
  val rob_idx_valid     = Vec(2,Bool())
  val rob_idx           = Vec(2,new RobPtr)
  val inner_idx         = Vec(2,UInt(3.W))
  val reg_offset        = Vec(2,UInt(4.W)) //Which element to write
  val offset            = Vec(2,UInt(4.W))
  val alignedType       = UInt(2.W)
}

class unitStrideBundle(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val counter = UInt(4.W)
}

class VlflowQueue(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper
{
  val io = IO(new VlflowQueueIOBundle())
  println("LoadFlowQueue: size:" + VlFlowSize)

  dontTouch(io.loadRegIn)
  // TODO: merge these to an FlowQueue Entry?
  val flow_entry       = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(VlFlowSize)(0.U.asTypeOf(new VlflowBundle))))))
  val flow_entry_valid = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(VlFlowSize)(false.B)))))
  val unitStrideValid  = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(UsQueueSize)(false.B)))))
  val unitStrideEntry  = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(UsQueueSize)(0.U.asTypeOf(new unitStrideBundle))))))

  val loadRegInValid  = WireInit(VecInit(Seq.fill(VecStorePipelineWidth)(false.B)))
  val UsNeedFlush     = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(UsQueueSize)(false.B)))))
  val needFlush       = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(VlFlowSize)(false.B)))))
  val UsRedirectCnt   = RegInit(VecInit(Seq.fill(VecStorePipelineWidth)(0.U(log2Up(UsQueueSize).W))))
  val flowRedirectCnt = RegInit(VecInit(Seq.fill(VecStorePipelineWidth)(0.U(log2Up(VsFlowSize).W))))
  val cam             = WireInit(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(VecLoadPipelineWidth)(VecInit(Seq.fill(UsQueueSize)(false.B)))))))
  val uSAlloc         = Wire(Vec(VecLoadPipelineWidth,Bool()))
  val needAlloc       = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val baseAddr        = Wire(Vec(VecLoadPipelineWidth, UInt(VAddrBits.W)))
  val dataWidth       = Wire(Vec(VecLoadPipelineWidth, UInt(8.W))) // only unit-stride use
  val vend            = Wire(Vec(VecLoadPipelineWidth, UInt(8.W)))
  val realFlowNum     = Wire(Vec(VecLoadPipelineWidth, UInt(5.W)))

  val cross128    = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val instType    = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
  val stride      = Wire(Vec(VecLoadPipelineWidth, UInt(XLEN.W)))
  val index       = Wire(Vec(VecLoadPipelineWidth, UInt(VLEN.W)))
  val eew         = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
  val sew         = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
  val emul        = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
  val lmul        = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
  val mul         = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
  val segEmulIdx  = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
  val segNfIdx    = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))

  val enqPtr = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U.asTypeOf(new VlflowPtr))))
  val deqPtr = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U.asTypeOf(new VlflowPtr))))
  val uSEnqPtr = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U.asTypeOf(new UsQueuePtr))))
  val uSDeqPtr = RegInit(VecInit(Seq.fill(VecLoadPipelineWidth)(0.U.asTypeOf(new UsQueuePtr))))

  for (i <- 0 until VecLoadPipelineWidth) {
    io.loadRegIn(i).ready := PopCount(flow_entry_valid(i)) <= 16.U && PopCount(unitStrideValid(i)) <= 7.U//TODO:
  }

  /**
    * vlFLowQueue enqPtr update */
  val lastRedirect = RegNext(io.Redirect)
  for (i <- 0 until VecLoadPipelineWidth) {
    flowRedirectCnt(i) := RegNext(PopCount(needFlush(i)))
    when (lastRedirect.valid) {
      enqPtr(i).value := enqPtr(i).value - flowRedirectCnt(i)
    }.otherwise {
      when (needAlloc(i)) {
        enqPtr(i).value := enqPtr(i).value + realFlowNum(i)
      }
    }
  }

  /**
    * unitStrideQueue enqPtr update */
  for (i <- 0 until VecLoadPipelineWidth) {
    UsRedirectCnt(i)   := RegNext(PopCount(UsNeedFlush(i)))
    when (lastRedirect.valid) {
      uSEnqPtr(i).value := uSEnqPtr(i).value - UsRedirectCnt(i)
    }.otherwise {
      when (uSAlloc(i)) {
        uSEnqPtr(i).value := uSEnqPtr(i).value + 1.U
      }
    }
  }

  for (i <- 0 until VecLoadPipelineWidth) {
    when (instType(i) === "b000".U) {
      for (j <- 0 until VecLoadPipelineWidth) {
        for (k <- 0 until UsQueueSize) {
          cam(i)(j)(k) := unitStrideValid(j)(k) &&
            io.loadRegIn(i).bits.uop.robIdx.value === unitStrideEntry(j)(k).robIdx.value
          when (cam(0)(j)(k) && cam(1)(j)(k) && io.loadRegIn(0).valid && io.loadRegIn(1).valid) {
            unitStrideEntry(j)(k).counter := unitStrideEntry(j)(k).counter - 2.U
          }.elsewhen (cam(i)(j)(k) && io.loadRegIn(i).valid) {
            unitStrideEntry(j)(k).counter := unitStrideEntry(j)(k).counter - 1.U
          }
        }
      }
    }
  }

  for (i <- 0 until VecLoadPipelineWidth) {
    io.flowFeedback(i).valid := io.loadRegIn(i).valid
    io.flowFeedback(i).bits := cam(i).asUInt.orR
  }

  val sameInst = loadRegInValid(0) && loadRegInValid(1) && instType(0) === "b000".U && instType(1) === "b000".U &&
    (io.loadRegIn(0).bits.uop.robIdx.value === io.loadRegIn(1).bits.uop.robIdx.value)

  for (i <- 0 until VecLoadPipelineWidth) {
    when (instType(i) === "b000".U) {
      when (sameInst) {
        when (i.U === 0.U) {
          uSAlloc(i) := !cam(i).asUInt.orR && loadRegInValid(0)
        }.otherwise {
          uSAlloc(i) := false.B
        }
      }.otherwise {
        uSAlloc(i) := !cam(i).asUInt.orR && loadRegInValid(i)
      }
    }.otherwise {
      uSAlloc(i) := false.B
    }
  }

  //queue update
  for (i <- 0 until VecLoadPipelineWidth) {
    when (uSAlloc(i)) {
      unitStrideValid(i)(uSEnqPtr(i).value) := true.B
      unitStrideEntry(i)(uSEnqPtr(i).value).robIdx := io.loadRegIn(i).bits.uop.robIdx
      when (sameInst) {
        unitStrideEntry(0)(uSEnqPtr(0).value).counter := io.loadRegIn(0).bits.uop.ctrl.total_num - 1.U
      }.otherwise {
        unitStrideEntry(i)(uSEnqPtr(i).value).counter := io.loadRegIn(i).bits.uop.ctrl.total_num
      }
    }
  }

  //deqPtr update
  for (i <- 0 until VecLoadPipelineWidth) {
    when (unitStrideEntry(i)(uSDeqPtr(i).value).counter === 0.U && unitStrideValid(i)(uSDeqPtr(i).value)) {
      uSDeqPtr(i).value := uSDeqPtr(i).value + 1.U
      unitStrideValid(i)(uSDeqPtr(i).value) := false.B
    }
  }
/**
  * Redirection occurred, flush flowQueue and unitStrideQueue*/
  for (i <- 0 until VecLoadPipelineWidth) {
    for (entry <- 0 until VlFlowSize) {
      needFlush(i)(entry) := flow_entry(i)(entry).rob_idx(0).needFlush(io.Redirect) && flow_entry_valid(i)(entry)
      when (needFlush(i)(entry)) {
        flow_entry_valid(i)(entry) := false.B
        flow_entry(i)(entry).mask := 0.U
        flow_entry(i)(entry).rob_idx_valid := VecInit(Seq.fill(2)(false.B))
      }
    }
    for (entry <- 0 until UsQueueSize) {
      UsNeedFlush(i)(entry) := unitStrideEntry(i)(uSEnqPtr(i).value).robIdx.needFlush(io.Redirect) && unitStrideValid(i)(entry)
      when (UsNeedFlush(i)(entry)) {
        unitStrideValid(i)(entry) := false.B
      }
    }
    loadRegInValid(i) := !io.loadRegIn(i).bits.uop.robIdx.needFlush(io.Redirect) && io.loadRegIn(i).fire
  }

  for (i <- 0 until VecLoadPipelineWidth) {
    when (instType(i) === "b000".U) { // unit-stride Inst
      needAlloc(i) := uSAlloc(i)
    }.otherwise {
      needAlloc(i) := loadRegInValid(i)
    }
  }

  for (i <- 0 until VecLoadPipelineWidth) {
    //loadInstDec(i).apply(io.loadRegIn(i).bits.uop.cf.instr)
    stride(i)          := io.loadRegIn(i).bits.src(1)
    index(i)           := io.loadRegIn(i).bits.src(1)
    eew(i)             := io.eew(i)
    sew(i)             := io.sew(i)
    emul(i)            := io.emul(i)
    lmul(i)            := io.loadRegIn(i).bits.uop.ctrl.vconfig.vtype.vlmul
    mul(i)             := Mux(instType(i)(1,0) === "b00".U || instType(i)(1,0) === "b10".U,emul(i),lmul(i))
    instType(i)        := io.instType(i)
    baseAddr(i)        := io.loadRegIn(i).bits.src(0)
    dataWidth(i)       := io.loadRegIn(i).bits.uop.ctrl.vconfig.vl << eew(i)(1,0)// only unit-stride use
    vend(i)            := baseAddr(i)(3,0) + dataWidth(i)
    segEmulIdx(i)      := GenSegMulIdx(mul = mul(i), inner_Idx = io.loadRegIn(i).bits.uop.ctrl.uopIdx)
    segNfIdx(i)        := GenSegNfIdx(mul = mul(i),inner_Idx = io.loadRegIn(i).bits.uop.ctrl.uopIdx)
  }

  for (i <- 0 until VecLoadPipelineWidth) {
    when (instType(i) === "b000".U) { // unit-stride Inst
      realFlowNum(i)  := vend(i)(7,4) + (vend(i)(3,0) =/= 0.U).asUInt//TODO:************
      cross128(i)     := baseAddr(i)(3, 0) =/= 0.U(4.W)
    }.otherwise {
      realFlowNum(i)  := GenRealFlowNum(instType = instType(i), emul = emul(i), lmul = lmul(i), eew = eew(i), sew = sew(i))
      cross128(i)     := false.B
    }
  }

  /**
   * Update flowQueue status bits
   * instType(i) === "b000".U means only unit-stride instructions use this logic*/
  for (i <- 0 until VecLoadPipelineWidth) {
    when (needAlloc(i)) {
      //startRobIdx(i) := io.loadRegIn(i).bits.uop.robIdx.value - io.loadRegIn(i).bits.inner_idx
      for (j <- 0 until 16) {
        when (j.U < realFlowNum(i)) {
          val queueIdx = Wire(UInt(5.W))
          val vaddr = Wire(UInt(VAddrBits.W))
          val uop = Wire(new MicroOp)
          queueIdx := enqPtr(i).value + j.U
          flow_entry(i)(queueIdx) := DontCare
          uop := DontCare
          flow_entry_valid(i)(queueIdx) := true.B
          flow_entry(i)(queueIdx).unit_stride_fof := io.uop_unit_stride_fof(i)
          vaddr := GenVLAddr(instType = instType(i), baseaddr = baseAddr(i), emul = emul(i), lmul = lmul(i), inner_Idx = io.loadRegIn(i).bits.uop.ctrl.uopIdx,
                              flow_inner_idx = j.U, stride = stride(i), index = index(i), eew = eew(i), sew = sew(i),
                              nf = io.uop_segment_num(i) + 1.U, segNfIdx = segNfIdx(i), segEmulIdx = segEmulIdx(i))
          flow_entry(i)(queueIdx).mask := GenVecLoadMask(instType = instType(i), emul = emul(i), eew = eew(i), sew = sew(i))
          flow_entry(i)(queueIdx).vaddr := vaddr
          flow_entry(i)(queueIdx).alignedType := Mux(instType(i)(1,0) === "b00".U || instType(i)(1,0) === "b10".U,eew(i)(1,0),sew(i)(1,0))

          when (realFlowNum(i) === 1.U && instType(i) === "b000".U) {
            flow_entry(i)(queueIdx).uop := io.loadRegIn(i).bits.uop
            flow_entry(i)(queueIdx).rob_idx_valid(0) := true.B
            flow_entry(i)(queueIdx).rob_idx(0)    := io.loadRegIn(i).bits.uop.robIdx
            flow_entry(i)(queueIdx).inner_idx(0)  := Mux(instType(i) === "b000".U,j.U,io.loadRegIn(i).bits.uop.ctrl.uopIdx)
            flow_entry(i)(queueIdx).offset(0)     := vaddr(3, 0)
            flow_entry(i)(queueIdx).reg_offset(0) := VLRegOffset(instType = instType(i), flow_inner_idx = j.U, eew = eew(i), sew = sew(i))
          }.otherwise {
            when (j.U =/= realFlowNum(i) - 1.U) {
              uop := io.loadRegIn(i).bits.uop
              uop.lqIdx := io.loadRegIn(i).bits.uop.lqIdx - io.loadRegIn(i).bits.uop.ctrl.uopIdx + j.U
              flow_entry(i)(queueIdx).uop := Mux(instType(i) === "b000".U,uop,io.loadRegIn(i).bits.uop)
              flow_entry(i)(queueIdx).rob_idx_valid(0) := true.B
              flow_entry(i)(queueIdx).rob_idx(0) := io.loadRegIn(i).bits.uop.robIdx
              flow_entry(i)(queueIdx).inner_idx(0)  := Mux(instType(i) === "b000".U,j.U,io.loadRegIn(i).bits.uop.ctrl.uopIdx)
              flow_entry(i)(queueIdx).offset(0) := vaddr(3, 0)
              flow_entry(i)(queueIdx).reg_offset(0) := VLRegOffset(instType = instType(i), flow_inner_idx = j.U, eew = eew(i), sew = sew(i))
            }.elsewhen (j.U === realFlowNum(i) - 1.U && !cross128(i)) {
              uop := io.loadRegIn(i).bits.uop
              uop.lqIdx := io.loadRegIn(i).bits.uop.lqIdx - io.loadRegIn(i).bits.uop.ctrl.uopIdx + j.U
              flow_entry(i)(queueIdx).uop := Mux(instType(i) === "b000".U,uop,io.loadRegIn(i).bits.uop)
              flow_entry(i)(queueIdx).rob_idx_valid(0) := true.B
              flow_entry(i)(queueIdx).rob_idx(0) := io.loadRegIn(i).bits.uop.robIdx
              flow_entry(i)(queueIdx).inner_idx(0)  := Mux(instType(i) === "b000".U,j.U,io.loadRegIn(i).bits.uop.ctrl.uopIdx)
              flow_entry(i)(queueIdx).offset(0) := vaddr(3, 0)
              flow_entry(i)(queueIdx).reg_offset(0) := VLRegOffset(instType = instType(i), flow_inner_idx = j.U, eew = eew(i), sew = sew(i))
            }.elsewhen (j.U === realFlowNum(i) - 1.U && cross128(i)) {
              flow_entry(i)(queueIdx).rob_idx_valid(0) := false.B
            }

            when (j.U =/= 0.U && instType(i) === "b000".U) {
              flow_entry(i)(queueIdx).rob_idx_valid(1) := cross128(i)
              flow_entry(i)(queueIdx).rob_idx(1)       := io.loadRegIn(i).bits.uop.robIdx
              flow_entry(i)(queueIdx).inner_idx(1)     := j.U - cross128(i).asUInt
              flow_entry(i)(queueIdx).offset(1)        := 0.U
              flow_entry(i)(queueIdx).reg_offset(1)    := 16.U - vaddr(3, 0)
            }.otherwise {
              flow_entry(i)(queueIdx).rob_idx_valid(1) := false.B
            }
          }
        }
      }
    }
  }

  // flow deqPtr
  for (i <- 0 until LoadPipelineWidth) {
    // TODO: Need to do some changes
    //  1. DontCare?
    //  2. Other information?
    io.loadPipeOut(i).bits := DontCare
    io.loadPipeOut(i).valid := false.B
    when (flow_entry_valid(i)(deqPtr(i).value)){
      io.loadPipeOut(i).valid                    := true.B
      io.loadPipeOut(i).bits.uop_unit_stride_fof := flow_entry(i)(deqPtr(i).value).unit_stride_fof
      io.loadPipeOut(i).bits.vaddr               := flow_entry(i)(deqPtr(i).value).vaddr
      io.loadPipeOut(i).bits.mask                := flow_entry(i)(deqPtr(i).value).mask
      io.loadPipeOut(i).bits.rob_idx_valid       := flow_entry(i)(deqPtr(i).value).rob_idx_valid
      io.loadPipeOut(i).bits.rob_idx             := flow_entry(i)(deqPtr(i).value).rob_idx
      io.loadPipeOut(i).bits.inner_idx           := flow_entry(i)(deqPtr(i).value).inner_idx
      io.loadPipeOut(i).bits.offset              := flow_entry(i)(deqPtr(i).value).offset
      io.loadPipeOut(i).bits.reg_offset          := flow_entry(i)(deqPtr(i).value).reg_offset
      io.loadPipeOut(i).bits.uop.lqIdx           := flow_entry(i)(deqPtr(i).value).uop.lqIdx
      io.loadPipeOut(i).bits.alignedType         := flow_entry(i)(deqPtr(i).value).alignedType
    }
  }

  for (i <- 0 until LoadPipelineWidth) {
    when (io.loadPipeOut(i).fire) {
      flow_entry_valid(i)(deqPtr(i).value) := false.B
      flow_entry(i)(deqPtr(i).value).mask := 0.U
      flow_entry(i)(deqPtr(i).value).rob_idx_valid := VecInit(Seq.fill(2)(false.B))
      deqPtr(i) := deqPtr(i) + 1.U
    }
  }

}
