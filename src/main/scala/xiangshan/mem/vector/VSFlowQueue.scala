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
import org.scalatest.Assertions.===
import utils._
import utility._
import xiangshan._
import xiangshan.cache._
import xiangshan.backend.rob.RobLsqIO

class VsFlowPtr (implicit p: Parameters) extends CircularQueuePtr[VsFlowPtr](
  p => p(XSCoreParamsKey).VsFlowSize
){
}

object VsFlowPtr {
  def apply (f: Bool, v: UInt)(implicit p: Parameters): VsFlowPtr = {
    val ptr = Wire(new VsFlowPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}
/*
object VSRegOffset {
  def apply (instType: UInt, flowIdx: UInt, eew: UInt, sew: UInt):UInt = {
    (LookupTree(instType,List(
    "b000".U -> (flowIdx << eew(1,0)).asUInt, // unit-stride, do not use
    "b010".U -> (flowIdx << eew(1,0)).asUInt, // strided
    "b001".U -> (flowIdx << sew(1,0)).asUInt, // indexed-unordered
    "b011".U -> (flowIdx << sew(1,0)).asUInt, // indexed-ordered
    "b100".U -> (flowIdx << eew(1,0)).asUInt, // segment unit-stride
    "b110".U -> (flowIdx << eew(1,0)).asUInt, // segment strided
    "b101".U -> (flowIdx << sew(1,0)).asUInt, // segment indexed-unordered
    "b111".U -> (flowIdx << sew(1,0)).asUInt // segment indexed-ordered
    )))}
}*/

object VSRegOffset {
  def apply (instType: UInt, flowIdx: UInt, eew: UInt, sew: UInt):UInt = {
    (LookupTree(instType(1,0),List(
      "b00".U -> (flowIdx << eew(1,0)).asUInt, // (segment) unit-stride(don't use),
      "b10".U -> (flowIdx << eew(1,0)).asUInt, // (segment) strided
      "b01".U -> (flowIdx << sew(1,0)).asUInt, // (segment) indexed-unordered
      "b11".U -> (flowIdx << sew(1,0)).asUInt, // (segment) indexed-ordered
    )))}
}

/**
  * (1) unit-stride instructions access to memory continously, so calculate the address by adding 16 directly (flow_inner_idx << 4.U)
  * (2) stride instructions: flow_inner_idx means the current number of UOP memory accesses,
  *     uopIdx << Log2Num(GenRealFlowNum(instType,emul,eew,sew)) means the number of all previous UOP memory accesses
  * (3) index instructions: According to flow_ inner_idx obtains immediate value from index, than Calculate address
  * (4) segment instructions: To calculate the address, segment instructions need calculate segEmulIdx and segNfIdx;
  * */
object GenVSAddr {
  def apply (instType: UInt, baseaddr: UInt, eleIdx: UInt, emul:UInt, lmul: UInt, uopIdx:UInt, flow_inner_idx: UInt, stride: UInt,
             index: UInt, eew: UInt, sew: UInt, nf:UInt, segNfIdx: UInt, segMulIdx: UInt): UInt = {
    (LookupTree(instType,List(
      "b000".U -> (baseaddr + (eleIdx << eew(1,0)).asUInt).asUInt,// unit-stride
      "b010".U -> (baseaddr + stride * eleIdx),// strided
      "b001".U -> (baseaddr + IndexAddr(index= index, flow_inner_idx = flow_inner_idx, eew = eew)), // indexed-unordered
      "b011".U -> (baseaddr + IndexAddr(index= index, flow_inner_idx = flow_inner_idx, eew = eew)), // indexed-ordered
      "b100".U -> (baseaddr +
        (((flow_inner_idx + (segMulIdx << Log2Num(GenRealFlowNum(instType,emul,lmul,eew,sew))).asUInt).asUInt * nf) << eew(1,0)).asUInt +
        (segNfIdx << eew(1,0)).asUInt),// segment unit-stride
      "b110".U -> (baseaddr +
        (flow_inner_idx + (segMulIdx << Log2Num(GenRealFlowNum(instType,emul,lmul,eew,sew))).asUInt).asUInt * stride +
        (segNfIdx << eew(1,0)).asUInt), // segment strided
      "b101".U -> (baseaddr + IndexAddr(index= index, flow_inner_idx = flow_inner_idx, eew = eew) + (segNfIdx << sew(1,0)).asUInt), // segment indexed-unordered
      "b111".U -> (baseaddr + IndexAddr(index= index, flow_inner_idx = flow_inner_idx, eew = eew) + (segNfIdx << sew(1,0)).asUInt)  // segment indexed-ordered
    )))}
}

object VSMaskCtrl {
  def apply (vstart: UInt, vl: UInt, eleIdx: UInt, vmask: UInt, mask: UInt, vma: Bool, vta: Bool) :(UInt,Bool) = {
    val vsMask = Wire(UInt(16.W))
    val exp = Wire(Bool())
    when (vstart >= vl || vl === 0.U) {
      vsMask := 0.U
      exp := false.B
    }.otherwise {
      when (eleIdx >= vstart && eleIdx < vl) {
        exp := true.B
        when(vmask === false.B && vma === false.B) {
          vsMask := 0.U
        }.otherwise {
          vsMask := mask
        }
      }.elsewhen(eleIdx >= vl) {
        exp := false.B
        when(vta === false.B) {
          vsMask := 0.U
        }.otherwise {
          vsMask := "hff".U
        }
      }.otherwise{
        vsMask := 0.U
        exp := false.B
      }
    }
    (vsMask,exp)
  }
}

object GenVSMask {
  def apply(reg_offset: UInt, offset: UInt, mask: UInt):UInt = {
    val vMask = Wire(UInt(16.W))
    when (offset <= reg_offset) {
      vMask := mask >> (reg_offset - offset)
    }.otherwise {
      vMask := mask << (offset - reg_offset)
    }
    vMask
  }
}

object GenVSData {
    def apply(reg_offset: UInt, offset: UInt, data: UInt,  vstart: UInt, vl: UInt, eleIdx: UInt, vmask: UInt, vma: Bool, vta: Bool):UInt = {
      val vtmpData = Wire(UInt(128.W))
      val vData = Wire(UInt(128.W))
      when (offset <= reg_offset) {
        vtmpData := data >> ((reg_offset - offset) << 3.U)
      }.otherwise {
        vtmpData := data << ((offset - reg_offset) << 3.U)
      }

      when( eleIdx >= vl) {
        when(vta){
          vData := "hfffffffff".U
        }.otherwise{
          vData := vtmpData
        }
      }.elsewhen(eleIdx >= vstart && eleIdx < vl) {
        when(vmask(eleIdx)) {
          vData := vtmpData
        }.otherwise {
          when(vma) {
            vData := "hfffffffff".U
          }.otherwise {
            vData := vtmpData
          }
        }
      }.otherwise{
        vData := vtmpData
      }
      vData
    }
}

object VSFQFeedbackType {
  val tlbMiss = 0.U(3.W)
  val mshrFull = 1.U(3.W)
  val dataInvalid = 2.U(3.W)
  val bankConflict = 3.U(3.W)
  val ldVioCheckRedo = 4.U(3.W)
  val feedbackInvalid = 7.U(3.W)

  def apply() = UInt(3.W)
}

class VSFQFeedback (implicit p: Parameters) extends XSBundle {
  val fqIdx = UInt(log2Up(VsFlowSize).W)
  val hit   = Bool()
  //val flushState = Bool()
  val sourceType = VSFQFeedbackType()
  //val dataInvalidSqIdx = new SqPtr
}

class VecFlowEntry (implicit p: Parameters) extends ExuInput(isVpu = true) {
  val exp                 = Bool()
  val mask                = UInt((VLEN/8).W)
  val uop_unit_stride_fof = Bool()
  val alignedType         = UInt(2.W)
  val reg_offset          = UInt(4.W)
  val uop_idx             = UInt(6.W)
  val vstart              = UInt(8.W)
  val vl                  = UInt(8.W)
  val eleIdx              = UInt(8.W)
  val vmask               = UInt(16.W)
  val vma                 = Bool()
  val vta                 = Bool()

  def apply = {
    this.mask := 0.U((VLEN/8).W)//TODO:
  }
}

class VecStorePipeBundle(implicit p: Parameters) extends ExuInput(isVpu = true) {
  val vaddr               = UInt(VAddrBits.W)
  val exp                 = Bool()
  val mask                = UInt((VLEN/8).W)
  val uop_unit_stride_fof = Bool()
  val alignedType         = UInt(2.W)
  val fqIdx               = UInt(log2Ceil(VsFlowSize).W)
}

class VsFlowBundle(implicit p: Parameters) extends VecFlowBundle {
  val data = UInt(VLEN.W)
}

// class VsFlowQueueIOBundle (implicit p: Parameters) extends XSBundle {
//   val uopIn        = Vec(VecStorePipelineWidth,Flipped(Decoupled(new Uop2Flow())))
//   val redirect     = Flipped(ValidIO(new Redirect))
//   val storePipeOut = Vec(VecStorePipelineWidth,Decoupled(new VecStorePipeBundle()))
//   val isFirstIssue = Vec(VecStorePipelineWidth,Output(Bool()))
//   val vsfqFeedback = Vec(VecStorePipelineWidth,Flipped(ValidIO(new VSFQFeedback)))
//   val issuePtrExt  = Input(new SqPtr)
//   val flowPtrExt   = Input(UInt(9.W))
//   val issueNum     = Output(UInt(3.W))
// }

class VsFlowQueueIOBundle(implicit p: Parameters) extends VLSUBundle {
  val redirect = Flipped(ValidIO(new Redirect))
  // receive 2 flows from uop queue each cycle at most
  val flowIn = Vec(VecStorePipelineWidth, Flipped(DecoupledIO(new VsFlowBundle())))
  // writeback 2 flows to uop queue each cycle at most
  val flowWriteback = Vec(VecStorePipelineWidth, DecoupledIO(new VecStoreExuOutput()))

  // each issue port corresponds to an stu
  val pipeIssue = Vec(VecStorePipelineWidth, DecoupledIO(new VecStorePipeBundle()))
  // store feedback, in which `hit` indicates whether tlb misses
  val pipeFeedback = Vec(VecStorePipelineWidth, Flipped(ValidIO(new VSFQFeedback())))

  // await commit signals from rob
  val rob = Flipped(new RobLsqIO)
  // write committed stores to sbuffer
  val sbuffer = Vec(EnsbufferWidth, DecoupledIO(new DCacheWordReqWithVaddr))
}
class VsFlowQueue(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper
{
  val io = IO(new VsFlowQueueIOBundle())

  /**
    * TODO @xzf
    */
  io <> DontCare

//   val valid        = RegInit(VecInit(Seq.fill(VecStorePipelineWidth)(VecInit(Seq.fill(VsFlowSize)(false.B)))))
//   val canDeq       = RegInit(VecInit(Seq.fill(VecStorePipelineWidth)(VecInit(Seq.fill(VsFlowSize)(false.B)))))
//   val isFirstIssue = RegInit(VecInit(Seq.fill(VecStorePipelineWidth)(VecInit(Seq.fill(VsFlowSize)(false.B)))))
//   val issued       = RegInit(VecInit(Seq.fill(VecStorePipelineWidth)(VecInit(Seq.fill(VsFlowSize)(false.B)))))
//   val counter      = RegInit(VecInit(Seq.fill(VecStorePipelineWidth)(VecInit(Seq.fill(VsFlowSize)(0.U(5.W))))))
//   val flowEntry    = Reg(Vec(VecStorePipelineWidth,Vec(VsFlowSize,new VecFlowEntry)))

//   val alignedType     = Wire(Vec(VecStorePipelineWidth, UInt(2.W)))
//   val realFlowNum     = Wire(Vec(VecStorePipelineWidth, UInt(5.W)))
//   val uopIdx          = Wire(Vec(VecStorePipelineWidth, UInt(6.W)))
//   val vma             = Wire(Vec(VecStorePipelineWidth, Bool()))
//   val vta             = Wire(Vec(VecStorePipelineWidth, Bool()))
//   val vl              = Wire(Vec(VecStorePipelineWidth, UInt(8.W)))
//   val vmask           = Wire(Vec(VecStorePipelineWidth, UInt(VLEN.W)))
//   val eew             = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
//   val sew             = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
//   val emul            = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
//   val lmul            = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
//   val mul             = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
//   val emulNum         = Wire(Vec(VecStorePipelineWidth, UInt(4.W)))
//   val lmulNum         = Wire(Vec(VecStorePipelineWidth, UInt(4.W)))
//   val instType        = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
//   val uop_segment_num = Wire(Vec(VecStorePipelineWidth, UInt(4.W)))
//   val stride          = Wire(Vec(VecStorePipelineWidth, UInt(XLEN.W)))
//   val index           = Wire(Vec(VecStorePipelineWidth, UInt(VLEN.W)))
//   val baseaddr        = Wire(Vec(VecStorePipelineWidth, UInt(VAddrBits.W)))
//   val segNfIdx        = Wire(Vec(VecStorePipelineWidth, UInt(6.W)))
//   val segMulIdx       = Wire(Vec(VecStorePipelineWidth, UInt(6.W)))
//   val canissue        = WireInit(VecInit(Seq.fill(VecStorePipelineWidth)(VecInit(Seq.fill(VsFlowSize)(false.B)))))
//   val stqAccept       = WireInit(VecInit(Seq.fill(VecStorePipelineWidth)(VecInit(Seq.fill(VsFlowSize)(false.B)))))
//   //val deqMask         = Wire(Vec(VecStorePipelineWidth,UInt(VsFlowSize.W)))
//   val deqIdx          = Wire(Vec(VecStorePipelineWidth,UInt(log2Ceil(VsFlowSize).W)))
//   val uopInValid      = WireInit(VecInit(Seq.fill(VecStorePipelineWidth)(false.B)))
//   val needFlush       = WireInit(VecInit(Seq.fill(VecStorePipelineWidth)(VecInit(Seq.fill(VsFlowSize)(false.B)))))
//   val free            = WireInit(VecInit(Seq.fill(VecStorePipelineWidth)(0.U(VsFlowSize.W))))

//   val vsFlowFreeList = Seq.fill(VecStorePipelineWidth)(Module(new FlowFreeList(size=VsFlowSize, freeWidth=4, maxIdxNum=16, moduleName = "VSFlowQueueFreeList")))

//   for (i <- 0 until VecStorePipelineWidth) {
//     io.uopIn(i).ready := vsFlowFreeList(i).io.allocReq.ready
//     uopInValid(i) := !io.uopIn(i).bits.uop.robIdx.needFlush(io.redirect) && io.uopIn(i).fire
//   }

//   /**
//     * enqueue updata */
//   for (i <- 0 until VecStorePipelineWidth) {
//     uopIdx(i)          := io.uopIn(i).bits.uop.ctrl.uopIdx
//     vma(i)             := Mux(io.uopIn(i).bits.uop_unit_whole_reg, false.B, io.uopIn(i).bits.uop.ctrl.vconfig.vtype.vma)
//     vta(i)             := Mux(io.uopIn(i).bits.uop_unit_whole_reg, false.B, io.uopIn(i).bits.uop.ctrl.vconfig.vtype.vta)
//     vl(i)              := Mux(io.uopIn(i).bits.uop_unit_whole_reg, OneRegNum(eew(i)) * io.uopIn(i).bits.uop_segment_num, io.uopIn(i).bits.uop.ctrl.vconfig.vl)
//     vmask(i)           := io.uopIn(i).bits.src(3)
//     eew(i)             := io.uopIn(i).bits.eew
//     sew(i)             := io.uopIn(i).bits.uop.ctrl.vconfig.vtype.vsew
//     emul(i)            := io.uopIn(i).bits.emul
//     lmul(i)            := io.uopIn(i).bits.uop.ctrl.vconfig.vtype.vlmul
//     mul(i)             := Mux(instType(i)(1,0) === "b00".U || instType(i)(1,0) === "b10".U,emul(i), Mux(emulNum(i) > lmulNum(i),emul(i),lmul(i)))
//     emulNum(i)         := MulNum(emul(i))
//     lmulNum(i)         := MulNum(lmul(i))
//     instType(i)        := io.uopIn(i).bits.instType
//     uop_segment_num(i) := io.uopIn(i).bits.uop_segment_num + 1.U
//     realFlowNum(i)     := GenRealFlowNum(instType=instType(i), emul=emul(i), lmul = lmul(i), eew=eew(i), sew=sew(i))
//     stride(i)          := io.uopIn(i).bits.src(1)
//     index(i)           := io.uopIn(i).bits.src(1)
//     baseaddr(i)        := io.uopIn(i).bits.src(0)
//     alignedType(i)     := DontCare
//     segMulIdx(i)       := GenSegMulIdx(mul = mul(i), uopIdx = uopIdx(i))
//     //segNfIdx(i)        := Mux((instType(i) === "b101".U || instType(i) === "b111".U) && ,,GenSegNfIdx(mul = mul(i),uopIdx = uopIdx(i)))
//     segNfIdx(i)        := Mux((instType(i) === "b101".U || instType(i) === "b111".U) && emulNum(i) > lmulNum(i),
//                               GenSegNfIdxMul(emul=emul(i), lmul=lmul(i), uopIdx=uopIdx(i)),
//                               GenSegNfIdx(mul = mul(i),uopIdx = uopIdx(i)))
//   }

//   //enqueue
//   for (i <- 0 until VecStorePipelineWidth) {
//     vsFlowFreeList(i).io.allocReq.valid := DontCare
//     vsFlowFreeList(i).io.allocReq.bits  := DontCare
//     when (uopInValid(i)) {
//       vsFlowFreeList(i).io.allocReq.valid := true.B
//       vsFlowFreeList(i).io.allocReq.bits :=  realFlowNum(i)
//       for (j <- 0 until 16) {
//         when (j.U < realFlowNum(i)) {
//           val enqValue = Wire(UInt(5.W))
//           val vdFlowIdx  = Wire(UInt(4.W))
//           val vs2FlowIdx = Wire(UInt(4.W))
//           val eleIdx = Wire(UInt(7.W))
//           enqValue := vsFlowFreeList(i).io.idxValue(j)
//           flowEntry(i)(enqValue) := DontCare
//           valid(i)(enqValue)        := true.B
//           canDeq(i)(enqValue)       := false.B
//           isFirstIssue(i)(enqValue) := true.B
//           issued(i)(enqValue)       := false.B
//           counter(i)(enqValue)      := 0.U
//           flowEntry(i)(enqValue).uop_unit_stride_fof := io.uopIn(i).bits.uop_unit_stride_fof
//           flowEntry(i)(enqValue).src(2) := io.uopIn(i).bits.src(2)
//           flowEntry(i)(enqValue).uop    := io.uopIn(i).bits.uop

//           when (instType(i)(1,0) === "b00".U || instType(i)(1,0) === "b10".U) { // (segment) unit-stride,stride
//             vdFlowIdx   := j.U
//             vs2FlowIdx  := j.U
//             alignedType(i) := eew(i)(1,0)
//           }.elsewhen (instType(i) === "b001".U || instType(i) === "b011".U) {//indexed
//             vdFlowIdx   := Mux(emulNum(i) > lmulNum(i), RegFLowCnt(emulNum=emulNum(i), lmulNum=lmulNum(i), eew=eew(i), uopIdx=uopIdx(i), flowIdx=j.U), j.U)
//             vs2FlowIdx  := Mux(emulNum(i) > lmulNum(i), j.U, AddrFLowCnt(emulNum=emulNum(i), lmulNum=lmulNum(i), sew=sew(i), uopIdx=uopIdx(i), flowIdx=j.U))
//             alignedType(i) := sew(i)(1,0)
//           }.otherwise { //segment  index
//             vdFlowIdx   := Mux(emulNum(i) > lmulNum(i), RegFLowCnt(emulNum = emulNum(i), lmulNum = lmulNum(i), eew = eew(i), uopIdx = segMulIdx(i), flowIdx = j.U), j.U)
//             vs2FlowIdx  := Mux(emulNum(i) > lmulNum(i), j.U, AddrFLowCnt(emulNum = emulNum(i), lmulNum = lmulNum(i), sew = sew(i), uopIdx = segMulIdx(i), flowIdx = j.U))
//             alignedType(i) := sew(i)(1,0)
//           }

//           eleIdx := GenEleIdx(instType = instType(i), emul = emul(i), lmul = lmul(i), eew = eew(i), sew = sew(i), uopIdx = uopIdx(i), flowIdx = j.U)
//           val (vsmask,exp) = VSMaskCtrl(vstart = io.uopIn(i).bits.vstart, vl = vl(i), eleIdx = eleIdx, vmask = vmask(i),
//             mask = io.uopIn(i).bits.mask, vma = vma(i), vta = vta(i))
//           //flowEntry(i)(enqValue).mask := io.uopIn(i).bits.mask << VSRegOffset(instType = instType(i), flowIdx = vdFlowIdx, eew = eew(i), sew = sew(i))
//           flowEntry(i)(enqValue).reg_offset := VSRegOffset(instType = instType(i), flowIdx = vdFlowIdx, eew = eew(i), sew = sew(i))
//           flowEntry(i)(enqValue).mask := vsmask << VSRegOffset(instType = instType(i), flowIdx = vdFlowIdx, eew = eew(i), sew = sew(i))
//           flowEntry(i)(enqValue).alignedType := alignedType(i)
//           flowEntry(i)(enqValue).exp := exp
//           flowEntry(i)(enqValue).uop_idx := uopIdx(i)
//           flowEntry(i)(enqValue).src(0) := GenVSAddr(instType = instType(i), baseaddr = baseaddr(i), eleIdx = eleIdx, emul = emul(i),
//                                                       lmul = lmul(i), uopIdx = uopIdx(i), flow_inner_idx = vs2FlowIdx,
//                                                       stride = stride(i), index = index(i), eew = eew(i), sew = sew(i),
//                                                       nf = uop_segment_num(i), segNfIdx = segNfIdx(i), segMulIdx = segMulIdx(i))
//           flowEntry(i)(enqValue).vstart := io.uopIn(i).bits.vstart
//           flowEntry(i)(enqValue).vl := vl(i)
//           flowEntry(i)(enqValue).eleIdx := eleIdx
//           flowEntry(i)(enqValue).vmask := vmask(i)
//           flowEntry(i)(enqValue).vma := vma(i)
//           flowEntry(i)(enqValue).vta := vta(i)
//         }
//       }
//     }
//   }

// /**
//   * issue control*/
//   for (i <- 0 until VecStorePipelineWidth) {
//     if (i == 0){
//       canissue(i) := VecInit((0 until VsFlowSize).map(j => valid(i)(j) && !canDeq(i)(j) && !issued(i)(j) && counter(i)(j) === 0.U && stqAccept(i)(j) && (io.flowPtrExt > 0.U)))
//       deqIdx(i) := PriorityEncoder(canissue(i))
//     }
//     if (i == 1) {
//       canissue(i) := VecInit((0 until VsFlowSize).map(j => valid(i)(j) && !canDeq(i)(j) && !issued(i)(j) && counter(i)(j) === 0.U && stqAccept(i)(j) && ((io.flowPtrExt - 1.U) > 0.U)))
//       deqIdx(i) := PriorityEncoder(canissue(i))
//     }
//   }

//   for (i <- 0 until VecStorePipelineWidth) {
//     io.storePipeOut(i).bits        := DontCare
//     io.storePipeOut(i).valid       := canissue(i)(deqIdx(i))
//     io.isFirstIssue(i)             := isFirstIssue(i)(deqIdx(i))
//     io.storePipeOut(i).bits.fqIdx  := deqIdx(i)
//     io.storePipeOut(i).bits.mask   := GenVSMask(reg_offset = flowEntry(i)(deqIdx(i)).reg_offset,
//                                               offset = flowEntry(i)(deqIdx(i)).src(0)(3,0),
//                                               mask = flowEntry(i)(deqIdx(i)).mask)
//     io.storePipeOut(i).bits.src(2) := GenVSData(reg_offset = flowEntry(i)(deqIdx(i)).reg_offset,
//                                                 offset = flowEntry(i)(deqIdx(i)).src(0)(3,0),
//                                                 data = flowEntry(i)(deqIdx(i)).src(2),
//                                                 vstart = flowEntry(i)(deqIdx(i)).vstart,
//                                                 vl =  flowEntry(i)(deqIdx(i)).vl,
//                                                 eleIdx = flowEntry(i)(deqIdx(i)).eleIdx,
//                                                 vmask = flowEntry(i)(deqIdx(i)).vmask,
//                                                 vma = flowEntry(i)(deqIdx(i)).vma,
//                                                 vta = flowEntry(i)(deqIdx(i)).vta
//     )
//     io.storePipeOut(i).bits.src(0)              := flowEntry(i)(deqIdx(i)).src(0)
//     io.storePipeOut(i).bits.uop_unit_stride_fof := flowEntry(i)(deqIdx(i)).uop_unit_stride_fof
//     io.storePipeOut(i).bits.alignedType         := flowEntry(i)(deqIdx(i)).alignedType
//     io.storePipeOut(i).bits.exp                 := flowEntry(i)(deqIdx(i)).exp
//     io.storePipeOut(i).bits.uop                 := flowEntry(i)(deqIdx(i)).uop
//   }

//   when(io.storePipeOut(0).valid && io.storePipeOut(1).valid){
//     io.issueNum := 2.U
//   }. elsewhen(io.storePipeOut(0).valid && !io.storePipeOut(1).valid){
//     io.issueNum := 1.U
//   }.elsewhen(!io.storePipeOut(0).valid && io.storePipeOut(1).valid) {
//     io.issueNum := 1.U
//   }.elsewhen(!io.storePipeOut(0).valid && !io.storePipeOut(1).valid) {
//     io.issueNum := 0.U
//   }.otherwise{
//     io.issueNum := 0.U
//   }

//   for (i <- 0 until VecStorePipelineWidth) {
//     when (io.storePipeOut(i).fire) {
//       issued(i)(deqIdx(i)) := true.B
//       isFirstIssue(i)(deqIdx(i)) := false.B
//     }
//   }

//   /**
//     * FeedBack control */
//   for (i <- 0 until VecStorePipelineWidth) {
//     when (io.vsfqFeedback(i).valid) {
//       when (io.vsfqFeedback(i).bits.hit) {
//         canDeq(i)(io.vsfqFeedback(i).bits.fqIdx) := true.B
//         flowEntry(i)(io.vsfqFeedback(i).bits.fqIdx).mask := 0.U
//         free(i) := UIntToOH(io.vsfqFeedback(i).bits.fqIdx)
//       }.otherwise {
//         issued(i)(io.vsfqFeedback(i).bits.fqIdx) := false.B
//         counter(i)(io.vsfqFeedback(i).bits.fqIdx) := 31.U
//       }
//     }
//   }

//   /* when uop_idx <  issuePtrExt, the uop has been allocated in Stq */
//   for (i <- 0 until VecStorePipelineWidth) {
//     for (j <- 0 until 16)
//     //when(isBefore(flowEntry(i)(j).uop_idx, io.issuePtrExt)) {
//     when(flowEntry(i)(j).uop_idx < io.issuePtrExt.value) {
//       stqAccept(i)(j) := true.B
//     }.otherwise {
//       stqAccept(i)(j) := false.B
//     }
//   }

//   for (i <- 0 until VecStorePipelineWidth) {
//     for (entry <- 0 until VsFlowSize) {
//       when (valid(i)(entry) && !canDeq(i)(entry) && !isFirstIssue(i)(entry)) {
//         counter(i)(entry) := counter(i)(entry) - 1.U
//       }
//     }
//   }

//   /**
//     * Redirection occurred, flush FlowQueue */
//   for (i <- 0 until VecStorePipelineWidth) {
//     for (entry <- 0 until VsFlowSize) {
//       needFlush(i)(entry) := flowEntry(i)(entry).uop.robIdx.needFlush(io.redirect) && valid(i)(entry)
//       when(needFlush(i)(entry)) {
//         valid(i)(entry) := false.B
//         canDeq(i)(entry) := false.B
//       }
//     }
//   }

//   val lastRedirect = RegNext(io.redirect)
//   for (i <- 0 until VecStorePipelineWidth) {
//     when (lastRedirect.valid) {
//       vsFlowFreeList(i).io.free := RegNext(needFlush(i).asUInt)
//     }.otherwise {
//       vsFlowFreeList(i).io.free := free(i)
//     }
//   }

}
