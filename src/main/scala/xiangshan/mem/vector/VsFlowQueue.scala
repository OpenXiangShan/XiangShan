
package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._

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
}

/**
  * (1) unit-stride instructions access to memory continously, so calculate the address by adding 16 directly (flow_inner_idx << 4.U)
  * (2) stride instructions: flow_inner_idx means the current number of UOP memory accesses,
  *     inner_Idx << Log2Num(GenRealFlowNum(instType,emul,eew,sew)) means the number of all previous UOP memory accesses
  * (3) index instructions: According to flow_ inner_idx obtains immediate value from index, than Calculate address
  * (4) segment instructions: To calculate the address, segment instructions need calculate segEmulIdx and segNfIdx;
  * */
object GenVSAddr {
  def apply (instType: UInt, baseaddr: UInt, emul:UInt, lmul: UInt, inner_Idx:UInt, flow_inner_idx: UInt, stride: UInt,
             index: UInt, eew: UInt, sew: UInt, nf:UInt, segNfIdx: UInt, segEmulIdx: UInt): UInt = {
    (LookupTree(instType,List(
      "b000".U -> (baseaddr + ((flow_inner_idx + (inner_Idx << Log2Num(GenRealFlowNum(instType,emul,lmul,eew,sew))).asUInt) << eew(1,0)).asUInt).asUInt,// unit-stride
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
    def apply(reg_offset: UInt, offset: UInt, data: UInt):UInt = {
      val vData = Wire(UInt(128.W))
      when (offset <= reg_offset) {
        vData := data >> ((reg_offset - offset) << 3.U)
      }.otherwise {
        vData := data << ((offset - reg_offset) << 3.U)
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
  val mask                = UInt((VLEN/8).W)
  val uop_unit_stride_fof = Bool()
  val alignedType         = UInt(2.W)
  val reg_offset          = UInt(4.W)

  def apply = {
    this.mask := 0.U((VLEN/8).W)//TODO:
  }
}

class VecPipeBundle(implicit p: Parameters) extends ExuInput(isVpu = true) {
  val mask                = UInt((VLEN/8).W)
  val uop_unit_stride_fof = Bool()
  val alignedType         = UInt(2.W)
  val fqIdx               = UInt(log2Ceil(VsFlowSize).W)
}

class VsFlowBundle (implicit p: Parameters) extends XSBundle {
  val uopIn        = Vec(VecStorePipelineWidth,Flipped(Decoupled(new Uop2Flow())))
  val Redirect     = Flipped(ValidIO(new Redirect))
  val storePipeOut = Vec(VecStorePipelineWidth,Decoupled(new VecPipeBundle()))
  val isFirstIssue = Vec(VecStorePipelineWidth,Output(Bool()))
  val vsfqFeedback = Vec(VecStorePipelineWidth,Flipped(ValidIO(new VSFQFeedback)))
}

class VsFlowQueue(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper
{
  val io = IO(new VsFlowBundle())

  val valid        = RegInit(VecInit(Seq.fill(VecStorePipelineWidth)(VecInit(Seq.fill(VsFlowSize)(false.B)))))
  val canDeq       = RegInit(VecInit(Seq.fill(VecStorePipelineWidth)(VecInit(Seq.fill(VsFlowSize)(false.B)))))
  val isFirstIssue = RegInit(VecInit(Seq.fill(VecStorePipelineWidth)(VecInit(Seq.fill(VsFlowSize)(false.B)))))
  val issued       = RegInit(VecInit(Seq.fill(VecStorePipelineWidth)(VecInit(Seq.fill(VsFlowSize)(false.B)))))
  val counter      = RegInit(VecInit(Seq.fill(VecStorePipelineWidth)(VecInit(Seq.fill(VsFlowSize)(0.U(5.W))))))
  val flowEntry    = Reg(Vec(VecStorePipelineWidth,Vec(VsFlowSize,new VecFlowEntry)))

  val realFlowNum     = Wire(Vec(VecStorePipelineWidth, UInt(5.W)))
  val eew             = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  val sew             = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  val emul            = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  val lmul            = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  val mul             = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  val instType        = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  val uop_segment_num = Wire(Vec(VecStorePipelineWidth, UInt(4.W)))
  val stride          = Wire(Vec(VecStorePipelineWidth, UInt(XLEN.W)))
  val index           = Wire(Vec(VecStorePipelineWidth, UInt(VLEN.W)))
  val baseaddr        = Wire(Vec(VecStorePipelineWidth, UInt(VAddrBits.W)))
  val segNfIdx        = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  val segMulIdx       = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  val canissue        = WireInit(VecInit(Seq.fill(VecStorePipelineWidth)(VecInit(Seq.fill(VsFlowSize)(false.B)))))
  val deqMask         = Wire(Vec(VecStorePipelineWidth,UInt(VsFlowSize.W)))
  val deqIdx          = Wire(Vec(VecStorePipelineWidth,UInt(log2Ceil(VsFlowSize).W)))
  val uopInValid      = WireInit(VecInit(Seq.fill(VecStorePipelineWidth)(false.B)))
  val needFlush       = WireInit(VecInit(Seq.fill(VecStorePipelineWidth)(VecInit(Seq.fill(VsFlowSize)(false.B)))))
  val flowRedirectCnt = RegInit(VecInit(Seq.fill(VecStorePipelineWidth)(0.U(log2Up(VsFlowSize).W))))

  val enqPtr = RegInit(VecInit(Seq.fill(VecStorePipelineWidth)(0.U.asTypeOf(new VsFlowPtr))))
  val deqPtr = RegInit(VecInit(Seq.fill(VecStorePipelineWidth)(0.U.asTypeOf(new VsFlowPtr))))

  def getFirstOne(mask: Vec[Bool], startMask: UInt): UInt = {
    val length = mask.length
    val highBits = (0 until length).map(i => mask(i) & ~startMask(i))
    val highBitsUint = Cat(highBits.reverse)
    PriorityEncoder(Mux(highBitsUint.orR(), highBitsUint, mask.asUInt))
  }


  for (i <- 0 until VecStorePipelineWidth) {
    io.uopIn(i).ready := distanceBetween(enqPtr(i),deqPtr(i)) <= 16.U
  }

/**
  * enqPtr updata*/
  val lastRedirect = RegNext(io.Redirect)
  for (i <- 0 until VecStorePipelineWidth) {
    flowRedirectCnt(i) := RegNext(PopCount(needFlush(i)))
    when (lastRedirect.valid) {
        enqPtr(i) := enqPtr(i) - flowRedirectCnt(i)
    }.otherwise {
      when (uopInValid(i)) {
        enqPtr(i) := enqPtr(i) + realFlowNum(i)
      }
    }
  }

/**
  * deqPtr updata*/
  for (i <- 0 until VecStorePipelineWidth) {
    when (valid(i)(deqPtr(i).value) && canDeq(i)(deqPtr(i).value)) {
      deqPtr(i) := deqPtr(i) + 1.U
      valid(i)(deqPtr(i).value) := false.B
    }
  }

  /**
    * FeedBack control*/
  for (i <- 0 until VecStorePipelineWidth) {
    when (io.vsfqFeedback(i).valid) {
      when (io.vsfqFeedback(i).bits.hit) {
        //valid(i)(io.vsfqFeedback(i).bits.fqIdx)          := false.B
        canDeq(i)(io.vsfqFeedback(i).bits.fqIdx)         := true.B
        flowEntry(i)(io.vsfqFeedback(i).bits.fqIdx).mask := 0.U
      }.otherwise {
        issued(i)(io.vsfqFeedback(i).bits.fqIdx)  := false.B
        counter(i)(io.vsfqFeedback(i).bits.fqIdx) := 31.U
      }
    }
  }

  for (i <- 0 until VecStorePipelineWidth) {
    for (entry <- 0 until VsFlowSize) {
      when (valid(i)(entry) && !canDeq(i)(entry) && !isFirstIssue(i)(entry)) {
        counter(i)(entry) := counter(i)(entry) - 1.U
      }
    }
  }

  /**
    * Redirection occurred, flush flowQueue*/
  for (i <- 0 until VecStorePipelineWidth) {
    for (entry <- 0 until VsFlowSize) {
      needFlush(i)(entry) := flowEntry(i)(entry).uop.robIdx.needFlush(io.Redirect) && valid(i)(entry)
      when (needFlush(i)(entry)) {
        valid(i)(entry)  := false.B
        //canDeq(i)(entry) := false.B
      }
    }
    uopInValid(i) := !io.uopIn(i).bits.uop.robIdx.needFlush(io.Redirect) && io.uopIn(i).fire
  }

  /**
    * enqueue updata */
  for (i <- 0 until VecStorePipelineWidth) {
    eew(i)             := io.uopIn(i).bits.eew
    sew(i)             := io.uopIn(i).bits.uop.ctrl.vconfig.vtype.vsew
    emul(i)            := io.uopIn(i).bits.emul
    lmul(i)            := io.uopIn(i).bits.uop.ctrl.vconfig.vtype.vlmul
    mul(i)             := Mux(instType(i)(1,0) === "b00".U || instType(i)(1,0) === "b10".U,emul(i),lmul(i))
    instType(i)        := io.uopIn(i).bits.instType
    uop_segment_num(i) := io.uopIn(i).bits.uop_segment_num + 1.U
    realFlowNum(i)     := GenRealFlowNum(instType=instType(i), emul=emul(i), lmul = lmul(i), eew=eew(i), sew=sew(i))
    stride(i)          := io.uopIn(i).bits.src(1)
    index(i)           := io.uopIn(i).bits.src(1)
    baseaddr(i)        := io.uopIn(i).bits.src(0)
    segMulIdx(i)       := GenSegMulIdx(mul = mul(i), inner_Idx = io.uopIn(i).bits.uop.ctrl.uopIdx)
    segNfIdx(i)        := GenSegNfIdx(mul = mul(i),inner_Idx = io.uopIn(i).bits.uop.ctrl.uopIdx)
  }

  //enqueue
  for (i <- 0 until VecStorePipelineWidth) {
    when (uopInValid(i)) {
      for (j <- 0 until 16) {
        when (j.U < realFlowNum(i)) {
          val enqValue = Wire(UInt(5.W))
          enqValue := enqPtr(i).value + j.U
          flowEntry(i)(enqValue) := DontCare
          valid(i)(enqValue)        := true.B
          canDeq(i)(enqValue)       := false.B
          isFirstIssue(i)(enqValue) := true.B
          issued(i)(enqValue)       := false.B
          counter(i)(enqValue)      := 0.U
          flowEntry(i)(enqValue).mask := io.uopIn(i).bits.mask << VSRegOffset(instType=instType(i), flowIdx=j.U, eew=eew(i), sew=sew(i))
          flowEntry(i)(enqValue).uop_unit_stride_fof := io.uopIn(i).bits.uop_unit_stride_fof
          flowEntry(i)(enqValue).alignedType := Mux(instType(i)(1,0) === "b00".U || instType(i)(1,0) === "b10".U,eew(i)(1,0),sew(i)(1,0))
          flowEntry(i)(enqValue).src(0) := GenVSAddr(instType=instType(i), baseaddr=baseaddr(i), emul=emul(i), lmul = lmul(i),
                                                    inner_Idx=io.uopIn(i).bits.uop.ctrl.uopIdx, flow_inner_idx=j.U,
                                                    stride=stride(i), index=index(i), eew=eew(i), sew=sew(i),
                                                    nf=uop_segment_num(i), segNfIdx=segNfIdx(i), segEmulIdx=segMulIdx(i))
          flowEntry(i)(enqValue).src(2) := io.uopIn(i).bits.src(2)
          flowEntry(i)(enqValue).reg_offset := VSRegOffset(instType=instType(i), flowIdx=j.U, eew=eew(i), sew=sew(i))
          flowEntry(i)(enqValue).uop := io.uopIn(i).bits.uop
        }
      }
    }
  }
/**
  * issue control*/
  for (i <- 0 until VecStorePipelineWidth) {
    canissue(i) := VecInit((0 until VsFlowSize).map(j => valid(i)(j) && !canDeq(i)(j) && !issued(i)(j) && counter(i)(j) === 0.U))
    deqMask(i)  := UIntToMask(deqPtr(i).value,VsFlowSize)
    deqIdx(i)   := getFirstOne(canissue(i),deqMask(i))
  }

  for (i <- 0 until VecStorePipelineWidth) {
    io.storePipeOut(i).valid       := canissue(i)(deqIdx(i))
    io.isFirstIssue(i)             := isFirstIssue(i)(deqIdx(i))
    io.storePipeOut(i).bits.fqIdx  := deqIdx(i)
    io.storePipeOut(i).bits        := DontCare
    io.storePipeOut(i).bits.mask   := GenVSMask(reg_offset = flowEntry(i)(deqIdx(i)).reg_offset,
                                              offset = flowEntry(i)(deqIdx(i)).src(0)(3,0),
                                              mask = flowEntry(i)(deqIdx(i)).mask)
    io.storePipeOut(i).bits.src(2) := GenVSData(reg_offset = flowEntry(i)(deqIdx(i)).reg_offset,
                                                offset = flowEntry(i)(deqIdx(i)).src(0)(3,0),
                                                data = flowEntry(i)(deqIdx(i)).src(2))
    io.storePipeOut(i).bits.src(0)              := flowEntry(i)(deqIdx(i)).src(0)
    io.storePipeOut(i).bits.uop_unit_stride_fof := flowEntry(i)(deqIdx(i)).uop_unit_stride_fof
    io.storePipeOut(i).bits.alignedType         := flowEntry(i)(deqIdx(i)).alignedType
    io.storePipeOut(i).bits.uop                 := flowEntry(i)(deqIdx(i)).uop
  }

  for (i <- 0 until VecStorePipelineWidth) {
    when (io.storePipeOut(i).fire) {
      issued(i)(deqIdx(i)) := true.B
      isFirstIssue(i)(deqIdx(i)) := false.B
    }
  }

}
