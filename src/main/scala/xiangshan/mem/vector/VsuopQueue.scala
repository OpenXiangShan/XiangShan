package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3.{util, _}
import chisel3.util._
import utils._
import utility._
import xiangshan._

class VsUopPtr(implicit p: Parameters) extends CircularQueuePtr[VsUopPtr](
  p => p(XSCoreParamsKey).VsUopSize
){
}

object VsUopPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): VsUopPtr = {
    val ptr = Wire(new VsUopPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class VsUopQueueIOBundle (implicit p: Parameters) extends XSBundle {
  val storeIn  = Vec(VecStorePipelineWidth,Flipped(Decoupled(new ExuInput(isVpu = true))))
  val Redirect = Flipped(ValidIO(new Redirect))
  val uop2Flow = Vec(VecStorePipelineWidth,Decoupled(new Uop2Flow()))
}
class VsUopQueue(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new VsUopQueueIOBundle())

  println("StoreUopQueue: size:" + VsUopSize)

  val valid = RegInit(VecInit(Seq.fill(VsUopSize)(false.B)))
  val vsUopEntry = Reg(Vec(VsUopSize,new Uop2Flow()))

  val loadInstDec = Wire(Vec(VecStorePipelineWidth,new VecDecode()))
  val eew = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  val sew = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  val lmul = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  val emul = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  val isSegment = Wire(Vec(VecStorePipelineWidth, Bool()))
  val instType = Wire(Vec(VecStorePipelineWidth, UInt(3.W)))
  val storeInValid = WireInit(VecInit(Seq.fill(VecStorePipelineWidth)(false.B)))
  val needFlush = WireInit(VecInit(Seq.fill(VsUopSize)(false.B)))

  val enqPtr = RegInit(0.U.asTypeOf(new VsUopPtr))
  val deqPtr = RegInit(0.U.asTypeOf(new VsUopPtr))

  for (i <- 0 until VecStorePipelineWidth) {
    io.storeIn(i).ready := PopCount(valid) < VsUopSize.U - 1.U
  }

  for (i <- 0 until VecStorePipelineWidth) {
    storeInValid(i) := !io.storeIn(i).bits.uop.robIdx.needFlush(io.Redirect) && io.storeIn(i).fire
  }

  /**
    * Redirection occurred, flush VsuopQueue */
  for (entry <- 0 until VsUopSize) {
    needFlush(entry) := vsUopEntry(entry).uop.robIdx.needFlush(io.Redirect) && valid(entry)
    when(needFlush(entry)) {
      valid(entry) := false.B
    }
  }

/**
  * VsUopQueue enqPtr update */
  val lastRedirect = RegNext(io.Redirect)
  val uopRedirectCnt = RegNext(PopCount(needFlush))
  when (lastRedirect.valid) {
    enqPtr.value := enqPtr.value - uopRedirectCnt
  }.otherwise {
    when (storeInValid(0) && storeInValid(1)) {
      enqPtr.value := enqPtr.value + 2.U
    }.elsewhen (storeInValid(0) && !storeInValid(1) || !storeInValid(0) && storeInValid(1)) {
      enqPtr.value := enqPtr.value + 1.U
    }
  }

  /**
    * VsUopQueue deqPtr update */
  when (io.uop2Flow(0).fire && io.uop2Flow(1).fire) {
    deqPtr := deqPtr + 2.U
  }.elsewhen (io.uop2Flow(0).fire && !io.uop2Flow(1).fire || !io.uop2Flow(0).fire && io.uop2Flow(1).fire) {
    deqPtr := deqPtr + 1.U
  }

  for (i <- 0 until VecStorePipelineWidth) {
    loadInstDec(i).apply(io.storeIn(i).bits.uop.cf.instr)
    eew(i)       := loadInstDec(i).uop_eew
    sew(i)       := io.storeIn(i).bits.uop.ctrl.vconfig.vtype.vsew
    lmul(i)      := io.storeIn(i).bits.uop.ctrl.vconfig.vtype.vlmul
    emul(i)      := EewLog2(eew(i)) - sew(i) + lmul(i)
    isSegment(i) := loadInstDec(i).uop_segment_num =/= "b000".U && !loadInstDec(i).uop_unit_stride_whole_reg
    instType(i)  := Cat(isSegment(i),loadInstDec(i).uop_type)
    vsUopEntry(enqPtr.value + i.U) := DontCare
  }

  //enqueue
  when (storeInValid(0) && storeInValid(1)) {
    for (i <- 0 until VecStorePipelineWidth) {
      valid(enqPtr.value + i.U) := true.B
      vsUopEntry(enqPtr.value + i.U).src      := io.storeIn(i).bits.src
      vsUopEntry(enqPtr.value + i.U).uop      := io.storeIn(i).bits.uop
      vsUopEntry(enqPtr.value + i.U).mask     := GenVecStoreMask(instType=instType(i), eew=eew(i), sew=sew(i))
      vsUopEntry(enqPtr.value + i.U).eew      := eew(i)
      vsUopEntry(enqPtr.value + i.U).emul     := emul(i)
      vsUopEntry(enqPtr.value + i.U).instType := instType(i)
      vsUopEntry(enqPtr.value + i.U).uop_unit_stride_fof := loadInstDec(i).uop_unit_stride_fof
      vsUopEntry(enqPtr.value + i.U).uop_segment_num := loadInstDec(i).uop_segment_num
    }
  }.elsewhen (storeInValid(0) && !storeInValid(1)) {
    valid(enqPtr.value) := true.B
    vsUopEntry(enqPtr.value).src := io.storeIn(0).bits.src
    vsUopEntry(enqPtr.value).uop := io.storeIn(0).bits.uop
    vsUopEntry(enqPtr.value).mask := GenVecStoreMask(instType=instType(0), eew=eew(0), sew=sew(0))
    vsUopEntry(enqPtr.value).eew := eew(0)
    vsUopEntry(enqPtr.value).emul := emul(0)
    vsUopEntry(enqPtr.value).instType := instType(0)
    vsUopEntry(enqPtr.value).uop_unit_stride_fof := loadInstDec(0).uop_unit_stride_fof
    vsUopEntry(enqPtr.value).uop_segment_num := loadInstDec(0).uop_segment_num
  }.elsewhen (!storeInValid(0) && storeInValid(1)){
    valid(enqPtr.value) := true.B
    vsUopEntry(enqPtr.value).src := io.storeIn(1).bits.src
    vsUopEntry(enqPtr.value).uop := io.storeIn(1).bits.uop
    vsUopEntry(enqPtr.value).mask := GenVecStoreMask(instType=instType(1), eew=eew(1), sew=sew(1))
    vsUopEntry(enqPtr.value).eew := eew(1)
    vsUopEntry(enqPtr.value).emul := emul(1)
    vsUopEntry(enqPtr.value).instType := instType(1)
    vsUopEntry(enqPtr.value).uop_unit_stride_fof := loadInstDec(1).uop_unit_stride_fof
    vsUopEntry(enqPtr.value).uop_segment_num := loadInstDec(1).uop_segment_num
  }

  //dequeue
  for (i <- 0 until VecStorePipelineWidth) {
    io.uop2Flow(i).valid := valid(deqPtr.value + i.U)//FIXME: performace, 1 interface may use incorrect valid
    io.uop2Flow(i).bits := DontCare
  }

  when (io.uop2Flow(0).fire && io.uop2Flow(1).fire) {
    for (i <- 0 until VecStorePipelineWidth) {
      io.uop2Flow(i).bits := vsUopEntry(deqPtr.value + i.U)
      valid(deqPtr.value + i.U) := false.B
    }
  }.elsewhen (io.uop2Flow(0).fire && !io.uop2Flow(1).fire) {
    io.uop2Flow(0).bits := vsUopEntry(deqPtr.value)
    valid(deqPtr.value) := false.B
  }.elsewhen (!io.uop2Flow(0).fire && io.uop2Flow(1).fire) {
    io.uop2Flow(1).bits := vsUopEntry(deqPtr.value)
    valid(deqPtr.value) := false.B
  }

}
