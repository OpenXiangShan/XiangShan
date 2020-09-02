package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend._

import xiangshan.backend.fu.FunctionUnit._

class FenceExeUnit extends FunctionUnit(fenceCfg) {
  val io = IO(new MulDivIO(len))

  val s_idle :: s_flush_sbuffer_req :: s_flush_sbuffer_resp :: s_flush_others_req :: s_finish :: Nil = Enum(5)
  val state = RegInit(s_idle)

  val src1 = Reg(UInt())
  val src2 = Reg(UInt())
  val ctrl = Reg(new MulDivCtrl())

  // assign default value to output signals
  io.in.ready  := false.B
  io.out.valid := false.B
  io.out.bits  := DontCare

  when (state === s_idle) {
    io.in.ready := true.B
    when (io.in.fire()) {
      src1 := io.in.bits.src1
      src2 := io.in.bits.src2
      ctrl := io.in.bits.ctrl
      state := s_flush_sbuffer_req
    }
  }

  // flush sbuffer
  val sbufferFlush = Wire(new SbufferFlushBundle)
  BoringUtils.addSource(sbufferFlush, "FenceUnitFlushSbufferBundle")
  sbufferFlush.req_valid := false.B

  when (state === s_flush_sbuffer_req) {
    sbufferFlush.req_valid := true.B
    when (sbufferFlush.req_valid && sbufferFlush.req_ready) {
      state := s_flush_sbuffer_resp
    }
  }

  when (state === s_flush_sbuffer_resp) {
    when (sbufferFlush.resp_valid) {
      state := s_flush_others_req
    }
  }

  val func = ctrl.fuOpType

  // flush icache/tlb when necessary
  val sfence = Wire(new SfenceBundle)
  BoringUtils.addSource(sfence, "SfenceBundle")
  sfence.valid := false.B
  sfence.bits  := DontCare

  val fencei = Wire(Bool())
  BoringUtils.addSource(fencei, "FenceI")
  fencei := false.B

  when (state === s_flush_others_req) {
    when (func === MDUOpType.sfence) {
      sfence.valid := true.B
      sfence.bits.rs1  := ctrl.lsrc1 === 0.U
      sfence.bits.rs2  := ctrl.lsrc2 === 0.U
      sfence.bits.addr := src1
    } .elsewhen (func === MDUOpType.fencei) {
      fencei := true.B
    } .elsewhen (func === MDUOpType.fence) {
      // fence does nothing
      // it just blocks everything behind
    } .otherwise {
      // you should never reach here
      asser(true.B)
    }

    state := s_finish
  }

  when (state === s_finish) {
    io.out.valid := true.B
    // fence has no output
    io.out.bits.data := 0.U
    io.out.bits.uop := ctrl.uop
    when (io.out.fire()) {
      state := s_idle
    }
  }
}
