// Dummy vector FuncUnit stub for build bringup
// Until full vector backend (VecFixLatFunc/VecFuConfig/vector.Exu) is ported
package xiangshan.backend.fu.vector

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.{FuncUnit, FuConfig, HasPipelineReg}

class DummyVecPipedFuncUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) with HasPipelineReg {
  override def latency: Int = cfg.latency.latencyVal.get
  
  // Drive res outputs to 0 (HasPipelineReg already handles ctrl/valid/ready)
  io.out.bits.res.data := 0.U
  io.out.bits.perfDebugInfo.foreach(_ := 0.U.asTypeOf(io.out.bits.perfDebugInfo.get))
  io.out.bits.debug_seqNum.foreach(_ := 0.U.asTypeOf(io.out.bits.debug_seqNum.get))
  if (cfg.writeVxsat) {
    io.out.bits.res.vxsat.get := false.B
  }
  if (cfg.writeFflags) {
    io.out.bits.res.fflags.get := 0.U
  }
}

class DummyVecNonPipedFuncUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  // Stub: always ready, never valid
  io.in.ready := true.B
  io.out.valid := false.B
  io.out.bits.ctrl := 0.U.asTypeOf(io.out.bits.ctrl)
  io.out.bits.res.data := 0.U
  io.out.bits.perfDebugInfo.foreach(_ := 0.U.asTypeOf(io.out.bits.perfDebugInfo.get))
  io.out.bits.debug_seqNum.foreach(_ := 0.U.asTypeOf(io.out.bits.debug_seqNum.get))
  if (cfg.writeVxsat) {
    io.out.bits.res.vxsat.get := false.B
  }
  if (cfg.writeFflags) {
    io.out.bits.res.fflags.get := 0.U
  }
}
