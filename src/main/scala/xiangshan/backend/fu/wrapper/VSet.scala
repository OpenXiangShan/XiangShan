package xiangshan.backend.fu.wrapper

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import xiangshan.backend.fu.{FuConfig, FuncUnit, VsetModule}

class VSetIVL(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  private val vsetModule = Module(new VsetModule)

  private val flushed = io.in.bits.robIdx.needFlush(io.flush)

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  private val in = io.in.bits
  private val out = io.out.bits
  vsetModule.io.src0 := in.src(0)
  vsetModule.io.src1 := in.src(1)
  vsetModule.io.func := in.fuOpType
  vsetModule.io.oldVConfig := 0.U
  out.data := vsetModule.io.vl
  connectNonPipedCtrlSingal
}

class VSetIVConfig(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  private val vsetModule = Module(new VsetModule)

  private val flushed = io.in.bits.robIdx.needFlush(io.flush)

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  private val in = io.in.bits
  private val out = io.out.bits
  vsetModule.io.src0 := in.src(0)
  vsetModule.io.src1 := in.src(1)
  vsetModule.io.func := in.fuOpType
  vsetModule.io.oldVConfig := 0.U
  out.data := vsetModule.io.vconfig
  connectNonPipedCtrlSingal
}

class VSetFVConfig(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  private val vsetModule = Module(new VsetModule)

  private val flushed = io.in.bits.robIdx.needFlush(io.flush)

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  private val in = io.in.bits
  private val out = io.out.bits
  vsetModule.io.src0 := 0.U
  vsetModule.io.src1 := in.src(1)
  vsetModule.io.func := in.fuOpType
  vsetModule.io.oldVConfig := in.src(0)
  out.data := vsetModule.io.vconfig
  connectNonPipedCtrlSingal
}