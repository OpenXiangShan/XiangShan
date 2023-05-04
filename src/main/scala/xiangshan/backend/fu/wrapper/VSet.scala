package xiangshan.backend.fu.wrapper

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import utility.ZeroExt
import xiangshan.VSETOpType
import xiangshan.backend.decode.Imm_VSETIVLI
import xiangshan.backend.decode.isa.bitfield.InstVType
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.fu.{FuConfig, FuncUnit, VsetModule}

class VSetBase(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  protected val vsetModule = Module(new VsetModule)

  protected val flushed = io.in.bits.robIdx.needFlush(io.flush)

  protected val avlImm = Imm_VSETIVLI().getAvl(in.src(1))
  protected val avl = Mux(VSETOpType.isVsetivli(in.fuOpType), avlImm, in.src(0))

  protected val instVType: InstVType = Imm_VSETIVLI().getVType(in.src(1))
  protected val vtypeImm: VType = VType.fromInstVType(instVType)
  protected val vtype: VType = Mux(VSETOpType.isVsetvl(in.fuOpType), in.src(1)(7, 0).asTypeOf(new VType), vtypeImm)

  vsetModule.io.in.func := in.fuOpType

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}


/**
  * Wrapper of VsetModule
  * This fu is uop of vset which reads two int regs and writes one int regs.<br>
  * uop: <br/>
  * [[VSETOpType.uvsetrd_ii]], <br/>
  * [[VSETOpType.uvsetrd_xi]], <br/>
  * [[VSETOpType.uvsetrd_xx]], <br/>
  * [[VSETOpType.uvsetrd_vlmax_i]], <br/>
  * [[VSETOpType.uvsetrd_vlmax_x]], <br/>
  * @param cfg [[FuConfig]]
  * @param p [[Parameters]]
  */
class VSetIVL(cfg: FuConfig)(implicit p: Parameters) extends VSetBase(cfg) {
  vsetModule.io.in.avl := avl
  vsetModule.io.in.vtype := vtype
  vsetModule.io.in.oldVl := 0.U

  out.data := vsetModule.io.out.vconfig.vl

  connectNonPipedCtrlSingal
}

/**
  * Wrapper of VsetModule
  * This fu is uop of vset which reads two int regs and writes one vf regs.<br>
  * uop: <br/>
  * [[VSETOpType.uvsetvcfg_ii]], <br/>
  * [[VSETOpType.uvsetvcfg_xi]], <br/>
  * [[VSETOpType.uvsetvcfg_xx]], <br/>
  * [[VSETOpType.uvsetvcfg_vlmax_i]], <br/>
  * [[VSETOpType.uvsetvcfg_vlmax_x]], <br/>
  * @param cfg [[FuConfig]]
  * @param p [[Parameters]]
  */
class VSetIVConfig(cfg: FuConfig)(implicit p: Parameters) extends VSetBase(cfg) {
  vsetModule.io.in.avl := avl
  vsetModule.io.in.vtype := vtype
  vsetModule.io.in.oldVl := 0.U

  out.data := ZeroExt(vsetModule.io.out.vconfig.asUInt, XLEN)

  connectNonPipedCtrlSingal
}

/**
  * Wrapper of VsetModule
  * This fu is uop of vset which reads two int regs and writes one vf regs.<br>
  * uop: <br/>
  * [[VSETOpType.uvsetvcfg_vv]], <br/>
  * [[VSETOpType.uvsetvcfg_keep_v]], <br/>
  * @param cfg [[FuConfig]]
  * @param p [[Parameters]]
  */
class VSetFVConfig(cfg: FuConfig)(implicit p: Parameters) extends VSetBase(cfg) {
  vsetModule.io.in.avl := 0.U
  vsetModule.io.in.vtype := vtype
  vsetModule.io.in.oldVl := in.src(0)

  out.data := ZeroExt(vsetModule.io.out.vconfig.asUInt, XLEN)

  connectNonPipedCtrlSingal
}
