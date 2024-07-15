package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utility.ZeroExt
import xiangshan.{VSETOpType, CSROpType}
import xiangshan.backend.decode.{Imm_VSETIVLI, Imm_VSETVLI}
import xiangshan.backend.decode.isa.bitfield.InstVType
import xiangshan.backend.fu.vector.Bundles.VsetVType
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit, VsetModule, VtypeStruct}
import xiangshan.backend.fu.vector.Bundles.VConfig

class VSetBase(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  val debugIO = IO(new Bundle() {
    val vconfig = Output(VConfig())
  })
  protected val in = io.in.bits
  protected val out = io.out.bits

  protected val vsetModule = Module(new VsetModule)

  protected val flushed = io.in.bits.ctrl.robIdx.needFlush(io.flush)

  protected val avlImm = Imm_VSETIVLI().getAvl(in.data.src(1))
  protected val avl = Mux(VSETOpType.isVsetivli(in.ctrl.fuOpType), avlImm, in.data.src(0))

  protected val instVType: InstVType = Mux(VSETOpType.isVsetivli(in.ctrl.fuOpType), Imm_VSETIVLI().getVType(in.data.src(1)), Imm_VSETVLI().getVType(in.data.src(1)))
  protected val vtypeImm: VsetVType = VsetVType.fromInstVType(instVType)
  protected val vtype: VsetVType = Mux(VSETOpType.isVsetvl(in.ctrl.fuOpType), VsetVType.fromVtypeStruct(in.data.src(1).asTypeOf(new VtypeStruct())), vtypeImm)

  vsetModule.io.in.func := in.ctrl.fuOpType
  connect0LatencyCtrlSingal
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
class VSetRiWi(cfg: FuConfig)(implicit p: Parameters) extends VSetBase(cfg) {
  vsetModule.io.in.avl := avl
  vsetModule.io.in.vtype := vtype

  out.res.data := vsetModule.io.out.vconfig.vl

  debugIO.vconfig := vsetModule.io.out.vconfig
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
class VSetRiWvf(cfg: FuConfig)(implicit p: Parameters) extends VSetBase(cfg) {
  vsetModule.io.in.avl := avl
  vsetModule.io.in.vtype := vtype
  val vl = vsetModule.io.out.vconfig.vl
  val vlmax = vsetModule.io.out.vlmax
  val isVsetvl = VSETOpType.isVsetvl(in.ctrl.fuOpType)

  out.res.data := vsetModule.io.out.vconfig.vl

  if (cfg.writeVlRf) io.vtype.get.bits := vsetModule.io.out.vconfig.vtype
  if (cfg.writeVlRf) io.vtype.get.valid := io.out.valid && isVsetvl
  if (cfg.writeVlRf) io.vlIsZero.get := vl === 0.U
  if (cfg.writeVlRf) io.vlIsVlmax.get := vl === vlmax

  debugIO.vconfig := vsetModule.io.out.vconfig
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
class VSetRvfWvf(cfg: FuConfig)(implicit p: Parameters) extends VSetBase(cfg) {
  vsetModule.io.in.avl := 0.U
  vsetModule.io.in.vtype := vtype

  val oldVL = in.data.src(4).asTypeOf(VConfig()).vl
  val res = WireInit(0.U.asTypeOf(VConfig()))
  val vlmax = vsetModule.io.out.vlmax
  val isVsetvl = VSETOpType.isVsetvl(in.ctrl.fuOpType)
  val isReadVl = in.ctrl.fuOpType === CSROpType.set
  res.vl := Mux(vsetModule.io.out.vconfig.vtype.illegal, 0.U,
              Mux(VSETOpType.isKeepVl(in.ctrl.fuOpType), oldVL, vsetModule.io.out.vconfig.vl))
  res.vtype := vsetModule.io.out.vconfig.vtype

  out.res.data := Mux(isReadVl, oldVL,
                    Mux(vsetModule.io.out.vconfig.vtype.illegal, 0.U,
                      Mux(VSETOpType.isKeepVl(in.ctrl.fuOpType), oldVL, vsetModule.io.out.vconfig.vl)))

  if (cfg.writeVlRf) io.vtype.get.bits := vsetModule.io.out.vconfig.vtype
  if (cfg.writeVlRf) io.vtype.get.valid := isVsetvl && io.out.valid
  if (cfg.writeVlRf) io.vlIsZero.get := !isReadVl && res.vl === 0.U
  if (cfg.writeVlRf) io.vlIsVlmax.get := !isReadVl && res.vl === vlmax

  debugIO.vconfig := res
}
