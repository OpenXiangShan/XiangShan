package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utility.ZeroExt
import xiangshan.{CSROpType, HasXSParameter, VSETOpType}
import xiangshan.backend.decode.{Imm_VSETIVLI, Imm_VSETVLI}
import xiangshan.backend.decode.isa.bitfield.InstVType
import xiangshan.backend.decode.opcode.Opcode.VSetOpcodes
import xiangshan.backend.fu.vector.Bundles.VsetVType
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit, VsetModule, VtypeStruct}
import xiangshan.backend.fu.vector.Bundles.VConfig
import xiangshan.backend.vector.Decoder.VSetFuncUnit

class VSetBase(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  val debugIO = IO(new Bundle() {
    val vconfig = Output(VConfig())
  })
  protected val in = io.in.bits
  protected val out = io.out.bits

  protected val vsetModule = Module(new VsetModule(cfg))

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

  out.res.data := vl
  out.ctrl.pdestVl.get := in.ctrl.pdestVl.get

  if (cfg.writeVlRf) io.vtype.get.bits := vsetModule.io.out.vconfig.vtype
  if (cfg.writeVlRf) io.vtype.get.valid := io.out.valid && isVsetvl
  if (cfg.writeVlRf) io.vlIsZero.get := io.out.valid && vl === 0.U
  if (cfg.writeVlRf) io.vlIsVlmax.get := io.out.valid && vl === vlmax

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
  val oldVL = in.data.vl.get
  vsetModule.io.in.avl := oldVL
  vsetModule.io.in.vtype := vtype

  // assume cfg of VSetRvfWvf is VSetRvfWvfCfg
  protected val oldVt = in.ctrl.oldVType.get
  vsetModule.io.in.oldVt.get := oldVt

  val vl = vsetModule.io.out.vconfig.vl
  val vlmax = vsetModule.io.out.vlmax
  val isVsetvl = VSETOpType.isVsetvl(in.ctrl.fuOpType)
  val isCSRReadVl = in.ctrl.fuOpType === VSETOpType.csrrvl

  // csrr vl instruction will use this exu to read vl
  out.res.data := Mux(isCSRReadVl, oldVL, vl)
  out.ctrl.pdestVl.get := in.ctrl.pdestVl.get

  if (cfg.writeVlRf) io.vtype.get.bits := vsetModule.io.out.vconfig.vtype
  if (cfg.writeVlRf) io.vtype.get.valid := isVsetvl && io.out.valid
  if (cfg.writeVlRf) io.vlIsZero.get := io.out.valid && !isCSRReadVl && vl === 0.U
  if (cfg.writeVlRf) io.vlIsVlmax.get := io.out.valid && !isCSRReadVl && vl === vlmax

  debugIO.vconfig := vsetModule.io.out.vconfig
}

class VSetUnit(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) with HasXSParameter {
  private val in = io.in.bits
  private val out = io.out.bits

  private val op = in.ctrl.fuOpType
  private val imm = in.data.imm

  private val vsetFu = Module(new VSetFuncUnit(vlen = VLEN, elen = ELEN, xlen = XLEN))

  vsetFu.in.vsetvlVType.valid := VSetOpcodes.isVSetvl(op)
  vsetFu.in.vsetvlVType.bits  := in.data.src(1) // rs2
  vsetFu.in.vsetvliVType.valid := VSetOpcodes.isVSetvli(op)
  vsetFu.in.vsetvliVType.bits  := Imm_VSETVLI().getVTypei(imm)
  vsetFu.in.vsetivliVType.valid := VSetOpcodes.isVSetivli(op)
  vsetFu.in.vsetivliVType.bits  := Imm_VSETIVLI().getVTypei(imm)
  vsetFu.in.readVl.valid := VSetOpcodes.isReadVl(op)
  vsetFu.in.readVl.bits := in.data.vl.get
  vsetFu.in.rdIsZero := VSetOpcodes.rdIsZero(op)
  vsetFu.in.rs1IsZero := VSetOpcodes.rs1IsZero(op)
  vsetFu.in.oldVType.valid := true.B
  vsetFu.in.oldVType.bits := in.ctrl.oldVType.get
  vsetFu.in.vlFromGp.valid := VSetOpcodes.vlIsReg(op)
  vsetFu.in.vlFromGp.bits := in.data.src(0) // rs1
  vsetFu.in.vlFromVl.valid := VSetOpcodes.vlIsKeep(op)
  vsetFu.in.vlFromVl.bits := in.data.vl.get // vl
  vsetFu.in.vlFromImm.valid := VSetOpcodes.vlIsImm(op)
  vsetFu.in.vlFromImm.bits := Imm_VSETIVLI().getAvl(imm)
  vsetFu.in.vill := VSetOpcodes.isIll(op)

  private val vl = vsetFu.out.vl
  private val vlmax = vsetFu.out.vlmax

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid

  this.connect0LatencyCtrlSingal

  out.res.data := vl

  io.vtype.get.valid := vsetFu.in.vsetvlVType.valid && io.out.valid
  io.vtype.get.bits := vsetFu.out.vtype
  io.vlIsZero.get := io.out.valid && vl === 0.U
  io.vlIsVlmax.get := io.out.valid && vl === vlmax
}
