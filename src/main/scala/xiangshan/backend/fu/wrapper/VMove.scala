package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.{Mgu, VecPipedFuncUnit}
import xiangshan.backend.fu.vector.Utils.VecDataToMaskDataVec
import yunsuan.vector.VectorMove.VectorMove
import yunsuan.VmoveType

class VMove(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VmoveType.dummy, "VMove OpType not supported")

  // param alias
  private val dataWidth = cfg.destDataBits
  private val vlenb = VLEN / 8

  private val valid = io.in.valid

  private val vMove = Module(new VectorMove)
  private val mgu = Module(new Mgu(dataWidth))

  // mask
  private val maskDataVec: Vec[UInt] = VecDataToMaskDataVec(srcMask, vsew)
  private val maskVec = maskDataVec(vuopIdx)

  vMove match {
    case mod =>
      mod.io.in.valid := valid
      mod.io.in.bits.opcode := fuOpType(3, 0)
      mod.io.in.bits.info.vm := vm
      mod.io.in.bits.info.vsew := vsew
      mod.io.in.bits.vs2 := vs2
      mod.io.in.bits.vs1 := vs1
      mod.io.in.bits.mask := maskVec
  }

  private val needNoMask = VmoveType.needNoMask(fuOpType)
  private val maskToMgu = Mux(needNoMask, allMaskTrue, srcMask)

  private val vlIsOne = VmoveType.vlIsOne(fuOpType)
  private val vlIsZeroUpdate = VmoveType.vlIsZeroUpdate(fuOpType)
  private val isVmvnr = VmoveType.isVmvnr(fuOpType)
  // when vstart >= vl, no need update vd, the old vd should be keep
  // vmv<nr>r.v, evl = EMUL * VLEN / VSEW, when vstart >= evl, no need update vd
  private val emulVmvnr = VmoveType.getEmulVmvnr(fuOpType)
  private val emul = Mux(isVmvnr, emulVmvnr, vlmul)
  private val eleCnt = vlenb.U >> vsew
  private val evl = Mux1H(Seq.tabulate(4)(i => (emul(1, 0) === i.U) -> (eleCnt << i.U)))
  private val vlTmp = Mux(isVmvnr, evl.asUInt, vl)
  private val newVl = Mux(vlIsOne, 1.U, vlTmp)
  private val vstartGeVl = (vstart >= vlTmp || !vlTmp.orR) & !vlIsZeroUpdate

  private val vd = vMove.io.out.bits.vd

  mgu.io.in.vd := vd
  mgu.io.in.oldVd := oldVd
  mgu.io.in.mask := maskToMgu
  mgu.io.in.info.ta := vta
  mgu.io.in.info.ma := vma
  mgu.io.in.info.vl := newVl
  mgu.io.in.info.vlmul := emul
  mgu.io.in.info.valid := valid
  mgu.io.in.info.vstart := vstart
  mgu.io.in.info.eew := vsew
  mgu.io.in.info.vsew := vsew
  mgu.io.in.info.vdIdx := vuopIdx
  mgu.io.in.info.narrow := false.B
  mgu.io.in.info.dstMask := false.B
  mgu.io.in.isIndexedVls := false.B

  io.out.bits.res.data := Mux(vstartGeVl, oldVd, mgu.io.out.vd)
}

