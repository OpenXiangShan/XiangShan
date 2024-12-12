package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import xiangshan.backend.fu._
import xiangshan.backend.fu.vector._
import xiangshan.backend.fu.vector.utils._
import xiangshan.backend.fu.vector.Utils._
import yunsuan._
import yunsuan.vector.VectorClz._

class VCLZ(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VclzType.dummy, "Vclz OpType not supported")

  // params alias
  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  // io alias
  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))

  // modules
  private val VClzs = Seq.fill(numVecModule)(Module(new VectorClz))
  private val mgu = Module(new Mgu(dataWidth))

  // mask
  private val mask = srcMask
  private val vmMask = Fill(VLEN, vm)
  private val vlMask = (~(Fill(VLEN, 1.U) << vl)).asUInt

  private val vs2m = Wire(UInt(VLEN.W))
  vs2m := vs2 & (mask | vmMask) & vlMask

  private val vs2mIsNotZero = vs2m.orR
  private val vs2mReverse = VecInit(vs2m.asBools.reverse).asUInt

  private val opcode = fuOpType.asTypeOf(VClzs.head.io.in.opcode)

  private val isVfirst = opcode.isFirst
  /**
   * In connection of [[vs2Split]]
   */
    vs2Split.io.inVecData := Mux(isVfirst, vs2mReverse, vs2)

  /**
   * [[VClz]]'s in connection
   */
  private val vs2VecUsed: Vec[UInt] = Wire(Vec(numVecModule, UInt(64.W)))
  vs2VecUsed := vs2Split.io.outVec64b

  VClzs.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.in.fire := io.in.valid
      mod.io.in.opcode := opcode
      mod.io.in.vsew := vsew
      mod.io.in.vs2 := vs2VecUsed(i)
  }

  private val outVd = Cat(VClzs.reverse.map(_.io.out.vd))

  // vfirst
  private val vs2mIsNotZeroReg = RegEnable(vs2mIsNotZero, io.in.valid)
  private val isVfirstReg = RegEnable(isVfirst, io.in.valid)

  private val outVdFirst = Wire(UInt(64.W))
  outVdFirst := Mux(vs2mIsNotZeroReg, VClzs.map(_.io.out.vd).reduce(_ +& _), Fill(xLen, 1.U(1.W)))


  // when vstart >= vl, no need to update vd, the old value should be keep
  private val outVstartGeVl = outVstart >= outVl

  /**
   * [[mgu]]'s in connection
   */
  mgu.io.in.vd := outVd
  mgu.io.in.oldVd := outOldVd
  mgu.io.in.mask := outSrcMask
  mgu.io.in.info.valid := validVec.last
  mgu.io.in.info.ta := outVecCtrl.vta
  mgu.io.in.info.ma := outVecCtrl.vma
  mgu.io.in.info.vl := outVl
  mgu.io.in.info.vstart := outVecCtrl.vstart
  mgu.io.in.info.eew := outVecCtrl.vsew
  mgu.io.in.info.vsew := outVecCtrl.vsew
  mgu.io.in.info.vdIdx := outVecCtrl.vuopIdx
  mgu.io.in.info.vlmul := outVecCtrl.vlmul
  mgu.io.in.info.narrow := false.B
  mgu.io.in.info.dstMask := false.B
  mgu.io.in.isIndexedVls := false.B

  io.out.bits.res.data := Mux(isVfirstReg, outVdFirst, Mux(outVstartGeVl, outOldVd, mgu.io.out.vd))

}
