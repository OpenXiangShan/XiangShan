package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu, Utils, VecPipedFuncUnit, VecSrcTypeModule}
import xiangshan.ExceptionNO
import yunsuan.VialuFixType
import yunsuan.encoding.{VdType, Vs1IntType, Vs2IntType}
import yunsuan.vector.mac.VIMac64b
import xiangshan.backend.decode.opcode.Opcode.VIMacOpcodes
import xiangshan.backend.vector.fu.VecFixLatFunc
import xiangshan.backend.vector.fu.VecFuConfig

class VIMacU(cfg: VecFuConfig)(implicit p: Parameters) extends VecFixLatFunc(cfg) {
  import VIMacOpcodes._

  // params alias
  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  // io alias
  private implicit val opcode: UInt = fuOpType
  private val widen = isWiden
  private val exchangeVs2Vd = overWriteMultiplicand

  // modules
  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val oldVdSplit  = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vimacs = Seq.fill(numVecModule)(Module(new VIMac64b))

  /**
    * In connection of [[vs2Split]], [[vs1Split]] and [[oldVdSplit]]
    */
  vs2Split.io.inVecData := ex0vs2
  vs1Split.io.inVecData := ex0vs1
  oldVdSplit.io.inVecData := ex0oldVd

  /**
    * [[vimacs]]'s in connection
    */
  // Vec(vs2(31,0), vs2(63,32), vs2(95,64), vs2(127,96)) ==>
  // Vec(
  //   Cat(vs2(95,64),  vs2(31,0)),
  //   Cat(vs2(127,96), vs2(63,32)),
  // )
  private val vs2GroupedVec: Vec[UInt] = VecInit(vs2Split.io.outVec32b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)
  private val vs1GroupedVec: Vec[UInt] = VecInit(vs1Split.io.outVec32b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)

  private val vs2VecUsed: Vec[UInt] = Mux(widen, vs2GroupedVec, vs2Split.io.outVec64b)
  private val vs1VecUsed: Vec[UInt] = Mux(widen, vs1GroupedVec, vs1Split.io.outVec64b)
  private val oldVdVecUsed: Vec[UInt] = WireInit(oldVdSplit.io.outVec64b)

  /*
  vimacs.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.fire := io.in(0).valid
      mod.io.info.vm := ex0vm
      mod.io.info.ma := ex0vma
      mod.io.info.ta := ex0vta
      mod.io.info.vlmul := vlmul
      mod.io.info.vl := ex0vl
      mod.io.info.vstart := vstart
      mod.io.info.uopIdx := ex0uopIdx
      mod.io.info.vxrm := vxrm
      mod.io.vs1Sign := vs1Sign
      mod.io.vs2Sign := vs2Sign
      mod.io.vdSign := vdSign
      mod.io.sew := getSew
      mod.io.vs1 := vs1VecUsed(i)
      mod.io.vs2 := Mux(exchangeVs2Vd, oldVdVecUsed(i), vs2VecUsed(i))
      mod.io.oldVd := Mux(exchangeVs2Vd, vs2VecUsed(i), oldVdVecUsed(i))
      mod.io.highHalf := ishighHalf
      mod.io.isMacc := isVmaccType
      mod.io.isSub := isSub
      mod.io.widen := widen
      mod.io.isFixP := isVsmul
  }
  */

  /*
  io.out.bits.res.data := mgu.io.out.vd
  io.out.bits.res.vxsat.get := (outVxsat & mgu.io.out.active).orR
  io.out.bits.ctrl.exceptionVec(ExceptionNO.illegalInstr) := mgu.io.out.illegal
  */
}
