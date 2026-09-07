package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.decode.opcode.Opcode.VIMacOpcodes
import xiangshan.backend.vector.fu.Func._
import xiangshan.backend.vector.fu.{VecFixLatFunc, VecFuConfig}
import yunsuan.vector.Common._
import yunsuan.vector.mac.VIMac64b

class VIMacU(cfg: VecFuConfig)(implicit p: Parameters) extends VecFixLatFunc(cfg) {
  import VIMacOpcodes._

  // params alias
  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  // io alias
  private implicit val opcode: UInt = fuOpType
  private val ex0NextOpcode = ex0Next.bits.ctrl.opcode
  private val exchangeVs2Vd = overWriteMultiplicand

  private val sel8     = VIMacOpcodes.isSourceE8(opcode)
  private val sel16    = VIMacOpcodes.isSourceE16(opcode)
  private val sel32    = VIMacOpcodes.isSourceE32(opcode)
  private val sel64    = VIMacOpcodes.isSourceE64(opcode)
  private val vs1Sign  = VIMacOpcodes.vs1Sign(opcode)
  private val vs2Sign  = VIMacOpcodes.vs2Sign(opcode)
  private val vdSign   = VIMacOpcodes.vdSign(opcode)
  private val isSub    = VIMacOpcodes.isSub(opcode)
  private val highHalf = VIMacOpcodes.ishighHalf(opcode)
  private val isMacc   = VIMacOpcodes.isVmaccType(opcode)
  private val isFixP   = VIMacOpcodes.isFixP(opcode)
  private val ex0NextWiden = VIMacOpcodes.isWiden(ex0NextOpcode)
  private val widen = makePipeReg(ex0NextWiden, pipeRegValids)

  // modules
  private val vimacs = Seq.fill(numVecModule)(Module(new VIMac64b))

  /**
    * [[vimacs]]'s in connection
    */

  private val vs2Vec = WireInit(ex0vs2.splitToVecN(numVecModule))
  private val vs1Vec = WireInit(ex0vs1.splitToVecN(numVecModule))
  private val oldVdVec = WireInit(ex0oldVd.splitToVecN(numVecModule))
  private val vs2NarrowVec = WireInit(ex0vs2.splitToVecN(numVecModule * 2))
  private val vs1NarrowVec = WireInit(ex0vs1.splitToVecN(numVecModule * 2))

  // Vec(vs2(31,0), vs2(63,32), vs2(95,64), vs2(127,96)) ==>
  // Vec(
  //   Cat(vs2(95,64),  vs2(31,0)),
  //   Cat(vs2(127,96), vs2(63,32)),
  // )
  private val vs2GroupedVec: Vec[UInt] = VecInit(vs2NarrowVec.grouped(2).toSeq.transpose.map(x => Cat(x.reverse)))
  private val vs1GroupedVec: Vec[UInt] = VecInit(vs1NarrowVec.grouped(2).toSeq.transpose.map(x => Cat(x.reverse)))

  private val vs2VecUsed: Vec[UInt] = Mux(widen.ex0, vs2GroupedVec, vs2Vec)
  private val vs1VecUsed: Vec[UInt] = Mux(widen.ex0, vs1GroupedVec, vs1Vec)
  private val oldVdVecUsed: Vec[UInt] = WireInit(oldVdVec)

  vimacs.zipWithIndex.foreach {
    case (mod, i) =>
      mod.in.info.uopIdx            := ex0uopIdx
      mod.in.info.vxrm.bits         := vxrm
      mod.in.ex0.valid              := ex(0).valid
      mod.in.ex0.bits.ctrl.sewIs8   := sel8
      mod.in.ex0.bits.ctrl.sewIs16  := sel16
      mod.in.ex0.bits.ctrl.sewIs32  := sel32
      mod.in.ex0.bits.ctrl.sewIs64  := sel64
      mod.in.ex0.bits.ctrl.vs1Sign  := vs1Sign
      mod.in.ex0.bits.ctrl.vs2Sign  := vs2Sign
      mod.in.ex0.bits.ctrl.vdSign   := vdSign
      mod.in.ex0.bits.ctrl.isSub    := isSub
      mod.in.ex0.bits.ctrl.highHalf := highHalf
      mod.in.ex0.bits.ctrl.isMacc   := isMacc
      mod.in.ex0.bits.ctrl.widen    := widen.ex0
      mod.in.ex0.bits.ctrl.isFixP   := isFixP
      mod.in.ex0.bits.data.vs1      := vs1VecUsed(i)
      mod.in.ex0.bits.data.vs2      := Mux(!exchangeVs2Vd, vs2VecUsed(i), oldVdVecUsed(i))
      mod.in.ex0.bits.data.oldVd    := Mux(!exchangeVs2Vd, oldVdVecUsed(i), vs2VecUsed(i))

      mod.in.ex1Valid := ex(1).valid
  }

  out.ex(0).bits.data.vec.foreach {
    case vecData =>
      vecData.normal  := 0.U
      vecData.narrow  := 0.U
      vecData.maskE8  := 0.U
      vecData.maskE16 := 0.U
      vecData.maskE32 := 0.U
      vecData.maskE64 := 0.U
      vecData.vxsatE8.foreach(_ := 0.U.asTypeOf(vecData.vxsatE8.get))
      vecData.narrowVxsatE8.foreach(_ := 0.U.asTypeOf(vecData.narrowVxsatE8.get))
  }

  out.ex(1).bits.data.vec.foreach {
    case vecData =>
      vecData.normal  := 0.U
      vecData.narrow  := 0.U
      vecData.maskE8  := 0.U
      vecData.maskE16 := 0.U
      vecData.maskE32 := 0.U
      vecData.maskE64 := 0.U
      vecData.vxsatE8.foreach(_ := 0.U.asTypeOf(vecData.vxsatE8.get))
      vecData.narrowVxsatE8.foreach(_ := 0.U.asTypeOf(vecData.narrowVxsatE8.get))
  }

  out.ex(2).bits.data.vec.foreach {
    case vecData =>
      vecData.normal  := Cat(vimacs.map(_.out.ex2.vd).reverse)
      vecData.narrow  := Cat(vimacs.map(_.out.ex2.narrowVd).reverse)
      vecData.maskE8  := Cat(vimacs.map(_.out.ex2.mask.e8).reverse)
      vecData.maskE16 := Cat(vimacs.map(_.out.ex2.mask.e16).reverse)
      vecData.maskE32 := Cat(vimacs.map(_.out.ex2.mask.e32).reverse)
      vecData.maskE64 := Cat(vimacs.map(_.out.ex2.mask.e64).reverse)
      vecData.vxsatE8.get := vimacs.flatMap(_.out.ex2.vxsat.asBools)
      vecData.narrowVxsatE8.get := vimacs.flatMap(_.out.ex2.narrowVxsat.asBools)
  }
}
