package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util.{Fill, RegEnable}
import org.chipsalliance.cde.config.Parameters
import utility.DelayN
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.{NewMgu, VecPipedFuncUnit}
import yunsuan.vector.v2.Crypto.{CryptoIteration, VSha256c, VSha256ms}
import yunsuan.encoding.Opcode.Opcodes.VSha256cOpcode


class VSha256cWrapper(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  private val VLENB = VLEN / 8

  private implicit val opcode = fuOpType
  private val valid = io.in.valid

  private val cMod = Module(new CryptoIteration)
  private val mgu = Module(new NewMgu(VLEN))

  cMod.in match {
    case in =>
      in.pipe0.valid := valid && VSha256cOpcode.isLegal
      in.pipe0.vs1 := vs1
      in.pipe0.vs2 := vs2
      in.pipe0.vs3 := oldVd
      in.pipe0.uop.vsha512c := false.B
      in.pipe0.uop.vsha256c := true.B // only support vsha256c[lh]
      in.pipe0.uop.vsha2cl := VSha256cOpcode.isCL
      in.pipe0.uop.vsm3c := false.B
      in.pipe0.uimm := 0.U // not support vsm3c yet
      in.pipe1 := DontCare
      in.pipe1.valid := false.B
      in.clear := false.B
  }

  mgu.io.in.mask := Fill(VLEN, true.B)
  mgu.io.in.info.ta := vta
  mgu.io.in.info.ma := vma
  mgu.io.in.info.vstart := 0.U
  mgu.io.in.info.vl := vl
  mgu.io.in.info.eew := vsew
  mgu.io.in.info.vsew := vsew
  mgu.io.in.info.vdIdx := vuopIdx
  mgu.io.in.isIndexedVls := false.B

  private val activeEn = mgu.io.out.activeEn

  private val activeEnS2 = DelayN(activeEn, 2)

  private val newVdBytes = Wire(Vec(VLENB, UInt(8.W)))
  private val oldVdBytes = Wire(Vec(VLENB, UInt(8.W)))

  newVdBytes := cMod.out.pipe0.vd.asTypeOf(newVdBytes)
  oldVdBytes := outOldVd.asTypeOf(oldVdBytes)

  private val resVecByte = Wire(Vec(VLENB, UInt(8.W)))

  for (i <- 0 until VLENB) {
    resVecByte(i) := Mux(
      activeEnS2(i),
      newVdBytes(i),
      oldVdBytes(i),
    )
  }

  io.out.bits.res.data := resVecByte.asUInt
}
