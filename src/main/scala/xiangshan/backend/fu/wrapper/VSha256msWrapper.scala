package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util.Fill
import org.chipsalliance.cde.config.Parameters
import utility.DelayN
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.{NewMgu, VecPipedFuncUnit}
import yunsuan.encoding.Opcode.Opcodes.VSha256msOpcode
import yunsuan.vector.v2.Crypto.VSha256ms


class VSha256msWrapper(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  private val VLENB = VLEN / 8

  private implicit val opcode = fuOpType
  private val valid = io.in.valid

  private val msMod = Module(new VSha256ms)
  private val mgu = Module(new NewMgu(VLEN))

  msMod.in match {
    case in =>
      in.valid := valid && VSha256msOpcode.isMS
      in.vs1 := vs1
      in.vs2 := vs2
      in.vs3 := oldVd
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

  private val activeEnS4 = DelayN(activeEn, 4)

  private val newVdBytes = Wire(Vec(VLENB, UInt(8.W)))
  private val oldVdBytes = Wire(Vec(VLENB, UInt(8.W)))

  newVdBytes := msMod.out.vd.asTypeOf(newVdBytes)
  oldVdBytes := outOldVd.asTypeOf(oldVdBytes)

  private val resVecByte = Wire(Vec(VLENB, UInt(8.W)))

  for (i <- 0 until VLENB) {
    resVecByte(i) := Mux(
      activeEnS4(i),
      newVdBytes(i),
      oldVdBytes(i),
    )
  }

  io.out.bits.res.data := resVecByte.asUInt
}
