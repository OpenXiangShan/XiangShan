package xiangshan.backend.vector

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{XSBundle, XSModule}
import xiangshan.backend.Bundles.UopIdx
import xiangshan.backend.fu.vector.Bundles.{VEew, Vl}
import yunsuan.vector.Common._
import yunsuan.vector.v2.MergeUnit

class VLdMergeUnit(implicit p: Parameters) extends XSModule {
  val in = IO(Input(new VLdMergeUnit.In))
  val out = IO(Output(new VLdMergeUnit.Out))

  private val vlenb = VLEN / 8
  private val mgu = Module(new MergeUnit(VLEN))
  private val eewCases = 1 << VEew.width
  private val byteCountWidth = log2Ceil(vlenb * (1 << UopIdx().getWidth) + 1)
  private val byteCountShiftWidth = byteCountWidth + eewCases - 1
  private val uopByteOffWidth = log2Ceil(vlenb)
  private val vlByteWidth = log2Ceil(vlenb + 1)
  private val vstartExt = in.vStart.pad(byteCountShiftWidth)
  private val vstartBytesFull = Mux1H(UIntToOH(in.vldMergeInfo.eew), Seq.tabulate(eewCases) { x =>
    (vstartExt << x)(byteCountShiftWidth - 1, 0)
  })
  private val vstartBlockIdx = vstartBytesFull(byteCountShiftWidth - 1, uopByteOffWidth)
  private val vstartByteOff = vstartBytesFull(uopByteOffWidth - 1, 0)
  private val uopIdxExt = in.vldMergeInfo.uopIdx.pad(vstartBlockIdx.getWidth)
  private val vstartBytesInUop = Mux(
    vstartBlockIdx > uopIdxExt,
    vlenb.U(vlByteWidth.W),
    Mux(vstartBlockIdx === uopIdxExt, vstartByteOff.pad(vlByteWidth), 0.U(vlByteWidth.W))
  )
  private val valid = in.vldMergeInfo.valid
  private val begin = Mux(valid && in.vldMergeInfo.useVstart, vstartBytesInUop, 0.U(vlByteWidth.W))
  private val mergeEnd = Mux(valid, in.vldMergeInfo.vlBytes, 0.U)

  assert(!valid || begin <= vlenb.U, "VLdMergeUnit begin should be less than or equal to VLEN bytes")
  assert(!valid || in.vldMergeInfo.vlBytes <= vlenb.U, "VLdMergeUnit vlBytes should be less than or equal to VLEN bytes")

  mgu.in.valid := valid
  mgu.in.ctrl.vma := valid && in.vldMergeInfo.vma
  mgu.in.ctrl.vta := valid && in.vldMergeInfo.vta
  mgu.in.data.mask := Mux(valid, in.vldMergeInfo.mask, 0.U)
  mgu.in.data.begin := begin
  mgu.in.data.end := mergeEnd
  mgu.in.data.oldVd := in.oldVd.toByteVec
  mgu.in.data.vd := in.vd.toByteVec

  out.vd := mgu.out.res.asUInt

}

object VLdMergeUnit {
  class In(implicit p: Parameters) extends XSBundle {
    val vStart = Vl()
    val vldMergeInfo = ValidIO(new VldMergeInfo())
    val oldVd = UInt(VLEN.W)
    val vd = UInt(VLEN.W)
  }

  class Out(implicit p: Parameters) extends XSBundle {
    val vd = UInt(VLEN.W)
  }

  class VldMergeInfo(implicit p: Parameters) extends XSBundle {
    val vlByteWidth = log2Ceil(VLENB) + 1

    val uopIdx = UopIdx()
    val mask = UInt(VLENB.W)
    val vlBytes = UInt(vlByteWidth.W)
    val eew = VEew()
    val vma = Bool()
    val vta = Bool()
    val useVstart = Bool()
  }
}
