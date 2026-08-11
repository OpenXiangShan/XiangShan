package xiangshan.mem.vector

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.fu.vector.Bundles.{VEew, Vl}
import xiangshan.backend.Bundles.UopIdx

class VldMaskGen(implicit p: Parameters) extends XSModule {
  private val maxBytes = VldMaskGen.uopBytes * (1 << UopIdx().getWidth)
  private val byteCountWidth = log2Ceil(maxBytes + 1)
  private val maxBytesLog2 = log2Ceil(maxBytes)
  private val eewCases = 1 << VEew.width

  val in = IO(Input(new VldMaskGen.In()))
  val out = IO(Output(new VldMaskGen.Out()))
  private val byteCountShiftWidth = byteCountWidth + eewCases - 1
  private val vstartExt = in.vstart.pad(byteCountShiftWidth)
  private val vlExt = in.vl.pad(byteCountShiftWidth)

  private val eewOH = UIntToOH(in.eew)
  private val uopIdxOH = UIntToOH(in.uopIdx)

  private val vlBytesFullMask = Mux1H(eewOH, Seq.tabulate(eewCases)(x => countToMask(vlExt << x)))
  private val vstartBytesFullMask = Mux1H(eewOH, Seq.tabulate(eewCases)(x => countToMask(vstartExt << x)))
  private val activeBytesFullMask = vlBytesFullMask & ~vstartBytesFullMask
  private val activeBytesUopMask = Mux1H(uopIdxOH, Seq.tabulate(uopIdxOH.getWidth)(x =>
    activeBytesFullMask((x + 1) * VldMaskGen.uopBytes - 1, x * VldMaskGen.uopBytes)
  ))
  private val vstartBytesUopMask = Mux1H(uopIdxOH, Seq.tabulate(uopIdxOH.getWidth)(x =>
    vstartBytesFullMask((x + 1) * VldMaskGen.uopBytes - 1, x * VldMaskGen.uopBytes)
  ))
  private val v0BytesUopMask = Mux(in.vm, Fill(VldMaskGen.uopBytes, 1.U), in.v0Mask)
  // Whole-register loads/stores (vlnr/vlm, memOpType MASK) ignore vl, so each uop covers its whole
  // register, but they are not exempt from vstart: the elements before vstart stay untouched.
  out.mask := Mux(in.isWhole, Fill(VldMaskGen.uopBytes, 1.U), activeBytesUopMask) &
    ~vstartBytesUopMask & v0BytesUopMask

  private val vlBytesFull = Mux1H(eewOH, Seq.tabulate(eewCases)(x => (vlExt << x)(byteCountShiftWidth - 1, 0)))
  private val uopByteOffWidth = log2Ceil(VldMaskGen.uopBytes)
  private val vlBlockIdx = vlBytesFull(byteCountShiftWidth - 1, uopByteOffWidth)
  private val vlByteOff  = vlBytesFull(uopByteOffWidth - 1, 0)
  private val uopIdxExt  = in.uopIdx.pad(vlBlockIdx.getWidth)

  private val vlBytesInUop = Mux(
    vlBlockIdx > uopIdxExt,
    VldMaskGen.uopBytes.U(VldMaskGen.vlBytesWidth.W),
    Mux(vlBlockIdx === uopIdxExt, vlByteOff.pad(VldMaskGen.vlBytesWidth), 0.U)
  )

  out.vlBytes := Mux(in.isWhole, VldMaskGen.uopBytes.U(VldMaskGen.vlBytesWidth.W), vlBytesInUop)

  private def prefixOrFromLSB(source: UInt): UInt = {
    require(source.getWidth == maxBytes)
    var result = source
    var shift = 1
    while (shift < maxBytes) {
      result = result | (result << shift)(maxBytes - 1, 0)
      shift = shift << 1
    }
    result
  }

  private def countToMask(count: UInt): UInt = {
    val isFull = count(count.getWidth - 1, maxBytesLog2).orR
    val countOH = UIntToOH(count(maxBytesLog2 - 1, 0), maxBytes)
    Fill(maxBytes, isFull) | ~prefixOrFromLSB(countOH)
  }
  out.useVstart := in.vstart =/= 0.U
}

object VldMaskGen{
  def uopBytes(implicit p: Parameters): Int = p(XSCoreParamsKey).VLEN / 8
  def vlBytesWidth(implicit p: Parameters): Int = log2Ceil(uopBytes + 1)
  class In(implicit p: Parameters) extends XSBundle {
    val vl = Vl()
    val vstart = Vl()
    val vm = Bool()
    val isWhole = Bool()
    val v0Mask = UInt(uopBytes.W)
    val eew = VEew()
    val uopIdx = UopIdx()
  }

  class Out(implicit p: Parameters) extends XSBundle {
    val mask = UInt(uopBytes.W)
    val vlBytes = UInt(vlBytesWidth.W)
    val useVstart = Bool()
  }
}
