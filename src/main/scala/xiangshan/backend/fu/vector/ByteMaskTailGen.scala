package xiangshan.backend.fu.vector

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.fu.vector.Bundles.{VSew, Vl}
import xiangshan.backend.fu.vector.utils.{MaskExtractor, UIntToContLow0s, UIntToContLow1s}
import utility.XSDebug
import yunsuan.vector.SewOH
import yunsuan.util.LookupTree


class ByteMaskTailGenIO(vlen: Int)(implicit p: Parameters) extends Bundle {
  private val numBytes = vlen / 8
  private val maxVLMUL = 8
  private val maxVLMAX = 8 * 16 // TODO: parameterize this
  private val elemIdxWidth = log2Up(maxVLMAX + 1)
  println(s"elemIdxWidth: $elemIdxWidth")

  val in = Input(new Bundle {
    val begin = UInt(elemIdxWidth.W)
    val end = UInt(elemIdxWidth.W)
    val vma = Bool()
    val vta = Bool()
    val vsew = VSew()
    val maskUsed = UInt(numBytes.W)
    val vdIdx = UInt(3.W)
  })
  val out = Output(new Bundle {
    val activeEn   = UInt(numBytes.W)
    val agnosticEn = UInt(numBytes.W)
  })
  val debugOnly = Output(new Bundle {
    val startBytes = UInt()
    val vlBytes = UInt()
    val prestartEn = UInt()
    val bodyEn = UInt()
    val tailEn = UInt()
    val maskEn = UInt()
    val maskAgnosticEn = UInt() // body mask when vma is true
    val tailAgnosticEn = UInt() // tail mask when vta is true
    val agnosticEn = UInt()
  })
}

/**
 * Generate byte mask for prestart, body, and tail
 * @param vlen vector length
 * @param p implicit parameter
 */
class ByteMaskTailGen(vlen: Int)(implicit p: Parameters) extends Module {
  require(isPow2(vlen))

  private val numBytes = vlen / 8
  private val byteWidth = log2Up(numBytes) // vlen=128, numBytes=16, byteWidth=log2(16)=4
  private val maxVLMUL = 8
  private val maxVLMAX = 8 * 16 // TODO: parameterize this
  private val elemIdxWidth = log2Up(maxVLMAX + 1) // 8

  println(s"numBytes: ${numBytes}, byteWidth: ${byteWidth}")

  val io = IO(new ByteMaskTailGenIO(vlen))

  // one-hot encoding of eew
  private val eewOH = SewOH(io.in.vsew).oneHot

  /** [[startBytes]]/[[vlBytes]]: shift left the [[io.in.begin]]/[[io.in.end]] field by 0, 1, 2, 3 bits,
   *  and choose one by 1-hot eew (8, 16, 32, 64)
   *  e.g. eew=32 => [[eewOH]]=0b0100 => [[vlBytes]] select x==2,
   *       vl=15 => [[vlBytes]] = vl(8-1-2, 0)<<2 = 0b00111100 = 0x3C = 60 = vl*4
   **/
  private val startBytes = Mux1H(eewOH, Seq.tabulate(4)(x => io.in.begin(elemIdxWidth - 1 - x, 0) << x)).asUInt
  private val vlBytes    = Mux1H(eewOH, Seq.tabulate(4)(x => io.in.end(elemIdxWidth - 1 - x, 0) << x)).asUInt
  private val vdIdx      = io.in.vdIdx

  /** Generate byte mask for prestart, body, and tail */
  private val prestartEn = UIntToContLow1s(startBytes, maxVLMAX)
  private val bodyEn = UIntToContLow0s(startBytes, maxVLMAX) & UIntToContLow1s(vlBytes, maxVLMAX)
  private val tailEn = UIntToContLow0s(vlBytes, maxVLMAX)
  /** Select the byte mask for the current vd index */
  private val prestartEnInVd = LookupTree(vdIdx, (0 until maxVLMUL).map(i => i.U -> prestartEn((i+1)*numBytes - 1, i*numBytes)))
  private val bodyEnInVd = LookupTree(vdIdx, (0 until maxVLMUL).map(i => i.U -> bodyEn((i+1)*numBytes - 1, i*numBytes)))
  private val tailEnInVd = LookupTree(vdIdx, (0 until maxVLMUL).map(i => i.U -> tailEn((i+1)*numBytes - 1, i*numBytes)))

  /** Generate byte mask for mask agnostic and tail agnostic */
  private val maskEn = MaskExtractor(vlen)(io.in.maskUsed, io.in.vsew)
  private val maskOffEn = (~maskEn).asUInt
  private val maskAgnosticEn = Mux(io.in.vma, maskOffEn, 0.U) & bodyEnInVd

  /** tail agnostic: tail mask when vta is true */
  private val tailAgnosticEn = Mux(io.in.vta, tailEnInVd, 0.U)

  /** [[activeEn]] is the mask for the active elements in the vector */
  private val activeEn = Mux(io.in.begin >= io.in.end, 0.U(numBytes.W), bodyEnInVd & maskEn)
  /** [[agnosticEn]] is the mask for the agnostic elements in the vector */
  private val agnosticEn = Mux(io.in.begin >= io.in.end, 0.U(numBytes.W), maskAgnosticEn | tailAgnosticEn)

  // TODO: delete me later
  dontTouch(eewOH)
  dontTouch(startBytes)
  dontTouch(vlBytes)
  dontTouch(vdIdx)
  dontTouch(prestartEn)
  dontTouch(bodyEn)
  dontTouch(tailEn)
  dontTouch(prestartEnInVd)
  dontTouch(bodyEnInVd)
  dontTouch(tailEnInVd)
  dontTouch(maskEn)
  dontTouch(maskOffEn)
  dontTouch(maskAgnosticEn)
  dontTouch(tailAgnosticEn)

  io.out.activeEn := activeEn
  io.out.agnosticEn := agnosticEn

  io.debugOnly.startBytes := startBytes
  io.debugOnly.vlBytes := vlBytes
  io.debugOnly.prestartEn := prestartEnInVd
  io.debugOnly.bodyEn := bodyEn
  io.debugOnly.tailEn := tailEnInVd
  io.debugOnly.maskEn := maskEn
  io.debugOnly.maskAgnosticEn := maskAgnosticEn
  io.debugOnly.tailAgnosticEn := tailAgnosticEn
  io.debugOnly.agnosticEn := agnosticEn
}

