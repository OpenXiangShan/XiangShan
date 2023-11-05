package xiangshan.backend.fu.vector

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.fu.vector.Bundles.{VSew, Vl}
import xiangshan.backend.fu.vector.utils.{MaskExtractor, UIntToContLow0s, UIntToContLow1s}
import _root_.utils.XSDebug
import yunsuan.vector.SewOH


class ByteMaskTailGenIO(vlen: Int)(implicit p: Parameters) extends Bundle {
  private val numBytes = vlen / 8
  private val elemIdxWidth = log2Up(numBytes + 1) // if numBytes is 16, begin and end should contains 0~16
  println(s"elemIdxWidth: $elemIdxWidth")

  val in = Input(new Bundle {
    val begin = UInt(elemIdxWidth.W)
    val end = UInt(elemIdxWidth.W)
    val vma = Bool()
    val vta = Bool()
    val vsew = VSew()
    val maskUsed = UInt(numBytes.W)
  })
  val out = Output(new Bundle {
    val keepEn     = UInt(numBytes.W)
    val agnosticEn = UInt(numBytes.W)
  })
  val debugOnly = Output(new Bundle {
    val startBytes = UInt()
    val vlBytes = UInt()
    val prestartEn = UInt()
    val activeEn = UInt()
    val tailEn = UInt()
    val maskEn = UInt()
    val maskAgnosticEn = UInt()
    val tailAgnosticEn = UInt()
    val agnosticEn = UInt()
  })
}

class ByteMaskTailGen(vlen: Int)(implicit p: Parameters) extends Module {
  require(isPow2(vlen))

  private val numBytes = vlen / 8
  private val byteWidth = log2Up(numBytes) // vlen=128, numBytes=16, byteWidth=log2(16)=4

  println(s"numBytes: ${numBytes}, byteWidth: ${byteWidth}")

  val io = IO(new ByteMaskTailGenIO(vlen))

  private val eewOH = SewOH(io.in.vsew).oneHot

  private val startBytes = Mux1H(eewOH, Seq.tabulate(4)(x => io.in.begin(byteWidth - x, 0) << x)).asUInt
  private val vlBytes    = Mux1H(eewOH, Seq.tabulate(4)(x => io.in.end(byteWidth - x, 0) << x)).asUInt

  private val prestartEn = UIntToContLow1s(startBytes, numBytes)
  private val activeEn = UIntToContLow0s(startBytes, numBytes) & UIntToContLow1s(vlBytes, numBytes)
  private val tailEn = UIntToContLow0s(vlBytes, numBytes)

  private val maskEn = MaskExtractor(vlen)(io.in.maskUsed, io.in.vsew)
  private val maskOffEn = (~maskEn).asUInt
  private val maskAgnosticEn = Mux(io.in.vma, maskOffEn, 0.U)

  private val tailAgnosticEn = Mux(io.in.vta, tailEn, 0.U)

  private val keepEn = Mux(io.in.begin >= io.in.end, 0.U(numBytes.W), activeEn & maskEn)
  private val agnosticEn = Mux(io.in.begin >= io.in.end, 0.U(numBytes.W), maskAgnosticEn | tailAgnosticEn)

  io.out.keepEn := keepEn
  io.out.agnosticEn := agnosticEn

  io.debugOnly.startBytes := startBytes
  io.debugOnly.vlBytes := vlBytes
  io.debugOnly.prestartEn := prestartEn
  io.debugOnly.activeEn := activeEn
  io.debugOnly.tailEn := tailEn
  io.debugOnly.maskEn := maskEn
  io.debugOnly.maskAgnosticEn := maskAgnosticEn
  io.debugOnly.tailAgnosticEn := tailAgnosticEn
  io.debugOnly.agnosticEn := agnosticEn
}

