package xiangshan.backend.fu.vector

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import top.ArgParser
import xiangshan._
import xiangshan.backend.fu.vector.Bundles.{VSew, Vl}
import xiangshan.backend.fu.vector.Utils.VecDataToMaskDataVec

class NewMgu(vlen: Int)(implicit p: Parameters) extends Module {
  private val numBytes = vlen / 8
  private val byteWidth = log2Up(numBytes)

  val io = IO(new NewMguIO(vlen))

  val in = io.in
  val info = in.info
  val mask = in.mask
  val isIndexedVls = in.isIndexedVls
  val vdIdx = info.vdIdx
  val vsew = info.vsew
  val eew = info.eew

  private val maskTailGen = Module(new ByteMaskTailGen(vlen))
  
  private val realEw = Mux(isIndexedVls, vsew, eew)
  private val maskDataVec: Vec[UInt] = VecDataToMaskDataVec(mask, realEw)
  protected lazy val maskUsed = maskDataVec(vdIdx)

  maskTailGen.io.in.begin := info.vstart
  maskTailGen.io.in.end := info.vl
  maskTailGen.io.in.vma := info.ma
  maskTailGen.io.in.vta := info.ta
  maskTailGen.io.in.vsew := realEw
  maskTailGen.io.in.maskUsed := maskUsed
  maskTailGen.io.in.vdIdx := vdIdx

  private val activeEn = maskTailGen.io.out.activeEn
  private val agnosticEn = maskTailGen.io.out.agnosticEn

  io.out.activeEn := activeEn
  io.out.agnosticEn := agnosticEn
}

class NewMguIO(vlen: Int)(implicit p: Parameters) extends Bundle {
  val in = Input(new NewMguInputBundle(vlen))
  val out = Output(new NewMguOutputBundle(vlen))
}

class NewMguInputBundle(vlen: Int)(implicit p: Parameters) extends Bundle {
  val mask = UInt(vlen.W)
  val info = new NewVecInfo
  val isIndexedVls = Bool()
}

class NewMguOutputBundle(vlen: Int)(implicit p: Parameters) extends Bundle {
  val activeEn = UInt((vlen / 8).W)
  val agnosticEn = UInt((vlen / 8).W)
}

class NewVecInfo(implicit p: Parameters) extends Bundle {
  val ta = Bool()
  val ma = Bool()
  val vstart = Vl()
  val vl = Vl()
  val eew = VSew()
  val vsew = VSew()
  val vdIdx = UInt(3.W)
}
