package xiangshan.backend.fu.vector

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.fu.vector.Bundles.VSew
import yunsuan.vector._

class DstMgu(vlen: Int)(implicit p: Parameters) extends Module {
  val io = IO(new DstMguIO(vlen))

  val numBytes = vlen / 8

  val valid = io.in.valid
  val oldVd = io.in.oldVd
  val mask = io.in.mask
  val ma = io.in.ma
  val eew = io.in.eew
  val vdIdx = io.in.vdIdx

  val vdS1 = io.in.toS1.vd
  val oldVdS1 = io.in.toS1.oldVdS1
  val eewS1 = io.in.toS1.eewS1
  val vdIdxS1 = io.in.toS1.vdIdxS1

  private val eewOH = SewOH(eew)
  private val maskOldVdBits = splitVdMask(oldVd, eewOH)(vdIdx)
  private val maskBits = splitVdMask(mask, eewOH)(vdIdx)
  private val maskMaOrOldVdBits = Wire(Vec(numBytes, UInt(1.W)))
  maskMaOrOldVdBits.zipWithIndex.foreach { case (mask, i) =>
    mask := Mux(ma, 1.U, maskOldVdBits(i)) 
  }

  private val maskMaOrOldVdBitsS1 = RegEnable(maskMaOrOldVdBits, valid)
  private val maskBitsS1 = RegEnable(maskBits, valid)

  private val maskVecByte = Wire(Vec(numBytes, UInt(1.W)))
  maskVecByte.zipWithIndex.foreach { case (mask, i) =>
    mask := Mux(maskBitsS1(i), vdS1(i), maskMaOrOldVdBitsS1(i))
  }
  private val maskVd = maskVecByte.asUInt

  private val maxVdIdx = 8
  private val meaningfulBitsSeq = Seq(16, 8, 4, 2)
  private val allPossibleResBit = Wire(Vec(4, Vec(maxVdIdx, UInt(vlen.W))))

  
  for (sew <- 0 to 3) {
    if (sew == 0) {
      allPossibleResBit(sew)(maxVdIdx - 1) := Cat(maskVd(meaningfulBitsSeq(sew) - 1, 0),
        oldVdS1(meaningfulBitsSeq(sew) * (maxVdIdx - 1) - 1, 0))
    } else {
      allPossibleResBit(sew)(maxVdIdx - 1) := Cat(oldVdS1(vlen - 1, meaningfulBitsSeq(sew) * maxVdIdx),
        maskVd(meaningfulBitsSeq(sew) - 1, 0), oldVdS1(meaningfulBitsSeq(sew) * (maxVdIdx - 1) - 1, 0))
    }
    for (i <- 1 until maxVdIdx - 1) {
      allPossibleResBit(sew)(i) := Cat(oldVdS1(vlen - 1, meaningfulBitsSeq(sew) * (i + 1)),
        maskVd(meaningfulBitsSeq(sew) - 1, 0), oldVdS1(meaningfulBitsSeq(sew) * i - 1, 0))
    }
    allPossibleResBit(sew)(0) := Cat(oldVdS1(vlen - 1, meaningfulBitsSeq(sew)), maskVd(meaningfulBitsSeq(sew) - 1, 0))
  }

  private val resVecBit = allPossibleResBit(eewS1)(vdIdxS1)

  io.out.vd := resVecBit.asUInt

  def splitVdMask(maskIn: UInt, sew: SewOH): Vec[UInt] = {
    val maskWidth = maskIn.getWidth
    val result = Wire(Vec(maskWidth / numBytes, UInt(numBytes.W)))
    for ((resultData, i) <- result.zipWithIndex) {
      resultData := Mux1H(Seq(
        sew.is8 -> maskIn(i * numBytes + (numBytes - 1), i * numBytes),
        sew.is16 -> Cat(0.U((numBytes - (numBytes / 2)).W), maskIn(i * (numBytes / 2) + (numBytes / 2) - 1, i * (numBytes / 2))),
        sew.is32 -> Cat(0.U((numBytes - (numBytes / 4)).W), maskIn(i * (numBytes / 4) + (numBytes / 4) - 1, i * (numBytes / 4))),
        sew.is64 -> Cat(0.U((numBytes - (numBytes / 8)).W), maskIn(i * (numBytes / 8) + (numBytes / 8) - 1, i * (numBytes / 8))),
      ))
    }
    result
  }
}

class DstMguIO(vlen: Int)(implicit p: Parameters) extends Bundle {
  val in = Input(new DstMguInputBundle(vlen))
  val out = Output(new DstMguOutputBundle(vlen))
}

class DstMguInputBundle(vlen: Int)(implicit p: Parameters) extends Bundle {
  val valid = Bool()
  val oldVd = UInt(vlen.W)
  val mask = UInt(vlen.W)
  val ma = Bool()
  val eew = VSew()
  val vdIdx = UInt(3.W)
  val toS1 = new DstMguS1InputBundle(vlen)
}

class DstMguOutputBundle(vlen: Int)(implicit p: Parameters) extends Bundle {
  val vd = UInt(vlen.W) 
}

class DstMguS1InputBundle(vlen: Int)(implicit p: Parameters) extends Bundle {
  val vd = UInt((vlen / 8).W)
  val oldVdS1 = UInt(vlen.W)
  val eewS1 = VSew()
  val vdIdxS1 = UInt(3.W)
}
