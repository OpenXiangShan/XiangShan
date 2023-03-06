package xiangshan.v2backend.issue

import chisel3._
import chisel3.util._
import fudian.utils.SignExt
import xiangshan.SelImm
import xiangshan.backend.decode.{ImmUnion, Imm_I}
import xiangshan.v2backend.IntData

class ImmExtractorIO(dataBits: Int) extends Bundle {
  val in = Input(new Bundle {
    val imm = UInt(64.W)
    val immType = SelImm()
  })
  val out = Output(new Bundle {
    val imm = UInt(dataBits.W)
  })
}

class ImmExtractor(dataBits: Int, immTypeSet: Set[BigInt]) extends Module {
  val io = IO(new ImmExtractorIO(dataBits))

  val extractMap = Map(
    SelImm.IMM_I.litValue  -> SignExt(ImmUnion.I.toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_S.litValue  -> SignExt(ImmUnion.S.toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_SB.litValue -> SignExt(ImmUnion.B.toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_U.litValue  -> SignExt(ImmUnion.U.toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_UJ.litValue -> SignExt(ImmUnion.J.toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_Z.litValue  -> SignExt(ImmUnion.Z.toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_B6.litValue -> SignExt(ImmUnion.B6.toImm32(io.in.imm),IntData().dataWidth),
  )

  val usedMap: Map[BigInt, UInt] = extractMap.filterKeys(x => immTypeSet.contains(x))
  println(usedMap)

  io.out.imm := MuxLookup(io.in.immType, 0.U, usedMap.map { case (k, v) => (k.U, v) }.toSeq )
}

object ImmExtractor {
  def apply(imm: UInt, immType: UInt, dataBits: Int, immTypeSet: Set[BigInt]): UInt = {
    val mod = Module(new ImmExtractor(dataBits, immTypeSet))
    mod.io.in.imm := imm
    mod.io.in.immType := immType
    mod.io.out.imm
  }
}

//class AluImmExtractor() extends ImmExtractor(64) {
//  val imm32 = MuxLookup(io.in.immType, 0.U, Seq(
//    SelImm.IMM_U -> ImmUnion.U.toImm32(io.in.imm),
//    SelImm.IMM_I -> ImmUnion.I.toImm32(io.in.imm),
//  ))
//  io.out.imm := SignExt(imm32, 64)
//}
//
//class MulImmExtractor() extends ImmExtractor(64) {
//  val imm32 = ImmUnion.I.toImm32(io.in.imm)
//  io.out.imm := SignExt(imm32, 64)
//}
