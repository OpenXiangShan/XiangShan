package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import fudian.utils.SignExt
import xiangshan.SelImm
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.datapath.DataConfig._

import scala.collection.MapView

class ImmExtractorIO(dataBits: Int) extends Bundle {
  val in = Input(new Bundle {
    val imm = UInt(32.W)
    val immType = SelImm()
  })
  val out = Output(new Bundle {
    val imm = UInt(dataBits.W)
  })
}

/**
 * Extract immediate value from a compressed instruction.
 *
 * @param dataBits data width of the immediate value
 * @param immTypeSet a set of immediate types that will be used
 */
class ImmExtractor(dataBits: Int, immTypeSet: Set[BigInt]) extends Module {
  val io = IO(new ImmExtractorIO(dataBits))

  val extractMap = Map(
    SelImm.IMM_I        .litValue -> SignExt(ImmUnion.I       .toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_S        .litValue -> SignExt(ImmUnion.S       .toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_SB       .litValue -> SignExt(ImmUnion.B       .toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_U        .litValue -> SignExt(ImmUnion.U       .toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_UJ       .litValue -> SignExt(ImmUnion.J       .toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_Z        .litValue -> SignExt(ImmUnion.Z       .toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_B6       .litValue -> SignExt(ImmUnion.B6      .toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_VSETVLI  .litValue -> SignExt(ImmUnion.VSETVLI .toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_VSETIVLI .litValue -> SignExt(ImmUnion.VSETIVLI.toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_OPIVIS   .litValue -> SignExt(ImmUnion.OPIVIS  .toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_OPIVIU   .litValue -> SignExt(ImmUnion.OPIVIU  .toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_LUI32    .litValue -> SignExt(ImmUnion.LUI32   .toImm32(io.in.imm), IntData().dataWidth),
    SelImm.IMM_VRORVI   .litValue -> SignExt(ImmUnion.VRORVI  .toImm32(io.in.imm), IntData().dataWidth),
  )

  // Extract the immediate value from the compressed instruction
  val usedMap: Seq[(BigInt, UInt)] = extractMap.view.filterKeys(x => immTypeSet.contains(x)).toSeq.sortWith(_._1 < _._1)
  println(s"usedMap: ${usedMap}")

  io.out.imm := MuxLookup(io.in.immType, 0.U)(usedMap.map { case (k, v) => (k.U, v) }.toSeq)
}

/**
  * Companion object to create ImmExtractor.
  * When called by others, the immediate extractor returns the extracted immediate value.
  */
object ImmExtractor {
  /**
   * Create an ImmExtractor instance.
   *
   * @param imm compressed immediate value
   * @param immType immediate type
   * @param dataBits data width of the immediate value
   * @param immTypeSet a set of immediate types that will be used
   * @return extracted immediate value
   */
  def apply(imm: UInt, immType: UInt, dataBits: Int, immTypeSet: Set[BigInt]): UInt = {
    val mod = Module(new ImmExtractor(dataBits, immTypeSet))
    mod.io.in.imm := imm
    mod.io.in.immType := immType
    mod.io.out.imm
  }
}
