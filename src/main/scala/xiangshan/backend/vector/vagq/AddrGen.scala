package xiangshan.backend.vector.vagq

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters


class AddrGen(implicit p: Parameters) extends VAGQModule {
  val in = IO(Input(new AddrGenInput))
  val out = IO(Output(new AddrGenOutput))

  private val elemIdx = (in.byteOffset >> in.deew)(vagqFlowByteWidth - 1, 0)

  private val elemOrdFromInst = (in.uopIdx << elemNum(in.deew)) | elemIdx  // element ordinal from inst
  private val strideElemOrd = elemOrdFromInst
  private val strideOffsetWide = in.op2Data(XLEN - 1, 0).asSInt * strideElemOrd.asSInt
  private val strideOffset = strideOffsetWide.asUInt(XLEN - 1, 0)

  private val idx8 = VecInit((0 until 16).map(i => in.op2Data(8 * i + 7, 8 * i)))(elemIdx)
  private val idx16 = VecInit((0 until 8).map(i => in.op2Data(16 * i + 15, 16 * i)))(elemIdx(2, 0))
  private val idx32 = VecInit((0 until 4).map(i => in.op2Data(32 * i + 31, 32 * i)))(elemIdx(1, 0))
  private val idx64 = VecInit((0 until 2).map(i => in.op2Data(64 * i + 63, 64 * i)))(elemIdx(0))
  private val indexOffset = MuxLookup(in.ieew, idx64)(Seq(
    0.U -> Cat(0.U((XLEN - 8).W), idx8),
    1.U -> Cat(0.U((XLEN - 16).W), idx16),
    2.U -> Cat(0.U((XLEN - 32).W), idx32),
    3.U -> idx64,
  ))

  private val offset = Mux(VAGQUopType.isStride(in.uopType), strideOffset, indexOffset)

  out.vaddr := in.baseAddr + offset
  out.elemIdx := elemIdx
}

class AddrGenInput(implicit p: Parameters) extends VAGQBundle {
  val uopType = UInt(3.W)
  val baseAddr = UInt(XLEN.W)
  val op2Data = UInt(VLEN.W)
  val uopIdx = UInt(vagqUopIdxWidth.W)
  val byteOffset = UInt(vagqFlowByteWidth.W)
  val deew = UInt(VAGQConstants.EewWidth.W)
  val ieew = UInt(VAGQConstants.EewWidth.W)
}

class AddrGenOutput(implicit p: Parameters) extends VAGQBundle {
  val vaddr = UInt(XLEN.W)
  val elemIdx = UInt(vagqFlowByteWidth.W)
}
