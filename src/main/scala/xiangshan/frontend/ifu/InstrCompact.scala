package xiangshan.frontend.ifu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.PrunedAddr

class InstrCompact(implicit p: Parameters) extends IfuModule {
  class InstrCompactIO extends IfuBundle {
    class InstrCompactReq(implicit p: Parameters) extends IfuBundle {
      val fetchSize               = Vec(FetchPorts, UInt(log2Ceil(FetchBlockInstNum + 1).W))
      val startVAddr              = Vec(FetchPorts, PrunedAddr(VAddrBits))
      val instrCountBeforeCurrent = Vec(FetchBlockInstNum + 1, UInt(log2Ceil(FetchBlockInstNum + 1).W))
      val rawInstrValid           = Vec(FetchBlockInstNum, Bool())
      val rawIsRvc                = Vec(FetchBlockInstNum, Bool())
      val identifiedCfi           = Vec(FetchBlockInstNum, Bool())
    }
    val req:  InstrCompactReq    = Input(new InstrCompactReq)
    val resp: InstrCompactBundle = Output(new InstrCompactBundle(FetchBlockInstNum))
  }
  val io: InstrCompactIO = IO(new InstrCompactIO)

  private val fetchSize               = io.req.fetchSize
  private val startVAddr              = io.req.startVAddr
  private val instrCountBeforeCurrent = io.req.instrCountBeforeCurrent
  private val rawInstrValid           = io.req.rawInstrValid
  private val rawIsRvc                = io.req.rawIsRvc
  private val rawIdentifiedCfi        = io.req.identifiedCfi
  private val fetchBlockSelect = VecInit.tabulate(FetchBlockInstNum)(i => Mux(fetchSize(0) > i.U, false.B, true.B))
  private val fetchPcLowerResult = VecInit.tabulate(FetchPorts)(i =>
    VecInit((0 until FetchBlockInstNum).map(j =>
      Cat(0.U(1.W), startVAddr(i)(PcCutPoint - 1, 0)) + (j * 2).U
    ))
  ) // cat with overflow bit

  private val fetchBlockIndex = VecInit.tabulate(FetchPorts)(i =>
    VecInit.tabulate(FetchBlockInstNum)(j => fetchPcLowerResult(i)(j)(log2Ceil(ICacheLineBytes) - 1, 1))
  )

  private val twoFetchBlockIndex = VecInit.tabulate(FetchBlockInstNum)(i =>
    Mux(fetchSize(0) > i.U, fetchBlockIndex(0)(i), fetchBlockIndex(1)(i))
  )

  private val twoFetchPcLowerResult = VecInit.tabulate(FetchBlockInstNum)(i =>
    Mux(fetchSize(0) > i.U, fetchPcLowerResult(0)(i), fetchPcLowerResult(1)(i))
  )

  private val rawPcLowerResult = twoFetchPcLowerResult

  private val instrSelectLowIndex   = WireDefault(VecInit.fill(FetchBlockInstNum)(true.B))
  private val instrSelectFetchBlock = WireDefault(VecInit.fill(FetchBlockInstNum)(false.B))

  private val instrPcLowerResult = WireDefault(VecInit.fill(FetchBlockInstNum)(0.U((PcCutPoint + 1).W)))
  private val instrIsRvc         = WireDefault(VecInit.fill(FetchBlockInstNum)(false.B))
  private val instrEndOffset     = WireDefault(VecInit.fill(FetchBlockInstNum)(0.U(FetchBlockInstOffsetWidth.W)))
  private val instrIndexEntry    = Wire(Vec(FetchBlockInstNum, new InstrIndexEntry))
  private val instrIdentifiedCfi = WireDefault(VecInit.fill(FetchBlockInstNum)(false.B))

  // Fetch PC and index info for valid instructions based on their positions.
  instrIndexEntry.zipWithIndex.foreach {
    case (instrIndex, idx) =>
      val instrRange = idx until Math.min(2 * idx + 2, FetchBlockInstNum)

      val validOH = instrRange.map {
        i => rawInstrValid(i) & (instrCountBeforeCurrent(i) === idx.U)
      }

      val index         = instrRange.map(twoFetchBlockIndex(_))
      val select        = instrRange.map(fetchBlockSelect(_))
      val pcLowerResult = instrRange.map(twoFetchPcLowerResult(_))
      val isRvc         = instrRange.map(rawIsRvc(_))
      val instrOffset   = instrRange.map(i => Mux(rawIsRvc(i), i.U, (i + 1).U))
      // FIXME: This is wrong when 2-taken is enabled
      val identifiedCfi = instrRange.map(rawIdentifiedCfi(_))

      instrIndex.valid           := validOH.reduce(_ || _)
      instrIndex.value           := Mux1H(validOH, index)
      instrSelectFetchBlock(idx) := Mux1H(validOH, select)
      instrPcLowerResult(idx)    := Mux1H(validOH, pcLowerResult)
      instrIsRvc(idx)            := Mux1H(validOH, isRvc)
      instrEndOffset(idx)        := Mux1H(validOH, instrOffset)
      instrIdentifiedCfi(idx)    := Mux1H(validOH, identifiedCfi)
  }

  io.resp.instrIndex     := instrIndexEntry
  io.resp.instrIsRvc     := instrIsRvc
  io.resp.selectBlock    := instrSelectFetchBlock
  io.resp.instrPcLower   := instrPcLowerResult
  io.resp.instrEndOffset := instrEndOffset
  io.resp.identifiedCfi  := instrIdentifiedCfi
}