package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.vector.Bundles.Vl
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.backend.rename.RatReadPort

class DecodeStageIO(implicit p: Parameters) extends XSBundle {
  // params alias
  private val numIntRegSrc = backendParams.numIntRegSrc
  private val numIntRatPorts = numIntRegSrc
  private val numFpRegSrc = backendParams.numFpRegSrc
  private val numFpRatPorts = numFpRegSrc
  private val numVecRegSrc = backendParams.numVecRegSrc
  private val numVecRatPorts = numVecRegSrc

  // Input
  val redirect = Input(Bool())
  // from Ibuffer
  val in = Vec(DecodeWidth, Flipped(DecoupledIO(new DecodeInUop)))
  // from FusionDecoder
  val fusion = Vec(DecodeWidth - 1, Input(Bool()))

  // from CSR
  val csrCtrl = Input(new CustomCSRCtrlIO)
  val fromCSR = Input(new CSRToDecode)
  val vstart = Input(Vl())

  // to Rename
  val out = Vec(DecodeWidth, DecoupledIO(new DecodeOutUop))

  // to RAT
  val intRat = Vec(RenameWidth, Vec(numIntRatPorts, Flipped(new RatReadPort(IntLogicRegs))))
  val fpRat = Vec(RenameWidth, Vec(numFpRatPorts, Flipped(new RatReadPort(FpLogicRegs))))
  val vecRat = Vec(RenameWidth, Vec(numVecRatPorts, Flipped(new RatReadPort(VecLogicRegs))))
  val v0Rat = Vec(RenameWidth, Flipped(new RatReadPort(V0LogicRegs)))
  val vlRat = Vec(RenameWidth, Flipped(new RatReadPort(VlLogicRegs)))

  val toFrontend = new Bundle {
    val canAccept = Output(Bool())
  }

  val toCSR = new Bundle {
    val trapInstInfo = ValidIO(new TrapInstInfo)
  }

  val stallReason = new Bundle {
    val in = Flipped(new StallReasonIO(DecodeWidth))
    val out = new StallReasonIO(DecodeWidth)
  }
}

class DecodeStage(implicit p: Parameters) extends XSModule
  with HasPerfEvents {

  val io = IO(new DecodeStageIO)


  val perfEvents = Seq()
  generatePerfEvent()
}
