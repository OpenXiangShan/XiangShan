package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.decode.isa.Extensions._
import xiangshan.backend.fu.vector.Bundles.Vl
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.backend.rename.RatReadPort

class DecodeStage()(implicit p: Parameters) extends LazyModule {
  override def shouldBeInlined: Boolean = false

  lazy val module = new DecodeStageImp(this)(p)
}

class DecodeStageImp(
  override val wrapper: DecodeStage
)(
  implicit p: Parameters
) extends LazyModuleImp(wrapper)
  with HasXSParameter
  with HasPerfEvents {

  val extensions: Seq[ExtBase] = Seq(
    I, M, A, F, D, Zicsr,
    System, S,
    Za64rs, /*Zacas,*/ Zawrs,
    Zba, Zbb, Zbc, Zbs, Zbkb, Zbkc, Zbkx,
    V,
    XSTrap,
    // Zcb, Zcmop,
    // Zfa, Zfh, ZfaZfh, ZfaF, ZfaD, Zfhmin,
  )

  import DecodeStage.{In, Out}

  val in = IO(new In)
  val out = IO(new Out)

  val stallReason = IO(new Bundle {
    val in = Flipped(new StallReasonIO(DecodeWidth))
    val out = new StallReasonIO(DecodeWidth)
  })

  // io alias
  private val outReadys = out.uop.map(_.ready)
  private val inValids  = in.mop.map(_.valid)
  private val outValids = out.uop.map(_.valid)
  private val inValid   = VecInit(inValids).asUInt.orR
  private val outValid  = VecInit(outValids).asUInt.orR

  /** Assume number of ready channels be "RenameWidth" if the first output channel is ready. If not, assume that be 0 */
  val readyCounter = Mux(outReadys.head, RenameWidth.U, 0.U)

  val decodeChannels: DecodeChannels = Module(new DecodeChannels(
    mopWidth = DecodeWidth,
    uopWidth = RenameWidth,
    extensions = extensions,
    numM2M4M8Channel = (8, 8, 8),
  ))

  val debug_globalCounter = RegInit(0.U(XLEN.W))


  for (i <- decodeChannels.in.mops.indices) {
    val inMopBits = in.mop(i).bits

    decodeChannels.in.redirect := in.redirect
    decodeChannels.in.renameCanAccept := out.uop.head.ready
    decodeChannels.in.mops(i).valid := in.mop(i).valid
    decodeChannels.in.mops(i).bits.info match {
      case info =>
        info.rawInst := inMopBits.instr
        info.vtype    := inMopBits.vtype
    }
    decodeChannels.in.mops(i).bits.ctrl match {
      case ctrl =>
        ctrl.foldpc           := inMopBits.foldpc
        ctrl.exceptionVec     := inMopBits.exceptionVec
        ctrl.isFetchMalAddr   := inMopBits.isFetchMalAddr
        ctrl.trigger          := inMopBits.trigger
        ctrl.isRVC            := inMopBits.isRVC
        ctrl.fixedTaken       := inMopBits.fixedTaken
        ctrl.predTaken        := inMopBits.predTaken
        ctrl.crossPageIPFFix  := inMopBits.crossPageIPFFix
        ctrl.ftqPtr           := inMopBits.ftqPtr
        ctrl.ftqOffset        := inMopBits.ftqOffset
        ctrl.isLastInFtqEntry := inMopBits.isLastInFtqEntry
        ctrl.rawInst          := inMopBits.instr
        ctrl.debug.foreach(_ := inMopBits.debug.get)
    }
  }

  in.mop.zipWithIndex.foreach {
    case (mop, i) =>
      mop.ready := decodeChannels.in.mops(i).ready
  }

  out.uop.zipWithIndex.foreach { case (uop, i) =>
    val coUop = decodeChannels.out.uops(i)
    val mopInfo = coUop.bits.ctrl
    val uopInfo = coUop.bits.info

    uop.valid := coUop.valid
    uop.bits match {
      case bits =>
        bits.foldpc := mopInfo.foldpc
        bits.exceptionVec := mopInfo.exceptionVec // Todo: exception
        bits.isFetchMalAddr := mopInfo.isFetchMalAddr
        bits.trigger := mopInfo.trigger
        bits.isRVC := mopInfo.isRVC
        bits.fixedTaken := mopInfo.fixedTaken
        bits.predTaken := mopInfo.predTaken
        bits.crossPageIPFFix := mopInfo.crossPageIPFFix
        bits.ftqPtr := mopInfo.ftqPtr
        bits.ftqOffset := mopInfo.ftqOffset
        bits.isLastInFtqEntry := mopInfo.isLastInFtqEntry
        bits.instr := mopInfo.rawInst
        bits.commitType := uopInfo.commitType
        bits.srcType := Seq(uopInfo.src1Type, uopInfo.src2Type, uopInfo.src3Type).map(_.toSrcType).padTo(bits.srcType.size, SrcType.no)
        bits.lsrc := Seq(uopInfo.lsrc1, uopInfo.lsrc2, uopInfo.lsrc3, /*mask*/ 0.U, /*vl*/ 0.U)
        bits.ldest := uopInfo.ldest
        bits.fuType := uopInfo.fuType
        bits.fuOpType := uopInfo.opcode
        bits.rfWen := uopInfo.gpWen
        bits.fpWen := uopInfo.fpWen
        bits.vecWen := uopInfo.vpWen && uopInfo.ldest =/= 0.U
        bits.v0Wen := uopInfo.vpWen && uopInfo.ldest === 0.U
        bits.vlWen := uopInfo.vlWen
        bits.waitForward := uopInfo.noSpec
        bits.blockBackward := uopInfo.blockBack
        bits.flushPipe := uopInfo.flushPipe
        bits.canRobCompress := uopInfo.canRobCompress
        bits.selImm := Mux(uopInfo.selImm.valid, uopInfo.selImm.bits, Types.SelImm.NO)
        bits.imm := uopInfo.imm
        bits.fpu := 0.U.asTypeOf(bits.fpu)
        bits.vpu := 0.U.asTypeOf(bits.vpu)
        bits.vlsInstr := false.B // Todo: remove
        bits.wfflags := uopInfo.fflagsWen
        bits.isMove := false.B // Todo
        bits.uopIdx := uopInfo.uopIdx
        bits.uopSplitType := 0.U // Todo: remove
        bits.isVset := uopInfo.isVset
        bits.firstUop := uopInfo.isFirstUop
        bits.lastUop := uopInfo.isLastUop
        bits.numWB := uopInfo.numUop +& 1.U
        bits.needFrm := 0.U.asTypeOf(bits.needFrm)
        bits.debug.foreach{ x =>
          x.pc := mopInfo.debug.get.pc
          x.seqNum.seqNum := mopInfo.debug.get.seqNum.seqNum
          x.seqNum.uopIdx := uopInfo.uopIdx
        }
    }
  }

  for (i <- out.uop.indices) {
    out.intRat(i)(0).addr := out.uop(i).bits.lsrc(0)
    out.intRat(i)(1).addr := out.uop(i).bits.lsrc(1)
    out.intRat(i).foreach(_.hold := !out.uop(i).ready)

    out.fpRat(i)(0).addr := out.uop(i).bits.lsrc(0)
    out.fpRat(i)(1).addr := out.uop(i).bits.lsrc(1)
    out.fpRat(i)(2).addr := out.uop(i).bits.lsrc(2)
    out.fpRat(i).foreach(_.hold := !out.uop(i).ready)

    out.vecRat(i)(0).addr := out.uop(i).bits.lsrc(0) // vs1
    out.vecRat(i)(1).addr := out.uop(i).bits.lsrc(1) // vs2
    out.vecRat(i)(2).addr := out.uop(i).bits.lsrc(2) // old_vd
    out.vecRat(i).foreach(_.hold := !out.uop(i).ready)

    out.v0Rat(i).addr := V0_IDX.U
    out.v0Rat(i).hold := !out.uop(i).ready

    out.vlRat(i).addr := Vl_IDX.U
    out.vlRat(i).hold := !out.uop(i).ready
  }

  out.toFrontend.canAccept := !in.redirect && out.uop.head.ready

  out.toCSR.trapInstInfo.valid := !in.redirect && false.B // Todo: illegal inst
  out.toCSR.trapInstInfo.bits := 0.U.asTypeOf(out.toCSR.trapInstInfo.bits)

  stallReason.in.backReason := stallReason.out.backReason
  stallReason.out.reason lazyZip stallReason.in.reason lazyZip in.mop.map(_.valid) foreach {
    case (out, in, valid) =>
      out := Mux(
        stallReason.out.backReason.valid,
        stallReason.out.backReason.bits,
        in
      )
  }


  val perfEvents = Seq()
  generatePerfEvent()
}

object DecodeStage {
  class In(implicit p: Parameters) extends XSBundle {
    val redirect = Input(Bool())

    // The ready of mop means this mop is accepted by DecodeStage
    // Ready signal depends on valid
    val mop = Vec(DecodeWidth, Flipped(DecoupledIO(new DecodeInUop)))
    // from FusionDecoder
    val fusion = Vec(DecodeWidth - 1, Input(Bool()))

    // from CSR
    // Todo: merge this bundle in only one
    val csrCtrl = Input(new CustomCSRCtrlIO)
    val fromCSR = Input(new CSRToDecode)
    val vstart = Input(Vl())
  }

  class Out(implicit p: Parameters) extends XSBundle {
    // params alias
    private val numIntRegSrc = backendParams.numIntRegSrc
    private val numIntRatPorts = numIntRegSrc
    private val numFpRegSrc = backendParams.numFpRegSrc
    private val numFpRatPorts = numFpRegSrc
    private val numVecRegSrc = backendParams.numVecRegSrc
    private val numVecRatPorts = numVecRegSrc

    // to Rename
    val uop = Vec(DecodeWidth, DecoupledIO(new DecodeOutUop))

    // to RAT
    val intRat = Vec(RenameWidth, Vec(numIntRatPorts, Flipped(new RatReadPort(log2Ceil(IntLogicRegs)))))
    val fpRat = Vec(RenameWidth, Vec(numFpRatPorts, Flipped(new RatReadPort(log2Ceil(FpLogicRegs)))))
    val vecRat = Vec(RenameWidth, Vec(numVecRatPorts, Flipped(new RatReadPort(log2Ceil(VecLogicRegs)))))
    // no v0Rat and vlRat Bundle because they are only one logic register
    val v0Rat = Vec(RenameWidth, Flipped(new RatReadPort(log2Ceil(V0LogicRegs))))
    val vlRat = Vec(RenameWidth, Flipped(new RatReadPort(log2Ceil(VlLogicRegs))))

    val toFrontend = new Bundle {
      val canAccept = Output(Bool())
    }

    val toCSR = new Bundle {
      val trapInstInfo = ValidIO(new TrapInstInfo)
    }
  }
}
