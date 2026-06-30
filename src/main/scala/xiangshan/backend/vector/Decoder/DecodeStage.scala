package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.decode.isa.Extensions._
import xiangshan.backend.fu.vector.Bundles.{Vl, Vstart}
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.backend.rename.RatReadPort
import xiangshan.backend.vector.Decoder.Types.DecodeSelImm
import xiangshan.backend.vector.LatDecoder
import xiangshan.ExceptionNO._

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

  import DecodeStage._

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
    numM2M4M8Channel = (8, 8, 8),
  ))

  val debug_globalCounter = RegInit(0.U(XLEN.W))

  /** whether instruction decoded are illegal */
  // Register uop fire/exception/uopBits to cut timing path before trapInstInfo output
  val uopFire_reg   = RegNext(VecInit(out.uop.map(_.fire)))
  val uopExcII_reg  = RegNext(VecInit(out.uop.map(_.bits.exceptionVec(illegalInstr))))
  val uopExcVI_reg  = RegNext(VecInit(out.uop.map(_.bits.exceptionVec(virtualInstr))))
  val uopBits_reg   = RegNext(VecInit(out.uop.map(_.bits)))

  val isIllegalInstVec = VecInit(uopFire_reg.zip(uopExcII_reg).zip(uopExcVI_reg).map {
    case ((fire, excII), excVI) => fire && (excII || excVI)
  })
  val hasIllegalInst = isIllegalInstVec.reduce(_ || _)
  val illegalInst = PriorityMuxDefault(isIllegalInstVec.zip(uopBits_reg), 0.U.asTypeOf(new DecodeOutUop))

  val illegalTrapInstInfo = Wire(chiselTypeOf(out.toCSR.trapInstInfo.bits))
  illegalTrapInstInfo := DontCare
  illegalTrapInstInfo.fromDecodedInst(illegalInst)

  val redirect_reg = RegNext(in.redirect.valid)
  out.toCSR.trapInstInfo.valid := !redirect_reg && hasIllegalInst
  out.toCSR.trapInstInfo.bits  := illegalTrapInstInfo


  for (i <- decodeChannels.in.mops.indices) {
    val inMopBits = in.mop(i).bits

    decodeChannels.in.redirect := in.redirect.valid
    decodeChannels.in.renameCanAccept := out.uop.head.ready
    decodeChannels.in.mops(i).valid := in.mop(i).valid
    decodeChannels.in.mops(i).bits.info match {
      case info =>
        info.rawInst := inMopBits.instr
        info.vtype   := inMopBits.vtype
        info.fromCSR := in.fromCSR
        info.vstart  := in.vstart
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
        ctrl.vtype            := inMopBits.vtype
        ctrl.oldVType         := inMopBits.specvtype
        ctrl.rawInst          := inMopBits.instr
        ctrl.debug.foreach(_  := inMopBits.debug.get)
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
        bits.exceptionVec extendFrom mopInfo.exceptionVec
        bits.exceptionVec(illegalInstr) := mopInfo.exceptionVec(illegalInstr) || uopInfo.exceptionII
        bits.exceptionVec(virtualInstr) := uopInfo.exceptionVI
        bits.exceptionVec(breakPoint)   := TriggerAction.isExp(mopInfo.trigger)
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
        bits.srcType :=
          Seq(uopInfo.src1Ren, uopInfo.src2Ren, uopInfo.src3Ren)
            .zip(Seq(uopInfo.src1Type, uopInfo.src2Type, uopInfo.src3Type))
            .map{ case (ren, typ) => Mux(ren, typ.toSrcType, SrcType.no) }
        bits.v0Ren := uopInfo.v0Ren
        bits.lsrc := Seq(uopInfo.lsrc1, uopInfo.lsrc2, uopInfo.lsrc3)
        bits.ldest := uopInfo.ldest
        bits.fuType := uopInfo.fuType
        bits.fuOpType := uopInfo.opcode
        bits.rfWen := uopInfo.gpWen
        bits.fpWen := uopInfo.fpWen
        bits.vecWen := uopInfo.vpWen
        bits.v0Wen := uopInfo.vpWen && uopInfo.ldest === 0.U
        bits.vlWen := uopInfo.vlWen
        bits.vlRen := uopInfo.vlRen
        bits.waitForward := uopInfo.noSpec
        bits.blockBackward := uopInfo.blockBack
        bits.flushPipe := uopInfo.flushPipe
        bits.canRobCompress := uopInfo.canRobCompress
        bits.selImm := Mux(uopInfo.selImm.valid, DecodeSelImm.toSelImm(uopInfo.selImm.bits), DecodeSelImm.NO)
        bits.imm := uopInfo.imm
        bits.vpu.vill     := mopInfo.vtype.illegal     // Todo: remove it
        bits.vpu.vma      := mopInfo.vtype.vma         // Todo: remove it
        bits.vpu.vta      := mopInfo.vtype.vta         // Todo: remove it
        bits.vpu.vsew     := mopInfo.vtype.vsew        // Todo: remove it
        bits.vpu.vlmul    := mopInfo.vtype.vlmul       // Todo: remove it
        bits.vpu.vm := uopInfo.vm
        bits.vpu.vstart := 0.U // Todo: remove it
        bits.vpu.frm := 0.U // Todo: remove it
        bits.vpu.fpu := 0.U.asTypeOf(bits.vpu.fpu) // Todo: remove it
        bits.vpu.vxrm := 0.U // Todo: remove it
        bits.vpu.vuopIdx := 0.U // Todo: remove it
        bits.vpu.lastUop := false.B // Todo: remove it
        bits.vpu.vmask := 0.U // Todo: remove it
        bits.vpu.vl := 0.U // Todo: remove it
        bits.vpu.nf := 0.U // Todo: remove it
        bits.vpu.veew := 0.U // Todo: remove it
        bits.vpu.isReverse := 0.U // Todo: remove it
        bits.vpu.isExt := 0.U // Todo: remove it
        bits.vpu.isNarrow := 0.U // Todo: remove it
        bits.vpu.isDstMask := 0.U // Todo: remove it
        bits.vpu.isOpMask := 0.U // Todo: remove it
        bits.vpu.isMove := 0.U // Todo: remove it
        bits.vpu.isDependOldVd := 0.U // Todo: remove it
        bits.vpu.isWritePartVd := 0.U // Todo: remove it
        bits.vpu.isVleff := false.B // Todo: remove it
        bits.vpu.maskVecGen := 0.U // Todo: remove it
        bits.vpu.sew8 := false.B // Todo: remove it
        bits.vpu.sew16 := false.B // Todo: remove it
        bits.vpu.sew32 := false.B // Todo: remove it
        bits.vpu.sew64 := false.B // Todo: remove it
        bits.frm := uopInfo.frm
        bits.vm := uopInfo.vm
        bits.vtype := mopInfo.vtype
        bits.oldVType := mopInfo.oldVType
        bits.vlsInstr := false.B // Todo: remove
        bits.fflagsWen := uopInfo.fflagsWen
        bits.isMove := false.B // Todo
        bits.uopIdx := uopInfo.uopIdx
        bits.uopSplitType := 0.U // Todo: remove
        bits.isVset := uopInfo.isVset
        bits.firstUop := uopInfo.isFirstUop
        bits.lastUop := uopInfo.isLastUop
        bits.isJR := uopInfo.isJR
        bits.isJ := uopInfo.isJ
        bits.isJr := uopInfo.isJr
        bits.numWB := uopInfo.numWb +& 1.U
        bits.latency := LatDecoder(bits.fuType, bits.fuOpType)
        bits.debug.foreach{ x =>
          x.pc := mopInfo.debug.get.pc
          x.debug_seqNum.seqNum := mopInfo.debug.get.debug_seqNum.seqNum
          x.debug_seqNum.uopIdx := uopInfo.uopIdx
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

  out.toFrontend.canAccept := !in.redirect.valid && out.uop.head.ready

  stallReason.out.reason := stallReason.in.reason

  val perfEvents = Seq()
  generatePerfEvent()
}

object DecodeStage {
  class In(implicit p: Parameters) extends XSBundle {
    val redirect = Input(ValidIO(new Redirect))

    // The ready of mop means this mop is accepted by DecodeStage
    // Ready signal depends on valid
    val mop = Vec(DecodeWidth, Flipped(DecoupledIO(new DecodeInUop)))
    // from FusionDecoder
    val fusion = Vec(DecodeWidth - 1, Input(Bool()))

    // from CSR
    val fromCSR = Input(new CSRToDecode)
    val vstart = Input(Vstart())
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

    val toFrontend = Output(new Bundle {
      val canAccept = Bool()
    })

    val toCSR = new Bundle {
      val trapInstInfo = ValidIO(new TrapInstInfo)
    }
  }
}
